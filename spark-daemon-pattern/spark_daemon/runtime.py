"""The skeleton's runtime: everything a daemon needs except its own observation logic.

Order at start (each step must succeed before the next):
 1. umask 077; no bytecode writes
 2. load and validate the manifest                       -> exit 2 on problems
 3. prepare the output directory (0700, owned, no symlink) -> exit 78 if unsafe
 4. take the single-instance lock                         -> exit 73 if already running
 5. static purity check of daemon.py                      -> exit 2 on problems
 6. apply the Landlock domain (r2, AP-01)                 -> exit 78 outside test mode if
    unavailable or too old (PD-15); before the audit hook, because the hook blocks ctypes,
    which this step still needs
 7. install the audit hook, then import daemon.py
 8. recover the ledger: verify, quarantine a torn tail     -> exit 65 if corrupt (nothing changed)
 9. inventory the output directory for foreign files
10. commit LEDGER_TAIL_QUARANTINED (if any), DAEMON_START (carries landlock: {abi, status,
    gaps}), and a foreign-files DAEMON_ERROR (if any)
11. notify READY=1
Then each cycle: sense -> decide -> commit events -> refresh digest -> gc.collect() ->
WATCHDOG=1 -> sleep (jittered). On SIGTERM/SIGINT or max cycles: DAEMON_STOP, STOPPING=1,
exit 0. Any ledger write or fsync failure: exit 70 at once. Any policy violation: exit 78
(fail closed).

Three additions (r2). Landlock (spark_daemon/landlock.py) is a fourth, kernel-enforced
layer next to purity.py, the audit hook and the systemd unit; see that module's docstring
for what it does and does not cover, in particular the gap it cannot close on its own (a
denied path nested inside a granted read). The poll sleep carries a small random jitter, so
many daemons started together do not wake in lockstep and spike the shared memory bus
(disabled in test mode, for deterministic timing). gc.collect() runs once per cycle, after
committing, to reclaim reference cycles (chiefly exception tracebacks from the error path,
plus anything a daemon's own decide() builds) and give the allocator a chance to return
freed arenas to the OS between cycles. Plain string and dict garbage is already freed by
refcounting the instant it goes out of scope; gc.collect() adds nothing for that. Measured
at about 0.2ms per idle call on this machine, negligible against any allowed poll interval.
"""

import collections
import fcntl
import gc
import importlib.util
import os
import random
import resource
import signal
import sys
import time

from . import (DIGEST_NAME, EXIT_ALREADY_RUNNING, EXIT_LEDGER_CORRUPT, EXIT_OK, EXIT_POLICY,
               EXIT_UNCERTAIN_COMMIT, EXIT_USAGE, LOCK_NAME, TMP_DIR, TMP_PREFIX, VERSION)
from . import guard, landlock, ledger, manifest as manifest_mod, notify as notify_mod, purity, render
from .canonical import CanonicalError, sha256_hex, to_json_value
from .context import Context, utc_now

EXCEPTION_NAME_MAX = 64
FOREIGN_NAMES_MAX = 20
FOREIGN_NAME_CHARS = 128
RECENT_RECORDS = 20
# Thundering-herd defense (r2): +/- this fraction of the interval, capped in absolute
# seconds so a long interval (up to 86400s) does not drift by hours. Disabled in test mode.
JITTER_FRACTION = 0.05
JITTER_MAX_SECONDS = 30.0


def log(message: str) -> None:
    """Bounded operational diagnostics to stderr (the journal). Never record content."""
    os.write(2, (f"spark-daemon: {message}"[:500] + "\n").encode("utf-8", "replace"))


def _jitter_bound(interval_seconds: float, test_mode: bool) -> float:
    """The one-sided jitter bound in seconds: 0 in test mode, otherwise a fixed fraction of
    the interval capped at JITTER_MAX_SECONDS so a long interval does not drift by hours."""
    if test_mode:
        return 0.0
    return min(JITTER_MAX_SECONDS, interval_seconds * JITTER_FRACTION)


def _exception_name(exc) -> str:
    name = type(exc).__name__
    return name if name.isidentifier() and len(name) <= EXCEPTION_NAME_MAX else "Exception"


def _tool_versions(policy, step_timeout):
    versions = {"python": sys.version.split()[0], "kernel": os.uname().release, "git": None}
    if "git" in policy.commands:
        from . import proc
        try:
            result = proc.run(["git", "--version"], executables=policy.commands,
                              timeout=step_timeout, max_bytes=256)
            if result.returncode == 0:
                versions["git"] = result.stdout.strip()[:64]
        except Exception:  # noqa: BLE001 - a version probe must never stop the daemon
            pass
    return versions


def _load_module(path):
    spec = importlib.util.spec_from_file_location("spark_daemon_user_code", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _atomic_write(directory, name, data: bytes):
    tmp = os.path.join(directory, f"{TMP_PREFIX}{name}-{os.getpid()}-{time.monotonic_ns()}")
    fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_CLOEXEC, 0o600)
    try:
        view = memoryview(data)
        while view:
            view = view[os.write(fd, view):]
        os.fsync(fd)
    finally:
        os.close(fd)
    os.replace(tmp, os.path.join(directory, name))
    dfd = os.open(directory, os.O_RDONLY | os.O_DIRECTORY | os.O_CLOEXEC)
    try:
        os.fsync(dfd)
    finally:
        os.close(dfd)


class _Stop:
    def __init__(self):
        self.requested = False
        self.reason = None

    def __call__(self, signum, _frame):
        self.requested = True
        self.reason = f"signal {signal.Signals(signum).name}"


def run(manifest_path: str, max_cycles: int | None = None) -> int:
    os.umask(0o077)
    sys.dont_write_bytecode = True

    test_mode = os.environ.get("SPARK_DAEMON_TEST") == "1"
    if test_mode and os.environ.get("INVOCATION_ID"):
        log("test mode is refused inside a systemd service")
        return EXIT_USAGE
    overrides = {}
    try:
        m = manifest_mod.load(manifest_path)
    except manifest_mod.ManifestError as e:
        for problem in e.problems:
            log(f"manifest: {problem}")
        return EXIT_USAGE

    interval = float(m.trigger.interval_seconds)
    audit_mode = "enforce"
    if test_mode:
        ms = os.environ.get("SPARK_DAEMON_TEST_INTERVAL_MS")
        if ms and ms.isdigit() and 50 <= int(ms) <= 60000:
            interval = int(ms) / 1000
            overrides["interval_ms"] = int(ms)
        if os.environ.get("SPARK_DAEMON_AUDIT") == "record":
            audit_mode = "record"
            overrides["audit_mode"] = "record"
    # Jitter is off in test mode so cycle timing stays exact for assertions; it is a fixed
    # function of the interval, not an opt-in knob, so it is recorded outside test_overrides.
    jitter_bound = _jitter_bound(interval, test_mode)
    jitter_rng = random.Random(os.urandom(16))

    policy = guard.Policy(m)
    out = policy.output_dir
    try:
        guard.prepare_output_dir(out)
    except (guard.GuardError, OSError) as e:
        log(f"output directory refused: {e}")
        return EXIT_POLICY

    lock_fd = os.open(os.path.join(out, LOCK_NAME), os.O_RDWR | os.O_CREAT | os.O_CLOEXEC, 0o600)
    try:
        fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        log("another instance holds the lock")
        return EXIT_ALREADY_RUNNING

    problems = purity.check_file(m.code_path)
    if problems:
        for problem in problems:
            log(f"purity: {problem}")
        return EXIT_USAGE
    with open(m.code_path, "rb") as fh:
        code_sha = sha256_hex(fh.read())

    guard.remove_stray_temp_files(out)
    tool_versions = _tool_versions(policy, m.step_timeout_seconds)
    # Landlock (r2, AP-01) before the audit hook: the hook's own blocked events include
    # ctypes, which this step still needs. A kernel too old or without Landlock is a
    # start-up refusal outside test mode (PD-15), so the self-tests and the battery still
    # run on whatever the developer or CI kernel offers.
    daemon_dir = os.path.dirname(m.code_path)
    try:
        landlock_info = landlock.apply_supervisor_domain(
            policy, extra_read_paths=landlock.system_read_paths() + (daemon_dir,),
            test_mode=test_mode)
    except landlock.LandlockError as e:
        log(f"Landlock refused: {e}")
        return EXIT_POLICY
    guard.install_audit_hook(policy, audit_mode)
    try:
        module = _load_module(m.code_path)
    except Exception as e:  # noqa: BLE001
        log(f"daemon code failed to import ({_exception_name(e)})")
        return EXIT_USAGE

    try:
        scan, quarantine = ledger.recover(out, m.name, m.ledger.record_max_bytes)
    except ledger.LedgerCorrupt as e:
        log(str(e))
        return EXIT_LEDGER_CORRUPT
    foreign = guard.inventory(out)

    writer = ledger.LedgerWriter(out, m.name, os.urandom(16).hex(), m.ledger.record_max_bytes,
                                 scan.tail, utc_now)
    stop = _Stop()
    signal.signal(signal.SIGTERM, stop)
    signal.signal(signal.SIGINT, stop)
    ctx = Context(policy, step_timeout=m.step_timeout_seconds, tmp_dir=os.path.join(out, TMP_DIR))
    recent = collections.deque(maxlen=RECENT_RECORDS)
    ping_every = max(0.2, min(1.0, m.watchdog_seconds / 4))

    try:
        writer.open()
        if quarantine:
            recent.append(writer.append("LEDGER_TAIL_QUARANTINED", {
                "file": quarantine.file, "length": quarantine.length, "sha256": quarantine.sha256}))
        recent.append(writer.append("DAEMON_START", {
            "skeleton_version": VERSION,
            "manifest_sha256": m.sha256,
            "daemon_code_sha256": code_sha,
            "tools": tool_versions,
            "interval_ms": int(interval * 1000),
            "jitter_max_ms": int(jitter_bound * 1000),  # ledger values are integers; see canonical.py
            "landlock": landlock_info,
            "previous_run_ended_cleanly": None if scan.records == 0 else scan.last_event_type == "DAEMON_STOP",
            "test_overrides": overrides,
        }))
        if foreign:
            recent.append(writer.append("DAEMON_ERROR", {
                "category": "OUTPUT_DIR_FOREIGN_FILES", "count": len(foreign),
                "names": [n[:FOREIGN_NAME_CHARS] for n in foreign[:FOREIGN_NAMES_MAX]]}))
        cpu_at_start = _cpu_us()
        notify_mod.notify("READY=1")
        notify_mod.notify(f"STATUS=observing; ledger seq {writer.tail.seq}")

        prev, streak, cycles = None, None, 0
        violations_seen = guard.VIOLATIONS["count"]
        while not stop.requested:
            if max_cycles is not None and cycles >= max_cycles:
                stop.reason = "max_cycles"
                break
            cycles += 1
            stage = "sense"
            error = None
            prepared = snapshot = None
            try:
                snapshot = to_json_value(module.sense(ctx))
                stage = "decide"
                events = module.decide(prev, snapshot)
                stage = "validate"
                prepared = writer.prepare(_validate_events(events, m.ledger.event_types))
            except guard.PolicyViolation as e:
                error = ("POLICY_VIOLATION", e)
            except (CanonicalError, ledger.RecordTooLarge, _EventError) as e:
                error = ("SNAPSHOT_INVALID" if stage == "sense" else "EVENT_INVALID", e)
            except Exception as e:  # noqa: BLE001 - daemon code may fail; the skeleton records it
                error = (f"{stage.upper()}_FAILED", e)

            if not error:
                for record in writer.commit(prepared):
                    recent.append(record)
                prev = snapshot
                if streak:
                    recent.append(_cleared(writer, streak, "success"))
                    streak = None
                if m.digest.enabled and hasattr(module, "digest"):
                    _refresh_digest(module, m, out, writer, snapshot, recent)

            # Counts violations even if the daemon's own code caught and ignored the exception.
            if guard.VIOLATIONS["count"] != violations_seen:
                error = ("POLICY_VIOLATION", error[1] if error else guard.PolicyViolation())

            if error:
                category, exc = error
                key = (category, _exception_name(exc))
                if streak and streak[0] == key:
                    streak = (key, streak[1] + 1)
                else:
                    if streak:
                        recent.append(_cleared(writer, streak, "different_error"))
                    recent.append(writer.append("DAEMON_ERROR", {"category": key[0], "exception_type": key[1]}))
                    streak = (key, 1)
                if category == "POLICY_VIOLATION":
                    log("policy violation; stopping (fail closed)")
                    notify_mod.notify("STOPPING=1")
                    return EXIT_POLICY

            gc.collect()
            notify_mod.notify("WATCHDOG=1")
            offset = jitter_rng.uniform(-jitter_bound, jitter_bound) if jitter_bound else 0.0
            deadline = time.monotonic() + max(0.0, interval + offset)
            while not stop.requested:
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    break
                time.sleep(min(remaining, ping_every))
                notify_mod.notify("WATCHDOG=1")

        if streak:
            recent.append(_cleared(writer, streak, "stop"))
        writer.append("DAEMON_STOP", {
            "reason": stop.reason or "unknown", "cycles": cycles,
            "cpu_us_since_ready": _cpu_us() - cpu_at_start,
            "max_rss_kb": resource.getrusage(resource.RUSAGE_SELF).ru_maxrss})
        notify_mod.notify("STOPPING=1")
        return EXIT_OK
    except ledger.UncertainCommit as e:
        log(f"uncertain ledger commit ({e}); exiting so recovery can decide from disk")
        return EXIT_UNCERTAIN_COMMIT
    finally:
        writer.close()


def _cpu_us() -> int:
    usage = resource.getrusage(resource.RUSAGE_SELF)
    return int((usage.ru_utime + usage.ru_stime) * 1_000_000)


class _EventError(ValueError):
    pass


def _validate_events(events, declared):
    if not isinstance(events, (list, tuple)):
        raise _EventError("decide() must return a list of (event_type, payload)")
    out = []
    for item in events:
        if not isinstance(item, (list, tuple)) or len(item) != 2:
            raise _EventError("each event must be (event_type, payload)")
        event_type, payload = item
        if event_type not in declared:
            raise _EventError("event type is not declared in the manifest")
        if not isinstance(payload, dict):
            raise _EventError("payload must be a mapping")
        out.append((event_type, to_json_value(payload)))
    return out


def _cleared(writer, streak, ended_by):
    (category, exception_type), repeats = streak
    return writer.append("DAEMON_ERROR_CLEARED", {
        "category": category, "exception_type": exception_type, "repeats": repeats, "ended_by": ended_by})


def _refresh_digest(module, m, out, writer, snapshot, recent):
    """A digest failure never affects the ledger; the previous digest stays with its older stamp."""
    try:
        sections = module.digest(snapshot, tuple(dict(r) for r in recent))
        stamp = {"seq": writer.tail.seq, "head": writer.tail.head,
                 "last_event_utc": recent[-1]["timestamp_utc"] if recent else None}
        data = render.render_digest(m.name, stamp, sections, m.digest.max_bytes)
        _atomic_write(out, DIGEST_NAME, data)
    except Exception as e:  # noqa: BLE001 - includes PolicyViolation; the violation counter catches that
        log(f"digest not refreshed ({_exception_name(e)})")
