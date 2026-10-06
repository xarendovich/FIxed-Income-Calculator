"""The skeleton's runtime: everything a daemon needs except its own observation logic.

Order at start (each step must succeed before the next):
 1. umask 077; no bytecode writes
 2. load and validate the manifest                       -> exit 2 on problems
 3. prepare the output directory (0700, owned, no symlink) -> exit 78 if unsafe
 4. take the single-instance lock                         -> exit 73 if already running
 5. static purity check of daemon.py                      -> exit 2 on problems
    then the qualification gate (r4.11): the digests a qualified unit carries must match;
    outside test mode a start without them is refused       -> exit 78
 6. apply the Landlock domain (r2, AP-01)                 -> exit 78 outside test mode if
    unavailable or too old (PD-15); before the audit hook, because the hook blocks ctypes,
    which this step still needs
 7. install the audit hook, then import daemon.py
 8. recover the ledger: verify, quarantine a torn tail     -> exit 65 if corrupt (nothing changed)
 9. inventory the output directory for foreign files
10. commit DAEMON_START (carries landlock: {abi, status, gaps}), then LEDGER_TAIL_QUARANTINED
    (if any) and a foreign-files DAEMON_ERROR (if any)
11. notify READY=1
Then each cycle: sense -> decide -> commit events -> refresh digest -> gc.collect() ->
WATCHDOG=1 -> sleep (jittered). On SIGTERM/SIGINT or max cycles: DAEMON_STOP, STOPPING=1,
exit 0. Any ledger write or fsync failure: exit 70 at once. Any policy violation: exit 78
(fail closed).

Blind period (r3.5). A cycle is accepted when sense() returns a snapshot and its events are
committed. sense() may instead return ctx.unsettled(reason) when what it read was not stable
(say, a worktree changing under a build): the cycle is abandoned before decide(), with no
event and no error. Failed cycles are not accepted either. The runtime keeps the monotonic
time of the last accepted cycle (start-up counts as one); while the gap stays under the
manifest's blind_limit_seconds it keeps pinging the watchdog, because being patient through
a long build is correct. Past the limit it records DAEMON_ERROR SENSE_BLIND and exits 78,
which the unit never restarts: a daemon that has seen nothing for that long needs a human,
and a watchdog that keeps ticking over an empty ledger would hide it.

Blindness survives restarts (r4.0, PD-70; HF-32). A per-process clock let any restart loop
slower than the unit's start limit hide blindness forever. At start-up the runtime now finds, in
the verified ledger, the latest evidence of an accepted cycle (a daemon event, or the
last_accepted_utc a heartbeat or clean stop carried) and starts the blind clock that far back,
measured on the wall clock across the restart. A fresh ledger starts at zero. A restart that
inherits more than the limit gets exactly one reacquisition cycle: accepted, and the clock
resets; not accepted, and SENSE_BLIND follows at once, without a fresh countdown. Neither an
operator restart nor a clean stop resets the clock; only an accepted cycle does. Every
blind_limit_seconds / 2 the runtime writes DAEMON_HEARTBEAT (mode observing or blind, the last
accepted cycle, counts), so a quiet, healthy daemon is distinguishable from a dead one and its
ledger can say "watching, nothing changed".

Which clock measures it (r4.5, HF-34). Measured on the wall clock, a backward step (an NTP
correction, a manual change) made every restart inherit zero and brought HF-32 back. DAEMON_START,
DAEMON_HEARTBEAT and DAEMON_STOP now carry the kernel's boot_id and CLOCK_BOOTTIME (which no
wall-clock change moves and which counts suspend), plus blind_since_boottime_ms, the boot-time
moment the current blind stretch began. A restart in the same boot measures on that clock. Across
a reboot only the wall clock is left: it is used when it moved forward, and when it did not the
runtime assumes the worst, blind at the limit, which leaves the one reacquisition cycle.
DAEMON_START and SENSE_BLIND record the clock_basis used: "boottime", "wall", "worst_case" or
"fresh". Since r4.11 (R-4) that rule is semantics.blindness, the one function `status` also uses.

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
import datetime
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
               EXIT_SENSE_BLIND, EXIT_UNCERTAIN_COMMIT, EXIT_USAGE, LOCK_NAME, TMP_DIR, TMP_PREFIX, VERSION)
from . import guard, landlock, ledger, manifest as manifest_mod, notify as notify_mod, purity, render, semantics
from .canonical import CanonicalError, sha256_hex, to_json_value
from .context import Context, CycleBudgetExceeded, Unsettled, utc_now

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


def run(manifest_path: str, max_cycles: int | None = None, expect: dict | None = None) -> int:
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
    blind_limit = float(m.blind_limit_seconds)
    cycle_budget = float(m.cycle_budget_seconds)
    audit_mode = "enforce"
    if test_mode:
        ms = os.environ.get("SPARK_DAEMON_TEST_INTERVAL_MS")
        if ms and ms.isdigit() and 50 <= int(ms) <= 60000:
            interval = int(ms) / 1000
            overrides["interval_ms"] = int(ms)
        ms = os.environ.get("SPARK_DAEMON_TEST_BLIND_LIMIT_MS")
        if ms and ms.isdigit() and 100 <= int(ms) <= 3600000:
            blind_limit = int(ms) / 1000
            overrides["blind_limit_ms"] = int(ms)
        ms = os.environ.get("SPARK_DAEMON_TEST_CYCLE_BUDGET_MS")
        if ms and ms.isdigit() and 100 <= int(ms) <= 1800000:
            cycle_budget = int(ms) / 1000
            overrides["cycle_budget_ms"] = int(ms)
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

    boot_id = _boot_id()      # read before Landlock and the audit hook narrow what may be opened
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
    # r4.11 (R-3): a daemon runs for real only from a qualified unit, whose ExecStart carries the
    # digests a qualifying battery PASS bound. Code or a manifest changed since then never starts,
    # and a preview unit (no digests) is refused outside test mode. Both need a person: 78.
    if expect is not None:
        from .contract import contract_identity
        actual = {"manifest_sha256": m.sha256, "daemon_code_sha256": code_sha,
                  "contract_sha256": contract_identity()["contract_sha256"]}
        changed = [key for key in actual if actual[key] != expect.get(key)]
        if changed:
            log(f"not the qualified daemon: {', '.join(changed)} differ from its battery PASS; "
                "refusing to start (qualify it again with battery --emit-unit)")
            return EXIT_POLICY
    elif not test_mode:
        log("not qualified: started without the digests of a battery PASS (a preview unit, or by "
            "hand); refusing to start. An installable unit comes from battery --emit-unit")
        return EXIT_POLICY

    guard.remove_stray_temp_files(out)
    tool_versions = _tool_versions(policy, cycle_budget)
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
    except BaseException as e:  # noqa: BLE001 - includes SystemExit raised by daemon code
        log(f"daemon code failed to import ({_exception_name(e)})")
        return EXIT_USAGE

    try:
        evidence = semantics.AcceptEvidence()
        scan, quarantine = ledger.recover(out, m.name, m.ledger.record_max_bytes, on_record=evidence.observe)
    except ledger.LedgerCorrupt as e:
        log(str(e))
        return EXIT_LEDGER_CORRUPT
    foreign = guard.inventory(out)

    writer = ledger.LedgerWriter(out, m.name, os.urandom(16).hex(), m.ledger.record_max_bytes,
                                 scan.tail, utc_now)
    stop = _Stop()
    signal.signal(signal.SIGTERM, stop)
    signal.signal(signal.SIGINT, stop)
    ctx = Context(policy, cycle_budget=cycle_budget, tmp_dir=os.path.join(out, TMP_DIR))
    signal.signal(signal.SIGALRM, _over_budget)
    recent = collections.deque(maxlen=RECENT_RECORDS)
    ping_every = max(0.2, min(1.0, m.watchdog_seconds / 4))

    try:
        writer.open()
        # DAEMON_START is always the first record of a run, and so of the ledger: a torn first-ever
        # write must not make the quarantine record seq 1 (WBS 3.0 r4's pre-baseline correction;
        # r3.2, HF-26). With no complete record but a quarantined tail, a previous run did start
        # and crashed, so it did not end cleanly.
        if scan.records == 0:
            ended_cleanly = False if quarantine else None
        else:
            ended_cleanly = scan.last_event_type == "DAEMON_STOP"
        # PD-70: how long this daemon has already been blind, carried across the restart, and on
        # which clock (r4.5, HF-34).
        # r4.11 (R-4): the same function `status` uses (semantics.py), fed the same verified records.
        inherited, clock_basis = semantics.blindness(evidence, _now(boot_id), blind_limit)
        start_boottime_ms = _boottime_ms()
        recent.append(writer.append("DAEMON_START", {
            "skeleton_version": VERSION,
            "qualified": expect is not None,
            "manifest_sha256": m.sha256,
            "daemon_code_sha256": code_sha,
            "tools": tool_versions,
            "interval_ms": int(interval * 1000),
            "cycle_budget_ms": int(cycle_budget * 1000),
            "jitter_max_ms": int(jitter_bound * 1000),  # ledger values are integers; see canonical.py
            "landlock": landlock_info,
            "previous_run_ended_cleanly": ended_cleanly,
            "last_accepted_utc": evidence.last_accepted_utc if scan.records else None,
            "inherited_blind_ms": int(inherited * 1000),
            "clock_basis": clock_basis,
            "boot_id": boot_id,
            "boottime_ms": start_boottime_ms,
            "blind_since_boottime_ms": start_boottime_ms - int(inherited * 1000),
            "test_overrides": overrides,
        }))
        if quarantine:
            recent.append(writer.append("LEDGER_TAIL_QUARANTINED", {
                "file": quarantine.file, "length": quarantine.length, "sha256": quarantine.sha256}))
        if foreign:
            recent.append(writer.append("DAEMON_ERROR", {
                "category": "OUTPUT_DIR_FOREIGN_FILES", "count": len(foreign),
                "names": [n[:FOREIGN_NAME_CHARS] for n in foreign[:FOREIGN_NAMES_MAX]]}))
        cpu_at_start = _cpu_us()
        notify_mod.notify("READY=1")
        notify_mod.notify(f"STATUS=observing; ledger seq {writer.tail.seq}")

        prev, streak, cycles = None, None, 0
        blind = _Blind(time.monotonic() - inherited)
        blind.last_accepted_utc = evidence.last_accepted_utc if scan.records else None
        heartbeat_every = blind_limit / 2
        next_heartbeat = time.monotonic() + heartbeat_every
        since_heartbeat = {"accepted": 0, "unsettled": 0, "failed": 0}
        violations_seen = guard.VIOLATIONS["count"]
        while not stop.requested:
            if max_cycles is not None and cycles >= max_cycles:
                stop.reason = "max_cycles"
                break
            cycles += 1
            stage = "sense"
            error = unsettled = None
            prepared = snapshot = None
            # r4.11 (R-6): one monotonic deadline per cycle, set here. Every ctx call gets what is
            # left of it, and an alarm interrupts sense() and decide() at the deadline, including
            # pure-Python loops and blocking reads the kernel lets a signal interrupt. The alarm is
            # off while the ledger is written, so a commit is never cut short.
            deadline = time.monotonic() + cycle_budget
            ctx._begin_cycle(deadline)
            try:
                try:
                    _alarm(cycle_budget)
                    sensed = module.sense(ctx)
                    if isinstance(sensed, Unsettled):
                        unsettled = sensed.reason        # abandoned before decide(): no event, no error
                    else:
                        snapshot = to_json_value(sensed)
                        stage = "decide"
                        events = module.decide(prev, snapshot)
                        stage = "validate"
                        prepared = writer.prepare(_validate_events(events, m.ledger.event_types))
                finally:
                    _alarm(0)
                if time.monotonic() > deadline:
                    # The daemon's own code caught the alarm: the cycle still overran.
                    raise CycleBudgetExceeded("the cycle overran its budget")
            except guard.PolicyViolation as e:
                error = ("POLICY_VIOLATION", e)
            except CycleBudgetExceeded as e:
                error, unsettled = ("CYCLE_BUDGET_EXCEEDED", e), None
            except (CanonicalError, ledger.RecordTooLarge, _EventError) as e:
                error = ("SNAPSHOT_INVALID" if stage == "sense" else "EVENT_INVALID", e)
            except BaseException as e:  # noqa: BLE001 - daemon code may fail, even with SystemExit
                # or KeyboardInterrupt raised by hand; the skeleton records it and carries on.
                # A stop request arrives as a flag (see _Stop), never as an exception.
                error = (f"{stage.upper()}_FAILED", e)

            if not error and not unsettled:
                for record in writer.commit(prepared):
                    recent.append(record)
                prev = snapshot
                if streak:
                    recent.append(_cleared(writer, streak, "success"))
                    streak = None
                left = deadline - time.monotonic()
                if m.digest.enabled and hasattr(module, "digest") and left > 0:
                    try:
                        try:
                            _alarm(left)
                            _refresh_digest(module, m, out, writer, snapshot, recent)
                        finally:
                            _alarm(0)
                    except CycleBudgetExceeded:
                        # The cycle's events are committed; only the view is late. The previous
                        # digest stays, with its older stamp.
                        log("digest not refreshed: the cycle budget ran out")
            ctx._end_cycle()

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

            now = time.monotonic()
            if error or unsettled:
                blind.miss(unsettled or error[0], "unsettled" if unsettled else "error")
                since_heartbeat["unsettled" if unsettled else "failed"] += 1
            else:
                blind.accept(now)
                since_heartbeat["accepted"] += 1
            # Checked after the cycle's outcome: a restart that inherited more than the limit
            # gets this one cycle to reacquire (PD-70).
            if blind.seconds(now) >= blind_limit:
                recent.append(writer.append("DAEMON_ERROR", {
                    "category": "SENSE_BLIND", "blind_ms": int(blind.seconds(now) * 1000),
                    "limit_ms": int(blind_limit * 1000), "unsettled_cycles": blind.unsettled,
                    "failed_cycles": blind.failed, "last_cause": blind.last_cause,
                    "last_cause_kind": blind.last_kind, "inherited_ms": int(inherited * 1000),
                    "clock_basis": clock_basis,
                    "last_accepted_utc": blind.last_accepted_utc}))
                log(f"no accepted cycle for {int(blind.seconds(now))} s (limit {blind_limit:g} s, "
                    f"last cause {blind.last_cause}); stopping, needs a human")
                notify_mod.notify("STOPPING=1")
                return EXIT_SENSE_BLIND
            if now >= next_heartbeat:
                recent.append(writer.append("DAEMON_HEARTBEAT", {
                    "mode": "blind" if (error or unsettled) else "observing",
                    "last_accepted_utc": blind.last_accepted_utc,
                    "blind_ms": int(blind.seconds(now) * 1000),
                    **_boot_stamp(boot_id, blind.seconds(now)),
                    "accepted_cycles": since_heartbeat["accepted"],
                    "unsettled_cycles": since_heartbeat["unsettled"],
                    "failed_cycles": since_heartbeat["failed"]}))
                since_heartbeat = {"accepted": 0, "unsettled": 0, "failed": 0}
                next_heartbeat = now + heartbeat_every

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
            "max_rss_kb": resource.getrusage(resource.RUSAGE_SELF).ru_maxrss,
            "last_accepted_utc": blind.last_accepted_utc,
            **_boot_stamp(boot_id, blind.seconds(time.monotonic()))})
        notify_mod.notify("STOPPING=1")
        return EXIT_OK
    except ledger.UncertainCommit as e:
        log(f"uncertain ledger commit ({e}); exiting so recovery can decide from disk")
        return EXIT_UNCERTAIN_COMMIT
    finally:
        writer.close()


def _over_budget(_signum, _frame):
    raise CycleBudgetExceeded("the cycle reached its budget")


def _alarm(seconds: float) -> None:
    """Arm (seconds > 0) or disarm (0) the cycle's alarm."""
    signal.setitimer(signal.ITIMER_REAL, max(0.0, seconds))


def _cpu_us() -> int:
    """CPU of this process plus every command it ran (ctx.run, ctx.git). r2 counted only
    RUSAGE_SELF, so a daemon that did its work in child processes looked free to DB-14."""
    total = 0.0
    for who in (resource.RUSAGE_SELF, resource.RUSAGE_CHILDREN):
        usage = resource.getrusage(who)
        total += usage.ru_utime + usage.ru_stime
    return int(total * 1_000_000)


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


class _Blind:
    """Time since the last accepted cycle, and what has happened since (r3.5)."""

    def __init__(self, now):
        self.accept(now)

    def accept(self, now):
        self.since, self.unsettled, self.failed = now, 0, 0
        self.last_cause = self.last_kind = None
        self.last_accepted_utc = utc_now()

    def miss(self, cause, kind):
        if kind == "unsettled":
            self.unsettled += 1
        else:
            self.failed += 1
        self.last_cause, self.last_kind = cause, kind

    def seconds(self, now):
        return now - self.since


def _boot_id():
    """The kernel's identifier for this boot, or None where it cannot be read."""
    try:
        with open("/proc/sys/kernel/random/boot_id") as fh:
            value = fh.read(64).strip()
    except OSError:
        return None
    return value if len(value) == 36 else None


def _boottime_ms() -> int:
    """Milliseconds since boot on CLOCK_BOOTTIME: shared by every process in this boot, counts
    suspend, and no wall-clock change moves it."""
    return int(time.clock_gettime(time.CLOCK_BOOTTIME) * 1000)


def _boot_stamp(boot_id, blind_seconds) -> dict:
    now_ms = _boottime_ms()
    return {"boot_id": boot_id, "boottime_ms": now_ms,
            "blind_since_boottime_ms": now_ms - int(blind_seconds * 1000)}


def _now(boot_id):
    """The clock readings the shared interpretation takes (semantics.Now)."""
    return semantics.Now(boot_id, _boottime_ms(), datetime.datetime.now(datetime.timezone.utc))


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
    except BaseException as e:  # noqa: BLE001 - includes PolicyViolation and a hand-raised SystemExit;
        # the violation counter catches the former
        log(f"digest not refreshed ({_exception_name(e)})")
