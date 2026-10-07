"""The conformance battery: the run checks of the one judge (judge.py), and its battery profile.

It runs the real daemon, in child processes, inside a disposable workspace with its own HOME,
so it never touches your real output directory, ~/spark-core or ~/spark-governance.

Each check ends PASS, FAIL, UNKNOWN (the evidence could not be gathered, for example strace
is missing) or N/A (does not apply to this daemon). Missing evidence is never a pass:
RESULT is PASS only when nothing FAILs and nothing is UNKNOWN; otherwise FAIL, or INCOMPLETE.

 DB-01 manifest validates (closed schema)
 DB-02 daemon code passes the static purity check
 DB-03 confinement: nothing written outside the output directory; no audit-hook violations
 DB-04 ledger integrity and provenance after a normal run
 DB-05 repeated SIGKILL at random points: chain verifies, every restart records the unclean stop
 DB-06 torn tails (garbage, and a complete record without its newline) are quarantined, not kept
 DB-07 a corrupted middle record makes the daemon refuse to start, changing nothing
 DB-08 an fsync failure exits at once (70) and a restart recovers           (needs strace)
 DB-09 a disk-full write failure exits at once (70) and a restart recovers  (needs strace)
 DB-10 the daemon's digest stays structure-safe under hostile values
 DB-11 systemd notify protocol: READY after DAEMON_START, WATCHDOG pings, STOPPING
 DB-12 a second instance is refused; SIGTERM stops cleanly with DAEMON_STOP
 DB-13 the audit hook blocks every forbidden operation for this manifest
 DB-14 resource budget: projected CPU and peak memory within the manifest
 DB-15 generated unit carries every required directive; systemd-analyze exposure under threshold
 DB-16 verification is read-only and repeatable
 DB-17 Landlock still blocks a forbidden operation with the audit hook in record-only mode (r2)
 DB-18 DAEMON_START.landlock is present and matches this host's ABI and the manifest's gaps (r2)
 DB-20 the daemon observes: accepted cycles, and no SENSE_BLIND within a short blind limit (r4.5)
 DB-22 a second, independent ledger verifier agrees, on the daemon's ledger and damaged copies (r4.11)

DB-24 the candidate envelope matches the files                (static; r4.11, judge.py)
DB-25 the generated unit: every required directive, and timings derived from the cycle budget (static; r4.11)

DB-19 is reserved for the direct cgroup memory reading (PD-76; the v0.1 line's registry), and
DB-21 and DB-23 for the planned budget table and worst-case fixtures. A
check whose meaning changes gets a new ID instead of an edit (PD-78). The list, the profiles
and the verdict rule live in judge.py; this module implements the checks that run the daemon.
"""

import json
import os
import re
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass

from . import (BATTERY_SCHEMA, EXIT_USAGE, LEDGER_NAME, SYSTEM_PATH,
               QUARANTINE_DIR, RESERVED_EVENT_TYPES, VERSION, guard, landlock, ledger,
               manifest as manifest_mod, unitgen)
from .canonical import sha256_hex, strict_loads

PATTERN_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ENTRY = os.path.join(PATTERN_ROOT, "bin", "spark-daemon")
TEST_INTERVAL_MS = 200


@dataclass
class Proc:
    code: int
    stdout: str
    stderr: str
    cpu_s: float
    maxrss_kb: int
    wall_s: float


class Workspace:
    def __init__(self, manifest_path: str, workdir: str | None):
        self.root = os.path.realpath(workdir or tempfile.mkdtemp(prefix="spark-battery-"))
        os.makedirs(self.root, exist_ok=True)
        self.home = os.path.join(self.root, "home")
        self.daemon_dir = os.path.join(self.root, "daemon")
        for d in (self.home, self.daemon_dir):
            os.makedirs(d, mode=0o700, exist_ok=True)
        src_dir = os.path.dirname(os.path.abspath(manifest_path))
        shutil.copyfile(os.path.join(src_dir, "daemon.py"), os.path.join(self.daemon_dir, "daemon.py"))
        fixture = os.path.join(src_dir, "fixture_home")
        if os.path.isdir(fixture):
            shutil.copytree(fixture, self.home, dirs_exist_ok=True)
        data = manifest_mod.to_dict(manifest_path)
        self.rewrites = []
        if isinstance(data.get("output_dir"), str) and not data["output_dir"].startswith("~"):
            new = f"~/.battery-output/{data.get('name', 'daemon')}"
            self.rewrites.append(f"output_dir {data['output_dir']} -> {new}")
            data["output_dir"] = new
        self.manifest = os.path.join(self.daemon_dir, "manifest.json")
        manifest_mod.dump(data, self.manifest)
        os.environ["SPARK_DAEMON_HOME"] = self.home       # this process expands "~" the same way
        self.canary = os.path.join(self.home, "spark-core", "data", "canary.txt")
        os.makedirs(os.path.dirname(self.canary), exist_ok=True)
        with open(self.canary, "w") as fh:
            fh.write("canary: must never be read\n")
        self.m = None
        self.output = None
        self.ledger = None

    def bind_manifest(self, m):
        from .paths import expand
        self.m = m
        self.output = expand(m.output_dir)
        self.ledger = os.path.join(self.output, LEDGER_NAME)

    def env(self, **extra):
        env = {"HOME": self.home, "SPARK_DAEMON_HOME": self.home, "PATH": SYSTEM_PATH,
               "LANG": "C.UTF-8", "SPARK_DAEMON_TEST": "1",
               "SPARK_DAEMON_TEST_INTERVAL_MS": str(TEST_INTERVAL_MS), "PYTHONDONTWRITEBYTECODE": "1"}
        env.update({k: v for k, v in extra.items() if v is not None})
        return env

    def cmd(self, *args):
        return [sys.executable, "-I", "-B", ENTRY, *args]

    def run_cmd(self, *args, cycles=None):
        extra = ["--max-cycles", str(cycles)] if cycles is not None else []
        return self.cmd("run", "--manifest", self.manifest, *extra)

    def reset_output(self):
        if self.output and os.path.exists(self.output):
            shutil.rmtree(self.output)


def run_proc(cmd, env, *, timeout=60.0, kill_after=None, term_after=None, prefix=None) -> Proc:
    cmd = (prefix or []) + cmd
    with tempfile.TemporaryFile() as out, tempfile.TemporaryFile() as err:
        start = time.monotonic()
        p = subprocess.Popen(cmd, env=env, stdin=subprocess.DEVNULL, stdout=out, stderr=err,
                             start_new_session=True)
        if kill_after is not None:
            time.sleep(kill_after)
            _signal(p.pid, signal.SIGKILL)
        elif term_after is not None:
            time.sleep(term_after)
            _signal(p.pid, signal.SIGTERM)
        deadline = start + timeout
        while True:
            pid, status, usage = os.wait4(p.pid, os.WNOHANG)
            if pid:
                break
            if time.monotonic() > deadline:
                _signal(p.pid, signal.SIGKILL)
                pid, status, usage = os.wait4(p.pid, 0)
                break
            time.sleep(0.02)
        p.returncode = os.waitstatus_to_exitcode(status)
        wall = time.monotonic() - start
        out.seek(0)
        err.seek(0)
        return Proc(p.returncode, out.read().decode("utf-8", "replace"), err.read().decode("utf-8", "replace"),
                    usage.ru_utime + usage.ru_stime, usage.ru_maxrss, wall)


def _signal(pid, sig):
    try:
        os.kill(pid, sig)
    except ProcessLookupError:
        pass


def _snapshot(root, exclude):
    state = {}
    for base, dirs, files in os.walk(root):
        if any(base == e or base.startswith(e + "/") for e in exclude):
            dirs[:] = []
            continue
        for name in files + [d for d in dirs if os.path.islink(os.path.join(base, d))]:
            path = os.path.join(base, name)
            try:
                with open(path, "rb") as fh:
                    digest = sha256_hex(fh.read())
            except OSError:
                digest = "unreadable"
            st = os.lstat(path)
            state[os.path.relpath(path, root)] = (digest, st.st_mode, st.st_size)
    return state


def _records(path):
    out = []
    if not os.path.exists(path):
        return out
    with open(path, "rb") as fh:
        for line in fh:
            if line.endswith(b"\n"):
                out.append(strict_loads(line[:-1]))
    return out


def _verify(ws):
    try:
        result = ledger.verify_file(ws.ledger, ws.m.name, ws.m.ledger.record_max_bytes)
    except ledger.LedgerCorrupt as e:
        return None, str(e)
    except FileNotFoundError:
        return None, "no ledger"
    if result.torn:
        return None, f"torn tail of {result.torn.length} bytes left in place"
    return result, ""


def db03(ws, c, cycles=5):
    ws.reset_output()
    exclude = [ws.output]
    before = _snapshot(ws.home, exclude) | {f"daemon/{k}": v for k, v in _snapshot(ws.daemon_dir, []).items()}
    p = run_proc(ws.run_cmd(cycles=cycles), ws.env(SPARK_DAEMON_AUDIT="record"))
    after = _snapshot(ws.home, exclude) | {f"daemon/{k}": v for k, v in _snapshot(ws.daemon_dir, []).items()}
    audits = [line for line in p.stderr.splitlines() if "spark_daemon_audit" in line]
    changed = sorted(k for k in set(before) | set(after) if before.get(k) != after.get(k))
    problems = []
    if p.code != 0:
        problems.append(f"exit {p.code}")
    if audits:
        problems.append(f"{len(audits)} audit violation(s), first: {audits[0][:160]}")
    if changed:
        problems.append(f"changed outside output dir: {', '.join(changed[:5])}")
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or f"{cycles} cycles; 0 audit events; no file outside the output directory changed"


def db04(ws, c):
    result, err = _verify(ws)
    if result is None:
        c.state, c.evidence = "FAIL", err
        return
    recs = _records(ws.ledger)
    declared = set(ws.m.ledger.event_types) | set(RESERVED_EVENT_TYPES)
    with open(os.path.join(ws.daemon_dir, "daemon.py"), "rb") as fh:
        code_sha = sha256_hex(fh.read())
    problems = []
    if not recs or recs[0]["event_type"] != "DAEMON_START":
        problems.append("first record is not DAEMON_START")
    if not recs or recs[-1]["event_type"] != "DAEMON_STOP":
        problems.append("last record is not DAEMON_STOP")
    undeclared = sorted({r["event_type"] for r in recs} - declared)
    if undeclared:
        problems.append(f"undeclared event types {undeclared}")
    # r3: a daemon whose every cycle fails still exits 0 and writes a valid chain; r2 passed
    # it. A clean run in the battery's own workspace must not record a single error.
    errors = [r["payload"] for r in recs if r["event_type"] == "DAEMON_ERROR"]
    if errors:
        problems.append(f"{len(errors)} DAEMON_ERROR record(s) in a clean run, first: "
                        f"{json.dumps(errors[0], sort_keys=True)[:120]}")
    start = recs[0]["payload"] if recs else {}
    if start.get("manifest_sha256") != ws.m.sha256:
        problems.append("DAEMON_START manifest_sha256 does not match")
    if start.get("daemon_code_sha256") != code_sha:
        problems.append("DAEMON_START daemon_code_sha256 does not match")
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or f"{result.records} records; chain head {result.tail.head[:16]}; provenance matches"


def db05(ws, c, rng, kills):
    ws.reset_output()
    delays = [round(rng.uniform(0.05, 0.9), 3) for _ in range(kills)]
    for delay in delays:
        run_proc(ws.run_cmd(), ws.env(), kill_after=delay)
    final = run_proc(ws.run_cmd(cycles=2), ws.env())
    result, err = _verify(ws)
    problems = []
    if final.code != 0:
        problems.append(f"final clean run exited {final.code}")
    if result is None:
        problems.append(err)
    recs = _records(ws.ledger)
    starts = [r for r in recs if r["event_type"] == "DAEMON_START"]
    flags = [r["payload"]["previous_run_ended_cleanly"] for r in starts]
    if flags and flags[0] is not None:
        problems.append("first DAEMON_START should have previous_run_ended_cleanly=null")
    if any(f is not False for f in flags[1:]):
        problems.append(f"a restart after SIGKILL did not record an unclean stop: {flags}")
    qfiles = sorted(os.listdir(os.path.join(ws.output, QUARANTINE_DIR)))
    qrecs = sorted(r["payload"]["file"] for r in recs if r["event_type"] == "LEDGER_TAIL_QUARANTINED")
    if qfiles != qrecs:
        problems.append("quarantine files and LEDGER_TAIL_QUARANTINED records do not match")
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or (
        f"{kills} kills at {delays} s; {len(starts)} starts recorded, all after the first flagged unclean; "
        f"{len(qfiles)} torn tail(s) quarantined; chain verifies")


def db06(ws, c):
    problems = []
    for label, fragment in (("garbage", b'{"partial": tr'), ("complete record without newline", None)):
        with open(ws.ledger, "rb") as fh:
            data = fh.read()
        if fragment is None:
            fragment = data.rstrip(b"\n").rsplit(b"\n", 1)[-1]   # a valid-looking, complete line
        with open(ws.ledger, "ab") as fh:
            fh.write(fragment)
        before_q = set(os.listdir(os.path.join(ws.output, QUARANTINE_DIR)))
        p = run_proc(ws.run_cmd(cycles=1), ws.env())
        after_q = set(os.listdir(os.path.join(ws.output, QUARANTINE_DIR)))
        new = sorted(after_q - before_q)
        if p.code != 0:
            problems.append(f"{label}: exit {p.code}")
            continue
        if len(new) != 1:
            problems.append(f"{label}: expected one new quarantine file, found {len(new)}")
            continue
        with open(os.path.join(ws.output, QUARANTINE_DIR, new[0]), "rb") as fh:
            if fh.read() != fragment:
                problems.append(f"{label}: quarantined bytes differ from the torn tail")
        recs = _records(ws.ledger)
        if not any(r["event_type"] == "LEDGER_TAIL_QUARANTINED" and r["payload"]["file"] == new[0]
                   and r["payload"]["sha256"] == sha256_hex(fragment) for r in recs):
            problems.append(f"{label}: no matching LEDGER_TAIL_QUARANTINED record")
        result, err = _verify(ws)
        if result is None:
            problems.append(f"{label}: {err}")
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or "both torn tails quarantined byte-for-byte, recorded, chain verifies"


def db07(ws, c):
    with open(ws.ledger, "rb") as fh:
        original = fh.read()
    lines = original.split(b"\n")
    if len(lines) < 4:
        c.state, c.evidence = "UNKNOWN", "ledger too short to corrupt a middle record"
        return
    target = len(lines) // 2
    line = bytearray(lines[target])
    pos = line.find(b'"prev_sha256":"') + len(b'"prev_sha256":"')
    line[pos] = ord("0") if line[pos] != ord("0") else ord("1")
    lines[target] = bytes(line)
    corrupted = b"\n".join(lines)
    with open(ws.ledger, "wb") as fh:
        fh.write(corrupted)
    q_before = sorted(os.listdir(os.path.join(ws.output, QUARANTINE_DIR)))
    p = run_proc(ws.run_cmd(cycles=1), ws.env())
    with open(ws.ledger, "rb") as fh:
        after = fh.read()
    q_after = sorted(os.listdir(os.path.join(ws.output, QUARANTINE_DIR)))
    problems = []
    if p.code != 65:
        problems.append(f"expected exit 65, got {p.code}")
    if after != corrupted:
        problems.append("ledger bytes changed")
    if q_after != q_before:
        problems.append("something was quarantined")
    with open(ws.ledger, "wb") as fh:                   # restore for the checks that follow
        fh.write(original)
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or f"flipped one byte in record {target + 1}: refused with exit 65, nothing changed"


def _strace():
    return shutil.which("strace", path=SYSTEM_PATH)


def _fault(ws, c, inject_args, label):
    strace = _strace()
    if not strace:
        c.state, c.evidence = "UNKNOWN", "strace not installed; fault injection not possible"
        return
    lines_before = len(_records(ws.ledger))
    p = run_proc(ws.run_cmd(cycles=2), ws.env(),
                 prefix=[strace, "-f", "-qq", "-o", "/dev/null", *inject_args])
    if p.code != 70:
        tooling = "strace:" in p.stderr and ("ptrace" in p.stderr or "not permitted" in p.stderr)
        c.state = "UNKNOWN" if tooling else "FAIL"
        c.evidence = f"under injected {label} expected exit 70, got {p.code} ({p.stderr.strip()[:120]})"
        return
    again = run_proc(ws.run_cmd(cycles=1), ws.env())
    result, err = _verify(ws)
    problems = []
    if again.code != 0:
        problems.append(f"restart exited {again.code}")
    if result is None:
        problems.append(err)
    c.state = "FAIL" if problems else "PASS"
    grew = len(_records(ws.ledger)) - lines_before
    c.evidence = "; ".join(problems) or f"exit 70 under injected {label}; restart recovered; chain verifies (+{grew} records)"


def db10(ws, c, seed):
    p = run_proc(ws.cmd("probe-digest", "--manifest", ws.manifest, "--seed", str(seed)), ws.env())
    try:
        result = json.loads(p.stdout.strip().splitlines()[-1])
    except (ValueError, IndexError):
        c.state, c.evidence = "FAIL", f"probe failed (exit {p.code})"
        return
    status = result.get("status")
    c.state = {"PASS": "PASS", "N/A": "N/A"}.get(status, "FAIL")
    c.evidence = (f"{result.get('trials')} hostile renderings of this daemon's digest, seed {seed}; structure unchanged"
                  if c.state == "PASS" else json.dumps(result)[:200])


def db11(ws, c):
    sock_path = os.path.join(ws.root, "notify.sock")
    if os.path.exists(sock_path):
        os.remove(sock_path)
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
    sock.bind(sock_path)
    sock.settimeout(0.2)
    messages, done = [], threading.Event()
    starts_before = sum(1 for r in _records(ws.ledger) if r["event_type"] == "DAEMON_START")

    def collect():
        while not done.is_set():
            try:
                data = sock.recv(4096).decode("ascii", "replace")
            except socket.timeout:
                continue
            ledger_starts = None
            if data.startswith("READY=1"):
                ledger_starts = sum(1 for r in _records(ws.ledger) if r["event_type"] == "DAEMON_START")
            messages.append((time.monotonic(), data, ledger_starts))

    t = threading.Thread(target=collect, daemon=True)
    t.start()
    p = run_proc(ws.run_cmd(cycles=6), ws.env(NOTIFY_SOCKET=sock_path))
    time.sleep(0.3)
    done.set()
    t.join()
    sock.close()
    problems = []
    kinds = [m[1] for m in messages]
    ready = [m for m in messages if m[1] == "READY=1"]
    if p.code != 0:
        problems.append(f"exit {p.code}")
    if not ready:
        problems.append("no READY=1")
    elif ready[0][2] != starts_before + 1:
        problems.append("READY=1 arrived before DAEMON_START was committed")
    pings = [m[0] for m in messages if m[1] == "WATCHDOG=1"]
    if len(pings) < 6:
        problems.append(f"only {len(pings)} WATCHDOG=1 pings in 6 cycles")
    gaps = [b - a for a, b in zip(pings, pings[1:])]
    worst = max(gaps) if gaps else 0.0
    if worst > ws.m.watchdog_seconds / 2:
        problems.append(f"longest gap between pings {worst:.2f} s exceeds half of WatchdogSec")
    if "STOPPING=1" not in kinds:
        problems.append("no STOPPING=1")
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or (
        f"READY after DAEMON_START; {len(pings)} pings, longest gap {worst:.2f} s; STOPPING at exit")


def db12(ws, c):
    first = subprocess.Popen(ws.run_cmd(), env=ws.env(), stdin=subprocess.DEVNULL,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, start_new_session=True)
    try:
        time.sleep(0.8)
        second = run_proc(ws.run_cmd(cycles=1), ws.env(), timeout=15)
        first.send_signal(signal.SIGTERM)
        try:
            code = first.wait(timeout=15)
        except subprocess.TimeoutExpired:
            first.kill()
            code = first.wait()
    finally:
        if first.poll() is None:
            first.kill()
            first.wait()
    recs = _records(ws.ledger)
    problems = []
    if second.code != 73:
        problems.append(f"second instance exited {second.code}, expected 73")
    if code != 0:
        problems.append(f"SIGTERM exit {code}, expected 0")
    if not recs or recs[-1]["event_type"] != "DAEMON_STOP" or "SIGTERM" not in recs[-1]["payload"].get("reason", ""):
        problems.append("last record is not DAEMON_STOP for SIGTERM")
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or "second instance refused (73); SIGTERM gave DAEMON_STOP and exit 0"


def db13(ws, c):
    p = run_proc(ws.cmd("probe-policy", "--manifest", ws.manifest, "--canary", ws.canary), ws.env())
    try:
        result = json.loads(p.stdout.strip().splitlines()[-1])
    except (ValueError, IndexError):
        c.state, c.evidence = "FAIL", f"probe failed (exit {p.code})"
        return
    if result.get("ok"):
        n = sum(1 for v in result["results"].values() if v == "blocked")
        c.state, c.evidence = "PASS", f"{n} forbidden operations blocked and counted; output-dir write allowed"
    else:
        wrong = {k: v for k, v in result.get("results", {}).items() if result.get("expected", {}).get(k) != v}
        c.state, c.evidence = "FAIL", f"unexpected: {json.dumps(wrong)[:200]}"


def db14(ws, c):
    cycles = 21
    p = run_proc(ws.run_cmd(cycles=cycles), ws.env())
    recs = _records(ws.ledger)
    if p.code != 0 or not recs or recs[-1]["event_type"] != "DAEMON_STOP":
        c.state, c.evidence = "FAIL", f"run exited {p.code}"
        return
    stop = recs[-1]["payload"]
    per_cycle_us = stop["cpu_us_since_ready"] / cycles
    projected_bp = per_cycle_us / 1_000_000 / ws.m.trigger.interval_seconds * 10000
    rss_kb = max(p.maxrss_kb, stop["max_rss_kb"])
    problems = []
    if projected_bp > ws.m.resources.cpu_budget_bp:
        problems.append(f"projected CPU {projected_bp / 100:.3f}% > budget {ws.m.resources.cpu_budget_bp / 100:.3f}%")
    if rss_kb > ws.m.resources.memory_max_mb * 1024:
        problems.append(f"peak RSS {rss_kb // 1024} MiB > {ws.m.resources.memory_max_mb} MiB")
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or (
        f"{per_cycle_us / 1000:.2f} ms CPU per cycle over {cycles} cycles, projected {projected_bp / 100:.3f}% "
        f"at a {ws.m.trigger.interval_seconds} s interval (budget {ws.m.resources.cpu_budget_bp / 100:.2f}%); "
        f"peak RSS {rss_kb / 1024:.1f} MiB (limit {ws.m.resources.memory_max_mb}); "
        f"whole process incl. start-up {p.cpu_s * 1000:.0f} ms CPU")


def db20(ws, c, cycles=12, limit_intervals=5):
    """r4.5 (HF-35): the daemon observes. A daemon whose every cycle is unsettled records no
    error, so DB-04 and every other check passed it: the battery gave PASS to a daemon that never
    saw anything. Runs the daemon with a blind limit of a few test intervals; it must accept
    cycles and must not stop with SENSE_BLIND."""
    ws.reset_output()
    limit_ms = limit_intervals * TEST_INTERVAL_MS
    p = run_proc(ws.run_cmd(cycles=cycles), ws.env(SPARK_DAEMON_TEST_BLIND_LIMIT_MS=str(limit_ms)))
    recs = _records(ws.ledger)
    blind = [r["payload"] for r in recs
             if r["event_type"] == "DAEMON_ERROR" and r["payload"].get("category") == "SENSE_BLIND"]
    beats = [r["payload"] for r in recs if r["event_type"] == "DAEMON_HEARTBEAT"]
    accepted = sum(b.get("accepted_cycles", 0) for b in beats)
    missed = sum(b.get("unsettled_cycles", 0) + b.get("failed_cycles", 0) for b in beats)
    if blind:
        b = blind[0]
        c.state = "FAIL"
        c.evidence = (f"SENSE_BLIND after {b.get('blind_ms')} ms against a {limit_ms} ms limit: no accepted "
                      f"cycle; last cause {b.get('last_cause')} ({b.get('last_cause_kind')})")
    elif p.code != 0:
        c.state, c.evidence = "FAIL", f"run exited {p.code}"
    elif not beats:
        c.state, c.evidence = "UNKNOWN", f"no DAEMON_HEARTBEAT in {cycles} cycles to count accepted cycles"
    elif accepted == 0:
        c.state, c.evidence = "FAIL", f"{len(beats)} heartbeat(s) and no accepted cycle"
    else:
        c.state = "PASS"
        c.evidence = (f"{accepted} accepted and {missed} unsettled or failed cycle(s) across {len(beats)} "
                      f"heartbeat(s); no SENSE_BLIND within a {limit_ms} ms limit")


def corrupted_copies(lines):
    """Damaged versions of a verified ledger (a list of lines with their newlines), named, with
    the (category, seq) both verifiers must report. Needs at least two lines."""
    first, second = lines[0], lines[1]
    out = []
    at = first.find(b'"run_id":"') + len(b'"run_id":"')
    if at > len(b'"run_id":"'):
        digit = b"0" if first[at:at + 1] != b"0" else b"1"
        out.append(("an earlier record changed", [first[:at] + digit + first[at + 1:]] + lines[1:], ("LINK", 2)))
    comma = second.find(b",")
    out.append(("a record re-encoded", [first, second[:comma + 1] + b" " + second[comma + 1:]] + lines[2:],
                ("NOT_CANONICAL", 2)))
    if len(lines) > 2:
        out.append(("a record removed", [first] + lines[2:], ("SEQUENCE", 2)))
    out.append(("seq written as true", [first.replace(b'"seq":1,', b'"seq":true,', 1)] + lines[1:],
                ("SEQUENCE", 1)))
    out.append(("an unparseable line", [first, b"{not json\n"] + lines[2:], ("UNPARSEABLE", 2)))
    return out


def db22(ws, c):
    """r4.11 (R-4; B-6, U-7): the ledger, verified by a second implementation. The skeleton's
    verifier and verifier/ledger_verify.py, which shares no code with it and encodes RFC 8785 by
    hand, must agree on this daemon's own ledger and on damaged copies of it. Writing that second
    verifier found HF-38 (a boolean seq verified as 1)."""
    from . import status
    ws.reset_output()
    p = run_proc(ws.run_cmd(cycles=4), ws.env())
    if p.code != 0:
        c.state, c.evidence = "FAIL", f"run exited {p.code}"
        return
    name, limit = ws.m.name, ws.m.ledger.record_max_bytes
    problems = [f"own ledger: {d}" for d in status.cross_check(ws.ledger, name, limit)]
    clean = status.primary_outcome(ws.ledger, name, limit)
    with open(ws.ledger, "rb") as fh:
        lines = fh.readlines()
    if len(lines) < 2:
        c.state, c.evidence = "UNKNOWN", f"only {len(lines)} record(s): nothing to damage"
        return
    copies = corrupted_copies(lines)
    for i, (label, damaged, expected) in enumerate(copies):
        path = os.path.join(ws.root, f"db22-{i}.jsonl")
        with open(path, "wb") as fh:
            fh.write(b"".join(damaged))
        problems += [f"{label}: {d}" for d in status.cross_check(path, name, limit)]
        got = status.primary_outcome(path, name, limit)
        found = (got.get("break") or {}).get("category"), (got.get("break") or {}).get("seq")
        if found != expected:
            problems.append(f"{label}: expected {expected[0]} at seq {expected[1]}, got {found}")
    if problems:
        c.state, c.evidence = "FAIL", "; ".join(problems[:4])
    else:
        c.state = "PASS"
        c.evidence = (f"both verifiers agree: own ledger intact to seq {clean['head_seq']} (head "
                      f"{clean['head_sha256'][:16]}), and the same break on {len(copies)} damaged copies")


def db15(ws, c, threshold, unit_options=None):
    text = unitgen.generate(ws.m, root=PATTERN_ROOT, **(unit_options or {}))
    missing = unitgen.lint(text)
    unit_path = os.path.join(ws.root, unitgen.unit_name(ws.m))
    with open(unit_path, "w") as fh:
        fh.write(text)
    if missing:
        c.state, c.evidence = "FAIL", f"missing directives: {', '.join(missing)}"
        return
    analyze = shutil.which("systemd-analyze", path=SYSTEM_PATH)
    if not analyze:
        c.state, c.evidence = "UNKNOWN", "all required directives present; systemd-analyze not installed"
        return
    p = run_proc([analyze, "security", "--offline=true", f"--threshold={threshold}", unit_path], dict(os.environ))
    match = re.search(r"Overall exposure level for \S+: (\d+\.\d)", p.stdout + p.stderr)
    if not match:
        c.state, c.evidence = "UNKNOWN", f"systemd-analyze gave no score (exit {p.code})"
        return
    score = match.group(1)
    blocked = _startup_syscalls_blocked(analyze, text)
    if blocked is None:
        c.state = "UNKNOWN"
        c.evidence = f"exposure {score}; could not expand the unit's SystemCallFilter groups"
        return
    c.state = "PASS" if p.code == 0 and not blocked else "FAIL"
    c.evidence = (f"all required directives present; systemd-analyze offline exposure {score} "
                  f"(threshold {threshold / 10:.1f}); start-up syscalls "
                  + (f"BLOCKED by the unit's seccomp filter: {', '.join(blocked)}" if blocked
                     else "permitted by the unit's seccomp filter"))


def _startup_syscalls_blocked(analyze, unit_text):
    """Start-up syscalls the unit's SystemCallFilter would refuse, or None if the groups
    could not be expanded. (r3: catches the missing @sandbox/Landlock allowance.)"""
    cache = {}

    def expand_group(group):
        if group not in cache:
            env = dict(os.environ, SYSTEMD_COLORS="0", NO_COLOR="1")
            p = run_proc([analyze, "syscall-filter", group], env, timeout=30)
            if p.code != 0:
                raise LookupError(group)
            names = set()
            for line in p.stdout.splitlines():
                item = re.sub(r"\x1b\[[0-9;]*m", "", line).strip()
                if not item or item.startswith("#") or item == group:
                    continue
                names.update(expand_group(item) if item.startswith("@") else {item})
            cache[group] = names
        return cache[group]

    try:
        allowed, denied = unitgen.resolve_syscall_filter(unit_text, expand_group)
    except LookupError:
        return None
    return [s for s in unitgen.STARTUP_SYSCALLS if s not in allowed or s in denied]


def db16(ws, c):
    with open(ws.ledger, "rb") as fh:
        before = fh.read()
    mtime = os.stat(ws.ledger).st_mtime_ns
    outs = [run_proc(ws.cmd("verify", "--manifest", ws.manifest), ws.env()) for _ in range(2)]
    with open(ws.ledger, "rb") as fh:
        after = fh.read()
    problems = []
    if any(o.code != 0 for o in outs):
        problems.append("verify failed")
    if outs[0].stdout != outs[1].stdout:
        problems.append("two verifications disagree")
    if before != after or os.stat(ws.ledger).st_mtime_ns != mtime:
        problems.append("verification changed the ledger")
    c.state = "FAIL" if problems else "PASS"
    try:
        head = json.loads(outs[0].stdout.splitlines()[0])
        summary = f"seq {head['seq']}, chain head {head['chain_head_sha256'][:16]}"
    except (ValueError, IndexError, KeyError):
        summary = "no summary"
    c.evidence = "; ".join(problems) or f"two read-only verifications agree ({summary}); bytes and mtime unchanged"


def db17(ws, c):
    p = run_proc(ws.cmd("probe-landlock", "--manifest", ws.manifest, "--canary", ws.canary), ws.env())
    try:
        result = json.loads(p.stdout.strip().splitlines()[-1])
    except (ValueError, IndexError):
        c.state, c.evidence = "FAIL", f"probe failed (exit {p.code})"
        return
    status = result.get("status")
    if status == "N/A":
        c.state, c.evidence = "N/A", result.get("reason", "Landlock unavailable")
        return
    if result.get("ok"):
        info = result.get("landlock", {})
        tcp = "; TCP connect also blocked (EACCES)" if result.get("tcp_checked") else ""
        c.state, c.evidence = "PASS", (
            f"ABI {info.get('abi')}: with the audit hook in record-only mode, the kernel alone still blocked "
            f"the forbidden write and the denied read{tcp}")
    else:
        wrong = {k: v for k, v in result.get("results", {}).items()
                if k in ("write_outside_output_dir", "read_denied_canary", "tcp_connect")}
        c.state, c.evidence = "FAIL", f"kernel did not block: {json.dumps(wrong)[:200]}"


def db18(ws, c):
    ws.reset_output()
    p = run_proc(ws.run_cmd(cycles=1), ws.env())
    recs = _records(ws.ledger)
    if p.code != 0 or not recs or recs[0]["event_type"] != "DAEMON_START":
        c.state, c.evidence = "FAIL", f"run exited {p.code}; no DAEMON_START to inspect"
        return
    info = recs[0]["payload"].get("landlock")
    if not isinstance(info, dict) or not {"abi", "status", "gaps"} <= info.keys():
        c.state, c.evidence = "FAIL", f"DAEMON_START.landlock missing or malformed: {json.dumps(info)[:200]}"
        return
    problems = []
    try:
        fresh_abi = landlock.abi_version()
    except OSError:
        fresh_abi = -1
    if info["status"] == "enforced":
        if info["abi"] != fresh_abi:
            problems.append(f"recorded ABI {info['abi']} does not match this host's ABI {fresh_abi}")
    elif info["status"] == "unavailable":
        if fresh_abi >= landlock.MIN_USABLE_ABI:
            problems.append(f"recorded unavailable, but this host now reports usable ABI {fresh_abi}")
    else:
        problems.append(f"unexpected status {info['status']!r} (expected enforced or unavailable)")
    fresh_gaps = landlock.gaps(guard.Policy(ws.m))
    if info["gaps"] != fresh_gaps:
        problems.append(f"recorded gaps {info['gaps']} do not match freshly computed gaps {fresh_gaps}")
    c.state = "FAIL" if problems else "PASS"
    c.evidence = "; ".join(problems) or (
        f"DAEMON_START.landlock: ABI {info['abi']}, status {info['status']}, {len(info['gaps'])} gap(s); "
        f"matches this host's ABI and the manifest's own gaps")


def main(manifest_path: str, *, seed: int, quick: bool, workdir: str | None, threshold: int,
         envelope: str | None = None, unit_options: dict | None = None,
         emit_unit: str | None = None) -> int:
    """The battery profile of the one judge (judge.py, r4.11): every registered check. With
    emit_unit, a qualifying PASS also writes the installable unit and its record (qualify.py)."""
    from . import contract, judge, qualify
    if not os.path.exists(manifest_path):
        print(f"no manifest at {manifest_path}")
        return EXIT_USAGE
    if emit_unit and quick:
        print("--emit-unit needs the full battery: --quick does not qualify")
        return EXIT_USAGE
    j = judge.run("battery", manifest_path, envelope=envelope, workdir=workdir, seed=seed,
                  quick=quick, threshold=threshold, unit_options=unit_options)
    if j.ws is None:
        j.ws = Workspace.__new__(Workspace)            # nothing ran: a report folder only
        j.ws.root = os.path.realpath(workdir or tempfile.mkdtemp(prefix="spark-battery-"))
        os.makedirs(j.ws.root, exist_ok=True)
        j.ws.rewrites, j.ws.m = [], None
    envelope_check = next(c for c in j.checks if c.id == "DB-24")
    candidate = None if envelope_check.state == judge.NA else {
        "summary": j.candidate, "diagnostics": envelope_check.diagnostics}
    report = {
        "schema": BATTERY_SCHEMA, "skeleton_version": VERSION, **contract.contract_identity(),
        "profile": j.profile, "qualifying": j.qualifying,
        "candidate": candidate, "result": j.result, "seed": seed,
        "quick": quick, "unit_options": j.unit_options, "host": qualify.host_facts(),
        "daemon": j.m.name if j.m else None,
        # The manifest as written, which is what a unit runs; run checks used the workspace's
        # copy, whose digest differs when an absolute output_dir was rewritten.
        "manifest_sha256": j.m.sha256 if j.m else None,
        "workspace_manifest_sha256": j.ws.m.sha256 if j.ws.m else None,
        "daemon_code_sha256": j.code_sha, "workspace": j.ws.root, "rewrites": j.ws.rewrites,
        "environment": {"python": sys.version.split()[0], "kernel": os.uname().release,
                        "machine": os.uname().machine, "strace": bool(_strace()),
                        "systemd_analyze": bool(shutil.which("systemd-analyze", path=SYSTEM_PATH))},
        "checks": [{"id": c.id, "title": c.title, "state": c.state, "evidence": c.evidence}
                   for c in j.checks],
    }
    report_path = os.path.join(j.ws.root, "battery-report.json")
    with open(report_path, "w") as fh:
        json.dump(report, fh, indent=2)
        fh.write("\n")
    width = max(len(c.title) for c in j.checks)
    for c in j.checks:
        print(f"{c.id}  {c.state:<10} {c.title:<{width}}  {c.evidence}")
    for note in j.ws.rewrites:
        print(f"note: battery rewrote {note}")
    if candidate:
        for d in candidate["diagnostics"]:
            print(f"candidate: {d['where']}: {d['message']}")
    print(f"report: {report_path}")
    if emit_unit:
        try:
            unit_path, record_path = qualify.emit(j, emit_unit, report_path)
        except qualify.Refused as e:
            print(f"installable unit: not emitted ({e})")
        else:
            print(f"installable unit: {unit_path}")
            print(f"qualification record: {record_path}")
            print("note: qualification is not activation; installing and enabling are a person's decision")
    print(f"RESULT: {j.result}")
    return judge.exit_code(j.result)
