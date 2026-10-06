"""Blind period (r3.5): ctx.unsettled(), blind_limit_seconds and SENSE_BLIND, and the unit
never restarting a fail-closed exit."""

import ast
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import unittest

from helpers import ENTRY, EXAMPLE, FIXTURES, ROOT, Sandbox
from spark_daemon import EXIT_OK, EXIT_SENSE_BLIND, context, manifest, unitgen

DECIDE = '''
def decide(prev, snapshot):
    if prev == snapshot:
        return []
    return [("VALUE_OBSERVED", {"value": snapshot["value"]})]
'''

# Unsettled while ~/data/mode.txt says "busy" (a build, say); settled otherwise.
MODE_DRIVEN = '''
def sense(ctx):
    if ctx.read_text("~/data/mode.txt").strip() == "busy":
        return ctx.unsettled("BUILD_RUNNING")
    return {"value": ctx.read_text("~/data/value.txt").strip()[:50]}
''' + DECIDE


def fixture(source):
    tmp = tempfile.mkdtemp(prefix="spark-blind-")
    shutil.copyfile(os.path.join(FIXTURES, "counter", "manifest.json"), os.path.join(tmp, "manifest.json"))
    with open(os.path.join(tmp, "daemon.py"), "w") as fh:
        fh.write(source)
    return tmp


class BlindPeriodTests(unittest.TestCase):
    def sandbox(self, source):
        self.fixture_dir = fixture(source)
        self.addCleanup(shutil.rmtree, self.fixture_dir)
        sb = Sandbox(self.fixture_dir)
        self.addCleanup(sb.cleanup)
        return sb

    def types(self, sb):
        return [r["event_type"] for r in sb.records()]

    def test_unsettled_is_neither_an_event_nor_an_error(self):
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "busy")
        sb.write("data/value.txt", "42")
        p = sb.run(cycles=4)
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        self.assertEqual(self.types(sb), ["DAEMON_START", "DAEMON_STOP"])
        sb.write("data/mode.txt", "idle")
        p = sb.run(cycles=2)
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        self.assertEqual(self.types(sb)[-3:], ["DAEMON_START", "VALUE_OBSERVED", "DAEMON_STOP"])

    def test_always_unsettled_stops_at_the_limit(self):
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "busy")
        p = sb.run(cycles=None, SPARK_DAEMON_TEST_BLIND_LIMIT_MS="600")
        self.assertEqual(p.returncode, EXIT_SENSE_BLIND, p.stderr)
        recs = sb.records()
        self.assertEqual(recs[0]["payload"]["test_overrides"]["blind_limit_ms"], 600)
        last = recs[-1]
        self.assertEqual(last["event_type"], "DAEMON_ERROR")
        payload = last["payload"]
        self.assertEqual(payload["category"], "SENSE_BLIND")
        self.assertEqual((payload["last_cause"], payload["last_cause_kind"]), ("BUILD_RUNNING", "unsettled"))
        self.assertEqual(payload["limit_ms"], 600)
        self.assertGreaterEqual(payload["blind_ms"], 600)
        self.assertGreaterEqual(payload["unsettled_cycles"], 3)
        self.assertEqual(payload["failed_cycles"], 0)
        self.assertNotIn("DAEMON_STOP", self.types(sb))            # not a clean stop
        self.assertIn("needs a human", p.stderr)

    def test_failed_cycles_count_towards_the_limit(self):
        # r2's silent-outage shape: a daemon failing every cycle kept pinging the watchdog forever.
        sb = self.sandbox("def sense(ctx):\n    raise ValueError('no')\n" + DECIDE)
        p = sb.run(cycles=None, SPARK_DAEMON_TEST_BLIND_LIMIT_MS="600")
        self.assertEqual(p.returncode, EXIT_SENSE_BLIND, p.stderr)
        errors = [r["payload"] for r in sb.records() if r["event_type"] == "DAEMON_ERROR"]
        self.assertEqual(errors[0], {"category": "SENSE_FAILED", "exception_type": "ValueError"})
        blind = errors[-1]
        self.assertEqual((blind["category"], blind["last_cause"], blind["last_cause_kind"]),
                         ("SENSE_BLIND", "SENSE_FAILED", "error"))
        self.assertGreaterEqual(blind["failed_cycles"], 3)

    def test_an_accepted_cycle_resets_the_clock(self):
        # Busy for about twice the limit in total, but never for a whole limit at once.
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/value.txt", "1")
        sb.write("data/mode.txt", "busy")
        proc = subprocess.Popen([sys.executable, "-I", "-B", ENTRY, "run", "--manifest", sb.manifest,
                                 "--max-cycles", "40"], env=sb.env(SPARK_DAEMON_TEST_BLIND_LIMIT_MS="1500"),
                                stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)
        try:
            for mode in ("idle", "busy", "idle"):
                time.sleep(0.9)
                sb.write("data/mode.txt", mode)
            _, err = proc.communicate(timeout=30)
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.wait()
        self.assertEqual(proc.returncode, EXIT_OK, err)
        types = self.types(sb)
        self.assertEqual(types[-1], "DAEMON_STOP")
        self.assertNotIn("DAEMON_ERROR", types)

    # ---- r4.0 (PD-70): blindness survives restarts; heartbeats tell quiet from dead ----

    def popen(self, sb, limit_ms, cycles=None):
        cmd = [sys.executable, "-I", "-B", ENTRY, "run", "--manifest", sb.manifest]
        if cycles is not None:
            cmd += ["--max-cycles", str(cycles)]
        return subprocess.Popen(cmd, env=sb.env(SPARK_DAEMON_TEST_BLIND_LIMIT_MS=str(limit_ms)),
                                stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)

    def finish(self, p, timeout=30):
        """Wait for a run and close its stderr pipe; returns (exit code, stderr)."""
        try:
            err = p.communicate(timeout=timeout)[1]
        finally:
            if p.poll() is None:
                p.kill()
                p.communicate()
        return p.returncode, err

    def blind_records(self, sb):
        return [r["payload"] for r in sb.records()
                if r["event_type"] == "DAEMON_ERROR" and r["payload"].get("category") == "SENSE_BLIND"]

    def test_blindness_survives_a_restart_loop(self):
        # HF-32: killed and restarted every 1.0 s against a 1.5 s limit. On r3.9 no run ever
        # reached the limit; now the second run inherits the first run's blindness.
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "busy")
        code = None
        for _ in range(6):
            p = self.popen(sb, 1500)
            try:
                code = p.wait(timeout=1.0)
                p.stderr.close()
                break
            except subprocess.TimeoutExpired:
                p.kill()
                p.communicate()
        self.assertEqual(code, EXIT_SENSE_BLIND)
        blind = self.blind_records(sb)
        self.assertEqual(len(blind), 1)
        self.assertGreater(blind[0]["inherited_ms"], 0)
        self.assertIsNone(blind[0]["last_accepted_utc"])      # never saw anything accepted
        starts = [r["payload"] for r in sb.records() if r["event_type"] == "DAEMON_START"]
        self.assertGreater(starts[-1]["inherited_blind_ms"], 0)

    # ---- r4.5 (HF-34): a wall-clock step must not reset inherited blindness ----

    SHIFTED = ("import datetime as d, runpy, sys\n"
               "real = d.datetime\n"
               "class Behind(real):\n"
               "    @classmethod\n"
               "    def now(cls, tz=None):\n"
               "        return real.now(tz) - d.timedelta(seconds={shift})\n"
               "d.datetime = Behind\n"
               "sys.argv = [{entry!r}] + sys.argv[1:]\n"
               "runpy.run_path({entry!r}, run_name='__main__')\n")

    def popen_shifted(self, sb, limit_ms, shift_seconds):
        """Runs the daemon with its wall clock `shift_seconds` behind; the system clock is untouched."""
        code = self.SHIFTED.format(shift=shift_seconds, entry=ENTRY)
        return subprocess.Popen([sys.executable, "-I", "-B", "-c", code, "run", "--manifest", sb.manifest],
                                env=sb.env(SPARK_DAEMON_TEST_BLIND_LIMIT_MS=str(limit_ms)),
                                stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)

    def test_a_backward_clock_step_does_not_hide_blindness(self):
        # On r4.4 every restart after a one-hour backward step inherited 0 ms, and six kill-restarts
        # (6 s blind against a 1.5 s limit) never reached SENSE_BLIND (evidence/r4.5/).
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "busy")
        code = None
        for i in range(6):
            p = self.popen_shifted(sb, 1500, 3600 if i else 0)
            try:
                code = p.wait(timeout=1.0)
                p.stderr.close()
                break
            except subprocess.TimeoutExpired:
                p.kill()
                p.communicate()
        self.assertEqual(code, EXIT_SENSE_BLIND)
        blind = self.blind_records(sb)
        self.assertEqual(len(blind), 1)
        self.assertEqual(blind[0]["clock_basis"], "boottime")
        self.assertGreater(blind[0]["inherited_ms"], 0)

    def test_start_heartbeat_and_stop_carry_the_boot_stamp(self):
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "busy")
        self.assertEqual(self.finish(self.popen(sb, 400, cycles=4))[0], EXIT_OK)
        records = {r["event_type"]: r["payload"] for r in sb.records()}
        with open("/proc/sys/kernel/random/boot_id") as fh:
            boot = fh.read().strip()
        for kind in ("DAEMON_START", "DAEMON_HEARTBEAT", "DAEMON_STOP"):
            payload = records[kind]
            self.assertEqual(payload["boot_id"], boot, kind)
            self.assertLessEqual(payload["blind_since_boottime_ms"], payload["boottime_ms"], kind)
        self.assertEqual(records["DAEMON_START"]["clock_basis"], "fresh")

    def test_across_a_reboot_the_wall_clock_is_used_and_a_backward_one_assumes_the_worst(self):
        from spark_daemon import runtime
        other_boot = runtime._AcceptEvidence()
        other_boot.anchor = ("00000000-0000-0000-0000-000000000000", 5)
        past = "2000-01-01T00:00:00.000000Z"
        future = "2999-01-01T00:00:00.000000Z"
        seconds, basis = runtime._inherited_blindness(3, other_boot, runtime._boot_id(), past, 60)
        self.assertEqual(basis, "wall")
        self.assertGreater(seconds, 60)
        self.assertEqual(runtime._inherited_blindness(3, other_boot, runtime._boot_id(), future, 60),
                         (60.0, "worst_case"))
        self.assertEqual(runtime._inherited_blindness(0, other_boot, runtime._boot_id(), None, 60), (0.0, "fresh"))

    def test_an_event_after_the_last_stamp_is_anchored_at_that_stamp(self):
        # A daemon event carries no boot time; the preceding stamp bounds it from below (safe side).
        from spark_daemon import runtime
        ev = runtime._AcceptEvidence()
        boot = "11111111-1111-1111-1111-111111111111"
        ev.observe({"event_type": "DAEMON_START", "timestamp_utc": "t0",
                    "payload": {"boot_id": boot, "boottime_ms": 1000, "blind_since_boottime_ms": 400}})
        self.assertEqual(ev.anchor, (boot, 400))
        ev.observe({"event_type": "VALUE_OBSERVED", "timestamp_utc": "t1", "payload": {"value": "1"}})
        self.assertEqual(ev.anchor, (boot, 1000))
        ev.observe({"event_type": "DAEMON_HEARTBEAT", "timestamp_utc": "t2",
                    "payload": {"boot_id": boot, "boottime_ms": 9000, "blind_since_boottime_ms": 8500,
                                "last_accepted_utc": "t1"}})
        self.assertEqual(ev.anchor, (boot, 8500))
        ev.observe({"event_type": "DAEMON_STOP", "timestamp_utc": "t3", "payload": {"last_accepted_utc": "t1"}})
        self.assertIsNone(ev.anchor)      # an older record without a stamp: no boot-time evidence

    def test_a_clean_stop_does_not_reset_blindness(self):
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "busy")
        self.assertEqual(self.finish(self.popen(sb, 3000, cycles=10))[0], EXIT_OK)   # ~1 s, then DAEMON_STOP
        started = time.monotonic()
        self.assertEqual(self.finish(self.popen(sb, 3000))[0], EXIT_SENSE_BLIND)
        self.assertLess(time.monotonic() - started, 2.7)      # a fresh countdown would need 3 s
        self.assertGreater(self.blind_records(sb)[0]["inherited_ms"], 900)

    def test_a_restart_past_the_limit_gets_one_reacquisition_cycle(self):
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "busy")
        sb.write("data/value.txt", "7")
        self.assertEqual(self.finish(self.popen(sb, 800))[0], EXIT_SENSE_BLIND)
        # Still blind on restart: SENSE_BLIND after one cycle, with no fresh countdown.
        self.assertEqual(self.finish(self.popen(sb, 800))[0], EXIT_SENSE_BLIND)
        second = self.blind_records(sb)[-1]
        self.assertGreaterEqual(second["inherited_ms"], 800)
        self.assertEqual(second["unsettled_cycles"], 1)
        # Seeing again on restart: the one reacquisition cycle is accepted and the run carries on.
        sb.write("data/mode.txt", "idle")
        code, err = self.finish(self.popen(sb, 800, cycles=3))
        self.assertEqual(code, EXIT_OK, err)
        types = self.types(sb)
        self.assertEqual(types[-3:], ["DAEMON_START", "VALUE_OBSERVED", "DAEMON_STOP"])
        self.assertEqual(len(self.blind_records(sb)), 2)

    def test_heartbeats_tell_quiet_from_dead(self):
        # A healthy daemon that sees no change still leaves evidence every limit/2.
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "idle")
        sb.write("data/value.txt", "same")
        code, err = self.finish(self.popen(sb, 600, cycles=15))   # about 1.5 s at 100 ms
        self.assertEqual(code, EXIT_OK, err)
        beats = [r["payload"] for r in sb.records() if r["event_type"] == "DAEMON_HEARTBEAT"]
        self.assertGreaterEqual(len(beats), 2)
        self.assertEqual({b["mode"] for b in beats}, {"observing"})
        self.assertTrue(all(b["last_accepted_utc"] for b in beats))
        self.assertTrue(all(b["accepted_cycles"] > 0 for b in beats))
        stop = sb.records()[-1]["payload"]
        self.assertIsNotNone(stop["last_accepted_utc"])

    def test_a_malformed_reason_is_a_sense_failure(self):
        sb = self.sandbox("def sense(ctx):\n    return ctx.unsettled('not a category')\n" + DECIDE)
        p = sb.run(cycles=2)
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        errors = [r["payload"] for r in sb.records() if r["event_type"] == "DAEMON_ERROR"]
        self.assertEqual(errors, [{"category": "SENSE_FAILED", "exception_type": "ValueError"}])

    def test_the_override_is_ignored_outside_test_mode(self):
        sb = self.sandbox(MODE_DRIVEN)
        sb.write("data/mode.txt", "busy")
        # Outside test mode the manifest's 600 s applies, and the interval is the manifest's
        # 5 s, so one cycle then max-cycles stops it well inside the limit.
        p = sb.run(cycles=1, qualified=True, SPARK_DAEMON_TEST="0", SPARK_DAEMON_TEST_BLIND_LIMIT_MS="100",
                   SPARK_DAEMON_AUDIT="enforce")
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        self.assertNotIn("blind_limit_ms", sb.records()[0]["payload"]["test_overrides"])


def example():
    with open(os.path.join(EXAMPLE, "manifest.json")) as fh:
        return json.load(fh)


class BlindLimitManifestTests(unittest.TestCase):
    def assertRefused(self, data, fragment):
        with self.assertRaises(manifest.ManifestError) as ctx:
            manifest.parse(data)
        self.assertIn(fragment, " | ".join(ctx.exception.problems))

    def test_required_with_no_default(self):
        d = example()
        del d["blind_limit_seconds"]
        self.assertRefused(d, "blind_limit_seconds")

    def test_bounded_both_ways(self):
        self.assertRefused(dict(example(), blind_limit_seconds=59), "blind_limit_seconds")
        self.assertRefused(dict(example(), blind_limit_seconds=86401), "blind_limit_seconds")
        self.assertRefused(dict(example(), blind_limit_seconds=None), "blind_limit_seconds")

    def test_at_least_three_intervals(self):
        d = dict(example(), blind_limit_seconds=600)
        d["trigger"] = {"kind": "poll", "interval_seconds": 300}
        self.assertRefused(d, "at least 3 x trigger.interval_seconds")
        d["trigger"] = {"kind": "poll", "interval_seconds": 200}
        self.assertEqual(manifest.parse(d).blind_limit_seconds, 600)

    def test_version_1_manifest_gets_a_migration_hint(self):
        d = example()
        del d["blind_limit_seconds"]
        d["manifest_schema"] = "spark-daemon-manifest/1"
        self.assertRefused(d, "adds the required blind_limit_seconds")


class StubResult:
    def __init__(self, stdout, truncated=False):
        self.stdout, self.truncated, self.timed_out = stdout, truncated, False
        self.returncode = None if truncated else 0


class GitWatchUnsettledTests(unittest.TestCase):
    """The reference use: git-watch re-reads refs and the dirty count and reports a moving
    repository as unsettled rather than recording a half-way state."""

    def setUp(self):
        spec = importlib.util.spec_from_file_location(
            "git_watch", os.path.join(ROOT, "examples", "git-watch", "daemon.py"))
        self.daemon = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.daemon)

    def ctx(self, refs_reads, status_reads):
        """Answers ctx.git from two queues of outputs (a StubResult stands for a truncated one)."""
        class StubCtx:
            Missing, TooLarge = context.Missing, context.TooLarge
            unsettled = context.Context.unsettled

            def git(self, repo, args, max_bytes=65536, index_copy=False):
                if args[0] == "rev-parse":
                    return StubResult("false\n" if "--is-bare-repository" in args else "main\n")
                out = (refs_reads if args[0] == "for-each-ref" else status_reads).pop(0)
                return out if isinstance(out, StubResult) else StubResult(out)
        return StubCtx()

    def test_moving_refs_are_unsettled(self):
        a, b = "a" * 40 + " refs/heads/main\n", "b" * 40 + " refs/heads/main\n"
        result = self.daemon.sense(self.ctx([a, b], ["", ""]))
        self.assertIsInstance(result, context.Unsettled)
        self.assertEqual(result.reason, "REPO_CHANGING")

    def test_a_changing_worktree_is_unsettled(self):
        refs = "a" * 40 + " refs/heads/main\n"
        result = self.daemon.sense(self.ctx([refs, refs], [" M one\0", " M one\0?? build.o\0"]))
        self.assertIsInstance(result, context.Unsettled)

    def test_output_over_the_cap_fails_the_cycle(self):
        # Before r3.5 this was accepted as "dirty: null" every cycle, so it never reached the limit.
        refs = "a" * 40 + " refs/heads/main\n"
        with self.assertRaises(context.TooLarge):
            self.daemon.sense(self.ctx([refs], [StubResult("?? x\0" * 1000, truncated=True)]))

    def test_a_still_repository_is_observed(self):
        refs = "a" * 40 + " refs/heads/main\n"
        result = self.daemon.sense(self.ctx([refs, refs], [" M one\0", " M one\0"]))
        self.assertEqual((result["head"], result["dirty"]), ("main", 1))


class DirWatchCapacityTests(unittest.TestCase):
    """r4.3 (HF-33, LTC-H01): past MAX_ENTRIES, ctx.list_dir returns the first entries in
    directory order. Diffing that partial listing as a full inventory reported files as
    removed that were still present each time a file was added."""

    def setUp(self):
        spec = importlib.util.spec_from_file_location(
            "dir_watch", os.path.join(ROOT, "examples", "dir-watch", "daemon.py"))
        self.daemon = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.daemon)
        self.tmp = tempfile.mkdtemp()
        self.daemon.WATCHED = self.tmp

        class Policy:
            def readable(self, path, follow=True):
                return path
        self.ctx = context.Context(Policy(), step_timeout=10, tmp_dir=self.tmp)

    def tearDown(self):
        shutil.rmtree(self.tmp)

    def touch(self, name):
        open(os.path.join(self.tmp, name), "w").close()

    def test_a_folder_over_the_cap_fails_the_cycle(self):
        for i in range(self.daemon.MAX_ENTRIES + 1):
            self.touch(f"f{i:05d}")
        with self.assertRaises(context.TooLarge):
            self.daemon.sense(self.ctx)

    def test_adding_a_file_over_the_cap_never_reports_a_removal(self):
        for i in range(self.daemon.MAX_ENTRIES + 88):
            self.touch(f"f{i:05d}")
        removed_but_present = []
        for k in range(10):
            try:
                before = self.daemon.sense(self.ctx)
            except context.TooLarge:
                before = None
            self.touch(f"new{k:03d}")
            try:
                after = self.daemon.sense(self.ctx)
            except context.TooLarge:
                continue
            for kind, payload in self.daemon.decide(before, after):
                if kind == "INBOX_ENTRIES_REMOVED":
                    removed_but_present += [n for n in payload["names"]
                                            if os.path.exists(os.path.join(self.tmp, n))]
        self.assertEqual(removed_but_present, [])

    def test_at_the_cap_the_inventory_is_complete(self):
        for i in range(self.daemon.MAX_ENTRIES - 1):
            self.touch(f"f{i:05d}")
        before = self.daemon.sense(self.ctx)
        self.touch("last")
        after = self.daemon.sense(self.ctx)
        self.assertEqual(len(after["entries"]), self.daemon.MAX_ENTRIES)
        self.assertEqual([k for k, _ in self.daemon.decide(before, after)], ["INBOX_ENTRIES_ADDED"])


class NoRestartTests(unittest.TestCase):
    def test_fail_closed_exits_are_never_restarted(self):
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        text = unitgen.generate(m, root=ROOT)
        self.assertIn("RestartPreventExitStatus=2 65 73 78\n", text)
        self.assertEqual(unitgen.lint(text), [])
        broken = text.replace(unitgen.RESTART_PREVENT + "\n", "")
        self.assertIn(unitgen.RESTART_PREVENT, unitgen.lint(broken))

    def test_a_stop_outlasts_one_watchdog_period(self):
        # r3.9 (HF-31): a stop is honoured between cycles; a fixed 30 s SIGKILLed slow cycles.
        for name in ("meminfo-watch", "git-watch"):
            m = manifest.load(os.path.join(ROOT, "examples", name, "manifest.json"))
            text = unitgen.generate(m, root=ROOT)
            self.assertIn(f"TimeoutStopSec={m.watchdog_seconds + 10}\n", text)

    def test_git_watch_worst_case_sense_fits_half_the_watchdog(self):
        # r3.9 (HF-30): six Git calls per worktree cycle since r3.5, each up to step_timeout.
        m = manifest.load(os.path.join(ROOT, "examples", "git-watch", "manifest.json"))
        with open(os.path.join(ROOT, "examples", "git-watch", "daemon.py")) as fh:
            tree = ast.parse(fh.read())
        calls = sum(1 for node in ast.walk(tree)     # counted by the parser, so line wrapping
                    if isinstance(node, ast.Call) and getattr(node.func, "id", None) == "_git")
        self.assertEqual(calls, 6)
        self.assertLessEqual(calls * m.step_timeout_seconds, m.watchdog_seconds // 2)

    def test_uncertain_commit_stays_restartable(self):
        self.assertNotIn(70, unitgen.NO_RESTART_EXIT_CODES)


if __name__ == "__main__":
    unittest.main()
