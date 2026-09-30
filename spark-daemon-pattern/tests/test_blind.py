"""Blind period (r3.5): ctx.unsettled(), blind_limit_seconds and SENSE_BLIND, and the unit
never restarting a fail-closed exit."""

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
        p = sb.run(cycles=1, SPARK_DAEMON_TEST="0", SPARK_DAEMON_TEST_BLIND_LIMIT_MS="100",
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
            calls = fh.read().count("_git(ctx, [")
        self.assertEqual(calls, 6)
        self.assertLessEqual(calls * m.step_timeout_seconds, m.watchdog_seconds // 2)

    def test_uncertain_commit_stays_restartable(self):
        self.assertNotIn(70, unitgen.NO_RESTART_EXIT_CODES)


if __name__ == "__main__":
    unittest.main()
