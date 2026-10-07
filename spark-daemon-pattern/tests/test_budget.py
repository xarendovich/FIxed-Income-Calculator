"""The cycle budget (r4.11, R-6): cycle_budget_seconds is the one declared cycle time; every
blocking call gets what is left of it, an overrunning cycle fails within its deadline, and the
unit's watchdog and stop timeouts are derived from it and cannot contradict it. The acceptance
tests of the owner's r4.9 handoff, Phase C (R-6), with the manifest migration (R-1). Since
contract 5 (R-2, E-2) the cycle's alarm is the only in-process deadline: there are no commands
to hand the time left to."""

import json
import os
import shutil
import tempfile
import time
import unittest
from unittest import mock

from helpers import EXAMPLE, FIXTURES, ROOT, Sandbox
from spark_daemon import judge, manifest, unitgen

SLOW = {
    "busy_loop": "    while True:\n        pass\n",
    "blocking_read": "    ctx.read_text('~/data/fifo')\n",
    "swallowed": "    try:\n        while True:\n            pass\n    except BaseException:\n        pass\n",
}


def fixture(body):
    tmp = tempfile.mkdtemp(prefix="spark-budget-")
    with open(os.path.join(FIXTURES, "counter", "manifest.json")) as fh:
        data = json.load(fh)
    with open(os.path.join(tmp, "manifest.json"), "w") as fh:
        json.dump(data, fh)
    with open(os.path.join(tmp, "daemon.py"), "w") as fh:
        fh.write("def sense(ctx):\n" + body + "    return {'value': 'x'}\n\n\n"
                 "def decide(prev, snapshot):\n    return []\n")
    return tmp


class CycleDeadlineTests(unittest.TestCase):
    def run_slow(self, kind):
        tmp = fixture(SLOW[kind])
        self.addCleanup(shutil.rmtree, tmp)
        sb = Sandbox(tmp)
        self.addCleanup(shutil.rmtree, sb.tmp)
        if kind == "blocking_read":
            os.makedirs(os.path.join(sb.home, "data"), exist_ok=True)
            os.mkfifo(os.path.join(sb.home, "data", "fifo"))     # open() blocks: no writer, ever
        started = time.monotonic()
        p = sb.run(cycles=2, timeout=60, cycle_budget_ms=400)
        return p, time.monotonic() - started, sb.records()

    def assertFailsWithinTheDeadline(self, kind):
        p, elapsed, records = self.run_slow(kind)
        self.assertEqual(p.returncode, 0, p.stderr)
        errors = [r["payload"] for r in records if r["event_type"] == "DAEMON_ERROR"]
        self.assertEqual(errors[0], {"category": "CYCLE_BUDGET_EXCEEDED", "exception_type": "CycleBudgetExceeded"},
                         errors)
        self.assertEqual(records[0]["payload"]["harness"]["cycle_budget_ms"], 400)
        # Two cycles of at most 0.4 s each, plus start-up and stop; without the deadline every one
        # of these would hang until the test's own 60 s timeout.
        self.assertLess(elapsed, 8, elapsed)

    def test_a_pure_python_loop_fails_within_the_deadline(self):
        self.assertFailsWithinTheDeadline("busy_loop")

    def test_a_blocking_read_fails_within_the_deadline(self):
        self.assertFailsWithinTheDeadline("blocking_read")

    def test_catching_the_alarm_does_not_save_the_cycle(self):
        self.assertFailsWithinTheDeadline("swallowed")

    def test_an_overrun_counts_as_blind_time(self):
        tmp = fixture(SLOW["busy_loop"])
        self.addCleanup(shutil.rmtree, tmp)
        sb = Sandbox(tmp)
        self.addCleanup(shutil.rmtree, sb.tmp)
        p = sb.run(cycles=None, timeout=60, cycle_budget_ms=200,
                   blind_limit_ms=900)
        self.assertEqual(p.returncode, 78, p.stderr)
        blind = [r["payload"] for r in sb.records() if r["payload"].get("category") == "SENSE_BLIND"]
        self.assertEqual(blind[0]["last_cause"], "CYCLE_BUDGET_EXCEEDED")


class DerivedTimingTests(unittest.TestCase):
    def manifest_with(self, budget):
        with open(os.path.join(EXAMPLE, "manifest.json")) as fh:
            data = json.load(fh)
        data["cycle_budget_seconds"] = budget
        return manifest.parse(data, os.path.join(EXAMPLE, "manifest.json"))

    def test_the_unit_cannot_contradict_the_budget(self):
        for budget in (5, 40, 115, 900):
            with self.subTest(budget=budget):
                m = self.manifest_with(budget)
                text = unitgen.generate(m, root=ROOT)
                self.assertEqual(unitgen.timing_problems(text, m), [])
                self.assertIn(f"WatchdogSec={2 * budget + manifest.WATCHDOG_MARGIN_SECONDS}\n", text)
                self.assertIn(f"TimeoutStopSec={m.watchdog_seconds + 10}\n", text)
                edited = text.replace(f"WatchdogSec={m.watchdog_seconds}\n", "WatchdogSec=9\n")
                self.assertEqual(len(unitgen.timing_problems(edited, m)), 1)

    def test_db25_fails_a_unit_whose_timing_contradicts_the_budget(self):
        real = unitgen.generate

        def wrong(m, **kw):
            return real(m, **kw).replace(f"TimeoutStopSec={m.stop_timeout_seconds}", "TimeoutStopSec=5")
        with mock.patch.object(unitgen, "generate", wrong):
            j = judge.run("validate", os.path.join(EXAMPLE, "manifest.json"))
        db25 = {c.id: c for c in j.checks}["DB-25"]
        self.assertEqual((j.result, db25.state), ("FAIL", "FAIL"))
        self.assertIn("TimeoutStopSec=5, but the cycle budget gives", db25.evidence)


class MigrationTests(unittest.TestCase):
    def test_a_contract_3_manifest_is_told_exactly_what_changed(self):
        with open(os.path.join(EXAMPLE, "manifest.json")) as fh:
            data = json.load(fh)
        data["manifest_schema"] = "spark-daemon-manifest/2"
        data.pop("cycle_budget_seconds")
        data.update(deny=[], watchdog_seconds=90, step_timeout_seconds=10)
        with self.assertRaises(manifest.ManifestError) as ctx:
            manifest.parse(data)
        where = [p.split(":")[0] for p in ctx.exception.problems]
        self.assertEqual(where, ["manifest_schema", "deny", "watchdog_seconds", "step_timeout_seconds",
                                 "cycle_budget_seconds"])
        self.assertIn("contract 5.0.0 needs", ctx.exception.problems[0])
        self.assertTrue(all("4.0.0" in p for p in ctx.exception.problems[1:]), ctx.exception.problems)

    def test_a_contract_4_manifest_is_told_commands_are_gone(self):
        """R-2: a schema-3 manifest learns, field by field, that commands left with contract 5."""
        with open(os.path.join(EXAMPLE, "manifest.json")) as fh:
            data = json.load(fh)
        data.update(manifest_schema="spark-daemon-manifest/3", commands=["git"])
        with self.assertRaises(manifest.ManifestError) as ctx:
            manifest.parse(data)
        where = [p.split(":")[0] for p in ctx.exception.problems]
        self.assertEqual(where, ["manifest_schema", "commands"])
        self.assertIn("removed in contract 5.0.0 (R-2)", ctx.exception.problems[1])

    def test_commands_are_refused_under_the_current_schema_too(self):
        with open(os.path.join(EXAMPLE, "manifest.json")) as fh:
            data = json.load(fh)
        data["commands"] = []
        with self.assertRaises(manifest.ManifestError) as ctx:
            manifest.parse(data)
        self.assertEqual([p.split(":")[0] for p in ctx.exception.problems], ["commands"])


if __name__ == "__main__":
    unittest.main()
