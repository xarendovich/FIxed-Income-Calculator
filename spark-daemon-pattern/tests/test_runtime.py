"""Runtime end to end: lifecycle, errors, fail-closed policy, the watchdog, refusals."""

import json
import os
import socket
import subprocess
import sys
import threading
import time
import unittest

from helpers import EXAMPLE, ROOT, Sandbox
from spark_daemon import EXIT_LEDGER_CORRUPT, EXIT_OK, EXIT_POLICY, EXIT_USAGE
from spark_daemon.runtime import JITTER_FRACTION, JITTER_MAX_SECONDS, _jitter_bound


class RuntimeTests(unittest.TestCase):
    def setUp(self):
        self.boxes = []

    def tearDown(self):
        for box in self.boxes:
            box.cleanup()

    def box(self, fixture, **overrides):
        b = Sandbox(fixture, overrides)
        self.boxes.append(b)
        return b

    def types(self, box):
        return [r["event_type"] for r in box.records()]

    def test_lifecycle_and_digest(self):
        b = self.box("counter")
        b.write("data/value.txt", "first")
        p = b.run(cycles=3)
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        self.assertEqual(self.types(b), ["DAEMON_START", "VALUE_OBSERVED", "DAEMON_STOP"])
        start = b.records()[0]["payload"]
        self.assertIsNone(start["previous_run_ended_cleanly"])
        self.assertEqual(start["test_overrides"], {"interval_ms": 100})
        b.write("data/value.txt", "second")
        p = b.run(cycles=2)
        recs = b.records()
        self.assertEqual(recs[3]["payload"]["previous_run_ended_cleanly"], True)
        with open(os.path.join(b.output, "digest.md")) as fh:
            digest = fh.read()
        self.assertIn('"second"', digest)
        self.assertIn("Untrusted evidence", digest)

    def test_hostile_value_is_contained_in_the_digest(self):
        b = self.box("counter")
        b.write("data/value.txt", "```\n# Forged\n" + chr(0x202E) + "x")
        self.assertEqual(b.run(cycles=1).returncode, EXIT_OK)
        with open(os.path.join(b.output, "digest.md"), encoding="utf-8") as fh:
            digest = fh.read()
        self.assertNotIn("\n# Forged", digest)
        self.assertNotIn(chr(0x202E), digest)
        self.assertIn("| # Forged", digest)

    def test_errors_are_bounded_and_streaks_suppressed(self):
        b = self.box("raiser")
        b.write("data/mode.txt", "raise")
        p = b.run(cycles=4)
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        recs = b.records()
        self.assertEqual([r["event_type"] for r in recs],
                         ["DAEMON_START", "DAEMON_ERROR", "DAEMON_ERROR_CLEARED", "DAEMON_STOP"])
        self.assertEqual(recs[1]["payload"], {"category": "SENSE_FAILED", "exception_type": "ValueError"})
        self.assertEqual(recs[2]["payload"]["repeats"], 4)
        self.assertEqual(recs[2]["payload"]["ended_by"], "stop")
        raw = b.ledger_bytes()
        self.assertNotIn(b"hostile", raw)
        self.assertNotIn(b"instructions", raw)

    def test_error_streak_clears_on_success(self):
        b = self.box("raiser")
        b.write("data/mode.txt", "raise")
        def calm_after_first_error():
            deadline = time.monotonic() + 20
            while time.monotonic() < deadline:
                if os.path.exists(b.ledger_path) and b"DAEMON_ERROR" in b.ledger_bytes():
                    b.write("data/mode.txt", "calm")
                    return
                time.sleep(0.02)

        proc_thread = threading.Thread(target=calm_after_first_error)
        proc_thread.start()
        p = b.run(cycles=10)
        proc_thread.join()
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        cleared = [r for r in b.records() if r["event_type"] == "DAEMON_ERROR_CLEARED"]
        self.assertEqual(len(cleared), 1)
        self.assertEqual(cleared[0]["payload"]["ended_by"], "success")

    def test_policy_violation_fails_closed_even_when_swallowed(self):
        b = self.box("sneaky")
        p = b.run(cycles=3)
        self.assertEqual(p.returncode, EXIT_POLICY)
        recs = b.records()
        self.assertEqual(recs[-1]["event_type"], "DAEMON_ERROR")
        self.assertEqual(recs[-1]["payload"]["category"], "POLICY_VIOLATION")
        self.assertNotIn(b"CANARY-7f3a", b.ledger_bytes())

    def test_undeclared_event_type_is_refused(self):
        b = self.box("undeclared")
        self.assertEqual(b.run(cycles=2).returncode, EXIT_OK)
        self.assertNotIn("NOT_DECLARED", self.types(b))
        errors = [r["payload"] for r in b.records() if r["event_type"] == "DAEMON_ERROR"]
        self.assertEqual(errors[0]["category"], "EVENT_INVALID")

    def test_float_payload_is_refused(self):
        b = self.box("floaty")
        self.assertEqual(b.run(cycles=1).returncode, EXIT_OK)
        errors = [r["payload"] for r in b.records() if r["event_type"] == "DAEMON_ERROR"]
        self.assertEqual(errors[0]["category"], "EVENT_INVALID")

    def test_test_mode_refused_under_systemd(self):
        b = self.box("counter")
        p = b.run(cycles=1, INVOCATION_ID="0123456789abcdef")
        self.assertEqual(p.returncode, EXIT_USAGE)
        self.assertFalse(os.path.exists(b.ledger_path))

    def test_impure_code_never_starts(self):
        b = self.box("opener")
        p = b.run(cycles=1)
        self.assertEqual(p.returncode, EXIT_USAGE)
        self.assertIn("call to open() is not allowed", p.stderr)
        self.assertFalse(os.path.exists(b.ledger_path))

    def test_unsafe_output_directory_is_refused(self):
        b = self.box("counter")
        os.makedirs(b.output, mode=0o755)
        os.chmod(b.output, 0o755)
        self.assertEqual(b.run(cycles=1).returncode, EXIT_POLICY)

    def test_foreign_files_are_reported_not_touched(self):
        b = self.box("counter")
        os.makedirs(b.output, mode=0o700)
        with open(os.path.join(b.output, "latest_digest.md"), "w") as fh:
            fh.write("written by someone else")
        self.assertEqual(b.run(cycles=1).returncode, EXIT_OK)
        errors = [r["payload"] for r in b.records() if r["event_type"] == "DAEMON_ERROR"]
        self.assertEqual(errors[0], {"category": "OUTPUT_DIR_FOREIGN_FILES", "count": 1, "names": ["latest_digest.md"]})
        with open(os.path.join(b.output, "latest_digest.md")) as fh:
            self.assertEqual(fh.read(), "written by someone else")

    def test_corrupt_ledger_refuses_and_changes_nothing(self):
        b = self.box("counter")
        b.write("data/value.txt", "x")
        b.run(cycles=2)
        data = bytearray(b.ledger_bytes())
        data[20] = ord("#")
        with open(b.ledger_path, "wb") as fh:
            fh.write(data)
        self.assertEqual(b.run(cycles=1).returncode, EXIT_LEDGER_CORRUPT)
        self.assertEqual(b.ledger_bytes(), bytes(data))

    def test_watchdog_pings_stop_while_an_observation_hangs(self):
        """No background pinger: a blocked sense() starves the watchdog, so systemd would restart it."""
        b = self.box("hang")
        sock_path = os.path.join(b.tmp, "notify.sock")
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
        sock.bind(sock_path)
        sock.settimeout(0.2)
        got, done = [], threading.Event()

        def collect():
            while not done.is_set():
                try:
                    got.append((time.monotonic(), sock.recv(256).decode()))
                except socket.timeout:
                    pass

        t = threading.Thread(target=collect)
        t.start()
        p = b.run(cycles=2, NOTIFY_SOCKET=sock_path)
        done.set()
        t.join()
        sock.close()
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        kinds = [m for _, m in got]
        self.assertEqual(kinds[0], "READY=1")
        times = [ts for ts, m in got if m in ("READY=1", "WATCHDOG=1")]
        longest = max(b2 - a for a, b2 in zip(times, times[1:]))
        self.assertGreater(longest, 2.5, "pings continued during a 3-second hang")
        self.assertIn("STOPPING=1", kinds)

    def test_jitter_bound_is_zero_in_test_mode(self):
        for interval in (5.0, 30.0, 86400.0):
            self.assertEqual(_jitter_bound(interval, test_mode=True), 0.0)

    def test_jitter_bound_is_a_fraction_of_the_interval(self):
        self.assertAlmostEqual(_jitter_bound(200.0, test_mode=False), 200.0 * JITTER_FRACTION)
        self.assertAlmostEqual(_jitter_bound(5.0, test_mode=False), 0.25)

    def test_jitter_bound_is_capped_for_long_intervals(self):
        # 86400s (the manifest maximum) times 5% would be 4320s; it must not drift by hours.
        self.assertEqual(_jitter_bound(86400.0, test_mode=False), JITTER_MAX_SECONDS)

    def test_jitter_disabled_in_test_mode_and_recorded_for_a_real_run(self):
        b = self.box("counter")  # interval_seconds: 5 in the fixture manifest
        b.write("data/value.txt", "x")
        self.assertEqual(b.run(cycles=1).returncode, EXIT_OK)
        self.assertEqual(b.records()[0]["payload"]["jitter_max_ms"], 0)

        b2 = self.box("counter")
        b2.write("data/value.txt", "x")
        env = b2.env()
        del env["SPARK_DAEMON_TEST"]
        del env["SPARK_DAEMON_TEST_INTERVAL_MS"]
        p = subprocess.run(
            [sys.executable, "-I", "-B", os.path.join(ROOT, "bin", "spark-daemon"),
             "run", "--manifest", b2.manifest, "--max-cycles", "1", *b2.qualified_args()],
            env=env, capture_output=True, text=True, timeout=30)
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        self.assertEqual(b2.records()[0]["payload"]["jitter_max_ms"], 250)  # 5s * 5%

    def test_example_daemon_runs(self):
        b = self.box(EXAMPLE)
        p = b.run(cycles=2)
        self.assertEqual(p.returncode, EXIT_OK, p.stderr)
        baseline = [r for r in b.records() if r["event_type"] == "MEMORY_OBSERVATION_BASELINE"]
        self.assertEqual(len(baseline), 1)
        self.assertIn(baseline[0]["payload"]["band"], ("ok", "tight", "critical"))
        self.assertIsInstance(baseline[0]["payload"]["mem_total_kb"], int)
        stop = b.records()[-1]["payload"]
        self.assertIn("cpu_us_since_ready", stop)
        self.assertIsInstance(json.dumps(stop), str)


if __name__ == "__main__":
    unittest.main()
