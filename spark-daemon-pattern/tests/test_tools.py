"""tools/daemon-start.py, the hardware gate's evidence extractor: it reads a ledger the way the
ledger is designed to be read (torn tail reported, never parsed; a bad line is a clear exit,
never a traceback), imports nothing from the pattern, and does not depend on the schema string,
so it also reads a vendored copy's renamed ledger."""

import json
import os
import shutil
import subprocess
import sys
import unittest

from helpers import ROOT, Sandbox

TOOL = os.path.join(ROOT, "tools", "daemon-start.py")


def run(*args):
    return subprocess.run([sys.executable, "-I", "-B", TOOL, *args], capture_output=True, text=True, timeout=30)


class DaemonStartToolTests(unittest.TestCase):
    def setUp(self):
        self.sb = Sandbox("counter")
        self.addCleanup(shutil.rmtree, self.sb.tmp)
        self.sb.write("data/value.txt", "x")
        self.assertEqual(self.sb.run(cycles=1).returncode, 0)
        self.assertEqual(self.sb.run(cycles=1).returncode, 0)        # two runs: two DAEMON_START
        self.ledger = self.sb.ledger_path

    def test_prints_the_last_start_and_all_starts(self):
        p = run(self.ledger)
        self.assertEqual(p.returncode, 0, p.stderr)
        record = json.loads(p.stdout)
        self.assertEqual(record["event_type"], "DAEMON_START")
        self.assertIs(record["payload"]["previous_run_ended_cleanly"], True)   # the second start
        p = run(self.ledger, "--all")
        self.assertEqual(p.stdout.count('"event_type": "DAEMON_START"'), 2)

    def test_a_torn_tail_is_reported_and_never_parsed(self):
        tail = b'{"schema":"spark-daemon-ledger/1","seq":99'                 # a crash mid-write
        with open(self.ledger, "ab") as fh:
            fh.write(tail)
        before = open(self.ledger, "rb").read()
        p = run(self.ledger)
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertIn(f"torn tail of {len(tail)} bytes at byte {len(before) - len(tail)}", p.stderr)
        self.assertEqual(json.loads(p.stdout)["event_type"], "DAEMON_START")
        self.assertEqual(open(self.ledger, "rb").read(), before)               # read-only

    def test_a_bad_committed_line_is_a_clear_exit_not_a_traceback(self):
        with open(self.ledger, "ab") as fh:
            fh.write(b"[" * 3000 + b"]" * 3000 + b"\n")                        # HF-43's shape
        p = run(self.ledger)
        self.assertEqual(p.returncode, 2)
        self.assertNotIn("Traceback", p.stderr)
        self.assertIn("does not parse", p.stderr)

    def test_no_start_no_file_and_a_renamed_schema(self):
        empty = os.path.join(self.sb.tmp, "empty.jsonl")
        open(empty, "w").close()
        self.assertEqual(run(empty).returncode, 1)
        self.assertEqual(run(os.path.join(self.sb.tmp, "missing.jsonl")).returncode, 2)
        renamed = os.path.join(self.sb.tmp, "renamed.jsonl")
        with open(self.ledger, "rb") as src, open(renamed, "wb") as dst:
            dst.write(src.read().replace(b"spark-daemon-ledger/1", b"fb-daemon-ledger/1"))
        p = run(renamed)
        self.assertEqual(p.returncode, 0, p.stderr)                            # no schema-string check

    def test_the_tool_imports_nothing_from_the_pattern(self):
        with open(TOOL) as fh:
            source = fh.read()
        self.assertNotIn("spark_daemon", source)
        self.assertNotIn("import ", source.split('"""')[2].split("import json")[0])   # only json, sys


if __name__ == "__main__":
    unittest.main()
