"""Ledger: append and verify, every corruption category, torn tails, uncertain commits."""

import json
import os
import shutil
import tempfile
import unittest
from unittest import mock

from helpers import ROOT  # noqa: F401
from spark_daemon import LEDGER_NAME, QUARANTINE_DIR, ledger
from spark_daemon.canonical import GENESIS, CanonicalError

NAME = "fixture"
MAX = 4096


def clock():
    return "2026-09-27T00:00:00.000000Z"


class LedgerTests(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        os.mkdir(os.path.join(self.dir, QUARANTINE_DIR), 0o700)
        self.path = os.path.join(self.dir, LEDGER_NAME)

    def tearDown(self):
        shutil.rmtree(self.dir)

    def write(self, n=5):
        w = ledger.LedgerWriter(self.dir, NAME, "r" * 32, MAX, ledger.Tail(), clock)
        w.open()
        for i in range(n):
            w.append("EVT", {"i": i, "text": "é" * (i % 50)})
        w.close()
        return w.tail

    def raw(self):
        with open(self.path, "rb") as fh:
            return fh.read()

    def lines(self):
        with open(self.path, "rb") as fh:
            return fh.read().split(b"\n")[:-1]

    def put(self, lines, tail=b""):
        with open(self.path, "wb") as fh:
            fh.write(b"\n".join(lines) + b"\n" + tail)

    def assertCorrupt(self, category):
        with self.assertRaises(ledger.LedgerCorrupt) as ctx:
            ledger.verify_file(self.path, NAME, MAX)
        self.assertEqual(ctx.exception.category, category)

    def test_append_and_verify(self):
        tail = self.write(5)
        result = ledger.verify_file(self.path, NAME, MAX)
        self.assertEqual(result.tail, tail)
        self.assertEqual(result.records, 5)
        first = json.loads(self.lines()[0])
        self.assertEqual(first["seq"], 1)
        self.assertEqual(first["prev_sha256"], GENESIS)

    def test_flipped_byte(self):
        self.write(5)
        lines = self.lines()
        lines[2] = lines[2].replace(b'"i":2', b'"i":7')
        self.put(lines)
        self.assertCorrupt("LINK")

    def test_swapped_lines(self):
        self.write(5)
        lines = self.lines()
        lines[1], lines[2] = lines[2], lines[1]
        self.put(lines)
        self.assertCorrupt("SEQUENCE")

    def test_missing_line(self):
        self.write(5)
        lines = self.lines()
        del lines[2]
        self.put(lines)
        self.assertCorrupt("SEQUENCE")

    def test_non_canonical_line(self):
        self.write(3)
        lines = self.lines()
        lines[1] = json.dumps(json.loads(lines[1]), indent=1).replace("\n", "").encode()
        self.put(lines)
        self.assertCorrupt("NOT_CANONICAL")

    def test_foreign_daemon(self):
        self.write(3)
        with self.assertRaises(ledger.LedgerCorrupt) as ctx:
            ledger.verify_file(self.path, "someone-else", MAX)
        self.assertEqual(ctx.exception.category, "DAEMON_MISMATCH")

    def test_newer_schema_fails_explicitly(self):
        self.write(2)
        lines = self.lines()
        lines[0] = lines[0].replace(b"spark-daemon-ledger/1", b"spark-daemon-ledger/2")
        self.put(lines)
        self.assertCorrupt("UNSUPPORTED_SCHEMA")

    def test_overlong_line(self):
        self.write(2)
        self.put(self.lines() + [b'{"x":"' + b"a" * (MAX + 10) + b'"}'])
        self.assertCorrupt("RECORD_TOO_LARGE")

    def test_corrupt_last_complete_line_is_not_a_torn_tail(self):
        self.write(3)
        lines = self.lines()
        lines[-1] = lines[-1][:-5]
        self.put(lines)
        self.assertCorrupt("UNPARSEABLE")

    def test_torn_tail_garbage_is_quarantined(self):
        tail = self.write(3)
        with open(self.path, "ab") as fh:
            fh.write(b'{"partial": tr')
        result, q = ledger.recover(self.dir, NAME, MAX)
        self.assertEqual(result.tail, tail)
        self.assertEqual(q.length, len(b'{"partial": tr'))
        with open(os.path.join(self.dir, QUARANTINE_DIR, q.file), "rb") as fh:
            self.assertEqual(fh.read(), b'{"partial": tr')
        self.assertEqual(len(self.lines()), 3)
        again, q2 = ledger.recover(self.dir, NAME, MAX)          # idempotent
        self.assertIsNone(q2)
        self.assertEqual(again.tail, tail)

    def test_complete_record_without_newline_is_quarantined(self):
        self.write(3)
        lines = self.lines()
        with open(self.path, "wb") as fh:
            fh.write(b"\n".join(lines))                  # last record loses its newline
        result, q = ledger.recover(self.dir, NAME, MAX)
        self.assertIsNotNone(q)
        self.assertEqual(result.records, 2)

    def test_two_torn_tails_never_overwrite(self):
        self.write(2)
        names = []
        for _ in range(2):
            with open(self.path, "ab") as fh:
                fh.write(b"same bytes")
            names.append(ledger.recover(self.dir, NAME, MAX)[1].file)
        self.assertEqual(len(set(names)), 2)

    def test_prepare_refuses_before_writing(self):
        self.write(1)
        before = self.raw()
        w = ledger.LedgerWriter(self.dir, NAME, "r" * 32, MAX, ledger.verify_file(self.path, NAME, MAX).tail, clock)
        w.open()
        with self.assertRaises(CanonicalError):
            w.append("EVT", {"ratio": 0.5})
        with self.assertRaises(ledger.RecordTooLarge):
            w.append("EVT", {"big": "x" * MAX})
        with self.assertRaises(ledger.RecordTooLarge):  # a batch is checked whole before any byte is written
            w.commit(w.prepare([("EVT", {"ok": 1}), ("EVT", {"big": "x" * MAX})]))
        w.close()
        self.assertEqual(self.raw(), before)

    def test_fsync_failure_is_uncertain(self):
        self.write(1)
        w = ledger.LedgerWriter(self.dir, NAME, "r" * 32, MAX, ledger.verify_file(self.path, NAME, MAX).tail, clock)
        w.open()
        with mock.patch("spark_daemon.ledger.os.fsync", side_effect=OSError(5, "EIO")):
            with self.assertRaises(ledger.UncertainCommit):
                w.append("EVT", {"i": 1})
        w.close()

    def test_short_write_is_uncertain(self):
        self.write(1)
        w = ledger.LedgerWriter(self.dir, NAME, "r" * 32, MAX, ledger.verify_file(self.path, NAME, MAX).tail, clock)
        w.open()
        with mock.patch("spark_daemon.ledger.os.write", return_value=3):
            with self.assertRaises(ledger.UncertainCommit):
                w.append("EVT", {"i": 1})
        w.close()

    def test_streaming_pass_memory_does_not_depend_on_length(self):
        import tracemalloc

        def peak_for(n):
            if os.path.exists(self.path):
                os.remove(self.path)
            self.write(n)
            tracemalloc.start()
            ledger.verify_file(self.path, NAME, MAX)
            peak = tracemalloc.get_traced_memory()[1]
            tracemalloc.stop()
            return peak

        small, large = peak_for(100), peak_for(3000)
        self.assertLess(large, small * 1.5 + 64 * 1024,
                        f"peak grew from {small} to {large} bytes with 30x more records")

if __name__ == "__main__":
    unittest.main()
