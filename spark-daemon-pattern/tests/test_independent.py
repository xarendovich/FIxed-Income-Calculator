"""One interpretation, two verifications (r4.11, R-4): start-up and `status` share one pure
interpreter (semantics.py); bytes and hashes are verified by two independent implementations
(spark_daemon/ledger.py and verifier/ledger_verify.py). The acceptance tests of the owner's r4.9
handoff, Phase D."""

import ast
import datetime
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

from helpers import ENTRY, FIXTURES, ROOT, Sandbox
from spark_daemon import EXIT_SENSE_BLIND, ledger, semantics, status
from spark_daemon.battery import corrupted_copies

MODE_DRIVEN = '''
def sense(ctx):
    if ctx.read_text("~/data/mode.txt").strip() == "busy":
        return ctx.unsettled("BUILD_RUNNING")
    return {"value": ctx.read_text("~/data/value.txt").strip()[:50]}

def decide(prev, snapshot):
    if prev == snapshot:
        return []
    return [("VALUE_OBSERVED", {"value": snapshot["value"]})]
'''


def mode_driven_fixture():
    tmp = tempfile.mkdtemp(prefix="spark-independent-")
    shutil.copyfile(os.path.join(FIXTURES, "counter", "manifest.json"), os.path.join(tmp, "manifest.json"))
    with open(os.path.join(tmp, "daemon.py"), "w") as fh:
        fh.write(MODE_DRIVEN)
    return tmp


class Ledgers(unittest.TestCase):
    def setUp(self):
        self.fixture = mode_driven_fixture()
        self.addCleanup(shutil.rmtree, self.fixture)
        self.sb = Sandbox(self.fixture)
        self.addCleanup(shutil.rmtree, self.sb.tmp)
        self.sb.write("data/mode.txt", "idle")
        self.sb.write("data/value.txt", "caf\u00e9 \u2615 \u2028 line")     # non-ASCII and a JCS-literal separator
        self.assertEqual(self.sb.run(cycles=3).returncode, 0)
        self.name = self.sb.data["name"]
        self.max_bytes = self.sb.data["ledger"]["record_max_bytes"]
        home = mock.patch.dict(os.environ, {"SPARK_DAEMON_HOME": self.sb.home})   # "~" as the daemon sees it
        home.start()
        self.addCleanup(home.stop)

    def lines(self):
        with open(self.sb.ledger_path, "rb") as fh:
            return fh.readlines()

    def write(self, lines, name):
        path = os.path.join(self.sb.tmp, name)
        with open(path, "wb") as fh:
            fh.write(b"".join(lines))
        return path


class IndependenceTests(unittest.TestCase):
    def test_the_verifier_imports_nothing_from_the_daemon_package(self):
        with open(status.VERIFIER_PATH) as fh:
            tree = ast.parse(fh.read())
        imported = {a.name.split(".")[0] for n in ast.walk(tree) if isinstance(n, ast.Import) for a in n.names}
        imported |= {(n.module or "").split(".")[0] for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
        self.assertEqual(imported - {"hashlib", "json", "sys", "argparse"}, set())

    def test_it_runs_on_its_own_from_anywhere(self):
        tmp = tempfile.mkdtemp()
        try:
            copy = os.path.join(tmp, "ledger_verify.py")
            shutil.copy(status.VERIFIER_PATH, copy)
            empty = os.path.join(tmp, "ledger.jsonl")
            open(empty, "wb").close()
            p = subprocess.run([sys.executable, "-I", "-B", copy, empty, "--daemon", "x"], cwd=tmp,
                               capture_output=True, text=True, timeout=30, env={"PATH": "/usr/bin:/bin"})
            self.assertEqual(p.returncode, 0, p.stderr)
            self.assertEqual(json.loads(p.stdout)["records"], 0)
        finally:
            shutil.rmtree(tmp)


class AgreementTests(Ledgers):
    def test_both_verifiers_agree_on_a_real_ledger_and_every_damaged_copy(self):
        self.assertEqual(status.cross_check(self.sb.ledger_path, self.name, self.max_bytes), [])
        lines = self.lines()
        cases = [(label, damaged) for label, damaged, _ in corrupted_copies(lines)]
        record = json.loads(lines[1])
        record["extra"] = 1
        from spark_daemon.canonical import canonical_bytes
        cases += [("an extra envelope key", [lines[0], canonical_bytes(record) + b"\n"] + lines[2:]),
                  ("another schema", [lines[0].replace(b"spark-daemon-ledger/1", b"spark-daemon-ledger/2")] + lines[1:]),
                  ("a torn tail", lines + [b'{"half":'])]
        for i, (label, damaged) in enumerate(cases):
            with self.subTest(label):
                path = self.write(damaged, f"case-{i}.jsonl")
                self.assertEqual(status.cross_check(path, self.name, self.max_bytes), [])
        for other, limit in (("someone-else", self.max_bytes), (self.name, 64)):
            self.assertEqual(status.cross_check(self.sb.ledger_path, other, limit), [], (other, limit))

    def test_a_mutated_primary_verifier_is_caught_by_the_independent_one(self):
        lines = self.lines()
        label, damaged, expected = corrupted_copies(lines)[0]
        self.assertEqual(expected, ("LINK", 2))
        path = self.write(damaged, "link.jsonl")
        real = ledger.verify_line

        def lenient(body, expected_seq, expected_prev, *rest):
            record, sha = real(body, expected_seq, json.loads(body)["prev_sha256"], *rest)   # ignores the link
            return record, sha
        with mock.patch.object(ledger, "verify_line", lenient):
            self.assertTrue(any("intact" in d for d in status.cross_check(path, self.name, self.max_bytes)))

    def test_a_mutated_independent_verifier_is_caught_by_the_primary(self):
        verifier = status.load_verifier()
        ascii_encoder = lambda value, depth=0: json.dumps(value, separators=(",", ":"), sort_keys=True)  # noqa: E731
        with mock.patch.object(verifier, "_encode", ascii_encoder):
            self.assertTrue(status.cross_check(self.sb.ledger_path, self.name, self.max_bytes))
        self.assertEqual(status.cross_check(self.sb.ledger_path, self.name, self.max_bytes), [])


class OneInterpretationTests(Ledgers):
    def now(self, seconds_later=0.0):
        from spark_daemon import runtime
        return semantics.Now(runtime._boot_id(), runtime._boottime_ms() + int(seconds_later * 1000),
                             datetime.datetime.now(datetime.timezone.utc) + datetime.timedelta(seconds=seconds_later))

    def test_startup_and_status_agree_given_the_same_verified_facts(self):
        limit = self.sb.data["blind_limit_seconds"]
        for later in (0.0, 30.0, limit * 2.0):
            at = self.now(later)
            evidence = semantics.AcceptEvidence()                 # start-up's path: the skeleton's verifier
            ledger.verify_file(self.sb.ledger_path, self.name, self.max_bytes, on_record=evidence.observe)
            startup = semantics.blindness(evidence, at, limit)
            report = status.status(self.sb.manifest, at=at)        # status's path: the independent verifier
            self.assertEqual(report["integrity"], "CHAIN_INTACT")
            self.assertEqual((report["blind_ms"], report["clock_basis"]), (int(startup[0] * 1000), startup[1]))

    def test_a_hash_invalid_chain_never_reaches_the_interpreter(self):
        lines = self.lines()
        damaged = corrupted_copies(lines)[0][1]
        with open(self.sb.ledger_path, "wb") as fh:
            fh.write(b"".join(damaged))
        with mock.patch.object(semantics, "interpret", side_effect=AssertionError("interpreted a broken chain")):
            report = status.status(self.sb.manifest)
        self.assertEqual((report["integrity"], report["state"]), ("CORRUPT", "ledger_corrupt"))
        facts = status.load_verifier().verify(self.sb.ledger_path, self.name, self.max_bytes, keep_records=True)
        with self.assertRaises(ValueError):
            semantics.Verified.from_facts(facts)
        with self.assertRaises(TypeError):
            semantics.interpret(facts["record_list"], self.now(), 60)

    def test_states_a_reader_sees(self):
        report = status.status(self.sb.manifest)
        self.assertEqual((report["state"], report["reason"]), ("stopped", "clean: max_cycles"))
        os.remove(self.sb.ledger_path)
        self.assertEqual(status.status(self.sb.manifest)["state"], "never_started")


class InterpreterMutationTests(unittest.TestCase):
    """Mutating the shared interpreter must fail end-to-end behaviour: the HF-32 restart loop
    (test_blind.py) reaches SENSE_BLIND with the real interpreter and does not with a copy of
    the package whose blindness() forgets inherited time."""

    def restart_loop(self, entry):
        fixture = mode_driven_fixture()
        sb = Sandbox(fixture)
        try:
            sb.write("data/mode.txt", "busy")
            for _ in range(5):
                p = subprocess.Popen([sys.executable, "-I", "-B", entry, *sb.argv(None, blind_limit_ms=1500)[4:]],
                                     env=sb.env(), stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                try:
                    return p.wait(timeout=1.0)
                except subprocess.TimeoutExpired:
                    p.kill()
                    p.wait()
            return None
        finally:
            shutil.rmtree(fixture)
            shutil.rmtree(sb.tmp)

    def test_forgetting_inherited_blindness_breaks_the_restart_loop_check(self):
        tmp = tempfile.mkdtemp(prefix="spark-mutant-")
        try:
            for part in ("spark_daemon", "bin", "verifier"):
                shutil.copytree(os.path.join(ROOT, part), os.path.join(tmp, part),
                                ignore=shutil.ignore_patterns("__pycache__"))
            path = os.path.join(tmp, "spark_daemon", "semantics.py")
            with open(path) as fh:
                source = fh.read()
            needle = 'return max(0.0, (now.boottime_ms - evidence.anchor[1]) / 1000), "boottime"'
            self.assertIn(needle, source)
            with open(path, "w") as fh:
                fh.write(source.replace(needle, 'return 0.0, "boottime"'))
            self.assertEqual(self.restart_loop(ENTRY), EXIT_SENSE_BLIND)
            self.assertIsNone(self.restart_loop(os.path.join(tmp, "bin", "spark-daemon")))
        finally:
            shutil.rmtree(tmp)


if __name__ == "__main__":
    unittest.main()


class CorruptionIsNeverACrashTests(Ledgers):
    """HF-43 and HF-44, found by the contract 5 verification pass. A ledger line that is merely
    corrupt (or tampered with) crashed a verifier instead of being reported as a break: the
    daemon exited 1 (restarted by systemd) instead of 65, and `status` crashed instead of
    reporting CORRUPT. HF-43 (very deep nesting) crashed both verifiers alike, so DB-22 could
    not see it; HF-44 (a lone surrogate escaped in a key) crashed only the primary one, and a
    differential fuzz of the two verifiers found it."""

    def cases(self):
        last = self.lines()[-1].rstrip(b"\n")
        assert b'"payload":{' in last
        return {
            "deep nesting (HF-43)": b"[" * 3000 + b"]" * 3000,
            # A whole record, envelope intact, with a lone surrogate escaped in a payload key.
            "lone-surrogate key (HF-44)": last.replace(b'"payload":{', b'"payload":{"\\ud800":1,', 1),
        }

    def test_both_verifiers_report_a_break_and_agree(self):
        good = self.lines()
        for name, line in self.cases().items():
            with self.subTest(name):
                path = self.write(good + [line + b"\n"], "bad.jsonl")
                self.assertEqual(status.cross_check(path, self.name, self.max_bytes), [])
                with self.assertRaises(ledger.LedgerCorrupt):
                    ledger.verify_file(path, self.name, self.max_bytes)
                facts = status.load_verifier().verify(path, self.name, self.max_bytes)
                self.assertFalse(facts["intact"])
                self.assertEqual(facts["break"]["seq"], len(good) + 1)

    def test_the_daemon_refuses_with_65_and_status_says_corrupt(self):
        good = b"".join(self.lines())
        for name, line in self.cases().items():
            with self.subTest(name):
                with open(self.sb.ledger_path, "wb") as fh:
                    fh.write(good + line + b"\n")
                p = self.sb.run(cycles=1)
                self.assertEqual(p.returncode, 65, p.stderr)
                self.assertNotIn("Traceback", p.stderr)
                with open(self.sb.ledger_path, "rb") as fh:
                    self.assertEqual(fh.read(), good + line + b"\n")          # nothing changed
                s = self.sb.cli("status", "--json", "--manifest", self.sb.manifest)
                self.assertEqual(json.loads(s.stdout)["integrity"], "CORRUPT", s.stderr)
                v = self.sb.cli("status", "--verify-only", "--manifest", self.sb.manifest)
                self.assertEqual(v.returncode, 1)
                self.assertTrue(v.stdout.strip().endswith("RESULT: FAIL"), v.stderr)


class DifferentialFuzzTests(Ledgers):
    """The two verifiers on seeded random mutations of a real ledger: same outcome every time,
    and never a crash. A 20,000-mutation run of the same generator found HF-44."""

    TOKENS = [b"true", b"1", b"-0", b"1.0", b"1e2", b"\\ud800", b"\\u0000", b"\r", b" ", b"\xef\xbb\xbf",
              b"\x80", b'"', b"\\", b"{", b"}", b"[", b"]", b",", b":", b"null", b"9007199254740993",
              b"\\u2028", b"\n", b"[" * 1500, b'{"\\udc00":0}']

    def test_seeded_mutations_agree(self):
        import random
        good = b"".join(self.lines())
        rng = random.Random(20261007)
        path = os.path.join(self.sb.tmp, "fuzz.jsonl")
        for i in range(1500):
            data = bytearray(good)
            for _ in range(rng.choice((1, 1, 2, 3))):
                op, at = rng.randrange(5), rng.randrange(len(data) + 1)
                if op == 0 and data:
                    data[min(at, len(data) - 1)] = rng.randrange(256)
                elif op == 1:
                    data[at:at] = rng.choice(self.TOKENS)
                elif op == 2 and data:
                    del data[at:at + rng.randrange(1, 4)]
                elif op == 3:
                    data = data[:at]
                else:
                    lines = bytes(data).split(b"\n")
                    j = rng.randrange(len(lines))
                    lines.insert(j, lines[j])
                    data = bytearray(b"\n".join(lines))
            with open(path, "wb") as fh:
                fh.write(data)
            with self.subTest(mutation=i):
                a = status.primary_outcome(path, self.name, self.max_bytes)
                b = status.independent_outcome(path, self.name, self.max_bytes)
                self.assertEqual(a, b)
                if a.get("intact") and a.get("head_seq"):
                    # What the chain guarantees: every record before the head is the original. The
                    # head itself can be altered undetectably (no later record links to it); see
                    # the residual on anchoring the head (HARDENING.md, residual risk 5).
                    k = a["head_seq"]
                    self.assertEqual(bytes(data).split(b"\n")[:k - 1], good.split(b"\n")[:k - 1])
