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
                p = subprocess.Popen([sys.executable, "-I", "-B", entry, "run", "--manifest", sb.manifest],
                                     env=sb.env(SPARK_DAEMON_TEST_BLIND_LIMIT_MS="1500"),
                                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
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
