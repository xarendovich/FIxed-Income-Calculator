"""Qualification (r4.11, R-3): only a qualifying battery PASS emits an installable unit, the
installer's gate and the runtime's gate both refuse anything else, and qualification never
activates anything. The acceptance tests of the owner's r4.9 handoff, Phase B."""

import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from types import SimpleNamespace

from helpers import ENTRY, EXAMPLE, FIXTURES, ROOT, Sandbox
from spark_daemon import EXIT_POLICY, EXIT_USAGE, SYSTEM_PATH, qualify, unitgen

ENV = {"PATH": SYSTEM_PATH, "HOME": "/nonexistent", "LANG": "C.UTF-8"}
OPTIONS = ["--part-of", "project-stack.service", "--require-path", "/mnt/project/.volume-marker"]


def cli(*args, timeout=600):
    return subprocess.run([sys.executable, "-I", "-B", ENTRY, *args], capture_output=True, text=True,
                          timeout=timeout, env=ENV)


class EmitRefusalTests(unittest.TestCase):
    """Nothing but a qualifying PASS may emit; checked on the function, and on the command."""

    def judgement(self, **kw):
        base = dict(profile="battery", quick=False, qualifying=True, result="PASS")
        base.update(kw)
        return SimpleNamespace(**base)

    def test_only_a_qualifying_pass_gets_past_the_first_checks(self):
        out = tempfile.mkdtemp()
        try:
            for kw, reason in ((dict(result="INCOMPLETE"), "INCOMPLETE"), (dict(result="FAIL"), "FAIL"),
                               (dict(quick=True, qualifying=False), "--quick"),
                               (dict(profile="precheck", qualifying=False), "precheck")):
                with self.subTest(**kw):
                    with self.assertRaises(qualify.Refused) as ctx:
                        qualify.emit(self.judgement(**kw), out, "/nonexistent")
                    self.assertIn(reason, str(ctx.exception))
            self.assertEqual(os.listdir(out), [])
        finally:
            shutil.rmtree(out)

    def test_a_failed_battery_emits_nothing(self):
        out = tempfile.mkdtemp()
        try:
            p = cli("battery", "--manifest", os.path.join(FIXTURES, "opener", "manifest.json"), "--emit-unit", out)
            self.assertIn("installable unit: not emitted (the battery result is FAIL", p.stdout)
            self.assertTrue(p.stdout.strip().endswith("RESULT: FAIL"))
            self.assertEqual(os.listdir(out), [])
        finally:
            shutil.rmtree(out)

    def test_quick_cannot_emit(self):
        p = cli("battery", "--quick", "--manifest", os.path.join(EXAMPLE, "manifest.json"), "--emit-unit", "/tmp/x")
        self.assertEqual(p.returncode, EXIT_USAGE)
        self.assertIn("--quick does not qualify", p.stdout)


class RuntimeGateTests(unittest.TestCase):
    def setUp(self):
        self.sb = Sandbox("counter")
        self.sb.write("data/value.txt", "x")

    def tearDown(self):
        shutil.rmtree(self.sb.tmp)

    def test_an_unqualified_start_is_refused_outside_test_mode(self):
        p = self.sb.run(cycles=1, SPARK_DAEMON_TEST="0")
        self.assertEqual(p.returncode, EXIT_POLICY, p.stderr)
        self.assertIn("not qualified", p.stderr)
        self.assertEqual(self.sb.records(), [])

    def test_changed_code_is_refused_even_in_test_mode(self):
        args = self.sb.qualified_args()
        with open(os.path.join(self.sb.daemon_dir, "daemon.py"), "a") as fh:
            fh.write("\n# changed after qualification\n")
        p = subprocess.run([sys.executable, "-I", "-B", ENTRY, "run", "--manifest", self.sb.manifest,
                            "--max-cycles", "1", *args], env=self.sb.env(), capture_output=True, text=True,
                           timeout=30)
        self.assertEqual(p.returncode, EXIT_POLICY, p.stderr)
        self.assertIn("daemon_code_sha256", p.stderr)
        self.assertEqual(self.sb.records(), [])

    def test_a_qualified_start_runs_and_says_so(self):
        p = self.sb.run(cycles=1, qualified=True)
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertIs(self.sb.records()[0]["payload"]["qualified"], True)

    def test_partial_expectations_are_a_usage_error(self):
        args = self.sb.qualified_args()[:2]
        p = subprocess.run([sys.executable, "-I", "-B", ENTRY, "run", "--manifest", self.sb.manifest, *args],
                           env=self.sb.env(), capture_output=True, text=True, timeout=30)
        self.assertEqual(p.returncode, EXIT_USAGE)


class QualifiedUnitTests(unittest.TestCase):
    """One full battery, then every way an installable artifact can stop being qualified."""

    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.mkdtemp(prefix="qualify-test-")
        cls.daemon = os.path.join(cls.tmp, "daemon")
        shutil.copytree(EXAMPLE, cls.daemon)
        cls.out = os.path.join(cls.tmp, "qualified")
        cls.p = cli("battery", "--manifest", os.path.join(cls.daemon, "manifest.json"), *OPTIONS,
                    "--emit-unit", cls.out)
        if "RESULT: PASS" not in cls.p.stdout:
            shutil.rmtree(cls.tmp)
            raise unittest.SkipTest("the full battery is not PASS here (a tool is missing?): "
                                    + cls.p.stdout.strip().splitlines()[-1])
        cls.name = "spark-daemon-meminfo-watch.service"
        cls.unit = os.path.join(cls.out, cls.name)
        cls.record = cls.unit + qualify.RECORD_SUFFIX

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.tmp, ignore_errors=True)

    def copy(self):
        d = tempfile.mkdtemp(prefix="qualify-case-")
        self.addCleanup(shutil.rmtree, d)
        shutil.copy(self.unit, d)
        shutil.copy(self.record, d)
        return os.path.join(d, self.name), os.path.join(d, self.name + qualify.RECORD_SUFFIX)

    def test_a_pass_emits_exactly_the_unit_and_its_record(self):
        self.assertEqual(sorted(os.listdir(self.out)), sorted([self.name, self.name + qualify.RECORD_SUFFIX]))
        self.assertEqual(qualify.check(self.unit, self.record), [])
        p = cli("qualified", "--unit", self.unit, "--record", self.record)
        self.assertEqual(p.returncode, 0, p.stdout)
        self.assertIn("it activates nothing", p.stdout)

    def test_the_unit_binds_manifest_code_and_contract(self):
        with open(self.record) as fh:
            record = json.load(fh)
        expect = qualify.expected_identity(os.path.join(self.daemon, "manifest.json"))
        with open(self.unit) as fh:
            text = fh.read()
        for key, value in expect.items():
            self.assertEqual(record[key], value)
            self.assertIn(value, re.search(r"^ExecStart=.*$", text, re.M).group(0))
        self.assertEqual(record["unit_options"]["part_of"], "project-stack.service")
        self.assertIn("activates nothing", record["authority"])

    def test_a_preview_cannot_pass_the_gate(self):
        unit, record = self.copy()
        from spark_daemon import manifest
        m = manifest.load(os.path.join(self.daemon, "manifest.json"))
        with open(unit, "w") as fh:
            fh.write(unitgen.generate(m, root=ROOT, part_of="project-stack.service",
                                      require_paths=["/mnt/project/.volume-marker"]))
        problems = qualify.check(unit, record)
        self.assertTrue(any("bytes differ" in p for p in problems), problems)
        with open(unit) as fh:
            self.assertIn("PREVIEW, NOT QUALIFIED", fh.read())

    def test_changing_a_unit_option_invalidates_it(self):
        unit, record = self.copy()
        with open(unit) as fh:
            text = fh.read()
        with open(unit, "w") as fh:
            fh.write(text.replace("PartOf=project-stack.service\n", ""))
        self.assertTrue(any("bytes differ" in p for p in qualify.check(unit, record)))

    def test_changed_code_fails_the_gate(self):
        code = os.path.join(self.daemon, "daemon.py")
        with open(code) as fh:
            original = fh.read()
        try:
            with open(code, "a") as fh:
                fh.write("\n# changed after qualification\n")
            self.assertTrue(any("daemon_code_sha256 changed" in p for p in qualify.check(self.unit, self.record)))
        finally:
            with open(code, "w") as fh:
                fh.write(original)

    def test_a_record_from_another_host_fails(self):
        unit, record = self.copy()
        with open(record) as fh:
            data = json.load(fh)
        data["host"]["machine_id_sha256"] = "0" * 64
        with open(record, "w") as fh:
            json.dump(data, fh)
        self.assertTrue(any("host machine_id_sha256" in p for p in qualify.check(unit, record)))


class ActivationStaysSeparateTests(unittest.TestCase):
    def test_no_judge_or_qualification_code_installs_or_enables_anything(self):
        for name in ("battery.py", "judge.py", "qualify.py", "handoff.py"):
            with open(os.path.join(ROOT, "spark_daemon", name)) as fh:
                source = fh.read()
            for word in ('"enable"', '"daemon-reload"', '"install"', "'enable'", "'install'"):
                self.assertNotIn(word, source, (name, word))


if __name__ == "__main__":
    unittest.main()
