"""Qualification (contract 5, E-10 and E-11): the report holds facts and its conclusions are derived
on read; the installable unit is a projection of a qualifying report, reproduced from the report
alone; installing checks that the report qualifies and was made on this host, and the runtime
checks the files at every start, so neither gate repeats the other's predicate. Qualification
never activates anything. (r4.11's acceptance tests for R-3, rewritten for the report: see
evidence/v5/cut2-accounting.md.)"""

import copy
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest

from helpers import ENTRY, EXAMPLE, FIXTURES, ROOT, Sandbox
from spark_daemon import EXIT_FAILED, EXIT_POLICY, EXIT_USAGE, SYSTEM_PATH, judge, qualify

OPTIONS = ["--part-of", "project-stack.service", "--require-path", "/mnt/project/.volume-marker"]


def cli(*args, env, timeout=600):
    return subprocess.run([sys.executable, "-I", "-B", ENTRY, *args], capture_output=True, text=True,
                          timeout=timeout, env=env)


class ConclusionTests(unittest.TestCase):
    """The verdict and "qualifies" are derived from the checks; a report cannot claim them."""

    @classmethod
    def setUpClass(cls):
        j = judge.run("precheck", os.path.join(EXAMPLE, "manifest.json"))
        cls.static = judge.report(j)
        battery = copy.deepcopy(cls.static)
        battery["profile"] = "battery"
        present = {c["id"] for c in battery["checks"]}
        battery["checks"] += [{"id": i, "title": "", "state": "PASS", "evidence": "", "diagnostics": []}
                              for i in judge.profile_ids("battery") if i not in present]
        battery["checks"].sort(key=lambda c: c["id"])
        cls.battery = battery

    def mutated(self, change):
        rep = copy.deepcopy(self.battery)
        change(rep)
        return judge.conclusions(rep)

    def test_no_conclusion_is_stored(self):
        for key in ("result", "qualifying", "qualifies", "valid", "error_count", "warning_count"):
            self.assertNotIn(key, self.static)
        self.assertEqual(judge.conclusions(self.static)["result"], "PASS")

    def test_only_a_complete_full_battery_pass_qualifies(self):
        self.assertEqual(judge.conclusions(self.battery), {"result": "PASS", "qualifies": True, "why_not": []})
        self.assertFalse(judge.conclusions(self.static)["qualifies"])
        cases = {
            "quick": (lambda r: r.update(quick=True), "--quick"),
            "validate profile": (lambda r: r.update(profile="validate"), "validate profile"),
            "an UNKNOWN check": (lambda r: r["checks"][5].update(state="UNKNOWN"), "INCOMPLETE"),
            "a FAIL check": (lambda r: r["checks"][5].update(state="FAIL"), "FAIL"),
            "a dropped check": (lambda r: r["checks"].pop(7), "exactly the battery's checks"),
            "an unknown state": (lambda r: r["checks"][5].update(state="GREAT"), "FAIL"),
            "no unit": (lambda r: r["unit"].update(unit_sha256=None), "complete unit"),
        }
        for name, (change, reason) in cases.items():
            with self.subTest(name):
                found = self.mutated(change)
                self.assertFalse(found["qualifies"])
                self.assertTrue(any(reason in w for w in found["why_not"]), found)

    def test_a_stored_result_field_changes_nothing(self):
        found = self.mutated(lambda r: (r["checks"][5].update(state="FAIL"), r.update(result="PASS", qualifies=True)))
        self.assertEqual((found["result"], found["qualifies"]), ("FAIL", False))


class RuntimeGateTests(unittest.TestCase):
    def setUp(self):
        self.sb = Sandbox("counter")
        self.sb.write("data/value.txt", "x")

    def tearDown(self):
        shutil.rmtree(self.sb.tmp)

    def test_an_unqualified_start_is_refused_everywhere(self):
        # Contract 5 (E-12): no start without the digests, under the harness either. Replaces
        # test_an_unqualified_start_is_refused_outside_test_mode.
        for production in (True, False):
            with self.subTest(production=production):
                p = self.sb.run(cycles=1, production=production, qualified=False)
                self.assertEqual(p.returncode, EXIT_POLICY, p.stderr)
                self.assertIn("not qualified", p.stderr)
                self.assertEqual(self.sb.records(), [])

    def test_changed_code_is_refused_everywhere(self):
        # Replaces test_changed_code_is_refused_even_in_test_mode.
        argv = {production: self.sb.argv(1, production=production) for production in (True, False)}
        with open(os.path.join(self.sb.daemon_dir, "daemon.py"), "a") as fh:
            fh.write("\n# changed after qualification\n")
        for production, args in argv.items():
            with self.subTest(production=production):
                p = subprocess.run(args, env=self.sb.env(), capture_output=True, text=True, timeout=30)
                self.assertEqual(p.returncode, EXIT_POLICY, p.stderr)
                self.assertIn("daemon_code_sha256", p.stderr)
                self.assertEqual(self.sb.records(), [])

    def test_a_qualified_start_runs_and_says_so(self):
        p = self.sb.run(cycles=1, production=True)
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertIs(self.sb.records()[0]["payload"]["qualified"], True)
        self.assertIsNone(self.sb.records()[0]["payload"]["harness"])

    def test_partial_expectations_are_a_usage_error(self):
        args = self.sb.qualified_args()[:2]
        p = subprocess.run([sys.executable, "-I", "-B", ENTRY, "run", "--manifest", self.sb.manifest, *args],
                           env=self.sb.env(), capture_output=True, text=True, timeout=30)
        self.assertEqual(p.returncode, EXIT_USAGE)


class ProjectedUnitTests(unittest.TestCase):
    """One full battery, then the unit projected from its report, and every way a report stops
    being installable."""

    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.mkdtemp(prefix="qualify-test-")
        cls.author_home = os.path.join(cls.tmp, "author-home")
        os.makedirs(cls.author_home)
        cls.env = {"PATH": SYSTEM_PATH, "HOME": "/nonexistent", "LANG": "C.UTF-8",
                   "SPARK_DAEMON_HOME": cls.author_home}
        cls.daemon = os.path.join(cls.tmp, "daemon")
        shutil.copytree(EXAMPLE, cls.daemon)
        cls.workdir = os.path.join(cls.tmp, "battery")
        cls.p = cli("battery", "--manifest", os.path.join(cls.daemon, "manifest.json"), *OPTIONS,
                    "--workdir", cls.workdir, env=cls.env)
        if "RESULT: PASS" not in cls.p.stdout:
            shutil.rmtree(cls.tmp)
            raise unittest.SkipTest("the full battery is not PASS here (a tool is missing?): "
                                    + cls.p.stdout.strip().splitlines()[-1])
        cls.report_path = os.path.join(cls.workdir, "battery-report.json")
        with open(cls.report_path) as fh:
            cls.report = json.load(fh)
        cls.name = "spark-daemon-meminfo-watch.service"

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.tmp, ignore_errors=True)

    def project(self, report_path=None, *extra):
        out = tempfile.mkdtemp(prefix="qualify-out-")
        self.addCleanup(shutil.rmtree, out)
        p = cli("unit", "--report", report_path or self.report_path, "--out", out, *extra, env=self.env)
        path = os.path.join(out, self.name)
        text = None
        if os.path.exists(path):
            with open(path) as fh:
                text = fh.read()
        return p, text

    def edited(self, change):
        rep = copy.deepcopy(self.report)
        change(rep)
        d = tempfile.mkdtemp(prefix="qualify-report-")
        self.addCleanup(shutil.rmtree, d)
        path = os.path.join(d, "report.json")
        with open(path, "w") as fh:
            json.dump(rep, fh)
        return path

    def test_a_qualifying_report_projects_its_unit(self):
        self.assertIn("qualifies: yes", self.p.stdout)
        p, text = self.project()
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertIn("installing and enabling it are a person's decision", p.stderr)
        self.assertEqual(qualify.file_sha256(os.path.join(os.path.dirname(p.stdout.strip()), self.name)),
                         self.report["unit"]["unit_sha256"])
        self.assertIn("# QUALIFIED by a battery PASS", text)

    def test_the_projection_is_the_unit_the_battery_scored(self):
        # DB-15 ran systemd-analyze on the unit it wrote into the workspace: the same bytes.
        with open(os.path.join(self.workdir, self.name)) as fh:
            scored = fh.read()
        _, first = self.project()
        _, second = self.project()
        self.assertEqual(first, second)
        self.assertEqual(first, scored)

    def test_the_unit_names_the_authors_home_not_the_workspace(self):
        # HF-41: before contract 5 the emitted unit's SPARK_DAEMON_HOME and ReadWritePaths named
        # the battery's disposable workspace.
        _, text = self.project()
        home = os.path.realpath(self.author_home)
        self.assertIn(f"Environment=SPARK_DAEMON_HOME={home}\n", text)
        self.assertIn(f"ReadWritePaths={home}/spark-daemons/meminfo-watch\n", text)
        self.assertNotIn(self.workdir, text)

    def test_the_unit_binds_manifest_code_contract_and_inputs(self):
        _, text = self.project()
        expect = qualify.expected_identity(os.path.join(self.daemon, "manifest.json"))
        exec_start = re.search(r"^ExecStart=.*$", text, re.M).group(0)
        for key, value in expect.items():
            self.assertEqual(self.report["unit"]["expect"][key], value)
            self.assertIn(value, exec_start)
        self.assertEqual(self.report["unit"]["part_of"], "project-stack.service")
        self.assertIn("PartOf=project-stack.service\n", text)
        self.assertIn("activates nothing", self.report["authority"])

    def test_a_report_that_does_not_qualify_is_refused(self):
        def unknown(r):
            next(c for c in r["checks"] if c["id"] == "DB-14")["state"] = "UNKNOWN"
        for name, change, reason in (("quick", lambda r: r.update(quick=True), "--quick"),
                                     ("unknown check", unknown, "INCOMPLETE"),
                                     ("other host", lambda r: r["host"].update(machine_id_sha256="0" * 64),
                                      "host machine_id_sha256")):
            with self.subTest(name):
                p, text = self.project(self.edited(change))
                self.assertEqual(p.returncode, EXIT_FAILED)
                self.assertIn(reason, p.stderr)
                self.assertIsNone(text)

    def test_an_edited_unit_input_does_not_reproduce(self):
        p, text = self.project(self.edited(lambda r: r["unit"].update(part_of="other.service")))
        self.assertEqual(p.returncode, EXIT_FAILED)
        self.assertIn("does not reproduce the unit the battery judged", p.stderr)
        self.assertIsNone(text)

    def test_unit_inputs_cannot_be_changed_at_install_time(self):
        p, _ = self.project(None, "--part-of", "other.service")
        self.assertEqual(p.returncode, EXIT_USAGE)

    def test_changed_code_is_the_runtimes_refusal_not_the_installers(self):
        code = os.path.join(self.daemon, "daemon.py")
        with open(code) as fh:
            original = fh.read()
        try:
            with open(code, "a") as fh:
                fh.write("\n# changed after qualification\n")
            p, text = self.project()
            self.assertEqual(p.returncode, 0, p.stderr)       # installing does not repeat the runtime's check
            args = re.search(r"^ExecStart=\S+ -I -B \S+ (run .*)$", text, re.M).group(1).split()
            run = cli(*args, "--max-cycles", "1", env=self.env, timeout=60)
            self.assertEqual(run.returncode, EXIT_POLICY, run.stderr)
            self.assertIn("daemon_code_sha256", run.stderr)
        finally:
            with open(code, "w") as fh:
                fh.write(original)

    def test_a_failed_battery_is_not_installable(self):
        workdir = tempfile.mkdtemp(prefix="qualify-fail-")
        self.addCleanup(shutil.rmtree, workdir)
        p = cli("battery", "--manifest", os.path.join(FIXTURES, "opener", "manifest.json"), "--workdir", workdir,
                env=self.env)
        self.assertTrue(p.stdout.strip().endswith("RESULT: FAIL"))
        self.assertIn("qualifies: no", p.stdout)
        u, text = self.project(os.path.join(workdir, "battery-report.json"))
        self.assertEqual(u.returncode, EXIT_FAILED)
        self.assertIn("the result is FAIL", u.stderr)
        self.assertIsNone(text)


class ActivationStaysSeparateTests(unittest.TestCase):
    def test_no_judge_or_qualification_code_installs_or_enables_anything(self):
        for name in ("battery.py", "judge.py", "qualify.py", "handoff.py", "cli.py"):
            with open(os.path.join(ROOT, "spark_daemon", name)) as fh:
                source = fh.read()
            for word in ('"enable"', '"daemon-reload"', '"install"', "'enable'", "'install'"):
                self.assertNotIn(word, source, (name, word))


if __name__ == "__main__":
    unittest.main()
