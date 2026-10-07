"""The one judge (r4.11, R-7): precheck, validate and the battery are profiles of one check
registry with one verdict rule. These are the acceptance tests of the owner's r4.9 handoff,
Phase A. Since contract 5 the profiles nest in the order they run (precheck within validate
within battery) and every profile writes the one report, whose verdict is derived on read."""

import os
import re
import unittest
from unittest import mock

from helpers import EXAMPLE, FIXTURES
from spark_daemon import battery, cli, handoff, judge, qualify

OPENER = os.path.join(FIXTURES, "opener", "manifest.json")      # impure: calls open()
GOOD = os.path.join(EXAMPLE, "manifest.json")


def by_id(checks):
    return {c.id if hasattr(c, "id") else c["id"]: c for c in checks}


class RegistryTests(unittest.TestCase):
    def test_ids_are_unique_and_reserved_ids_stay_free(self):
        ids = sorted(judge.registry())
        self.assertTrue(all(re.fullmatch(r"DB-\d\d", i) for i in ids), ids)
        for reserved in ("DB-19", "DB-21", "DB-23"):
            self.assertNotIn(reserved, ids)

    def test_profiles_nest_and_the_battery_runs_everything(self):
        precheck, validate, full = (set(judge.profile_ids(p)) for p in ("precheck", "validate", "battery"))
        self.assertLess(precheck, validate)
        self.assertLess(validate, full)
        self.assertEqual(full, set(judge.registry()))

    def test_precheck_starts_no_process_and_validate_needs_no_extra_tool(self):
        reg = judge.registry()
        self.assertTrue(all(reg[i].kind == "static" for i in judge.profile_ids("precheck")))
        # DB-08/09 need strace and DB-15 systemd-analyze; validate must not depend on them.
        self.assertFalse({"DB-08", "DB-09", "DB-15"} & set(judge.profile_ids("validate")))

    def test_the_renamed_profiles_say_so(self):
        self.assertEqual(sorted(judge.RENAMED), ["precheck", "validate"])
        self.assertIn("were called validate", judge.RENAMED["precheck"])
        self.assertIn("was called precheck", judge.RENAMED["validate"])

    def test_one_verdict_rule(self):
        self.assertEqual(judge.verdict(["PASS", "N/A", "SKIPPED"]), "PASS")
        self.assertEqual(judge.verdict(["PASS", "UNKNOWN"]), "INCOMPLETE")
        self.assertEqual(judge.verdict(["UNKNOWN", "FAIL"]), "FAIL")


class OneInvariantOneIdTests(unittest.TestCase):
    """The same failing invariant yields the same check ID and reason in every profile that
    contains it."""

    def test_impure_code_fails_db02_identically_everywhere(self):
        seen = {}
        for profile in ("precheck", "validate", "battery"):
            j = judge.run(profile, OPENER)
            c = by_id(j.checks)["DB-02"]
            self.assertEqual((j.result, c.state), ("FAIL", "FAIL"), profile)
            seen[profile] = c.evidence
            for other in j.checks:
                if judge.registry()[other.id].kind == "run":
                    self.assertEqual(other.state, "SKIPPED", (profile, other.id))
        self.assertEqual(len(set(seen.values())), 1, seen)

    def test_a_mutation_to_shared_judge_logic_is_caught_by_every_profile(self):
        def broken(j, c):
            c.state, c.evidence = "FAIL", "mutated unit lint"
        with mock.patch.object(judge, "_db25", broken):
            for profile in ("precheck", "validate", "battery"):
                with self.subTest(profile=profile):
                    j = judge.run(profile, GOOD)
                    self.assertEqual(j.result, "FAIL")
                    self.assertEqual(by_id(j.checks)["DB-25"].evidence, "mutated unit lint")


class NoStandaloneJudgeTests(unittest.TestCase):
    def test_validate_and_precheck_take_their_verdict_from_the_judge(self):
        reports = [judge.report(judge.run(p, GOOD)) for p in ("precheck", "validate")]
        with mock.patch.object(judge, "verdict", lambda states: "INCOMPLETE"):
            for rep in reports:
                self.assertEqual(judge.conclusions(rep)["result"], "INCOMPLETE")

    def test_no_module_but_the_judge_decides_a_verdict(self):
        for module in (handoff, battery, cli, qualify):
            with open(module.__file__) as fh:
                source = fh.read()
            self.assertNotIn('"INCOMPLETE"', source, module.__name__)
            self.assertNotIn("PC-0", source, module.__name__)

    def test_reports_bind_the_manifest_as_written(self):
        # An absolute output_dir is rewritten into the battery's workspace. The report must
        # still name the manifest that a unit will run, not the workspace's copy.
        import json
        import shutil
        import tempfile
        tmp = tempfile.mkdtemp()
        try:
            with open(GOOD) as fh:
                data = json.load(fh)
            data["output_dir"] = "/srv/example/logs/meminfo-watch"
            with open(os.path.join(tmp, "manifest.json"), "w") as fh:
                json.dump(data, fh)
            shutil.copy(os.path.join(EXAMPLE, "daemon.py"), tmp)
            j = judge.run("validate", os.path.join(tmp, "manifest.json"))
            from spark_daemon import manifest
            self.assertEqual(j.m.sha256, manifest.load(os.path.join(tmp, "manifest.json")).sha256)
            # Contract 5 (E-3): no copy. The run checks bind the same manifest, and the absolute
            # output_dir is redirected by one recorded harness parameter.
            self.assertIs(j.ws.m, j.m)
            rep = judge.report(j)
            self.assertEqual(rep["manifest_sha256"], j.m.sha256)
            self.assertEqual(rep["manifest"]["output_dir"], "/srv/example/logs/meminfo-watch")
            self.assertEqual(rep["harness"]["output_dir"], os.path.join(j.ws.home, ".battery-output", "meminfo-watch"))
            self.assertEqual(sorted(os.listdir(tmp)), ["daemon.py", "manifest.json"])     # nothing written beside it
            self.assertEqual(j.result, "PASS", [(c.id, c.state, c.evidence) for c in j.checks])
        finally:
            shutil.rmtree(tmp)


if __name__ == "__main__":
    unittest.main()
