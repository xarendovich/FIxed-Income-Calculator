"""The one judge (r4.11, R-7): validate, precheck and the battery are profiles of one check
registry with one verdict rule. These are the acceptance tests of the owner's r4.9 handoff,
Phase A."""

import os
import re
import unittest
from unittest import mock

from helpers import EXAMPLE, FIXTURES
from spark_daemon import battery, handoff, judge

OPENER = os.path.join(FIXTURES, "opener", "manifest.json")      # impure: calls open()
GOOD = os.path.join(EXAMPLE, "manifest.json")


def by_id(checks):
    return {c.id if hasattr(c, "id") else c["id"]: c for c in checks}


class RegistryTests(unittest.TestCase):
    def test_ids_are_unique_and_reserved_ids_stay_free(self):
        ids = sorted(judge.registry())
        self.assertTrue(all(re.fullmatch(r"DB-\d\d", i) for i in ids), ids)
        for reserved in ("DB-19", "DB-21", "DB-22", "DB-23"):
            self.assertNotIn(reserved, ids)

    def test_profiles_nest_and_the_battery_runs_everything(self):
        validate, precheck, full = (set(judge.profile_ids(p)) for p in ("validate", "precheck", "battery"))
        self.assertLess(validate, precheck)
        self.assertLess(precheck, full)
        self.assertEqual(full, set(judge.registry()))

    def test_validate_starts_no_process_and_precheck_needs_no_extra_tool(self):
        reg = judge.registry()
        self.assertTrue(all(reg[i].kind == "static" for i in judge.profile_ids("validate")))
        # DB-08/09 need strace and DB-15 systemd-analyze; precheck must not depend on them.
        self.assertFalse({"DB-08", "DB-09", "DB-15"} & set(judge.profile_ids("precheck")))

    def test_only_the_full_battery_qualifies(self):
        for profile, quick, expected in (("validate", False, False), ("precheck", False, False),
                                         ("battery", True, False), ("battery", False, True)):
            with self.subTest(profile=profile, quick=quick):
                self.assertEqual(judge.Judgement(profile, GOOD, quick=quick).qualifying, expected)

    def test_one_verdict_rule(self):
        self.assertEqual(judge.verdict(["PASS", "N/A", "SKIPPED"]), "PASS")
        self.assertEqual(judge.verdict(["PASS", "UNKNOWN"]), "INCOMPLETE")
        self.assertEqual(judge.verdict(["UNKNOWN", "FAIL"]), "FAIL")


class OneInvariantOneIdTests(unittest.TestCase):
    """The same failing invariant yields the same check ID and reason in every profile that
    contains it."""

    def test_impure_code_fails_db02_identically_everywhere(self):
        seen = {}
        for profile in ("validate", "precheck", "battery"):
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
            for profile in ("validate", "precheck", "battery"):
                with self.subTest(profile=profile):
                    j = judge.run(profile, GOOD)
                    self.assertEqual(j.result, "FAIL")
                    self.assertEqual(by_id(j.checks)["DB-25"].evidence, "mutated unit lint")


class NoStandaloneJudgeTests(unittest.TestCase):
    def test_validate_and_precheck_take_their_verdict_from_the_judge(self):
        with mock.patch.object(judge, "verdict", lambda states: "INCOMPLETE"):
            self.assertEqual(handoff.validate_report(GOOD)[0]["result"], "INCOMPLETE")
            self.assertEqual(handoff.precheck_report(GOOD)["result"], "INCOMPLETE")

    def test_no_module_but_the_judge_decides_a_verdict(self):
        for module in (handoff, battery):
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
            j = judge.run("precheck", os.path.join(tmp, "manifest.json"))
            from spark_daemon import manifest
            self.assertEqual(j.m.sha256, manifest.load(os.path.join(tmp, "manifest.json")).sha256)
            self.assertNotEqual(j.ws.m.sha256, j.m.sha256)
            self.assertEqual(j.result, "PASS", [(c.id, c.state, c.evidence) for c in j.checks])
        finally:
            shutil.rmtree(tmp)


if __name__ == "__main__":
    unittest.main()
