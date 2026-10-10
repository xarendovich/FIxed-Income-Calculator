"""One canonical path policy (contract 5, seam 11): the Python checks, the Landlock grants and the
unit's path directives are projections of one PathPolicy, and they agree on every shipped
manifest. Enforcement stays diverse; the policy it enforces is single."""

import glob
import os
import shutil
import tempfile
import unittest
from unittest import mock

from helpers import FIXTURES, ROOT
from spark_daemon import guard, landlock, manifest, pathpolicy, unitgen
from spark_daemon.pathpolicy import PathPolicy

MANIFESTS = sorted(glob.glob(os.path.join(ROOT, "examples", "*", "manifest.json"))
                   + glob.glob(os.path.join(FIXTURES, "*", "manifest.json")))


class AgreementTests(unittest.TestCase):
    def setUp(self):
        self.home = tempfile.mkdtemp(prefix="pathpolicy-home-")
        self.addCleanup(shutil.rmtree, self.home)

    def projections(self, path):
        m = manifest.load(path)
        policy = PathPolicy.of(m, home=self.home)
        text = unitgen.generate(m, root=ROOT, daemon_home=self.home)
        return m, policy, text

    def test_the_three_projections_agree_on_every_shipped_manifest(self):
        self.assertGreaterEqual(len(MANIFESTS), 10)
        extra = landlock.system_read_paths()
        for path in MANIFESTS:
            with self.subTest(manifest=os.path.relpath(path, ROOT)):
                _, policy, text = self.projections(path)
                self.assertEqual(pathpolicy.agreement(policy, text, extra), [])

    def test_guard_policy_is_the_same_policy(self):
        for path in MANIFESTS:
            with self.subTest(manifest=os.path.relpath(path, ROOT)):
                m, policy, _ = self.projections(path)
                g = guard.Policy(m, home=self.home)
                self.assertEqual(g.paths, policy)
                self.assertEqual((g.output_dir, g.reads, g.deny), (policy.output_dir, policy.reads, policy.deny))

    def test_a_drifted_unit_is_caught(self):
        path = os.path.join(ROOT, "examples", "dir-watch", "manifest.json")
        _, policy, text = self.projections(path)
        widened = text.replace(f"ReadWritePaths={policy.output_dir}",
                               f"ReadWritePaths={policy.output_dir} {policy.reads[0]}")
        self.assertTrue(any(p.startswith("writable:") for p in pathpolicy.agreement(policy, widened)))
        no_deny = "\n".join(line for line in text.splitlines() if not line.startswith("InaccessiblePaths="))
        self.assertTrue(any(p.startswith("denied:") for p in pathpolicy.agreement(policy, no_deny)))

    def test_a_drifted_kernel_grant_is_caught(self):
        path = os.path.join(ROOT, "examples", "dir-watch", "manifest.json")
        _, policy, text = self.projections(path)
        real = pathpolicy.grants

        def wider(p, extra=()):
            return real(p, extra) + [(p.deny[0], pathpolicy.READ)]
        with mock.patch.object(pathpolicy, "grants", wider):
            problems = pathpolicy.agreement(policy, text)
        self.assertTrue(any("inside the denied" in p for p in problems), problems)

    def test_a_drifted_python_check_is_caught(self):
        path = os.path.join(ROOT, "examples", "dir-watch", "manifest.json")
        _, policy, text = self.projections(path)
        with mock.patch.object(PathPolicy, "denied", lambda self, full: False):
            problems = pathpolicy.agreement(policy, text)
        self.assertTrue(any("python allows the denied" in p for p in problems), problems)

    def test_the_layers_use_the_projections(self):
        """Each layer derives its paths from pathpolicy, not from the manifest on its own."""
        for name, needle in (("landlock.py", "pathpolicy.grants("), ("unitgen.py", "unit_paths(PathPolicy.of("),
                             ("guard.py", "PathPolicy.of(")):
            with open(os.path.join(ROOT, "spark_daemon", name)) as fh:
                source = fh.read()
            self.assertIn(needle, source, name)
            self.assertNotIn("m.all_deny", source, name)
            self.assertNotIn("manifest.all_deny", source, name)

    def test_expansion_uses_the_policys_home_not_the_environment(self):
        path = os.path.join(ROOT, "examples", "dir-watch", "manifest.json")
        m = manifest.load(path)
        with mock.patch.dict(os.environ, {"SPARK_DAEMON_HOME": "/nonexistent-elsewhere"}):
            policy = PathPolicy.of(m, home=self.home)
            self.assertTrue(policy.resolve("~/x").startswith(os.path.realpath(self.home)))
            self.assertTrue(policy.output_dir.startswith(os.path.realpath(self.home)))

    def test_resolved_policy_evidence_is_closed_absolute_and_exactly_from_the_instance(self):
        path = os.path.join(ROOT, "examples", "dir-watch", "manifest.json")
        m = manifest.load(path)
        policy = PathPolicy.of(m, home=self.home)
        evidence = policy.to_evidence()
        self.assertEqual(set(evidence), {"home", "reads", "output_dir", "deny"})
        self.assertEqual(evidence["home"], policy.home)
        self.assertEqual(evidence["reads"], sorted(policy.reads))
        self.assertEqual(evidence["output_dir"], policy.output_dir)
        self.assertEqual(evidence["deny"], sorted(policy.deny))
        for value in [evidence["home"], evidence["output_dir"], *evidence["reads"], *evidence["deny"]]:
            self.assertTrue(os.path.isabs(value))
            self.assertNotIn("~", value)
        self.assertNotIn("landlock_abi", evidence)


if __name__ == "__main__":
    unittest.main()
