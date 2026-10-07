"""Manifest: the example validates; every refusal category is enforced."""

import json
import os
import unittest

from helpers import EXAMPLE
from spark_daemon import manifest


def example():
    with open(os.path.join(EXAMPLE, "manifest.json")) as fh:
        return json.load(fh)


class ManifestTests(unittest.TestCase):
    def assertRefused(self, data, fragment):
        with self.assertRaises(manifest.ManifestError) as ctx:
            manifest.parse(data)
        joined = " | ".join(ctx.exception.problems)
        self.assertIn(fragment, joined)

    def test_example_is_valid(self):
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        self.assertEqual(m.name, "meminfo-watch")
        self.assertEqual(len(m.sha256), 64)

    def test_hash_ignores_key_order_and_whitespace(self):
        a = manifest.parse(example())
        b = manifest.parse(json.loads(json.dumps(example(), indent=7, sort_keys=True)))
        self.assertEqual(a.sha256, b.sha256)

    def test_unknown_keys_refused_at_every_level(self):
        self.assertRefused(dict(example(), surprise=1), "unknown key 'surprise'")
        d = example()
        d["resources"]["gpu"] = 1
        self.assertRefused(d, "resources: unknown key 'gpu'")

    def test_missing_key(self):
        d = example()
        del d["cycle_budget_seconds"]
        self.assertRefused(d, "missing key 'cycle_budget_seconds'")

    def test_acting_daemons_refused(self):
        self.assertRefused(dict(example(), daemon_class="act"), "reserved")

    def test_network_refused(self):
        self.assertRefused(dict(example(), network={"mode": "named"}), "relaxation R2")

    def test_commands_refused(self):
        # Contract 5 (R-2): no command list at all, so no list of forbidden names to keep
        # complete. Replaces test_forbidden_commands.
        for cmd in ("bash", "curl", "git"):
            with self.subTest(cmd=cmd):
                self.assertRefused(dict(example(), commands=[cmd]), "removed in contract 5.0.0")

    def test_reads_inside_denied_paths(self):
        for path in ("~/spark-core/data", "~/spark-core/data/spark.db", "~/.ssh/id_ed25519",
                     "~/spark-governance/history"):
            with self.subTest(path=path):
                self.assertRefused(dict(example(), reads=[path]), "inside the denied path")

    def test_output_dir_placement(self):
        self.assertRefused(dict(example(), output_dir="~/spark-core/out"), "must not be inside ~/spark-core")
        self.assertRefused(dict(example(), output_dir="~/spark-governance/history"), "must not be inside")
        self.assertRefused(dict(example(), reads=["~/watched"], output_dir="~/watched/out"),
                           "never observes its own output")

    def test_reserved_event_types(self):
        d = example()
        d["ledger"]["event_types"] = ["DAEMON_START"]
        self.assertRefused(d, "reserved for the skeleton")

    def test_cycle_budget_bounded_by_the_blind_limit(self):
        self.assertRefused(dict(example(), cycle_budget_seconds=901, blind_limit_seconds=1800),
                           "half of blind_limit_seconds")

    def test_system_unit_needs_non_root_user(self):
        self.assertRefused(dict(example(), run_as={"unit": "system"}), "dedicated user")
        self.assertRefused(dict(example(), run_as={"unit": "system", "user": "root"}), "must not be root")

    def test_paths_limited_to_safe_characters(self):
        for path in ("~/has space", "/tmp/a;b", "relative/path", "~/%h", "/tmp/$(x)"):
            with self.subTest(path=path):
                self.assertRefused(dict(example(), reads=[path]), "absolute or ~/ path")

    def test_purpose_cannot_carry_systemd_specifiers(self):
        self.assertRefused(dict(example(), purpose="Uses 100% of nothing, honestly."), "purpose")

    def test_floats_refused(self):
        self.assertRefused(dict(example(), cycle_budget_seconds=40.0), "must be an integer")


if __name__ == "__main__":
    unittest.main()
