"""The six invariants (contract 5, E-8): each names where it is enforced and the battery checks
and self-tests that hold it. An invariant that loses its own check when merged is the way E-8
could go wrong, so every named check and test must exist."""

import os
import unittest

from helpers import ROOT
from spark_daemon import contract, judge


def all_test_ids():
    loader = unittest.defaultTestLoader
    suite = loader.discover(os.path.join(ROOT, "tests"), top_level_dir=os.path.join(ROOT, "tests"))

    def walk(s):
        for item in s:
            if isinstance(item, unittest.TestSuite):
                yield from walk(item)
            else:
                yield item.id()
    return set(walk(suite))


class InvariantTests(unittest.TestCase):
    def test_there_are_six_and_they_are_published(self):
        published = contract.describe()["contract"]["invariants"]["list"]
        self.assertEqual([i["id"] for i in published], ["I-1", "I-2", "I-3", "I-4", "I-5", "I-6"])

    def test_every_owner_and_inv_item_is_absorbed_once(self):
        absorbed = [a for i in contract.INVARIANTS for a in i["absorbs"]]
        expected = [f"owner {n}" for n in range(1, 8)] + [f"INV-{n}" for n in range(1, 10)]
        self.assertEqual(sorted(absorbed), sorted(expected))      # owner 8 is the preamble
        self.assertIn("No change may relax", contract.INVARIANTS_PREAMBLE)

    def test_every_named_check_and_test_exists(self):
        checks, tests = set(judge.registry()), all_test_ids()
        for inv in contract.INVARIANTS:
            with self.subTest(invariant=inv["id"]):
                self.assertTrue(inv["checks"] and inv["tests"] and inv["enforced_in"])
                self.assertEqual([c for c in inv["checks"] if c not in checks], [])
                self.assertEqual([t for t in inv["tests"] if t not in tests], [])


if __name__ == "__main__":
    unittest.main()
