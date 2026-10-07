"""Vendoring under other names (r4.11, R-8b): a deterministic rename map, and a copy bound by the
source tree, map and transformed tree hashes. The acceptance tests of the owner's r4.9 handoff,
Phase F."""

import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

from helpers import ROOT

VENDOR = os.path.join(ROOT, "vendoring", "vendor.py")
MAP = os.path.join(ROOT, "vendoring", "example-rename-map.json")


def load_vendor():
    spec = importlib.util.spec_from_file_location("vendor_under_test", VENDOR)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class VendorTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.v = load_vendor()

    def source_copy(self):
        tmp = tempfile.mkdtemp(prefix="vendor-src-")
        self.addCleanup(shutil.rmtree, tmp)
        for root in self.v.load_map(MAP)["roots"]:
            shutil.copytree(os.path.join(ROOT, root), os.path.join(tmp, root),
                            ignore=shutil.ignore_patterns("__pycache__"))
        return tmp

    def map_copy(self, change=None):
        tmp = tempfile.mkdtemp(prefix="vendor-map-")
        self.addCleanup(shutil.rmtree, tmp)
        with open(MAP) as fh:
            data = json.load(fh)
        if change:
            change(data)
        path = os.path.join(tmp, "map.json")
        with open(path, "w") as fh:
            json.dump(data, fh)
        return path

    def test_the_rename_is_deterministic(self):
        first, _, _ = self.v.binding(ROOT, MAP)
        second, _, _ = self.v.binding(ROOT, MAP)
        self.assertEqual(first, second)
        self.assertGreater(first["files"], 20)

    def test_one_source_byte_changes_the_source_and_transformed_hashes(self):
        src = self.source_copy()
        before, _, _ = self.v.binding(src, MAP)
        path = os.path.join(src, "spark_daemon", "judge.py")
        with open(path, "rb") as fh:
            data = fh.read()
        with open(path, "wb") as fh:
            fh.write(data.replace(b"SHORT_RUN_CYCLES = 2 ", b"SHORT_RUN_CYCLES = 3 ", 1))
        after, _, _ = self.v.binding(src, MAP)
        self.assertNotEqual(before["source_tree_sha256"], after["source_tree_sha256"])
        self.assertNotEqual(before["transformed_tree_sha256"], after["transformed_tree_sha256"])
        self.assertEqual(before["rename_map_sha256"], after["rename_map_sha256"])

    def test_one_map_change_changes_the_map_and_transformed_hashes(self):
        before, _, _ = self.v.binding(ROOT, MAP)
        other = self.map_copy(lambda d: d["replace"][0].__setitem__(1, "other_daemon"))
        after, _, _ = self.v.binding(ROOT, other)
        self.assertNotEqual(before["rename_map_sha256"], after["rename_map_sha256"])
        self.assertNotEqual(before["transformed_tree_sha256"], after["transformed_tree_sha256"])
        self.assertEqual(before["source_tree_sha256"], after["source_tree_sha256"])

    def test_a_copy_renamed_by_the_map_checks_out_and_runs(self):
        out = tempfile.mkdtemp(prefix="vendor-out-")
        self.addCleanup(shutil.rmtree, out)
        run = [sys.executable, "-I", "-B", VENDOR, "--source", ROOT, "--map", MAP]
        p = subprocess.run(run + ["--write", out], capture_output=True, text=True, timeout=60)
        self.assertEqual(p.returncode, 0, p.stderr)
        p = subprocess.run(run + ["--check", out], capture_output=True, text=True, timeout=60)
        self.assertEqual(p.returncode, 0, p.stdout)
        self.assertTrue(json.loads(p.stdout)["checked"]["matches"])
        # Renaming known symbols by the map is not a contract change: the copy runs under its new
        # names and reports the same contract version.
        p = subprocess.run([sys.executable, "-I", "-B", os.path.join(out, "bin", "acme-daemon"), "describe",
                            "--identity"], capture_output=True, text=True, timeout=60)
        self.assertEqual(p.returncode, 0, p.stderr)
        from spark_daemon import contract
        self.assertEqual(json.loads(p.stdout)["contract_version"], contract.CONTRACT_VERSION)
        self.assertFalse(os.path.exists(os.path.join(out, "spark_daemon")))

    def test_an_edited_copy_fails_the_check(self):
        out = tempfile.mkdtemp(prefix="vendor-out-")
        self.addCleanup(shutil.rmtree, out)
        with open(os.devnull, "w") as quiet:
            stdout, sys.stdout = sys.stdout, quiet
            try:
                self.v.main(["--source", ROOT, "--map", MAP, "--write", out])
                with open(os.path.join(out, "acme_daemon", "judge.py"), "ab") as fh:
                    fh.write(b"\n# a local edit\n")
                code = self.v.main(["--source", ROOT, "--map", MAP, "--check", out])
            finally:
                sys.stdout = stdout
        self.assertEqual(code, 1)

    def test_the_tool_imports_nothing_from_the_pattern(self):
        with open(VENDOR) as fh:
            source = fh.read()
        self.assertNotIn("spark_daemon", source.replace("spark-daemon", ""))


if __name__ == "__main__":
    unittest.main()
