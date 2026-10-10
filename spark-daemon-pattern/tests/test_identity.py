"""UDC 6 QualifiedExecutionIdentity: closed semantic root and frozen runtime bundle."""

import copy
import hashlib
import importlib.util
import os
import shutil
import tempfile
import unittest

from helpers import ROOT
from spark_daemon import identity


ZERO_IDENTITY = {
    "contract_sha256": "0" * 64,
    "manifest_sha256": "1" * 64,
    "daemon_code_sha256": "2" * 64,
    "runtime_bundle_sha256": "3" * 64,
    "interpreter": {
        "implementation": "cpython",
        "version": "3.12.3",
        "executable_realpath": "/usr/bin/python3.12",
        "executable_sha256": "4" * 64,
    },
}


class QualifiedExecutionIdentityTests(unittest.TestCase):
    def test_domain_and_golden_root_are_pinned(self):
        self.assertEqual(identity.EXECUTION_DOMAIN, "UDC.EXECUTION.OBSERVE.V1")
        self.assertEqual(
            identity.execution_sha256(ZERO_IDENTITY),
            "fe2b720eb24c5079ebdf01773d3e0f3a314989b62dc0e276d3450555da3cded0",
        )

    def test_schema_is_closed_and_prefixes_are_not_hashes(self):
        extra = copy.deepcopy(ZERO_IDENTITY)
        extra["profile"] = "observe"
        with self.assertRaises(identity.IdentityError):
            identity.execution_sha256(extra)
        missing = copy.deepcopy(ZERO_IDENTITY)
        del missing["manifest_sha256"]
        with self.assertRaises(identity.IdentityError):
            identity.execution_sha256(missing)
        short = copy.deepcopy(ZERO_IDENTITY)
        short["contract_sha256"] = "0" * 16
        with self.assertRaises(identity.IdentityError):
            identity.execution_sha256(short)
        nested = copy.deepcopy(ZERO_IDENTITY)
        nested["interpreter"]["unknown"] = "x"
        with self.assertRaises(identity.IdentityError):
            identity.execution_sha256(nested)

    def test_every_leaf_mutation_changes_the_root(self):
        baseline = identity.execution_sha256(ZERO_IDENTITY)
        mutations = []
        for key, digit in (
            ("contract_sha256", "a"),
            ("manifest_sha256", "b"),
            ("daemon_code_sha256", "c"),
            ("runtime_bundle_sha256", "d"),
        ):
            item = copy.deepcopy(ZERO_IDENTITY)
            item[key] = digit * 64
            mutations.append(item)
        for key, value in (
            ("implementation", "pypy"),
            ("version", "3.12.4"),
            ("executable_realpath", "/opt/python/bin/python3"),
            ("executable_sha256", "e" * 64),
        ):
            item = copy.deepcopy(ZERO_IDENTITY)
            item["interpreter"][key] = value
            mutations.append(item)
        roots = {identity.execution_sha256(item) for item in mutations}
        self.assertEqual(len(roots), len(mutations))
        self.assertNotIn(baseline, roots)

    def test_interpreter_measurement_uses_the_running_executable(self):
        measured = identity.measure_interpreter()
        with open("/proc/self/exe", "rb", buffering=0) as fh:
            expected = hashlib.sha256(fh.read()).hexdigest()
        self.assertEqual(measured["executable_sha256"], expected)
        self.assertEqual(measured["executable_realpath"], os.path.realpath("/proc/self/exe"))
        self.assertTrue(measured["implementation"])
        self.assertTrue(measured["version"])

    def test_environment_predicate_records_names_not_values(self):
        env = {
            "LD_PRELOAD": "/tmp/evil.so",
            "PYTHONPATH": "/tmp/evil",
            "SAFE": "ok",
        }
        self.assertEqual(
            identity.execution_environment_problems(env, isolated=True),
            ("LD_PRELOAD", "PYTHONPATH"),
        )
        self.assertEqual(
            identity.execution_environment_problems({}, isolated=False),
            ("PYTHON_NOT_ISOLATED",),
        )


class RuntimeBundleTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp(prefix="udc-runtime-bundle-")
        self.addCleanup(shutil.rmtree, self.tmp)
        for rel in identity.RUNTIME_BUNDLE_FILES:
            src = os.path.join(ROOT, *rel.split("/"))
            dst = os.path.join(self.tmp, *rel.split("/"))
            os.makedirs(os.path.dirname(dst), exist_ok=True)
            shutil.copyfile(src, dst)

    def test_frozen_bundle_is_deterministic_and_uses_existing_tree_hash_shape(self):
        first = identity.runtime_bundle_sha256(self.tmp)
        second = identity.runtime_bundle_sha256(self.tmp)
        self.assertEqual(first, second)
        vendor_path = os.path.join(ROOT, "vendoring", "vendor.py")
        spec = importlib.util.spec_from_file_location("vendor_identity_crosscheck", vendor_path)
        vendor = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(vendor)
        tree = {}
        for rel in identity.RUNTIME_BUNDLE_FILES:
            with open(os.path.join(self.tmp, *rel.split("/")), "rb") as fh:
                tree[rel] = fh.read()
        self.assertEqual(first, vendor.tree_sha256(tree))

    def test_one_runtime_byte_changes_the_bundle_root(self):
        before = identity.runtime_bundle_sha256(self.tmp)
        path = os.path.join(self.tmp, "spark_daemon", "runtime.py")
        with open(path, "ab") as fh:
            fh.write(b"\n# mutation\n")
        after = identity.runtime_bundle_sha256(self.tmp)
        self.assertNotEqual(before, after)

    def test_unlisted_package_code_is_refused(self):
        with open(os.path.join(self.tmp, "spark_daemon", "extra.py"), "w") as fh:
            fh.write("x = 1\n")
        with self.assertRaisesRegex(identity.IdentityError, "unexpected"):
            identity.runtime_bundle_sha256(self.tmp)

    def test_bytecode_and_symlinks_are_refused(self):
        cache = os.path.join(self.tmp, "spark_daemon", "__pycache__")
        os.makedirs(cache)
        with open(os.path.join(cache, "x.pyc"), "wb") as fh:
            fh.write(b"x")
        with self.assertRaisesRegex(identity.IdentityError, "__pycache__"):
            identity.runtime_bundle_sha256(self.tmp)
        shutil.rmtree(cache)
        target = os.path.join(self.tmp, "spark_daemon", "canonical.py")
        os.remove(target)
        os.symlink("identity.py", target)
        with self.assertRaisesRegex(identity.IdentityError, "symlink"):
            identity.runtime_bundle_sha256(self.tmp)


if __name__ == "__main__":
    unittest.main()
