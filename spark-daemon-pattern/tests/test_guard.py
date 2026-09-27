"""Guard: output-directory checks, inventory, path policy, and the audit hook in a child process."""

import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

from helpers import Sandbox
from spark_daemon import guard


class OutputDirTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp)

    def test_creates_0700_tree(self):
        out = os.path.join(self.tmp, "a", "out")
        old = os.umask(0o077)
        try:
            guard.prepare_output_dir(out)
        finally:
            os.umask(old)
        for d in (out, os.path.join(out, "quarantine"), os.path.join(out, "tmp")):
            self.assertEqual(os.stat(d).st_mode & 0o777, 0o700)

    def test_refuses_loose_permissions(self):
        out = os.path.join(self.tmp, "out")
        os.mkdir(out, 0o755)
        os.chmod(out, 0o755)
        with self.assertRaises(guard.GuardError):
            guard.prepare_output_dir(out)

    def test_refuses_symlink(self):
        real = os.path.join(self.tmp, "real")
        os.mkdir(real, 0o700)
        link = os.path.join(self.tmp, "link")
        os.symlink(real, link)
        with self.assertRaises(guard.GuardError):
            guard.prepare_output_dir(link)

    def test_stray_temp_files_removed(self):
        out = os.path.join(self.tmp, "out")
        guard.prepare_output_dir(out)
        stray = os.path.join(out, "tmp", "index-copy-1")
        open(stray, "w").close()
        guard.prepare_output_dir(out)
        self.assertFalse(os.path.exists(stray))

    def test_inventory_reports_foreign_files(self):
        out = os.path.join(self.tmp, "out")
        guard.prepare_output_dir(out)
        open(os.path.join(out, "ledger.jsonl"), "w").close()
        open(os.path.join(out, "latest_digest.md"), "w").close()
        open(os.path.join(out, ".tmp-digest.md-1-2"), "w").close()
        self.assertEqual(guard.inventory(out), ["latest_digest.md"])


class AuditHookTests(unittest.TestCase):
    """The hook is process-wide and cannot be removed, so it is exercised in child processes."""

    def test_enforce_mode_blocks_every_forbidden_operation(self):
        box = Sandbox("counter")
        try:
            p = box.cli("probe-policy", "--manifest", box.manifest, "--canary",
                        os.path.join(box.home, "spark-core", "data", "canary.txt"))
            result = json.loads(p.stdout.strip().splitlines()[-1])
            self.assertTrue(result["ok"], result)
            self.assertEqual(result["violations_counted"], 10)
        finally:
            box.cleanup()

    def test_record_mode_reports_without_blocking(self):
        box = Sandbox("counter")
        try:
            code = (
                "import sys, os; sys.path.insert(0, %r)\n"
                "from spark_daemon import guard, manifest\n"
                "m = manifest.load(%r); pol = guard.Policy(m); guard.prepare_output_dir(pol.output_dir)\n"
                "guard.install_audit_hook(pol, 'record')\n"
                "open(os.path.join(%r, 'outside.txt'), 'w').write('x')\n"
                "print('count', guard.VIOLATIONS['count'])\n"
            ) % (os.path.dirname(os.path.dirname(os.path.abspath(guard.__file__))), box.manifest, box.home)
            p = subprocess.run([sys.executable, "-c", code], env=box.env(), capture_output=True, text=True)
            self.assertIn("count 1", p.stdout)
            self.assertIn("write-outside-output-dir", p.stderr)
            self.assertTrue(os.path.exists(os.path.join(box.home, "outside.txt")))
        finally:
            box.cleanup()

    def test_policy_readable(self):
        box = Sandbox("counter")
        try:
            os.environ["SPARK_DAEMON_HOME"] = box.home
            from spark_daemon import manifest
            pol = guard.Policy(manifest.load(box.manifest))
            before = guard.VIOLATIONS["count"]
            self.assertTrue(pol.readable("~/data/value.txt").endswith("/data/value.txt"))
            with self.assertRaises(guard.GuardError):
                pol.readable("~/spark-core/data/canary.txt")
            with self.assertRaises(guard.GuardError):
                pol.readable("/etc/passwd")
            self.assertEqual(guard.VIOLATIONS["count"], before + 2)
        finally:
            os.environ.pop("SPARK_DAEMON_HOME", None)
            box.cleanup()


if __name__ == "__main__":
    unittest.main()
