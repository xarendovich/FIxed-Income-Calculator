"""Hardened subprocess: allowlist, scrubbed environment, bounds, timeout, and the F4 index-copy fix."""

import os
import shutil
import subprocess
import tempfile
import time
import unittest

from helpers import ROOT  # noqa: F401
from spark_daemon import proc

EXE = {name: shutil.which(name, path=proc.SYSTEM_PATH) for name in ("git", "seq", "sleep", "cat")}


def sh_git(repo, *args, env=None):
    return subprocess.run(["git", "-C", repo, *args], check=True, capture_output=True, text=True, env=env)


class ProcTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp)

    def test_only_allowlisted_commands_run(self):
        with self.assertRaises(proc.CommandNotAllowed):
            proc.run(["sh", "-c", "true"], executables=EXE, timeout=5, max_bytes=100)

    def test_output_is_bounded(self):
        r = proc.run(["seq", "1", "1000000"], executables=EXE, timeout=10, max_bytes=1000)
        self.assertTrue(r.truncated)
        self.assertLessEqual(len(r.stdout), 1000)
        self.assertIsNone(r.returncode)

    def test_timeout_kills_the_group(self):
        start = time.monotonic()
        r = proc.run(["sleep", "5"], executables=EXE, timeout=0.5, max_bytes=100)
        self.assertTrue(r.timed_out)
        self.assertLess(time.monotonic() - start, 3)

    def test_an_exception_mid_wait_still_kills_the_group(self):
        # r4.11 (HF-40): the cycle's alarm (R-6) can raise inside proc.run's cleanup wait. A
        # command that closed its stdout and kept running then outlived the cycle, one per cycle.
        import signal

        class Alarm(BaseException):
            pass

        def raise_alarm(_signum, _frame):
            raise Alarm()
        marker = f"sleep {40 + os.getpid() % 9}"
        old = signal.signal(signal.SIGALRM, raise_alarm)
        try:
            signal.setitimer(signal.ITIMER_REAL, 0.3)
            with self.assertRaises(Alarm):
                proc.run(["sh", "-c", f"exec 1>&-; {marker}"], executables={"sh": "/bin/sh"}, timeout=0.3,
                         max_bytes=100)
        finally:
            signal.setitimer(signal.ITIMER_REAL, 0)
            signal.signal(signal.SIGALRM, old)
        time.sleep(0.3)
        alive = subprocess.run(["pgrep", "-f", marker], capture_output=True, text=True).stdout.split()
        for pid in alive:
            os.kill(int(pid), signal.SIGKILL)
        self.assertEqual(alive, [])

    def test_git_ignores_user_configuration(self):
        """Script-board F5: color.ui=always and diff.external in the user's config must not leak."""
        home = os.path.join(self.tmp, "home")
        os.makedirs(home)
        with open(os.path.join(home, ".gitconfig"), "w") as fh:
            fh.write("[color]\n\tui = always\n[diff]\n\texternal = /bin/false\n")
        repo = os.path.join(self.tmp, "repo")
        env = dict(os.environ, HOME=home)
        sh_git(self.tmp, "init", "-q", "repo", env=env)
        with open(os.path.join(repo, "a.txt"), "w") as fh:
            fh.write("one\n")
        sh_git(repo, "add", ".", env=env)
        sh_git(repo, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "c1", env=env)
        with open(os.path.join(repo, "a.txt"), "a") as fh:
            fh.write("two\n")
        leaked = subprocess.run(["git", "-C", repo, "diff"], capture_output=True, text=True, env=env)
        self.assertNotIn("+two", leaked.stdout)          # the user's config really is hostile
        r = proc.git(repo, ["diff", *proc.GIT_DIFF_FLAGS], executables=EXE, timeout=10, max_bytes=10000)
        self.assertIn("+two", r.stdout)
        self.assertNotIn("\x1b[", r.stdout)

    def test_inherited_git_selection_variables_cannot_redirect_git(self):
        """r4.0 (hardening review, item 2): GIT_DIR, GIT_WORK_TREE and GIT_OBJECT_DIRECTORY in the
        daemon's own environment must not redirect a call to a decoy repository. Every child starts
        from SAFE_ENV (an allowlist), so nothing inherited reaches Git."""
        env = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        for name in ("real", "decoy"):
            sh_git(self.tmp, "init", "-q", "-b", "main", name, env=env)
            sh_git(os.path.join(self.tmp, name), "-c", "user.name=t", "-c", "user.email=t@t",
                   "commit", "-q", "--allow-empty", "-m", f"{name} commit", env=env)
        decoy = os.path.join(self.tmp, "decoy")
        hostile = {"GIT_DIR": os.path.join(decoy, ".git"), "GIT_WORK_TREE": decoy,
                   "GIT_OBJECT_DIRECTORY": os.path.join(decoy, ".git", "objects")}
        # The variables really do redirect plain Git, so the test cannot pass vacuously.
        plain = subprocess.run(["git", "-C", os.path.join(self.tmp, "real"), "log", "-1", "--format=%s"],
                               capture_output=True, text=True, env=dict(env, **hostile))
        self.assertEqual(plain.stdout.strip(), "decoy commit")
        saved = {k: os.environ.get(k) for k in hostile}
        os.environ.update(hostile)
        try:
            r = proc.git(os.path.join(self.tmp, "real"), ["log", "-1", "--format=%s"],
                         executables=EXE, timeout=10, max_bytes=1000)
        finally:
            for k, v in saved.items():
                if v is None:
                    os.environ.pop(k, None)
                else:
                    os.environ[k] = v
        self.assertEqual(r.stdout.strip(), "real commit")

    def test_index_copy_leaves_the_real_index_untouched(self):
        """Script-board F4: porcelain `git diff` rewrites .git/index; the index copy does not."""
        repo = os.path.join(self.tmp, "repo")
        sh_git(self.tmp, "init", "-q", "repo")
        for name in ("a.txt", "b.txt"):
            with open(os.path.join(repo, name), "w") as fh:
                fh.write(name)
        sh_git(repo, "add", ".")
        sh_git(repo, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "c1")
        tmp_dir = os.path.join(self.tmp, "scratch")
        os.mkdir(tmp_dir)

        def stat_dirty_index():
            time.sleep(1.1)
            os.utime(os.path.join(repo, "b.txt"))        # stat changes, content does not
            with open(os.path.join(repo, ".git", "index"), "rb") as fh:
                return fh.read()

        before = stat_dirty_index()
        r = proc.git(repo, ["diff", *proc.GIT_DIFF_FLAGS, "--name-only"], executables=EXE, timeout=10,
                     max_bytes=10000, tmp_dir=tmp_dir, index_copy=True)
        with open(os.path.join(repo, ".git", "index"), "rb") as fh:
            self.assertEqual(fh.read(), before)
        self.assertEqual(r.stdout.strip(), "")
        self.assertEqual(os.listdir(tmp_dir), [])        # the copy is removed

        before = stat_dirty_index()
        proc.git(repo, ["diff", *proc.GIT_DIFF_FLAGS, "--name-only"], executables=EXE, timeout=10,
                 max_bytes=10000)
        with open(os.path.join(repo, ".git", "index"), "rb") as fh:
            self.assertNotEqual(fh.read(), before, "control: plain porcelain diff should rewrite the index")


if __name__ == "__main__":
    unittest.main()
