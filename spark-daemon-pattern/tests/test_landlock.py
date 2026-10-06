"""Landlock (AP-01): ABI detection, the pure helpers, and real enforcement in forked
children (never in the test runner's own process - a Landlock domain is irreversible for
the rest of the process's life, so every enforcing test isolates itself with os.fork(),
the same discipline evidence/ap/landlock_probe.py uses)."""

import errno
import json
import os
import shutil
import tempfile
import unittest
from collections import namedtuple

from helpers import ROOT  # noqa: F401  (puts the package on sys.path)
from spark_daemon import landlock, proc

FakePolicy = namedtuple("FakePolicy", ["output_dir", "reads", "deny", "commands"], defaults=({},))


def _run_in_child(fn):
    """Runs fn() (no arguments) in a forked child and returns its JSON-safe return value.
    An uncaught exception in the child is reported as {"error": repr(e)}."""
    r, w = os.pipe()
    pid = os.fork()
    if pid == 0:
        os.close(r)
        try:
            result = fn()
        except Exception as e:  # noqa: BLE001
            result = {"error": repr(e)}
        try:
            os.write(w, json.dumps(result).encode())
        finally:
            os._exit(0)
    os.close(w)
    chunks = []
    while True:
        chunk = os.read(r, 65536)
        if not chunk:
            break
        chunks.append(chunk)
    os.close(r)
    os.waitpid(pid, 0)
    return json.loads(b"".join(chunks))


def _attempt(label, fn):
    try:
        fn()
        return {"attempt": label, "result": "allowed"}
    except OSError as e:
        return {"attempt": label, "result": "blocked", "errno": errno.errorcode.get(e.errno, e.errno)}


def _exec(path, *args):
    """Fork and exec path; the exit code, or 100 + errno if exec itself was refused."""
    pid = os.fork()
    if pid == 0:
        devnull = os.open(os.devnull, os.O_WRONLY)
        os.dup2(devnull, 1)
        os.dup2(devnull, 2)
        try:
            os.execv(path, [path, *args])
        except OSError as e:
            os._exit(100 + (e.errno or 0) % 100)
    return os.waitpid(pid, 0)[1] >> 8


def _read(path):
    with open(path, "rb") as fh:
        fh.read()


def _write(path, mode="wb"):
    with open(path, mode) as fh:
        fh.write(b"x")


class AbiTests(unittest.TestCase):
    def test_abi_version_is_an_int(self):
        self.assertIsInstance(landlock.abi_version(), int)

    def test_this_workspace_kernel_supports_landlock(self):
        # Documents the environment this suite runs in; the DGX (aarch64, a different
        # kernel) must be checked separately per ADJUDICATION-AP.md.
        self.assertGreaterEqual(landlock.abi_version(), 1)


class PureHelperTests(unittest.TestCase):
    def test_fs_access_mask_grows_with_abi(self):
        m1 = landlock._fs_access_mask(1)
        m2 = landlock._fs_access_mask(2)
        m3 = landlock._fs_access_mask(3)
        m5 = landlock._fs_access_mask(5)
        self.assertEqual(m1 & landlock._ACCESS_FS_REFER, 0)
        self.assertEqual(m2 & landlock._ACCESS_FS_REFER, landlock._ACCESS_FS_REFER)
        self.assertEqual(m2 & landlock._ACCESS_FS_TRUNCATE, 0)
        self.assertEqual(m3 & landlock._ACCESS_FS_TRUNCATE, landlock._ACCESS_FS_TRUNCATE)
        self.assertEqual(m5 & landlock._ACCESS_FS_IOCTL_DEV, landlock._ACCESS_FS_IOCTL_DEV)
        self.assertTrue(m1 < m2 < m3 < m5)

    def test_fs_access_for_file_drops_directory_only_rights(self):
        with tempfile.NamedTemporaryFile() as f:
            granted = landlock._fs_access_for(f.name, landlock.ALL_FS_ACCESS)
        self.assertEqual(granted & landlock.ACCESS_FS_READ_DIR, 0)
        self.assertNotEqual(granted & landlock.ACCESS_FS_READ_FILE, 0)

    def test_fs_access_for_directory_keeps_every_requested_right(self):
        with tempfile.TemporaryDirectory() as d:
            granted = landlock._fs_access_for(d, landlock.ALL_FS_ACCESS)
        self.assertEqual(granted, landlock.ALL_FS_ACCESS)

    def test_gaps_finds_a_denied_path_inside_a_granted_read(self):
        policy = FakePolicy(output_dir="/tmp/out", reads=("/home/x/spark-core",),
                            deny=("/home/x/spark-core/data", "/home/x/.ssh"))
        self.assertEqual(landlock.gaps(policy), ["/home/x/spark-core/data"])

    def test_gaps_empty_when_reads_and_denies_do_not_overlap(self):
        policy = FakePolicy(output_dir="/tmp/out", reads=("/home/x/project",),
                            deny=("/home/x/.ssh",))
        self.assertEqual(landlock.gaps(policy), [])


class SupervisorDomainTestModeTests(unittest.TestCase):
    """apply_supervisor_domain's PD-15 branch (unavailable/too-old ABI) needs no privilege
    change at all, so these run directly, not forked."""

    def test_unavailable_landlock_is_non_fatal_in_test_mode(self):
        original = landlock.abi_version
        landlock.abi_version = lambda: -1
        try:
            policy = FakePolicy(output_dir="/tmp", reads=(), deny=())
            result = landlock.apply_supervisor_domain(policy, test_mode=True)
        finally:
            landlock.abi_version = original
        self.assertEqual(result, {"abi": 0, "status": "unavailable", "gaps": [], "execute": []})

    def test_unavailable_landlock_refuses_outside_test_mode(self):
        original = landlock.abi_version
        landlock.abi_version = lambda: -1
        try:
            policy = FakePolicy(output_dir="/tmp", reads=(), deny=())
            with self.assertRaises(landlock.LandlockError):
                landlock.apply_supervisor_domain(policy, test_mode=False)
        finally:
            landlock.abi_version = original


class EnforcementTests(unittest.TestCase):
    """Real Landlock enforcement, each in its own forked child so the test runner's process
    is never restricted. Skipped below MIN_USABLE_ABI, since restrict_self() would refuse."""

    def setUp(self):
        if landlock.abi_version() < landlock.MIN_USABLE_ABI:
            self.skipTest(f"Landlock ABI < {landlock.MIN_USABLE_ABI} on this kernel")
        self.root = tempfile.mkdtemp(prefix="ll-test-")
        home = os.path.join(self.root, "home")
        for d in (".ssh", "spark-core/data", "reads", "out"):
            os.makedirs(os.path.join(home, d))
        for f, body in ((".ssh/id_test", "SECRET"), ("spark-core/data/canary.txt", "CANARY"),
                        ("reads/value.txt", "v"), ("out/ledger.jsonl", "")):
            with open(os.path.join(home, f), "w") as fh:
                fh.write(body)
        self.home = home

    def tearDown(self):
        shutil.rmtree(self.root, ignore_errors=True)

    def test_supervisor_domain_blocks_denied_reads_and_outside_writes(self):
        home, root = self.home, self.root

        def child():
            policy = FakePolicy(
                output_dir=os.path.join(home, "out"),
                reads=(os.path.join(home, "reads"),),
                deny=(os.path.join(home, ".ssh"), os.path.join(home, "spark-core", "data")))
            landlock.apply_supervisor_domain(
                policy, extra_read_paths=landlock.system_read_paths(), test_mode=False)
            out = []
            out.append(_attempt("read a declared read", lambda: _read(os.path.join(home, "reads/value.txt"))))
            out.append(_attempt("read a denied path", lambda: _read(os.path.join(home, ".ssh/id_test"))))
            out.append(_attempt("write inside output_dir", lambda: _write(os.path.join(home, "out/new.txt"))))
            out.append(_attempt("write outside output_dir", lambda: _write(os.path.join(root, "escape.txt"))))
            return out

        results = {a["attempt"]: a for a in _run_in_child(child)}
        self.assertEqual(results["read a declared read"]["result"], "allowed")
        self.assertEqual(results["read a denied path"]["result"], "blocked")
        self.assertEqual(results["write inside output_dir"]["result"], "allowed")
        self.assertEqual(results["write outside output_dir"]["result"], "blocked")

    def test_nested_child_domain_can_only_narrow_never_widen(self):
        home = self.home

        def child():
            policy = FakePolicy(output_dir=os.path.join(home, "out"),
                                reads=(os.path.join(home, "reads"),), deny=())
            landlock.apply_supervisor_domain(
                policy, extra_read_paths=landlock.system_read_paths(), test_mode=False)
            # A grandchild applies a strictly narrower domain: read-only, nothing granted
            # on the output directory at all (the AP-04 worker shape).
            def grandchild():
                landlock.restrict_self([(os.path.join(home, "reads"), landlock.READ)])
                return _attempt("grandchild write to output", lambda: _write(os.path.join(home, "out/x.txt")))
            return _run_in_child(grandchild)

        result = _run_in_child(child)
        self.assertEqual(result["result"], "blocked")

    def test_nothing_the_daemon_can_write_or_merely_reads_can_be_executed(self):
        """r4.6 (HF-36): the domain granted EXECUTE on the output directory and on every declared
        read path, so if the in-process layers were bypassed, a binary written into the output
        directory, or dropped by someone else into a watched folder, could be run. r4.11 (R-2b):
        a daemon that declares no command may execute nothing at all, system tools included."""
        home = self.home
        true_bin = shutil.which("true", path=proc.SYSTEM_PATH)
        dropped = os.path.join(home, "reads", "dropped")
        shutil.copyfile(true_bin, dropped)
        os.chmod(dropped, 0o755)
        # Control: the copy runs outside the domain, so a block below is Landlock, not noexec.
        self.assertEqual(os.spawnv(os.P_WAIT, dropped, [dropped]), 0)

        def child():
            policy = FakePolicy(output_dir=os.path.join(home, "out"),
                                reads=(os.path.join(home, "reads"),), deny=())
            info = landlock.apply_supervisor_domain(
                policy, extra_read_paths=landlock.system_read_paths(), test_mode=False)
            written = os.path.join(home, "out", "payload")
            shutil.copyfile(true_bin, written)          # writing into the output dir is allowed
            os.chmod(written, 0o755)
            return {"system": _exec(true_bin), "output_dir": _exec(written), "declared_read": _exec(dropped),
                    "granted": info["execute"]}

        result = _run_in_child(child)
        self.assertEqual(result["granted"], [])
        for where in ("system", "output_dir", "declared_read"):
            self.assertEqual(result[where], 100 + errno.EACCES, (where, result))

    def test_a_declared_command_runs_and_nothing_else_does(self):
        """r4.11 (R-2b): execute only on the declared executable and its ELF loader. A declared
        command is an allowlist, not a switch that lets the daemon run any program."""
        git = shutil.which("git", path=proc.SYSTEM_PATH)
        if not git:
            self.skipTest("git not installed")
        git = os.path.realpath(git)
        true_bin = shutil.which("true", path=proc.SYSTEM_PATH)
        home = self.home

        def child():
            policy = FakePolicy(output_dir=os.path.join(home, "out"), reads=(os.path.join(home, "reads"),),
                                deny=(), commands={"git": git})
            info = landlock.apply_supervisor_domain(
                policy, extra_read_paths=landlock.system_read_paths(), test_mode=False)
            return {"git": _exec(git, "--version"), "true": _exec(true_bin), "granted": info["execute"]}

        result = _run_in_child(child)
        self.assertEqual(result["git"], 0, result)
        self.assertEqual(result["true"], 100 + errno.EACCES, result)
        self.assertEqual(result["granted"], sorted({git, landlock.elf_interpreter(git)}))

    def test_native_modules_still_import_without_any_execute_right(self):
        home = self.home

        def child():
            policy = FakePolicy(output_dir=os.path.join(home, "out"), reads=(), deny=())
            landlock.apply_supervisor_domain(policy, extra_read_paths=landlock.system_read_paths(),
                                             test_mode=False)
            import _bz2  # noqa: F401  native modules this process has not loaded yet
            import _decimal  # noqa: F401
            import _lzma  # noqa: F401
            return "ok"

        self.assertEqual(_run_in_child(child), "ok")

    def test_the_loader_residual_is_recorded_not_hidden(self):
        """The known residual of R-2b (evidence/r4.11/exec_grant_probe.txt): with execute on a
        declared command and its loader, the loader itself can be run as a program on another
        readable binary. Python-level code cannot reach it (proc and the audit hook allow only the
        declared executable); closing it at the kernel needs PD-72 or R-2. If this test starts
        failing, the residual is gone: update ADJUDICATION-R4.9-OWNER-IMPLEMENTATION.md."""
        git = shutil.which("git", path=proc.SYSTEM_PATH)
        if not git:
            self.skipTest("git not installed")
        git = os.path.realpath(git)
        loader = landlock.elf_interpreter(git)
        true_bin = shutil.which("true", path=proc.SYSTEM_PATH)
        home = self.home

        def child():
            policy = FakePolicy(output_dir=os.path.join(home, "out"), reads=(), deny=(), commands={"git": git})
            landlock.apply_supervisor_domain(policy, extra_read_paths=landlock.system_read_paths(),
                                             test_mode=False)
            return {"direct": _exec(true_bin), "via_loader": _exec(loader, true_bin)}

        result = _run_in_child(child)
        self.assertEqual(result["direct"], 100 + errno.EACCES, result)
        self.assertEqual(result["via_loader"], 0, result)

    def test_output_directory_gets_every_right_the_abi_handles(self):
        home = self.home

        def child():
            policy = FakePolicy(output_dir=os.path.join(home, "out"), reads=(), deny=())
            landlock.apply_supervisor_domain(
                policy, extra_read_paths=landlock.system_read_paths(), test_mode=False)
            path = os.path.join(home, "out", "ledger.jsonl")
            results = []
            results.append(_attempt("append", lambda: _write(path, "ab")))
            fd = os.open(path, os.O_WRONLY)
            try:
                results.append(_attempt("ftruncate", lambda: os.ftruncate(fd, 0)))
            finally:
                os.close(fd)
            return results

        for r in _run_in_child(child):
            self.assertEqual(r["result"], "allowed", r)


if __name__ == "__main__":
    unittest.main()
