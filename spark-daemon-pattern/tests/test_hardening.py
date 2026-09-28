"""Regression tests for the r3 hardening: every escape or crash found in the r2 review has a
test here that failed on r2 and passes now. See HARDENING.md for the findings."""

import json
import os
import random
import shutil
import subprocess
import sys
import tempfile
import unittest

from helpers import ENTRY, EXAMPLE, FIXTURES, ROOT, Sandbox
from spark_daemon import guard, manifest, proc, purity, render, unitgen

SENSE_DECIDE = '''
def decide(prev, snapshot):
    return [("VALUE_OBSERVED", {"value": str(snapshot["value"])[:200]})]
'''


def fixture_with(source: str, commands=()):
    """A throwaway fixture directory: the counter manifest with a new daemon.py."""
    tmp = tempfile.mkdtemp(prefix="spark-fixture-")
    with open(os.path.join(FIXTURES, "counter", "manifest.json")) as fh:
        data = json.load(fh)
    data["commands"] = list(commands)
    with open(os.path.join(tmp, "manifest.json"), "w") as fh:
        json.dump(data, fh)
    with open(os.path.join(tmp, "daemon.py"), "w") as fh:
        fh.write(source)
    return tmp


def base_manifest(**changes):
    with open(os.path.join(FIXTURES, "counter", "manifest.json")) as fh:
        data = json.load(fh)
    data.update(changes)
    return data


class ManifestHardeningTests(unittest.TestCase):
    def problems(self, **changes):
        try:
            manifest.parse(base_manifest(**changes))
        except manifest.ManifestError as e:
            return e.problems
        return []

    def test_interpreter_families_and_command_runners_are_refused(self):
        for cmd in ("python3.12", "python3.11", "perl5.38", "node18", "php8", "tar", "less",
                    "timeout", "nice", "gawk", "busybox", "openssl"):
            with self.subTest(cmd=cmd):
                self.assertTrue(any("never allowed" in p for p in self.problems(commands=[cmd])))

    def test_harmless_commands_still_allowed(self):
        for cmd in ("git", "df", "uptime", "nvidia-smi", "sleep"):
            with self.subTest(cmd=cmd):
                self.assertEqual(self.problems(commands=[cmd]), [])

    def test_dot_segments_and_trailing_slashes_refused(self):
        self.assertTrue(self.problems(reads=["~/data/../.ssh"]))
        self.assertTrue(self.problems(reads=["/proc/./meminfo"]))
        self.assertTrue(self.problems(reads=["~/data/"]))
        self.assertTrue(self.problems(output_dir="~/out//x"))

    def test_output_dir_must_not_contain_a_protected_tree(self):
        # r2 accepted "~": Landlock and ReadWritePaths= would then grant write access to the
        # whole home directory, ~/.ssh and ~/spark-core included.
        problems = self.problems(output_dir="~", reads=["/proc/meminfo"])
        self.assertTrue(any("must not contain the protected path" in p for p in problems), problems)
        self.assertTrue(self.problems(output_dir="/", reads=["/proc/meminfo"]))

    def test_output_dir_inside_the_manifests_own_deny_is_refused(self):
        problems = self.problems(output_dir="~/out/counter", deny=["~/out"])
        self.assertTrue(any("must not be inside ~/out" in p for p in problems), problems)

    def test_trailing_newlines_are_refused(self):
        # r2 used re.match with "$", which also matches before a final newline: a name of
        # "x\n" split the generated unit's Description= line in two.
        self.assertTrue(self.problems(name="fixture-counter\n"))
        self.assertTrue(self.problems(purpose="Test fixture daemon for the skeleton self-tests.\n"))
        self.assertTrue(self.problems(reads=["~/data\n"]))
        self.assertTrue(self.problems(run_as={"unit": "system", "user": "spark\n"}))

    def test_to_dict_is_bounded_and_strict(self):
        tmp = tempfile.mkdtemp()
        try:
            path = os.path.join(tmp, "manifest.json")
            with open(path, "w") as fh:
                fh.write("{not json")
            with self.assertRaises(manifest.ManifestError):
                manifest.to_dict(path)
            with open(path, "w") as fh:
                fh.write(" " * 70000 + "{}")
            with self.assertRaises(manifest.ManifestError):
                manifest.to_dict(path)
        finally:
            shutil.rmtree(tmp)


class GitHardeningTests(unittest.TestCase):
    def test_refusals(self):
        refused = [
            [], ["-c", "alias.x=!id", "x"], ["config", "--get", "x"], ["symbolic-ref", "HEAD", "x"],
            ["log", "--output=/tmp/x"], ["log", "--outp=/tmp/x"], ["diff", "--no-index", "a", "b"],
            ["diff", "/etc/passwd", "/etc/hosts"], ["log", "--", "../outside"],
            ["log", "--format=%G?"], ["log", "--show-signature"], ["log", "--ext-diff"],
            ["blame", "--contents", "x", "f"], ["blame", "-Sfile", "f"], ["ls-files", "-X", "f"],
            ["diff", "-Oorder"], ["cat-file", "--batch"], ["log", "--st"],
        ]
        for args in refused:
            with self.subTest(args=args):
                self.assertIsNotNone(proc.git_refusal(args))

    def test_allowed(self):
        allowed = [
            ["rev-parse", "HEAD"], ["rev-parse", "--abbrev-ref", "HEAD"], ["rev-parse", "--git-dir"],
            ["status", "--porcelain=v1", "-z"], ["log", "-n", "5", "--pretty=format:%H %s"],
            ["log", "HEAD~3..HEAD", "--stat"], ["log", "-S", "needle"], ["diff", "--", "src/x.py"],
        ]
        for args in allowed:
            with self.subTest(args=args):
                self.assertIsNone(proc.git_refusal(args))

    def test_diff_subcommands_get_no_ext_diff_and_no_textconv(self):
        seen = {}

        def fake_run(argv, **kwargs):
            seen["argv"] = argv
            return proc.RunResult(0, "", False, False)

        original = proc.run
        proc.run = fake_run
        try:
            proc.git("/repo", ["log", "-p"], executables={"git": "/usr/bin/git"}, timeout=5, max_bytes=100)
        finally:
            proc.run = original
        argv = seen["argv"]
        i = argv.index("log")
        self.assertEqual(argv[i:i + 4], ["log", "--no-ext-diff", "--no-textconv", "-p"])
        # Only the declared repository is trusted for Git's ownership check (PD-25).
        self.assertIn("safe.directory=/repo", argv)
        self.assertNotIn("safe.directory=*", argv)


@unittest.skipUnless(shutil.which("git", path="/usr/bin:/bin"), "git not installed")
class GitHistoryIntegrityTests(unittest.TestCase):
    """The Observer's WBS 2.5 hardened Git profile (A1, evidence E1-E3), adopted for ctx.git
    after a cross-check against the Spark handoffs: each case was reproduced against r3."""

    GIT = {"git": "/usr/bin/git"}

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.repo = os.path.join(self.tmp, "r")
        self.env = {"PATH": "/usr/bin:/bin", "HOME": self.tmp, "GIT_CONFIG_GLOBAL": "/dev/null",
                    "GIT_CONFIG_NOSYSTEM": "1", "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t",
                    "GIT_COMMITTER_NAME": "t", "GIT_COMMITTER_EMAIL": "t@t"}
        self.g("init", "-q", "-b", "main", self.repo, cwd=self.tmp)
        for i in range(3):
            self.g("commit", "-q", "--allow-empty", "-m", f"c{i}")

    def tearDown(self):
        shutil.rmtree(self.tmp)

    def g(self, *args, cwd=None, data=None):
        return subprocess.run(["git", *args], cwd=cwd or self.repo, env=self.env, input=data,
                              capture_output=True, check=True).stdout.decode().strip()

    def ctx_git(self, *args):
        return proc.git(self.repo, list(args), executables=self.GIT, timeout=10, max_bytes=65536)

    def test_repository_gpg_program_never_runs(self):
        raw = self.g("cat-file", "commit", "HEAD")
        head, _, msg = raw.partition("\n\n")
        signed = (head + "\ngpgsig -----BEGIN PGP SIGNATURE-----\n \n iQEzBAABCAAd\n =AAAA\n"
                  " -----END PGP SIGNATURE-----\n\n" + msg + "\n")
        oid = self.g("hash-object", "-t", "commit", "-w", "--stdin", data=signed.encode())
        self.g("update-ref", "HEAD", oid)
        sentinel = os.path.join(self.tmp, "SENTINEL")
        script = os.path.join(self.tmp, "gpg.sh")
        with open(script, "w") as fh:
            fh.write(f"#!/bin/sh\ntouch {sentinel}\nexit 1\n")
        os.chmod(script, 0o755)
        self.g("config", "log.showSignature", "true")
        self.g("config", "gpg.program", script)
        subprocess.run(["git", "log", "-1"], cwd=self.repo, env=self.env, capture_output=True)
        self.assertTrue(os.path.exists(sentinel), "fixture must prove plain git runs gpg.program")
        os.remove(sentinel)
        for args in (["log", "-1", "--pretty=format:%s"], ["show", "-s", "HEAD"]):
            with self.subTest(args=args):
                self.assertEqual(self.ctx_git(*args).returncode, 0)
                self.assertFalse(os.path.exists(sentinel))

    def test_replace_refs_do_not_forge_history(self):
        self.g("commit", "-q", "--allow-empty", "-m", "forged")
        forged = self.g("rev-parse", "HEAD")
        self.g("reset", "-q", "--hard", "HEAD~1")
        self.g("replace", self.g("rev-parse", "HEAD"), forged)
        self.assertEqual(self.g("log", "-1", "--pretty=format:%s"), "forged")    # the fixture is real
        self.assertEqual(self.ctx_git("log", "-1", "--pretty=format:%s").stdout, "c2")
        self.assertEqual(self.ctx_git("rev-list", "--count", "HEAD").stdout.strip(), "3")

    def test_grafts_do_not_rewrite_ancestry(self):
        with open(os.path.join(self.repo, ".git", "info", "grafts"), "w") as fh:
            fh.write(self.g("rev-parse", "HEAD") + "\n")
        self.assertEqual(self.g("rev-list", "--count", "HEAD"), "1")            # the fixture is real
        self.assertEqual(self.ctx_git("rev-list", "--count", "HEAD").stdout.strip(), "3")


@unittest.skipUnless(shutil.which("git", path="/usr/bin:/bin"), "git not installed")
class GitInjectionRuntimeTests(unittest.TestCase):
    """End to end: the r2 injection ran `id` through ctx.git(["-c", "alias.y=!..."]).
    Now the call is refused, counted as a violation, and the daemon fails closed (78)."""

    def run_daemon(self, source):
        fixture = fixture_with(source, commands=["git"])
        sb = Sandbox(fixture)
        try:
            repo = os.path.join(sb.home, "data", "repo")
            subprocess.run(["git", "init", "-q", repo], check=True)
            subprocess.run(["git", "-C", repo, "-c", "user.email=t@t", "-c", "user.name=t", "commit",
                            "-q", "--allow-empty", "-m", "x"], check=True)
            marker = os.path.join(sb.output, "pwned.txt")
            p = sb.run(cycles=1)
            return p, os.path.exists(marker), sb.records()
        finally:
            sb.cleanup()
            shutil.rmtree(fixture)

    def test_alias_injection_through_ctx_git_is_refused(self):
        source = '''
def sense(ctx):
    try:
        ctx.git("~/data/repo", ["-c", "alias.y=!id > OUT/pwned.txt", "y"])
    except ctx.NotAllowed:
        pass
    return {"value": "done"}
''' + SENSE_DECIDE
        p, marker, records = self.run_daemon(source)
        self.assertEqual(p.returncode, 78, p.stderr)
        self.assertFalse(marker)
        self.assertEqual(records[-1]["payload"]["category"], "POLICY_VIOLATION")

    def test_git_through_ctx_run_is_refused(self):
        source = '''
def sense(ctx):
    try:
        ctx.run(["git", "-C", "/", "-c", "alias.x=!id", "x"])
    except ctx.NotAllowed:
        pass
    return {"value": "done"}
''' + SENSE_DECIDE
        p, _, records = self.run_daemon(source)
        self.assertEqual(p.returncode, 78, p.stderr)

    def test_read_only_git_still_works(self):
        source = '''
def sense(ctx):
    r = ctx.git("~/data/repo", ["rev-parse", "HEAD"])
    return {"value": r.stdout.strip()}
''' + SENSE_DECIDE
        p, _, records = self.run_daemon(source)
        self.assertEqual(p.returncode, 0, p.stderr)
        observed = [r for r in records if r["event_type"] == "VALUE_OBSERVED"]
        self.assertRegex(observed[0]["payload"]["value"], r"^[0-9a-f]{40}$")


class PurityHardeningTests(unittest.TestCase):
    HEAD = '"""Doc."""\nimport operator\nimport string\nimport typing\n\n'
    TAIL = "\n\ndef decide(prev, snapshot):\n    return []\n"

    def check(self, body):
        return purity.check_source(self.HEAD + body + self.TAIL)

    def assert_refused(self, body, fragment):
        problems = self.check(body)
        self.assertTrue(any(fragment in p for p in problems), problems)

    def test_frame_escape_to_real_builtins(self):
        self.assert_refused("def _g():\n    yield 1\n\ndef sense(ctx):\n"
                            "    return {'v': _g().gi_frame.f_builtins['open']}", "introspection attribute")

    def test_traceback_frame_escape(self):
        self.assert_refused("def sense(ctx):\n    try:\n        1 / 0\n    except Exception as e:\n"
                            "        return {'v': e.with_traceback(None)}", "introspection attribute")

    def test_private_attribute(self):
        self.assert_refused("def sense(ctx):\n    ctx._policy.deny = ()\n    return {}", "private attribute")

    def test_aliasing_a_forbidden_builtin(self):
        self.assert_refused("def sense(ctx):\n    o = open\n    return {'v': o('/x')}", "aliasing")
        self.assert_refused("def sense(ctx, f=print):\n    return {}", "aliasing")

    def test_dynamic_attribute_helpers(self):
        self.assert_refused("def sense(ctx):\n    return {'v': operator.attrgetter('x')(ctx)}", "attrgetter")
        self.assert_refused("def sense(ctx):\n    return {'v': string.Formatter()}", "Formatter")
        self.assert_refused("def sense(ctx):\n    return {'v': typing.get_type_hints(ctx)}", "get_type_hints")

    def test_format_string_reaching_a_dunder(self):
        self.assert_refused("def sense(ctx):\n    return {'v': '{0.__class__}'.format(ctx)}", "format fields")

    def test_exit_and_quit(self):
        self.assert_refused("def sense(ctx):\n    exit(0)", "exit")

    def test_import_time_calls(self):
        self.assert_refused("def _boom():\n    return 1\n\nX = _boom()\n\ndef sense(ctx):\n    return {}",
                            "at import time")
        self.assert_refused("def _deco(f):\n    return f\n\n@_deco\ndef _helper():\n    pass\n\n"
                            "def sense(ctx):\n    return {}", "at import time")
        self.assert_refused("class C:\n    x = 1\n    while True:\n        pass\n\n"
                            "def sense(ctx):\n    return {}", "class body")

    def test_entry_points_cannot_be_reassigned_or_redefined(self):
        self.assert_refused("def sense(ctx):\n    return {}\n\nsense = decide", "defined with def")
        self.assert_refused("def sense(ctx):\n    return {}\n\ndef sense(ctx):\n    return {}", "more than once")

    def test_allowed_idioms_still_pass(self):
        body = ("import re\nimport dataclasses\nimport collections\n"
                "PATTERN = re.compile(r'x')\nLIMITS = frozenset({1, 2})\n"
                "Row = collections.namedtuple('Row', 'a b')\n\n"
                "@dataclasses.dataclass(frozen=True)\nclass Point:\n    x: int = 0\n\n"
                "def _helper(text):\n    return sorted(text.splitlines())\n\n"
                "def sense(ctx):\n    try:\n        return {'v': ctx.read_text('~/x')}\n"
                "    except ctx.Missing:\n        return {'v': None}")
        self.assertEqual(self.check(body), [])

    def test_every_shipped_daemon_is_still_pure(self):
        paths = [os.path.join(EXAMPLE, "daemon.py")]
        examples = os.path.join(ROOT, "examples")
        paths += [os.path.join(examples, d, "daemon.py") for d in sorted(os.listdir(examples))]
        for path in paths:
            with self.subTest(path=path):
                self.assertEqual(purity.check_file(path), [])


class RuntimeHardeningTests(unittest.TestCase):
    def test_system_exit_in_sense_is_recorded_not_a_silent_exit(self):
        # r2: `raise SystemExit(0)` ended the process with exit 0 and no DAEMON_STOP, so
        # systemd's Restart=on-failure would never have restarted it.
        source = "def sense(ctx):\n    raise SystemExit(0)\n" + SENSE_DECIDE
        fixture = fixture_with(source)
        sb = Sandbox(fixture)
        try:
            p = sb.run(cycles=3)
            self.assertEqual(p.returncode, 0, p.stderr)
            types = [r["event_type"] for r in sb.records()]
            self.assertEqual(types[-1], "DAEMON_STOP")
            errors = [r["payload"] for r in sb.records() if r["event_type"] == "DAEMON_ERROR"]
            self.assertEqual(errors[0], {"category": "SENSE_FAILED", "exception_type": "SystemExit"})
            cleared = [r["payload"] for r in sb.records() if r["event_type"] == "DAEMON_ERROR_CLEARED"]
            self.assertEqual(cleared[0]["repeats"], 3)
        finally:
            sb.cleanup()
            shutil.rmtree(fixture)

    def test_undeclared_command_request_fails_closed_even_when_swallowed(self):
        source = ("def sense(ctx):\n    try:\n        ctx.run(['uptime'])\n    except Exception:\n"
                  "        pass\n    return {'value': 1}\n" + SENSE_DECIDE)
        fixture = fixture_with(source)
        sb = Sandbox(fixture)
        try:
            p = sb.run(cycles=2)
            self.assertEqual(p.returncode, 78, p.stderr)
        finally:
            sb.cleanup()
            shutil.rmtree(fixture)

    def test_policy_is_immutable(self):
        sb = Sandbox("counter")
        try:
            os.environ["SPARK_DAEMON_HOME"] = sb.home
            m = manifest.load(sb.manifest)
            policy = guard.Policy(m)
            with self.assertRaises(AttributeError):
                policy.deny = ()
            with self.assertRaises(TypeError):
                policy.commands["sh"] = "/bin/sh"
        finally:
            os.environ.pop("SPARK_DAEMON_HOME", None)
            sb.cleanup()


class CpuAccountingTests(unittest.TestCase):
    def test_cpu_includes_commands_the_daemon_ran(self):
        from spark_daemon import runtime
        before = runtime._cpu_us()
        subprocess.run([sys.executable, "-c", "import time\nt=time.process_time()\n"
                        "while time.process_time()-t<0.3: pass"], check=True)
        self.assertGreater(runtime._cpu_us() - before, 200_000)


class UnitHardeningTests(unittest.TestCase):
    def test_unit_allows_the_landlock_syscalls(self):
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        text = unitgen.generate(m, root=ROOT)
        self.assertIn(f"SystemCallFilter={unitgen.LANDLOCK_SYSCALLS}", text)
        self.assertEqual(unitgen.lint(text), [])
        broken = text.replace(f"SystemCallFilter={unitgen.LANDLOCK_SYSCALLS}\n", "")
        self.assertIn(f"SystemCallFilter={unitgen.LANDLOCK_SYSCALLS}", unitgen.lint(broken))

    def test_resolve_syscall_filter(self):
        groups = {"@system-service": {"read", "prctl", "setrlimit"}, "@resources": {"setrlimit"},
                  "@privileged": {"chroot"}}
        text = ("SystemCallFilter=@system-service\nSystemCallFilter=landlock_add_rule\n"
                "SystemCallFilter=~@privileged @resources\n")
        allowed, denied = unitgen.resolve_syscall_filter(text, lambda g: groups[g])
        self.assertIn("landlock_add_rule", allowed)
        self.assertIn("setrlimit", denied)

    def test_unsafe_paths_are_refused(self):
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        with self.assertRaises(unitgen.UnitError):
            unitgen.generate(m, root="/opt/spark tools")
        with self.assertRaises(unitgen.UnitError):
            unitgen.generate(m, root=ROOT, python="/usr/bin/python3 -c evil")


class RenderLabelTests(unittest.TestCase):
    def test_label_with_trailing_newline_is_refused(self):
        with self.assertRaises(render.RenderError):
            render.normalize_sections([("Title\n", [])])
        with self.assertRaises(render.RenderError):
            render.normalize_sections([("Title", [("Label\n", 1)])])


class RenderEfficiencyTests(unittest.TestCase):
    def reference(self, name, stamp, sections, max_bytes):
        """r2's linear scan, kept as the oracle for the binary search."""
        total = sum(len(rows) for _, rows in sections)
        for limit in range(total, -1, -1):
            data = _rebuild(name, stamp, sections, limit)
            if len(data) <= max_bytes:
                return data
        raise render.RenderError("too small")

    def test_binary_search_matches_the_linear_scan(self):
        rng = random.Random(7)
        stamp = {"seq": 3, "head": "a" * 64, "last_event_utc": None}
        for _ in range(60):
            sections = [(f"S{i}", [(f"L{j}", "x" * rng.randint(0, 80)) for j in range(rng.randint(0, 12))])
                        for i in range(rng.randint(1, 4))]
            max_bytes = rng.randint(200, 3000)
            try:
                expected = self.reference("d", stamp, sections, max_bytes)
            except render.RenderError:
                with self.assertRaises(render.RenderError):
                    render.render_digest("d", stamp, sections, max_bytes)
                continue
            self.assertEqual(render.render_digest("d", stamp, sections, max_bytes), expected)


def _rebuild(name, stamp, sections, limit):
    # Mirror render.render_digest's build(limit) exactly (the oracle must not share code
    # paths with the search it checks, but must produce identical bytes).
    header = [f"# {name} digest", "", render.BANNER, "",
              f"Reflects ledger seq {stamp['seq']}, chain head {stamp['head'][:16]}, last event none yet.", ""]
    lines, kept, total = list(header), 0, 0
    for title, rows in sections:
        lines += [f"## {title}", ""]
        for label, value in rows:
            total += 1
            if kept >= limit:
                continue
            kept += 1
            lines += [f"**{label}**", "", *render.render_block(value), ""]
    if kept < total:
        lines += [f"_{total - kept} rows omitted to stay within the digest size limit._", ""]
    return "\n".join(lines).encode("utf-8")


class CliRobustnessTests(unittest.TestCase):
    def cli(self, *args):
        return subprocess.run([sys.executable, "-I", "-B", ENTRY, *args], capture_output=True, text=True,
                              timeout=120, env={"PATH": "/usr/bin:/bin", "HOME": "/nonexistent", "LANG": "C.UTF-8"})

    def test_verify_with_a_bad_manifest_reports_instead_of_crashing(self):
        tmp = tempfile.mkdtemp()
        try:
            path = os.path.join(tmp, "manifest.json")
            with open(path, "w") as fh:
                fh.write('{"name": "x"}')
            p = self.cli("verify", "--manifest", path)
            self.assertEqual(p.returncode, 1)
            self.assertNotIn("Traceback", p.stderr)
            self.assertIn("RESULT: FAIL", p.stdout)
        finally:
            shutil.rmtree(tmp)

    def test_battery_with_invalid_json_reports_instead_of_crashing(self):
        tmp = tempfile.mkdtemp()
        try:
            with open(os.path.join(tmp, "manifest.json"), "w") as fh:
                fh.write("{not json")
            with open(os.path.join(tmp, "daemon.py"), "w") as fh:
                fh.write("def sense(ctx):\n    return {}\n\ndef decide(prev, snapshot):\n    return []\n")
            p = self.cli("battery", "--manifest", os.path.join(tmp, "manifest.json"))
            self.assertEqual(p.returncode, 1)
            self.assertNotIn("Traceback", p.stderr)
            self.assertIn("RESULT: FAIL", p.stdout)
        finally:
            shutil.rmtree(tmp)


if __name__ == "__main__":
    unittest.main()
