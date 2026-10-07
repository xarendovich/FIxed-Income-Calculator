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
from spark_daemon import SYSTEM_PATH, guard, manifest, purity, render, unitgen

SENSE_DECIDE = '''
def decide(prev, snapshot):
    return [("VALUE_OBSERVED", {"value": str(snapshot["value"])[:200]})]
'''


def fixture_with(source: str):
    """A throwaway fixture directory: the counter manifest with a new daemon.py."""
    tmp = tempfile.mkdtemp(prefix="spark-fixture-")
    with open(os.path.join(FIXTURES, "counter", "manifest.json")) as fh:
        data = json.load(fh)
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

    def test_a_commands_list_is_refused_with_its_migration(self):
        # Contract 5 (R-2): a daemon runs no program, so the commands key is gone; any value,
        # the empty list included, is refused with the reason. Replaces the r3 allow/deny tests
        # of individual command names (test_interpreter_families_and_command_runners_are_refused,
        # test_harmless_commands_still_allowed).
        for value in ([], ["git"], ["python3.12"]):
            with self.subTest(commands=value):
                problems = self.problems(commands=value)
                self.assertEqual(len(problems), 1, problems)
                self.assertTrue(problems[0].startswith("commands: removed in contract 5.0.0"), problems)

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

    def test_a_deny_list_is_refused_with_its_migration(self):
        # r4.11 (R-1): contract 4.0.0 removes the manifest's deny list outright; a v4 manifest
        # that still has one is refused with the reason, not as a bare unknown key.
        problems = self.problems(deny=["~/out"])
        self.assertTrue(any(p.startswith("deny: removed in contract 4.0.0") for p in problems), problems)

    def test_no_read_may_contain_an_always_denied_path(self):
        # r4.11 (R-1, PD-100): no gaps. Landlock cannot carve ~/.ssh out of a grant on ~, so a
        # read of ~ was enforced only in Python and by the unit (F-1). Now it is refused.
        problems = self.problems(reads=["~"])
        self.assertTrue(any("contains the always-denied path ~/.ssh" in p for p in problems), problems)

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

    def test_ctx_offers_no_way_to_run_a_program(self):
        # Contract 5 (R-2): ctx.run and ctx.git are gone, so the r3 test that an undeclared
        # command fails closed (test_undeclared_command_request_fails_closed_even_when_swallowed)
        # becomes: there is nothing to request, and asking starts no process.
        source = ("def sense(ctx):\n    found = []\n    for name in ('run', 'git'):\n        try:\n"
                  "            ctx.run if name == 'run' else ctx.git\n            found.append(name)\n"
                  "        except AttributeError:\n            pass\n    return {'value': ','.join(found) or 'none'}\n"
                  + SENSE_DECIDE)
        fixture = fixture_with(source)
        sb = Sandbox(fixture)
        try:
            p = sb.run(cycles=1)
            self.assertEqual(p.returncode, 0, p.stderr)
            observed = [r["payload"] for r in sb.records() if r["event_type"] == "VALUE_OBSERVED"]
            self.assertEqual(observed, [{"value": "none"}])
        finally:
            sb.cleanup()
            shutil.rmtree(fixture)

    def test_rejected_duplicate_launch_touches_nothing(self):
        # r3.2 (WBS 3.1 §4.2): the second instance cleaned tmp/ before taking the lock and
        # deleted the running instance's in-flight files (for git-watch, its index copy).
        sb = Sandbox("counter")
        try:
            first = subprocess.Popen(sb.argv(None, interval_ms=60000), env=sb.env(), stdout=subprocess.DEVNULL,
                                     stderr=subprocess.DEVNULL)
            try:
                ledger = sb.ledger_path
                for _ in range(100):
                    if os.path.exists(ledger) and os.path.getsize(ledger):
                        break
                    import time
                    time.sleep(0.05)
                inflight = os.path.join(sb.output, "tmp", "index-copy-inflight")
                with open(inflight, "w") as fh:
                    fh.write("in use")
                p = sb.run(cycles=1)
                self.assertEqual(p.returncode, 73, p.stderr)
                self.assertTrue(os.path.exists(inflight), "a rejected duplicate deleted a running instance's file")
            finally:
                first.terminate()
                first.wait(timeout=15)
        finally:
            sb.cleanup()

    def test_torn_first_write_still_starts_the_ledger_with_daemon_start(self):
        # r3.2 (WBS 3.0 r4's pre-baseline correction): a crash during the first-ever append left
        # bytes but no complete record; the next start wrote LEDGER_TAIL_QUARANTINED as seq 1, a
        # ledger the battery's own DB-04 then rejected ("first record is not DAEMON_START").
        sb = Sandbox("counter")
        try:
            os.makedirs(sb.output, mode=0o700, exist_ok=True)
            fd = os.open(sb.ledger_path, os.O_WRONLY | os.O_CREAT, 0o600)
            os.write(fd, b'{"schema":"spark-daemon-ledger/1","seq":1')
            os.close(fd)
            p = sb.run(cycles=1)
            self.assertEqual(p.returncode, 0, p.stderr)
            recs = sb.records()
            self.assertEqual([r["event_type"] for r in recs[:2]], ["DAEMON_START", "LEDGER_TAIL_QUARANTINED"])
            self.assertIs(recs[0]["payload"]["previous_run_ended_cleanly"], False)
            self.assertEqual(recs[1]["payload"]["length"], 41)
        finally:
            sb.cleanup()

    def test_policy_is_immutable(self):
        sb = Sandbox("counter")
        try:
            os.environ["SPARK_DAEMON_HOME"] = sb.home
            m = manifest.load(sb.manifest)
            policy = guard.Policy(m)
            with self.assertRaises(AttributeError):
                policy.deny = ()
            with self.assertRaises(AttributeError):
                policy.reads = ("/",)
        finally:
            os.environ.pop("SPARK_DAEMON_HOME", None)
            sb.cleanup()


class CpuAccountingTests(unittest.TestCase):
    def test_cpu_includes_any_child_process(self):
        # A daemon runs no program since contract 5 (R-2); children are still counted so a
        # regression that spawned one would show in DB-14. Renamed from
        # test_cpu_includes_commands_the_daemon_ran.
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
                              timeout=120, env={"PATH": SYSTEM_PATH, "HOME": "/nonexistent", "LANG": "C.UTF-8"})

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
