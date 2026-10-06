"""Unit generator, purity check, battery and source hygiene."""

import json
import os
import re
import subprocess
import sys
import unicodedata
import unittest

from helpers import EXAMPLE, ENTRY, FIXTURES, ROOT
from spark_daemon import manifest, proc, purity, unitgen


class UnitgenTests(unittest.TestCase):
    def test_example_unit_carries_every_required_directive(self):
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        text = unitgen.generate(m, root=ROOT)
        self.assertEqual(unitgen.lint(text), [])
        self.assertIn("User=spark-meminfo", text)
        self.assertIn("InaccessiblePaths=", text)
        self.assertNotIn("ReadOnlyPaths=-/proc", text)          # /proc is already read-only

    def test_lint_detects_a_removed_directive(self):
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        text = unitgen.generate(m, root=ROOT).replace("PrivateNetwork=yes\n", "")
        self.assertEqual(unitgen.lint(text), ["PrivateNetwork=yes"])

    def test_user_units_carry_a_warning(self):
        m = manifest.load(os.path.join(FIXTURES, "counter", "manifest.json"))
        text = unitgen.generate(m, root=ROOT)
        self.assertIn("WARNING: user unit", text)
        self.assertIn("WantedBy=default.target", text)
        self.assertNotIn("User=", text)

    def test_a_required_path_becomes_a_start_condition(self):
        # r4.8 (from the first pilot): with a locked drive, a daemon whose output is on it must
        # be skipped by systemd, not stopped fail-closed (78, never restarted).
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        marker = "/mnt/project/.volume-marker"
        text = unitgen.generate(m, root=ROOT, require_paths=[marker])
        unit_section = text.split("[Service]")[0]
        self.assertIn(f"ConditionPathExists={marker}\n", unit_section)
        self.assertEqual(unitgen.lint(text), [])
        self.assertNotIn("ConditionPathExists", unitgen.generate(m, root=ROOT))
        plan = unitgen.install_plan(m, root=ROOT, require_paths=[marker])
        self.assertIn(f"--require-path {marker}", plan)
        with self.assertRaises(unitgen.UnitError):
            unitgen.generate(m, root=ROOT, require_paths=["relative/marker"])

    def test_part_of_binds_the_unit_to_another(self):
        # r4.8 (from the first pilot): an open ledger keeps a drive busy, so a daemon writing to
        # a drive another unit owns must stop before it and start with it, never at boot.
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        owner = "project-stack.service"
        text = unitgen.generate(m, root=ROOT, part_of=owner)
        unit_section = text.split("[Service]")[0]
        self.assertIn(f"PartOf={owner}\n", unit_section)
        self.assertIn(f"After={owner}\n", unit_section)
        self.assertIn(f"WantedBy={owner}\n", text)
        self.assertNotIn("multi-user.target", text)
        self.assertEqual(unitgen.lint(text), [])
        self.assertNotIn("PartOf=", unitgen.generate(m, root=ROOT))
        plan = unitgen.install_plan(m, root=ROOT, part_of=owner)
        self.assertIn(f"--part-of {owner}", plan)
        self.assertIn(f"starts and stops with {owner}", plan)
        for bad in ("project-stack", "a b.service", "x.service\nExecStart=/bin/sh"):
            with self.assertRaises(unitgen.UnitError):
                unitgen.generate(m, root=ROOT, part_of=bad)

    def test_install_plan_is_text_only(self):
        m = manifest.load(os.path.join(EXAMPLE, "manifest.json"))
        plan = unitgen.install_plan(m, root=ROOT)
        self.assertTrue(plan.startswith("# Install plan"))
        self.assertIn("Nothing below has been run", plan)
        self.assertIn("never grant anything under ~/spark-core/data", plan)


GOOD = '''"""Doc."""
import re

LIMIT = re.compile("x")


def sense(ctx):
    return {"v": ctx.read_text("~/data/v")}


def decide(prev, snapshot):
    return []
'''

BAD = {
    "import os": "import of 'os'",
    "from subprocess import run": "import from 'subprocess'",
    "from . import x": "relative imports",
    "from re import *": "star imports",
    "X = open('f')": "call to open()",
    "Y = eval('1')": "call to eval()",
    "Z = getattr(1, 'real')": "call to getattr()",
    "W = (1).__class__": "dunder attribute",
    "V = __builtins__": "dunder name",
    "if True:\n    pass": "top-level If",
    "for i in range(3):\n    pass": "top-level For",
    "def helper():\n    global LIMIT": "global and nonlocal",
    "async def later():\n    pass": "async code",
}


class PurityTests(unittest.TestCase):
    def test_good_module(self):
        self.assertEqual(purity.check_source(GOOD), [])

    def test_example_is_pure(self):
        self.assertEqual(purity.check_file(os.path.join(EXAMPLE, "daemon.py")), [])

    def test_each_forbidden_construct(self):
        for snippet, expected in BAD.items():
            with self.subTest(snippet=snippet):
                problems = purity.check_source(GOOD + "\n" + snippet + "\n")
                self.assertTrue(any(expected in p for p in problems), problems)

    def test_required_functions_and_arity(self):
        self.assertTrue(any("missing required function decide()" in p
                            for p in purity.check_source("def sense(ctx):\n    return {}\n")))
        wrong = GOOD.replace("def decide(prev, snapshot):", "def decide(snapshot):")
        self.assertTrue(any("exactly 2" in p for p in purity.check_source(wrong)))


class BatteryTests(unittest.TestCase):
    def run_battery(self, manifest_path):
        return subprocess.run([sys.executable, "-I", "-B", ENTRY, "battery", "--manifest", manifest_path,
                               "--quick"], capture_output=True, text=True, timeout=300,
                              env={"PATH": proc.SYSTEM_PATH, "HOME": "/nonexistent", "LANG": "C.UTF-8"})

    def test_example_passes_or_is_incomplete_never_fails(self):
        p = self.run_battery(os.path.join(EXAMPLE, "manifest.json"))
        last = p.stdout.strip().splitlines()[-1]
        self.assertIn(last, ("RESULT: PASS", "RESULT: INCOMPLETE"), p.stdout[-2000:])
        self.assertNotIn(" FAIL ", p.stdout)
        report_path = re.search(r"report: (\S+)", p.stdout).group(1)
        with open(report_path) as fh:
            report = json.load(fh)
        ids = [c["id"] for c in report["checks"]]
        self.assertEqual(ids[-4:], ["DB-18", "DB-20", "DB-24", "DB-25"])   # DB-19, DB-21 to 23 reserved
        self.assertEqual(len(ids), 21)
        self.assertEqual((report["profile"], report["qualifying"]), ("battery", False))   # --quick
        self.assertEqual(report["schema"], "spark-daemon-battery/1")

    def test_impure_daemon_fails(self):
        p = self.run_battery(os.path.join(FIXTURES, "opener", "manifest.json"))
        self.assertTrue(p.stdout.strip().endswith("RESULT: FAIL"))
        self.assertRegex(p.stdout, r"DB-02\s+FAIL")
        self.assertRegex(p.stdout, r"DB-03\s+SKIPPED\s+.*not run: DB-02 failed")


class RuntimeStructureTests(unittest.TestCase):
    def test_gc_collect_runs_once_per_cycle_not_inside_the_sleep_loop(self):
        """Structural guard for the multi-daemon GC-forcing addition (r2): gc.collect() must
        be called exactly once per cycle of the outer loop, and not inside the inner sleep
        loop (which would call it many times per cycle, once per watchdog ping)."""
        import ast

        def is_gc_collect(node):
            return (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
                   and node.func.attr == "collect" and isinstance(node.func.value, ast.Name)
                   and node.func.value.id == "gc")

        path = os.path.join(ROOT, "spark_daemon", "runtime.py")
        with open(path, encoding="utf-8") as fh:
            tree = ast.parse(fh.read(), filename=path)
        run_fn = next(n for n in ast.walk(tree) if isinstance(n, ast.FunctionDef) and n.name == "run")
        whiles = [n for n in ast.walk(run_fn) if isinstance(n, ast.While)]
        self.assertEqual(len(whiles), 2, "expected exactly the cycle loop and the sleep loop")
        outer = next(w for w in whiles if any(inner is not w and inner in ast.walk(w) for inner in whiles))
        inner = next(w for w in whiles if w is not outer)

        in_inner = {id(n) for n in ast.walk(inner) if is_gc_collect(n)}
        per_cycle = [n for n in ast.walk(outer) if is_gc_collect(n) and id(n) not in in_inner]
        self.assertEqual(len(per_cycle), 1, "expected exactly one gc.collect() per cycle")
        self.assertEqual(len(in_inner), 0, "gc.collect() must not run inside the sleep loop")


class SourceHygieneTests(unittest.TestCase):
    def test_no_raw_invisible_characters_in_source(self):
        """Trojan-source guard: bidi, zero-width and control characters appear only as escapes."""
        offenders = []
        for base, _, files in os.walk(ROOT):
            for name in files:
                if name.endswith((".py", ".json", ".md")) or name == "spark-daemon":
                    path = os.path.join(base, name)
                    with open(path, encoding="utf-8") as fh:
                        text = fh.read()
                    for ch in set(text):
                        if ch not in "\n\t" and unicodedata.category(ch) in ("Cc", "Cf", "Zl", "Zp", "Co", "Cs"):
                            offenders.append(f"{os.path.relpath(path, ROOT)}: U+{ord(ch):04X}")
        self.assertEqual(offenders, [])


if __name__ == "__main__":
    unittest.main()
