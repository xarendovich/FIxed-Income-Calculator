"""Framework / services boundary (r3.4, PD-53; a Python reading of the Asterinas framekernel
split, and the same shape as WBS 3.0 r4's observer_ledger.py / observer_writer.py).

Services are pure: no operating-system modules and no open(). The framework holds every
system call. Python cannot enforce this the way a compiler can, so this is a structure test
over the source, like r4's semantic audit. It locks in the modules that conform today;
ledger.py, manifest.py and purity.py still mix both halves (PD-53 and PD-34)."""

import ast
import os
import unittest

from helpers import ROOT

PACKAGE = os.path.join(ROOT, "spark_daemon")

SERVICE_MODULES = ("canonical", "render")
OS_MODULES = frozenset({"os", "ctypes", "fcntl", "subprocess", "socket", "signal", "select",
                        "mmap", "shutil", "tempfile", "resource", "threading", "io", "pathlib",
                        "posix", "sys"})
# landlock.py makes the three Landlock system calls. probes.py is a test instrument: it
# tries to load ctypes inside the sandbox to prove that the attempt is blocked.
CTYPES_ALLOWED = frozenset({"landlock", "probes"})


def _tree(name):
    with open(os.path.join(PACKAGE, name + ".py"), encoding="utf-8") as fh:
        return ast.parse(fh.read(), filename=name + ".py")


def _imports(tree):
    """(absolute top-level module names, relative module names) imported anywhere in the file."""
    absolute, relative = set(), set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            absolute.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom):
            if node.level:
                if node.module:
                    relative.add(node.module.split(".")[0])
                else:
                    relative.update(alias.name for alias in node.names)
            elif node.module:
                absolute.add(node.module.split(".")[0])
    return absolute, relative


def _modules():
    return sorted(f[:-3] for f in os.listdir(PACKAGE) if f.endswith(".py"))


class LayerBoundaryTests(unittest.TestCase):
    def test_service_modules_import_nothing_that_reaches_the_os(self):
        for name in SERVICE_MODULES:
            absolute, relative = _imports(_tree(name))
            self.assertEqual(absolute & OS_MODULES, set(), f"{name}.py imports an OS module")
            self.assertLessEqual(relative, set(SERVICE_MODULES),
                                 f"{name}.py imports a framework module: {sorted(relative - set(SERVICE_MODULES))}")

    def test_service_modules_never_call_open(self):
        for name in SERVICE_MODULES:
            calls = [n.lineno for n in ast.walk(_tree(name))
                     if isinstance(n, ast.Call) and isinstance(n.func, ast.Name) and n.func.id == "open"]
            self.assertEqual(calls, [], f"{name}.py calls open() at line(s) {calls}")

    def test_ctypes_only_where_declared(self):
        users = {name for name in _modules() if "ctypes" in _imports(_tree(name))[0]}
        self.assertLessEqual(users, CTYPES_ALLOWED, f"ctypes imported in {sorted(users - CTYPES_ALLOWED)}")

    def test_exactly_one_truncate_site(self):
        # r4's audit rule: truncation destroys bytes, so it happens in one reviewed place
        # (ledger.recover, after the torn tail is copied to quarantine).
        sites = [(name, n.lineno) for name in _modules() for n in ast.walk(_tree(name))
                 if isinstance(n, ast.Attribute) and n.attr in ("ftruncate", "truncate")]
        self.assertEqual([s[0] for s in sites], ["ledger"], f"truncate sites: {sites}")


if __name__ == "__main__":
    unittest.main()
