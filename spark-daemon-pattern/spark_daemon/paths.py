"""Path expansion shared by the manifest validator, the guard and the unit generator.

"~" expands to SPARK_DAEMON_HOME when it is set, otherwise to HOME. The unit generator
sets SPARK_DAEMON_HOME to the home directory of the person who generated the unit, so a
daemon running as a dedicated system user still resolves "~/spark-core" to the same place
the manifest's author meant.
"""

import os


def home() -> str:
    value = os.environ.get("SPARK_DAEMON_HOME") or os.path.expanduser("~")
    return os.path.realpath(value)


def expand(path: str, base: str | None = None) -> str:
    """Expand "~" (to base when given, otherwise home()) and make the path absolute and
    canonical (symlinks resolved). The unit generator passes base explicitly (HF-41), so a unit
    never depends on the environment of the process that happens to generate it."""
    top = home() if base is None else os.path.realpath(base)
    if path == "~":
        path = top
    elif path.startswith("~/"):
        path = os.path.join(top, path[2:])
    return os.path.realpath(path)


def within(path: str, root: str) -> bool:
    """True if path equals root or lies inside it. Both must already be expanded."""
    if path == root:
        return True
    return path.startswith(root.rstrip("/") + "/")
