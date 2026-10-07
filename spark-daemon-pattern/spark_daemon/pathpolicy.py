"""One canonical path policy, projected into each enforcement layer (contract 5, seam 11).

Three layers enforce what a daemon may read and write: the Python checks (ctx and the audit
hook, guard.py), the kernel (Landlock, landlock.py) and the systemd sandbox (unitgen.py). Up
to 4.x each derived its own paths from the manifest, so they could drift apart. The
enforcement stays diverse on purpose (a defect in one layer is caught by another), but the
policy they enforce is now one value, PathPolicy, and each layer is a projection of it:

    python      PathPolicy.denied / may_read / may_write    (guard.Policy, ctx, the audit hook)
    kernel      grants(policy, extra_reads) -> [(path, READ | WRITE)]    (landlock.py)
    unit        unit_paths(policy) -> ReadWritePaths / ReadOnlyPaths / InaccessiblePaths

agreement() checks that the three say the same thing for a manifest, and the self-tests run
it on every shipped manifest. The outcome for a path refused by more than one layer is
unchanged: a Python refusal is a policy violation (exit 78), pending the owner's ruling on
E-4.

Every path is expanded once, against an explicit home (SPARK_DAEMON_HOME for a unit), so no
projection depends on the environment of the process that happens to compute it (HF-41).
"""

import os
from dataclasses import dataclass

from .paths import expand, home as current_home, within

READ, WRITE = "read", "write"
# Read paths the unit does not list: /proc and /sys are already read-only under the unit's
# ProtectSystem=strict, and listing a /proc path in ReadOnlyPaths= would be redundant.
UNIT_IMPLIED_READ_ONLY = ("/proc/", "/sys/")


@dataclass(frozen=True)
class PathPolicy:
    home: str
    output_dir: str
    reads: tuple
    deny: tuple

    @classmethod
    def of(cls, manifest, *, home=None, output_dir=None) -> "PathPolicy":
        """The policy of a manifest. home is what "~" expands to (default: this process's
        home()); output_dir is the harness's recorded override for an absolute output_dir."""
        base = current_home() if home is None else os.path.realpath(home)
        return cls(home=base,
                   output_dir=expand(output_dir if output_dir is not None else manifest.output_dir, base),
                   reads=tuple(expand(p, base) for p in manifest.reads),
                   deny=tuple(expand(p, base) for p in manifest.all_deny))

    # ---- the Python projection

    def resolve(self, path: str) -> str:
        return expand(path, self.home)

    def denied(self, full: str) -> bool:
        return any(within(full, d) for d in self.deny)

    def may_read(self, full: str) -> bool:
        return not self.denied(full) and any(within(full, r) for r in self.reads)

    def may_write(self, full: str) -> bool:
        return within(full, self.output_dir) and not self.denied(full)


# ---- the kernel and unit projections. They read only output_dir, reads and deny, so anything
# with those three attributes (guard.Policy, a test's stand-in) projects the same way.

def grants(policy, extra_reads=()) -> list:
    """What the Landlock domain grants: read (never execute) on extra_reads (the interpreter and
    system paths, the daemon's folder) and on every declared read; read and write (never
    execute) on the output directory; nothing else, and execute nowhere (R-2)."""
    return ([(p, READ) for p in extra_reads] + [(p, READ) for p in policy.reads]
            + [(policy.output_dir, WRITE)])


def unit_paths(policy) -> dict:
    """The systemd sandbox's path directives, on top of ProtectSystem=strict and
    ProtectHome=read-only: the output directory writable, the reads read-only, the denied
    paths inaccessible."""
    return {"ReadWritePaths": [policy.output_dir],
            "ReadOnlyPaths": [p for p in policy.reads if not p.startswith(UNIT_IMPLIED_READ_ONLY)],
            "InaccessiblePaths": list(policy.deny)}


def gaps(policy) -> list:
    """Denied paths nested inside a granted read: Landlock cannot carve them out. Since
    contract 4.0.0 (R-1) the manifest refuses them, so for a valid manifest this is empty;
    it stays reported, never assumed away."""
    return sorted({d for d in policy.deny for r in policy.reads if within(d, r)})


def agreement(policy, unit_text: str, extra_reads=()) -> list:
    """Where the three projections disagree for one policy (empty: they agree). The unit is
    read back from its text, so this checks the unit as generated, not unit_paths()."""
    problems = []
    directives = {}
    for line in unit_text.splitlines():
        key, sep, value = line.partition("=")
        if sep and key in ("ReadWritePaths", "ReadOnlyPaths", "InaccessiblePaths"):
            directives.setdefault(key, set()).update(v.lstrip("-") for v in value.split())
    kernel = grants(policy, extra_reads)
    kernel_write = {p for p, kind in kernel if kind == WRITE}
    kernel_read = {p for p, kind in kernel if kind == READ} - set(extra_reads)
    unit_write = directives.get("ReadWritePaths", set())
    unit_read = directives.get("ReadOnlyPaths", set())
    unit_deny = directives.get("InaccessiblePaths", set())
    python_write = {p for p in kernel_write | unit_write | set(policy.reads) | set(policy.deny)
                    if PathPolicy.may_write(policy, p)}
    python_read = {p for p in set(policy.reads) | unit_read | kernel_read if PathPolicy.may_read(policy, p)}

    if not (python_write == kernel_write == unit_write == {policy.output_dir}):
        problems.append(f"writable: python {sorted(python_write)}, kernel {sorted(kernel_write)}, "
                        f"unit {sorted(unit_write)}")
    implied = {p for p in kernel_read if p.startswith(UNIT_IMPLIED_READ_ONLY)}
    if not (python_read == kernel_read == unit_read | implied):
        problems.append(f"readable: python {sorted(python_read)}, kernel {sorted(kernel_read)}, "
                        f"unit {sorted(unit_read | implied)}")
    if unit_deny != set(policy.deny):
        problems.append(f"denied: python {sorted(policy.deny)}, unit {sorted(unit_deny)}")
    for path, _ in kernel:
        inside = [d for d in policy.deny if within(path, d)]
        if inside:
            problems.append(f"the kernel grants {path}, inside the denied {inside[0]}")
    for d in gaps(policy):
        problems.append(f"the kernel cannot keep the denied {d} out of a granted read")
    for d in policy.deny:
        # denied() on its own, too: the audit hook refuses an open anywhere under a denied path,
        # not only under a read (the no-gaps rule keeps reads clear of them).
        if (not PathPolicy.denied(policy, d) or PathPolicy.may_read(policy, d)
                or PathPolicy.may_write(policy, d)):
            problems.append(f"python allows the denied {d}")
    return problems
