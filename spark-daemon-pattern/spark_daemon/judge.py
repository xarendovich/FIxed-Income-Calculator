"""One judge (r4.11, R-7): a registry of checks, and the profiles that select from it.

validate, precheck and the battery used to be three judges: validate built its own
diagnostics, precheck re-labelled four battery functions as PC-01 to PC-04, and the battery
kept a third list. Two judges of one thing drift: HF-37 was precheck and the battery binding
different copies of the same manifest. Now there is one registry. Each check has one ID, one
meaning and one implementation; a profile only chooses which checks run, plus a few
parameters (how many cycles a short run takes). The same failing invariant gives the same ID
and the same reason in every profile that contains it, and a check whose meaning changes gets
a new ID (PD-78).

Profiles:
  validate   DB-01 DB-02 DB-24 DB-25     static: no process is started (milliseconds)
  precheck   validate + DB-03 DB-04      a short confined run (seconds); offline, no extra tools
  battery    every registered check      the qualification profile (minutes)

Check states: PASS, FAIL, UNKNOWN (the evidence could not be gathered, for example strace is
missing), N/A (does not apply to this daemon), SKIPPED (not run, because a static check it
depends on failed). One verdict rule for every profile: FAIL if any check FAILs, INCOMPLETE if
any is UNKNOWN, PASS otherwise. Missing evidence is never a pass.

A verdict is feedback unless it comes from the qualifying profile: the full battery, not
--quick. Every report says which profile produced it and whether it qualifies. Only a
qualifying PASS may emit an installable unit (R-3), and even that is evidence for a human's
activation decision, never permission.
"""

import os
import random
from dataclasses import dataclass, field

from . import manifest as manifest_mod
from . import purity, unitgen
from .canonical import sha256_hex

PASS, FAIL, UNKNOWN, NA, SKIPPED = "PASS", "FAIL", "UNKNOWN", "N/A", "SKIPPED"
STATES = (PASS, FAIL, UNKNOWN, NA, SKIPPED)
QUALIFYING_PROFILE = "battery"
PRECHECK_CYCLES = 2
BATTERY_CYCLES = 5


def diagnostic(layer, message, *, where="", line=None, severity="error") -> dict:
    """The one diagnostic shape (modelled on `terraform validate -json`)."""
    return {"layer": layer, "severity": severity, "where": where, "line": line, "message": message}


@dataclass
class Check:
    id: str
    title: str
    state: str = UNKNOWN
    evidence: str = ""
    diagnostics: list = field(default_factory=list)


@dataclass(frozen=True)
class Spec:
    id: str
    title: str
    kind: str          # "static" (no process is started) or "run" (runs the daemon in a workspace)
    run: object        # callable(judgement, check)


def verdict(states) -> str:
    """The only verdict rule. Every profile's result comes from here."""
    states = list(states)
    if FAIL in states:
        return "FAIL"
    if UNKNOWN in states:
        return "INCOMPLETE"
    return "PASS"


def exit_code(result) -> int:
    """The one mapping from a verdict to a process exit code: PASS 0, INCOMPLETE 3, FAIL 1."""
    from . import EXIT_FAILED, EXIT_FLAGGED, EXIT_OK
    return {"PASS": EXIT_OK, "INCOMPLETE": EXIT_FLAGGED}.get(result, EXIT_FAILED)


class Judgement:
    """What one run of one profile saw and decided."""

    def __init__(self, profile, manifest_path, *, envelope=None, workdir=None, seed=20260927,
                 quick=False, threshold=20, unit_options=None):
        if profile not in PROFILES:
            raise ValueError(f"unknown profile {profile!r}")
        self.profile = profile
        self.manifest_path = os.path.abspath(manifest_path)
        self.directory = os.path.dirname(self.manifest_path)
        self.code_path = os.path.join(self.directory, "daemon.py")
        self.envelope = envelope
        self.workdir = workdir
        self.seed = seed
        self.quick = quick
        self.threshold = threshold
        # The options of the unit being qualified (r4.11, R-3): inputs to the judgement, so DB-15
        # and DB-25 judge the exact unit an emitted record binds.
        opts = dict(unit_options or {})
        self.unit_options = {"python": opts.get("python") or "/usr/bin/python3",
                             "require_paths": list(opts.get("require_paths") or ()),
                             "part_of": opts.get("part_of")}
        self.rng = random.Random(seed)
        self.m = None                 # the manifest as written: the identity reports bind
        self.code_sha = None
        self.candidate = None
        self.ws = None                # battery.Workspace, for profiles with run checks
        self.checks = []
        self.result = None

    @property
    def qualifying(self) -> bool:
        return self.profile == QUALIFYING_PROFILE and not self.quick

    def diagnostics(self) -> list:
        return [d for c in self.checks for d in c.diagnostics]


# ---------------------------------------------------------------- static checks

def _manifest_diags(problems):
    import re
    out = []
    for problem in problems:
        m = re.match(r"^([A-Za-z_][\w.\[\]]*): (.*)$", problem, re.S)
        where, message = (m.group(1), m.group(2)) if m else ("manifest", problem)
        out.append(diagnostic("manifest", message, where=where))
    return out


def _purity_diags(problems):
    import re
    out = []
    for problem in problems:
        m = re.match(r"^[^:]+:(\d+): (.*)$", problem, re.S)
        if m:
            out.append(diagnostic("purity", m.group(2), where="daemon.py", line=int(m.group(1))))
            continue
        m = re.match(r"^[^:]+: (.*)$", problem, re.S)
        out.append(diagnostic("purity", m.group(1) if m else problem, where="daemon.py"))
    return out


def _db01(j, c):
    try:
        j.m = manifest_mod.load(j.manifest_path)
    except manifest_mod.ManifestError as e:
        c.state, c.evidence = FAIL, "; ".join(e.problems[:5])
        c.diagnostics = _manifest_diags(e.problems)
        return
    c.state, c.evidence = PASS, f"manifest sha256 {j.m.sha256[:16]}"


def _db02(j, c):
    problems = purity.check_file(j.code_path)
    if os.path.isfile(j.code_path):
        with open(j.code_path, "rb") as fh:
            j.code_sha = sha256_hex(fh.read())
    c.state = FAIL if problems else PASS
    c.evidence = "; ".join(problems[:5]) if problems else "no findings"
    c.diagnostics = _purity_diags(problems)


def _db24(j, c):
    if not j.envelope:
        c.state, c.evidence = NA, "no candidate envelope given"
        return
    from . import handoff
    summary, diags = handoff.check_envelope(j.envelope, j.directory)
    j.candidate = summary
    c.diagnostics = diags
    errors = [d for d in diags if d["severity"] == "error"]
    warnings = len(diags) - len(errors)
    if errors:
        c.state = FAIL
        c.evidence = "; ".join(f"{d['where']}: {d['message']}" for d in errors[:3])
    else:
        c.state = PASS
        c.evidence = (f"envelope matches the files; contract {summary['contract_match']}"
                      + (f"; {warnings} warning(s)" if warnings else ""))


def _db25(j, c):
    if j.m is None:
        c.state, c.evidence = SKIPPED, "not run: DB-01 failed"
        return
    from .battery import PATTERN_ROOT
    try:
        text = unitgen.generate(j.m, root=PATTERN_ROOT, **j.unit_options)
    except unitgen.UnitError as e:
        c.state, c.evidence = FAIL, str(e)
        c.diagnostics = [diagnostic("unit", str(e), where="unit")]
        return
    missing = unitgen.lint(text)
    if missing:
        c.state, c.evidence = FAIL, f"missing directives: {', '.join(missing)}"
        c.diagnostics = [diagnostic("unit", c.evidence, where="unit")]
        return
    c.state = PASS
    c.evidence = f"all {len(unitgen.REQUIRED_DIRECTIVES)} required directives present"


# ---------------------------------------------------------------- the registry

def _registry():
    from . import battery as b

    def fault(spec, label):
        return lambda j, c: b._fault(j.ws, c, spec, label)

    specs = (
        Spec("DB-01", "manifest validates", "static", _db01),
        Spec("DB-02", "purity check", "static", _db02),
        Spec("DB-03", "confinement", "run",
             lambda j, c: b.db03(j.ws, c, cycles=PRECHECK_CYCLES if j.profile == "precheck" else BATTERY_CYCLES)),
        Spec("DB-04", "ledger integrity and provenance", "run", lambda j, c: b.db04(j.ws, c)),
        Spec("DB-05", "crash and restart", "run", lambda j, c: b.db05(j.ws, c, j.rng, 3 if j.quick else 8)),
        Spec("DB-06", "torn-tail quarantine", "run", lambda j, c: b.db06(j.ws, c)),
        Spec("DB-07", "refuses a corrupt ledger", "run", lambda j, c: b.db07(j.ws, c)),
        Spec("DB-08", "fsync failure", "run",
             fault(["-e", "trace=fsync", "-e", "inject=fsync:error=EIO:when=3+"], "fsync EIO")),
        Spec("DB-09", "disk-full failure", "run",
             fault(["-e", "trace=write", "-e", "inject=write:error=ENOSPC:when=1"], "write ENOSPC")),
        Spec("DB-10", "digest containment", "run", lambda j, c: b.db10(j.ws, c, j.seed)),
        Spec("DB-11", "notify protocol", "run", lambda j, c: b.db11(j.ws, c)),
        Spec("DB-12", "single instance and SIGTERM", "run", lambda j, c: b.db12(j.ws, c)),
        Spec("DB-13", "audit hook blocks forbidden operations", "run", lambda j, c: b.db13(j.ws, c)),
        Spec("DB-14", "resource budget", "run", lambda j, c: b.db14(j.ws, c)),
        Spec("DB-15", "unit hardening", "run", lambda j, c: b.db15(j.ws, c, j.threshold, j.unit_options)),
        Spec("DB-16", "read-only verification", "run", lambda j, c: b.db16(j.ws, c)),
        Spec("DB-17", "Landlock blocks with the audit hook record-only", "run", lambda j, c: b.db17(j.ws, c)),
        Spec("DB-18", "DAEMON_START.landlock matches host and manifest", "run", lambda j, c: b.db18(j.ws, c)),
        # DB-19 is reserved for the direct cgroup memory reading (PD-76).
        Spec("DB-20", "observes within a blind limit", "run", lambda j, c: b.db20(j.ws, c)),
        # DB-21 to DB-23 are reserved for the budget table, the independent verifier and the
        # worst-case fixtures.
        Spec("DB-24", "candidate envelope matches the files", "static", _db24),
        Spec("DB-25", "generated unit carries every required directive", "static", _db25),
    )
    return {s.id: s for s in specs}


_VALIDATE = ("DB-01", "DB-02", "DB-24", "DB-25")
PROFILES = {
    "validate": _VALIDATE,
    "precheck": _VALIDATE + ("DB-03", "DB-04"),
    "battery": None,           # every registered check
}


def registry() -> dict:
    return _registry()


def profile_ids(profile) -> list:
    ids = PROFILES[profile]
    return sorted(registry()) if ids is None else sorted(ids)


def _guarded(spec, j, c):
    try:
        spec.run(j, c)
    except Exception as e:  # noqa: BLE001 - a crashed check is a failed check, with evidence
        c.state, c.evidence = FAIL, f"check crashed: {type(e).__name__}: {str(e)[:160]}"
    if c.state not in STATES:
        c.state, c.evidence = FAIL, f"check reported an unknown state {c.state!r}"
    if c.state == FAIL and not any(d["severity"] == "error" for d in c.diagnostics):
        c.diagnostics.append(diagnostic(c.id, c.evidence or "failed"))


def run(profile, manifest_path, **options) -> Judgement:
    """Runs one profile. Static checks first; run checks only when every static check passed
    and the workspace's copy of the manifest loads."""
    j = Judgement(profile, manifest_path, **options)
    reg = registry()
    specs = [reg[i] for i in profile_ids(profile)]
    checks = {s.id: Check(s.id, s.title) for s in specs}
    for s in specs:
        if s.kind == "static":
            _guarded(s, j, checks[s.id])
    runs = [s for s in specs if s.kind == "run"]
    if runs:
        failed = [s.id for s in specs if s.kind == "static" and checks[s.id].state == FAIL]
        if failed or j.m is None:
            for s in runs:
                checks[s.id].state = SKIPPED
                checks[s.id].evidence = f"not run: {', '.join(failed) or 'DB-01'} failed"
        else:
            from . import battery
            problem = None
            try:
                j.ws = battery.Workspace(j.manifest_path, j.workdir)
                # HF-37: run checks bind the workspace's copy (an absolute output_dir is
                # rewritten into the workspace); reports bind the manifest as written.
                j.ws.bind_manifest(manifest_mod.load(j.ws.manifest))
            except (manifest_mod.ManifestError, OSError) as e:
                problem = f"the workspace could not be prepared: {e}"
            for s in runs:
                if problem:
                    checks[s.id].state, checks[s.id].evidence = FAIL, problem
                else:
                    _guarded(s, j, checks[s.id])
    j.checks = [checks[s.id] for s in specs]
    j.result = verdict(c.state for c in j.checks)
    return j
