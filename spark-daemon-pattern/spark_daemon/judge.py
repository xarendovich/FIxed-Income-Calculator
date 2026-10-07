"""One judge (r4.11, R-7): a registry of checks, the profiles that select from it, and the one
report every profile writes (contract 5, E-10).

validate, precheck and the battery used to be three judges: validate built its own
diagnostics, precheck re-labelled four battery functions as PC-01 to PC-04, and the battery
kept a third list. Two judges of one thing drift: HF-37 was precheck and the battery binding
different copies of the same manifest. Now there is one registry. Each check has one ID, one
meaning and one implementation; a profile only chooses which checks run, plus a few
parameters (how many cycles a short run takes). The same failing invariant gives the same ID
and the same reason in every profile that contains it, and a check whose meaning changes gets
a new ID (PD-78).

Profiles, nested (contract 5: PRECHECK within VALIDATE within BATTERY):
  precheck   DB-01 DB-02 DB-24 DB-25     static: no process is started (milliseconds)
  validate   precheck + DB-03 DB-04      a short confined run (seconds); offline, no extra tools
  battery    every registered check      the qualification profile (minutes), DB-22 included
Before contract 5 the first two names were the other way round; the CLI says so for one release.

Check states: PASS, FAIL, UNKNOWN (the evidence could not be gathered, for example strace is
missing), N/A (does not apply to this daemon), SKIPPED (not run, because a static check it
depends on failed). One verdict rule for every profile: FAIL if any check FAILs, INCOMPLETE if
any is UNKNOWN, PASS otherwise. Missing evidence is never a pass.

The report (spark-daemon-report/1) holds facts only: what was judged, with which inputs, on
which host, and each check's state and evidence. The verdict and whether the report qualifies
are conclusions, derived on read by conclusions(); they are never stored, so a report cannot
say PASS over a failed check. A report qualifies only if it comes from the full battery (not
--quick), ran every battery check, and every check passed or did not apply. The installable
unit is a projection of a qualifying report (unit_from_report): the report binds every input
of the unit generator, so the unit is reproducible from the report alone (E-11). That is
evidence for a person's activation decision, never permission.
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
SHORT_RUN_CYCLES = 2          # DB-03 in the validate profile
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
        # Every input of the unit generator (E-11), fixed before anything runs: DB-25 and DB-15
        # judge exactly the unit the report binds, and unit_from_report reproduces it. The home
        # is taken now, before the battery's workspace changes this process's environment (HF-41).
        from .battery import PATTERN_ROOT
        from .paths import home
        opts = dict(unit_options or {})
        self.unit_inputs = {"pattern_root": PATTERN_ROOT,
                            "python": opts.get("python") or "/usr/bin/python3",
                            "require_paths": list(opts.get("require_paths") or ()),
                            "part_of": opts.get("part_of"),
                            "spark_daemon_home": home()}
        self.unit_text = None         # the unit DB-25 generated, with the expected digests
        self.manifest_written = None  # the manifest object as read by DB-01
        self.rng = random.Random(seed)
        self.m = None                 # the manifest as written: the identity reports bind
        self.code_sha = None
        self.candidate = None
        self.ws = None                # battery.Workspace, for profiles with run checks
        self.checks = []
        self.result = None

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
        # One read: the manifest the report embeds is exactly the one judged.
        written = manifest_mod.to_dict(j.manifest_path)
        j.m = manifest_mod.parse(written, j.manifest_path)
        j.manifest_written = written
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


def expected_digests(j) -> dict | None:
    """The digests the unit's ExecStart carries and the runtime checks at every start, or None
    when the manifest or daemon.py could not be identified."""
    from .contract import contract_identity
    if j.m is None or j.code_sha is None:
        return None
    return {"manifest_sha256": j.m.sha256, "daemon_code_sha256": j.code_sha,
            "contract_sha256": contract_identity()["contract_sha256"]}


def _generate(m, inputs, expect):
    return unitgen.generate(m, root=inputs["pattern_root"], python=inputs["python"],
                            require_paths=inputs["require_paths"], part_of=inputs["part_of"],
                            expect=expect, daemon_home=inputs["spark_daemon_home"])


def _db25(j, c):
    if j.m is None:
        c.state, c.evidence = SKIPPED, "not run: DB-01 failed"
        return
    try:
        text = _generate(j.m, j.unit_inputs, expected_digests(j))
    except unitgen.UnitError as e:
        c.state, c.evidence = FAIL, str(e)
        c.diagnostics = [diagnostic("unit", str(e), where="unit")]
        return
    j.unit_text = text
    problems = [f"missing directive {d}" for d in unitgen.lint(text)] + unitgen.timing_problems(text, j.m)
    if problems:
        c.state, c.evidence = FAIL, "; ".join(problems)
        c.diagnostics = [diagnostic("unit", p, where="unit") for p in problems]
        return
    c.state = PASS
    c.evidence = (f"all {len(unitgen.REQUIRED_DIRECTIVES)} required directives present; watchdog "
                  f"{j.m.watchdog_seconds} s and stop timeout {j.m.stop_timeout_seconds} s derived from a "
                  f"{j.m.cycle_budget_seconds} s cycle budget")


# ---------------------------------------------------------------- the registry

def _registry():
    from . import battery as b

    def fault(spec, label):
        return lambda j, c: b._fault(j.ws, c, spec, label)

    specs = (
        Spec("DB-01", "manifest validates", "static", _db01),
        Spec("DB-02", "purity check", "static", _db02),
        Spec("DB-03", "confinement", "run",
             lambda j, c: b.db03(j.ws, c, cycles=SHORT_RUN_CYCLES if j.profile == "validate" else BATTERY_CYCLES)),
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
        Spec("DB-15", "unit hardening", "run", lambda j, c: b.db15(j.ws, c, j.threshold, j.unit_text)),
        Spec("DB-16", "read-only verification", "run", lambda j, c: b.db16(j.ws, c)),
        Spec("DB-17", "Landlock blocks with the audit hook record-only", "run", lambda j, c: b.db17(j.ws, c)),
        Spec("DB-18", "DAEMON_START.landlock matches host and manifest", "run", lambda j, c: b.db18(j.ws, c)),
        # DB-19 is reserved for the direct cgroup memory reading (PD-76).
        Spec("DB-20", "observes within a blind limit", "run", lambda j, c: b.db20(j.ws, c)),
        # DB-21 is reserved for the budget table and DB-23 for the worst-case fixtures.
        Spec("DB-22", "a second, independent verifier agrees", "run", lambda j, c: b.db22(j.ws, c)),
        Spec("DB-24", "candidate envelope matches the files", "static", _db24),
        Spec("DB-25", "generated unit: every required directive, timings from the cycle budget", "static", _db25),
    )
    return {s.id: s for s in specs}


_PRECHECK = ("DB-01", "DB-02", "DB-24", "DB-25")
PROFILES = {
    "precheck": _PRECHECK,
    "validate": _PRECHECK + ("DB-03", "DB-04"),
    "battery": None,           # every registered check
}
# Contract 5 swapped the first two names so that the profiles nest in the order they run.
RENAMED = {
    "precheck": "since contract 5, precheck is the static checks (DB-01, DB-02, DB-24, DB-25), which "
                "were called validate; the short confined run is now validate",
    "validate": "since contract 5, validate adds the short confined run (DB-03, DB-04), which was "
                "called precheck; the static checks alone are now precheck",
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


# ---------------------------------------------------------------- the one report (E-10)

REPORT_SCHEMA = "spark-daemon-report/1"
AUTHORITY_NOTE = ("Evidence only. This report installs, enables and activates nothing; activation is "
                  "a separate decision by a person with that authority.")


def report(j) -> dict:
    """The facts of one judgement. No verdict, no "qualifying", no "valid": conclusions() derives
    those on read, from the checks below."""
    import shutil
    import sys
    from . import SYSTEM_PATH, VERSION
    from .contract import contract_identity
    from .qualify import host_facts
    ws = j.ws
    return {
        "schema": REPORT_SCHEMA,
        "skeleton_version": VERSION,
        **contract_identity(),
        "profile": j.profile,
        "quick": j.quick,
        "seed": j.seed,
        "daemon": j.m.name if j.m else None,
        "manifest_path": j.manifest_path,
        "manifest_sha256": j.m.sha256 if j.m else None,
        "manifest": j.manifest_written if j.m is not None else None,
        "daemon_code_sha256": j.code_sha,
        "candidate": j.candidate,
        "unit": {**j.unit_inputs,
                 "unit_name": unitgen.unit_name(j.m) if j.m else None,
                 "expect": expected_digests(j),
                 "unit_sha256": sha256_hex(j.unit_text.encode()) if j.unit_text else None},
        "host": host_facts(),
        "environment": {"python": sys.version.split()[0], "kernel": os.uname().release,
                        "machine": os.uname().machine,
                        "strace": bool(shutil.which("strace", path=SYSTEM_PATH)),
                        "systemd_analyze": bool(shutil.which("systemd-analyze", path=SYSTEM_PATH))},
        # Until contract 5's third cut the run checks use a workspace copy of the manifest.
        "workspace": ws.root if ws is not None else None,
        "workspace_manifest_sha256": ws.m.sha256 if ws is not None and ws.m else None,
        "rewrites": list(ws.rewrites) if ws is not None else [],
        "checks": [{"id": c.id, "title": c.title, "state": c.state, "evidence": c.evidence,
                    "diagnostics": c.diagnostics} for c in j.checks],
        "authority": AUTHORITY_NOTE,
    }


def conclusions(rep) -> dict:
    """What a report means, derived from its facts and nothing else: the verdict, whether it
    qualifies for an installable unit, and why not."""
    checks = rep.get("checks") if isinstance(rep, dict) else None
    if rep.get("schema") != REPORT_SCHEMA or not isinstance(checks, list):
        return {"result": "FAIL", "qualifies": False, "why_not": [f"not a {REPORT_SCHEMA} report"]}
    states = [c.get("state") if isinstance(c, dict) else None for c in checks]
    result = verdict(s if s in STATES else FAIL for s in states)
    why_not = []
    if rep.get("profile") != QUALIFYING_PROFILE:
        why_not.append(f"the {rep.get('profile')} profile does not qualify; only the full battery does")
    if rep.get("quick") is not False:
        why_not.append("a --quick battery does not qualify")
    ids = sorted(c.get("id") for c in checks if isinstance(c, dict))
    if rep.get("profile") == QUALIFYING_PROFILE and ids != profile_ids(QUALIFYING_PROFILE):
        why_not.append("the report does not hold exactly the battery's checks")
    if result != "PASS":
        why_not.append(f"the result is {result}, not PASS")
    unit = rep.get("unit") or {}
    if not (unit.get("expect") and unit.get("unit_sha256") and rep.get("manifest")):
        why_not.append("the report does not bind a complete unit")
    return {"result": result, "qualifies": not why_not, "why_not": why_not}


def unit_from_report(rep) -> tuple:
    """(unit_name, unit_text) for a report, from the report alone (E-11). Raises ValueError when
    the report's own facts disagree or the generator no longer reproduces the unit the battery
    judged (another skeleton version, or a symlink that now resolves elsewhere). It does not
    decide whether the report qualifies (conclusions) or whether the files still match (the
    runtime checks that at every start)."""
    from . import manifest as manifest_mod
    unit = rep.get("unit") or {}
    try:
        m = manifest_mod.parse(rep["manifest"], rep["manifest_path"])
    except (KeyError, TypeError, manifest_mod.ManifestError) as e:
        raise ValueError(f"the report's manifest does not load ({e.__class__.__name__})") from None
    if m.sha256 != rep.get("manifest_sha256"):
        raise ValueError("the report's manifest does not match its own manifest_sha256")
    try:
        text = _generate(m, unit, unit.get("expect"))
    except (KeyError, unitgen.UnitError) as e:
        raise ValueError(f"the unit cannot be generated from the report: {e}") from None
    if sha256_hex(text.encode()) != unit.get("unit_sha256"):
        raise ValueError("the generator does not reproduce the unit the battery judged (another "
                         "skeleton version, or a path that now resolves elsewhere): run the battery again")
    return unitgen.unit_name(m), text


def print_report(rep, *, report_path=None, as_json=False) -> int:
    """Presentation only: prints a report (as JSON, or as lines for people ending in the
    RESULT: line installers read) and returns the exit code of its derived verdict."""
    import json
    found = conclusions(rep)
    if as_json:
        print(json.dumps(rep, indent=2, ensure_ascii=False))
        return exit_code(found["result"])
    checks = rep["checks"]
    width = max((len(c["title"]) for c in checks), default=0)
    for c in checks:
        print(f"{c['id']}  {c['state']:<10} {c['title']:<{width}}  {c['evidence']}")
    for c in checks:
        for d in c.get("diagnostics") or []:
            where = d["where"] + (f":{d['line']}" if d["line"] else "")
            prefix = "" if d["severity"] == "error" else "warning: "
            print(f"{d['layer']}: {prefix}{where}: {d['message']}" if where else f"{d['layer']}: {d['message']}")
    for note in rep.get("rewrites") or []:
        print(f"note: battery rewrote {note}")
    if rep.get("manifest_sha256"):
        print(f"manifest sha256 {rep['manifest_sha256']}")
    if rep.get("candidate"):
        print(f"candidate envelope sha256 {rep['candidate']['envelope_sha256']} "
              f"(contract {rep['candidate']['contract_match']})")
    if report_path:
        print(f"report: {report_path}")
    if rep.get("profile") == QUALIFYING_PROFILE:
        if found["qualifies"]:
            print(f"qualifies: yes. The installable unit is `spark-daemon unit --report {report_path or '<report>'}`;"
                  " installing and enabling it are a person's decision")
        else:
            print("qualifies: no (" + "; ".join(found["why_not"]) + ")")
    print(f"RESULT: {found['result']}")
    return exit_code(found["result"])
