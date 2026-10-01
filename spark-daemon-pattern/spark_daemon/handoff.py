"""The inbound half of the handoff: candidate envelopes, the structured validate report and
the precheck fast lane.

Whatever produces a candidate daemon (a person, a script or a model) hands back a folder:

    manifest.json    the closed-schema manifest
    daemon.py        sense / decide / digest
    candidate.json   the candidate envelope (spark-daemon-candidate/1), optional for people

The envelope names the exact files (sha256) and the contract they were built against
(contract_version and contract_sha256, from `spark-daemon describe`). Its schema is closed
and has no field for results, verdicts or approvals: a candidate cannot carry a claim about
itself. validate and precheck answer with their own reports, which quote the envelope's
digest the way a ProcessingObservation quotes the HandoffEnvelope it observed.

Reports use one diagnostic shape everywhere (modelled on `terraform validate -json`):
    {"layer", "severity": "error"|"warning", "where", "line", "message"}
"""

import os
import re

from . import manifest as mf
from . import purity
from .canonical import CanonicalError, canonical_bytes, sha256_hex, strict_loads
from .contract import (AUTHORITY, CANDIDATE_SCHEMA, PRECHECK_SCHEMA, VALIDATE_SCHEMA,
                       contract_identity)

ENVELOPE_NAME = "candidate.json"
ENVELOPE_MAX_BYTES = 16384
SUBJECT_NAMES = ("daemon.py", "manifest.json")
PRODUCER_KINDS = ("human", "script", "model")
_HEX64 = re.compile(r"[0-9a-f]{64}")
_SEMVER = re.compile(r"(0|[1-9]\d{0,3})\.(0|[1-9]\d{0,3})\.(0|[1-9]\d{0,3})")
_PRODUCER_ID = re.compile(r"[A-Za-z0-9 ._:/@+()-]{1,120}")


def diagnostic(layer, message, *, where="", line=None, severity="error") -> dict:
    return {"layer": layer, "severity": severity, "where": where, "line": line, "message": message}


def _file_sha256(path):
    with open(path, "rb") as fh:
        return sha256_hex(fh.read())


# ---------------------------------------------------------------- candidate envelopes

def make_envelope(directory: str, *, producer_kind: str, producer_id: str, intent: str) -> dict:
    """Build the envelope for the manifest.json and daemon.py in `directory`, targeting the
    contract this skeleton implements. Raises ValueError on a bad producer or intent."""
    if producer_kind not in PRODUCER_KINDS:
        raise ValueError(f"producer kind must be one of {', '.join(PRODUCER_KINDS)}")
    if not _PRODUCER_ID.fullmatch(producer_id or ""):
        raise ValueError("producer id must be 1-120 plain characters")
    if not mf.PURPOSE_RE.fullmatch(intent or ""):
        raise ValueError("intent must be one line of 10-200 plain characters (as a manifest purpose)")
    return {
        "schema": CANDIDATE_SCHEMA,
        "required_contract": contract_identity(),
        "subject": [{"name": n, "sha256": _file_sha256(os.path.join(directory, n))} for n in SUBJECT_NAMES],
        "producer": {"kind": producer_kind, "id": producer_id},
        "intent": intent,
    }


def envelope_sha256(envelope: dict) -> str:
    return sha256_hex(canonical_bytes(envelope))


def check_envelope(envelope_path: str, directory: str):
    """Returns (summary | None, diagnostics). The summary is what reports quote back."""
    diags = []

    def fail(where, message, severity="error"):
        diags.append(diagnostic("envelope", message, where=where, severity=severity))

    try:
        with open(envelope_path, "rb") as fh:
            raw = fh.read(ENVELOPE_MAX_BYTES + 1)
    except OSError as e:
        fail("candidate", f"cannot read {envelope_path}: {e.strerror}")
        return None, diags
    if len(raw) > ENVELOPE_MAX_BYTES:
        fail("candidate", "larger than 16 KiB")
        return None, diags
    try:
        env = strict_loads(raw)
    except (UnicodeDecodeError, ValueError):
        fail("candidate", "not valid strict JSON")
        return None, diags

    expected = {"schema", "required_contract", "subject", "producer", "intent"}
    if not isinstance(env, dict):
        fail("candidate", "must be an object")
        return None, diags
    for key in sorted(set(env) - expected):
        # A closed schema: there is deliberately no field for results, verdicts or approval.
        fail(key, f"unknown key {key!r} (a candidate carries no claims about itself)")
    for key in sorted(expected - set(env)):
        fail(key, f"missing key {key!r}")
    if diags:
        return None, diags
    if env["schema"] != CANDIDATE_SCHEMA:
        fail("schema", f"must be {CANDIDATE_SCHEMA!r}")

    running = contract_identity()
    rc = env["required_contract"]
    match = None
    if not (isinstance(rc, dict) and set(rc) == {"contract_version", "contract_sha256"}
            and isinstance(rc["contract_version"], str) and _SEMVER.fullmatch(rc["contract_version"])
            and isinstance(rc["contract_sha256"], str) and _HEX64.fullmatch(rc["contract_sha256"])):
        fail("required_contract", "must be {contract_version: x.y.z, contract_sha256: 64 hex}")
    elif rc == running:
        match = "exact"
    elif rc["contract_version"].split(".")[0] != running["contract_version"].split(".")[0]:
        match = "incompatible"
        fail("required_contract.contract_version",
             f"targets contract {rc['contract_version']}; this skeleton implements "
             f"{running['contract_version']} (a different major version)")
    elif rc["contract_version"] == running["contract_version"]:
        match = "mismatch"
        fail("required_contract.contract_sha256",
             "same contract_version but a different contract_sha256: built against rules that "
             "are not the ones running here")
    else:
        match = "compatible"
        fail("required_contract.contract_version",
             f"targets {rc['contract_version']}; validated against {running['contract_version']} "
             "(same major version)", severity="warning")

    subject = env["subject"]
    if not (isinstance(subject, list) and [s.get("name") if isinstance(s, dict) else None
                                          for s in subject] == list(SUBJECT_NAMES)):
        fail("subject", f"must list exactly {', '.join(SUBJECT_NAMES)} in that order")
    else:
        for i, item in enumerate(subject):
            if set(item) != {"name", "sha256"} or not isinstance(item["sha256"], str) \
                    or not _HEX64.fullmatch(item["sha256"]):
                fail(f"subject[{i}]", "must be {name, sha256}")
                continue
            path = os.path.join(directory, item["name"])
            try:
                actual = _file_sha256(path)
            except OSError:
                fail(f"subject[{i}]", f"{item['name']} not found beside the envelope")
                continue
            if actual != item["sha256"]:
                fail(f"subject[{i}]", f"{item['name']} does not match the envelope's sha256 "
                                      "(the files changed after the envelope was made)")

    producer = env["producer"]
    if not (isinstance(producer, dict) and set(producer) == {"kind", "id"}
            and producer.get("kind") in PRODUCER_KINDS
            and isinstance(producer.get("id"), str) and _PRODUCER_ID.fullmatch(producer["id"])):
        fail("producer", f"must be {{kind: {'|'.join(PRODUCER_KINDS)}, id: 1-120 plain characters}}")
    if not (isinstance(env["intent"], str) and mf.PURPOSE_RE.fullmatch(env["intent"])):
        fail("intent", "must be one line of 10-200 plain characters")

    try:
        digest = envelope_sha256(env)
    except CanonicalError:
        fail("candidate", "not representable canonically")
        return None, diags
    summary = {"envelope_sha256": digest, "required_contract": rc if isinstance(rc, dict) else None,
               "contract_match": match,
               "producer": producer if isinstance(producer, dict) else None}
    return summary, diags


# ---------------------------------------------------------------- validate --json

_WHERE_MESSAGE = re.compile(r"^([A-Za-z_][\w.\[\]]*): (.*)$", re.S)
_PURITY_LINE = re.compile(r"^[^:]+:(\d+): (.*)$", re.S)
_PURITY_FILE = re.compile(r"^[^:]+: (.*)$", re.S)


def _manifest_diags(problems):
    out = []
    for problem in problems:
        m = _WHERE_MESSAGE.match(problem)
        where, message = (m.group(1), m.group(2)) if m else ("manifest", problem)
        out.append(diagnostic("manifest", message, where=where))
    return out


def _purity_diags(problems):
    out = []
    for problem in problems:
        m = _PURITY_LINE.match(problem)
        if m:
            out.append(diagnostic("purity", m.group(2), where="daemon.py", line=int(m.group(1))))
            continue
        m = _PURITY_FILE.match(problem)
        out.append(diagnostic("purity", m.group(1) if m else problem, where="daemon.py"))
    return out


def validate_report(manifest_path: str, envelope_path: str | None = None):
    """Returns (report, manifest | None). Checks the manifest, the code's purity and, when
    given, the candidate envelope. Milliseconds; no process is started."""
    diags, m, code_sha = [], None, None
    directory = os.path.dirname(os.path.abspath(manifest_path))
    try:
        m = mf.load(manifest_path)
    except mf.ManifestError as e:
        diags += _manifest_diags(e.problems)
    code_path = os.path.join(directory, "daemon.py")
    diags += _purity_diags(purity.check_file(code_path))
    if os.path.isfile(code_path):
        code_sha = _file_sha256(code_path)
    candidate = None
    if envelope_path:
        candidate, env_diags = check_envelope(envelope_path, directory)
        diags += env_diags
    errors = sum(1 for d in diags if d["severity"] == "error")
    report = {
        "schema": VALIDATE_SCHEMA,
        **contract_identity(),
        "result": "FAIL" if errors else "PASS",
        "valid": not errors,
        "error_count": errors,
        "warning_count": len(diags) - errors,
        "daemon": m.name if m else None,
        "manifest_sha256": m.sha256 if m else None,
        "daemon_code_sha256": code_sha,
        "candidate": candidate,
        "diagnostics": diags,
        "authority": AUTHORITY,
    }
    return report, m


# ---------------------------------------------------------------- precheck

def precheck_report(manifest_path: str, envelope_path: str | None = None, workdir: str | None = None) -> dict:
    """The fast lane between validate and the battery (about 2 s): validate, then a short
    confined run in a disposable workspace (Landlock applied, audit hook recording), the
    ledger's provenance, and the generated unit's lint and seccomp allowances. A precheck
    result is "OK" or "FAIL", never "PASS": it is feedback for whoever is iterating, not
    evidence for activation."""
    from . import battery, unitgen
    validation, m = validate_report(manifest_path, envelope_path)
    checks = [{"id": "PC-01", "title": "validate (manifest, purity, envelope)",
               "state": "PASS" if validation["valid"] else "FAIL",
               "evidence": f"{validation['error_count']} error(s), {validation['warning_count']} warning(s)"}]
    if validation["valid"]:
        ws = battery.Workspace(manifest_path, workdir)
        # r4.7 (HF-37): bind the workspace's copy, as the battery does (DB-01). An absolute
        # output_dir is rewritten into the workspace; binding the original manifest made the
        # confined run look for its ledger in the real output directory and count the
        # rewritten one as a write outside the output directory.
        from . import manifest as manifest_mod
        ws.bind_manifest(manifest_mod.load(ws.manifest))
        smoke = battery.Check("PC-02", "confined run (2 cycles, Landlock, audit hook recording)")
        battery.db03(ws, smoke, cycles=2)
        provenance = battery.Check("PC-03", "ledger integrity and provenance")
        battery.db04(ws, provenance)
        unit = battery.Check("PC-04", "unit lint and start-up syscalls")
        try:
            text = unitgen.generate(ws.m, root=battery.PATTERN_ROOT)
        except unitgen.UnitError as e:
            text, unit_error = "", str(e)
        else:
            unit_error = ""
        missing = unitgen.lint(text) if text else []
        analyze = battery.shutil.which("systemd-analyze", path="/usr/bin:/bin")
        blocked = battery._startup_syscalls_blocked(analyze, text) if analyze and text else None
        if unit_error:
            unit.state, unit.evidence = "FAIL", unit_error
        elif missing or blocked:
            unit.state = "FAIL"
            unit.evidence = "; ".join(filter(None, [
                f"missing directives: {', '.join(missing)}" if missing else "",
                f"seccomp blocks: {', '.join(blocked)}" if blocked else ""]))
        else:
            unit.state = "PASS" if blocked is not None else "UNKNOWN"
            unit.evidence = "all required directives present" + (
                "; start-up syscalls permitted" if blocked is not None else "; systemd-analyze not installed")
        checks += [{"id": c.id, "title": c.title, "state": c.state, "evidence": c.evidence}
                   for c in (smoke, provenance, unit)]
        workspace = ws.root
    else:
        checks += [{"id": cid, "title": title, "state": "SKIPPED", "evidence": "validate failed"}
                   for cid, title in (("PC-02", "confined run"), ("PC-03", "ledger provenance"),
                                      ("PC-04", "unit lint"))]
        workspace = None
    failed = any(c["state"] == "FAIL" for c in checks)
    return {
        "schema": PRECHECK_SCHEMA,
        **contract_identity(),
        "result": "FAIL" if failed else "OK",
        "activation_evidence": False,
        "daemon": validation["daemon"],
        "manifest_sha256": validation["manifest_sha256"],
        "daemon_code_sha256": validation["daemon_code_sha256"],
        "candidate": validation["candidate"],
        "checks": checks,
        "diagnostics": validation["diagnostics"],
        "workspace": workspace,
        "next": "spark-daemon battery --manifest <manifest.json>" if not failed else "fix the diagnostics",
        "authority": AUTHORITY,
    }
