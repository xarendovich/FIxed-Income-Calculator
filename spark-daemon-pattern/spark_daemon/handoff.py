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
digest the way a ProcessingObservation quotes the HandoffEnvelope it observed. Since r4.11
both are profiles of the one judge (judge.py); the envelope is its check DB-24.

Reports use one diagnostic shape everywhere (modelled on `terraform validate -json`):
    {"layer", "severity": "error"|"warning", "where", "line", "message"}
"""

import os
import re

from . import judge
from . import manifest as mf
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


diagnostic = judge.diagnostic


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


# ---------------------------------------------------------------- validate and precheck
#
# r4.11 (R-7): both are profiles of the one judge (judge.py). These functions only shape its
# result into their published report formats; they decide nothing themselves.

def _identity(j):
    return {"daemon": j.m.name if j.m else None,
            "manifest_sha256": j.m.sha256 if j.m else None,
            "daemon_code_sha256": j.code_sha,
            "candidate": j.candidate}


def _checks(j):
    return [{"id": c.id, "title": c.title, "state": c.state, "evidence": c.evidence} for c in j.checks]


def validate_report(manifest_path: str, envelope_path: str | None = None):
    """Returns (report, manifest | None): the validate profile (DB-01, DB-02, DB-24, DB-25).
    Milliseconds; no process is started."""
    j = judge.run("validate", manifest_path, envelope=envelope_path)
    diags = j.diagnostics()
    errors = sum(1 for d in diags if d["severity"] == "error")
    report = {
        "schema": VALIDATE_SCHEMA,
        **contract_identity(),
        "profile": j.profile,
        "qualifying": j.qualifying,
        "result": j.result,
        "valid": j.result == "PASS",
        "error_count": errors,
        "warning_count": len(diags) - errors,
        **_identity(j),
        "checks": _checks(j),
        "diagnostics": diags,
        "authority": AUTHORITY,
    }
    return report, j.m


def precheck_report(manifest_path: str, envelope_path: str | None = None, workdir: str | None = None) -> dict:
    """The precheck profile: validate's checks, then a short confined run in a disposable
    workspace (DB-03 with two cycles) and the ledger's provenance (DB-04). Seconds; offline; no
    tool beyond Python is needed. Its result is feedback for whoever is iterating, never
    qualification: `qualifying` is false, and only the battery may emit an installable unit."""
    j = judge.run("precheck", manifest_path, envelope=envelope_path, workdir=workdir)
    return {
        "schema": PRECHECK_SCHEMA,
        **contract_identity(),
        "profile": j.profile,
        "qualifying": j.qualifying,
        "result": j.result,
        "activation_evidence": False,
        **_identity(j),
        "checks": _checks(j),
        "diagnostics": j.diagnostics(),
        "workspace": j.ws.root if j.ws else None,
        "next": ("spark-daemon battery --manifest <manifest.json>" if j.result != "FAIL"
                 else "fix the diagnostics"),
        "authority": AUTHORITY,
    }
