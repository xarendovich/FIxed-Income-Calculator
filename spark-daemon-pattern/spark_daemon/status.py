"""`spark-daemon status`: a daemon's state, read from outside (r4.11, R-4).

The ledger is verified by the independent verifier (verifier/ledger_verify.py, which does not
import this package) and only an intact chain is interpreted, by the same function start-up
uses (semantics.py). A broken chain is reported as such and never interpreted: a syntactically
valid record behind a bad hash is not evidence of anything.

Consumers (notifiers, timers, other projects) should read a daemon through this command or the
verifier, not by parsing the ledger themselves (N-19).

cross_check() runs both verifiers over one file and lists where they disagree (DB-22).

Since contract 5 (E-9) `verify` is `status --verify-only`: verification without interpretation,
by the same independent verifier, cross-checked against the primary one. A reader therefore
always meets the same verification.
"""

import datetime
import importlib.util
import os

from . import LEDGER_NAME, ledger
from . import manifest as manifest_mod
from . import semantics
from .paths import expand

STATUS_SCHEMA = "spark-daemon-status/1"
VERIFIER_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "verifier",
                             "ledger_verify.py")
_VERIFIER = None


def load_verifier():
    """The independent verifier, loaded from its file: it is not part of this package."""
    global _VERIFIER
    if _VERIFIER is None:
        spec = importlib.util.spec_from_file_location("spark_daemon_independent_verifier", VERIFIER_PATH)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        _VERIFIER = module
    return _VERIFIER


def now() -> semantics.Now:
    from .runtime import _boot_id, _boottime_ms
    return semantics.Now(_boot_id(), _boottime_ms(), datetime.datetime.now(datetime.timezone.utc))


def primary_outcome(path, daemon, max_bytes) -> dict:
    """The skeleton's own verifier, as comparable facts."""
    try:
        scan = ledger.verify_file(path, daemon, max_bytes)
    except FileNotFoundError:
        return {"missing": True}
    except ledger.LedgerCorrupt as e:
        return {"intact": False, "break": {"seq": e.seq, "category": e.category}}
    return {"intact": True, "head_seq": scan.tail.seq, "head_sha256": scan.tail.head,
            "torn_tail": scan.torn.length if scan.torn else None}


def independent_outcome(path, daemon, max_bytes) -> dict:
    facts = load_verifier().verify(path, daemon, max_bytes)
    if facts.get("missing"):
        return {"missing": True}
    if not facts["intact"]:
        return {"intact": False, "break": {"seq": facts["break"]["seq"], "category": facts["break"]["category"]}}
    return {"intact": True, "head_seq": facts["head_seq"], "head_sha256": facts["head_sha256"],
            "torn_tail": facts["torn_tail"]["length"] if facts["torn_tail"] else None}


def cross_check(path, daemon, max_bytes) -> list:
    """Where the two verifiers disagree about one file (empty: they agree)."""
    a, b = primary_outcome(path, daemon, max_bytes), independent_outcome(path, daemon, max_bytes)
    return [f"{key}: skeleton {a.get(key)!r}, independent {b.get(key)!r}"
            for key in sorted(set(a) | set(b)) if a.get(key) != b.get(key)]


def verify_only(manifest_path, output_dir=None) -> dict:
    """The ledger's integrity and chain head, interpreted not at all. integrity is NO_LEDGER,
    CHAIN_INTACT, CORRUPT, or VERIFIERS_DISAGREE (a defect in one of them: never healthy)."""
    m = manifest_mod.load(manifest_path)
    path = os.path.join(expand(output_dir or m.output_dir), LEDGER_NAME)
    facts = load_verifier().verify(path, m.name, m.ledger.record_max_bytes)
    report = {"schema": STATUS_SCHEMA, "daemon": m.name, "ledger": path, "verification": facts}
    disagreement = [] if facts.get("missing") else cross_check(path, m.name, m.ledger.record_max_bytes)
    if facts.get("missing"):
        report["integrity"] = "NO_LEDGER"
    elif disagreement:
        report.update(integrity="VERIFIERS_DISAGREE", disagreement=disagreement)
    elif not facts["intact"]:
        report.update(integrity="CORRUPT",
                      reason=f"{facts['break']['category']} at seq {facts['break']['seq']}")
    else:
        report["integrity"] = "CHAIN_INTACT"
    return report


def status(manifest_path, at: semantics.Now | None = None, output_dir=None) -> dict:
    m = manifest_mod.load(manifest_path)
    path = os.path.join(expand(output_dir or m.output_dir), LEDGER_NAME)
    facts = load_verifier().verify(path, m.name, m.ledger.record_max_bytes, keep_records=True)
    report = {"schema": STATUS_SCHEMA, "daemon": m.name, "ledger": path,
              "blind_limit_seconds": m.blind_limit_seconds,
              "verification": {k: v for k, v in facts.items() if k != "record_list"}}
    if facts.get("missing"):
        report.update(integrity="NO_LEDGER", **semantics.interpret(semantics.Verified([], facts), at or now(),
                                                                    m.blind_limit_seconds))
        return report
    if not facts["intact"]:
        report.update(integrity="CORRUPT", state="ledger_corrupt",
                      reason=f"{facts['break']['category']} at seq {facts['break']['seq']}: not interpreted")
        return report
    report["integrity"] = "CHAIN_INTACT"
    report.update(semantics.interpret(semantics.Verified.from_facts(facts), at or now(), m.blind_limit_seconds))
    return report
