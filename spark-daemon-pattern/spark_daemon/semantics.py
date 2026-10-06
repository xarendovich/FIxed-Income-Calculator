"""What a verified ledger means: one pure interpretation, shared by start-up and `status`
(r4.11, R-4).

Start-up (runtime.py) and `status` both ask how long a daemon has been blind, on which clock,
and whether anyone is watching. Two interpretations of one rule drift, so both call the
functions here. They are pure: every clock reading comes in as a `Now`, so the same records and
the same `Now` always give the same answer.

Verification and interpretation are kept apart on purpose. Bytes and hashes are verified by two
independent implementations (spark_daemon/ledger.py, and verifier/ledger_verify.py, which does
not import this package); interpretation is single. Only records that a verifier accepted reach
these functions: `interpret` takes a `Verified`, which can only be built from an intact chain.
"""

import datetime
from dataclasses import dataclass

from . import RESERVED_EVENT_TYPES

STAMPED = ("DAEMON_START", "DAEMON_HEARTBEAT", "DAEMON_STOP")
# The stops a daemon chooses: a DAEMON_ERROR of one of these categories is followed by exit 78.
FATAL_CATEGORIES = ("SENSE_BLIND", "POLICY_VIOLATION")


@dataclass(frozen=True)
class Now:
    """Clock readings, taken once by the caller: boot_id and CLOCK_BOOTTIME, plus the wall clock."""
    boot_id: str | None
    boottime_ms: int
    wall: datetime.datetime


def _is_int(value) -> bool:
    return type(value) is int          # not bool: True == 1 in Python (HF-38)


class AcceptEvidence:
    """The latest evidence in a ledger that a cycle was accepted (r4.0, PD-70). Fed records in
    chain order, never by comparing timestamps, so a wall-clock step cannot reorder it (U-4)."""

    def __init__(self):
        self.records = 0
        self.last_accepted_utc = None
        self.first_start_utc = None
        self.anchor = None          # (boot_id, blind_since_boottime_ms); r4.5, HF-34
        self._stamp = None          # (boot_id, boottime_ms) of the latest boot-stamped record

    def observe(self, record):
        """r4.5 (HF-34): also tracks the boot-time anchor of that evidence. A start, heartbeat or
        stop states blind_since_boottime_ms exactly. A daemon event states only that a cycle was
        accepted after the latest boot-stamped record, so that record's boottime_ms stands in:
        it can over-state blindness by at most one heartbeat interval, in the safe direction."""
        self.records += 1
        event_type, payload = record["event_type"], record["payload"]
        if event_type in STAMPED and isinstance(payload, dict):
            boot, now_ms, since_ms = (payload.get("boot_id"), payload.get("boottime_ms"),
                                      payload.get("blind_since_boottime_ms"))
            if isinstance(boot, str) and _is_int(now_ms) and _is_int(since_ms):
                self._stamp, self.anchor = (boot, now_ms), (boot, since_ms)
            else:
                self._stamp = self.anchor = None       # an older record: no boot-time evidence
        if event_type == "DAEMON_START":
            if self.first_start_utc is None:
                self.first_start_utc = record["timestamp_utc"]
        elif event_type in ("DAEMON_HEARTBEAT", "DAEMON_STOP"):
            value = payload.get("last_accepted_utc") if isinstance(payload, dict) else None
            if isinstance(value, str):
                self.last_accepted_utc = value
        elif event_type not in RESERVED_EVENT_TYPES:
            self.last_accepted_utc = record["timestamp_utc"]     # events come only from accepted cycles
            self.anchor = self._stamp                             # accepted no earlier than that stamp


def wall_seconds_since(utc_text, now: Now):
    """Signed wall-clock seconds since a ledger timestamp (negative if it lies in the future), or
    None if it cannot be read. Used only where no shared clock exists (across a reboot)."""
    try:
        then = datetime.datetime.strptime(utc_text, "%Y-%m-%dT%H:%M:%S.%fZ").replace(tzinfo=datetime.timezone.utc)
    except (TypeError, ValueError):
        return None
    return (now.wall - then).total_seconds()


def blindness(evidence: AcceptEvidence, now: Now, limit: float):
    """Seconds of blindness, and the clock that measured them (r4.5, HF-34): the one rule.

    "fresh" for an empty ledger; "boottime" within one boot (no wall-clock step moves it); across
    a reboot "wall" if the wall clock moved forward, otherwise "worst_case" (blind at the limit,
    which leaves exactly one reacquisition cycle)."""
    since_utc = evidence.last_accepted_utc or evidence.first_start_utc
    if evidence.records == 0 or since_utc is None:
        return 0.0, "fresh"
    if now.boot_id is not None and evidence.anchor is not None and evidence.anchor[0] == now.boot_id:
        return max(0.0, (now.boottime_ms - evidence.anchor[1]) / 1000), "boottime"
    wall = wall_seconds_since(since_utc, now)
    if wall is None or wall < 0:
        return float(limit), "worst_case"      # the wall clock went back: elapsed time is unknown
    return wall, "wall"


def _age(record, now: Now):
    """Seconds since a record was written, preferring the shared boot clock."""
    payload = record.get("payload")
    if isinstance(payload, dict) and payload.get("boot_id") == now.boot_id and _is_int(payload.get("boottime_ms")):
        return max(0.0, (now.boottime_ms - payload["boottime_ms"]) / 1000), "boottime"
    wall = wall_seconds_since(record.get("timestamp_utc"), now)
    if wall is None or wall < 0:
        return None, "unknown"
    return wall, "wall"


class Verified:
    """Records a verifier accepted, in chain order. Built only from an intact chain."""

    def __init__(self, records, facts):
        self.records = list(records)
        self.facts = facts

    @classmethod
    def from_facts(cls, facts):
        """From the independent verifier's facts (verifier/ledger_verify.py, keep_records=True)."""
        if not facts.get("intact") or facts.get("missing") or "record_list" not in facts:
            raise ValueError("only an intact, verified chain can be interpreted")
        if len(facts["record_list"]) != facts["records"]:
            raise ValueError("the verified records do not match the verifier's count")
        return cls(facts["record_list"], facts)


def interpret(verified: Verified, now: Now, limit: float) -> dict:
    """The daemon's state from its verified ledger. States:

      never_started   no record at all
      stopped         the last record is a clean DAEMON_STOP, or a fatal DAEMON_ERROR (reason)
      not_watching    no start or heartbeat within the blind limit: nobody vouches for the daemon
      blind           alive, but in a blind stretch: the last heartbeat or start says so, or at
                      least half the limit has passed without an accepted cycle (PD-01.3)
      observing       alive and seeing

    Every gap between one run's last record and the next DAEMON_START is reported in `gaps`:
    time not watched, never quiet time (INV-6)."""
    if not isinstance(verified, Verified):
        raise TypeError("interpret() takes Verified records only")
    records = verified.records
    out = {"state": "never_started", "reason": None, "blind_ms": 0, "clock_basis": "fresh",
           "last_accepted_utc": None, "last_record_utc": None, "gaps": []}
    if not records:
        return out
    evidence = AcceptEvidence()
    previous = None
    for record in records:
        if record["event_type"] == "DAEMON_START" and previous is not None:
            clean = previous["event_type"] == "DAEMON_STOP"
            out["gaps"].append({"from": previous["timestamp_utc"], "to": record["timestamp_utc"],
                                "previous_run_ended_cleanly": clean})
        evidence.observe(record)
        previous = record
    blind, basis = blindness(evidence, now, limit)
    out.update(blind_ms=int(blind * 1000), clock_basis=basis, last_accepted_utc=evidence.last_accepted_utc,
               last_record_utc=records[-1]["timestamp_utc"])
    last = records[-1]
    payload = last.get("payload") if isinstance(last.get("payload"), dict) else {}
    if last["event_type"] == "DAEMON_STOP":
        out.update(state="stopped", reason=f"clean: {payload.get('reason', 'unknown')}")
        return out
    if last["event_type"] == "DAEMON_ERROR" and payload.get("category") in FATAL_CATEGORIES:
        out.update(state="stopped", reason=payload["category"])
        return out
    vouching = [r for r in records if r["event_type"] in ("DAEMON_START", "DAEMON_HEARTBEAT")]
    age, _ = _age(vouching[-1], now) if vouching else (None, "unknown")
    if age is None or age >= limit:
        out.update(state="not_watching",
                   reason="no start or heartbeat within the blind limit (crashed, killed, or never restarted)")
        return out
    latest = vouching[-1]["payload"] if isinstance(vouching[-1].get("payload"), dict) else {}
    if vouching[-1]["event_type"] == "DAEMON_HEARTBEAT":
        alive_blind = latest.get("mode") == "blind"
    else:
        inherited = latest.get("inherited_blind_ms")
        alive_blind = _is_int(inherited) and inherited > 0
    # Half the limit without an accepted cycle is PD-01.3's degraded point: report it as blind.
    out["state"] = "blind" if alive_blind or blind >= limit / 2 else "observing"
    return out
