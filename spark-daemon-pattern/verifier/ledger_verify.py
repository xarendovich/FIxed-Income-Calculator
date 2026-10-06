#!/usr/bin/env python3
"""An independent verifier for daemon ledgers (spark-daemon-ledger/1). Standard library only.

It does not import the daemon package (spark_daemon) and shares no code with it: the skeleton's
own verifier (spark_daemon/ledger.py) re-encodes each record with json.dumps; this one encodes
RFC 8785 (JCS) by hand, character by character. A defect in either shows up as a disagreement
(battery check DB-22). Consumers in other projects may copy this one file.

It reports verification facts and never a conclusion about the daemon: whether the chain is
intact up to which record, where the first break is and why, the head, and the torn tail (bytes
after the last newline, which a crash leaves and the next start quarantines). What a record
*means* (watching, blind, stopped) is decided elsewhere, and only for an intact chain.

    python3 -I -B ledger_verify.py LEDGER --daemon NAME [--max-bytes N] [--records]

Break categories match the skeleton's: RECORD_TOO_LARGE, UNPARSEABLE, ENVELOPE,
UNSUPPORTED_SCHEMA, NOT_CANONICAL, SEQUENCE, LINK, DAEMON_MISMATCH.
"""

import hashlib
import json
import sys

SCHEMA = "spark-daemon-ledger/1"
SCHEMA_FAMILY = "spark-daemon-ledger/"
GENESIS = "0" * 64
ENVELOPE = ("daemon", "event_id", "event_type", "payload", "prev_sha256", "run_id", "schema", "seq",
            "timestamp_utc")
SAFE = 2 ** 53 - 1
DEPTH = 64
VERIFIER = "spark-daemon-independent-verifier/1"


class _NotCanonical(Exception):
    pass


# ---------------------------------------------------------------- RFC 8785, by hand

_SHORT = {0x08: "\\b", 0x09: "\\t", 0x0A: "\\n", 0x0C: "\\f", 0x0D: "\\r", 0x22: '\\"', 0x5C: "\\\\"}


def _string(s: str) -> str:
    out = ['"']
    for ch in s:
        code = ord(ch)
        if code in _SHORT:
            out.append(_SHORT[code])
        elif code < 0x20:
            out.append("\\u%04x" % code)
        elif 0xD800 <= code <= 0xDFFF:
            raise _NotCanonical("lone surrogate")
        else:
            out.append(ch)
    out.append('"')
    return "".join(out)


def _utf16_key(s: str) -> list:
    units = []
    for ch in s:
        code = ord(ch)
        if code > 0xFFFF:
            code -= 0x10000
            units += [0xD800 + (code >> 10), 0xDC00 + (code & 0x3FF)]
        else:
            units.append(code)
    return units


def _encode(value, depth=0) -> str:
    if depth > DEPTH:
        raise _NotCanonical("nesting too deep")
    if value is None:
        return "null"
    if value is True:
        return "true"
    if value is False:
        return "false"
    if type(value) is int:
        if not -SAFE <= value <= SAFE:
            raise _NotCanonical("integer out of the safe range")
        return str(value)
    if type(value) is str:
        return _string(value)
    if type(value) is list:
        return "[" + ",".join(_encode(v, depth + 1) for v in value) + "]"
    if type(value) is dict:
        keys = sorted(value, key=_utf16_key)
        return "{" + ",".join(_string(k) + ":" + _encode(value[k], depth + 1) for k in keys) + "}"
    raise _NotCanonical(f"type {type(value).__name__}")


def _parse(body: bytes):
    def pairs(items):
        out = {}
        for key, item in items:
            if key in out:
                raise ValueError("duplicate key")
            out[key] = item
        return out

    def refuse(_text):
        raise ValueError("float or constant")

    return json.loads(body.decode("utf-8"), object_pairs_hook=pairs, parse_float=refuse,
                      parse_constant=refuse)


# ---------------------------------------------------------------- the chain

def verify(path: str, daemon: str, max_bytes: int = 1 << 20, keep_records: bool = False) -> dict:
    """Verification facts for one ledger file. Read-only."""
    facts = {"verifier": VERIFIER, "path": path, "daemon": daemon, "intact": True, "records": 0,
             "head_seq": 0, "head_sha256": GENESIS, "break": None, "torn_tail": None}
    kept = []
    offset, seq, head = 0, 0, GENESIS

    def broke(category):
        facts["intact"] = False
        facts["break"] = {"seq": seq + 1, "offset": offset, "category": category}

    try:
        fh = open(path, "rb")
    except FileNotFoundError:
        facts["missing"] = True
        return facts
    with fh:
        while True:
            line = fh.readline(max_bytes + 2)
            if not line:
                break
            if not line.endswith(b"\n"):
                if fh.read(1):
                    broke("RECORD_TOO_LARGE")
                else:
                    facts["torn_tail"] = {"offset": offset, "length": len(line)}
                break
            body = line[:-1]
            if len(body) > max_bytes:
                broke("RECORD_TOO_LARGE")
                break
            try:
                record = _parse(body)
            except (UnicodeDecodeError, ValueError):
                broke("UNPARSEABLE")
                break
            if type(record) is not dict:
                broke("ENVELOPE")
                break
            schema = record.get("schema")
            if schema != SCHEMA:
                broke("UNSUPPORTED_SCHEMA" if type(schema) is str and schema.startswith(SCHEMA_FAMILY)
                      else "ENVELOPE")
                break
            if tuple(sorted(record)) != ENVELOPE:
                broke("ENVELOPE")
                break
            try:
                canonical = _encode(record).encode("utf-8") == body
            except _NotCanonical:
                canonical = False
            if not canonical:
                broke("NOT_CANONICAL")
                break
            if type(record["seq"]) is not int or record["seq"] != seq + 1:      # True == 1 in Python
                broke("SEQUENCE")
                break
            if record["prev_sha256"] != head:
                broke("LINK")
                break
            if record["daemon"] != daemon:
                broke("DAEMON_MISMATCH")
                break
            seq, head = record["seq"], hashlib.sha256(body).hexdigest()
            if keep_records:
                kept.append(record)
            offset += len(line)
    facts.update(records=seq, head_seq=seq, head_sha256=head)
    if keep_records:
        facts["record_list"] = kept
    return facts


def main(argv=None) -> int:
    import argparse
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("ledger")
    p.add_argument("--daemon", required=True)
    p.add_argument("--max-bytes", type=int, default=1 << 20)
    p.add_argument("--records", action="store_true", help="include the verified records")
    args = p.parse_args(argv)
    facts = verify(args.ledger, args.daemon, args.max_bytes, keep_records=args.records)
    print(json.dumps(facts, indent=2, ensure_ascii=False))
    return 0 if facts["intact"] and not facts.get("missing") else 1


if __name__ == "__main__":
    sys.exit(main())
