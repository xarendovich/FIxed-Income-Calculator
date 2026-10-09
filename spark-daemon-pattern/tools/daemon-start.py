#!/usr/bin/env python3
"""Print the last DAEMON_START record of a ledger, reading it the way the ledger is designed to
be read. Standard library only; imports nothing from the pattern. One file, copyable.

This is the evidence extractor for the hardware gate (HARDWARE-GATE-DGX.md, step 3.7). It
encodes two ledger facts a host runbook must not reinvent:

  - the torn tail: everything after the last newline was never committed (a crash mid-write);
    the next start quarantines it. It is reported, never parsed, never an error;
  - a line that cannot be parsed, or that parses but is not a record, is reported with its
    position and exits 2, never a traceback
    (HF-43: a deeply nested line used to crash both verifiers).

It verifies nothing (no chain, no canonical check, no schema string), so it also reads the ledger
of a vendored copy that renamed the schema. Verification is a separate step with the host's
own verifier (`verify` at 3.1.0; `status --verify-only` from contract 5), which must leave the
file's bytes and mtime unchanged.

    python3 -I -B daemon-start.py LEDGER            the last DAEMON_START, as sorted JSON
    python3 -I -B daemon-start.py LEDGER --all      every DAEMON_START, oldest first

Exit 0: printed. Exit 1: no DAEMON_START. Exit 2: an unparseable committed line, or no file.
"""

import json
import sys


def read(path):
    """(starts, torn) where starts is every DAEMON_START in order and torn is None or
    {"offset", "length"} for an unterminated last line. Raises ValueError(seq, offset, why) on
    a committed line that does not parse, or that parses but is not a record (a JSON value that
    is not an object): on some CPython releases a deeply nested line parses, on others it does
    not, and neither outcome may pass silently."""
    starts, offset, seq = [], 0, 0
    with open(path, "rb") as fh:
        for line in fh:
            if not line.endswith(b"\n"):
                return starts, {"offset": offset, "length": len(line)}
            seq += 1
            try:
                record = json.loads(line.decode("utf-8"))
            except (UnicodeDecodeError, ValueError, RecursionError):
                raise ValueError(seq, offset, "does not parse") from None
            if not isinstance(record, dict):
                raise ValueError(seq, offset, "parses but is not a record")
            if record.get("event_type") == "DAEMON_START":
                starts.append(record)
            offset += len(line)
    return starts, None


def main(argv=None) -> int:
    argv = sys.argv[1:] if argv is None else list(argv)
    every = "--all" in argv
    paths = [a for a in argv if a != "--all"]
    if len(paths) != 1:
        print(__doc__.strip().splitlines()[0], file=sys.stderr)
        print("usage: daemon-start.py LEDGER [--all]", file=sys.stderr)
        return 2
    try:
        starts, torn = read(paths[0])
    except OSError as e:
        print(f"daemon-start: cannot read {paths[0]} ({e.__class__.__name__})", file=sys.stderr)
        return 2
    except ValueError as e:
        seq, offset, why = e.args
        print(f"daemon-start: line {seq} at byte {offset} is a committed line that {why}; "
              "verify the ledger before using it as evidence", file=sys.stderr)
        return 2
    if torn:
        print(f"note: torn tail of {torn['length']} bytes at byte {torn['offset']} (never committed; "
              "the next start quarantines it)", file=sys.stderr)
    if not starts:
        print("daemon-start: no DAEMON_START record found", file=sys.stderr)
        return 1
    for record in (starts if every else starts[-1:]):
        print(json.dumps(record, indent=2, sort_keys=True, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    sys.exit(main())
