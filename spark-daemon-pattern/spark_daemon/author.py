"""The authoring tool, `spark-daemon-author` (contract 5, E-9): scaffold and envelope.

These help a person, a script or a model write a candidate daemon. A frozen core does not
need them to judge, qualify or run one, so they left the runtime CLI (`spark-daemon`). They
produce nothing the core trusts: a scaffold is a starting point, and an envelope is a claim
the judge checks (DB-24).
"""

import argparse
import json
import os
import sys

from . import EXIT_OK, EXIT_USAGE, VERSION


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(prog="spark-daemon-author",
                                     description=f"Spark daemon pattern {VERSION}: authoring tools (draft)")
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("scaffold", help="write a starting manifest.json, daemon.py and candidate.json")
    p.add_argument("--name", required=True)
    p.add_argument("--dir", required=True)
    p.add_argument("--purpose", default=None)
    p.add_argument("--unit", choices=("system", "user"), default="system")

    p = sub.add_parser("envelope", help="write candidate.json for the manifest.json and daemon.py in a folder")
    p.add_argument("--dir", required=True)
    p.add_argument("--producer-kind", required=True, choices=("human", "script", "model"))
    p.add_argument("--producer-id", required=True)
    p.add_argument("--intent", required=True)

    args = parser.parse_args(argv)

    if args.command == "scaffold":
        from . import scaffold
        try:
            written = scaffold.scaffold(args.dir, name=args.name, purpose=args.purpose, unit=args.unit)
        except (ValueError, FileExistsError) as e:
            print(f"scaffold: {e}", file=sys.stderr)
            return EXIT_USAGE
        for path in written:
            print(path)
        return EXIT_OK

    from . import handoff
    try:
        env = handoff.make_envelope(args.dir, producer_kind=args.producer_kind,
                                    producer_id=args.producer_id, intent=args.intent)
    except (ValueError, OSError) as e:
        print(f"envelope: {e}", file=sys.stderr)
        return EXIT_USAGE
    path = os.path.join(args.dir, handoff.ENVELOPE_NAME)
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(env, fh, indent=2)
        fh.write("\n")
    print(path)
    print(f"envelope sha256 {handoff.envelope_sha256(env)}")
    return EXIT_OK
