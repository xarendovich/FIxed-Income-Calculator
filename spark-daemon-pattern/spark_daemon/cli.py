"""Command line.

Authoring and handoff: describe | schema | scaffold | envelope | validate | precheck
Evidence and operation: battery | unit | run | verify
Internal (the battery's child processes): probe-policy | probe-landlock | probe-digest
"""

import argparse
import json
import os
import sys

from . import EXIT_FAILED, EXIT_OK, EXIT_USAGE, VERSION


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(prog="spark-daemon", description=f"Spark daemon pattern {VERSION} (draft)")
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("describe", help="print the daemon contract (spark-daemon-contract/1) as JSON")
    p.add_argument("--identity", action="store_true", help="print only contract_version and contract_sha256")

    sub.add_parser("schema", help="print the manifest JSON Schema (draft 2020-12)")

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

    p = sub.add_parser("validate", help="validate a manifest, the purity of its daemon.py and a candidate envelope")
    p.add_argument("--manifest", required=True)
    p.add_argument("--envelope", default=None, help="candidate.json to check against the files and the contract")
    p.add_argument("--json", action="store_true", help="print a spark-daemon-validate/1 report")

    p = sub.add_parser("precheck", help="fast pre-battery lane: validate plus a short confined run (JSON report)")
    p.add_argument("--manifest", required=True)
    p.add_argument("--envelope", default=None)
    p.add_argument("--workdir", default=None)

    p = sub.add_parser("unit", help="print the generated systemd unit and install plan (never installs)")
    p.add_argument("--manifest", required=True)
    p.add_argument("--python", default="/usr/bin/python3")
    p.add_argument("--plan", action="store_true", help="print the install plan instead of the unit")

    p = sub.add_parser("run", help="run the daemon in the foreground (systemd calls this)")
    p.add_argument("--manifest", required=True)
    p.add_argument("--max-cycles", type=int, default=None)

    p = sub.add_parser("verify", help="verify a daemon's ledger read-only and print its chain head")
    p.add_argument("--manifest", required=True)

    p = sub.add_parser("battery", help="run the conformance battery in a disposable workspace")
    p.add_argument("--manifest", required=True)
    p.add_argument("--seed", type=int, default=20260927)
    p.add_argument("--quick", action="store_true")
    p.add_argument("--workdir", default=None)
    p.add_argument("--threshold", type=int, default=20, help="systemd-analyze exposure threshold in tenths (20 = 2.0)")
    p.add_argument("--envelope", default=None, help="candidate.json to quote in the report")

    p = sub.add_parser("probe-policy", help=argparse.SUPPRESS)
    p.add_argument("--manifest", required=True)
    p.add_argument("--canary", required=True)

    p = sub.add_parser("probe-landlock", help=argparse.SUPPRESS)
    p.add_argument("--manifest", required=True)
    p.add_argument("--canary", required=True)

    p = sub.add_parser("probe-digest", help=argparse.SUPPRESS)
    p.add_argument("--manifest", required=True)
    p.add_argument("--seed", type=int, required=True)

    args = parser.parse_args(argv)

    if args.command == "describe":
        from . import contract
        doc = contract.contract_identity() if args.identity else contract.describe()
        print(json.dumps(doc, indent=2, ensure_ascii=False))
        return EXIT_OK

    if args.command == "schema":
        from . import contract
        print(json.dumps(contract.manifest_json_schema(), indent=2, ensure_ascii=False))
        return EXIT_OK

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

    if args.command == "envelope":
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

    if args.command == "validate":
        from . import handoff
        report, _ = handoff.validate_report(args.manifest, args.envelope)
        if args.json:
            print(json.dumps(report, indent=2, ensure_ascii=False))
        else:
            for d in report["diagnostics"]:
                where = d["where"] + (f":{d['line']}" if d["line"] else "")
                prefix = "" if d["severity"] == "error" else "warning: "
                print(f"{d['layer']}: {prefix}{where}: {d['message']}" if where else f"{d['layer']}: {d['message']}")
            if report["manifest_sha256"]:
                print(f"manifest sha256 {report['manifest_sha256']}")
            if report["candidate"]:
                print(f"candidate envelope sha256 {report['candidate']['envelope_sha256']} "
                      f"(contract {report['candidate']['contract_match']})")
            print(f"RESULT: {report['result']}")
        return EXIT_OK if report["valid"] else EXIT_FAILED

    if args.command == "precheck":
        from . import handoff
        if not os.path.exists(args.manifest):
            print(f"no manifest at {args.manifest}", file=sys.stderr)
            return EXIT_USAGE
        report = handoff.precheck_report(args.manifest, args.envelope, args.workdir)
        print(json.dumps(report, indent=2, ensure_ascii=False))
        return EXIT_OK if report["result"] == "OK" else EXIT_FAILED

    if args.command == "unit":
        from . import manifest, unitgen
        try:
            m = manifest.load(args.manifest)
        except manifest.ManifestError as e:
            for problem in e.problems:
                print(f"manifest: {problem}", file=sys.stderr)
            return EXIT_USAGE
        root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        try:
            if args.plan:
                print(unitgen.install_plan(m, root=root, python=args.python))
            else:
                print(unitgen.generate(m, root=root, python=args.python), end="")
        except unitgen.UnitError as e:
            print(f"unit: {e}", file=sys.stderr)
            return EXIT_USAGE
        return EXIT_OK

    if args.command == "run":
        from . import runtime
        return runtime.run(args.manifest, max_cycles=args.max_cycles)

    if args.command == "verify":
        from . import LEDGER_NAME, ledger, manifest
        from .paths import expand
        try:
            m = manifest.load(args.manifest)
        except manifest.ManifestError as e:
            for problem in e.problems:
                print(f"manifest: {problem}")
            print("RESULT: FAIL")
            return EXIT_FAILED
        path = os.path.join(expand(m.output_dir), LEDGER_NAME)
        try:
            result = ledger.verify_file(path, m.name, m.ledger.record_max_bytes)
        except FileNotFoundError:
            print("no ledger yet")
            print("RESULT: PASS")
            return EXIT_OK
        except ledger.LedgerCorrupt as e:
            print(str(e))
            print("RESULT: FAIL")
            return EXIT_FAILED
        print(json.dumps({"seq": result.tail.seq, "chain_head_sha256": result.tail.head,
                          "torn_tail_bytes": result.torn.length if result.torn else 0}))
        print("RESULT: PASS")
        return EXIT_OK

    if args.command == "battery":
        from . import battery
        return battery.main(args.manifest, seed=args.seed, quick=args.quick, workdir=args.workdir,
                            threshold=args.threshold, envelope=args.envelope)

    if args.command == "probe-policy":
        from . import probes
        return probes.probe_policy(args.manifest, args.canary)

    if args.command == "probe-landlock":
        from . import probes
        return probes.probe_landlock(args.manifest, args.canary)

    if args.command == "probe-digest":
        from . import probes
        return probes.probe_digest(args.manifest, args.seed)
    return EXIT_USAGE
