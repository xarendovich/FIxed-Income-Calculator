"""Command line: validate | unit | run | verify | battery | probe-policy | probe-landlock |
probe-digest."""

import argparse
import json
import os
import sys

from . import EXIT_FAILED, EXIT_OK, EXIT_USAGE, VERSION


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(prog="spark-daemon", description=f"Spark daemon pattern {VERSION} (draft)")
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("validate", help="validate a manifest and the purity of its daemon.py")
    p.add_argument("--manifest", required=True)

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

    if args.command == "validate":
        from . import manifest, purity
        try:
            m = manifest.load(args.manifest)
        except manifest.ManifestError as e:
            for problem in e.problems:
                print(f"manifest: {problem}")
            print("RESULT: FAIL")
            return EXIT_FAILED
        problems = purity.check_file(m.code_path)
        for problem in problems:
            print(f"purity: {problem}")
        print(f"manifest sha256 {m.sha256}")
        print("RESULT: FAIL" if problems else "RESULT: PASS")
        return EXIT_FAILED if problems else EXIT_OK

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
                            threshold=args.threshold)

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
