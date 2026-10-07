"""Command line.

Authoring and handoff: describe | schema | scaffold | envelope
Evidence: precheck (static) | validate (+ a short confined run) | battery (qualifies), one report shape
Operation: unit (--report: the installable unit; --manifest: a preview) | run | verify | status
Harness (the battery and the self-tests): harness, with explicit, recorded parameters
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

    p = sub.add_parser("precheck", help="the static checks: manifest, purity, envelope, unit (milliseconds; no process is started)")
    p.add_argument("--manifest", required=True)
    p.add_argument("--envelope", default=None, help="candidate.json to check against the files and the contract")
    p.add_argument("--json", action="store_true", help="print the report (spark-daemon-report/1)")

    p = sub.add_parser("validate", help="precheck plus a short confined run and its ledger (seconds)")
    p.add_argument("--manifest", required=True)
    p.add_argument("--envelope", default=None)
    p.add_argument("--workdir", default=None)
    p.add_argument("--json", action="store_true", help="print the report (spark-daemon-report/1)")

    p = sub.add_parser("unit", help="print the installable unit projected from a qualifying battery report, "
                                    "or a PREVIEW from a manifest (never installs)")
    source = p.add_mutually_exclusive_group(required=True)
    source.add_argument("--report", help="a battery report: the unit is reproduced from it alone, if it qualifies "
                                         "and was made on this host")
    source.add_argument("--manifest", help="a PREVIEW: it carries no digests, so the runtime refuses to start it")
    p.add_argument("--out", default=None, metavar="DIR", help="with --report: write DIR/<unit name> instead of printing")
    p.add_argument("--python", default="/usr/bin/python3")
    p.add_argument("--plan", action="store_true", help="with --manifest: print the install plan instead of the unit")
    p.add_argument("--require-path", action="append", default=[],
                   help="a path that must exist for the unit to start (ConditionPathExists=); repeatable")
    p.add_argument("--part-of", default=None,
                   help="start and stop with this unit (PartOf=, After=, WantedBy=)")

    from .qualify import EXPECT_KEYS
    for name, text in (("run", "run the daemon in the foreground (systemd calls this)"),
                       ("harness", "run the daemon under the test harness, with explicit, recorded parameters "
                                   "(the battery and the self-tests; refused inside a systemd service)")):
        p = sub.add_parser(name, help=text)
        p.add_argument("--manifest", required=True)
        p.add_argument("--max-cycles", type=int, default=None)
        for key in EXPECT_KEYS:
            p.add_argument(f"--expect-{key.replace('_', '-')}", dest=f"expect_{key}", default=None,
                           help="refuse to start unless this digest matches (a qualified unit carries all three)")
    p.add_argument("--interval-ms", type=int, default=None)
    p.add_argument("--blind-limit-ms", type=int, default=None)
    p.add_argument("--cycle-budget-ms", type=int, default=None)
    p.add_argument("--output-dir", default=None, help="for a manifest whose output_dir is absolute")
    p.add_argument("--audit", choices=("enforce", "record"), default="enforce",
                   help="record: the audit hook counts but never blocks (DB-03, DB-17)")

    p = sub.add_parser("verify", help="verify a daemon's ledger read-only and print its chain head")
    p.add_argument("--manifest", required=True)
    p.add_argument("--output-dir", default=None, help=argparse.SUPPRESS)    # the battery's harness override

    p = sub.add_parser("status", help="a daemon's state from its ledger, verified independently (r4.11)")
    p.add_argument("--manifest", required=True)
    p.add_argument("--json", action="store_true")

    p = sub.add_parser("battery", help="run the conformance battery in a disposable workspace (the qualifying profile)")
    p.add_argument("--manifest", required=True)
    p.add_argument("--seed", type=int, default=20260927)
    p.add_argument("--quick", action="store_true", help="fewer crash trials; never qualifies")
    p.add_argument("--workdir", default=None)
    p.add_argument("--threshold", type=int, default=20, help="systemd-analyze exposure threshold in tenths (20 = 2.0)")
    p.add_argument("--envelope", default=None, help="candidate.json to check and quote in the report")
    p.add_argument("--json", action="store_true", help="print the report instead of the check lines")
    p.add_argument("--require-path", action="append", default=[],
                   help="unit input: a path that must exist for the unit to start; repeatable")
    p.add_argument("--part-of", default=None, help="unit input: start and stop with this unit")
    p.add_argument("--python", default="/usr/bin/python3", help="unit input: the interpreter the unit runs")

    p = sub.add_parser("probe-policy", help=argparse.SUPPRESS)
    p.add_argument("--manifest", required=True)
    p.add_argument("--canary", required=True)
    p.add_argument("--output-dir", default=None)

    p = sub.add_parser("probe-landlock", help=argparse.SUPPRESS)
    p.add_argument("--manifest", required=True)
    p.add_argument("--canary", required=True)
    p.add_argument("--output-dir", default=None)

    p = sub.add_parser("probe-digest", help=argparse.SUPPRESS)
    p.add_argument("--manifest", required=True)
    p.add_argument("--seed", type=int, required=True)
    p.add_argument("--output-dir", default=None)

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

    if args.command in ("precheck", "validate"):
        from . import judge
        if not os.path.exists(args.manifest):
            print(f"no manifest at {args.manifest}", file=sys.stderr)
            return EXIT_USAGE
        print(f"note: {judge.RENAMED[args.command]}", file=sys.stderr)        # contract 5, for one release
        options = {"envelope": args.envelope}
        if args.command == "validate":
            options["workdir"] = args.workdir
        j = judge.run(args.command, args.manifest, **options)
        return judge.print_report(judge.report(j), as_json=args.json)

    if args.command == "unit":
        if args.report:
            return _unit_from_report(args)
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
                print(unitgen.install_plan(m, root=root, python=args.python,
                                           require_paths=args.require_path, part_of=args.part_of))
            else:
                print(unitgen.generate(m, root=root, python=args.python,
                                       require_paths=args.require_path, part_of=args.part_of), end="")
        except unitgen.UnitError as e:
            print(f"unit: {e}", file=sys.stderr)
            return EXIT_USAGE
        return EXIT_OK

    if args.command in ("run", "harness"):
        from . import runtime
        from .qualify import EXPECT_KEYS
        given = {key: getattr(args, f"expect_{key}") for key in EXPECT_KEYS}
        if any(given.values()) and not all(given.values()):
            print("the three --expect-*-sha256 options go together", file=sys.stderr)
            return EXIT_USAGE
        harness = None
        if args.command == "harness":
            try:
                harness = runtime.Harness(interval_ms=args.interval_ms, blind_limit_ms=args.blind_limit_ms,
                                          cycle_budget_ms=args.cycle_budget_ms, output_dir=args.output_dir,
                                          audit=args.audit)
            except ValueError as e:
                print(f"harness: {e}", file=sys.stderr)
                return EXIT_USAGE
        return runtime.run(args.manifest, max_cycles=args.max_cycles,
                           expect=given if all(given.values()) else None, harness=harness)

    if args.command == "status":
        from . import EXIT_FLAGGED, status
        from . import manifest as manifest_mod
        try:
            report = status.status(args.manifest)
        except manifest_mod.ManifestError as e:
            for problem in e.problems:
                print(f"manifest: {problem}", file=sys.stderr)
            return EXIT_USAGE
        if args.json:
            print(json.dumps(report, indent=2, ensure_ascii=False))
        else:
            print(f"{report['daemon']}: {report['state']}" + (f" ({report['reason']})" if report.get("reason") else ""))
            print(f"integrity: {report['integrity']}")
            if report["integrity"] == "CHAIN_INTACT":
                print(f"blind for {report['blind_ms'] / 1000:.1f} s of {report['blind_limit_seconds']} s "
                      f"(measured on the {report['clock_basis']} clock)")
                for gap in report["gaps"]:
                    print(f"not watching from {gap['from']} to {gap['to']}"
                          + ("" if gap["previous_run_ended_cleanly"] else " (after an unclean end)"))
        if report["integrity"] == "CORRUPT":
            return EXIT_FAILED
        return EXIT_OK if report["state"] == "observing" else EXIT_FLAGGED

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
        path = os.path.join(expand(args.output_dir or m.output_dir), LEDGER_NAME)
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
                            threshold=args.threshold, envelope=args.envelope, as_json=args.json,
                            unit_options={"python": args.python, "require_paths": args.require_path,
                                          "part_of": args.part_of})

    if args.command == "probe-policy":
        from . import probes
        return probes.probe_policy(args.manifest, args.canary, args.output_dir)

    if args.command == "probe-landlock":
        from . import probes
        return probes.probe_landlock(args.manifest, args.canary, args.output_dir)

    if args.command == "probe-digest":
        from . import probes
        return probes.probe_digest(args.manifest, args.seed, args.output_dir)
    return EXIT_USAGE


def _unit_from_report(args) -> int:
    """The installer's gate and the projection (contract 5, E-11): the report must qualify and
    have been made on this host; the unit is then reproduced from the report alone. Whether
    the files still match is the runtime's check, at every start."""
    from . import judge, qualify
    from .canonical import strict_loads
    if args.plan or args.require_path or args.part_of or args.python != "/usr/bin/python3":
        print("unit --report takes its inputs from the report; set them on the battery instead", file=sys.stderr)
        return EXIT_USAGE
    try:
        with open(args.report, "rb") as fh:
            rep = strict_loads(fh.read(4 * 1024 * 1024))
    except (OSError, ValueError) as e:
        print(f"not installable: no readable report at {args.report} ({e.__class__.__name__})", file=sys.stderr)
        return EXIT_FAILED
    problems = []
    if isinstance(rep, dict):
        problems += judge.conclusions(rep)["why_not"] + qualify.host_differences(rep.get("host"))
    else:
        problems.append("the report is not an object")
    if not problems:
        try:
            name, text = judge.unit_from_report(rep)
        except ValueError as e:
            problems.append(str(e))
    if problems:
        for problem in problems:
            print(f"not installable: {problem}", file=sys.stderr)
        return EXIT_FAILED
    if args.out:
        os.makedirs(args.out, exist_ok=True)
        path = os.path.join(args.out, name)
        with open(path, "w") as fh:
            fh.write(text)
        print(path)
    else:
        print(text, end="")
    print("note: a qualified unit is evidence for an activation decision; installing and enabling it are "
          "a person's decision", file=sys.stderr)
    return EXIT_OK
