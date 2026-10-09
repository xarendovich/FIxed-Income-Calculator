"""The daemon contract: everything a daemon's author (a person, a script or a model) must
satisfy, as one machine-readable document, generated from the constants the skeleton
actually enforces. Replaces "go and read README.md, context.py, purity.py and manifest.py".

The contract is published outward (spark-daemon describe; contract/daemon-contract.json);
a candidate daemon comes back inward in a candidate envelope (envelope.py) that names the
contract it targeted. The shape mirrors the Step 9 HandoffEnvelope: exact identity
(contract_version plus a deterministic contract_sha256) and a required contract, and the
same rule - observation is not activation: nothing a candidate says about itself is
evidence. Only precheck, validate and the battery produce evidence (one report shape,
spark-daemon-report/1), and activation remains a separate owner activation decision;
nothing in this package installs or enables a unit.

contract_version follows semantic versioning for daemon authors: MAJOR when a daemon that
satisfied the old contract can fail the new one (a new purity rule, a narrower bound), MINOR
when the contract only grows (a new ctx method, a new allowed import), PATCH for wording.
contract_sha256 is the canonical hash of the "contract" object below; it changes whenever
any enforced value changes, so a stale README can never be mistaken for the running rules.
"""

import inspect
import re

from . import (DIGEST_NAME, EXIT_ALREADY_RUNNING, EXIT_LEDGER_CORRUPT, EXIT_OK,
               EXIT_POLICY, EXIT_UNCERTAIN_COMMIT, EXIT_USAGE, KNOWN_OUTPUT_ENTRIES, LEDGER_NAME,
               LEDGER_SCHEMA, MANIFEST_SCHEMA, RESERVED_EVENT_TYPES, VERSION)
from . import manifest as mf
from . import purity, render, unitgen
from .canonical import MAX_SAFE_INT, canonical_bytes, sha256_hex

CONTRACT_SCHEMA = "spark-daemon-contract/1"
CONTRACT_VERSION = "5.0.1"
CANDIDATE_SCHEMA = "spark-daemon-candidate/1"
MANIFEST_SCHEMA_ID = "urn:spark:schema:spark-daemon-manifest:4"

AUTHORITY = (
    "A candidate carries no authority. Whoever or whatever produced it - a person, a script "
    "or a model - it cannot certify itself: precheck and validate are fast feedback, the "
    "conformance battery's PASS is the only admissible evidence, and activation remains a "
    "separate owner activation decision. A producer is never granted authority that no one adjudicated."
)

# Rules manifest.py enforces that JSON Schema cannot express. Listed so an offline checker
# knows exactly what it is not checking.
CROSS_FIELD_RULES = (
    "cycle_budget_seconds must be at most half of blind_limit_seconds",
    f"blind_limit_seconds must be at least {mf.BLIND_LIMIT_INTERVALS} x trigger.interval_seconds",
    "run_as.user is required for unit 'system', must not be 'root', and is refused for unit 'user'",
    "paths are compared after expanding '~' (SPARK_DAEMON_HOME, else HOME) and resolving symlinks",
    "every read path must lie outside every base-deny path, and must not contain one (no gaps: the "
    "kernel's grant is the whole policy)",
    "output_dir must not lie inside, or contain, ~/spark-core, ~/spark-governance or a base-deny path",
    "output_dir must not overlap any read path (a daemon never observes its own output)",
)


def _anchored(pattern: str) -> str:
    """A Python fullmatch pattern as a JSON Schema pattern. JSON Schema patterns search, and
    "$" also matches before a final newline in Python's re (which Python validators use);
    "(?![\\s\\S])" means end of string in both ECMA-262 and Python."""
    body = pattern[1:] if pattern.startswith("^") else pattern
    body = body[:-1] if body.endswith("$") else body
    return "^" + body + "(?![\\s\\S])"


def _path_schema():
    return {
        "type": "string",
        "pattern": _anchored(mf.PATH_RE.pattern),
        "not": {"anyOf": [{"pattern": "//"}, {"pattern": ".+/$"},
                          {"pattern": "(^|/)\\.\\.?(/|$)"}]},
        "description": "Absolute or ~/ path of letters, digits and ._/- ; no '.' or '..' "
                       "segments, no '//', no trailing '/'.",
    }


def manifest_json_schema() -> dict:
    """The manifest as a JSON Schema (draft 2020-12). manifest.py remains the enforcer; this
    is the same closed schema in a form any language can check offline. Patterns are written
    to mean the same in Python's re and ECMA-262 (the validator uses fullmatch)."""
    int_range = lambda low, high, desc: {"type": "integer", "minimum": low, "maximum": high,  # noqa: E731
                                         "description": desc}
    return {
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "$id": MANIFEST_SCHEMA_ID,
        "title": f"Spark daemon manifest ({MANIFEST_SCHEMA})",
        "description": "Closed schema: unknown keys are refused at every level, every value is "
                       "bounded, floats are refused. Cross-field rules that JSON Schema cannot "
                       "express are listed under x-spark-cross-field-rules.",
        "type": "object",
        "additionalProperties": False,
        "required": sorted(mf.TOP_KEYS),
        "properties": {
            "manifest_schema": {"const": MANIFEST_SCHEMA},
            "name": {"type": "string", "pattern": _anchored(mf.NAME_RE.pattern),
                     "description": "Slug: 3-40 characters, lowercase letters, digits and '-'."},
            "version": {"type": "string", "pattern": _anchored(mf.VERSION_RE.pattern.replace("\\d", "[0-9]")),
                        "description": "x.y.z"},
            "purpose": {"type": "string", "pattern": _anchored(mf.PURPOSE_RE.pattern),
                        "description": "One plain line; no '%' (a systemd specifier)."},
            "daemon_class": {"enum": ["observe"],
                             "description": "'act' is reserved: acting daemons need their own pattern "
                                            "and authority gate (PD-01)."},
            "trigger": {
                "type": "object", "additionalProperties": False,
                "required": ["kind", "interval_seconds"],
                "properties": {
                    "kind": {"enum": ["poll"], "description": "'inotify-wakeup' is reserved (C2)."},
                    "interval_seconds": int_range(5, 86400, "Poll interval."),
                },
            },
            "reads": {"type": "array", "minItems": 1, "maxItems": 32, "uniqueItems": True,
                      "items": {"$ref": "#/$defs/path"},
                      "description": "What ctx may read, and all it may read: none inside, and none "
                                     "containing, a base-deny path."},
            "output_dir": {"$ref": "#/$defs/path"},
            "network": {"type": "object", "additionalProperties": False, "required": ["mode"],
                        "properties": {"mode": {"enum": ["none"],
                                                "description": "'named' is reserved (relaxation R2)."}}},
            "run_as": {
                "oneOf": [
                    {"type": "object", "additionalProperties": False, "required": ["unit", "user"],
                     "properties": {"unit": {"const": "system"},
                                    "user": {"type": "string", "pattern": _anchored(mf.USER_RE.pattern),
                                             "not": {"const": "root"}}}},
                    {"type": "object", "additionalProperties": False, "required": ["unit"],
                     "properties": {"unit": {"const": "user"}}},
                ],
            },
            "resources": {
                "type": "object", "additionalProperties": False,
                "required": ["cpu_weight", "cpu_budget_bp", "memory_max_mb", "tasks_max", "io_class"],
                "properties": {
                    "cpu_weight": int_range(1, 100, "systemd CPUWeight="),
                    "cpu_budget_bp": int_range(1, 10000, "CPU budget in basis points (100 = 1%), "
                                                         "checked by DB-14 including child processes."),
                    "memory_max_mb": int_range(64, 2048, "systemd MemoryMax=; floor 64 (PD-20)."),
                    "tasks_max": int_range(4, 64, "systemd TasksMax="),
                    "io_class": {"enum": ["idle", "best-effort"]},
                },
            },
            "cycle_budget_seconds": int_range(mf.CYCLE_BUDGET_MIN, mf.CYCLE_BUDGET_MAX,
                                              "The longest one whole cycle (sense, decide, digest) may take. "
                                              "One runtime alarm enforces the whole-cycle deadline; at the budget "
                                              "the cycle fails. WatchdogSec = 2 x budget + "
                                              f"{mf.WATCHDOG_MARGIN_SECONDS}, TimeoutStopSec = WatchdogSec + 10, "
                                              "TimeoutStartSec = max(60, WatchdogSec); at most half of "
                                              "blind_limit_seconds."),
            "blind_limit_seconds": int_range(mf.BLIND_LIMIT_MIN, mf.BLIND_LIMIT_MAX,
                                             "Longest time without an accepted cycle before the daemon "
                                             "exits SENSE_BLIND (78, never restarted). Set it above the "
                                             "longest legitimate unsettled period on the host, plus a margin; "
                                             f"at least {mf.BLIND_LIMIT_INTERVALS} x trigger.interval_seconds."),
            "ledger": {
                "type": "object", "additionalProperties": False,
                "required": ["record_max_bytes", "event_types"],
                "properties": {
                    "record_max_bytes": int_range(1024, 1048576, "Largest canonical record, bytes."),
                    "event_types": {
                        "type": "array", "minItems": 1, "maxItems": 32, "uniqueItems": True,
                        "items": {"type": "string", "pattern": _anchored(mf.EVENT_RE.pattern),
                                  "not": {"enum": list(RESERVED_EVENT_TYPES)}},
                        "description": "The only event types decide() may return.",
                    },
                },
            },
            "digest": {
                "type": "object", "additionalProperties": False, "required": ["enabled", "max_bytes"],
                "properties": {"enabled": {"type": "boolean"},
                               "max_bytes": int_range(1024, 262144, "Digest size bound, bytes.")},
            },
        },
        "$defs": {"path": _path_schema()},
        "x-spark-cross-field-rules": list(CROSS_FIELD_RULES),
    }


def _first_line(obj) -> str:
    doc = inspect.getdoc(obj) or ""
    return doc.split("\n\n")[0].replace("\n", " ").strip()


def _type_name(annotation):
    """A version-stable spelling of an annotation: bare class names, no module paths."""
    if annotation is inspect.Parameter.empty:
        return None
    if isinstance(annotation, type):
        return annotation.__name__
    return re.sub(r"\b(?:[A-Za-z_]\w*\.)+([A-Za-z_]\w*)", r"\1", str(annotation))


def _signature(name, fn) -> dict:
    params = []
    for p in list(inspect.signature(fn).parameters.values())[1:]:     # drop self
        item = {"name": p.name, "type": _type_name(p.annotation)}
        if p.default is not inspect.Parameter.empty:
            item["default"] = p.default
        params.append(item)
    shown = ", ".join(p["name"] + (f"={p['default']!r}" if "default" in p else "") for p in params)
    ret = _type_name(inspect.signature(fn).return_annotation)
    return {"name": name, "call": f"ctx.{name}({shown})", "params": params, "returns": ret,
            "summary": _first_line(fn)}


def ctx_capabilities() -> dict:
    """The ctx object's public surface, read from the class the runtime actually uses."""
    from . import context
    methods = [_signature(name, fn)
               for name, fn in inspect.getmembers(context.Context, predicate=inspect.isfunction)
               if not name.startswith("_")]
    results = []
    for cls in (context.StatInfo, context.DiskUsage, context.Listing):
        fields = [{"name": k, "type": _type_name(v)} for k, v in cls.__annotations__.items()]
        results.append({"name": cls.__name__, "fields": fields})
    exceptions = [{"name": f"ctx.{cls.__name__}", "summary": _first_line(cls)}
                  for cls in (context.Missing, context.TooLarge, context.NotAllowed)]
    return {"methods": methods, "results": results, "exceptions": exceptions,
            "writes": "none: ctx has no method that writes, deletes, sends or executes anything",
            "programs": "none: a daemon runs no program (contract 5, R-2); the kernel grants execute nowhere"}


def _evidence() -> dict:
    """The one judge's profiles (judge.py, r4.11 R-7), read from its registry: the check list
    cannot drift from the checks that run (the 3.x text still listed DB-01 to DB-18)."""
    from . import judge
    reg = judge.registry()
    checks = {i: reg[i].title for i in sorted(reg)}
    return {
        "checks": checks,
        "states": list(judge.STATES),
        "verdict": "FAIL if any check FAILs, INCOMPLETE if any is UNKNOWN, PASS otherwise; exit 0, 3 or 1",
        "report": {"schema": judge.REPORT_SCHEMA,
                   "holds": "facts only: inputs, host, and each check's state and evidence",
                   "derived_on_read": "the verdict and whether the report qualifies (judge.conclusions); "
                                      "never stored"},
        "profiles": {
            "precheck": {"cost": "milliseconds", "checks": judge.profile_ids("precheck")},
            "validate": {"cost": "seconds", "checks": judge.profile_ids("validate")},
            "battery": {"cost": "minutes", "checks": judge.profile_ids("battery")},
        },
        "qualifies": "a report from the full battery (not --quick) that holds every battery check, "
                     "none FAIL or UNKNOWN, and binds a complete unit",
        "installable_unit": "a projection of a qualifying report (spark-daemon unit --report): the report "
                            "binds every input of the unit generator. Installing checks that the report "
                            "qualifies and was made on this host; the runtime checks at every start that "
                            "the manifest, daemon.py and contract are those the unit names",
        "admissible_for_activation": "a qualifying battery PASS is evidence; activation remains a separate "
                                     "owner activation decision",
    }


# The six invariants (contract 5, E-8; ADJUDICATION-V5.md). Each is enforced in one place and
# names the battery checks and self-tests that hold it; tests/test_invariants.py checks that
# every one of them exists. They absorb the owner's eight and r4.9's INV-1 to INV-9; the owner's
# eighth ("nothing relaxes confinement, blindness, authentication, or proposal-versus-authority")
# governs all six and is the preamble.
INVARIANTS_PREAMBLE = ("No change may relax any of these. Each is enforced in one place; the checks and "
                       "tests named with it fail if it breaks.")
INVARIANTS = (
    {"id": "I-1", "invariant": "Observe only, and the kernel enforces it: no network, no program, writes only "
                               "in output_dir, reads only the declared paths, under one path policy for every layer",
     "absorbs": ["owner 2", "owner 3", "INV-1", "INV-2"],
     "enforced_in": "Landlock and the systemd unit, both projected from pathpolicy.PathPolicy; purity and the "
                    "audit hook are diagnostics and a tripwire",
     "checks": ["DB-15", "DB-17", "DB-18"],
     "tests": ["test_pathpolicy.AgreementTests.test_the_three_projections_agree_on_every_shipped_manifest",
               "test_landlock.EnforcementTests.test_nothing_the_daemon_can_write_or_merely_reads_can_be_executed"]},
    {"id": "I-2", "invariant": "Never record a guess: only an accepted cycle produces events; an unsettled, "
                               "truncated or failed read abandons the cycle",
     "absorbs": ["INV-3"],
     "enforced_in": "the runtime's cycle (runtime.py), with ctx raising on truncation",
     "checks": ["DB-04"],
     "tests": ["test_blind.BlindPeriodTests.test_unsettled_is_neither_an_event_nor_an_error",
               "test_blind.DirWatchCapacityTests.test_adding_a_file_over_the_cap_never_reports_a_removal"]},
    {"id": "I-3", "invariant": "Blindness surfaces within blind_limit_seconds, across restarts and clock steps, "
                               "and is never restarted away",
     "absorbs": ["INV-4"],
     "enforced_in": "semantics.blindness, the one function the runtime and status use",
     "checks": ["DB-20"],
     "tests": ["test_blind.BlindPeriodTests.test_always_unsettled_stops_at_the_limit",
               "test_blind.BlindPeriodTests.test_blindness_survives_a_restart_loop",
               "test_blind.BlindPeriodTests.test_a_backward_clock_step_does_not_hide_blindness"]},
    {"id": "I-4", "invariant": "The ledger is the canonical local record: append-only and hash-chained, verified two "
                               "independent ways relative to the observed head, interpreted once; integrity "
                               "uncertainty and every gap are visible, never rendered healthy; completeness "
                               "across observations requires a head anchor outside the daemon output directory",
     "absorbs": ["owner 4", "owner 5", "INV-5", "INV-6"],
     "enforced_in": "ledger.py writes; ledger.py and verifier/ledger_verify.py verify; semantics.interpret interprets",
     "checks": ["DB-04", "DB-06", "DB-07", "DB-16", "DB-22"],
     "tests": ["test_independent.AgreementTests.test_both_verifiers_agree_on_a_real_ledger_and_every_damaged_copy",
               "test_hardening.CliRobustnessTests.test_verify_only_prints_what_verify_printed"]},
    {"id": "I-5", "invariant": "It runs only what was judged, and judging is not activation: the installable unit "
                               "is a projection of a qualifying report made on this host, the runtime refuses "
                               "files that differ from the unit's digests, and nothing here installs or enables "
                               "a unit",
     "absorbs": ["owner 1", "owner 7", "INV-7"],
     "enforced_in": "judge.conclusions and `unit --report` when installing; the runtime's digest gate at every start",
     "checks": ["DB-24"],
     "tests": ["test_qualify.RuntimeGateTests.test_changed_code_is_refused_everywhere",
               "test_qualify.ProjectedUnitTests.test_a_report_that_does_not_qualify_is_refused",
               "test_qualify.ActivationStaysSeparateTests.test_no_judge_or_qualification_code_installs_or_enables_anything"]},
    {"id": "I-6", "invariant": "Bounded: one budget for each whole cycle, every unit timing derived from it, and "
                               "restart decided by the exit class",
     "absorbs": ["owner 6", "INV-8", "INV-9"],
     "enforced_in": "the cycle's alarm (runtime.py); timings derived from cycle_budget_seconds (manifest.py, "
                    "unitgen.py); RestartPreventExitStatus from NO_RESTART_EXIT_CODES",
     "checks": ["DB-14", "DB-25"],
     "tests": ["test_budget.CycleDeadlineTests.test_catching_the_alarm_does_not_save_the_cycle",
               "test_blind.NoRestartTests.test_fail_closed_exits_are_never_restarted"]},
)


def contract_body() -> dict:
    return {
        "manifest": {
            "schema_id": MANIFEST_SCHEMA_ID,
            "manifest_schema": MANIFEST_SCHEMA,
            "json_schema_sha256": sha256_hex(canonical_bytes(manifest_json_schema())),
            "max_bytes": mf.MANIFEST_MAX_BYTES,
            "base_deny": list(mf.BASE_DENY),
            "forbidden_output_roots": list(mf.FORBIDDEN_OUTPUT_ROOTS),
            "reserved": {"daemon_class": ["act"], "trigger.kind": ["inotify-wakeup"],
                         "network.mode": ["named"]},
            "cross_field_rules": list(CROSS_FIELD_RULES),
        },
        "code": {
            "file": "daemon.py",
            "max_bytes": 262144,
            "required_functions": [{"name": k, "positional_args": v, "signature": s} for k, v, s in (
                ("sense", 1, "sense(ctx) -> snapshot (plain data: None, bool, int, str, list, dict), "
                             "or ctx.unsettled(reason) when what it read was not stable"),
                ("decide", 2, "decide(prev, snapshot) -> [(event_type, payload_dict), ...]"))],
            "optional_functions": [{"name": "digest", "positional_args": 2,
                                    "signature": "digest(snapshot, recent) -> [(title, [(label, value), ...]), ...]"}],
            "io_rule": "sense() is the only place with I/O, and only through ctx; decide() and digest() are pure",
            "allowed_imports": sorted(purity.ALLOWED_IMPORTS),
            "forbidden_builtins": sorted(purity.FORBIDDEN_CALLS),
            "forbidden_names": sorted(purity.FORBIDDEN_NAMES),
            "forbidden_attributes": sorted(purity.FORBIDDEN_ATTRIBUTES),
            "import_time_calls_allowed": sorted(purity.TOPLEVEL_CALLS),
            "other_rules": [
                "no dunder names or attributes; no private attributes (names starting with '_')",
                "no format string that reaches a dunder field",
                "no global or nonlocal; state flows only through prev and snapshot",
                "no async code; no star or relative imports",
                "top level: imports, def, class, constant assignments and a docstring only",
                "sense, decide and digest are each defined once with def, undecorated, never reassigned",
            ],
        },
        "values": {
            "types": ["null", "bool", "int", "str", "list", "dict (str keys)"],
            "floats": "refused everywhere; use integers with a stated unit (permille, basis points, kB)",
            "int_range": [-MAX_SAFE_INT, MAX_SAFE_INT],
            "canonical_form": "RFC 8785 (JCS) for every accepted value; UTF-8; no lone surrogates",
        },
        "ctx": ctx_capabilities(),
        "cycle": {
            "accepted": "sense() returned a snapshot, and decide()'s events were validated and committed "
                        "(zero events is still accepted); start-up counts as accepted",
            "unsettled": "sense() returned ctx.unsettled(reason): the cycle is abandoned before decide(), "
                         "with no event, no DAEMON_ERROR and no new digest",
            "failed": "sense() or decide() raised, or returned invalid data: DAEMON_ERROR (repeats collapsed)",
            "budget": "one monotonic deadline per cycle, cycle_budget_seconds from its start: one runtime alarm "
                      "interrupts sense() and decide() at the deadline, including pure-Python loops; the cycle "
                      "then fails with DAEMON_ERROR category "
                      "CYCLE_BUDGET_EXCEEDED. The ledger write is never interrupted. The digest gets what is "
                      "left; past it the previous digest stays",
            "blind_limit": "no accepted cycle for blind_limit_seconds (monotonic clock), whether from "
                           "unsettled or failed cycles: DAEMON_ERROR category SENSE_BLIND with blind_ms, "
                           "limit_ms, unsettled_cycles, failed_cycles, last_cause and last_cause_kind; then exit 78",
            "watchdog": "pinged from the main loop throughout, including unsettled and failed cycles within "
                        "the limit; the limit, not the watchdog, catches a daemon that sees nothing",
            "across_restarts": "the blind clock survives restarts (PD-70): at start-up it begins at the latest "
                               "evidence of an accepted cycle in the verified ledger (a daemon event, or the "
                               "last_accepted_utc of a heartbeat or clean stop). Within one boot it is measured on "
                               "CLOCK_BOOTTIME (boot_id, boottime_ms and blind_since_boottime_ms in "
                               "DAEMON_START, DAEMON_HEARTBEAT and DAEMON_STOP), so no wall-clock step moves "
                               "it; across a reboot on the wall clock, and if that went back, blind at the "
                               "limit is assumed; DAEMON_START and SENSE_BLIND record the clock_basis; a "
                               "fresh ledger starts at zero. A restart that inherits more than the limit gets one "
                               "reacquisition cycle, then SENSE_BLIND without a fresh countdown. Neither an "
                               "operator restart nor a clean stop resets it; only an accepted cycle does",
            "heartbeat": "DAEMON_HEARTBEAT every blind_limit_seconds / 2: mode (observing or blind), "
                         "last_accepted_utc (null before the first accepted cycle), blind_ms, and the accepted, "
                         "unsettled and failed cycle counts since the previous heartbeat, and the boot stamp "
                         "(boot_id, boottime_ms, blind_since_boottime_ms)",
            "blind_forester": {
                "observe": "fail closed: SENSE_BLIND, exit 78, never restarted (the only class in this contract)",
                "active_classes": "a pre-validated survival loop (PD-01.9) is not available in contract 5; it "
                                  "requires an Act-family class, the supervisor/worker split (PD-72) and "
                                  "pre-authorized survival actions",
            },
        },
        "notifications": {
            "transport": "systemd sd_notify lifecycle/status only",
            "messages": ["READY=1", "STATUS=...", "WATCHDOG=1", "STOPPING=1"],
            "semantics": "host lifecycle and operator status only; carries no observation payload, grant, "
                         "decision or outcome",
            "status_sequence": "STATUS may display the current ledger sequence for operator context; ledger "
                               "sequence is an ordering position, never a clock or authority input",
        },
        "digest": {
            "label_pattern": render.LABEL_RE.pattern,
            "row_value_types": ["str", "int", "bool", "null"],
            "containment": "every value is rendered inside a fenced code block with visible escapes",
        },
        "ledger": {
            "schema": LEDGER_SCHEMA,
            "reserved_event_types": list(RESERVED_EVENT_TYPES),
            "event_type_pattern": mf.EVENT_RE.pattern,
            "output_dir_entries": sorted(KNOWN_OUTPUT_ENTRIES),
            "ledger_file": LEDGER_NAME,
            "digest_file": DIGEST_NAME,
        },
        "exit_codes": {
            str(EXIT_OK): "stopped cleanly",
            str(EXIT_USAGE): "bad manifest, impure code, or daemon code failed to import",
            str(EXIT_LEDGER_CORRUPT): "corrupt ledger; nothing changed",
            str(EXIT_UNCERTAIN_COMMIT): "uncertain ledger commit; recovery decides on restart",
            str(EXIT_ALREADY_RUNNING): "another instance holds the lock",
            str(EXIT_POLICY): "unsafe output directory, Landlock refused, a policy violation, SENSE_BLIND "
                              "(no accepted cycle within blind_limit_seconds), or a start that is not "
                              "qualified (no battery PASS digests, or files changed since): fail closed, "
                              "needs a human. The reason is the ledger's DAEMON_ERROR category or the log",
            "never_restarted": list(unitgen.NO_RESTART_EXIT_CODES),
        },
        "invariants": {"preamble": INVARIANTS_PREAMBLE, "list": [dict(i) for i in INVARIANTS]},
        "evidence": _evidence(),
        "candidate": {"schema": CANDIDATE_SCHEMA, "file": "candidate.json"},
        "authority": AUTHORITY,
    }


def describe() -> dict:
    body = contract_body()
    return {
        "schema": CONTRACT_SCHEMA,
        "contract_version": CONTRACT_VERSION,
        "contract_sha256": sha256_hex(canonical_bytes(body)),
        "skeleton_version": VERSION,
        "contract": body,
    }


def contract_identity() -> dict:
    """What a candidate must name: {contract_version, contract_sha256}."""
    doc = describe()
    return {"contract_version": doc["contract_version"], "contract_sha256": doc["contract_sha256"]}
