"""The daemon contract: everything a daemon's author (a person, a script or a model) must
satisfy, as one machine-readable document, generated from the constants the skeleton
actually enforces. Replaces "go and read README.md, context.py, purity.py and manifest.py".

The contract is published outward (spark-daemon describe; contract/daemon-contract.json);
a candidate daemon comes back inward in a candidate envelope (envelope.py) that names the
contract it targeted. The shape mirrors the Step 9 HandoffEnvelope: exact identity
(contract_version plus a deterministic contract_sha256) and a required contract, and the
same rule - observation is not activation: nothing a candidate says about itself is
evidence. Only validate, precheck and the battery produce evidence, and only the battery's
PASS plus a human Class C ruling activates anything.

contract_version follows semantic versioning for daemon authors: MAJOR when a daemon that
satisfied the old contract can fail the new one (a new purity rule, a narrower bound), MINOR
when the contract only grows (a new ctx method, a new allowed import), PATCH for wording.
contract_sha256 is the canonical hash of the "contract" object below; it changes whenever
any enforced value changes, so a stale README can never be mistaken for the running rules.
"""

import inspect
import re

from . import (BATTERY_SCHEMA, DIGEST_NAME, EXIT_ALREADY_RUNNING, EXIT_LEDGER_CORRUPT, EXIT_OK,
               EXIT_POLICY, EXIT_UNCERTAIN_COMMIT, EXIT_USAGE, KNOWN_OUTPUT_ENTRIES, LEDGER_NAME,
               LEDGER_SCHEMA, MANIFEST_SCHEMA, RESERVED_EVENT_TYPES, VERSION)
from . import manifest as mf
from . import proc, purity, render, unitgen
from .canonical import MAX_SAFE_INT, canonical_bytes, sha256_hex

CONTRACT_SCHEMA = "spark-daemon-contract/1"
CONTRACT_VERSION = "3.0.0"
CANDIDATE_SCHEMA = "spark-daemon-candidate/1"
VALIDATE_SCHEMA = "spark-daemon-validate/1"
PRECHECK_SCHEMA = "spark-daemon-precheck/1"
MANIFEST_SCHEMA_ID = "urn:spark:schema:spark-daemon-manifest:2"

AUTHORITY = (
    "A candidate carries no authority. Whoever or whatever produced it - a person, a script "
    "or a model - it cannot certify itself: validate and precheck are fast feedback, the "
    "conformance battery's PASS is the only admissible evidence, and activation remains a "
    "human Class C decision. A producer is never granted authority that no one adjudicated."
)

# Rules manifest.py enforces that JSON Schema cannot express. Listed so an offline checker
# knows exactly what it is not checking.
CROSS_FIELD_RULES = (
    "step_timeout_seconds must be at most half of watchdog_seconds",
    f"blind_limit_seconds must be at least {mf.BLIND_LIMIT_INTERVALS} x trigger.interval_seconds",
    "run_as.user is required for unit 'system', must not be 'root', and is refused for unit 'user'",
    "paths are compared after expanding '~' (SPARK_DAEMON_HOME, else HOME) and resolving symlinks",
    "every read path must lie outside every denied path (base deny plus the manifest's deny)",
    "output_dir must not lie inside, or contain, ~/spark-core, ~/spark-governance, a base-deny path "
    "or a path in the manifest's deny",
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
    forbidden = sorted(mf.FORBIDDEN_COMMANDS)
    prefixes = "|".join(mf.FORBIDDEN_COMMAND_PREFIXES)
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
                      "description": "What ctx may read; none inside a denied path."},
            "commands": {
                "type": "array", "maxItems": 8, "uniqueItems": True,
                "items": {"type": "string", "pattern": _anchored(mf.COMMAND_RE.pattern),
                          "not": {"anyOf": [{"enum": forbidden}, {"pattern": f"^({prefixes})"}]}},
                "description": "Bare command names ctx.run may execute ('git' only via ctx.git).",
            },
            "output_dir": {"$ref": "#/$defs/path"},
            "deny": {"type": "array", "maxItems": 16, "uniqueItems": True,
                     "items": {"$ref": "#/$defs/path"},
                     "description": "Extra denied paths, added to the fixed base list."},
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
            "watchdog_seconds": int_range(10, 3600, "systemd WatchdogSec="),
            "step_timeout_seconds": int_range(1, 600, "Timeout for each ctx.run/ctx.git; at most "
                                                      "half of watchdog_seconds."),
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
    for cls in (context.StatInfo, context.DiskUsage, context.Listing, proc.RunResult):
        fields = [{"name": k, "type": _type_name(v)} for k, v in cls.__annotations__.items()]
        results.append({"name": cls.__name__, "fields": fields})
    exceptions = [{"name": f"ctx.{cls.__name__}", "summary": _first_line(cls)}
                  for cls in (context.Missing, context.TooLarge, context.NotAllowed)]
    return {"methods": methods, "results": results, "exceptions": exceptions,
            "writes": "none: ctx has no method that writes, deletes, sends or executes anything "
                      "outside the manifest's commands"}


def contract_body() -> dict:
    return {
        "manifest": {
            "schema_id": MANIFEST_SCHEMA_ID,
            "manifest_schema": MANIFEST_SCHEMA,
            "json_schema_sha256": sha256_hex(canonical_bytes(manifest_json_schema())),
            "max_bytes": mf.MANIFEST_MAX_BYTES,
            "base_deny": list(mf.BASE_DENY),
            "forbidden_output_roots": list(mf.FORBIDDEN_OUTPUT_ROOTS),
            "forbidden_commands": sorted(mf.FORBIDDEN_COMMANDS),
            "forbidden_command_prefixes": list(mf.FORBIDDEN_COMMAND_PREFIXES),
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
            "blind_limit": "no accepted cycle for blind_limit_seconds (monotonic clock), whether from "
                           "unsettled or failed cycles: DAEMON_ERROR category SENSE_BLIND with blind_ms, "
                           "limit_ms, unsettled_cycles, failed_cycles, last_cause and last_cause_kind; then exit 78",
            "watchdog": "pinged from the main loop throughout, including unsettled and failed cycles within "
                        "the limit; the limit, not the watchdog, catches a daemon that sees nothing",
            "across_restarts": "the blind clock survives restarts (PD-70): at start-up it begins at the latest "
                               "evidence of an accepted cycle in the verified ledger (a daemon event, or the "
                               "last_accepted_utc of a heartbeat or clean stop), measured on the wall clock; a "
                               "fresh ledger starts at zero. A restart that inherits more than the limit gets one "
                               "reacquisition cycle, then SENSE_BLIND without a fresh countdown. Neither an "
                               "operator restart nor a clean stop resets it; only an accepted cycle does",
            "heartbeat": "DAEMON_HEARTBEAT every blind_limit_seconds / 2: mode (observing or blind), "
                         "last_accepted_utc (null before the first accepted cycle), blind_ms, and the accepted, "
                         "unsettled and failed cycle counts since the previous heartbeat",
            "blind_forester": {
                "observe": "fail closed: SENSE_BLIND, exit 78, never restarted (the only class in this contract)",
                "active_classes": "a pre-validated survival loop (PD-01.9) is not available in contract 3.x; it "
                                  "requires an Act-family class, the supervisor/worker split (PD-72) and "
                                  "pre-authorized survival actions",
            },
        },
        "git": {
            "via": "ctx.git(repo, args, max_bytes=65536, index_copy=False); ctx.run(['git', ...]) is refused",
            "subcommands": sorted(proc.GIT_SUBCOMMANDS),
            "diff_subcommands_forced_flags": {"subcommands": sorted(proc.GIT_DIFF_SUBCOMMANDS),
                                              "flags": ["--no-ext-diff", "--no-textconv"]},
            "refused_long_options": list(proc.GIT_REFUSED_OPTIONS),
            "refused_short_options": {k: list(v) for k, v in sorted(proc.GIT_REFUSED_SHORT.items())},
            "other_rules": ["the subcommand is always the first argument",
                            "no argument may be absolute, start with '~' or contain a '..' segment",
                            "no %G signature placeholders in --format/--pretty",
                            "safe.directory is set to exactly the declared repository"],
            "global_options_always_set": list(proc.GIT_BASE),
            "environment_always_set": dict(sorted(proc.GIT_ENV.items())),
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
            str(EXIT_POLICY): "unsafe output directory, Landlock refused, a policy violation, or SENSE_BLIND "
                              "(no accepted cycle within blind_limit_seconds): fail closed, needs a human",
            "never_restarted": list(unitgen.NO_RESTART_EXIT_CODES),
        },
        "evidence": {
            "validate": {"schema": VALIDATE_SCHEMA, "cost": "milliseconds",
                         "checks": "manifest schema and cross-field rules, purity, candidate envelope"},
            "precheck": {"schema": PRECHECK_SCHEMA, "cost": "about 2 seconds",
                         "checks": "validate, plus a short confined run under Landlock with the audit "
                                   "hook recording, ledger provenance, and unit lint"},
            "battery": {"schema": BATTERY_SCHEMA, "cost": "about 15 seconds",
                        "checks": [f"DB-{i:02d}" for i in range(1, 19)],
                        "admissible": True},
            "admissible_for_activation": "battery PASS only, then a human Class C ruling",
        },
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
