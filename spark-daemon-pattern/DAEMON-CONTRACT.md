# Daemon contract and candidate handoff — draft for adjudication (r3)

- **Status:** DRAFT FOR ADJUDICATION, a companion to `README.md` in the way `ADJUDICATION-AP.md` is. Implemented and tested (see `tests/test_handoff.py`). Every PD below is PENDING until the human records a ruling.
- **Revision:** r3, 2026-09-27, by Claude.
- **Roadmap position:** groundwork. The subsystem that will *produce* daemons has no slot in the roadmap yet; it gets one by kernel v2 (PD-31). This document fixes the interface that subsystem will plug into, so that when it arrives nothing about the pattern has to change and nothing about its authority is left implicit.

## 1. The problem

Until r2, the handoff to whoever writes the next `daemon.py` was informal. A person or a model read the README's prose, free-typed a manifest and three functions, and found out what was wrong by running `validate` (text output) or the 15-second battery. `spark-new daemon` was deliberately not built, so this was the real interface. It lost information in six places:

| Where the handoff lost information (r2) | What exists now (r3) |
| --- | --- |
| The manifest schema lived only in `manifest.py`'s Python code; nothing could check a manifest offline | `contract/manifest.schema.json` (JSON Schema 2020-12), generated from the validator's own constants and held in agreement by tests (25 mutations refused by both; cross-field rules listed as validator-only) |
| The interface was scattered across README prose, `context.py`, `purity.py` and `manifest.py` | `spark-daemon describe`: one document (`spark-daemon-contract/1`) with ctx methods and result types, purity rules, the import allowlist, Git rules, base deny, reserved names, bounds, exit codes and evidence lanes |
| `validate` printed text lines; only the battery had JSON | `validate --json` (`spark-daemon-validate/1`) with per-field, per-line diagnostics |
| The only evidence lane was the full battery (~15 s, SIGKILLs, fault injection) | `precheck` (~1 s): validate plus a short confined run, provenance and unit checks |
| Nothing said which version of the rules a daemon was written against | `contract_version` (semver for authors) plus `contract_sha256` (exact identity), pinned in `contract/versions.json` |
| One reference daemon, so one idiom to imitate | Four, one idiom each (section 7), all 18/18 on the battery |

## 2. The shape, borrowed from the Step 9 HandoffEnvelope

The project already solved this problem once, pointed the other way. Step 9 hands X1/Step 10 a `HandoffEnvelope` with exact source identity, a `control_version`, a deterministic metadata digest and a `required_contract`, and gets back a `ProcessingObservation`. Here the daemon pattern is the supply side:

| Step 9 / X1 | Daemon pattern |
| --- | --- |
| `control_version` | `contract_version` (semver for daemon authors) |
| Deterministic metadata digest | `contract_sha256`: RFC 8785 canonical hash of the contract body, identical on Python 3.10 to 3.13 |
| `required_contract` | The candidate's `required_contract: {contract_version, contract_sha256}` |
| Exact source / revision identity | The candidate's `subject`: `sha256` of `daemon.py` and `manifest.json` |
| `HandoffEnvelope` | `candidate.json` (`spark-daemon-candidate/1`) |
| `ProcessingObservation` | The validate, precheck and battery reports, each quoting the envelope's digest |
| "Observation is not activation" | "Generation is not adjudication": a candidate carries no authority (section 5) |
| "Expose only enough for downstream to support it later; never redesign downstream ownership" | The contract exposes what an author must satisfy and nothing else: no hook into activation, the decision log or the battery's verdict |

## 3. Outbound: the contract

```bash
python3 -I -B bin/spark-daemon describe              # the whole contract
python3 -I -B bin/spark-daemon describe --identity   # {contract_version, contract_sha256}
python3 -I -B bin/spark-daemon schema                # the manifest JSON Schema
```

The same documents are committed under `contract/` so that a generator on another machine, or in another language, can read them without running this package. They are generated, never hand-edited. `make check-contract` and the self-tests fail if the committed files drift from the enforced rules.

**Versioning, for daemon authors (PD-24).** MAJOR when a daemon that satisfied the old contract can fail the new one (a new purity rule, a narrower bound, a newly refused Git option). MINOR when the contract only grows (a new ctx method, a new allowed import, a new Git subcommand). PATCH for wording. `contract_sha256` changes with *any* enforced value. The tests fail until `CONTRACT_VERSION` is bumped and the new pair is appended to `contract/versions.json`, so a rule can never change silently under an unchanged version.

## 4. Inbound: the candidate

A candidate is a folder: `manifest.json`, `daemon.py` and, from a script or a model, `candidate.json`:

```json
{
  "schema": "spark-daemon-candidate/1",
  "required_contract": {
    "contract_version": "1.0.0",
    "contract_sha256": "ef03a744961cfad81abced56b0c12b18b222ce3219d779e5f6f79a64319fb585"
  },
  "subject": [
    {"name": "daemon.py", "sha256": "28ed4652..."},
    {"name": "manifest.json", "sha256": "536f0f30..."}
  ],
  "producer": {"kind": "script", "id": "spark-daemon scaffold"},
  "intent": "Scaffolded observe-only daemon hostname-watch; replace this purpose line."
}
```

- **Closed schema.** Unknown keys are refused. There is deliberately no field for a result, a verdict, an approval or a ruling. `"battery_result": "PASS"` is a validation *error* ("a candidate carries no claims about itself").
- **Sealed.** If either file changes after the envelope is made, `validate --envelope` fails. The envelope digest (`envelope_sha256`) is what every report quotes back.
- **Contract match:** `exact` (same version and sha) passes. `compatible` (same major, different minor) passes with a warning, validated against the running rules. `mismatch` (same version, different sha) and `incompatible` (different major) fail.
- **Producer:** `human`, `script` or `model`, plus a free-text id. This is provenance only. It does not change what is checked or who decides.

`spark-daemon-author envelope --dir D --producer-kind model --producer-id "..." --intent "..."` writes one. `spark-daemon-author scaffold` writes a starting folder with one. (Contract 5 moved both out of the runtime CLI, E-9.)

## 5. The authority rule

Stated once, and embedded verbatim in the contract (`authority`); every report carries its own authority note:

> A candidate carries no authority. Whoever or whatever produced it - a person, a script or a model - it cannot certify itself: validate and precheck are fast feedback, the conformance battery's PASS is the only admissible evidence, and activation remains a human Class C decision. A producer is never granted authority that no one adjudicated.

This is the discipline the project already applies to daemon *content*, applied identically to whatever *produces* daemon content. The same scope-control rule holds as in Step 9: the pattern exposes what a producer needs in order to be checked, and never gives a producer a way to reach activation, the decision log, the contract itself, or the battery's verdict.

## 5a. The six invariants (contract 5)

No change may relax any of these. Each is enforced in one place; the checks and tests named with it fail if it breaks. They are published in the contract (`contract.invariants`), with the self-tests that hold each one; `tests/test_invariants.py` fails if a named check or test stops existing. They replace the owner's eight and r4.9's INV-1 to INV-9 (E-8, `ADJUDICATION-V5.md`). The owner's eighth ("nothing relaxes confinement, blindness, authentication, or proposal-versus-authority") is the preamble.

| ID | Invariant | Enforced in | Battery checks | Absorbs |
| --- | --- | --- | --- | --- |
| I-1 | Observe only, and the kernel enforces it: no network, no program, writes only in output_dir, reads only the declared paths, under one path policy for every layer | Landlock and the systemd unit, both projected from pathpolicy.PathPolicy; purity and the audit hook are diagnostics and a tripwire | DB-15, DB-17, DB-18 | owner 2, owner 3, INV-1, INV-2 |
| I-2 | Never record a guess: only an accepted cycle produces events; an unsettled, truncated or failed read abandons the cycle | the runtime's cycle (runtime.py), with ctx raising on truncation | DB-04 | INV-3 |
| I-3 | Blindness surfaces within blind_limit_seconds, across restarts and clock steps, and is never restarted away | semantics.blindness, the one function the runtime and status use | DB-20 | INV-4 |
| I-4 | The ledger is the one source of truth: append-only and hash-chained, verified two independent ways, interpreted once; integrity uncertainty and every gap are visible, never rendered healthy | ledger.py writes; ledger.py and verifier/ledger_verify.py verify; semantics.interpret interprets | DB-04, DB-06, DB-07, DB-16, DB-22 | owner 4, owner 5, INV-5, INV-6 |
| I-5 | It runs only what was judged, and judging is not activation: the installable unit is a projection of a qualifying report made on this host, the runtime refuses files that differ from the unit's digests, and nothing here installs or enables a unit | judge.conclusions and `unit --report` when installing; the runtime's digest gate at every start | DB-24 | owner 1, owner 7, INV-7 |
| I-6 | Bounded: one budget for each whole cycle, every unit timing derived from it, and restart decided by the exit class | the cycle's alarm (runtime.py); timings derived from cycle_budget_seconds (manifest.py, unitgen.py); RestartPreventExitStatus from NO_RESTART_EXIT_CODES | DB-14, DB-25 | owner 6, INV-8, INV-9 |

## 6. Three lanes of evidence

| Lane | Cost | Checks | Answers | Admissible for activation |
| --- | --- | --- | --- | --- |
| `precheck [--json] [--envelope]` | milliseconds, no process started | DB-01, DB-02, DB-24, DB-25: manifest schema and cross-field rules, purity, envelope, unit lint and derived timings | PASS / FAIL | No |
| `validate [--json] [--envelope]` | about 1 s | precheck, plus DB-03 (a 2-cycle run under Landlock with the audit hook recording; no file outside the output dir changed) and DB-04 (ledger provenance with zero `DAEMON_ERROR`) | PASS / FAIL | No |
| `battery [--envelope]` | about 15 s | every registered check (DB-01 to DB-25; DB-19, 21, 23 reserved) | PASS / FAIL / INCOMPLETE | **Only a qualifying report** (full battery, PASS), then a human Class C ruling |

Contract 5 swapped the first two names so the profiles nest in the order they run (precheck ⊂ validate ⊂ battery); the CLI says so on stderr for one release. All three write one report shape, `spark-daemon-report/1`, which holds facts only; the verdict and "qualifies" are derived on read. The installable unit is a projection of a qualifying battery report: `unit --report R` refuses unless the report qualifies and was made on this host, and the runtime refuses to start the unit if the manifest, `daemon.py` or contract differ from those it names.

The battery quotes the envelope as well. An envelope that does not match the files the battery ran cannot yield PASS (INCOMPLETE), because the report would be about different bytes than the candidate claims.

## 7. The loop a kernel-v2 authoring subsystem would run

Nothing here builds that subsystem. This is the protocol it would follow, so its boundary is settled before it exists:

1. `describe` → keep `contract_version` and `contract_sha256`. Read `contract.code` and `contract.ctx`: that is the whole interface.
2. `spark-daemon-author scaffold --name N --dir D` → a folder that already passes the battery. Edit only `daemon.py` and the manifest's `purpose`, `reads`, `trigger`, `ledger.event_types`, `digest`. Imitate the closest reference daemon (table below).
3. `spark-daemon-author envelope --producer-kind model --producer-id <run id>` after every edit.
4. `precheck --json --envelope` until `RESULT: PASS`. Diagnostics carry `layer`, `where` (manifest field or `daemon.py`) and `line`.
5. `validate --envelope` until `RESULT: PASS`.
6. `battery --envelope` once. The report goes to a human, with the envelope, the diff against the closest reference daemon, and the install plan (`unit --plan`).
7. Stop. Activation is Class C.

**It must never:** edit anything under `spark_daemon/`, `contract/` or `tests/`; widen a manifest (`reads`, `resources`) just to make a check pass without saying so in `intent`; retry the battery with a different seed to fish for a PASS (the seed is recorded); or present a precheck OK as a pass.

| Reference daemon | Idiom | ctx surface |
| --- | --- | --- |
| `meminfo-watch` | Parse a kernel text file into bands | `read_text` on `/proc` |
| `disk-watch` | Threshold bands over a numeric gauge; path presence | `disk_usage` |
| `dir-watch` | Inventory diff over untrusted names; bounded, batched events | `list_dir`, `stat` (symlink-safe) |

Contract 5 removed `git-watch` with `ctx.run` and `ctx.git` (R-2): a daemon runs no program. It remains in history at `a3599fd`.

Every reference daemon ships a `fixture_home/` when it reads under `~`, because the battery now requires a clean run with zero `DAEMON_ERROR` (PD-28).

## 8. Design precedents from widely used projects

The goal was to borrow shapes that many people already know, not to invent vocabulary:

| Project | What it does | Borrowed | Not borrowed |
| --- | --- | --- | --- |
| Terraform (`terraform validate -json`, `terraform providers schema -json`) | Machine-readable validation with `valid`, `error_count`, `warning_count` and `diagnostics[]` carrying severity and location; a separate command dumps the full provider schema | The validate report's shape (`valid`, counts, `diagnostics` with `severity`/`where`/`line`); `describe` as the schema dump | Provider plugin protocol: our daemons are not plugins and never talk back to the skeleton |
| Kubernetes CRDs, `kubectl explain`, `--dry-run=server` | A structural OpenAPI schema published with the API; admission runs the real checks without persisting | Schema published beside the enforcer; precheck as a dry run of the real runtime rather than a separate linter | Mutating admission: nothing here rewrites a candidate |
| Helm `values.schema.json` | JSON Schema shipped with a chart and checked before install | `contract/manifest.schema.json` as a first-class artifact beside the code | Schema-less "just try it" installs |
| in-toto attestations / SLSA provenance | `subject: [{name, digest: {sha256}}]`; provenance must come from the build platform, not from the producer's own claims | `subject` with file digests; the rule that a producer's statement is never evidence | Signing and key custody (that is PD-19/SCITT territory, deferred) |
| Cookiecutter, `cargo new`, kubebuilder | A scaffold that compiles and passes its own checks on day one | `scaffold` output passes the battery 18/18 | Templating engines and hooks that run code at generation time |
| Semantic Versioning 2.0 | MAJOR/MINOR/PATCH meaning | `contract_version` rules, defined from the daemon author's point of view | — |
| pre-commit / fast linters in front of CI | Cheap local signal before the authoritative run | `precheck` in front of the battery | Letting the fast lane gate anything |
| systemd (`systemd-analyze security`, `syscall-filter`) | Offline unit scoring and seccomp group listing | DB-15 now resolves the unit's filter and checks start-up syscalls | — |

## 9. Considered and deliberately not built

- **`spark-new daemon` with authority.** `scaffold` is a better blank page, not a generator, and it grants nothing.
- **A model inside this package.** The pattern stays standard-library-only and deterministic. A model-based producer is an external client of the contract with `producer.kind: "model"`.
- **Auto-activation on battery PASS.** Unchanged: Class C.
- **Signing envelopes.** An envelope identifies, it does not authenticate. If a producer later runs on another host, add signing with the SCITT/Sigstore work (PD-19), not before.
- **A contract diff tool** (what changed between 1.0.0 and 1.1.0 for authors). Cheap to add once there is a second version. Not needed yet.
- **DB-19, a run under real systemd** (`systemd-run --wait` with the generated unit's properties). This is the most valuable next battery check. It needs a host where PID 1 is systemd, which the build workspace is not. Recommended as the first addition after the DGX smoke test (HARDENING.md, residual risk 3).

## 10. Decisions for adjudication

Continuing the README numbering (PD-01 to PD-22). Each has a recommendation and a decision line.

**PD-23. The contract is the only normative author interface.** `describe` and `contract/*.json` are normative; README prose explains, and where the two disagree the contract wins.
Recommendation: APPROVE. Decision: PENDING

**PD-24. Contract versioning.** Semver defined for daemon authors; `contract_sha256` as exact identity; `contract/versions.json` append-only and enforced by tests.
Recommendation: APPROVE. Decision: PENDING

**PD-25. Git `safe.directory` scoped to the declared repository.** Without it, every Git daemon running as its own system user (PD-09) observes nothing (HF-08). With it, that repository's own config is trusted under Landlock and systemd confinement, with the residual filter-driver risk noted in HARDENING.md. Alternative: require repositories owned by, or mirrored for, the service user.
Recommendation: APPROVE, scoped to exact paths; prefer bare mirrors for repositories the owner does not control. Decision: PENDING

**PD-26. The candidate envelope and the no-authority rule** (section 4 and section 5): closed schema, no result fields, sealed digests, `exact`/`compatible`/`mismatch`/`incompatible` matching.
Recommendation: APPROVE. Decision: PENDING

**PD-27. Evidence lanes.** Precheck answers OK/FAIL, never PASS, and is not admissible; only battery PASS is.
Recommendation: APPROVE. Decision: PENDING

**PD-28. A clean battery run records zero `DAEMON_ERROR`** (HF-16). Consequence: a daemon that reads under `~` ships a `fixture_home/`.
Recommendation: APPROVE. Decision: PENDING

**PD-29. Contract 1.0.0's rule set as the starting line**: the r3 purity rules (private and introspection attributes, aliasing, dynamic access, import-time calls), the read-only Git allowlist and refused options, the extended command denylist, and `StatInfo.mtime_us`. Adding a Git subcommand or an import later is MINOR; refusing something new is MAJOR.
Recommendation: APPROVE. Decision: PENDING

**PD-30. The unit allows exactly the three Landlock syscalls**, rather than the whole `@sandbox` group (which also allows `seccomp`) (HF-07).
Recommendation: APPROVE. Confirm on the DGX under real systemd. Decision: PENDING

**PD-31. Roadmap: a "daemon authoring" slot at kernel v2, with its boundary fixed now.** It consumes `describe`; it produces candidate folders; it may loop on validate and precheck and invoke the battery; its outputs are evidence, never rulings; it never activates, never edits the contract or the skeleton, and never widens a manifest without saying so in `intent`.
Recommendation: APPROVE the boundary now; DEFER the slot's placement to kernel v2 planning. Decision: PENDING

## 11. Final recommendation

Adopt r3 as the pattern's baseline, subject to the PDs above. The hardening closes every escape found in review, and the layered design is confirmed: Landlock contained the file-system effects of every escape it could see. Adopt the contract and envelope as the handoff to the future authoring subsystem, with PD-31's boundary recorded before that subsystem exists. The order of the next steps matters more than their size:

1. **On the DGX Spark:** run `make test` and `make battery-examples` (aarch64, Landlock ABI 7 expected). Then install `meminfo-watch` as a real unit and confirm `DAEMON_START.landlock.status == "enforced"`. This is the first time anything runs under PID 1 (HF-07 and PD-30).
2. **Rule on PD-23 to PD-31** together with the pending PD-01 to PD-22, since several interact (PD-09 with PD-25, PD-11 with PD-27).
3. **Only then** give the authoring subsystem its kernel-v2 slot. It inherits a published, versioned, test-pinned contract and an evidence path that it cannot shortcut.
