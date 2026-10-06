# Spark daemon pattern — r4.9 adjudication and implementation handoff

Status: OWNER-APPROVED DECISIONS; IMPLEMENTATION AUTHORIZED WITHIN THIS BOUNDED PACKAGE
Date: 2026-10-06
Scope: R-1 through R-8 only. This handoff does not unlock a new daemon class, authorize activation, or change Break Glass/LTC authority.

## 1. Owner-approved decisions

| ID | Final ruling | Freeze interpretation |
|---|---|---|
| R-1 | APPROVE WITH MODIFICATION | Contract 4.0.0 removes manifest `deny` outright. Do not carry a `deny: []` compatibility field inside v4. A v3-to-v4 migration/validation error must explain the removal. |
| R-2 | DEFER | Do not yet remove all program execution from the core. Revisit after the planned daemon set establishes whether Git/program execution remains a real requirement. |
| R-2b | APPROVE | Execution is least-privilege and declaration-driven. A command-free daemon receives no child-process launch grant. A command-using daemon may launch only the exact executable/command surface its manifest authorizes; one declared command must not become a Boolean "all execution allowed" switch. |
| R-3 | APPROVE WITH GUARD | Only a successful qualification battery may emit an installable unit. `unit` remains an unqualified preview. Battery PASS is evidence only and never becomes activation authority. |
| R-4 | APPROVE WITH SPLIT | Use one pure function to interpret verified records into blindness/integrity state for startup and `status`. Keep byte/hash-chain verification independently implemented; the independent verifier must not import the daemon package. |
| R-5 | REJECT | Do not adopt unanchored tail-only startup verification. A valid suffix alone does not preserve the old meaning of "chain intact." |
| R-5b | DO NOT FREEZE AS FINAL RULE | A deterministic records/bytes threshold may be used only as a temporary implementation bridge if necessary; never use host load or a percentage of timeout to decide integrity semantics. Deferred full verification must be reported explicitly and must not render as ordinary healthy integrity. |
| R-5c | APPROVE AS TARGET | Introduce checkpointed verification: a completed independent full-chain verification may establish a trusted checkpoint; startup verifies the checkpoint and only the suffix after it. Until a trustworthy checkpoint anchor exists, preserve full startup verification. |
| R-6 | APPROVE | `cycle_budget_seconds` is the single manifest-level cycle-time authority. Watchdog/stop relationships are mechanically derived by framework policy. If commands exist, command/local blocking limits must fit inside the remaining cycle deadline. This remains required even for command-free daemons. |
| R-7 | APPROVE | `precheck`, `validate`, and full battery become profiles over one check registry/result model. No independent judges with duplicate semantics. |
| R-8 | DEFER | Do not rename the core until a second real consumer/project exists. |
| R-8b | APPROVE WITH MODIFICATION | Keep current names and store a deterministic rename map. Verify source hash + rename-map hash + transformed-tree hash; do not rely only on a broad normalized contract hash. |

## 2. Non-negotiable invariants preserved

1. Battery PASS is qualification evidence, not permission to activate.
2. No daemon class above the currently authorized class is unlocked by this package.
3. A command declaration is an allowlist, not a Boolean capability flag.
4. Startup and `status` may share semantic interpretation, but corruption/tamper verification remains diverse.
5. Integrity uncertainty must be visible. `FULL_CHECK_DEFERRED` or equivalent cannot be rendered as ordinary healthy integrity.
6. `cycle_budget_seconds` constrains the whole sense/decide/digest cycle, not only subprocesses.
7. Activation remains a separate authority decision after unit qualification.
8. No change here relaxes confinement, blindness, authentication, or proposal-vs-authority invariants.

## 3. Implementation order

### Phase A — unify the judge first (R-7)

Create one check registry and one result vocabulary. `precheck`, `validate`, and `battery` select named profiles from that registry.

Required properties:
- `precheck` stays fast, offline, and dependency-free.
- `validate` is a larger deterministic profile.
- `battery` is the full qualification profile and the only profile allowed to produce installable output.
- Check IDs and meanings are identical regardless of profile.
- A check ID is retired/replaced rather than silently changing meaning.

Acceptance evidence:
- Same failing invariant yields the same check ID and reason in every profile containing that check.
- A mutation to shared judge logic is caught by all applicable profiles.
- No old standalone precheck/validate decision path remains reachable.

### Phase B — move installable-unit production behind the battery (R-3)

The battery may call deterministic unit rendering only after the full qualifying profile passes.

Required properties:
- `unit` produces preview output only.
- Preview output lacks a qualification binding accepted by the installer/runtime.
- An installable unit binds at least the qualified manifest/contract/code identity already required by the pattern.
- Changing an install-affecting option requires a fresh battery qualification.
- A battery PASS still does not activate or install automatically.

Acceptance evidence:
- Failed/incomplete battery cannot emit an installable unit.
- Preview cannot pass the install/activation qualification gate.
- Changing a unit option invalidates the prior installable artifact.
- Activation still requires the existing separate authority path.

### Phase C — perform the contract 4.0.0 manifest/budget/execution cut (R-1, R-2b, R-6)

Batch the breaking schema work once.

R-1:
- Remove `deny` from schema v4.
- Reject v4 manifests that contain it as an unknown/removed field.
- Provide an explicit v3 migration diagnostic.
- Express readable scope entirely through the positive allow model/no-gaps rule.

R-2b:
- Empty command declaration => no child-process launch authority.
- Non-empty command declaration => exact approved executable/argv surface only.
- Do not implement "has any command => may exec arbitrary binaries."
- Preserve Python/native-extension imports required by the daemon runtime; this decision governs child-program launch, not executable memory in the interpreter.

R-6:
- Replace independently tunable cycle/watchdog timing inputs with `cycle_budget_seconds` plus framework-owned derivation policy.
- Create one monotonic cycle deadline at cycle entry.
- Every potentially blocking operation receives/derives a remaining-deadline cap.
- A command timeout, if commands remain, cannot exceed the remaining cycle budget.
- Unit watchdog and stop timing are derived/cross-checked from the same cycle budget and framework version.

Acceptance evidence:
- v4 manifest with `deny` fails with the specific migration explanation.
- Command-free daemon cannot spawn a process.
- Command-free daemon can still import the compiled/native Python modules required by the normal runtime.
- Command-using daemon cannot launch an undeclared executable.
- A cycle whose blocking call attempts to exceed the remaining budget fails within the cycle deadline.
- Generated unit watchdog/stop values cannot contradict the declared cycle budget.

### Phase D — collapse blindness semantics without collapsing verification diversity (R-4)

Create one pure interpretation function over already verified/typed records. Both startup and `status` use it.

Keep a second independent ledger verifier that:
- does not import `spark_daemon`;
- independently parses/verifies the durable chain representation;
- reports verification facts, not daemon policy conclusions.

Acceptance evidence:
- Startup and `status` cannot disagree when given the same verified facts.
- Mutating the shared semantic interpreter fails end-to-end behavior tests.
- Mutating the primary verifier is caught by the independent verifier fixture and vice versa.
- A syntactically valid but hash-invalid chain never reaches the semantic interpreter as verified evidence.

### Phase E — checkpointed ledger verification (R-5c)

Do not weaken startup integrity merely to gain constant-time startup.

Target checkpoint record/projection should bind at minimum:
- ledger identity;
- verified-through sequence/record position;
- verified chain-head hash at that position;
- verifier/schema version;
- creation/verification evidence identity.

A checkpoint is valid for startup acceleration only if it was produced by a completed independent full-chain verification and is anchored outside the unverified suffix it is intended to replace.

Startup behavior:
1. No trustworthy checkpoint: verify the full chain as today.
2. Trustworthy checkpoint: verify checkpoint identity, then verify the suffix from checkpoint head to current head.
3. Any mismatch: fail closed as ledger-integrity failure.
4. No "best effort" silent fallback from a failed checkpoint to a healthy state.

`status` behavior:
- Independent full verification remains available/schedulable.
- A successful full verification may advance the checkpoint.
- A deferred full check is explicitly reported as deferred; it is not equivalent to `CHAIN_INTACT`.

Important gate:
- If no trustworthy checkpoint anchor exists yet (for example because the activation/register/evidence authority needed to bind it is not built), stop this phase and retain full startup verification. Do not implement unanchored tail-only verification as a substitute.

Acceptance evidence:
- Corrupt a record before the checkpoint: checkpoint/full-verifier path detects invalidity rather than trusting the suffix blindly.
- Corrupt a record after the checkpoint: startup suffix verification fails.
- Replace checkpoint with one for a different ledger: startup refuses it.
- Truncate/extend/replay suffix: startup detects continuity failure.
- Startup cost with a valid checkpoint scales with new suffix length, not historical ledger length.

### Phase F — vendoring/name compatibility seam (R-8b)

Add the rename map and deterministic transformation test without renaming the live core.

Bind:
- source-tree/content hash;
- rename-map hash;
- transformed-tree hash.

Acceptance evidence:
- Deterministic rename produces the same transformed-tree hash twice.
- One semantic source-byte mutation changes the transformed-tree hash.
- One rename-map mutation changes the map hash and transformed-tree hash.
- Merely renaming known symbols according to the frozen map does not create a false contract mismatch.

## 4. R-5c checkpoint authority note

R-5c is intentionally stricter than the proposed R-5b heuristic. A checkpoint that is merely another writable file beside the ledger is a performance cache, not a trust anchor. Such a cache may still be useful, but it MUST NOT justify skipping historical verification under the frozen integrity claim.

Therefore the coding pass must first identify what existing authority can bind the checkpoint. If no existing mechanism can do so without inventing a new authority subsystem, preserve full startup verification and record R-5c as implementation-pending rather than weakening PD-32/INV-5.

## 5. Freeze/qualification gate for this package

Before calling this package closed:

- contract/schema version is 4.0.0 wherever these breaking semantics require it;
- all previous conformance/self-tests remain green or are explicitly migrated with stable replacement check IDs;
- new focused tests above pass;
- mutation/adversarial tests cover the new command boundary, unified judge, cycle deadline, shared blindness interpreter, and checkpoint continuity;
- generated installable unit can only come from a complete battery PASS;
- battery PASS still cannot activate the daemon;
- no daemon class unlock occurs;
- `git diff --check`/repository hygiene passes;
- final review records exact candidate SHA and test evidence.

## 6. Explicit deferrals

- Full R-2 (no programs anywhere in the core): revisit when the planned daemon inventory establishes whether Git/program execution is still required.
- R-8 neutral renaming: trigger only when a second real consumer vendors the core.
- R-5c trusted checkpoint acceleration: implementation may wait for a suitable existing anchor; until then full startup chain verification remains normative.

## 7. Reviewer instruction

Review the implementation by seam, not only by individual function:

- manifest command declaration -> OS/audit execution grant;
- cycle budget -> blocking operations -> watchdog -> stop timeout;
- shared semantic interpreter -> startup/status callers;
- independent verifier -> semantic interpreter;
- full verifier -> checkpoint -> startup suffix verification;
- battery result -> unit generator -> installer/activation boundary;
- source tree -> rename map -> transformed tree.

For every seam ask: what does each side assume about the other, and what happens when that assumption is false?
