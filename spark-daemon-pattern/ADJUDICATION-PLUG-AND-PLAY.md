# Plug-and-play contract stack × daemon pattern — review for adjudication (r3.8)

- **Status:** DRAFT FOR ADJUDICATION, a companion to `DAEMON-CONTRACT.md` and `ADJUDICATION-SPARK-SOURCES.md`. One small evidence fix (PX-01) and one structure test (PD-53, section 7) are implemented. Everything else is a recommendation; PD-46 to PD-68 are PENDING.
- **Revision:** r3.4, 2026-09-29, by Claude. r3.3 reviewed the plug-and-play proposal (sections 1 to 6). r3.4 adds the framework/services boundary (section 7) and the decisions kernel v2 will need to take (section 8). r3.8 adds the universal contract's alignment with the stack and four further considerations (section 9).
- **Source:** a pasted analysis proposing a "Plug-and-Play Contract Stack": an L0 component envelope, L1 plane contracts, L2 capability contracts, L3 optional optimization profiles, L4 policy contracts, and three connection modes (zero-touch, guided, learned adapter). **Caveat:** its author worked from a review summary, not the roadmap documents, and its X1 and Step 9 details are that summary's. This review did not read X1 or Step 9 either. Nothing here should be read as a statement about what X1 says.
- **Question asked:** what in that analysis is worth applying to the standard daemon pattern?
- **Short answer:** the daemon pattern is already a working instance of the proposal. It has an L0-style envelope, one L1 plane contract, a machine-executable conformance suite, pinned contract versions and a no-authority candidate path, with evidence for all of them. It also covers a lifecycle shape that neither proposed pilot exercises: a **resident** process rather than an invoked one. The pattern's experience argues for changing the proposed L0 in two places (section 3), and it exposed one evidence gap here, now fixed (PX-01).

## 1. Where the daemon pattern sits in the proposed tiers

| Tier (proposal) | Daemon pattern |
| --- | --- |
| **Fixed, not pluggable** | The skeleton: ledger, recovery, lock, confinement, battery. It owns durability and confinement, never meaning. It stays on Spark's side. |
| **Candidates, propose-only / observe** | `manifest.json` and `daemon.py`: the replaceable part. `daemon_class` has one value, `observe`. |
| **Fixed: the governance observer's isolation** | Already respected. `output_dir` may not lie inside or contain `~/spark-core` or `~/spark-governance` (cross-field rule), and WBS 3.0E.1 and 3.1 D-6 keep the Observer outside this pattern. The pattern must never become a way to replace the Observer. |
| **Declared, not connected (substrate)** | `resources` (`cpu_weight`, `cpu_budget_bp`, `memory_max_mb`, `tasks_max`, `io_class`) and `run_as` are declared needs. The unit generator turns `cpu_weight`, `memory_max_mb`, `tasks_max` and `io_class` into systemd directives, the battery checks `cpu_budget_bp` against a measured projection (DB-14), and IF-01 sets the memory floor. Nothing is "hooked up". |

## 2. The proposed L0, field by field

"Today" was checked by reading `handoff.py`, `manifest.py`, `contract.py`, `battery.py` and `runtime.py`. Values are real, from `examples/git-watch` at contract 1.0.1.

| L0 field (proposal) | Daemon pattern today | Where |
| --- | --- | --- |
| `implementation_id`, `version` | `name: git-watch`, `version: 0.1.0` | manifest |
| `artifact_digest` | Per file: `daemon.py` `d2ff22e9…`, `manifest.json` `97bd9686…` (file bytes). The runtime and battery also use the **canonical** manifest digest `3fe3b064…` (see P5) | envelope `subject`; `DAEMON_START`; battery report |
| `claims: contract, version: 1.x` | `required_contract: {contract_version: 1.0.1, contract_sha256: af8f169e…}`, graded as exact, compatible, mismatch or incompatible | envelope; `check_envelope` |
| `lifecycle` verbs | Resident, not invoked. The skeleton owns start → cycles → stop (WBS 3.1's signal contract, PD-37); the daemon supplies only `sense`/`decide`/`digest`. Liveness is the systemd watchdog, not a `health` call | runtime |
| `effects: reads/writes/network` | `reads`, `commands`, `deny`, `network.mode`, `output_dir`, enforced three times: purity check, audit hook, Landlock plus the systemd sandbox (DB-17 shows the kernel layer holds alone) | manifest; guard; landlock; unitgen |
| `execution_profile` | `resources`, `run_as`, `watchdog_seconds`, `step_timeout_seconds` → generated unit (the CPU budget is checked by DB-14, not a unit directive) | manifest; unitgen |
| `authority: propose_only` | **Deliberately absent.** `daemon_class: observe` is a ceiling the skeleton enforces. The envelope is a closed schema that refuses any extra key: "a candidate carries no claims about itself" | handoff; `AUTHORITY` |
| `provenance: source, build_attestation` | `producer: {kind: human | script | model, id}` and `intent`. A build attestation does not apply, because the source file is the artifact | envelope |
| `provenance: conformance_report` | **Deliberately not carried by the candidate.** Only a battery PASS counts; `precheck` says OK or FAIL, never PASS, with `activation_evidence: false` | battery; precheck |
| "never rewrite a released contract" | Enforced by test: a rule change changes `contract_sha256`, and the suite fails until a new version line is added to `contract/versions.json` | `test_handoff` |
| L3 optimization profile | Polling is the baseline binding. inotify is reserved as a wake-up only (Proposal C2), which is the proposal's L3 shape exactly | runtime |
| L4 policy (T0–T3) | Every daemon is T0 (recommended in `ADJUDICATION-SPARK-SOURCES.md` §3); `act` is reserved | contract |

## 3. Where the pattern's experience says the proposal should change

These are recommendations to whoever designs X1's L0. They are not changes to this pattern.

**P1. Authority must not be a component field.** The proposed L0 has the component state `authority: propose_only`. A self-declared authority can only be trusted if it is checked, and if it is checked, the checked value is what matters. The pattern's rule works better: a component may declare a **ceiling** that can only narrow what it gets (`daemon_class: observe`), and granted authority lives only in Spark's own admission record. Rename the field `requested_ceiling` and keep the grant in X1's record.

**P2. Conformance evidence is produced by Spark, not supplied by the component.** A report the component brings with it is a hint. The pattern's battery is re-run by the adjudicating side against the exact digests, and fast feedback is labelled so it cannot be mistaken for evidence (`precheck` is never PASS). X1 should re-execute the conformance suite. A shipped report can shorten iteration, but it cannot admit anything.

**P3. A contract claim should pin a digest, not only a version range.** `version: 1.x` cannot tell "same version, different text" apart from a match. The pattern hit exactly that case, which is why `contract_sha256` exists and why "mismatch" is its own state. Recommend the four match states (exact, compatible, mismatch, incompatible) with a declared support horizon (KECC, PD-39).

**P4. L0 needs a second lifecycle shape.** The proposed verbs (`describe`, `handshake`, `health`, `invoke`, `quiesce`, `shutdown`) fit invoked components such as a chunker or a tool. A resident component schedules itself, owns a durable output, and is stopped by a signal. For it, `quiesce` means WBS 3.1's gate: a stop is honored at the gate and an accepted commit finishes. Recommend two shapes, `invoked` and `resident`, with this pattern and WBS 3.1 as the reference for `resident`.

**P5. Name every digest by what it covers.** The pattern uses two manifest digests: file bytes (in the envelope) and canonical JSON (in `DAEMON_START` and the battery report). Both are correct, but a whitespace-only edit changes one and not the other, and a register that compares the wrong pair will refuse a good daemon or accept a changed one. L0, and the pattern's PD-40 register, should name them `manifest_file_sha256` and `manifest_canonical_sha256`.

**P6. Declared effects are worth what the host enforces.** An effects list such as `reads: [payload]` is documentation until something enforces it. The pattern writes effects in terms the host can enforce (paths, commands, a network mode), enforces them in three layers, and proves the kernel layer holds alone. Recommend that each L1 plane define its effects vocabulary in enforceable terms.

**Agreed as written:** no LLM in the trust path; additive evolution within a major version; transport as a binding; L3 profiles advertised, never assumed. The "learned adapter" mode is this pattern's candidate path with `producer.kind: model`: it gets `validate` and `precheck` for fast iteration and the battery for evidence, and it gains no authority from either.

## 4. Fixed in this revision

**PX-01. The battery report did not name the code it tested.** Without `--envelope`, a report bound its verdict to the manifest digest and the contract but not to `daemon.py`. A PASS could not be tied to the code it passed, which is the one binding PD-40's activation register and the proposal's "conformance evidence" both need. The report now carries `daemon_code_sha256` for the bytes the checks ran (the workspace copy). The test, in `tests/test_handoff.py::BatteryCatchesAlwaysFailingDaemonsTests`, fails without the change with `KeyError` and passes with it. The contract is unchanged: the battery report's fields are not part of it.

## 5. Decisions for adjudication

Continuing from PD-45, in the r3 format.

**PD-46. Offer the daemon pattern to X1 as pilot 0 for L0**, alongside the proposed `text_chunker.v1` and Step 13A tool pilots. It already runs end to end (candidate → validate → battery → evidence), and it is the only one of the three with the resident shape (P4) and host-enforced effects (P6). Map it; do not rebuild it.
Recommendation: APPROVE (a recommendation to the X1 design; no change here). Decision: PENDING

**PD-47. Carry P1 to P3 into the X1 L0 design:** a requested ceiling instead of an authority field, Spark-executed conformance, and digest-pinned contract claims with four match states.
Recommendation: MODIFY the proposal's L0 as described. Decision: PENDING

**PD-48. Record the contract identity in `DAEMON_START`** (`contract_version`, `contract_sha256`), so the ledger states which rules each run was held to. It is additive, a KECC-C1 change.
Recommendation: APPROVE. Decision: PENDING

**PD-49. Name both manifest digests** (P5) in the envelope, the battery report and the PD-40 register before the register is built. A rename in the envelope is KECC-C3 for `spark-daemon-candidate/1`, so do it together with PD-35 and PD-39.
Recommendation: APPROVE. Decision: PENDING

**PD-50. No zero-touch activation for daemons.** A daemon is resident with host read access, and activation installs a unit. It stays at "guided" at most: a declarative manifest, a deterministically generated unit, and a human Class C activation through the PD-40 register. Revisit only when X1 exists and admits resident components.
Recommendation: APPROVE. Decision: PENDING

**PD-51. Defer L2 capability contracts for daemon event types** (for example, a shared `repo_state_observer.v1` that two `git-watch` implementations would both emit). One implementation per observation exists today; this follows WBS 3.1 D-6's "demonstrated multi-daemon need" test.
Recommendation: DEFER until a second implementation of the same observation exists. Decision: PENDING

**PD-52. Keep the ledger and digest files as the pattern's only output binding.** The proposal's "transport is a binding" is sound for invoked components. For this pattern, the on-disk ledger is the durable record (WBS 3.0), not a transport, and making it pluggable would move durability out of the fixed tier.
Recommendation: APPROVE (record only). Decision: PENDING

## 6. Recommended order

1. PD-47 and PD-46 go to the X1 design before its L0 freezes. They are cheapest to change there, and they are the only items here that affect more than this pattern.
2. PD-48 and PD-49 go with the next contract change (PD-35 and PD-39), so the contract moves once.
3. PD-50 goes in the activation register's design (PD-40).

## 7. Framework / services boundary (r3.4)

**Source:** the owner's question whether to adopt the Asterinas framekernel split. Asterinas confines all `unsafe` Rust to a small framework, and its compiler refuses `unsafe` in the services built on top. Services and framework share one address space, so the boundary is enforced by the language, not by hardware.

**What fits.** The pattern already has this shape at the daemon boundary. `daemon.py` is a service: `sense`, `decide` and `digest` are pure, and `ctx` is the only door to the system. The skeleton is the framework. The purity check does the job of Asterinas's compiler rule, and, as in a framekernel, both run in one process with Landlock as the kernel backstop. The Spark project has also already made this split for its own writer. The r4 closeout describes `observer_ledger.py` as "pure verification/stamping helpers" and `observer_writer.py` as the "filesystem shell", and its semantic audit requires exactly one `ftruncate` site.

**What does not transfer.** Python has no compiler-enforced `unsafe` boundary. Here the boundary is a syntactic check, already residual risk 2 in `HARDENING.md`, with Landlock as the backstop. "Memory-safe services" is also the wrong property to claim: Python is memory-safe except through `ctypes`, which only `landlock.py` uses (plus `probes.py`, which tries it in order to prove it is blocked). The property that means something in Python is **no I/O and no system calls in services**.

**The skeleton today**, by what each module imports and calls:

| Layer | Modules |
| --- | --- |
| Services, pure today | `canonical.py` (RFC 8785 JCS, hashing), `render.py` (digest rendering) |
| Mixed: a pure core plus I/O | `ledger.py` (hash-chain building and verification, alongside append, fsync, truncate and quarantine), `manifest.py` (validation alongside file read and path resolution), `purity.py` (`check_source` alongside `check_file`), `contract.py` (pure, but imports `proc` for constants) |
| Framework | `runtime.py`, `guard.py`, `landlock.py` (the only `ctypes`), `proc.py`, `notify.py`, `context.py`, `paths.py`, `unitgen.py` |
| Tools, outside the runtime | `battery.py`, `probes.py`, `cli.py`, `handoff.py`, `scaffold.py` |

**Implemented now (no behaviour change).** `tests/test_layers.py` checks four things:
- The service modules import no OS-facing module and nothing from the framework.
- The service modules never call `open()`.
- `ctypes` appears only in `landlock.py` and `probes.py`.
- There is exactly one truncate site, in `ledger.py`, as in r4's audit rule.

Each check was confirmed to fail on a planted violation: an `import os` in `render.py`, and a second `ftruncate` in `guard.py`.

**PD-53. Adopt the framework/services boundary as a rule of the pattern.**
- Name both layers in the contract.
- Enforce them with `tests/test_layers.py`.
- Move the mixed modules across as they are touched. `purity.py` and `manifest.py` need small splits.
- Do not split `ledger.py` by hand. PD-34 replaces it with the Observer's modules, which r4 already split, so doing it now would mean doing it twice.
- Borrow the shape, not the name: the contract should not claim Asterinas's guarantee.

Recommendation: APPROVE. Decision: PENDING

## 8. Decisions kernel v2 will need

**Source:** the owner's statement that this subsystem "doesn't have a spot in the road map but will have one by kernel v.2". The items below are the ones this pattern cannot settle alone: each touches the Observer, X1, deployment or model-facing delivery. I read the Kernel v0.2 Stage A review, but not a kernel v2 roadmap, so these are questions to put to it, not claims about it.

**Already on the list, and kernel-level in scope:**
- PD-39: KECC change classes and a support horizon for every published contract.
- PD-40: the activation register.
- PD-41: reserved human-correction event names across ledgers.
- PD-46 and PD-47: L0 in X1, with this pattern as pilot 0.
- PD-50: no zero-touch activation for resident components.

**PD-54. A roadmap slot and an owner for resident components.** The daemon subsystem has no WBS number. Kernel v2 should give it one, as an observe-only plane below the fixed kernel. The skeleton goes in the fixed tier (it holds durability and confinement); manifests and `daemon.py` files are the pluggable part.
Recommendation: APPROVE. Decision: PENDING

**PD-55. A shared exit-reason taxonomy across Spark's long-running processes.** WBS 3.1 D-6 left this for "a separate cross-cutting decision" once multi-daemon need is demonstrated. Kernel v2 is where that decision belongs. The evidence it needs is the Observer plus at least one installed daemon. PD-35's voluntary alignment means a shared table would cost this pattern nothing later.
Recommendation: DEFER to kernel v2; keep PD-35 meanwhile. Decision: PENDING

**PD-56. How the Observer and the daemons share one durable writer.** PD-34 needs a mechanism, and this pattern must not import from `~/spark-governance` without one. The options are a kernel-owned library, or a vendored copy pinned by commit and SHA-256 with a test that it still matches `d2785406`.
Recommendation: MODIFY. Use the vendored pinned copy now, and let kernel v2 decide whether a kernel-owned library replaces it. Decision: PENDING

**PD-57. One owner for unit generation.** WBS 3.1 §11 gives WBS 4.0 the Observer's unit, `WatchdogSec`, restart policy, dedicated user, sandboxing and startup timeout. This pattern generates its own units with `unitgen.py`, checked by DB-15. Two generators for the same kind of process will drift apart.
Recommendation: APPROVE one generator for every resident component, chosen at kernel v2. The other becomes the first one's conformance reference (DB-15's syscall check and lint). Decision: PENDING

**PD-58. The off-host anchor.** r4's known limit is that deleting all of `history/` still looks like a first run until an off-host commitment exists. The same is true of a daemon's output directory. Chain heads for every ledger (the Observer's and each daemon's) need one off-host place, one writer and one verification schedule.
Recommendation: APPROVE as a kernel v2 requirement; PD-40's register records into it. Decision: PENDING

**PD-59. The implementation language of the framework half** (from section 7). Stay with the Python standard library for kernel v2. The only memory-unsafe code is about 250 lines of `ctypes` making three Landlock system calls, covered by DB-17 and DB-18. Record the trigger for revisiting: a second kernel interface that needs `ctypes`, or a measured cost that Python cannot meet.
Recommendation: APPROVE (stay; record the trigger). Decision: PENDING

**PD-60. Whether any resident component may act or send.** `daemon_class: act` (PD-01, reopened in `PD-01-DAEMON-CLASSES.md` as classes 3a and 3b) and `network.mode: named` (PD-42) are reserved. Their preconditions are recorded: the H-Track declared safe state, and the egress threat model with an allowlist, rate limit and byte-exact copy. Kernel v2 should say whether either is in scope at all.
Recommendation: keep both reserved through kernel v2. Lifting either needs its own adjudication against those preconditions. Decision: PENDING

**PD-61. How untrusted evidence reaches a model.** A daemon digest is untrusted text. The WBS 3.0 writers' rule (a user or tool turn, inside delimiters, never a system prompt, with the stamp checked for staleness) and the Closing Brief's single read-only tool (§6.2) should become one kernel rule, owned by the Inference Interface, for every observer's output, not a per-daemon convention.
Recommendation: APPROVE as a kernel v2 requirement. Decision: PENDING

**PD-62. Producer identity never raises trust.** Whether a candidate came from a person, a script or a model (`producer.kind`) is provenance only. Every candidate faces the same conformance suite and the same Class C activation, including adapters a model writes for itself. This pattern's `AUTHORITY` statement already says so. Kernel v2 should make it an X1 rule, so no future admission path grants trust by origin.
Recommendation: APPROVE. Decision: PENDING

**Suggested order for kernel v2:**
1. PD-54, which gives the rest an owner.
2. PD-57 and PD-58, which are deployment facts before any daemon is installed.
3. PD-56, before PD-34 is carried out.
4. PD-61 and PD-62, before any model reads a digest or writes a candidate.
5. PD-55, PD-59 and PD-60 once their evidence exists.

## 9. The universal contract: alignment with the stack and four additions (r3.8)

**Source:** the owner's questions "How are we aligning this with our standard pattern?" (the L0 to L4 stack) and "Have you introduced all these considerations for the universal contract? Are there any others…?"

### 9.1 What is in place, and what is only on paper

The universal contract (an L0 envelope, plus a conformance suite that every plugin runs) does not exist as code. It is X1's to define, and PD-46 and PD-47 recommend this pattern as its pilot.

| State | What |
| --- | --- |
| **In code** (daemon contract 2.0.0) | A closed manifest with declared effects and an execution profile; digest-pinned contract claims with four match states; a candidate envelope with no authority; the conformance battery (DB-01 to DB-18) bound to the manifest and code digests; the blind period (`ctx.unsettled`, `blind_limit_seconds`, `SENSE_BLIND`); never-restarted exits; the framework/services test |
| **Recorded, pending** | P1 to P6 and PD-46 to PD-62 (this document); daemon classes, consequence levels, invariants S-1 to S-7 and the connector gate (`PD-01-DAEMON-CLASSES.md`); amendments A-1 to A-12 (`PRIOR-ART-REVIEW.md`) |
| **Not written down before this revision** | Where the recent pieces sit in the stack (9.2), three alignment rules (9.3), four further considerations (9.4) |

### 9.2 Where the recent pieces sit

**The dividing line:** what a component claims about itself belongs in its envelope; what Spark decides about it belongs in Spark's own records.

| Layer | Pieces |
| --- | --- |
| **L0 envelope** (claimed by the component) | The requested class, as a ceiling (P1); declared outward connections (PD-01 §6); the lifecycle shape, resident or invoked (P4; daemons are resident, scripts are invoked); `blind_limit_seconds`, the resident health bound; the execution profile, from which the unit is generated (A-2's `OnFailure=` notifier belongs here) |
| **Spark-side records** (decided by Spark) | The granted class; the consequence level, from a recorded risk assessment (A-3); conformance evidence produced by Spark; revocations (U-5) |
| **L1 planes** | **Telemetry** = the daemon contract: accepted, unsettled and blind cycles, `SENSE_DEGRADED` and `SENSE_BLIND`, and `spark-escalation/1`, shared by daemons and scripts. **Action** = the connector gate and executor, with the black-channel error model (A-1) and logical supervision (A-8). **Inference** = where an Analyst's (2b) requests go |
| **L2 capabilities** | Deferred (PD-51) |
| **L3 profiles** | inotify wake-up over polling. Nothing safety-related (U-2) |
| **L4 policy** | The PD-01 class ladder: Observe may not propose; Advise may propose but not commit; Act may request commits through a gate. Also consequence rules, shelving limits (A-6), alarm rules (A-5), the security level per connection (A-11), and who may stop or revoke (U-5) |
| **Conformance** | The battery today; a base suite that every contract inherits (U-1); checks per option (degraded and recovered records, `OnFailure=` present, the gate's erratic-behaviour suite); drills (U-7) |

**Provisional tier mapping** for L4's "T0 to T3". It still has to be checked against the script board's own definitions (PD-01 open question 1). The consequence level stays a separate field.

| L4 tier | PD-01 classes |
| --- | --- |
| T0 | Observe (1a, 1b) |
| T1 | Advise (2a, 2b) |
| T2 | Operator (3a) |
| T3 | Actuator (3b) |

### 9.3 Three alignment rules

- **U-1. The safety invariants are a base conformance suite, not L0 fields.** The proposal's warning holds: L0 must stay descriptive, or it becomes a universal API. The invariants (S-1 to S-7 in PD-01, plus S-8 proposed in U-4) are prohibitions, not behaviour. They therefore become a suite that every contract's own suite inherits, so every plugin in every plane is tested against them.
- **U-2. Nothing safety-related lives in L3.** L3 is optional and negotiated by definition. Blindness detection, escalation, stopping and authentication must all work on the baseline binding. A faster path may deliver sooner, but it is never the only path.
- **U-3. What an escalation means is a contract; how it is delivered is a binding.** This is the transport contract's own principle: "freeze semantics, not transport technology". `spark-escalation/1` is defined once. The ledger record, the systemd status line, the `OnFailure=` hook, the watcher daemon and an off-host heartbeat are bindings, and any of them can be added without changing it.

### 9.4 Four further considerations from this conversation

Each is new (in no earlier document), and each follows from something this conversation built or found.

**U-4. Temporal honesty: an observation states the window it covers.**

*Problem.*
- Records carry `timestamp_utc`, the moment of commit. Since r3.5, unsettled cycles leave no record unless the blind limit trips (PD-63, choice 3). So a change that happened while git-watch was unsettled through a long build appears, once the repository settles, as an ordinary event stamped up to two hours after it happened, and nothing in the ledger says so.
- Failed cycles do leave `DAEMON_ERROR` records, and the digest's stamp goes stale. But the event itself claims more precision in time than the daemon had. S-2 forbids recording a guess as an observation; its twin for time is missing.
- No contract states which clock does what. The runtime uses a monotonic clock for the blind period, and on Linux that clock does not advance while the host is suspended. Wall-clock record stamps can step backwards after a clock correction, and ledger verification checks sequence and hashes, not timestamp order.

*Proposal.*
- **(a) A new invariant, S-8: never imply more temporal precision than you have.** When the time since the last accepted cycle exceeds a bound (for example, twice the poll interval), the skeleton writes one gap record before that cycle's events. The record carries the gap's length and the counts of unsettled and failed cycles, so consumers read those events as "happened within this window".
- **(b) Clock rules for every plugin**, following the transport contract (LTC §9):
  - monotonic time for liveness, timeouts, blind periods and leases;
  - wall-clock time only for human-readable stamps and credential validity, with a stated skew margin;
  - order only from sequence numbers and the hash chain;
  - a declared behaviour across host suspend: either suspended time counts as blind, or the host is declared never to suspend.

*Where.* L0 lifecycle semantics; the base conformance suite; S-8 in PD-01.

*Leaves open.*
- The gap record's exact form. A new reserved record type is a contract change, best batched with PD-35 and PD-39.
- Whether suspended time counts toward `T`.

**U-5. Stop and revocation: the inverse of activation.**

*Problem.* Everything so far governs starting safely: validation, the battery, digest-bound activation (PD-40), no zero-touch activation (PD-50), unlocking classes one at a time. Nothing defines how to withdraw a component or a whole class, although industrial safety treats stopping as the most basic safety function (stop categories in IEC 60204-1, emergency stop in ISO 13850). Specifically:
- A stop today is a manual `systemctl stop` or `systemctl mask`.
- The ledger records `DAEMON_STOP` with reason `signal SIGTERM` whether an operator stopped the daemon or it was withdrawn for a defect.
- Nothing stops every component of an affected skeleton or contract version at once.
- For the Act family, stopping the process would not by itself revoke a credential that is still valid.

*Proposal.* Universal stop semantics: each is a lifecycle verb with a tested meaning.

| Verb | Meaning |
| --- | --- |
| **Stop** | Controlled, within a declared drain bound, with a recorded reason |
| **Abort** | Forced. The component must be crash-consistent, and the next start must record the unclean end. The daemon pattern already meets both (DB-05) |
| **Revoke** | At the activation register: the digest is marked revoked, the runtime refuses to start, the refusal is never restarted, and the reason is recorded |
| **Emergency stop** (Act family) | At the gate: every ticket is refused at once; short-lived credentials expire without renewal; actions in flight are reported as unknown, not as cancelled (LTC-13) |

No stop may depend on the cooperation of the component being stopped, which is the same principle as S-5.

*Where.* L0 lifecycle verbs; L4 (who may stop, revoke and emergency-stop); conformance tests: drain within the bound, a forced abort at random points leaves a verifiable chain, and a revoked digest is refused.

*Leaves open.* Where the revocation list lives before the register exists; who may revoke when the owner is unreachable.

**U-6. Cumulative bounds: what grows over a lifetime.**

*Problem.*
- The contract bounds everything instantaneous: record size, digest size, memory, CPU budget, command output, per-step timeouts, the blind period. It bounds nothing cumulative.
- The ledger is append-only with no rotation (WBS 3.0 r3 K1 forbids checkpoints). Start-up verifies the whole chain before `READY=1` (r3 VR6), under a fixed `TimeoutStartSec` of `max(60, watchdog_seconds)`.
- **Measured here** (`evidence/r3.8/ledger_verify_bench.txt`; x86-64, Python 3.11): verification runs at about 20,000 records per second, linear from 20,000 to 200,000 records. git-watch's 120-second start timeout is therefore exceeded at about 2.4 million records (about 1.6 GB).

| Write rate | Time to reach that size |
| --- | --- |
| git-watch's worst case (three events every 60-second cycle) | About a year and a half |
| A 5-second daemon writing once per cycle | About four and a half months |
| Realistic rates | Years |

- It is not urgent, but it is unbounded, and when it arrives the failure is severe: start timeout, restart, start limit, failed, with no way back without intervention. Every restart after exit 70 also pays the full verification cost.
- Quarantine fragments accumulate without limit too, and a full disk fails every daemon's ledger (A-12).
- The same limit applies to the Observer's own ledger, whose start timeout WBS 4.0 owns. The DGX's ARM cores have not been measured.

*Proposal.*
- Every component declares cumulative bounds in its execution profile: a ceiling on durable growth per day, a retention rule, and a bound on start-up work.
- A conformance check projects the time to reach each limit from measured growth, as DB-14 projects CPU.
- For ledgers, the design must keep K1's intent (no second source of truth):
  - the ledger is split into sealed segments, and each sealed segment's final head is anchored off-host (PD-58);
  - start-up fully verifies the open segment, and checks each sealed segment's head against its anchor;
  - full re-verification runs on a schedule (`spark-daemon verify` on a timer).

  Every byte stays verifiable; not every byte is verified at start-up. The credit that `ADJUDICATION-SPARK-SOURCES.md` §5 gives to full verification at every start still stands for integrity; this proposal bounds its cost.

*Where.* L0 execution profile; the base conformance suite; the ledger design, together with PD-34 and PD-58.

*Leaves open.* Segment size and anchoring cadence. Also whether the Observer adopts the same rule, which would reopen WBS 3.0 (its closeout says any reopening must be deliberate).

**U-7. Assurance of the assurance: evidence expires, and independence must be shown.**

*Problem.*
- Conformance evidence is produced once, before activation, and bound to digests. Nothing re-proves in production that the safety functions still work.
- The functions that fire only on failure are exactly the ones that decay unnoticed: the blind-limit exit, an `OnFailure=` notifier, Landlock enforcement after a kernel update, the escalation path. IEC 61508 answers this with a proof test at a stated interval. Operations tooling answers it with an alert that always fires, to prove the pipeline works (the Prometheus watchdog).
- Independence is asserted rather than shown. A staleness watcher built with this same pattern (one way to provide A-2's external check) would share every skeleton defect with the daemons it watches. A bug in ledger verification or in the runtime loop would blind watcher and watched in the same way at the same moment. This is a common-mode failure; IEC 61508 calls the fraction of failures shared this way the β-factor.

*Proposal.*
- **(a) Conformance evidence carries a validity period**, set by consequence level, and is re-proven by scheduled drills:
  - a probe daemon run to its blind limit in a sandbox, with the notification checked end to end;
  - the Landlock probe (DB-17) re-run after every kernel update;
  - a heartbeat that always fires through the escalation path.

  Expired evidence is itself an alert.
- **(b) Every monitoring or redundancy claim names what it shares with what it watches:** host, kernel, interpreter, skeleton code, disk, clock, credentials, network. A monitor's most critical check is implemented diversely. A-2's staleness check becomes a small separate program that does not import `spark_daemon`; a ledger's age needs only a file's modification time and one JSON line. Richer checks, such as chain verification, may reuse the skeleton.

*Where.* A validity period on L0's conformance evidence; a drill category in the base suite; L4 intervals per consequence level.

*Leaves open.* The intervals themselves; who is paged when a drill fails and nobody answers (the no-response path noted below).

**Considered and not added.** Two further points came up, and are left as open questions rather than additions:
- **Interactions between components:** one daemon observing another's output or, once Act classes exist, an action → alert → action oscillation. A per-component contract cannot see these; they belong to the activation register as a cross-manifest check.
- **A declared no-response path for every escalation.** Everything ends at "a human", and nothing says what happens when that human is unavailable.

### 9.5 Decisions

**PD-64. Adopt the stack alignment:** the placement in 9.2, rules U-1 to U-3, and the provisional T0 to T3 mapping, pending the script board's definitions.
Recommendation: APPROVE. Decision: PENDING

**PD-65. Temporal honesty (U-4):** invariant S-8, a gap record when the time since the last accepted cycle exceeds a stated bound, and the clock rules.
Recommendation: APPROVE. Batch the gap record with the next contract change (PD-35, PD-39, PD-49). Decision: PENDING

**PD-66. Stop and revocation (U-5):** stop, abort, revoke and emergency stop as lifecycle verbs with tested meanings; revocation held in the activation register (PD-40).
Recommendation: APPROVE. Build revocation with the register. Decision: PENDING

**PD-67. Cumulative bounds (U-6):** declared growth, retention and start-up bounds for every component, with a conformance check that projects time to each limit.
Recommendation: APPROVE the principle. DEFER the ledger segmentation design to PD-34 and PD-58, and raise it with the Observer's WBS 3.0 owners. Decision: PENDING

**PD-68. Assurance of the assurance (U-7):** evidence validity periods, scheduled drills, stated shared dependencies, and a diverse implementation of each monitor's critical check.
Recommendation: APPROVE. Decision: PENDING
