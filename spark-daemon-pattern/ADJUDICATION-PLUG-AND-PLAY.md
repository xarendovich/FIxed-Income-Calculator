# Plug-and-play contract stack × daemon pattern — review for adjudication (r3.3)

- **Status:** DRAFT FOR ADJUDICATION, a companion to `DAEMON-CONTRACT.md` and `ADJUDICATION-SPARK-SOURCES.md`. One small evidence fix is implemented (PX-01). Everything else is a recommendation; PD-46 to PD-52 are PENDING.
- **Revision:** r3.3, 2026-09-29, by Claude.
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
