# Plug-and-play contract stack × daemon pattern — review for adjudication (r3.4)

- **Status:** DRAFT FOR ADJUDICATION, a companion to `DAEMON-CONTRACT.md` and `ADJUDICATION-SPARK-SOURCES.md`. One small evidence fix (PX-01) and one structure test (PD-53, section 7) are implemented. Everything else is a recommendation; PD-46 to PD-62 are PENDING.
- **Revision:** r3.4, 2026-09-29, by Claude. r3.3 reviewed the plug-and-play proposal (sections 1 to 6). r3.4 adds the framework/services boundary (section 7) and the decisions kernel v2 will need to take (section 8).
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

**PD-60. Whether any resident component may act or send.** `daemon_class: act` (PD-01) and `network.mode: named` (PD-42) are reserved. Their preconditions are recorded: the H-Track declared safe state, and the egress threat model with an allowlist, rate limit and byte-exact copy. Kernel v2 should say whether either is in scope at all.
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
