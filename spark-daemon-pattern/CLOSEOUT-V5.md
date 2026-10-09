# Closeout: the Spark daemon pattern at contract 5.0.0

- **Date:** 2026-10-09. **Source:** this pattern at `97d6841` (contract **5.0.1**, `26e543fd8b36dd1d945be953c12d705c306adaa7cddf1cc2d24b893eff09dd16`, a wording-only PATCH over 5.0.0 `2d080b40…` merged as this repository's PR 2; skeleton 0.5.0, manifest schema 4); `Kernel-Update` at `ddec911` (PR 2 merged 2026-10-06); `X1-registry` at `a018c4e`; the FirstBorn pilot at vendored 3.1.0.
- **The owner's instruction:** review Kernel-Update PR 2 against the current work and close the process out. "The architecture has been completed. We no longer need to grow it."
- **What this document is:** the review of Kernel-Update PR 2 (§1, §2), the statement of what is complete (§3), the closing rule that stops growth (§4), the index that says which documents are live and which are records (§5), the owner's exit list (§6), the review of this repository's PR 2, the 5.0.1 consolidation (§7), and the logical framework the owner asked for: the goal, the three axioms, the six invariants as their checks, and the simpler solutions found by looking back (§8). It adds no mechanism, no document class and no decision ID.

## 1. PR 2 reviewed against the pattern

PR 2 ("Owner direction 2026-10-06; move historical kernel files (KU-32)") records the owner's "go with the recommendations" for Gate B. It changes documents only, signs nothing, and consolidates nothing. Each of its items against this pattern:

| PR 2 item | What it directs | Where the pattern stands | Action |
| --- | --- | --- | --- |
| **D2 / K2-VER-003: adopt** | Preserve historical meaning during upcasting; missing facts never become invented permissive *or restrictive* defaults; unknown stays unknown; authority-dependent use fails closed | Already the pattern's rule in three places: `contract/versions.json` never edits an existing line (STB-6, pin test); a schema 2 or 3 manifest is told field by field what changed rather than given a default; a ledger whose integrity is uncertain is rendered uncertain, never healthy (I-4), and a corrupt ledger refuses and changes nothing (exit 65) | **None.** Aligned by construction |
| **D1 / K2-AUTH-004: owner-profile first** | Atomicity scope and preconditions at the authoritative effect boundary, proven by a race fixture, promoted to the kernel only when a second subsystem needs it | The pattern has no effect boundary: it causes nothing (I-1). Its one commit point is the ledger append, one writer, canonical bytes, fsync, tail quarantine on a torn write. D1 does not apply to it and it is not the "second subsystem" that would promote D1 | **None.** Not applicable; must not be cited as the second subsystem |
| **KU-01..31 accepted as classified; KU-06 read as "re-verify at every start, not re-resolve"** | Binding lifetime is the owning contract's; missing proof of a current binding does not permit execution | The runtime gate re-verifies `manifest_sha256`, `daemon_code_sha256` and `contract_sha256` at every start and refuses without them (exit 78, never restarted). X1's own open item S-9 names "the daemon pattern's start-up digest check" as one of three places this one rule applies | **None now.** The half the rule still owes, a revocation reaching the start-up check, is the recorded U-5/PD-58 item: "revocations have to reach the start-up check; fail-closed when they can't" is X1's wording of the same gap |
| **KU-07, KU-20: MOVE OUT** | Exit tables, lifecycle states, watchdog, signal safe points and stop latches leave the kernel for adjacent owners; "no shared daemon exit code table in Core" | The pattern owns its own: exit 2 usage, 65 ledger corrupt, 70 uncertain commit (the only restarted stop), 73 already running, 78 policy; `WatchdogSec` derived from one cycle budget; `TimeoutStopSec` outlasting a watchdog period. Nothing is shared with the kernel and nothing is imported from it | **None.** The pattern is the adjacent owner KU-20 implies; `KERNEL-REGISTER-CANDIDATE-KU-33.md` names it |
| **KU-21: REMOVE** | The close criterion "done when an export is named" is deleted; WBS 3.1A is a lifecycle package, not a telemetry sidecar | The pattern never claimed a telemetry export and has no close condition of that shape. Its closure is this document plus the hardware gate | **None** |
| **KU-32: historical files moved with banners** | Superseded text must not be citable as current | Applied here as §5: every document in the package is classified live, record, or candidate. The pattern already keeps its own superseded line under `sources/v0.1-line/` | **Done in §5**, by index rather than by editing twenty files |
| **K-3: owner's own capture for Gate C** | The runtime SHA, the Step 8.5 freeze record and the S4 inventory, read by the owner, under SP-10 | The pattern's analogue is the DGX evidence bundle (`HARDWARE-GATE-DGX.md` §4), also the owner's or operator's capture, also not a model's | **None here.** Recorded as the same kind of item |
| **Steps 9/10 indexed in X1** | X1's `steps-9-10/` is navigation, not a second authority | X1's census already holds `A12.DAEMON_LEDGER` as an evidence identity that "remains in its owning evidence store" and bears no authority. That is KU-33's "facts without authority" in X1's words | **Cite it** in KU-33 when the owner carries the row (§6) |

**Verdict on PR 2.** Nothing in it asks for a change to the pattern, and nothing in the pattern contradicts it. The direction of PR 2 is shrink the kernel and move daemon concerns to adjacent owners; the pattern took that direction first. The one thing PR 2 does not do, and by its own admission rule could not, is name the adjacent owner. That remains KU-33, a Gate B candidate the owner carries.

## 2. The moved-out constraints, checked anyway

PR 2 moved `OBSERVER_DAEMON_CONSTRAINTS.md` to `spec/historical/` as superseded. It is not current kernel text and is not cited here as a requirement. Its "Forbidden" list is still the clearest statement anyone has written of what a daemon beside this kernel must never do, so the pattern is checked against it once, for the record:

| Forbidden (historical text) | The pattern |
| --- | --- |
| An operation on `LtcProvider` or any transport SPI | Runs no program, opens no socket but `NOTIFY_SOCKET`, `network.mode` admits only `none` |
| A read of raw cursors, pointers, lease handles or ring layout | Reads only declared files through `ctx`; no shared memory, no mapping |
| A writable mapping of bytes the producer has validated | The output directory is the only writable place and may not overlap a read (I-5) |
| Any effect on admission, commit, lease, authorization or reclamation | None; a candidate carries no authority, a report is facts, a daemon pushes nothing |
| Manufacturing a transport outcome, including rewriting `UNKNOWN_AFTER_RESTART` | Exit 70 is the pattern's own uncertain-commit class and is documented as not that outcome (`DAEMON-TAXONOMY-RECONCILIATION.md`) |
| Promoting trust, issuing a grant, choosing an execution profile | Producer kind is provenance only (PD-62); qualification is evidence, not permission |
| Blocking the send path, the commit path or reclaim | Not on any such path; `CPUWeight`, `Nice`, idle I/O class, one bounded cycle |
| Treating receipt time as sample time, or a display sequence as a ledger sequence | `seq`, `timestamp_utc` and `boottime_ms` are kept distinct; the notify `STATUS=` seq is lifecycle text and must never be read as a clock (5.0.1 wording) |
| Becoming a parent of Step 8.5 | `~/spark-core/data` is base-denied; the kernel takes no dependency on any daemon |

Every row holds. This table is evidence that the pattern sits where the kernel said a daemon must sit, from a document the kernel has since retired; it creates no requirement.

## 3. What is complete

| Deliverable | State at `dceb568` |
| --- | --- |
| A published, versioned, test-pinned contract | 5.0.1 (`26e543fd…`), a wording-only PATCH over 5.0.0; `make check-contract` fails on drift; support history 1.0.0 → 5.0.1 in `versions.json` with field-by-field migration messages; identity byte-identical on Python 3.10, 3.11, 3.12 and 3.13 (`evidence/v5/consolidation-501/`) |
| An evidence path a producer cannot shortcut | One facts-only report; verdict and qualification derived on read; the unit a projection of a qualifying report; the runtime refuses anything else at every start |
| Confinement | Landlock required at every start; execute granted nowhere; one `PathPolicy` projected three ways with an agreement test |
| The ledger | Append-only hash chain, canonical JCS, one writer, two independent verifiers, cross-checked; a corrupt ledger refuses and changes nothing |
| Six invariants | Published in the contract, each naming its checks and tests, held by `tests/test_invariants.py` |
| The authoring boundary | Fixed before any authoring subsystem exists: describe → scaffold → envelope → precheck → validate → battery → stop; the envelope carries no authority |
| Self-tests and batteries | 270 test methods in the tree; 264 and then 270 passed in the recorded runs on the 5.0.0 tree (`VERIFICATION-V5.md`, `evidence/v5/`), and 270 passed on the 5.0.1 tree at `97d6841` under CPython 3.11.15 (`evidence/v5/consolidation-501/full-suite-97d6841.txt`; the interpreter was unstated until the UDC pass, which exposed HF-45 on 3.12/3.13) and 271 on 3.12.3 and 3.13.12 at the commit that fixes HF-45 (`suite-matrix.txt`), Landlock ABI 7; three reference batteries, 21 PASS and DB-24 N/A each. **A count in a tree is not a pass; the runs are the evidence** |
| Independent review | Verification pass (HF-43, HF-44 fixed), adversarial handoff, alignment review, class-requirements review, three reviewer rounds on placement and the DGX gate, all adjudicated with the evidence stated |
| Hardware qualification | **Not run.** The one deliverable that needs a machine (`HARDWARE-GATE-DGX.md`) |

The architecture is complete in the sense the owner means: every mechanism the pattern needs exists, is tested, and is named by an invariant. What is not complete is evidence that only a machine can give, and decisions that only the owner can make.

## 4. The closing rule

From this commit the pattern is **closed to growth**. It accepts exactly four kinds of change, and a change that is none of them is refused with a pointer to whose it is:

1. **A defect fix** with a reproduction and a regression test, recorded as the next HF row in `HARDENING.md`. Contract identity unchanged unless the fix changes a rule.
2. **A wording defect**, PATCH, one digest bump, no rule change. The 5.0.1 bundle itself is done (PR 2, §7): I-4 relative to the observed head, "owner activation decision" in place of "human Class C", the notify boundary, the cycle-budget text.
3. **The 6.0.0 cut**: V-1 / ADM-9, the skeleton tree digest as a fourth expected digest. One MAJOR change, nothing else in it.
4. **Evidence**: the DGX bundle, and nothing but the bundle and its accounting.

Refused by this rule, with the owner named:

| Asked for | Whose it is |
| --- | --- |
| A new class (`advise`, `act`), a `PROPOSAL` event, the Blind Forester | A class decision of the owner, each against its recorded preconditions (PD-60, PD-72); not a pattern change |
| A command-backed or wrapped-process daemon | Outside the confined process (PD-72); not this core |
| A capability or event-type registry (option C) | The pattern owns the schema when the trigger fires; the trigger has not fired |
| A consumer, Spark Core's reader, the activation register, the anchor | The consumer (PD-40, PD-58): the first attachment is the anchor |
| A model reading a digest | PD-61, a kernel-side rule; nothing here |
| A tool-head taxonomy | Not in either repository; X1's L0 |
| A new review document | Closed for the 5.x line. The UDC track is the design review for change 3 (the 6.0.0 cut); its passes live on its branch and their adjudications (`UDC-PASS-1-ADJUDICATION.md`) are inside this rule. A finding still goes into `HARDENING.md`; a decision into the owner's register |

The reviewers' habit this rule ends is the useful one: every review this week produced a document and every document produced a corrected pin somewhere else. The architecture does not need another pass of that; it needs the machine and the owner.

## 5. Document index: live, record, candidate

**Live** (normative or operative at 5.0.0; a change to one is a change to the pattern):

| Document | Role |
| --- | --- |
| `DAEMON-CONTRACT.md`, `contract/` | The contract, its schemas, versions and invariants |
| `README.md` | Map, operating notes, revision history and the decision log |
| `HARDENING.md` | Every defect HF-01..44 and residual risks 1–6 |
| `ROADMAP.md` | What remains, in order |
| `HARDWARE-GATE-DGX.md` | The one open deliverable |
| `CLOSEOUT-V5.md` | This document: the closing rule and this index |
| `tools/daemon-start.py`, `verifier/ledger_verify.py` | Copyable consumer tools; part of the contract's reading rules |

**Records** (adjudications and review packages; history under the kernel's D2 rule, never edited, never cited as current requirements):

`ADJUDICATION-AP.md`, `ADJUDICATION-R4.5.md`, `ADJUDICATION-R4.9-OWNER.md`, `ADJUDICATION-PLUG-AND-PLAY.md`, `ADJUDICATION-V5.md`, `ADJUDICATION-ALIGNMENT-REVIEW.md`, `ADJUDICATION-CLASS-REQUIREMENTS.md`, `REVIEW-PACKAGE-R4.5.md`, `REVIEW-PACKAGE-V5.md`, `VERIFICATION-V5.md`, `ADVERSARIAL-REVIEW-HANDOFF-V5.md`, `HANDOFF-REVIEW-PACKAGE.md`, `INTEGRATION-REVIEW.md`, `LTC-INTEGRATION-REVIEW.md`, `FREEZE-PROPOSAL-R4.9.md`, `DECISION-BRIEF.md`, `PD-01-DAEMON-CLASSES.md`, `DAEMON-TAXONOMY-RECONCILIATION.md`, `RECONCILIATION-V01-LINE.md`, `sources/v0.1-line/`, `evidence/`.

**Candidates** (written here, decided elsewhere; the owner carries them and they leave this package when recorded):

| Document | Destination |
| --- | --- |
| `KERNEL-REGISTER-CANDIDATE-KU-33.md` | `Kernel-Update` register, Gate B, as an ADD |
| `HANDOFF-PD-51-L2-EVENT-TYPES.md` | The other reviewers; outcome recorded in the decision log as PD-51's disposition |
| `HARDWARE-GATE-DGX.md` §3 (the FirstBorn click-path holding copy) | The pilot, pinned to a pattern commit and the contract digest |

## 6. The owner's exit list

Everything below is the owner's. None of it is pattern work, and the pattern does not wait on any of it to be complete as an architecture.

1. **Freeze 5.0.1** as the software baseline for DGX qualification, as it stands at `97d6841` plus this commit. In the consolidation's own vocabulary: SOFTWARE BASELINE FROZEN is the owner's word; DGX QUALIFIED is the machine's.
2. **Open the pull request for the branch.** Forty-four commits and contracts 3.0.0 through 5.0.0 exist only on `claude/clever-bardeen-qqkxez`; `main` carries 2.0.0. Every cross-repository pin to the pattern names commits `main` does not have.
3. **Carry KU-33 to Gate B**, adding two citations from X1: `A12.DAEMON_LEDGER` (the identity X1 already holds) and X1-1A (the shared start-up rule).
4. **Record the three confirmations** already asked for: V-1 is the 6.0.0 change after the freeze; the first consumer attachment is PD-58's anchor; owner-ruling records stay as records.
5. **Closed by PR 2.** The transport-contract wording no longer needs to be supplied: renaming the pattern's term to "owner activation decision" dissolved the collision whatever the other text says (§8.3, the first simpler solution).
6. **The DGX visit**: G-6 at 3.1.0 with the pilot, then migration to 5.0.0 and the v5 battery as the service's user; one visit can also serve the kernel's Gate E, recorded separately.
7. **Close PD-51** on the reviewers' markup; D now is the recommendation, C when two implementations emit one observation.
8. **`CODEOWNERS`** or not: X1's own closeout recorded a PR merged through a documented HOLD and named mandatory review on protected `main` as the remedy. This repository has one reviewer and the same exposure.

After item 1 the package is frozen; after item 2 it is on `main`; after item 6 it is qualified. Nothing else grows.

## 7. This repository's PR 2: the 5.0.1 consolidation, reviewed

PR 2 (`spark-daemon: consolidate contract 5.0.1 wording and scope`, head `6a860d8`, merged by the owner at `97d6841` into this branch on 2026-10-09) was written by another reviewer after the alignment and class-requirements reviews. It is the 5.0.1 wording bundle this closeout had listed for the owner, done. Reviewed here against the code, not against its own description.

**Verified.**

| Claim | Check here | Result |
| --- | --- | --- |
| Contract identity 5.0.1 / `26e543fd…` | `spark-daemon describe --identity` on the merged tree | Matches |
| Manifest-schema digest `955844b5…` | Raw file hash is `e7d625aa…`; the digest is over the **canonical** bytes and is published as `json_schema_sha256` in the contract | Matches, once the method is known. The PR should have said "canonical" |
| `make check-contract` PASS | Rerun | PASS |
| Cross-Python identity test skipped (one interpreter) | Rerun on 3.10.20, 3.11.15, 3.12.3, 3.13.12 | Ran, not skipped; identical digest on all four (`evidence/v5/consolidation-501/`) |
| "No runtime enforcement rule changes" | The whole code diff read: `contract.py` (version, four strings, I-4 text, a `notifications` section), `__init__.py` docstring, `manifest.schema.json` one description, `versions.json` one appended line | True. No module that enforces anything changed |
| Full suite on the 5.0.1 tree | Not claimed by the PR; run here | 270 tests, OK, Landlock ABI 7 |

**Each consolidation decision, with a verdict.**

| Decision in PR 2 | Verdict | Why |
| --- | --- | --- |
| I-4 relative to the observed head; completeness needs an external anchor | **Agree.** The exact guarantee, said in the invariant | It is what the differential fuzz proved a chain can and cannot show |
| "human Class C" → "owner activation decision" | **Agree, and it is the best move this week.** A rename dissolved a collision the previous review had wanted to *reconcile* by sourcing the other document's text | The new phrase cannot collide with a transport class whatever that text says. One owner item (exit list 5) closed by subtraction |
| sd_notify is lifecycle only; the `STATUS=` seq is never a clock | **Agree.** The sentence this closeout had queued, written | A simpler form remains available: drop the seq from `STATUS=` and the sentence is unnecessary (§8.4) |
| Cycle-budget text corrected to one alarm | **Agree.** Stale text from the removed per-call design | A documentation defect of the kind HARDENING would otherwise have gained a row for |
| E-4 closed as intentional layered asymmetry | **Agree.** One `PathPolicy` owns policy; layers with different knowledge need not agree on an error label | Closing a question by declining to build is the pattern's own habit (R-5c, R-8) |
| ADM/STB/LAT stay review matrices | **Agree** | Already this closeout's reading; a seventh invariant was never justified |
| V-2, V-3, hand parser, R-5c, Advise/Act, L2 registry: trigger-deferred | **Agree** on all six | Each trigger is stated and testable |
| PD-58 is a consumer-side receipt, not a daemon service | **Agree.** The same as "the first attachment is the anchor", stated with its limit: it cannot prove pre-attachment completeness | The limit is the honest part and had been missing |
| V-1 as 6.0.0 `runtime_bundle_sha256`, strengthening I-5 | **Agree**, including the name: a named trusted bundle beats "a tree digest" | The bundle's file set is the 6.0.0 design's one real question |
| KU-33 narrowed to an owner-map boundary; the per-head analysis made non-normative | **Agree.** An owner-map row must not carry an integration roadmap | The analysis is kept as the record it was |
| PD-51 option D as an implementation citation; a registry needs interchangeable implementations and a consumer need | **Agree.** Sharper than the earlier trigger ("two daemons emit the same observation"): similar names and shared lifecycle events do not qualify | It also closes handoff question 4 without a registry |
| FirstBorn qualifies 5.0.1 directly; a 3.1.0 run is preliminary only | **Agree with one bound.** Do not run a campaign to qualify an old contract. But if no 3.1.0 run occurs naturally, the 5.0.1 run is the **first start of any unit under a real host systemd**, so its bundle must capture `DAEMON_START.landlock.status == "enforced"` and the DB-15 seccomp evidence on that first start. HARDENING residual 3 stays open until one of the two runs exists | The order changed; the evidence required did not |

**Findings, and what was done about them.**

1. Two "Class C" strings survived in code: the install-plan text every generated unit carries (`unitgen.py`) and the Landlock module's docstring. Neither reaches the contract body (the contract imports only the exit-code list from `unitgen`), so neither changes the digest. Both reworded in this commit; `make check-contract` and the identity are unchanged.
2. The merge dropped the roadmap's one-line pointer to the closing rule. Restored, pointing at both stop lines (this §4 and the consolidation's §9), which say the same thing.
3. This closeout said 5.0.0 and `dceb568`. Updated to 5.0.1 and `97d6841`; exit-list item 5 closed; item 1 now names the 5.0.1 baseline.
4. The PR's own verification was honest about what it could not run (one interpreter; no Landlock suite). Both are now run and recorded. Nothing it claimed was wrong.
5. The PR described itself as "draft for review before merge" and was merged with no review comment 36 minutes after opening, by the one person who can. That is the exposure X1's own closeout named (exit list 8). It is not a finding against the content, which holds.
6. The revision row records the author as "OpenAI review". A record; the merge is the owner's authorization of the identity change.

**Verdict.** PR 2 is the correct closing move for this line: it subtracts. It made three claims exact (chain, activation, notify), closed one question by declining to build (E-4), and removed one owner dependency by renaming (Class C). Nothing in it grows the pattern. It is accepted as the software baseline candidate; freezing it is the owner's word.

## 8. The logical framework: goal, axioms, invariants, and the simpler solutions

The owner asked for this so a reviewer can *derive* the pattern rather than audit it, and so the invariants are seen to be the minimum. What follows is a reading of what exists, not a change to it. If the owner wants the three axioms published, that is a text change for 6.0.0 (§8.5).

### 8.1 The end goal, in one sentence

**A reader can rely on what a resident daemon recorded without trusting the code that recorded it, the person or model who wrote that code, or the host's good day.**

Everything else is a consequence. The owner's later sentence, "treat the daemon as the stable contract and the agent as the replaceable decision maker", resolves under this goal into: the daemon is the stable, judged, confined *producer of facts*; anything that decides is an agent, outside, consuming those facts through its own contract. PR 2 states the conclusion: no replaceable decision maker lives inside the daemon. Replaceability is free precisely because it is outside.

### 8.2 Three axioms, and the six invariants as their checks

| Axiom | Says | Invariants that check it |
| --- | --- | --- |
| **A. Harmless** | The daemon can affect nothing but its own output directory, and the *operating-system kernel* enforces that, not the daemon's code | I-1 (observe only; kernel-enforced); the resource half of I-6 (bounded CPU, memory, tasks, one cycle budget) |
| **B. Honest** | The record says neither more nor less than was observed: only accepted observations are written, the record is tamper-evident relative to its head, and every absence (a guess not written, silence, a gap, integrity uncertainty) is *visible*, never rendered healthy | I-2 (never record a guess), I-3 (blindness surfaces, across restarts and clock steps, never restarted away), I-4 (append-only, hash-chained, verified two ways relative to the head, interpreted once) |
| **C. Judged** | What runs is exactly what was judged, every bound it runs under derives from the one declared budget, and judging is evidence, never permission | I-5 (runs only what was judged; judging is not activation); the derivation half of I-6 (watchdog, stop and start timeouts, restart by exit class, all from `cycle_budget_seconds`) |

So the six invariants are not six independent axioms. They are the checks of three. I-2 and I-3 are the two halves of B's "neither more nor less": I-2 forbids writing what was not observed; I-3 forbids hiding that nothing was. I-6 is wholly derived: its resource bounds serve A, its timing derivations serve C, and its watchdog exists so that I-3 holds under systemd (a hung loop is killed, so silence cannot persist unseen). A reviewer who verifies A, B and C has verified the pattern; I-1..I-6 are where the tests and battery checks attach, and E-8 requires that attachment, so the six names stay.

**What a fourth axiom would have to be.** Every proposal this month that looked like a seventh invariant turned out to be one of these three said again: ADM-9 (the enforcer is not judged) is C with a gap; PD-58 (the head is not anchored) is B with a limit; the notify sentence is B (nothing on the socket is an observation). That is the test for any future addition: if it is not a new axiom, it is a check, a gap, or a limit of an existing one, and goes into `HARDENING.md`, not the contract.

### 8.3 Forced by the axioms, or chosen

A reviewer should distinguish what the goal forces from what was chosen, because only the choices can be simplified.

| Mechanism | Forced or chosen | By |
| --- | --- | --- |
| Landlock, required at every start; execute granted nowhere; `network.mode: none` | **Forced** by A. The code is untrusted, so the boundary must be the kernel's. Purity and the audit hook are diagnostics (PD-101), allowed to have gaps | A |
| One writable directory that may not overlap a read | **Forced** by A and B together (a daemon that could write what it reads could fabricate its own observations) | A, B |
| One canonical `PathPolicy` projected three ways | **Chosen.** Three hand-kept lists satisfied A until they drifted (HF-41's neighbourhood); one source with an agreement test removes the drift. The simplification, not the rule | A |
| Append-only hash chain, canonical bytes, one writer | **Forced** by B | B |
| Two verifiers that share no code | **Chosen**, and justified: HF-44 was found only because they disagreed. The cost is a second copy of the chain rule; the alternative (one verifier) makes a common-mode bug invisible, which HF-43 showed is real | B |
| Accepted cycle only writes; `ctx.unsettled` abandons a cycle | **Forced** by B (I-2) | B |
| Blind limit on the boot clock, surviving restarts; one reacquisition cycle; exit 78 never restarted | **Forced** by B (I-3). The boot clock is forced by "across clock steps"; "never restarted" is forced by "never restarted away" | B |
| Heartbeat at half the blind limit | **Chosen** parameter; the *existence* of a liveness fact is forced by I-3 | B |
| Digest gate at every start (`--expect-*`); refuse without it | **Forced** by C (I-5) | C |
| The report as facts, verdict derived on read; the unit a projection of a qualifying report | **Chosen**, to make C hold by construction: a stored "qualified" bit could lie, a derived one cannot; a unit that only a qualifying report can produce cannot install an unjudged daemon. Cut 2 replaced a mechanism (the qualification record) with an absence | C |
| `harness` with explicit parameters instead of a test mode | **Chosen**, for C: a daemon with a test switch is two daemons, only one of them judged | C |
| One cycle budget; every unit timing derived | **Forced** by C's "every bound derives", once the budget exists; the budget itself is forced by A (a resident loop must be bounded) | A, C |
| Five exit classes (2, 65, 70, 73, 78) | **Chosen.** The forced minimum is two: restart, or stop for a person. The other three are operator information at no cost | C |
| Six reserved lifecycle events | **Chosen** shape; forced *content* is smaller (start, heartbeat, error, quarantine). `DAEMON_ERROR_CLEARED` and `DAEMON_STOP` could be derived by readers, at the cost of every reader deriving them | B |
| Judging is not activation; a person installs | **Forced** by C and by the owner's standing rule (no zero-touch activation) | C |

Everything in the "forced" rows a reviewer can derive from the goal without reading the code. Everything in the "chosen" rows is where simplification could still happen, and §8.4 says what and at what cost.

### 8.4 Simpler solutions found by looking back

**Already applied.** Each of these replaced a mechanism, or a dependency, with less:

1. **A rename instead of a reconciliation** (PR 2): "owner activation decision" removed the need to obtain and read the transport contract's "Class C" text. One owner item deleted.
2. **A citation instead of a registry** (PD-51 option D): the digests a consumer needs were already in every `DAEMON_START` and report; saying a consumer may cite them cost nothing and deferred a registry to a real trigger.
3. **A receipt instead of a service** (PD-58): the first consumer that stores the head it saw *is* the anchor. No anchor daemon, protocol or invariant.
4. **Accepting asymmetry instead of unifying errors** (E-4): layers with different knowledge may say different things; one policy source is enough.
5. **A projection instead of a record** (cut 2): the unit is derived from a qualifying report; the qualification record and the `qualified` flag ceased to exist.
6. **One source instead of three lists** (cut 4, `PathPolicy`).
7. **Explicit parameters instead of a mode** (cut 3, `harness`).
8. **Measure instead of build** (R-5c deferred until the DGX start time says otherwise).
9. **No campaign for an old contract** (PR 2, G-6): take a 3.1.0 run if it occurs, qualify 5.0.1 directly.
10. **Subtract the capability instead of guarding it** (cut 1): removing `commands` removed every execute grant, the `/dev/null` exemption, the per-call timeouts and `git-watch` at once. The largest simplification in the line, and the one that made I-1 a sentence a kernel can enforce.

**Still available, not adopted.** Each is a choice from §8.3 that could shrink further. None is for now; each names where it would go and what it costs:

| Candidate | Removes | Cost | Where |
| --- | --- | --- | --- |
| Drop the ledger seq from the `STATUS=` line | The contract sentence "never a clock", and the one place a ledger fact appears on the lifecycle socket | One operator convenience (`systemctl status` shows the seq) | 6.0.0, with V-1, since the 5.0.1 text now describes the seq. Recommended |
| ~~Fold `precheck` and `validate` into `judge --profile`~~ | Withdrawn 2026-10-09 (UDC pass 1, U-4): the architecture is already one judge; the names are vocabulary and removing them deletes nothing | — | Never |
| Publish the three axioms as the invariants' preamble | Nothing; it adds a reading. Keeps I-1..I-6 as the checks | A contract text change | 6.0.0 text, if the owner wants it published; otherwise this section is enough |
| ~~Derive `DAEMON_ERROR_CLEARED` and `DAEMON_STOP` in readers~~ | Withdrawn 2026-10-09: not derivable. An accepted cycle that observes no change writes nothing, so "the error ended" has no other witness; a clean stop is distinguishable from a crash only by its record (`UDC-PASS-1-ADJUDICATION.md` §3) | — | Never |
| One consumer surface instead of `status --verify-only` plus the copyable verifier | One command | The copyable verifier is required for diversity; `status` is the operator's. Keep both | Not a simplification; recorded so it is not proposed again |

The rule for all of them is the closing rule: nothing before the DGX, and anything that changes a published word rides 6.0.0 or never.

### 8.5 What the reviewer should verify, in this order

1. **A**, by reading the generated unit and `pathpolicy.grants()`: no execute anywhere, no network, one writable directory, and the unit's `InaccessiblePaths` agreeing with the Landlock grants (the agreement test).
2. **B**, by breaking the ledger: edit a middle record, truncate, replace the last record. The first two must be refused by both verifiers; the third must *not* be detected, and the contract must say so (I-4 "relative to the observed head"). Then starve `sense()` and watch blindness surface and the heartbeat stop.
3. **C**, by editing `daemon.py` after qualification and starting the unit: exit 78, never restarted. Then edit the skeleton and start: it runs. That is ADM-9, the one gap, and the 6.0.0 change.
4. Only then the choices (§8.3), asking of each: does removing it break A, B or C? If not, it is a candidate for §8.4; if so, it was forced and misfiled.

A reviewer who does these four things has reviewed the pattern. The documents are the record of how it got here; the three axioms are why.
