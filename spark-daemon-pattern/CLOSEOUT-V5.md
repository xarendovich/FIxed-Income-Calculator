# Closeout: the Spark daemon pattern at contract 5.0.0

- **Date:** 2026-10-09. **Source:** this pattern at `dceb568` (contract 5.0.0, `2d080b407b6c82b230563670746f4a615b2c96660ec68f4509574562c282001d`, skeleton 0.5.0, manifest schema 4); `Kernel-Update` at `ddec911` (PR 2 merged 2026-10-06); `X1-registry` at `a018c4e`; the FirstBorn pilot at vendored 3.1.0.
- **The owner's instruction:** review Kernel-Update PR 2 against the current work and close the process out. "The architecture has been completed. We no longer need to grow it."
- **What this document is:** the review of PR 2 (§1, §2), the statement of what is complete (§3), the closing rule that stops growth (§4), the index that says which documents are live and which are records (§5), and the owner's exit list (§6). It adds no mechanism, no document class and no decision ID.

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
| A published, versioned, test-pinned contract | 5.0.0; `make check-contract` fails on drift; support history 1.0.0 → 5.0.0 in `versions.json` with field-by-field migration messages |
| An evidence path a producer cannot shortcut | One facts-only report; verdict and qualification derived on read; the unit a projection of a qualifying report; the runtime refuses anything else at every start |
| Confinement | Landlock required at every start; execute granted nowhere; one `PathPolicy` projected three ways with an agreement test |
| The ledger | Append-only hash chain, canonical JCS, one writer, two independent verifiers, cross-checked; a corrupt ledger refuses and changes nothing |
| Six invariants | Published in the contract, each naming its checks and tests, held by `tests/test_invariants.py` |
| The authoring boundary | Fixed before any authoring subsystem exists: describe → scaffold → envelope → precheck → validate → battery → stop; the envelope carries no authority |
| Self-tests and batteries | 270 test methods in the tree; 264 and then 270 passed in the recorded runs (`VERIFICATION-V5.md`, `evidence/v5/`), on Python 3.12.3 and 3.13, Landlock ABI 7, from a clean checkout; three reference batteries, 21 PASS and DB-24 N/A each. **A count in a tree is not a pass; the runs are the evidence** |
| Independent review | Verification pass (HF-43, HF-44 fixed), adversarial handoff, alignment review, class-requirements review, three reviewer rounds on placement and the DGX gate, all adjudicated with the evidence stated |
| Hardware qualification | **Not run.** The one deliverable that needs a machine (`HARDWARE-GATE-DGX.md`) |

The architecture is complete in the sense the owner means: every mechanism the pattern needs exists, is tested, and is named by an invariant. What is not complete is evidence that only a machine can give, and decisions that only the owner can make.

## 4. The closing rule

From this commit the pattern is **closed to growth**. It accepts exactly four kinds of change, and a change that is none of them is refused with a pointer to whose it is:

1. **A defect fix** with a reproduction and a regression test, recorded as the next HF row in `HARDENING.md`. Contract identity unchanged unless the fix changes a rule.
2. **The 5.0.1 wording bundle**, one digest bump, no rule change: I-4 "relative to the head"; the Class C split once the owner supplies the transport-contract wording it collides with; the notify invariant (`ROADMAP.md` §2).
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
| A new review document | Closed. A finding goes into `HARDENING.md`; a decision goes into the owner's register |

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

1. **Freeze 5.0.0** with HF-43/44, as it stands at `dceb568`.
2. **Open the pull request for the branch.** Forty-four commits and contracts 3.0.0 through 5.0.0 exist only on `claude/clever-bardeen-qqkxez`; `main` carries 2.0.0. Every cross-repository pin to the pattern names commits `main` does not have.
3. **Carry KU-33 to Gate B**, adding two citations from X1: `A12.DAEMON_LEDGER` (the identity X1 already holds) and X1-1A (the shared start-up rule).
4. **Record the three confirmations** already asked for: V-1 is the 6.0.0 change after the freeze; the first consumer attachment is PD-58's anchor; owner-ruling records stay as records.
5. **Supply the transport-contract wording** the pattern's "Class C" collides with (the `utc/` note the class-requirements reviewer cited; the commit is in neither repository this session reads), so the 5.0.1 bundle can be cut as one change.
6. **The DGX visit**: G-6 at 3.1.0 with the pilot, then migration to 5.0.0 and the v5 battery as the service's user; one visit can also serve the kernel's Gate E, recorded separately.
7. **Close PD-51** on the reviewers' markup; D now is the recommendation, C when two implementations emit one observation.
8. **`CODEOWNERS`** or not: X1's own closeout recorded a PR merged through a documented HOLD and named mandatory review on protected `main` as the remedy. This repository has one reviewer and the same exposure.

After item 1 the package is frozen; after item 2 it is on `main`; after item 6 it is qualified. Nothing else grows.
