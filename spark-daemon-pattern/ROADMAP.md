# Roadmap: what remains, and the kernel v2 track

- **As of:** 2026-10-08, contract 5.0.0 at `6aa8671` (HF-43/44 fixed, verification pass done).
- **Sources.** This repository (the pattern) and, since 2026-10-08, the kernel repository `xarendovich/Kernel-Update` at `ddec911` (2026-10-06, "Merge pull request #2"), read in full: its README, the 2026-10-04 review register (KU-01..31, V-01..12, gates A to E), the 2026-10-06 owner direction (KU-32), `spec/next/`, and `spec/historical/`. §3 and §4 are written from that source. Under the kernel's SP-10 rule no model session reads `~/spark-core`; nothing here does.

## 1. Where the pattern stands

| Deliverable | State |
| --- | --- |
| A published, versioned, test-pinned contract | **Done.** 5.0.0, `2d080b40…`, recorded in `contract/versions.json`; `make check-contract` fails if the published files drift from the enforced rules |
| An evidence path a producer cannot shortcut | **Done.** One facts-only report; verdict and qualification derived on read; the unit is a projection of a qualifying report; the runtime refuses anything else |
| The authoring boundary fixed before the authoring subsystem exists | **Done.** DAEMON-CONTRACT §7: `describe → scaffold → envelope → precheck → validate → battery → stop`; the envelope carries no authority; producer kind is provenance only (PD-62) |
| Six invariants, each enforced in one place with its checks and tests named | **Done** (E-8); `test_invariants.py` holds them |
| Independent verification | **Partly done.** The first reviewer verified source, chain, contract, inventory and the static suites; the Landlock-dependent half was reproduced here (not independently) on Python 3.12.3/3.13 and from a clean checkout |
| Hardware qualification | **OPEN.** `HARDWARE-GATE-DGX.md` |

## 2. What remains, in order

### Now: the software freeze decision (owner)
1. **Freeze 5.0.0 with HF-43/HF-44.** The fixes change no contract rule; the identity is unchanged. (`REVIEW-PACKAGE-V5.md`, `VERIFICATION-V5.md`.)
2. **Residual 5 / PD-58 wording.** Reword I-4 as "verified two ways *relative to the head*" in **5.0.1** (a text change, so a new contract digest), and decide PD-58, the off-host anchor, which is the only thing that closes the gap.
3. **Residual 6 / V-1.** Bind a skeleton tree digest (`spark_daemon/`, `verifier/`) as a fourth expected digest, so a post-qualification edit to the enforcing code is refused at start. A qualification-contract change: **5.1.0** or **6.0.0** depending on whether existing units must be regenerated (they must, so likely 6.0.0).
4. **E-4**, **V-2** (signed reports), **V-3** (`unit --report --check FILE`), **R-8** (neutral names/packaging, which needs the owner's ruling on whether the pilot is the second consumer).

### Next: the hardware gate (operator, with the DGX)
5. **G-6 at 3.1.0** with the FirstBorn `pressure-watch` pilot — recommended now, nothing waits for v5. Closes the basic "does this work under the real kernel and host systemd" question.
6. **FirstBorn migrates directly to contract 5** (`REVIEW-PACKAGE-V5.md` §6 is the migration list; the pilot skips 4.0.0 as adjudicated).
7. **Contract 5.0.0 qualified on the DGX**: battery run on the DGX as the service's user, unit from `unit --report`, evidence bundle per `HARDWARE-GATE-DGX.md` §4. **R-5c measurement** collected in the same visit.

### Then: the second independent pass (reviewer)
8. The ranked targets in `ADVERSARIAL-REVIEW-HANDOFF-V5.md` §4, led by the JSON parser as a common-mode fault behind both verifiers (HF-43 showed one; the hand-parse option is open), post-qualification skeleton tampering (V-1), and an audit-hook escape that the kernel must still contain.
9. Rerun the 265-test suite and the three batteries on the DGX environment — the one run no container can stand in for.

### Deferred, with recorded triggers
- **R-5c** (checkpointed start-up verification): only if the DGX projection exceeds half the start timeout.
- **R-8** (rename the core): only with a second real consumer.
- **`daemon_class: act`, `network.mode: named`**: reserved through kernel v2 (PD-60); each needs its own adjudication against its recorded preconditions.
- **A command-backed extension** (the Repository Observer's need, PD-72): not in the generic core; commands would run outside the confined process.

## 3. The kernel track, from the kernel repository

**What the kernel repository is.** Not kernel v2 shipped: a *smallest-stable-foundation boundary pass* over the `v0.2.1-review` text, status **PROPOSED FOR REVIEW**, "no Class C adoption, runtime implementation, activation, or release freeze". Its own words: kernel work is "adjacent to the capability roadmap, not another numbered step", and the release identity is deliberately unnamed ("use 'next kernel review draft' until the owner selects the release identity", KU-30). So "kernel v2" has no date in that repository either; it has gates.

**Where it stands, by its gates** (`review/2026-10-04/VERIFICATION.md` §3; `review/2026-10-06/OWNER-DIRECTION.md`):

| Gate | What it is | State at `ddec911` |
| --- | --- | --- |
| A — review preparation | inspect source and history, classify, publish the bounded kernel/KECC drafts and the review plan | **Done** (2026-10-04): register KU-01..31, drafts in `spec/next/` |
| B — reviewer and owner disposition | reviewers use the fixed candidate; the owner records keep/modify/add/move-out/remove/defer | **Directed, unsigned** (2026-10-06): "go with the recommendations"; D2 adopted, D1 as an owner-profile first, KU-01..31 accepted as classified, KU-32 added and done. Every row still reads `Owner decision: PENDING` until the owner records the disposition itself |
| C — complete contract build | reconcile the full S4 baseline, apply only accepted deltas, select the release identity | **Open; blocked on the owner's own capture**: the real runtime SHA, the Step 8.5 freeze record and the full S4 baseline inventory (K-3) |
| D — implementation only where needed | amend types, adapters or tests in the owning repository where an accepted requirement actually changes them | Not started |
| E — qualification and release | isolated regression/replay/provider tests; device-only tests on the device; owner sign-off, exact release identity | Not started. "DGX/ARM64/native ordering, resource and stop-signal qualification: **NOT RUN**" |

**What it says about daemons.** The direction of the whole register is *shrink and move out*. Three rows touch this pattern directly:

- **KU-07 (MOVE OUT):** process exit, slot states, incarnation, binding ids, `admit()`, L0/L2 and channel grants leave the kernel text for the transport/policy/host/X1 profiles.
- **KU-20 (MOVE OUT):** lifecycle states, sticky failures, root policy, signal safe points and the local exit table "remain Observer WBS 3.1A and adjacent owners' contracts. No shared daemon exit code table" in the kernel.
- **KU-21 (REMOVE):** the proposed criterion that WBS 3.1A is done once an export is *named* is deleted; a live export is required.

And the historical line it inherits: "the kernel MUST NOT take a dependency on … an observer daemon"; "the observer daemon may finish WBS 3.1a against the constraints"; importing the observer daemon as a kernel module is a non-goal.

**The material finding for this pattern.** The register contains **no row** that creates the slot this pattern was told to expect: no "daemon authoring" slot (PD-31), no WBS number or owner for resident components (PD-54), and no mention of the pattern, `spark-daemon`, PD-54..62, or a hardware gate. The kernel is not absorbing daemon concerns; it is pushing them to "adjacent owners". That is **compatible** with PD-54's own proposal ("an observe-only plane *below* the fixed kernel; the skeleton in the fixed tier; manifests and `daemon.py` pluggable") and with KU-20's "adjacent owners' contracts" — but compatibility is not a decision. Nobody has yet been named as that adjacent owner, and PD-54..62 appear in this repository only.

**What that means for the recorded order.** PD-54 (an owner and a slot) still gates the rest, and it now has a concrete home: it should be raised as a row in the kernel register (an **ADD**, in the register's vocabulary, naming the resident-component plane as an adjacent owner below the kernel), or else recorded as *out of the kernel's scope by design* with this pattern's own contract as the owning document. Either is a decision the owner records at Gate B; neither is implied by the current register.

Two of the kernel's own needs line up with work already done here, and should be cross-referenced when PD-54 is raised: **KU-14/KECC** (support inventory and a shrinking-horizon rule) is what `contract/versions.json`, the pin test and the migration messages already do for daemon contracts 1.0.0 → 5.0.0 (PD-39 asked for exactly this); **KU-28/V-12** (explicit review → disposition → consolidation → implementation → deployment gates, with code identity and tested environment recorded) is the shape of this pattern's adjudication → cuts → verification → hardware gate.

## 4. On track?

**For what the pattern owes the kernel line: yes, and ahead of it.** The pattern's deliverable was a published, test-pinned contract and an evidence path a producer cannot shortcut, with the authoring boundary fixed in advance. That is delivered at 5.0.0. The kernel repository, read in full, asks for nothing from this side that is missing, and its direction (move daemon lifecycle out to adjacent owners) is the direction this pattern already took.

**For the kernel line itself: Gate A done, Gate B directed but unsigned, Gate C blocked on the owner's own capture, D and E not started.** There is no schedule in the kernel repository to be on or off track against; there are gates, and the next two are the owner's.

**The one thing that is not on track, because it is on nobody's list:** the slot. PD-31 said the daemon subsystem "will have a spot by kernel v2". The kernel register does not give it one, and by its own design would not. **Recommendation:** raise PD-54 as a register row at Gate B (an ADD naming the adjacent owner), or record the alternative explicitly. **Drafted:** `KERNEL-REGISTER-CANDIDATE-KU-33.md`, in the register's own format and under its admission rule, with the per-head attachment review. Until then the pattern is a well-reviewed thing with no named place in the kernel's map.

**The DGX visit serves both lines.** The kernel's Gate E device qualification ("DGX/ARM64/native ordering, resource and stop-signal") and this pattern's hardware gate are both NOT RUN and both need the same machine. They are different tests and must be recorded separately, but one visit can be planned to do both.
