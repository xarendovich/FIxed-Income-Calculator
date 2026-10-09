# Roadmap: what remains, and the kernel track

- **As of:** 2026-10-09, contract 5.0.1 candidate, digest `26e543fd8b36dd1d945be953c12d705c306adaa7cddf1cc2d24b893eff09dd16`.
- **Scope:** the active pattern is a small deterministic resident observer below the Spark kernel. It is host-managed, observe-only, runs no program, uses no network, and produces verified local facts without authority.
- **Kernel source:** `xarendovich/Kernel-Update` at `ddec911` remains the comparison point for §3 and §4. Its direction is to move daemon/process lifecycle concerns out to adjacent owners, not into the Spark kernel.

## 1. Where the pattern stands

| Deliverable | State |
| --- | --- |
| Published, versioned, test-pinned contract | **5.0.1 candidate.** Wording-only consolidation of 5.0.0; no enforcement rule changes |
| Evidence path a producer cannot shortcut | **Done.** One facts-only report; verdict/qualification derived on read; unit is a projection; activation stays separate |
| Observe-only runtime boundary | **Done.** No child programs, no network, one `PathPolicy`, Landlock + systemd enforcement |
| Ledger verification | **Done relative to the observed head.** Two independent verifiers, one interpreter; completeness across observations requires an external head receipt |
| Independent software verification | **Partly done.** Static/structural work was independently checked; Landlock-dependent reruns were reproduced by the code author and remain subject to the DGX pass |
| Hardware qualification | **OPEN.** `HARDWARE-GATE-DGX.md` |
| Higher daemon classes / event registry / model-in-daemon | **Not built and not on the current line.** Contract 5.x remains `daemon_class: observe` |

Exact self-test and battery counts live in immutable evidence records. General architecture documents do not duplicate mutable totals.

## 2. What remains, in order

### A. Consolidate and software-freeze 5.0.1

1. **Finish the 5.0.1 wording-only consolidation.** The patch:
   - changes I-4 from an absolute source-of-truth claim to a chain verified **relative to the observed head**;
   - states that completeness across observations needs an anchor outside the daemon output directory;
   - replaces the ambiguous current-use `human Class C` label with **owner activation decision** while preserving historical records;
   - records the `sd_notify` boundary: READY/STATUS/WATCHDOG/STOPPING are host lifecycle/operator status only, not observation, grant, outcome, authority or clock;
   - corrects stale cycle-budget text left from the removed per-call timeout design.
   This is a PATCH release because no daemon that satisfied 5.0.0 fails 5.0.1.

2. **Close E-4 as intentional layered asymmetry.** One canonical `PathPolicy` owns policy. If Python proves a resolved path is explicitly forbidden, exit 78 is a policy violation. A generic `EACCES` is not automatically reclassified because the runtime may not know whether Landlock, Unix permissions, a mount or another host condition caused it. Do not add a mechanism just to force identical error labels from different enforcement layers.

3. **Keep reviewer requirement sets non-normative.** ADM/STB/LAT are useful review matrices, not a second contract hierarchy. Map findings back to I-1..I-6, hardware evidence, or deferred work. In particular ADM-9 is V-1 and strengthens I-5; it does not become I-7.

4. **Defer convenience/security machinery without a present trust boundary.**
   - **V-2 signed reports:** trigger only when a qualifying report crosses a trust boundary or becomes input to automated activation.
   - **V-3 `unit --report --check FILE`:** no critical-path work; deterministic regeneration plus digest/diff already proves the fact.
   - **Independent hand-written JSON parser:** do not build one merely for diversity. Keep bounded records, normalized parser failures, independent canonical re-encoding and differential fuzzing; reopen only if another concrete common-mode parser fault cannot be contained this way.
   - **R-5c checkpoint acceleration:** measurement only; reopen only if DGX projection exceeds half the start timeout.

5. **R-8 neutral packaging: trigger met, implementation later.** FirstBorn is a real second project consumer. Keep vendoring through the hardware gate, then define neutral/configurable package, CLI, environment and unit names before finalizing the contract-6 runtime bundle layout. Do not mix packaging changes into 5.0.1 qualification.

### B. Qualify the current target on the DGX

6. **Do not create a separate 3.1.0 campaign.** If the existing FirstBorn 3.1.0 `pressure-watch` pilot naturally comes up during `fb update`, preserve its G-6 evidence as a useful preliminary host/systemd proof. If it is not already present, do not spend a separate qualification cycle installing an old contract merely to remove it again.

7. **Migrate FirstBorn directly to 5.0.1.** Skip 4.0.0 and 5.0.0 as deployment targets; the 5.0.1 change is wording-only relative to 5.0.0.

8. **DGX-qualify the exact 5.0.1 baseline.** On the DGX, as the service user:
   - run the full current self-test suite and all three reference batteries;
   - create the installable unit from a qualifying report;
   - run it under the real host systemd, with systemd as PID 1;
   - capture the evidence bundle in `HARDWARE-GATE-DGX.md`;
   - collect the R-5c verification-cost measurement without changing the architecture.

Use two status concepts, not one overloaded word:
- **SOFTWARE BASELINE FROZEN** = exact contract/code bytes fixed for qualification.
- **DGX QUALIFIED** = that exact baseline passed the hardware gate.

### C. Next major admission change after hardware qualification

9. **V-1 becomes contract 6.0.0 and strengthens I-5.** Bind a `runtime_bundle_sha256`, not an arbitrary repository tree. The bundle covers the exact trusted code that can affect admission, confinement setup, ledger writing, interpretation and verification — including the runtime entrypoint, `spark_daemon/`, and the independent verifier. Tests, evidence, README/PDFs, vendoring and authoring tools are outside the trusted runtime bundle unless the 6.0.0 design proves otherwise. Reuse the existing deterministic tree-hash mechanics; do not invent a second hashing framework.

10. **PD-58 is a consumer-side attachment property, not a new daemon service.** A consumer that records a verified head in a storage/authority domain the daemon cannot modify establishes the first externally held checkpoint. That receipt can detect rollback/truncation **after** the checkpoint; it cannot retroactively prove pre-attachment completeness. Do not build an anchor daemon, anchor protocol or new daemon invariant.

### D. Deferred with explicit triggers

- **Event-type/L2 registry:** no registry now. Current consumers may cite one implementation by `(manifest digest, daemon-code digest, event type)`; after 6.0.0 include the runtime-bundle digest. Build a shared event contract only when two independently qualified implementations intentionally emit the same semantic observation and a consumer needs to switch between them.
- **Advise/Act classes:** taxonomy only. Contract 5.x/6.0 remains observe-only. If a model is involved, it is an external agent consuming verified facts, not code inside the resident daemon.
- **Command-backed extension:** outside the generic core; only when a concrete consumer such as the Repository Observer requires it, under its own boundary.
- **R-5c:** as above, only after the measured trigger.

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
