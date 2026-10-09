# Candidate register row for the kernel update: KU-33 (PD-54), an owner for resident components

- **Status:** CANDIDATE FOR THE KERNEL REVIEW. Drafted in the pattern repository for the owner to carry into `Kernel-Update` at Gate B. Nothing here is adopted; the register's rows keep `Owner decision: PENDING` until the owner records them.
- **Written against:** `xarendovich/Kernel-Update` at `ddec911` (register KU-01..32; `spec/next/KERNEL_CONTRACT_DRAFT.md` §6 owner map; `VERIFICATION.md` gates A–E), and this pattern at contract 5.0.0 (`2d080b407b6c82b230563670746f4a615b2c96660ec68f4509574562c282001d`).
- **Admission rule honoured:** the register says `ADD` "adds review/evidence work, not a kernel pillar, provider interface, or runtime mechanism". This row adds one informative owner-map line and one evidence pin. It adds no K2 requirement ID, no type, no interface, no daemon state to the kernel.

## 1. The row, in the register's format

| ID | Classification | Evidence or candidate problem | Recommended treatment / owner |
|---|---|---|---|
| KU-33 | ADD | KU-07 and KU-20 move process/lifecycle concerns out to adjacent owners, but the owner map does not name the owner or owning document for resident observe-only components. | Add one **informative owner-map row only**: resident components are owned by the daemon-pattern contract outside the Spark kernel; they are host-managed and observe-only; the Spark kernel holds no daemon lifecycle state and takes no runtime dependency on them. Any integration consumes typed verified facts through an external adapter rather than storage-format internals. No K2 requirement, pillar, provider interface, CLI dependency, daemon state or runtime mechanism is added. **Owner:** role "resident-components owner", holder `xarendovich` until explicitly delegated. Evidence pin S10 identifies the pattern contract/source; FirstBorn remains consumer evidence, not a kernel dependency. |

`Owner decision: PENDING`.

**Proposed §6 owner-map line** (replaces the generic "Observer / daemon / UI" entry's daemon half; the Observer and UI halves are unchanged):

| Owner | Responsibility | Exclusion |
|---|---|---|
| Resident components (observe-only daemons) — daemon pattern contract, S10 | Own their manifest, host confinement, local ledger, blindness, qualification and lifecycle; resident and host-managed. Integration, if present, exposes typed verified facts through an external adapter. | The Spark kernel holds no daemon lifecycle state, takes no runtime dependency on a daemon, grants no daemon authority, and does not depend on daemon CLI or ledger-file formats. |

**Proposed evidence pin for `EVIDENCE.md`:**

| ID | Source | What it supplies |
|---|---|---|
| S10 | `xarendovich/FIxed-Income-Calculator`, `spark-daemon-pattern/`. **Contract pin** 5.0.0 `2d080b40…` (unchanged since `7d128e1`). **Source pin:** the commit at which the owner carries this row — reviewed at `165aa17`; the deltas since `6ab59a0` are documents only, the contract bytes are identical. The contract digest is the identity; the commit pins the documents | The resident-component contract, its conformance battery, the independent verifier, the hardware gate (`HARDWARE-GATE-DGX.md`), and the pattern's own decisions deferred to the kernel (PD-54..62). SOURCE-INSPECTED by the owner; no runtime executed for the kernel review. |
| S11 | FirstBorn pilot (vendored copy at 3.1.0) — *the owner supplies the commit* | The first consumer; the G-6 hardware run. REPORTED until the evidence bundle exists. |

**The named owner, discovered from the repositories (2026-10-08).** The owner asked whether the repositories can say who the adjacent owner is. They can: neither repository has a `CODEOWNERS` or `MAINTAINERS` file; the pattern's decision log records every ruling under "Owner", singular; the kernel's `OWNER-DIRECTION.md` records "the owner instructed … in the operator session"; and both repositories belong to one GitHub account, `xarendovich`, which is the only human author in either (pattern: 4 commits, against 52 by Claude sessions; kernel: 1). So the adjacent owner is, today, that one person. **Recommendation:** the row names a *role*, "resident-components owner", and records its holder as `xarendovich`, so delegation later is a one-line change to the register rather than a re-reading of history. Adding a `CODEOWNERS` file to the pattern repository would make this discoverable by tooling rather than by inspection; that is the owner's choice, since it also routes GitHub review requests.

**Why this and not a kernel pillar.** KU-01 keeps six boundaries and "no new pillar". A daemon is not a seventh: it is a thing that lives *under* the kernel's floor, confined by the kernel of the operating system rather than by Spark's. The row places it there explicitly, which is what KU-07/KU-20 imply and never say.

**Cross-references the row should carry.** KU-14/KECC ↔ the pattern's `contract/versions.json`, pin test and field-by-field migration messages (a support inventory for contracts 1.0.0 → 5.0.0, which PD-39 asked for). KU-28/V-12 ↔ the pattern's adjudication → cuts → verification → hardware-gate sequence. V-09's "DGX/ARM64 … NOT RUN" ↔ `HARDWARE-GATE-DGX.md`: one device visit can serve both, recorded separately.

## 2. Non-normative integration analysis (not part of KU-33)

The material below records why the owner-map row is sufficient and where future integrations would belong. It is **not** proposed kernel-register content and does not authorize, sequence or define kernel-facing attachments. Those edges stay in their owning projects and contracts.

The kernel's six domains ("heads"): Orchestrator, Agent, Tool, Memory, Inference, State/Event. Below, for each: what attaches to the pattern today, which contract protects that edge, who can use it, and what is **not** built. The short answer is that the pattern's *internal* contracts are all built and tested; the *kernel-facing* edges are recorded as decisions and, with two exceptions, not built — which is the right order, since every one of them needs the owner named by KU-33 first.

| Head | What attaches today | The contract that protects it | Built? |
|---|---|---|---|
| **Orchestrator** | Nothing. The kernel never starts, stops or schedules a daemon; the host's systemd does, from a unit that only a qualifying report can produce, and only a person installs or enables it (I-5, PD-50: no zero-touch activation). | The unit as a projection of the report; the runtime's digest gate at every start; the hardware gate. | **Built** for start. **Recorded, not built:** withdrawal and revocation (U-5: drain within a bound, a revoked digest refused), the activation register (PD-40). |
| **Agent** | Nothing at runtime. In the other direction an agent may *author* a daemon: `describe → scaffold → envelope → precheck → validate → battery → stop` (DAEMON-CONTRACT §7). | The candidate envelope has no field for a result or an approval; producer kind is provenance only, never trust (`AUTHORITY`; PD-62). | **Built** (`spark-daemon-author`, DB-24, the authoring protocol). PD-62 as a *kernel* rule: recorded, pending. |
| **Tool** | A daemon **is not a Tool** and uses none. It runs no program (R-2, contract 5); it is resident, not invoked (P4: the L0 verbs `invoke`/`quiesce` do not fit it). Its only sensing surface is `ctx`: `read_text`, `list_dir`, `stat`, `disk_usage`, `now_utc`, `unsettled`, published in the contract with signatures. | The contract's `ctx` section; purity; the audit hook; Landlock (execute granted nowhere). | `ctx` is **built**. A **tool-head taxonomy does not exist** in either repository: the kernel draft defines Tool only as "executable capability boundary, not permission granted by a generated request" (and KU-29 removes concrete type names from Core); the pattern recommended X1's L0 as the place such a taxonomy is defined, with this pattern as pilot 0 (PD-46/47) and the Step 13A tool pilots beside it. **Not built, and not this pattern's to build.** |
| **Memory** | Never. A daemon cannot write Memory; `~/spark-core/data` is base-denied in every manifest and by Landlock. | The no-gaps rule (R-1), `PathPolicy`, the unit's `InaccessiblePaths`. | **Built** as a denial. Nothing to add; the kernel row should say "no Memory write" so it is a stated exclusion, not an accident of a path list. |
| **Inference** | Nothing at runtime (I-1: observe only). The one future path is a model *reading* a digest. | The digest is rendered contained (`render.py`, DB-10: hostile text cannot escape its structure) and stamped with the ledger head. How it reaches a model is PD-61: a user or tool turn, delimited, staleness-checked, never a system prompt. | Containment **built**. PD-61 as a kernel rule: **recorded, not built**. Until it exists no model should read a digest as evidence. |
| **State / Event** | The ledger. Readers attach through `status --json`, `status --verify-only`, the copyable `verifier/ledger_verify.py`, and `tools/daemon-start.py`; never by parsing the file themselves (N-19). | `spark-daemon-ledger/1` (append-only, hash-chained, canonical JCS, one writer); `spark-daemon-report/1`; `spark-daemon-status/1`; two independent verifiers (DB-22). The kernel draft's "ledger sequence is an ordering position, not a clock" is already the pattern's rule (`seq`, `timestamp_utc`, `boottime_ms` kept distinct). | **Built.** The kernel-side consumers are **not**: the activation register (PD-40), the off-host anchor (PD-58, residual risk 5), a shared exit taxonomy (PD-55; KU-20 says none in the kernel). |

**Who can use these, today:**

| Consumer | How | State |
|---|---|---|
| **FirstBorn** | A vendored copy under its own names, bound by the three-hash rename map (R-8b); 3.1.0 in use; migration to 5.0.0 listed in `REVIEW-PACKAGE-V5.md` §6. | **Built and in use** (the pilot). |
| **Spark Core** | **No attachment exists, by design on both sides.** The pattern denies `~/spark-core/data`; the kernel's SP-10 forbids model sessions reading `~/spark-core`; the kernel "takes no dependency on an observer daemon". A Spark-side reader of daemon facts would attach through State/Event (above) once PD-40/PD-58 give it a place. | **Not built; not designed.** Needs KU-33 first. |
| **An LLM operating in a Git repository** | As a **producer**: the authoring protocol, built. As a **reviewer**: the review packages, the evidence directories and the clean-checkout reruns, which is how r4.11 → 5.0.0 has been run. As a **reader of evidence**: PD-61, not built. As a **kernel-side consumer**: undefined until KU-33. | Producer and reviewer paths **built**; reader path **not**. |

**Scope of KU-33.** The row names an adjacent owner and nothing more. It does not gate or authorize PD-40, PD-58, PD-61/62, X1 taxonomy work, higher daemon classes, or any kernel-facing adapter. Each of those keeps its own trigger and owning contract. This prevents an owner-map clarification from becoming a hidden integration roadmap.
