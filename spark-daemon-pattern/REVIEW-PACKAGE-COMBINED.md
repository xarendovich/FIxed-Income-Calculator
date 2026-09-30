# Spark Daemon Pattern: review and decision package (r4.4)

> **Update (r4.5):** the items below are now adjudicated in `ADJUDICATION-R4.5.md`. PD-70's clock basis is built (HF-34), and the battery has a new check, DB-20 (HF-35). The implementation-status table below is as of r4.4.

- **Status:** PROPOSED FOR INDEPENDENT REVIEW. NOT ADJUDICATED. It authorizes no LTC version change, no daemon class unlock and no activation. Where it recommends amending a settled ruling, the owner decides.
- **Date:** 2026-09-30. Prepared by Claude for the owner, to hand to other reviewers (people or models).
- **Repository state:** `spark-daemon-pattern/` at revision r4.4, on branch `claude/clever-bardeen-qqkxez`:
  - 205 of 205 self-tests pass;
  - the four reference daemons pass 18 of 18 conformance-battery checks on the build workspace (Landlock ABI 7);
  - the daemon contract is 3.0.0.
- **This document stands alone.** It combines the review handoff (`HANDOFF-REVIEW-PACKAGE.md`) and the decision brief (`DECISION-BRIEF.md`). Every claim names the file in the repository that holds the detail, for reviewers who can read it. A reviewer without the repository can still mark every item.

**What is inside:**
- **Reviewer focus (read first).** For every decision: what is built and where, what remains, and the new seams the remaining work would create.
- **Part 1: the review handoff.** What the pattern is and what is built; the owner's settled rulings; the three review packages and their decision registers:
  - Package A: the Local Transport Contract;
  - Package B: Break Glass and operator-gated recovery;
  - Package C: other open decisions.
- **Part 2: the decision brief.** The settled rulings revisited (none reversed; one amendment or clarification each), and every open decision with its options, pros, cons and a recommendation, in a suggested order.
- **Part 3: one markup table** for everything above, and the document map.

Section numbers restart in each part; "Part 2 §2.4" means section 2.4 of Part 2.

## Reviewer focus: what is built, what remains, and the new seams

Read this section first. It says, for every decision in this package:
- what is implemented today and where in the code;
- what remains to be built;
- which **seams** the remaining work would create.

A seam is a new boundary where two parts meet and each assumes something about the other.

**Why the seams deserve the most attention.** Every defect found since r3.5 sat at a seam, where two limits or two components were each correct on their own and wrong together:

| Defect | The seam it sat on |
| --- | --- |
| HF-28 | The runtime's exit codes and the unit's restart policy: `SENSE_BLIND` was restarted forever |
| HF-30 | A daemon's worst-case sense time and its watchdog period |
| HF-31 | The unit's stop timeout and the length of one cycle |
| HF-32 | Process restarts and the blind clock |
| HF-33 | The skeleton's `ctx` API and daemon authors: a truncated listing was returned as a value and diffed as complete |

Reviewers should test each seam below with the question: "What does each side assume about the other, and what happens when that assumption is false?"

### Implementation status

Status labels:
- **Built:** in code, with tests.
- **Partial:** some of it is in code.
- **Designed:** recorded in documents only.
- **Proposed:** new in this package.

Code paths are under `spark-daemon-pattern/`.

| Decision | Status | What exists today (where) | What remains |
| --- | --- | --- | --- |
| **PD-63** Blind period (settled) | **Built** (r3.5) | `ctx.unsettled` (`spark_daemon/context.py`); `blind_limit_seconds` (`spark_daemon/manifest.py`); the blind clock `_Blind` and `SENSE_BLIND` exit 78 (`spark_daemon/runtime.py`); `RestartPreventExitStatus=2 65 73 78` (`spark_daemon/unitgen.py`) | **Proposed amendment:** split exit 78 between `EXIT_POLICY` and `EXIT_SENSE_BLIND` (`spark_daemon/__init__.py`, the unit, the contract's exit table). **Designed:** where `T` is set, deferred to PD-40 |
| **PD-70** Blindness across restarts (settled) | **Built** (r4.0) | `_AcceptEvidence` reads the verified ledger at start-up; `DAEMON_HEARTBEAT` every `T`/2; one reacquisition cycle (`spark_daemon/runtime.py`); four tests (`tests/test_blind.py`) | **Proposed amendment:** measure on `CLOCK_BOOTTIME` within a boot (`boot_id` and boot time in the heartbeat, start and stop records); assume the worst across boots; add a test for a backward clock step |
| **PD-01.9** Blind Forester (settled) | **Partial** | The contract names the slot: fail closed for observers (`spark_daemon/contract.py`, `cycle.blind_forester`) | Everything for active classes: the survival loop in the supervisor (BF-1), marked outputs (BF-2), validity windows (BF-3), pre-authorized actions (BF-4), recovery exit (BF-6). **Blocked on** PD-72. **Proposed:** the scope clarification |
| **PD-76** Direct memory reading (settled) | **Designed**; feasibility **measured** | DB-14 still decides on a per-process peak (`spark_daemon/battery.py`, `db14`). The feasibility probe is in `evidence/r4.4/cgroup_direct_peak.py` | DB-19: spawn the daemon inside a fresh cgroup before it runs, read v2 `memory.peak` or v1 `memory.max_usage_in_bytes`, record the version, retire DB-14. The CI rule for `INCOMPLETE` |
| **HF-33** Capped listing | **Built** (r4.3), for `dir-watch` only | A listing over the cap is a failed cycle (`examples/dir-watch/daemon.py`); three tests (`tests/test_blind.py::DirWatchCapacityTests`) | The framework-level rule is PD-83 |
| **PD-82** Bind, do not embed | **Proposed** | Nothing | The manifest field for declared connections; register fields; a battery hook for the LTC suite |
| **PD-83** Truncated reads raise | **Proposed** | `ctx.read_text` already raises (`spark_daemon/context.py`). `ctx.run`, `ctx.git` and `ctx.list_dir` return truncation as a value | Change the three methods; add an opt-in partial result type; contract 4.0.0 |
| **PD-84** Register vocabulary | **Proposed** | The battery report binds the manifest, contract and code digests (`spark_daemon/battery.py`) | The PD-40 register itself, with LTC binding fields |
| **PD-85** Declared pacing | **Proposed** | Uniform random jitter, ±min(5 %, 30 s), zero in test mode (`spark_daemon/runtime.py`, `_jitter_bound`) | A `pacing` manifest field; deterministic spread by default |
| **PD-86** Retry bounds over time | **Proposed** | The unit's start limit, 5 in 300 s (`spark_daemon/unitgen.py`) | A lifetime bound per retrying layer; a battery cross-check |
| **PD-01.10 to PD-01.14** Break Glass and recovery | **Designed** (`BREAK-GLASS.md`) | Nothing | All of it. **Blocked on** PD-72, PD-01.7, PD-40 and a heartbeat acknowledgement path |
| **BF-1 to BF-6** | **Designed** | — | See PD-01.9 |
| **PD-72** Supervisor/worker split | **Designed** (`ADJUDICATION-AP.md`, AP-04) | Nothing | A supervisor process with its own Landlock domain; a per-cycle worker deadline |
| **PD-40** Activation register | **Designed** | Candidate envelopes and battery reports carry the digests it would bind | The register, revocation (U-5), evidence validity periods (U-7) |
| **PD-01.3** Sentinel | **Designed** | Heartbeats already expose `mode: blind` | `SENSE_DEGRADED` at `T`/2, `spark-escalation/1`, a read-only escalations command |
| **PD-69** Limits checked against each other | **Partial** | Individual cross-checks are tests: the git-watch sense budget against its watchdog, the stop timeout against one cycle, the restart policy (`tests/test_blind.py`) | The per-daemon budget table in DB-15 (Phase 1) |
| **PD-71** (remainder) | **Designed** | One small fixture per daemon | Named worst-case fixtures; a scaling series to derive `N_max` |
| **C-1, PD-75 to PD-81** | **Designed** | The battery exits PASS 0, INCOMPLETE 3, FAIL 1 (`spark_daemon/__init__.py`, `EXIT_FLAGGED = 3`) | The code change with contract 4.0.0; importing and reconciling the v0.1 line's registry (PD-78) |
| **Independent ledger verifier** (B-6, U-7) | **Designed** | DB-04 and DB-16 use the skeleton's own verifier | A verifier that does not import `spark_daemon` |
| **Phase 1** | **Designed** | — | An `OnFailure=` notifier; a diverse staleness checker; a budget report |

### New potential seams

Each seam is introduced by the decision named. Each entry states the assumption that could fail and what to check.

| # | Seam (introduced by) | The assumption that could fail | What a reviewer should check |
| --- | --- | --- | --- |
| **N-1** | **Supervisor and worker** (PD-72, BF-1) | The supervisor keeps a survival loop running while the worker is blind or dead. But it shares the host, and under LTC-14 it must not fabricate the worker's results | Are survival outputs the supervisor's *own*, marked records? Does exit 78 still mean "never restart" for the worker, while the supervisor never exits blind? Does each side have its own Landlock domain? |
| **N-2** | **Daemon and activation register** (PD-40, PD-49, PD-84, PD-63's option B) | The register binds exactly what the daemon runs | Which digest is compared (file bytes or canonical JSON)? Is a trust result checked for age and revocation? If `T` moves to the register, is its value recorded in `DAEMON_START`? |
| **N-3** | **Declared connection and LTC provider** (PD-82, LI-2, LI-11) | The manifest's bounds and the LTC profile's bounds are the same numbers | Is there one source for each bound (U-8)? Does the battery run the LTC suite against the *bound* provider? |
| **N-4** | **Battery and cgroup** (PD-76, DB-19) | The daemon is inside the cgroup from its first instruction, and readings from cgroup v1 and v2 are comparable | Is the daemon placed in the cgroup before `exec`, as the probe does? v1 counts page cache differently, so is the report's cgroup version compared only with a limit for the same kind of host? The battery gains a privileged step: what if it fails halfway? |
| **N-5** | **Ledger and the external staleness checker** (Phase 1, A-2, U-7) | "The ledger file's age" measures liveness | File age compares a modification time with the wall clock, so it has the same clock-step weakness as PD-70. Prefer a checker that notes the file's size or hash and checks for change on its *own* monotonic clock |
| **N-6** | **Heartbeat and orchestrator acknowledgement** (Break Glass trigger, BG-1) | An acknowledgement is only ever read | This is the first *inbound* path into a daemon that is pull-only today. It must never carry commands, and its absence must be one of two witness signals, not a trigger on its own (BG-12) |
| **N-7** | **Break-glass relay and external system** (BG-5, BG-10) | Every command to the target passes the one gate that enforces "highest epoch wins" | Is `single_path: true` verifiable, or only declared? What does the external system do with an out-of-epoch command that reaches it by another path? |
| **N-8** | **Witness and supervisor** (BG-12, BG-13) | The two signals are independent, and the epoch copies agree | Do the two signals share a clock, a network link or a host? On a single DGX, is a separate process tree enough? |
| **N-9** | **Flight recorder and daemon ledger** (BG-15 to BG-17) | Two separate chains can be reconciled and verified independently | Is the seal's head hash anchored somewhere the supervisor cannot rewrite? Is the independent verifier used for both chains? |
| **N-10** | **The `ctx` API and daemon authors** (PD-83) | An opt-in partial result keeps its flag all the way into the records built from it | Can the flag be lost by copying fields into a new dictionary? Should the record builder refuse a partial value that has lost its marker? |
| **N-11** | **Pacing and watchdog** (PD-85) | A phase offset never delays a watchdog ping | The runtime pings the watchdog during the sleep, independently of the offset. Confirm this holds for `FIXED_PHASE` and for offsets close to the interval |
| **N-12** | **One exit-code table and its four consumers** (PD-63's split, PD-35, C-1) | The runtime, the unit's `RestartPreventExitStatus`, an `OnFailure=` notifier and the Observer's table agree | Is there a single table from which all four are generated, and a test that fails if one of them drifts? |
| **N-13** | **Contract 4.0.0 and existing manifests** (the batched change) | Every author migrates once | Is there a schema-3 migration hint, like the one from version 1 to 2? Do the battery check IDs change meaning (PD-78: retire and replace, never edit in place)? |
| **N-14** | **Survival mode and emergency authority** (PD-01.9 and PD-01.12) | A restart resumes survival mode (PD-70) and ends break-glass authority (BG-9) | Can any record written in survival mode be read at start-up as authority? The two must use different record types |

## Part 1: Review handoff

### 0. How to review

1. **Mark each item** in the decision registers (Part 1 sections 3.5, 4.4 and 5, and Part 2 sections 1 to 3): **APPROVE**, **MODIFY**, **REJECT** or **DEFER**, with notes. Part 3 has one blank markup table for both parts.
2. **Owner rulings are settled** (section 2). Do not reopen them. Their *implementation conditions* are open, and are marked as such.
3. **The three packages are separate.** Review them separately, and do not move an identifier from one to another.
   - **Package A:** the Local Transport Contract (LTC) and the daemon pattern.
   - **Package B:** Break Glass and operator-gated recovery.
   - **Package C:** other open decisions.

   The LTC hardening markup requires that Break Glass "remain a separate review package and not silently consume" LTC placeholder numbers. This handoff keeps that rule.
4. **Evidence standard.** A finding counts when it is reproduced, measured or traced to code. Anything else is recorded as an opinion. Say which yours is.
5. **Identifier namespaces**, so that nothing collides:

| Prefix | Owner | Meaning |
| --- | --- | --- |
| PD-nn, PD-01.n | This daemon pattern | Pending decisions. PD-75 to PD-81 are aliases of the parallel v0.1 line's PD-23 to PD-29 |
| HF-nn | This daemon pattern | Defects found, reproduced and fixed, each with a test |
| S-n, U-n, BM-n, BF-n | This daemon pattern | Safety invariants, universal-contract rules, blind modes, Blind Forester conditions |
| BG-n, OR-n | This daemon pattern (Package B) | Break Glass and recovery requirements. **Not LTC numbers** |
| LI-n | This daemon pattern (Package A) | LTC integration requirements. **Not LTC numbers** |
| LTC-nn, LTC-V2-nn, CT-nn | The LTC | The LTC's own requirements and conformance tests |
| LTC-H01 to H03 | The LTC hardening markup | Placeholders until the LTC adjudication assigns numbers |

### 1. What the daemon pattern is, and what is built

A **daemon** is a long-running Spark process. The pattern has three parts:
- **A closed-schema manifest** declares everything about the daemon's safety: what it reads, which commands it may run, resource limits, watchdog, blind limit and ledger rules.
- **A fixed skeleton** supplies every safety mechanism. The author writes only `sense` (read), `decide` (pure) and `digest` (pure). The skeleton provides:
  - a purity check;
  - a Python audit hook;
  - Landlock file-system and network confinement;
  - a generated systemd unit with a sandbox, watchdog and restart policy;
  - a hash-chained, append-only JSONL ledger (JCS canonical form, one fsync per record);
  - a bounded Markdown digest.
- **A conformance battery** runs the real daemon through 18 checks (DB-01 to DB-18) before anyone may activate it.

**Only class 1a (observe and record) exists.** Activation is a human Class C decision; a battery PASS is evidence for it, not permission. Nothing here installs or starts a service.

**Built since the r3 baseline** (all tested):
- **32 defects fixed, each with a regression test** (HF-01 to HF-33; HF-23 is a missing-evidence note, not a defect), including two confinement escapes and a Git code-execution path. Three (HF-28, HF-30, HF-31) were found by reading and arithmetic; the rest were reproduced by running.
- **The blind period.** A daemon that accepts no observation for `blind_limit_seconds` (`T`) stops with `SENSE_BLIND` (exit 78) for a human, and 78 is never restarted.
- **Blindness survives restarts** (PD-70). The clock starts at the ledger's latest evidence of an accepted cycle. A `DAEMON_HEARTBEAT` is written every `T`/2, and one reacquisition cycle is allowed per restart.
- **A published, versioned daemon contract** (3.0.0), and candidate envelopes that carry no authority.

**Designed, not built:**
- the supervisor/worker split (PD-72);
- the activation register (PD-40);
- the connector gate (PD-01.7);
- every class above 1a;
- everything in Packages A and B except HF-33.

**Known limits of the evidence:**
- **Never run under systemd as PID 1.** The unit's directives are checked by generation and lint, not by a real service manager.
- **Landlock ABI 7 comes from the build workspace.** The DGX's ABI is inferred (`ADJUDICATION-AP.md`).
- **No test ran on DGX hardware.**

### 2. Owner rulings already made (settled)

Do not reopen these here. The owner asked separately whether any is worth revisiting. Part 2 §2 answers that: none should be reversed, and each has one proposed amendment or clarification:
- PD-63: separate exit codes for a policy violation and `SENSE_BLIND`;
- PD-70: measure inherited blindness on `CLOCK_BOOTTIME` within a boot;
- PD-01.9: state that it does not unlock `critical`, and make BF-5 mandatory;
- PD-76: accept a direct reading from either cgroup version (measured: 208 MiB for the cgroup against DB-14's 128 MiB).

Review those amendments there.

| Ruling | What it settled | Still open for review |
| --- | --- | --- |
| **PD-63** (2026-09-29) | The blind period is in force, as built | — |
| **PD-70** (2026-09-30) | Heartbeat every `T`/2; blindness counted across restarts. Built with one reacquisition cycle | — |
| **PD-01.9, "Blind Forester"** (2026-09-30), adopted as core pattern | An *active* daemon whose stopping would drop a critical downstream payload shifts into a pre-validated degraded survival loop instead of failing closed. For observers, the survival action remains "fail closed" | **BF-1 to BF-6**, the implementation conditions: the loop runs in the supervisor; survival is signalled; last-known-safe outputs have a validity window; survival actions are pre-authorized; the far side keeps its own procedure; leaving survival mode requires revalidation |
| **PD-76** (= the v0.1 line's PD-24(a)), adopted | Memory for the budget check needs a direct cgroup `memory.peak` reading. RSS-only runs are `INCOMPLETE` | How CI treats `INCOMPLETE` where the reading cannot be taken (Package C) |

### 3. Package A: the Local Transport Contract and the daemon pattern

**Source documents:**
- *Spark Core Local Transport Contract v0.1 + v0.2 Upgrade Requirements* (LTC-01 to LTC-15, CT-01 to CT-26, LTC-V2-01 to LTC-V2-20);
- *LTC Hardening Review: Reviewer Markup Handoff* (LTC-H01 to LTC-H03).

Detail: `LTC-INTEGRATION-REVIEW.md`.

#### 3.1 The question

Should a standardized transport contract sit *within* the daemon pattern?

**Proposed answer: bind, do not embed.** The LTC defines how a component submits bounded local work to an authorized provider and receives bounded completions. Today's daemons only observe and append to their own ledger, so they have no LTC channel. The ledger is expressly *not* one: LTC-01 makes transport state ephemeral and non-canonical, while the ledger is the durable evidence layer.

The LTC becomes relevant the moment a daemon gains a *request channel*. That is how every class above 1a is designed to grow:
- an Analyst asking an inference engine;
- an Operator submitting a ticket to an executor;
- the connector gate as a provider;
- the break-glass relay as an alternate provider.

| Option | For | Against |
| --- | --- | --- |
| A. Embed the LTC in the pattern | One package, one version | Couples two version lines. Pulls daemon constants into the LTC. Observers carry unused transport code. The LTC is still a v0.1 candidate |
| **B. Bind at declared edges (recommended)** | One transport meaning across daemons, scripts and services. Gate conformance *is* the LTC suite. Fencing, backpressure and `UNKNOWN_AFTER_RESTART` defined once | Needs a small seam (manifest field, register fields, battery hook), designed before its first use |
| C. Keep them separate | Nothing to do now | Every class re-derives transport rules; the ad hoc borrowing already visible in PD-01 §6 continues |

#### 3.2 Evidence: the LTC-H01 rule found a real defect

**LTC-H01's rule:** "Exceeding any declared bound MUST produce a typed failure and MUST NOT yield a successful truncated payload."

**HF-33** (Medium, reproduced, fixed in r4.3). The reference daemon `dir-watch` capped its folder listing at 512 entries. Past the cap, the skeleton returns the first entries *in directory order* and flags the result as truncated. The daemon diffed that partial listing as if it were the whole folder. Directory order shifts when a file is added, so each addition recorded some *other, unchanged* file as removed:
- 17 false removals in 20 additions on ext4;
- 20 in 20 on tmpfs;
- no truncation flag on the events.

The fix makes a folder over the cap a failed cycle, so it counts towards the blind limit and reaches a human. The tests fail on the previous revision (`evidence/r4.3/`, `tests/test_blind.py::DirWatchCapacityTests`).

**The same shape caused HF-29 earlier:** Git output over the cap was accepted as "unavailable". Two defects from one API design (truncation returned as a value that each author must remember to check) motivate PD-83.

#### 3.3 Integration requirements (LI-1 to LI-14, all PENDING)

- **LI-1.** The ledger is not an LTC channel. Transport outcomes that matter are recorded in it as typed events; completions are observations, never authority.
- **LI-2.** A declared outward connection names an LTC channel profile: contract and version, schema, ordering scope, finite bounds, overflow policy, and a payload-handling profile.
- **LI-3.** The LTC binding is resolved at activation. The activation-register row *is* the LTC §12 binding output, and the daemon never consults a registry per message.
- **LI-4.** Compatibility (battery plus LTC conformance) and authorization (human activation) stay distinct.
- **LI-5.** The supervisor keeps to LTC-14. It never fabricates completions. **Consequence:** the Blind Forester's survival outputs are the supervisor's own marked submissions, never the worker's completions.
- **LI-6.** Channel incarnation (LTC-10) and break-glass authority epochs are separate fences. Both rotate at boot.
- **LI-7.** An alternate-provider switch (the break-glass relay) is a pre-bound, explicitly authorized rebind (LTC-V2-19). This belongs to Package B.
- **LI-8.** No skeleton read returns a truncated payload as success (PD-83).
- **LI-9.** Retry bounds hold over time, not per window (PD-86).
- **LI-10.** Pacing is a declared execution-profile field. The default for background observers is deterministic spread (PD-85).
- **LI-11.** The battery runs the LTC provider-neutral suite for each declared channel, and the report binds the LTC text's hash.
- **LI-12.** No constant crosses the boundary in either direction.
- **LI-13.** Telemetry (the heartbeat) never gains transport authority.
- **LI-14.** BG-, OR- and LI- identifiers are never used as LTC numbers.

#### 3.4 This repository's markup on LTC-H01 to H03

| Item | The markup's disposition | This review | Added |
| --- | --- | --- | --- |
| **LTC-H01** Payload-handling profiles | APPROVE WITH MODIFICATION | **Agree** | (1) Profile fields are mandatory; `NOT_APPLICABLE` only explicitly, with a reason. (2) `STREAM_BOUNDED` needs a *total-operation* deadline, not only an idle one: start-up ledger verification here is bounded per record and never idle, yet grows without limit. (3) Test both sides of the boundary (exactly at the ceiling passes, one byte over fails). Test that a capped partial result offered as complete is rejected at consumption (HF-33's shape). (4) The typed failure appears in the operation's own outcome, not only in telemetry |
| **LTC-H02** Authenticated binding | MODIFY / SPLIT OWNERSHIP | **Agree** | (1) Trust evidence names the digest it covers, so evidence cannot be replayed onto another artifact. (2) A trust result has an age and a revocation state; offline verification reproduces both. (3) Name every digest by what it covers |
| **LTC-H03** Retry and pacing | REJECT JITTER MANDATE; APPROVE BOUNDED RETRY | **Agree, with a stronger rule** | Per-layer, independently budgeted retry bounds are *not enough*: they must hold over an unbounded horizon. Evidence: HF-28 here had a finite, independent restart budget (5 per 300 s), yet a daemon failing every `T` > 75 s was restarted forever, because the retries were spaced wider than the window. Proposed extra test: retries spaced just outside the window must still terminate |

#### 3.5 Package A decision register

| ID | Decision | Recommendation |
| --- | --- | --- |
| **PD-82** | Bind, do not embed (option B, LI-1 to LI-7, LI-11 to LI-14) | APPROVE |
| **PD-83** | `ctx.run`, `ctx.git` and `ctx.list_dir` raise `TooLarge` on truncation by default; partial results only by explicit opt-in that carries the flag. A contract major (4.0.0), batched with other contract changes | APPROVE |
| **PD-84** | The activation register adopts the LTC binding vocabulary, with H02's three modifications | APPROVE |
| **PD-85** | Pacing is a declared field (`NONE`, `FIXED_PHASE`, `DETERMINISTIC_SPREAD`, `BOUNDED_JITTER`). The default is deterministic spread from the daemon's name and the host's identity. This amends PD-21, today's uniform random ±min(5 %, 30 s) | APPROVE |
| **PD-86** | Retry bounds hold over time. Submit the H03 finding to the LTC review | APPROVE |

**Questions for reviewers:**
- **A1.** Is "bind, do not embed" right, given that no daemon has a request channel yet? Or should the seam wait until the first one?
- **A2.** Is making truncation a typed failure worth a contract major (PD-83), or should the rule be enforced by the battery instead?
- **A3.** Deterministic spread gives a fixed phase per daemon, while random jitter random-walks. Is there a case where the fixed phase is worse for background observers?
- **A4.** Is the H03 strengthening ("bounded over time") correct and testable in provider-neutral terms?

### 4. Package B: Break Glass for external connections, and operator-gated recovery

Detail: `BREAK-GLASS.md`. Sub-decisions PD-01.10 to PD-01.14 in `PD-01-DAEMON-CLASSES.md`. This package is outside the LTC hardening review by that review's own scope guard.

#### 4.1 The proposal and its constraints (owner)

**The three mechanisms:**
- Dormant Egress Relaxation;
- a Sovereign Survival Socket;
- Pre-Signed Capability Envelopes.

**Then the owner added:**
- emergency epochs with fencing;
- an independent two-signal witness;
- an Emergency Flight Recorder with mandatory reconciliation;
- an operator-gated recovery boundary: `SAFE_TO_ISOLATE`, a restart that opens a new epoch, and `REJOIN_PROBATION`.

**Constraints:**
- An AI model never decides that an emergency exists. "Do not let model judgment automatically confer state-mutating authority" (Governing Principle 10).
- The mechanisms apply only to connecting to *external* systems, never to internal spark-core systems.

#### 4.2 Adjudication proposed

| Item | Proposed verdict | Key reason |
| --- | --- | --- |
| Reflex, not judgment | ADOPT (BG-1) | Only a fixed rule over mechanical signals starts an emergency; no model output is an input |
| External only | ADOPT, enforced (BG-2 to BG-4) | The validator refuses internal targets, and the relay re-checks the resolved address at connect time. Break glass never touches Spark's own confinement, control plane or authentication |
| Dormant egress | ADOPT MODIFIED (BG-5) | Landlock can only be narrowed, and its network rules cannot name a host. So the egress belongs to a separate, pre-granted, dormant relay that stops at fence |
| Survival socket (raw, unauthenticated, `/tmp`) | REJECT as first written; ADOPT the owner's revision (BG-6) | `/tmp` can be squatted, an unauthenticated socket is open to any local process, and it contradicts "authentication never degrades". The revision: exists only during an emergency epoch, lives in `/run/spark-emergency/`, identifies the peer by `SO_PEERCRED`, has a closed command set, requires a hardware key for a technician at the machine, and is removed at fence |
| Pre-signed envelopes | ADOPT with conditions (BG-7) | Single use, epoch-bound and digest-bound; only from a human-approved runbook; verified by the supervisor or relay; spent write-ahead |
| Epochs and fencing | ADOPT (BG-8 to BG-11) | Authority belongs to one epoch. A trip, the fence and every boot open a new epoch. Fencing is enforced at the external target, because the orchestrator may be alive but partitioned. Rollback defence |
| Witness | ADOPT (BG-12 to BG-14) | Two independent signals (2-of-3 where a physical signal exists). The witness answers only "may emergency mode begin?". Independence is audited |
| Flight recorder | ADOPT (BG-15 to BG-18) | A separate chain written by the supervisor. Write-ahead ("no record, no action"), preallocated space, sealed and anchored |
| Recovery boundary | ADOPT as first-class states (OR-1 to OR-8) | The fence is one durable epoch write, done first. Reconciliation is mandatory. `SAFE_TO_ISOLATE` is a supervisor state. The human boundary is required at `critical`. `HELD` never times out. Probation has a closed checklist. Emergency authority has a hard maximum duration |

**Added in review:**
1. **Two tiers.** A fixed outbound beacon (BG-S), which can come early. Acting on an external system (BG-A) is by definition class 3b, which is deferred to kernel v2.
2. **Every boot opens a new epoch.** Authority then dies even if power is cut mid-fence.
3. **A recovery panel that shows evidence, not verdicts.** "Trigger not observed for 14 min" instead of "cause cleared".

#### 4.3 Costs stated deliberately (for review)

- **An unplanned restart during an emergency ends break glass.** Old envelopes are dead in the new epoch. The survival loop continues, and protection falls to the far side's own lost-link procedure. The alternative, allowing a crash-fenced emergency to trip again with its old envelopes, is weaker.
- **An emergency during probation gets no break glass.**
- **Break glass needs** the supervisor split (PD-72), the connector gate (PD-01.7), the activation register (PD-40) and an acknowledgement path for heartbeats. None is built.

#### 4.4 Package B decision register

| ID | Decision | Recommendation |
| --- | --- | --- |
| **PD-01.10** | Premise, scope rule and two tiers | APPROVE; build BG-S after PD-01.3, PD-72 and PD-01.7; DEFER BG-A with 3b |
| **PD-01.11** | The three mechanisms as modified; a dormant relay rather than one started on demand | APPROVE |
| **PD-01.12** | Epochs and fencing; the conservative restart rule | APPROVE |
| **PD-01.13** | Witness and flight recorder | APPROVE |
| **PD-01.14** | The operator-gated recovery boundary as the only exit from any emergency *or survival* mode (this completes BF-6) | APPROVE |

**Questions for reviewers:**
- **B1.** Is the external-only rule enforceable as specified (validator plus a connect-time check plus `single_path`)? Can you find an internal target it misses?
- **B2.** Is the conservative restart rule the right trade?
- **B3.** Can the witness be independent on a single DGX, or does it require separate hardware?
- **B4.** Does the state machine have a path by which authority survives a fence?

### 5. Package C: other open decisions that gate the next steps

These are compact. Each is set out in full in the file named.

| ID | Decision | Recommendation | File |
| --- | --- | --- | --- |
| BF-1 to BF-6 | Implementation conditions of the adopted Blind Forester ruling | APPROVE | `PD-01-DAEMON-CLASSES.md` §9b |
| PD-72 | Supervisor/worker split as a precondition for any active class | APPROVE | `INTEGRATION-REVIEW.md` §7 |
| PD-01.1, PD-01.2 | The class ladder and invariants S-1 to S-8; every manifest declares a consequence level (`critical` refused) | APPROVE | `PD-01-DAEMON-CLASSES.md` §7 |
| PD-01.3 | Unlock 1b Sentinel: `SENSE_DEGRADED` at `T`/2, the `spark-escalation/1` format, a read-only escalations command | APPROVE | same |
| PD-69 | Limits are checked against each other (U-8), and every new timeout goes in a budget table | APPROVE | `INTEGRATION-REVIEW.md` §5 |
| PD-71 (the part PD-76 does not cover) | A named worst-case fixture per observation type; a scaling series to derive `N_max` | APPROVE | same |
| PD-76, open part | How the workspace and CI treat `INCOMPLETE`. Since r4.4, a direct reading is shown to be possible here without systemd, so `INCOMPLETE` should be rare | Require PASS where a direct reading is possible (Part 2 §2.4) | `RECONCILIATION-V01-LINE.md` §3 |
| PD-75 to PD-81 | Alias of the v0.1 line's PD-23 to PD-29, or renumber this repository's | Confirm the alias | same, §2 |
| C-1 (with PD-81) | Battery exit codes. Here: PASS 0, INCOMPLETE 3, FAIL 1. v0.1 line: PASS 0, FAIL 3, INCOMPLETE 4 | Adopt the v0.1 line's codes (0, 3, 4) in the batched contract change | same, §4 |
| PD-64 to PD-68 | Universal-contract rules: stack alignment; temporal honesty (S-8); stop and revocation; cumulative bounds; assurance of the assurance | APPROVE | `ADJUDICATION-PLUG-AND-PLAY.md` §9 |
| A-1 to A-12 | Amendments from the prior-art review (IEC 61784-3 black channel, NAMUR NE 107, ISA-18.2, IEC 62443, AUTOSAR, SOTIF, UL 4600, and others) | Rule on each | `PRIOR-ART-REVIEW.md` |
| Phase 1 go-ahead | An `OnFailure=` notifier; a diverse staleness checker that does not import the skeleton; a budget report. No contract change | APPROVE | `INTEGRATION-REVIEW.md` §4 |

### 6. Invariants every review should check against

Nothing in Packages A or B may weaken these:
- **S-1.** Never report health you do not have.
- **S-2.** Never record a guess as an observation. A capped or unsettled read is not an observation (HF-29, HF-33).
- **S-3.** Always surface blindness within `T`. Nothing pauses the blind clock.
- **S-4.** Stay confined. Higher classes gain request channels, never wider confinement.
- **S-5.** Authentication and handshakes never degrade.
- **S-6.** Proposals are not authority.
- **S-7.** Another system's safety protocol wins on its side.
- **S-8.** Never imply more temporal precision than you have.
- **U-7.** Evidence expires, and independence must be shown, not assumed.
- **U-8.** Limits are checked against each other, including over time (Package A's H03 finding).

### 7. Acceptance gates for this handoff

- Every item in Part 1 sections 3.5, 4.4 and 5, and every settled-ruling amendment in Part 2 section 2, is marked APPROVE, MODIFY, REJECT or DEFER.
- No Package B requirement is given an LTC number, and no LTC placeholder is consumed by Package B.
- No reviewer claim of a defect is accepted without a reproduction, a measurement or a code trace.
- No approval here activates a daemon, unlocks a class or changes the LTC version. Those remain the owner's Class C decisions.

## Part 2: Decision brief

**How to read the verdicts.** A settled ruling can be kept, kept with an amendment (the decision stands; how it is built or worded changes), clarified (the decision stands; its scope is stated), or reversed. Every finding says what kind of evidence stands behind it:
- *measured*: run and recorded;
- *traced*: read in the code;
- *reasoned*: argued, not run.

### 1. Answer in brief

**No settled ruling should be reversed.** Each has held up, and one was confirmed by a new measurement. Each of the four does have one amendment or clarification worth making, and two of them fix real weaknesses in how the ruling was built:

| Ruling | Verdict | What is worth revisiting | Evidence | Urgency |
| --- | --- | --- | --- | --- |
| **PD-63** Blind period | **Keep, amend how it was built** | `SENSE_BLIND` and a policy violation share exit code 78. When the output directory is unsafe, the ledger cannot be written and the exit code is the only signal left, so a security event looks like an operational one | Traced (`spark_daemon/__init__.py`: `EXIT_POLICY = 78`, `EXIT_SENSE_BLIND = 78`) | Medium; batch with the exit-code work (PD-35, C-1) |
| **PD-70** Blindness survives restarts | **Keep, amend how it was built** | Blindness inherited across a restart is measured on the wall clock. If the clock steps back, inherited blindness reads as zero, and the slow restart loop that PD-70 fixed (HF-32) hides again. If it steps forward, a healthy daemon is judged blind | Traced (`_seconds_since` clamps a future timestamp to 0) | Medium-high; small and additive |
| **PD-01.9** Blind Forester | **Keep, clarify its scope** | "Critical downstream payload" can be read as the `critical` consequence level, which PD-01.2 refuses until a certification path exists. The ruling should say that it does not unlock `critical`, and that the far side's own procedure (BF-5) is mandatory, not recommended | Reasoned | High, before any active class is designed |
| **PD-76** Direct memory reading | **Keep; amend the mechanism's wording** | The ruling names cgroup v2's `memory.peak` from a systemd transient scope. This workspace has neither, yet a direct reading *can* be taken here, from a directly created cgroup (v1). The measurement confirms the ruling's premise: DB-14's basis read 128 MiB where the cgroup peaked at 208 MiB | **Measured** (`evidence/r4.4/`) | Medium; it removes most of the "`INCOMPLETE` in CI" problem |

The r4.1 statement that "this workspace cannot take the reading" was wrong, and it is corrected where it appeared (`RECONCILIATION-V01-LINE.md` §3, the decision log).

### 2. The settled rulings, one by one

#### 2.1 PD-63: the blind period (settled 2026-09-29)

**Decided:** a daemon that accepts no observation for `T` (`blind_limit_seconds`) stops with `SENSE_BLIND`, exit 78, and the unit never restarts it. `T` is declared in the manifest.

**Learned since:**
- **The exit code is shared.** `EXIT_POLICY` (an unsafe output directory, or a purity or policy violation: possibly an attack) and `EXIT_SENSE_BLIND` (the daemon cannot see: operational) are both 78. The ledger distinguishes them, but the unsafe-output-directory case is exactly the one where the ledger cannot be written, so the exit status is all that systemd, the journal and an `OnFailure=` notifier see. The two need different people at different urgency.
- **Why 78 was chosen:** the Observer's own table uses 78 for `SENSE_BLIND` (WBS 3.1), and this pattern aligns with the Observer voluntarily (PD-35).
- **A consequence of HF-33:** with a persistently over-capacity folder, `dir-watch` now fails every cycle and stops at `T`. That is the ruling working as intended: a capacity problem becomes a visible stop, not a silently wrong inventory. It is stated here so that it is chosen knowingly.

**Options for the exit code:**

| Option | For | Against |
| --- | --- | --- |
| A. Keep both at 78 | No change; matches the Observer for `SENSE_BLIND` | A security stop and an operational stop are indistinguishable without the ledger, and the ledger may be the thing that failed |
| **B. Move `EXIT_POLICY` to its own code; keep `SENSE_BLIND` at 78 (recommended)** | Keeps the Observer alignment for the code the Observer defines. Separates security from operations where it matters most. `RestartPreventExitStatus` simply lists both | A contract major (exit codes are in the contract). Batch it with PD-35 and C-1 |
| C. Move `SENSE_BLIND` instead | — | Breaks the Observer alignment |

**Options for where `T` is set** (a lower priority):

| Option | For | Against |
| --- | --- | --- |
| **A. In the manifest (as ruled; recommended for now)** | Hashed into `DAEMON_START` and bound to activation; changing it is visible | Deploying the same daemon on two hosts with different `T` needs two manifests |
| B. The manifest declares a range; the activation register (PD-40) picks `T` within it and records the choice | Per-deployment tuning without re-approving the daemon | Needs the register, which is not built. Two places to look for one number |

**Recommendation:** keep PD-63. Adopt exit-code option B in the batched contract change. Revisit where `T` is set when PD-40 is built.

#### 2.2 PD-70: blindness survives restarts (settled 2026-09-30)

**Decided:**
- a heartbeat every `T`/2;
- at start-up, the blind clock begins at the ledger's latest evidence of an accepted cycle;
- one reacquisition cycle per restart, so that no daemon is locked out.

**Learned since:** the time since that evidence is measured on the **wall clock** (`_seconds_since`, which returns 0 when the timestamp lies in the future). Two failure cases follow from the code:
- **The clock steps back** (an NTP correction after a fast real-time clock, or a manual change). The last accepted timestamp then lies "in the future", inherited blindness reads as 0, and every restart starts a fresh countdown. This brings HF-32 back: a slow restart loop hides blindness indefinitely, which is the silent outage PD-70 was adopted to prevent.
- **The clock steps forward** (a real-time clock reset at boot, then NTP). A healthy daemon inherits a large blindness and gets only its single reacquisition cycle. That is safe, but it is a false stop.

Within one boot there is a clock that no wall-clock change moves: Linux's `CLOCK_BOOTTIME`, which is shared by all processes since boot and counts suspended time. Across a reboot no local clock is reliable. The Observer's own rule (D-7) already keeps monotonic time within a process.

| Option | For | Against |
| --- | --- | --- |
| A. Keep the wall clock (as built) | Simple | A backward step silently brings back the defect PD-70 fixed |
| **B. Record `boot_id` and `CLOCK_BOOTTIME` with every piece of acceptance evidence (heartbeat, start, stop). Within the same boot, measure on `CLOCK_BOOTTIME`. Across boots, use the wall clock; if it went backward, or cannot be trusted, assume the worst (blindness at the limit, so one reacquisition cycle). Record `clock_basis` (recommended)** | Immune to clock steps within a boot, which is where restart loops happen. Honest across boots (S-8), and errs towards a check, never towards silence | Additive fields in reserved records (a contract minor). Blindness measured from the last heartbeat instead of the last event can over-state it by up to `T`/2, in the safe direction |
| C. Always assume the worst across restarts | Simplest safe rule | Every restart costs its one reacquisition cycle; a daemon on a flaky input stops more often |

**Recommendation:** keep PD-70 and build option B. A test can inject a backward step through the ledger's timestamps; it should fail on the current code and pass after the change.

#### 2.3 PD-01.9: the "Blind Forester" protocol (settled 2026-09-30, adopted as core pattern)

**Decided:** an active daemon whose stopping would drop a critical downstream payload does not fail closed when blind. It shifts into a pre-validated, degraded survival loop and signals distress. A restart resumes that loop. For observers the survival action stays "fail closed". The ruling superseded, in part, the earlier boundary that no DGX process may sit in a loop that keeps a person safe.

**Learned since:**
1. **The word "critical" is ambiguous.** It can be read as the consequence level `critical`, which PD-01.2 refuses "until a certification path exists". Read that way, the ruling would unlock life-safety duty for a general-purpose AI host with no certification. That is almost certainly not intended, but the text does not rule it out.
2. **BF-5 (the far side keeps its own procedure) is still only a recommendation.** It is the condition that keeps a person safe if the whole DGX fails: its kernel, power or disk.
3. **Break Glass (Package B) separates survival *mode* from emergency *authority*.** The mode continues across a restart; the authority does not. The Blind Forester ruling should say which one it covers: the mode.
4. **Exit from survival mode.** BF-6 requires revalidation. PD-01.14 proposes that the operator-gated recovery path be the only exit.
5. **Unit rules.** Today the unit never restarts exit 78. An active daemon that exits 78 when blind would drop its payload, the opposite of the ruling. BF-1 (the loop runs in the supervisor, which does not exit) resolves this, so the supervisor split (PD-72) is a hard prerequisite, not an option.

| Option | For | Against |
| --- | --- | --- |
| A. Keep as ruled; conditions pending | No change | The scope ambiguity stays open until someone designs against it |
| **B. Keep, and add a scope clarification (recommended):** the protocol applies to active classes at `standard` and `elevated` consequence; it does not unlock `critical`, which stays refused under PD-01.2; BF-5 is mandatory wherever a person could be harmed; it governs survival *mode*, never emergency *authority*; the supervisor split is a prerequisite | Keeps the owner's intent. Closes the reading that would put an uncertified host in a life-safety loop. Aligns with Package B | The owner may have intended `critical` to be reachable. If so, that needs its own decision with a certification path |
| C. Narrow the ruling to `elevated` only, or return to the old boundary | The simplest safety case | Loses a first line of defence the owner deliberately adopted |

**Recommendation:** keep PD-01.9 with the clarification in option B, and confirm BF-1 to BF-6, with BF-5 as mandatory.

#### 2.4 PD-76 (= the v0.1 line's PD-24(a)): a direct memory reading (settled 2026-09-29 in that line)

**Decided:** the memory check needs a direct reading of cgroup `memory.peak` from a fresh transient scope containing the daemon and every child. A process-RSS reading is a proxy that never decides, and an RSS-only run is `INCOMPLETE`.

**Learned since (measured, `evidence/r4.4/`).** Running as root on this workspace, with no systemd user manager, a child memory cgroup was created under the shell's own cgroup. A process was moved into it that held 80 MiB while its child allocated 120 MiB:

| Basis | Reading |
| --- | --- |
| DB-14 today: max(daemon, largest child) | 128 MiB |
| cgroup v1 `memory.max_usage_in_bytes`, daemon plus every child | **208 MiB** |

- **The premise is confirmed.** DB-14's basis reads about 38 % low whenever a daemon holds memory while a child runs, and `MemoryMax=` enforces the cgroup figure.
- **The mechanism in the ruling is narrower than necessary.** This workspace uses cgroup v1 (no `memory.peak`) and has no systemd user manager (no transient scope), yet a direct reading was taken. So the r4.1 conclusion, that every battery run here would be `INCOMPLETE`, was wrong.

| Option | For | Against |
| --- | --- | --- |
| A. Keep the wording (v2 `memory.peak` from a systemd transient scope) | Exactly what the target, the DGX on cgroup v2 with systemd, will use | `INCOMPLETE` on every host without both, including this one, although a direct reading is possible |
| **B. Keep the ruling; widen the mechanism (recommended):** "a direct cgroup peak for a fresh cgroup holding the daemon and every child: v2 `memory.peak`, or v1 `memory.max_usage_in_bytes`, created by a systemd transient scope or directly where the battery may create cgroups. The report records the cgroup version and how the cgroup was created. `INCOMPLETE` only where no direct reading is possible" | Same substance. `INCOMPLETE` becomes rare instead of universal, and CI can require PASS where it runs as root or with delegated cgroups | v1 and v2 account for memory differently (page cache, kernel memory), so readings from different versions are not interchangeable. Compare each only against a limit on the same kind of host. The battery needs cgroup write access |
| C. Go back to RSS | — | Measured 38 % low; reversing would contradict the evidence |

**Recommendation:** keep PD-76 and adopt option B's wording. Build DB-19 on it, and record the cgroup version in the report.

### 3. The open decisions: choices, pros and cons

Each row gives the realistic options. The recommended option is marked **(rec.)**.

#### 3.1 Package A: the Local Transport Contract

| Decision | Options | Pros | Cons |
| --- | --- | --- | --- |
| **PD-82** Where the LTC sits | **Bind at declared connections (rec.)** / embed in the pattern / keep separate | Bind: one transport meaning, observers untouched, the gate's tests *are* the LTC suite | Bind: the seam is designed before its first use. Embed: couples version lines and pulls daemon constants into the LTC. Separate: each class re-derives transport rules |
| **PD-83** Truncated reads | **The framework raises `TooLarge` by default; partial results only by opt-in (rec.)** / a battery check only / leave it to authors | Framework: closes the API shape behind HF-29 and HF-33 for every author | Framework: a contract major. Battery-only: catches only what a fixture exercises. Authors: two defects already show this fails |
| **PD-84** Register vocabulary | **Adopt the LTC binding fields, with H02's modifications (rec.)** / keep this pattern's own names | One vocabulary for daemons and providers | Ties the register's design to an unadjudicated LTC; mitigated because the fields are semantic, not a wire format |
| **PD-85** Pacing | **Declared field; deterministic spread by default (rec.)** / keep random jitter (PD-21) / none | Deterministic: reproducible and testable; fixed-phase workloads get no hidden jitter | Deterministic: a fixed phase per daemon, so two daemons that hash close together stay close (mitigated by also hashing the host identity). Random: not reproducible |
| **PD-86** Retry bounds | **Bounded over time, checked across layers (rec.)** / per-layer bounds only | Closes the HF-28 class of loop | Every retrying layer must declare a lifetime bound or a terminal state |

#### 3.2 Package B: Break Glass and recovery

| Decision | Options | Pros | Cons |
| --- | --- | --- | --- |
| **PD-01.10** Tiers | **Beacon first, acting with class 3b (rec.)** / build both together / defer all | Beacon first: useful early, cannot act, proves the recovery states before anything depends on them | Beacon first: two build stages. Both together: waits for kernel v2. Defer: no distress signal when the control plane is lost |
| **PD-01.11** Egress relay | **Always present and dormant (rec.)** / started only on a trip | Dormant: nothing privileged has to start a unit at the worst moment | Dormant: a standing egress permission, although policy-gated and narrowly scoped. On-trip: no standing egress, but needs a privileged starter |
| **PD-01.11** Survival socket | **The owner's revised socket (rec.)** / no socket (physical console only) / the original unauthenticated socket | Revised: zero friction for declared peers; exists only during an emergency | No socket: the smallest attack surface, but no local controller path. Unauthenticated: rejected, because it can be squatted and contradicts S-5 |
| **PD-01.12** Restart during an emergency | **Break glass ends and survival continues (rec.)** / allow re-arming with old envelopes | Conservative: authority never outlives its epoch | Conservative: after a crash, only the far side's procedure protects the payload. Permissive: stolen or stale envelopes live longer |
| **PD-01.13** Witness | **Separate hardware where a person could be harmed; a separate process tree otherwise (rec.)** / always same-host / always separate hardware | Matches independence to consequence (U-7, BM-3) | Separate hardware costs money and needs integration. Same-host shares failure modes |
| **PD-01.14** Recovery gate | **Operator required at `critical`, declared per deployment below that (rec.)** / always operator / always automatic | A human boundary where it matters; automatic where a stop costs only time | Always-operator: slow recovery for trivial daemons. Always-automatic: broad authority returns because a heartbeat did |

#### 3.3 Package C: other decisions that gate the next steps

| Decision | Options | Pros | Cons |
| --- | --- | --- | --- |
| **BF-1 to BF-6** | **Confirm all, BF-5 mandatory (rec.)** / confirm a subset | Makes the adopted ruling buildable and safe | None material; each condition follows from a recorded invariant |
| **PD-72** Supervisor/worker split | **A precondition for any active class; build with the first one (rec.)** / build now / never | Required by BF-1 and LTC-14; building it later avoids premature complexity | Build now: effort with no active class to use it. Never: the Blind Forester cannot be built |
| **PD-01.1 to PD-01.3** Classes, consequence field, Sentinel | **Approve; ship 1b next (rec.)** / keep 1a only | 1b closes silent outages at the source; a consequence field makes risk explicit | A contract change; alert fatigue unless ISA-18.2's rules (A-5) come with it |
| **PD-69** Limits checked against each other | **Approve (rec.)** / ad hoc | Four defects came from limits checked alone (HF-28, HF-30, HF-31, HF-32) | A budget table to maintain |
| **PD-71** (remainder) Worst-case fixtures, `N_max` | **Approve with DB-19 (rec.)** / defer | Evidence where the risk is, not on one small fixture | More battery time |
| **PD-75 to PD-81** Numbering | **Alias (rec.)** / renumber this repository | Alias: no document rewritten | Renumber: churn across many documents; either works if chosen once |
| **C-1** Battery exit codes | **Adopt 0/3/4 (PASS/FAIL/INCOMPLETE) in the batched change (rec.)** / keep 0/1/3 / map | One convention with SPS-1 | A breaking change for anyone scripting on today's codes; batch it once |
| **`INCOMPLETE` in CI** | **Require PASS where a direct reading is possible; allow `INCOMPLETE` with a stated environment reason elsewhere (rec.)** / always require PASS / always allow | Honest, and after §2.4 rarely needed | Always PASS: fails on hosts that cannot measure. Always allow: hides real gaps |
| **Phase 1** (`OnFailure=` notifier, diverse staleness checker, budget report) | **Go (rec.)** / wait | No contract change; each item closes a monitoring gap | Small effort |

### 4. Suggested order of rulings

1. **Clarify PD-01.9** (§2.3). It frames everything designed for active classes.
2. **Confirm BF-1 to BF-6 and the Phase 1 go-ahead.** Neither changes the contract.
3. **Amend PD-70's clock basis** (§2.2). Small, additive, and it closes a regression path for an owner ruling.
4. **Adopt PD-76's wider mechanism** (§2.4), and with it the CI rule for `INCOMPLETE`. Then build DB-19.
5. **The batched contract change (4.0.0):**
   - PD-63's exit-code split;
   - C-1 and PD-81 (battery exit codes);
   - PD-35, PD-39 and PD-49;
   - PD-83 (truncated reads);
   - PD-85 (pacing);
   - PD-01.2 (consequence field).
6. **PD-82, PD-84 and PD-86**, as design rules for the register and the gate.
7. **Package B** (PD-01.10 to PD-01.14), after PD-72 and PD-01.7 are ruled on.

## Part 3: Markup and document map

### Markup table

Mark each row. Settled rulings (Part 2 §2): KEEP, AMEND, CLARIFY or REVERSE. Open decisions: APPROVE, MODIFY, REJECT or DEFER. Evidence type: reproduced, measured, traced or opinion.

| Item | Where | Verdict | Evidence type | Notes |
| --- | --- | --- | --- | --- |
| PD-63: exit-code split | Part 2 §2.1 | | | |
| PD-63: where `T` is set | Part 2 §2.1 | | | |
| PD-70: clock basis | Part 2 §2.2 | | | |
| PD-01.9: scope clarification | Part 2 §2.3 | | | |
| PD-76: mechanism wording, and `INCOMPLETE` in CI | Part 2 §2.4 | | | |
| PD-82: bind, do not embed | Part 1 §3.5, Part 2 §3.1 | | | |
| PD-83: truncated reads raise | Part 1 §3.5, Part 2 §3.1 | | | |
| PD-84: register vocabulary | Part 1 §3.5, Part 2 §3.1 | | | |
| PD-85: declared pacing | Part 1 §3.5, Part 2 §3.1 | | | |
| PD-86: retry bounds over time | Part 1 §3.5, Part 2 §3.1 | | | |
| LTC-H01 to H03: this review's additions | Part 1 §3.4 | | | |
| PD-01.10: tiers and scope | Part 1 §4.4, Part 2 §3.2 | | | |
| PD-01.11: relay and socket | Part 1 §4.4, Part 2 §3.2 | | | |
| PD-01.12: epochs and restart rule | Part 1 §4.4, Part 2 §3.2 | | | |
| PD-01.13: witness and flight recorder | Part 1 §4.4, Part 2 §3.2 | | | |
| PD-01.14: recovery boundary | Part 1 §4.4, Part 2 §3.2 | | | |
| BF-1 to BF-6 | Part 1 §5, Part 2 §3.3 | | | |
| PD-72: supervisor/worker split | Part 1 §5, Part 2 §3.3 | | | |
| PD-01.1 to PD-01.3 | Part 1 §5, Part 2 §3.3 | | | |
| PD-69, PD-71 | Part 1 §5, Part 2 §3.3 | | | |
| PD-75 to PD-81, C-1 | Part 1 §5, Part 2 §3.3 | | | |
| PD-64 to PD-68, A-1 to A-12 | Part 1 §5 | | | |
| Phase 1 go-ahead | Part 1 §5, Part 2 §3.3 | | | |
| Seams N-1 to N-14: any not covered by the design | Reviewer focus | | | |

### Appendix: document map

| File | Contents |
| --- | --- |
| `README.md` | Overview, the decision list, the decision log, the revision history |
| `HANDOFF-REVIEW-PACKAGE.md` | Part 1 of this document, on its own |
| `DECISION-BRIEF.md` | Part 2 of this document, on its own |
| `HARDENING.md` | HF-01 to HF-33: each defect, its reproduction, its fix and its test |
| `DAEMON-CONTRACT.md` | The author contract, the candidate envelope, evidence lanes |
| `PD-01-DAEMON-CLASSES.md` | Classes, consequence levels, invariants, blind modes, Blind Forester, Break Glass summary |
| `BREAK-GLASS.md` | Package B in full |
| `LTC-INTEGRATION-REVIEW.md` | Package A in full |
| `INTEGRATION-REVIEW.md` | Limits checked against each other, the integration plan, owner rulings, the hardening review |
| `ADJUDICATION-PLUG-AND-PLAY.md` | The L0 to L4 contract stack, kernel v2 agenda, universal-contract rules U-1 to U-7 |
| `PRIOR-ART-REVIEW.md` | Industrial, flight, automotive, robotics and AI-agent prior art; A-1 to A-12 |
| `RECONCILIATION-V01-LINE.md` | The parallel v0.1 line: alias table, conflicts, the owner's ruling PD-76 |
| `evidence/` | Reproductions and measurements by revision |
