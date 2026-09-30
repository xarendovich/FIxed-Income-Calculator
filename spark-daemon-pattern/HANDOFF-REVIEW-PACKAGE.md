# Spark Daemon Pattern: review and adjudication handoff (r4.3)

- **Status:** PROPOSED FOR INDEPENDENT REVIEW. NOT ADJUDICATED. It authorizes no LTC version change, no daemon class unlock and no activation.
- **Date:** 2026-09-30. Prepared by Claude for the owner, to hand to other reviewers (people or models).
- **Repository state:** `spark-daemon-pattern/` at revision r4.3, on branch `claude/clever-bardeen-qqkxez`:
  - 205 of 205 self-tests pass;
  - the reference daemons pass 18 of 18 conformance-battery checks on the build workspace (Landlock ABI 7);
  - the daemon contract is 3.0.0.
- **This document stands alone.** Every claim names the file in the repository that holds the detail, for reviewers who can read it. A reviewer without the repository can still mark every item.

## 0. How to review

1. **Mark each item** in the decision registers (sections 3, 4 and 5): **APPROVE**, **MODIFY**, **REJECT** or **DEFER**, with notes. Section 8 has a blank markup table.
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

## 1. What the daemon pattern is, and what is built

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

## 2. Owner rulings already made (settled; do not reopen)

| Ruling | What it settled | Still open for review |
| --- | --- | --- |
| **PD-63** (2026-09-29) | The blind period is in force, as built | — |
| **PD-70** (2026-09-30) | Heartbeat every `T`/2; blindness counted across restarts. Built with one reacquisition cycle | — |
| **PD-01.9, "Blind Forester"** (2026-09-30), adopted as core pattern | An *active* daemon whose stopping would drop a critical downstream payload shifts into a pre-validated degraded survival loop instead of failing closed. For observers, the survival action remains "fail closed" | **BF-1 to BF-6**, the implementation conditions: the loop runs in the supervisor; survival is signalled; last-known-safe outputs have a validity window; survival actions are pre-authorized; the far side keeps its own procedure; leaving survival mode requires revalidation |
| **PD-76** (= the v0.1 line's PD-24(a)), adopted | Memory for the budget check needs a direct cgroup `memory.peak` reading. RSS-only runs are `INCOMPLETE` | How CI treats `INCOMPLETE` where the reading cannot be taken (Package C) |

## 3. Package A: the Local Transport Contract and the daemon pattern

**Source documents:**
- *Spark Core Local Transport Contract v0.1 + v0.2 Upgrade Requirements* (LTC-01 to LTC-15, CT-01 to CT-26, LTC-V2-01 to LTC-V2-20);
- *LTC Hardening Review: Reviewer Markup Handoff* (LTC-H01 to LTC-H03).

Detail: `LTC-INTEGRATION-REVIEW.md`.

### 3.1 The question

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

### 3.2 Evidence: the LTC-H01 rule found a real defect

**LTC-H01's rule:** "Exceeding any declared bound MUST produce a typed failure and MUST NOT yield a successful truncated payload."

**HF-33** (Medium, reproduced, fixed in r4.3). The reference daemon `dir-watch` capped its folder listing at 512 entries. Past the cap, the skeleton returns the first entries *in directory order* and flags the result as truncated. The daemon diffed that partial listing as if it were the whole folder. Directory order shifts when a file is added, so each addition recorded some *other, unchanged* file as removed:
- 17 false removals in 20 additions on ext4;
- 20 in 20 on tmpfs;
- no truncation flag on the events.

The fix makes a folder over the cap a failed cycle, so it counts towards the blind limit and reaches a human. The tests fail on the previous revision (`evidence/r4.3/`, `tests/test_blind.py::DirWatchCapacityTests`).

**The same shape caused HF-29 earlier:** Git output over the cap was accepted as "unavailable". Two defects from one API design (truncation returned as a value that each author must remember to check) motivate PD-83.

### 3.3 Integration requirements (LI-1 to LI-14, all PENDING)

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

### 3.4 This repository's markup on LTC-H01 to H03

| Item | The markup's disposition | This review | Added |
| --- | --- | --- | --- |
| **LTC-H01** Payload-handling profiles | APPROVE WITH MODIFICATION | **Agree** | (1) Profile fields are mandatory; `NOT_APPLICABLE` only explicitly, with a reason. (2) `STREAM_BOUNDED` needs a *total-operation* deadline, not only an idle one: start-up ledger verification here is bounded per record and never idle, yet grows without limit. (3) Test both sides of the boundary (exactly at the ceiling passes, one byte over fails). Test that a capped partial result offered as complete is rejected at consumption (HF-33's shape). (4) The typed failure appears in the operation's own outcome, not only in telemetry |
| **LTC-H02** Authenticated binding | MODIFY / SPLIT OWNERSHIP | **Agree** | (1) Trust evidence names the digest it covers, so evidence cannot be replayed onto another artifact. (2) A trust result has an age and a revocation state; offline verification reproduces both. (3) Name every digest by what it covers |
| **LTC-H03** Retry and pacing | REJECT JITTER MANDATE; APPROVE BOUNDED RETRY | **Agree, with a stronger rule** | Per-layer, independently budgeted retry bounds are *not enough*: they must hold over an unbounded horizon. Evidence: HF-28 here had a finite, independent restart budget (5 per 300 s), yet a daemon failing every `T` > 75 s was restarted forever, because the retries were spaced wider than the window. Proposed extra test: retries spaced just outside the window must still terminate |

### 3.5 Package A decision register

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

## 4. Package B: Break Glass for external connections, and operator-gated recovery

Detail: `BREAK-GLASS.md`. Sub-decisions PD-01.10 to PD-01.14 in `PD-01-DAEMON-CLASSES.md`. This package is outside the LTC hardening review by that review's own scope guard.

### 4.1 The proposal and its constraints (owner)

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

### 4.2 Adjudication proposed

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

### 4.3 Costs stated deliberately (for review)

- **An unplanned restart during an emergency ends break glass.** Old envelopes are dead in the new epoch. The survival loop continues, and protection falls to the far side's own lost-link procedure. The alternative, allowing a crash-fenced emergency to trip again with its old envelopes, is weaker.
- **An emergency during probation gets no break glass.**
- **Break glass needs** the supervisor split (PD-72), the connector gate (PD-01.7), the activation register (PD-40) and an acknowledgement path for heartbeats. None is built.

### 4.4 Package B decision register

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

## 5. Package C: other open decisions that gate the next steps

These are compact. Each is set out in full in the file named.

| ID | Decision | Recommendation | File |
| --- | --- | --- | --- |
| BF-1 to BF-6 | Implementation conditions of the adopted Blind Forester ruling | APPROVE | `PD-01-DAEMON-CLASSES.md` §9b |
| PD-72 | Supervisor/worker split as a precondition for any active class | APPROVE | `INTEGRATION-REVIEW.md` §7 |
| PD-01.1, PD-01.2 | The class ladder and invariants S-1 to S-8; every manifest declares a consequence level (`critical` refused) | APPROVE | `PD-01-DAEMON-CLASSES.md` §7 |
| PD-01.3 | Unlock 1b Sentinel: `SENSE_DEGRADED` at `T`/2, the `spark-escalation/1` format, a read-only escalations command | APPROVE | same |
| PD-69 | Limits are checked against each other (U-8), and every new timeout goes in a budget table | APPROVE | `INTEGRATION-REVIEW.md` §5 |
| PD-71 (the part PD-76 does not cover) | A named worst-case fixture per observation type; a scaling series to derive `N_max` | APPROVE | same |
| PD-76, open part | How the workspace and CI treat `INCOMPLETE` where no systemd user manager exists | Decide before building DB-19 | `RECONCILIATION-V01-LINE.md` §3 |
| PD-75 to PD-81 | Alias of the v0.1 line's PD-23 to PD-29, or renumber this repository's | Confirm the alias | same, §2 |
| C-1 (with PD-81) | Battery exit codes. Here: PASS 0, INCOMPLETE 3, FAIL 1. v0.1 line: PASS 0, FAIL 3, INCOMPLETE 4 | Adopt the v0.1 line's codes (0, 3, 4) in the batched contract change | same, §4 |
| PD-64 to PD-68 | Universal-contract rules: stack alignment; temporal honesty (S-8); stop and revocation; cumulative bounds; assurance of the assurance | APPROVE | `ADJUDICATION-PLUG-AND-PLAY.md` §9 |
| A-1 to A-12 | Amendments from the prior-art review (IEC 61784-3 black channel, NAMUR NE 107, ISA-18.2, IEC 62443, AUTOSAR, SOTIF, UL 4600, and others) | Rule on each | `PRIOR-ART-REVIEW.md` |
| Phase 1 go-ahead | An `OnFailure=` notifier; a diverse staleness checker that does not import the skeleton; a budget report. No contract change | APPROVE | `INTEGRATION-REVIEW.md` §4 |

## 6. Invariants every review should check against

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

## 7. Acceptance gates for this handoff

- Every item in sections 3.5, 4.4 and 5 is marked APPROVE, MODIFY, REJECT or DEFER.
- No Package B requirement is given an LTC number, and no LTC placeholder is consumed by Package B.
- No reviewer claim of a defect is accepted without a reproduction, a measurement or a code trace.
- No approval here activates a daemon, unlocks a class or changes the LTC version. Those remain the owner's Class C decisions.

## 8. Markup template

| ID | Decision (APPROVE / MODIFY / REJECT / DEFER) | Evidence type (reproduced / measured / traced / opinion) | Notes |
| --- | --- | --- | --- |
| PD-82 | | | |
| PD-83 | | | |
| PD-84 | | | |
| PD-85 | | | |
| PD-86 | | | |
| LTC-H01 (this review's additions) | | | |
| LTC-H02 (this review's additions) | | | |
| LTC-H03 (this review's addition) | | | |
| PD-01.10 | | | |
| PD-01.11 | | | |
| PD-01.12 | | | |
| PD-01.13 | | | |
| PD-01.14 | | | |
| BF-1 to BF-6 | | | |
| PD-72 | | | |
| Package C (other rows) | | | |

## Appendix: document map

| File | Contents |
| --- | --- |
| `README.md` | Overview, the decision list, the decision log, the revision history |
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
