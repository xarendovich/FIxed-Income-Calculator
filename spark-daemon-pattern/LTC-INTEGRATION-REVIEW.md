# The Local Transport Contract and the daemon pattern: integration review

- **Status:** REVIEW AND RECOMMENDATIONS. One defect was found and fixed while testing the LTC-H01 rule: HF-33, in the reference daemon `dir-watch`. Nothing else changes behaviour. The LTC is not adjudicated, and nothing here authorizes an LTC version change.
- **Revision:** r4.3, 2026-09-30, by Claude.
- **Asked:** the owner asked to review the *LTC Hardening Review — Reviewer Markup Handoff* (LTC-H01 to LTC-H03) "for full integration", with three questions:
  - What would change?
  - What are the requirements?
  - "Is this good to have a standardized transport contract that sits within the standard daemon pattern?", with the pros and cons weighed.
- **Sources:**
  - *Spark Core Local Transport Contract v0.1 + v0.2 Upgrade Requirements* (29 September 2026): LTC-01 to LTC-15, CT-01 to CT-26, LTC-V2-01 to LTC-V2-20.
  - The hardening markup (30 September 2026).
  - This repository at r4.2.

## 1. Answer in brief

**A standardized transport contract: yes. Inside the daemon pattern: no. The pattern should bind to it at declared edges.**

The LTC defines how a component *submits bounded local work to an authorized provider and receives bounded completions*. Today every daemon is an observer (class 1a): it reads the host and appends to its own ledger. It has nothing to submit and nothing to wait for, so there is no LTC channel in the pattern today. The ledger itself is expressly *not* one: LTC-01 makes transport state ephemeral and non-canonical, and puts durable delivery above the transport. The ledger is exactly that durable, higher-level evidence.

The LTC becomes the daemon's business as soon as a daemon gets a *request channel*, which is how PD-01 §3 grows every class above 1a ("the daemon never gains power; it gains request channels to something separate"):
- **2b (Analyst)** submits a diagnosis request to an inference engine and receives a completion.
- **3a and 3b** submit an action ticket to an executor and receive a completion, including `UNKNOWN_AFTER_RESTART`.
- **The connector gate (PD-01.7)** is an LTC provider. The break-glass relay (`BREAK-GLASS.md`) is an alternate provider.
- **The supervisor split (PD-72)** gives the pattern exactly the role LTC calls the "Python Supervisor".

So the right shape is:
1. The LTC stays a cross-cutting contract. The LTC calls itself "not a seventh kernel pillar".
2. Every channel a daemon declares names an LTC channel profile.
3. The battery runs the LTC conformance suite for each declared channel.
4. The pattern adopts the LTC's *bounded-payload rule* for its own reads now. That rule found a real defect here (section 3).

## 2. Three options, weighed

| | **A. Embed the LTC in the pattern** (the pattern owns transport) | **B. Bind at declared edges** (recommended) | **C. Keep them separate** (status quo) |
| --- | --- | --- | --- |
| What it means | The daemon skeleton ships a transport, and its contract includes LTC semantics | The LTC stays its own versioned contract. A daemon's declared connection names an LTC profile, and the battery runs the LTC suite for it | Daemons invent request channels class by class |
| For | One package to review; one version | One transport meaning across daemons, scripts and services (plug-and-play, U-3). The connector gate's conformance is the LTC's CT suite plus section 6's erratic cases, not a bespoke one. Bounds, overflow, fencing and `UNKNOWN_AFTER_RESTART` are defined once. Observers carry no transport code | Nothing to do now |
| Against | Couples two version lines: every LTC change becomes a daemon-contract major, and the reverse. Pulls daemon constants into the LTC, which the markup forbids for WBS 3.1's constants for the same reason. Observers carry transport code they never use (smallest stable kernel). The LTC is also still a v0.1 candidate: a moving target | Needs a seam: a manifest field, register fields and a battery hook. The first real use is a class that is not unlocked yet, so the seam is designed before it is exercised | Every higher class re-derives fencing, backpressure and failure meanings. Section 6 of PD-01 already borrows LTC-07, LTC-10, LTC-11 and CT-02/03/08/20 ad hoc; that drift would continue |
| Risk | High | Low; the seam is small and testable | Medium, growing with each class |

Recommendation: **B**, recorded as PD-82. Adopt the bounded-payload rule inside the pattern now (PD-83). Design the seam now, but build it with the first class that needs a request channel.

## 3. What the markup's rules find in this repository

**LTC-H01 (payload-handling profiles).** The rule "exceeding any declared bound MUST produce a typed failure and MUST NOT yield a successful truncated payload" is the rule this repository learned twice.

| Read | Behaviour against the rule | Status |
| --- | --- | --- |
| `ctx.read_text` | Reads `max_bytes + 1` and raises `TooLarge` | **Conforms** (collect-bounded) |
| Digest rendering | Keeps whole rows and states "*N* rows omitted" | **Conforms**: explicit partial semantics, which the markup allows |
| Ledger writer | Refuses a record over `record_max_bytes` before writing a byte | **Conforms** |
| Ledger recovery | Streams record by record with a per-record bound. No aggregate time bound except systemd's start timeout, which kills the process | **Gap**, already recorded as U-6 (about 2.4 million records exceed git-watch's 120 s start timeout). See the modification to H01 below |
| `ctx.run`, `ctx.git` | Return a truncated result *as a value* (`truncated=True`, `returncode=None`); each author must check | **Does not conform at the framework level.** HF-29 (r3.5) was this: git-watch accepted output over its cap as "unavailable" |
| `ctx.list_dir` | Returns the first *N* names *in directory order*, then sorts them, as a value with `truncated=True` | **Does not conform. HF-33, reproduced and fixed in r4.3** (below) |

**HF-33 (Medium).** `dir-watch` diffed a capped listing as if it were the whole folder. Past 512 entries, the first entries come back in directory order, which shifts when a file is added. Each addition then recorded some *other*, unchanged file as removed:
- 17 false removals in 20 single-file additions on ext4;
- 20 in 20 on tmpfs;
- none of the events carried the truncation flag.

The fix makes a folder over the cap a failed cycle (`TooLarge`), as HF-29 did for Git output, so it counts towards the blind limit and reaches a human within `T`. The tests are in `tests/test_blind.py::DirWatchCapacityTests`. Two of them fail on r4.2; the third is a control at the cap. The reproduction is in `evidence/r4.3/`.

Two defects from one API shape is the argument for PD-83. The framework, not each author, should enforce the rule:
- `ctx.run`, `ctx.git` and `ctx.list_dir` raise `TooLarge` on truncation by default;
- partial results only by explicit opt-in, with a result type that carries the flag into every record built from it.

This changes the meaning of existing `ctx` methods, so it is a contract major (4.0.0). It is best batched with PD-35, PD-39 and PD-49.

**LTC-H02 (authenticated binding).** The pattern already keeps the three things the markup separates:
- **identity:** the manifest and code digests (HF-27, PD-49);
- **compatibility:** a battery PASS;
- **authorization:** a human Class C activation in the PD-40 register.

A candidate carries no authority (PD-26), and there is no self-declared authority field (P3 in `ADJUDICATION-PLUG-AND-PLAY.md`). H02's fields therefore map directly onto the register's design: `trust_policy_id`, `trust_verification_result` and `trust_evidence_ref` (PD-84).

**LTC-H03 (retry and pacing).**
- **Retry.** The skeleton has no transport retry. Its one retry is systemd's restart, bounded by `StartLimitBurst=5` in 300 s and `RestartPreventExitStatus`.
- **Pacing.** PD-21's polling jitter is exactly what H03 reclassifies as pacing for the execution profile. It is uniform random (`os.urandom` seed), ±min(5 % of the interval, 30 s), and zero in test mode. H03 prefers deterministic spreading derived from identity for ordinary background workers, because it can be reproduced and tested (PD-85).
- **Composition.** The repository has evidence that H03's final test is necessary but not sufficient (section 5, H03).

## 4. Requirements for integration (option B)

Proposed, all PENDING. "LI" means LTC integration. These are daemon-pattern identifiers, never LTC numbers.

| ID | Requirement | Source |
| --- | --- | --- |
| **LI-1** | **The ledger is not an LTC channel.** It stays the durable evidence layer (LTC-01). A transport outcome that matters is recorded in the ledger as a typed event. A completion is an observation, never authority | LTC-01, S-6, PD-52 (the ledger and digest are the pattern's only output binding) |
| **LI-2** | **A declared connection names an LTC channel profile:** `contract_id`, `contract_version`, `schema_id`, ordering scope, finite descriptor and inflight-byte bounds, overflow policy (LTC-07), and a payload-handling profile (H01). It attaches to `network.mode: named` / section 6's declared connections. It is a contract major, batched with PD-35, PD-39 and PD-49 | LTC-07, LTC-V2-12, PD-42 |
| **LI-3** | **The binding is resolved at activation, never at run time.** The PD-40 register row *is* the LTC §12 binding output: contract, implementation identity and version, artifact digest, trust result, execution profile, configuration reference, compatibility result. The daemon never consults a registry per message | LTC-03, LTC-V2-07, H02 |
| **LI-4** | **Compatibility and authorization stay distinct:** a battery PASS plus the LTC conformance suite is the compatibility result, and the human activation is the trust result. Neither implies the other | H02, PD-26 |
| **LI-5** | **The supervisor keeps to LTC-14.** When the supervisor split (PD-72) exists, the supervisor starts, stops and restarts processes and reports health upwards. It never reclaims slots, fabricates completions or mutates queues. **Consequence for the Blind Forester (BF-1):** survival outputs are the supervisor's *own* submissions, marked as survival (BF-2), on its own channel. They are never passed off as the worker's completions | LTC-14, BF-1, BF-2 |
| **LI-6** | **Two fences, never confused.** A channel incarnation rotates on every restart of a channel (LTC-10, LTC-V2-10). A break-glass epoch (BG-8) is an *authority* axis. Both rotate at boot, and neither stands in for the other (LTC-V2's axes table) | LTC-10, BG-9 |
| **LI-7** | **Switching to an alternate provider is an explicit, pre-bound rebind.** The break-glass relay is an alternate provider. Under LTC-V2-19, reselection is an explicit X1-authorized bind or rebind, and silent fallback is prohibited. X1 is unreachable in an emergency, so the relay's binding is resolved at activation and held dormant, and the trip is the recorded rebind. The markup puts this *outside* H01 to H03, so it belongs to the Break Glass package | LTC-V2-19, BG-5, markup non-goals |
| **LI-8** | **Bounded payloads inside the pattern (PD-83).** No read the skeleton offers returns a truncated payload as success. `TooLarge` by default; partial results only by an explicit opt-in that carries the flag | H01, HF-29, HF-33 |
| **LI-9** | **Retry bounds compose over time, not per window** (section 5, H03). Every retrying layer declares a lifetime bound or a terminal state, and the battery checks the layers against each other (U-8) | H03, HF-28, U-8 |
| **LI-10** | **Pacing is the execution profile's business (PD-85).** The manifest declares `NONE`, `FIXED_PHASE`, `DETERMINISTIC_SPREAD` or `BOUNDED_JITTER`. The default for background observers is deterministic spread, derived from the daemon's name and the host's identity. Test mode stays zero | H03, PD-21 |
| **LI-11** | **Conformance.** For each declared channel, the battery runs the LTC provider-neutral suite (LTC-V2-02's harness) plus PD-01 §6's erratic-conditions suite. The report binds the LTC contract text's hash, as it already binds the daemon contract | CT-01 to CT-26, LTC-V2-02 |
| **LI-12** | **No constant crosses the boundary.** The pattern's constants (the 64 MB floor, the jitter fraction, the start limit 5/300 s, `T`'s range) never become LTC defaults. LTC qualification thresholds (LTC-V2-17) never become daemon constants | Markup §1; the same guard in reverse |
| **LI-13** | **Telemetry is not authority.** The daemon's heartbeat (PD-70) may summarise channel health (LTC-V2-16), but never gains transport authority | LTC-V2-16 |
| **LI-14** | **Numbering.** LTC-H01 to H03 remain the markup's placeholders. This repository's BG-, OR- and LI- identifiers are not LTC v0.2 numbers; if the LTC adopts any, they are aliased, as PD-75 to PD-81 were | Markup §9, `RECONCILIATION-V01-LINE.md` |

## 5. This review's verdicts on LTC-H01 to H03, as a reviewer

**LTC-H01: APPROVE WITH MODIFICATION** (as the markup proposes), plus:
1. **Every profile field is mandatory.** `NOT_APPLICABLE` is allowed only when declared explicitly, with a reason (reviewer question 2). A silent default is how HF-29 and HF-33 happened.
2. **`STREAM_BOUNDED` needs a total-operation deadline as well as an idle deadline.** Per-unit bounds plus an idle deadline still leave a stream unbounded in total time: this repository's start-up ledger verification is bounded per record and never idle, yet it grows without limit (U-6).
3. **Two more conformance tests:**
   - Exactly at the ceiling succeeds and one byte over fails. The boundary is tested from both sides.
   - A capped partial result offered as a complete one is rejected. That is HF-33's shape: consumption, not only publication, must see the typed failure.
4. The typed failure must appear in the operation's own outcome, not only in telemetry (LTC-V2-16).

**LTC-H02: MODIFY / SPLIT OWNERSHIP** (as the markup proposes), plus:
1. **The evidence names the digest it covers.** Otherwise evidence for one artifact can be replayed onto another.
2. **A trust result has an age and can be revoked.** Record `verified_at` and the revocation state it was checked against (U-5, U-7). Offline verification must reproduce *that* check, not only the signature.
3. **Name every digest by what it covers.** This repository has two manifest digests (file bytes and canonical JSON) that a whitespace edit separates (P5, PD-49).

**LTC-H03: REJECT THE JITTER MANDATE, APPROVE BOUNDED RETRY** (as the markup proposes), plus one finding that strengthens it. **Bounded per layer and budgeted independently is not enough; the bound must hold over time.**

HF-28 in this repository had exactly that: a finite, independent restart budget of 5 starts per 300 s. Yet a daemon exiting `SENSE_BLIND` every `T` > 75 s was restarted forever, because the retries were spaced wider than the budget's window. Each window was within budget; the lifetime was not.

Proposed extra normative sentence: "Every retrying layer MUST have a bound that holds over an unbounded horizon (a lifetime cap, or backoff to a terminal state). Budgets of stacked layers MUST be checked against each other, including their time windows."

Proposed extra test: retries spaced just outside the budget's window must still terminate.

**Answers to the markup's reviewer questions** from this repository's evidence:

| # | Question | Answer from here |
| --- | --- | --- |
| 1 | Is COLLECT/STREAM a stable distinction across provider shapes? | Yes. It is about *retention*, not mechanism. Here it separates `read_text` and `list_dir` (collect) from ledger recovery (stream) |
| 2 | Mandatory fields, or `NOT_APPLICABLE`? | Mandatory, with explicit `NOT_APPLICABLE` plus a reason. Silent defaults caused HF-29 and HF-33 |
| 3 | Termination left to the execution profile? | Yes. Interoperability needs only a typed failure plus resource release. Here the daemon *is* its own execution profile, and it kills the child's process group |
| 4 | Does H02 keep content identity while refusing a bare digest? | Yes, provided the evidence names the digest it covers (modification 1 to H02) |
| 5 | Technology-neutral and offline-verifiable? | Yes, if the retained verification material and the revocation state are themselves digest-referenced |
| 6 | Conformance separate from trust? | Yes. It is already separate here (battery against activation) |
| 7 | Does bounded retry capture the thundering-herd risk? | For amplification, yes. It also needs the bound over time (HF-28) |
| 8 | Is any existing LTC requirement redundant? | LTC-07's "overflow MUST NOT be silent" and §8's "no overflow policy may silently convert a limitation into success" become special cases of H01's rule. Keep both, and have H01 cite them |
| 9 | Final v0.2 numbers? | For the LTC to assign. This repository asks only that BG-, OR- and LI- identifiers are never reused as LTC numbers (LI-14) |

## 6. What changes, by phase

| When | Change | Contract impact |
| --- | --- | --- |
| **Now (r4.3)** | HF-33 fixed in `dir-watch` (example only; version 0.2.0) | None |
| With PD-35, PD-39, PD-49 | PD-83: `ctx` truncation raises by default. PD-85: a `pacing` field | Contract 4.0.0 (a changed method meaning and a new field) |
| With PD-40 (register) | LI-3, LI-4, PD-84: the register row is the LTC binding output | Register design only |
| With PD-72 (supervisor split) | LI-5: the supervisor role under LTC-14 | Design constraint |
| With PD-01.4 or PD-01.5 (first request channel) | LI-2, LI-11: declared LTC channels; LTC suite in the battery | Contract major |
| With the Break Glass package | LI-7: a pre-bound dormant rebind | The Break Glass package, not this one |

## 7. Decisions

**PD-82. Bind, do not embed (option B).** The LTC stays a separate, cross-cutting contract, and the daemon pattern binds to it only at declared connections (LI-1 to LI-7, LI-11 to LI-14).
Recommendation: APPROVE. Decision: PENDING

**PD-83. Bounded payloads in the skeleton (LI-8).** `ctx.run`, `ctx.git` and `ctx.list_dir` raise `TooLarge` on truncation by default; partial results only by explicit opt-in. Contract 4.0.0, batched.
Recommendation: APPROVE. Decision: PENDING

**PD-84. The activation register uses the LTC binding vocabulary** (LI-3, LI-4, H02 with its three modifications).
Recommendation: APPROVE for the register's design. Decision: PENDING

**PD-85. Pacing becomes a declared execution-profile field** (LI-10). The default is deterministic spread. PD-21's random jitter stays available as `BOUNDED_JITTER`.
Recommendation: APPROVE; amends PD-21. Decision: PENDING

**PD-86. Retry bounds hold over time** (LI-9), and this review's H03 finding is submitted to the LTC review as markup.
Recommendation: APPROVE. Decision: PENDING
