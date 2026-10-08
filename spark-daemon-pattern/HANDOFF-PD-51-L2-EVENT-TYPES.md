# Review handoff: PD-51, should `event_types` become the L2 attachment unit?

- **Status:** FOR REVIEW by the other reviewers. Nothing here is adopted. The owner asked for this handoff on 2026-10-08 after the KU-33 discussion.
- **The question:** PD-51 (r3.3) deferred L2 capability contracts for daemon event types because one implementation per observation existed. Two things changed: there are now two consumers (the FirstBorn pilot and the reference set), and the owner's stated intent is that Spark Core and FirstBorn "treat the daemon as the stable contract and the agent as the replaceable decision maker" and that the system "attach the proper head" to a daemon's output. The attachment point has to be named. Is it `event_types`, and what, if anything, must be built?
- **Source:** this pattern at contract 5.0.0; `ADJUDICATION-PLUG-AND-PLAY.md` §§2, 9 (L0–L4); `KERNEL-REGISTER-CANDIDATE-KU-33.md` §2 (the per-head review); the kernel draft's six domains.

## 1. What exists today (facts, no proposal)

| Fact | Where | Consequence |
| --- | --- | --- |
| Every manifest declares `ledger.event_types`: 1–32 names, upper-case, none reserved | `manifest.py`; schema 4 | The set is closed and digest-bound: the manifest digest in every `DAEMON_START` and every report pins it |
| The writer refuses an undeclared event (`EVENT_INVALID`); DB-04 checks provenance against the declared set | `runtime.py`, `battery.py` | A ledger can only ever contain declared names |
| Six **reserved** lifecycle events are shared by every daemon: `DAEMON_START`, `DAEMON_STOP`, `DAEMON_HEARTBEAT`, `DAEMON_ERROR`, `DAEMON_ERROR_CLEARED`, `LEDGER_TAIL_QUARANTINED` | `__init__.py`; `semantics.py` | **An L2 contract already exists for lifecycle**, consumed by `status` and the independent verifier. It is the only one |
| Payload shape is pinned only implicitly: by the digest of the `daemon.py` that emits it | the qualification gate | A consumer that pins `(manifest_sha256, daemon_code_sha256, event_type)` has a fully pinned contract today, with zero new mechanism |
| Consumers read a daemon's facts through `status` or the independent verifier, never by parsing the ledger (N-19); `tools/daemon-start.py` extracts one record and verifies nothing | `status.py`, `verifier/` | Any attachment is a **pull** of verified facts, never raw lines and never a push |
| A daemon never pushes; it writes its ledger and nothing else | I-1 | Routing lives on the consumer side, by construction |
| Model consumption of a digest is PD-61, not built | `KERNEL-REGISTER-CANDIDATE-KU-33.md` §2 | A model is a consumer like any other, behind the same attachment, once PD-61 exists |

## 2. Why "head attachment" reduces to event types

The kernel's heads are consumers. A head attaches to a daemon by **pulling verified facts** about what the daemon emits (through `status` or the verifier; the daemon pushes nothing, I-1), and the only thing a daemon emits is declared event types into a verified ledger. So `event_types` is the **citation unit**: what a consumer names when it says which facts it reads. It is not a subscription and not a channel; no push seam exists in this contract (reviewer correction, 2026-10-08). The attachment unit is already decided by the design; what PD-51 asks is whether a *contract* for an event type should exist independently of the one daemon that emits it, so two implementations can emit the same thing and a consumer can switch between them. That is the whole of L2.

## 3. Options

| | What it is | Cost | What it buys | What would make it wrong |
| --- | --- | --- | --- | --- |
| **D. Pin the implementation** (zero build; possible today) | A consumer cites `(manifest_sha256, daemon_code_sha256, event_type)`. Every digest already exists in `DAEMON_START` and the report | None | A fully pinned contract now; nothing to design | The moment a second implementation of the same observation exists, every consumer must be re-pointed. It is coupling, not a contract |
| **A. Names only** (today's state, used as the contract) | Consumers read by `event_type` string | None | Decoupled across implementations by name | Two daemons can emit the same name with different payloads; nothing checks it |
| **B. Shape in the manifest** | `ledger.event_types` entries become `{name, version, payload_schema}`; the judge validates emitted payloads against the declared schema (a new check ID, per PD-78) | Manifest schema 5; one new battery check; `scaffold` update | Each daemon's payloads are checked and digest-bound; a consumer can read the shape from the manifest | Still per-daemon: two daemons must copy the same schema by hand (N-23 drift) |
| **C. A capability registry** | Event-type contracts `{id, version, payload_schema, semantics}` live in one place, **owned by the pattern**; a manifest *cites* `id@version`; the judge checks the cited contract exists and the emitted payloads conform. X1 may cite an id; it does not own the payload schema (X1 is a review registry; a declaration there is not a grant) | A registry, a citation syntax, the check, versioning rules (KECC-style: additive within a major) | The real L2: one definition, many implementations, consumers attach by id | Built before a second implementation exists it is a registry of one, which is paperwork; placed in X1 it would make a review registry own a runtime schema |

## 4. Recommendation

**D now, C when the trigger fires, B never on its own.**

- **Now:** record that the attachment unit *is* `event_types`, and that consumers pin the digests (option D). This costs nothing, is already true, and gives Spark Core and FirstBorn a precise thing to attach to today.
- **Trigger for C:** the first time two daemons emit the same observation (the pilot's `pressure-watch` and the reference `meminfo-watch` are candidates: both emit memory-band events). At that moment define one contract for it and make both cite it. That is the earliest moment a registry stops being paperwork.
- **Where C lives:** in the pattern, which owns the payload schemas it enforces; X1 may cite an event-type id and never owns the schema (reviewer correction, 2026-10-08; this narrows the earlier PD-46/47 reading that placed it in X1's L0). The lifecycle events are the first entry, and they already have two independent consumers.
- **B alone** adds a schema per daemon without decoupling, which is the drift the owner ruled against (N-23).

**D's cost, stated so it is never mistaken for L2.** A consumer that pins `(manifest_sha256, daemon_code_sha256, event_type)` is bound to *one implementation*: when that daemon is re-qualified for any reason, every consumer must be re-pointed. That is the price of zero mechanism, and it is acceptable exactly until two implementations of one observation exist (the trigger above). Adoption of D is the owner's decision at the close of this review, not a step taken before it (`ADJUDICATION-ALIGNMENT-REVIEW.md`, item 6).

**The even simpler thing found by looking back:** option D is not a workaround; it is what the qualification design already produces. Every report and `DAEMON_START` carries the digests. Nobody had said out loud that a consumer may cite them as a contract.

## 5. The Advise class and *where* the model sits (the owner's open sentence)

The owner confirmed that "agent inside" means an Advise-class daemon (PD-01 class 2: may propose, may not commit), and asked where. Three placements, one recommended:

| Placement | Verdict | Why |
| --- | --- | --- |
| (i) The model is consulted inside the cycle, in `decide()` | **Reject** | Breaks I-1 (no network, no inference), I-2 (never record a guess), the digest gate (a model's output is not qualifiable), the plug-and-play rule the owner agreed ("no LLM in the trust path"), and the kernel draft ("model judgment is not authority") |
| (ii) A deterministic Advise daemon: `decide()` emits `PROPOSAL`-shaped events by rule, no model | **Accept as the daemon-side Advise class, not as a free unlock** | Fully qualifiable; a proposal is a fact in the daemon's own ledger and stays that ledger (no second authority source). Unlocking it is a **named class decision (PD-01 class 2a) plus two things that do not exist**: a proposal envelope that carries no authority, and validation of a proposal before a person sees it. `PROPOSAL` would be a new declared type, never a reserved name (ADM-4). No `PROPOSAL` event exists in this increment |
| (iii) A model-backed advisor **outside** the daemon: an agent reads the daemon's verified facts (PD-61), produces a proposal, records it in **its own** ledger or the activation register (PD-40), never in the daemon's | **Accept as the agent-side Advise path** | The daemon stays pure and resident; the agent is replaceable at any time with no re-qualification, which is exactly the owner's goal. The line is clean: *anything with a model in it is an agent, outside; anything resident and confined is a daemon, deterministic* |

So "the replaceable decision maker inside the daemon" becomes two things with two names, and the replaceability the owner wants comes from (iii), where it is free, rather than (i), where it is forbidden.

## 6. What the handoff does not decide

- PD-61 (how a digest reaches a model) stays a prerequisite for (iii). Nothing here builds it.
- The owner for L2 contracts is the adjacent owner KU-33 names; until that row is recorded nothing in §3 should be built (`KERNEL-REGISTER-CANDIDATE-KU-33.md` §2, last paragraph).
- Whether the pilot's `pressure-watch` and `meminfo-watch` are "the same observation" is a judgement for the trigger in §4, made when FirstBorn migrates to contract 5.

## 7. Questions for reviewers

1. Is option D's "cite the digests" acceptable as the attachment contract for Spark Core today, or does Spark need a name-level contract (A/C) before it attaches anything?
2. Is the trigger in §4 the right one, or should C exist before the first consumer attaches, to avoid re-pointing later?
3. Does the (ii)/(iii) split in §5 match PD-01's class ladder as the owner intends it, or is a model-backed advisor a *third* thing that needs its own class?
4. With C pattern-owned, is a cited id in X1 (no schema there) enough for the kernel review to refer to an event type, or does the register need to hold more?

## 8. Markup table

| # | Item | Agree | Modify (how) | Reject (why) |
| --- | --- | --- | --- | --- |
| 1 | `event_types` is the attachment unit | | | |
| 2 | Option D now | | | |
| 3 | Option C at the trigger, pattern-owned (X1 cites, never owns) | | | |
| 4 | B alone rejected | | | |
| 5 | §5 placement (i) rejected | | | |
| 6 | §5 (ii) deterministic Advise daemon: class decision plus envelope and validation | | | |
| 7 | §5 (iii) model-backed advisor outside, own ledger | | | |
| 8 | PD-61 before any model attaches | | | |
