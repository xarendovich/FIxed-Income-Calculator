# Review handoff: PD-51, should `event_types` become the L2 attachment unit?

- **Status:** FOR REVIEW by the other reviewers. Nothing here is adopted. The owner asked for this handoff on 2026-10-08 after the KU-33 discussion.
- **The question:** PD-51 (r3.3) deferred cross-implementation event contracts because one implementation per observation existed. Multiple project deployments and reference implementations now exist, but that is **not** the trigger by itself. The question is narrower: when a consumer cites a daemon fact today, what identity does it cite, and what concrete evidence would justify building a shared event contract later?
- **Source:** this pattern at contract 5.0.1; `ADJUDICATION-PLUG-AND-PLAY.md` §§2, 9 (L0–L4); `KERNEL-REGISTER-CANDIDATE-KU-33.md` §2 (the per-head review); the kernel draft's six domains.

## 1. What exists today (facts, no proposal)

| Fact | Where | Consequence |
| --- | --- | --- |
| Every manifest declares `ledger.event_types`: 1–32 names, upper-case, none reserved | `manifest.py`; schema 4 | The set is closed and digest-bound: the manifest digest in every `DAEMON_START` and every report pins it |
| The writer refuses an undeclared event (`EVENT_INVALID`); DB-04 checks provenance against the declared set | `runtime.py`, `battery.py` | A ledger can only ever contain declared names |
| Six **reserved** lifecycle events are shared by every daemon: `DAEMON_START`, `DAEMON_STOP`, `DAEMON_HEARTBEAT`, `DAEMON_ERROR`, `DAEMON_ERROR_CLEARED`, `LEDGER_TAIL_QUARANTINED` | `__init__.py`; `semantics.py` | A shared **internal lifecycle protocol** exists and has multiple readers. That does not by itself justify a general event-contract registry |
| Payload shape is pinned to one qualified implementation by the manifest/code identities | the qualification gate | A consumer can cite `(manifest_sha256, daemon_code_sha256, event_type)` today with zero new mechanism. This is an **implementation citation**, not a cross-implementation semantic contract; contract 6 will also bind the runtime-bundle digest |
| Consumers read a daemon's facts through `status` or the independent verifier, never by parsing the ledger (N-19); `tools/daemon-start.py` extracts one record and verifies nothing | `status.py`, `verifier/` | Any attachment is a **pull** of verified facts, never raw lines and never a push |
| A daemon never pushes; it writes its ledger and nothing else | I-1 | Routing lives on the consumer side, by construction |
| Model consumption of a digest is PD-61, not built | `KERNEL-REGISTER-CANDIDATE-KU-33.md` §2 | A model is a consumer like any other, behind the same attachment, once PD-61 exists |

## 2. `event_types` are citation labels, not yet an L2 contract

Consumers pull verified facts; the daemon pushes nothing (I-1). `event_type` is therefore a useful **citation label** inside one qualified implementation. But the string alone does not define payload shape or semantics across implementations. Today the precise citation is the implementation identity plus the event type. PD-51 should open only when a consumer needs a semantic contract that survives switching between independently qualified implementations.

## 3. Options

| | What it is | Cost | What it buys | What would make it wrong |
| --- | --- | --- | --- | --- |
| **D. Cite one implementation** (zero build; possible today) | A consumer cites `(manifest_sha256, daemon_code_sha256, event_type)`; contract 6 will also include the runtime-bundle digest | None | Exact provenance for the implementation being consumed | It is coupling, **not L2**. Re-qualification or implementation replacement requires the consumer citation to move |
| **A. Names only** (today's state, used as the contract) | Consumers read by `event_type` string | None | Decoupled across implementations by name | Two daemons can emit the same name with different payloads; nothing checks it |
| **B. Shape in the manifest** | `ledger.event_types` entries become `{name, version, payload_schema}`; the judge validates emitted payloads against the declared schema (a new check ID, per PD-78) | Manifest schema 5; one new battery check; `scaffold` update | Each daemon's payloads are checked and digest-bound; a consumer can read the shape from the manifest | Still per-daemon: two daemons must copy the same schema by hand (N-23 drift) |
| **C. A capability registry** | Event-type contracts `{id, version, payload_schema, semantics}` live in one place, **owned by the pattern**; a manifest *cites* `id@version`; the judge checks the cited contract exists and the emitted payloads conform. X1 may cite an id; it does not own the payload schema (X1 is a review registry; a declaration there is not a grant) | A registry, a citation syntax, the check, versioning rules (KECC-style: additive within a major) | The real L2: one definition, many implementations, consumers attach by id | Built before a second implementation exists it is a registry of one, which is paperwork; placed in X1 it would make a review registry own a runtime schema |

## 4. Recommendation

**D now as an implementation citation; C only when the interoperability trigger fires; B never on its own.**

- **Now:** consumers may cite one qualified implementation with option D. `event_type` is a label inside that citation, not a freestanding contract.
- **Trigger for C:** two **independently qualified implementations intentionally claim the same semantic observation**, and a consumer has a real need to switch between them without binding to either implementation. Similar-looking events or two projects existing are not enough. `pressure-watch` and `meminfo-watch` are only candidates until that semantic equivalence is deliberately established.
- **Where C would live if triggered:** in the pattern that enforces the payload contract; X1 may cite an id and never owns the runtime schema. Do not bootstrap a general registry from the lifecycle protocol alone.
- **B alone** adds a schema per daemon without decoupling, which is the drift the owner ruled against (N-23).

**D's cost, stated so it is never mistaken for L2.** A consumer that pins one implementation must move its citation when that implementation is re-qualified or replaced. That coupling is acceptable until the interoperability trigger above actually fires.

**The even simpler thing found by looking back:** option D is not a workaround; it is what the qualification design already produces. Every report and `DAEMON_START` carries the digests. Nobody had said out loud that a consumer may cite them as a contract.

## 5. Future Advise taxonomy — not on the current contract line

Contract 5.x and the planned contract-6 admission change remain **observe-only**. The placements below are preserved as future taxonomy only; none authorizes class 2, a `PROPOSAL` event, proposal envelopes, or model consumption.

| Placement | Verdict | Why |
| --- | --- | --- |
| (i) The model is consulted inside the cycle, in `decide()` | **Reject** | Breaks I-1 (no network, no inference), I-2 (never record a guess), the digest gate (a model's output is not qualifiable), the plug-and-play rule the owner agreed ("no LLM in the trust path"), and the kernel draft ("model judgment is not authority") |
| (ii) A deterministic Advise daemon: `decide()` emits proposal-shaped events by rule, no model | **DEFER** | This changes the semantic class of the resident pattern and introduces proposal semantics, validation, staleness and consumer handling. It is not needed by the observe-only line and must not be prebuilt |
| (iii) A model-backed advisor **outside** the daemon: an agent reads verified facts and produces its own proposal | **Architecturally compatible when separately authorized; not built** | This preserves the current boundary: anything with a model in it is an external agent, not code inside the resident daemon. Its own authority/provenance contract belongs to the agent side |

The current conclusion is simpler: **no replaceable decision maker lives inside the daemon**. A future model-backed advisor, if authorized, is external and consumes verified facts through its own contract.

## 6. What the handoff does not decide

- PD-61 (how a digest reaches a model) stays a prerequisite for (iii). Nothing here builds it.
- KU-33 only names the adjacent owner; it does not gate or authorize an L2 mechanism. Option C remains trigger-based regardless of the kernel owner map.
- Whether `pressure-watch` and `meminfo-watch` are semantically interchangeable is explicitly **not assumed**. That question is asked only if a consumer actually needs to switch between them.

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
