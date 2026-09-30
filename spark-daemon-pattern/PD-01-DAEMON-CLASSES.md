# PD-01 (reopened): daemon classes, consequence levels and cross-class safety invariants

- **Status:** OPEN FOR EXPANSION. Reopened by the owner on 2026-09-29. Nothing in this document is decided; every sub-item is PENDING. New classes, invariants and questions are added below as numbered entries and listed in the expansion log (section 9), and nothing already recorded is rewritten.
- **Supersedes:** the original PD-01 text, "Observe-only in v1. `daemon_class: act` is reserved and refused." That text came with the pattern as received. It was inherited from the Repository Observer (v0.3 §6.2, "observation never authorizes action") and was never ruled on.
- **Code today:** unchanged. The manifest accepts only `daemon_class: observe`, which corresponds to class 1a below. Nothing here unlocks another class until its own sub-decision is recorded.
- **Revision:** r3.6, 2026-09-29, by Claude, from the owner's discussion of the taxonomy, consequence of failure, escalation and authentication under erratic conditions.

## 1. Why reopen it

"Observe-only" was a starting scope, not a law of daemons. It was chosen because an observer is the lowest-risk class on which to prove the skeleton: its worst case is a wrong or missing record. Several recent decisions quietly depend on it:
- stop at `T` (PD-63);
- no push channel;
- never restart a fail-closed exit (HF-28).

Those decisions are right for observers and may be wrong for other classes. This document names the classes, so each decision can say which class it holds for.

## 2. The classifier: two independent axes

1. **Capability:** what the daemon may cause. This is the ladder in section 3.
2. **Consequence:** how bad its failure would be. This is the label in section 4.

They are independent on purpose. A heart monitor only observes, but its failure can hurt someone. "It only observes" must never be read as "it is harmless".

## 3. Axis 1: capability classes (three families, two levels each)

Each step up adds exactly one capability and one new control. **The daemon itself never gains power.** Higher classes get request channels to something separate that holds the authority. So "observation never authorizes action" stays true at every level: a daemon asks, and a gate decides.

| Class | Name | May do | Never does | New control it needs |
| --- | --- | --- | --- | --- |
| **1a** | Recorder | Observe; write its own ledger and digest | Signal, propose or change anything | None beyond today's pattern (built and tested) |
| **1b** | Sentinel | 1a, plus raise a declared, typed, bounded alert that an orchestrator reads (pull) | Push over a network; extend its own `T` | A typed alert format; rate limits; alerts never pause the blind clock |
| **2a** | Advisor | Turn observations into a proposal with deterministic rules | Carry out the proposal | Proposal envelope with no authority; validation before a human sees it |
| **2b** | Analyst | 2a, plus ask an inference engine for a diagnosis or proposal | Carry out the proposal; feed raw observed text to a model without containment | PD-61 (untrusted evidence reaches a model only in delimiters, never as instructions); PD-62 (origin never raises trust); proposals validated like any candidate |
| **3a** | Operator | Request pre-approved, **reversible** actions inside Spark's own domain, through an executor | Hold credentials; act directly; run anything off the runbook | A digest-bound runbook; one H-Track-style ticket per action; the connector gate (section 6); an action ledger; the activation register (PD-40) |
| **3b** | Actuator | Request **consequential** actions (irreversible, external or physical) through an executor | Anything in 3a's "never" column | Everything for 3a, plus a declared safe state, an independent monitor, redundancy, and the relevant standards (for example IEC 61508 or IEC 62304) with independent testing |

**Where a level ends:** the line between neighbouring levels is what the daemon can cause. Anything that changes state outside the daemon's own output directory belongs to family 3, even "restart my own unit". Between 3a and 3b, the line is reversibility and reach: something undone in minutes and confined to Spark is 3a; everything else is 3b.

**Pros and cons, in brief:**

| Family | For | Against |
| --- | --- | --- |
| Observe | Worst case is a wrong or missing record; built today | Cannot help; relies on someone noticing (1b addresses this) |
| Advise | Faster diagnosis; where inference adds value; still changes nothing | Wrong or manipulated proposals; alert fatigue; 2b is not repeatable |
| Act | Closes the loop without waiting for a human | Real harm; a bad fix can spread; hardest to test; accountability |

## 4. Axis 2: consequence levels (a label on every daemon)

| Level | A failure costs | What changes for the daemon |
| --- | --- | --- |
| **Standard** | A missing or wrong record | Stop at `T` and wait for a human (today's rule) |
| **Elevated** | Time or money | Escalate early (1b behaviour at half of `T`, whatever the class); a human is paged, not merely informed |
| **Critical** | Possible harm to a person | A declared safe state instead of "stop"; independent monitor; redundancy. Refused by this pattern until a certification path exists |

## 5. Cross-class safety invariants (never flex, whatever the class or consequence)

Safety invariants hold in every state. Functional policy, such as what to do when blind or how patient to be, varies by class and is decided per class. When a system turns erratic, it may lose functionality; it never loses these.

- **S-1. Never report health you do not have.** The watchdog saying "alive" is never evidence of seeing.
- **S-2. Never record a guess as an observation.** Unsettled or failed cycles are not observations (PD-63), and a capacity limit is not "unavailable" (HF-29).
- **S-3. Always surface blindness within `T`.** No class, escalation or pending diagnosis pauses the blind clock. Only a human, or a rule the human approved in advance, extends `T`, and the extension is recorded.
- **S-4. Stay confined.** The purity check, the audit hook, Landlock and the systemd sandbox apply to every class. Higher classes gain request channels, never wider confinement.
- **S-5. Authentication and handshakes never degrade.** Details in section 6. If authentication cannot complete, or its state is uncertain, nothing is sent. There is no "erratic, so skip authentication" path.
- **S-6. Proposals are not authority.** An inference engine or rule may propose; only a gate with pre-approved rules, or a human, disposes.
- **S-7. Another system's safety protocol wins on its side.** We are a compliant participant in its handoff and never override or work around its interlocks. On any deviation we hand back control by its safe-state rules, not ours.
- **S-8 (proposed in r3.8, PD-65). Never imply more temporal precision than you have.** An observation states the window it covers. A gap longer than the declared cadence is recorded, never folded silently into the next event's timestamp. See `ADJUDICATION-PLUG-AND-PLAY.md` U-4.

*Where these live (proposed in r3.8, PD-64, rule U-1):* the invariants become a base conformance suite that every contract's own suite inherits, so they apply to every plugin in every plane without making the L0 envelope prescribe behaviour.

## 6. Authentication and handshakes when systems are erratic (families 1b to 3)

The design principle is that **the erratic part never holds the keys and never runs the handshake.**

- **A connector gate.** The daemon never holds credentials. A small, separately tested connector gate holds them, runs the other system's protocol exactly, and refuses anything it cannot complete. This follows the simplex architecture (a complex part that may fail, plus a small verified part with the last word), the H-Track ticket, and the transport contract's split: the consumer submits, X1 authorizes, the provider carries it out.
- **Pull, not push, for 1b and family 2.** Alerts and proposals are published for the orchestrator to read, so those classes need no credentials at all.
- **Declared connections for family 3.** Every outward connection is declared in the manifest:
  - a name;
  - the protocol and its version;
  - a credential reference (never the secret);
  - the conformance suite the gate passed for that protocol.

  This is where the reserved `network.mode: named` (PD-42) attaches.
- **Defences against erratic behaviour**, borrowed from the transport contract (LTC):

| Erratic behaviour | Defence |
| --- | --- |
| A restarted or late process continues an old session | Fencing: a session belongs to one process incarnation (LTC-10); after any restart, handshake again, never resume |
| A timeout mid-handshake | The timeout ends the session and never counts as authorization (LTC-11) |
| Retries send a command twice | Idempotency keys and one-time nonces |
| A late or replayed message | Sequence numbers; short-lived tokens bound to one operation (a digest-bound ticket) |
| Outcome unknown after a crash | Report it as unknown and reconcile; never assume success (LTC `UNKNOWN_AFTER_RESTART`) |
| A request flood | Rate limits and bounded backpressure at the gate (LTC-07) |
| Credentials leaking through a crash dump | Credentials live only in the gate's memory with short lifetimes; core dumps disabled in the unit |

- **Proven under erratic conditions.** A connector passes only if every case of this suite fails closed and is recorded truthfully:
  - kill it mid-handshake;
  - replay old tokens;
  - duplicate and reorder messages;
  - skew the clock;
  - crash after sending but before the acknowledgement.

  This is the same approach as LTC CT-02, CT-03, CT-08 and CT-20. Every attempt and outcome goes into an action ledger: typed, bounded, and never containing secrets.

## 7. Sub-decisions (each unlocks one step; all PENDING)

**PD-01.1. Adopt the classifier:** the two axes (sections 2 to 4) and the invariants S-1 to S-7 (section 5), with v1 shipping class 1a only.
Recommendation: APPROVE. Decision: PENDING

**PD-01.2. Every manifest declares a consequence level.** `critical` is refused until a certification path exists. Existing daemons would declare `standard`. This is a contract change, best batched with PD-35, PD-39 and PD-49.
Recommendation: APPROVE. Decision: PENDING

**PD-01.3. Unlock 1b (Sentinel).**
- At half of `T`, write one typed `SENSE_DEGRADED` record per blind streak and update the systemd status line.
- A shared escalation format, `spark-escalation/1` (typed, bounded, no observed free text), used by daemons and scripts alike.
- A read-only `spark-daemon escalations` command that verifies the chain and emits JSON for the orchestrator.
- Alerts never extend `T` (S-3).

This is the smallest step, and it closes the silent-outage problem at its source.
Recommendation: APPROVE. Decision: PENDING

**PD-01.4. Unlock family 2 (Advisor, Analyst) after PD-61 and PD-62 are ruled on.** It needs a proposal envelope with no authority, and validation of proposals before a human sees them. An inference engine (2b) sees only typed escalation records and the verified ledger.
Recommendation: DEFER until PD-61 and PD-62. Decision: PENDING

**PD-01.5. Unlock 3a (Operator) after the activation register (PD-40) and the connector gate (PD-01.7) exist.** It needs a digest-bound runbook of reversible actions, one ticket per action, and an action ledger.
Recommendation: DEFER. Decision: PENDING

**PD-01.6. 3b (Actuator) is out of scope before kernel v2 and a certification path** (PD-60).
Recommendation: DEFER to kernel v2. Decision: PENDING

**PD-01.7. The connector gate:** design and conformance suite as in section 6, built before any class needs an outward connection.
Recommendation: APPROVE the design direction; build when PD-01.5 is approached. Decision: PENDING

**PD-01.8. Scripts adopt the same invariants.** A script's blind period is a run deadline. A script that observed nothing never reports success: it ends with a non-success result, a typed `SENSE_BLIND` reason, and the shared escalation record. The scripts live in the Spark Script Repository, so the exact result and exit code (under SC2) are the script board's decision.
Recommendation: APPROVE the invariant; the script board sets the exit code. Decision: PENDING

**PD-01.9. Critical consequence requires declared blind modes** (section 9b). Where a running process keeps a person safe, blindness is handled by a procedure declared and validated in advance, and executed by the side that still works. It is never handled by a stop, and never by improvisation. This is a precondition for ever accepting `critical` or unlocking 3b. It adds nothing for today's observers, whose declared blind mode is "stop and call a human".
Recommendation: APPROVE as a precondition. Decision: PENDING

## 8. Open questions (for expansion)

1. **Names.** Should these classes align with the script board's tiers (T0 Observe up to TS)? The board's exact tier definitions have not been compared with this ladder.
2. **Class changes.** Can a daemon move between classes at run time, for example becoming a Sentinel only while elevated? Recommendation so far: no. The class is part of the approved manifest, and changing it means re-approval.
3. **Alert delivery.** Is pull (ledger plus systemd status) enough for the elevated level, or does paging a human need a dedicated, declared alert connector behind the gate?
4. **Runbook ownership.** Who writes and approves a 3a runbook action, and how is "reversible" proven and not merely claimed?
5. **Multi-daemon effects.** When two Operators request conflicting actions, does the executor need an interlock, like railway routes?
6. **Certification path.** Which standard, and which independent tester, would make `critical` possible at all?

## 9a. Additions proposed by the prior-art review (r3.7)

`PRIOR-ART-REVIEW.md` sets out twelve proposed amendments (A-1 to A-12) with sources. Each is PENDING and, once ruled on, amends the section named in brackets:
- **A-1:** the IEC 61784-3 black-channel error model for the connector gate [section 6, PD-01.7].
- **A-2:** an independent monitor for every class: a systemd `OnFailure=` notifier and an external staleness check [S-1, PD-01.3].
- **A-3:** consequence levels from a recorded risk assessment [PD-01.2, section 4].
- **A-4:** the NAMUR NE 107 status vocabulary for alerts [PD-01.3].
- **A-5:** ISA-18.2 alarm rules for Sentinels (defined response, priority, flood limits, shelving) [PD-01.3].
- **A-6:** shelving as a bounded, recorded maintenance window extending `T` [S-3].
- **A-7:** internal versus external cause in blind and degraded records (PackML Held and Suspended) [PD-01.3].
- **A-8:** logical supervision for the Act-family executor (AUTOSAR) [PD-01.5].
- **A-9:** a per-daemon list of triggering conditions (ISO 21448 SOTIF) [section 4].
- **A-10:** an instrumented safety case with safety performance indicators (UL 4600) [section 7].
- **A-11:** IEC 62443 conduits and security levels for declared connections [section 6].
- **A-12:** freedom from interference, disk in particular, between daemons of different consequence [section 4].

## 9b. Blind modes for critical consequence (r3.9)

**Source:** the owner's requirement that the design consider "events in which connection and stability of the running process ensures the safety of life on the other end: a way for it to be flown and controlled if blind". Established practice answers consistently:
- **pitch and power:** when airspeed data fails, pilots fly memorised attitude and thrust settings that are safe without the failed sensor;
- **lost link:** a drone that loses its command link executes a pre-set procedure on board (loiter, return home, land);
- **minimal-risk manoeuvre:** an automated vehicle steps down through fallback modes (Autoware, `PRIOR-ART-REVIEW.md`);
- **simplex:** a monitor switches to a verified recovery function (ASTM F3269).

Requirements, for any function whose failure could hurt someone:
- **BM-1. Blind is a declared mode, not an accident.** Before activation, the function declares what it does when blind: sensor-independent settings proven safe across its operating envelope, or a lost-link procedure. The declaration is tested like any other contract.
- **BM-2. The procedure runs on the side that still works.** A lost connection means Spark can command nothing, so the far side (the device or controller) must be able to act safely alone.
- **BM-3. An independent monitor makes the switch, not the blind component.** This is the simplex rule. The monitor shares as little as possible with what it watches (U-7).
- **BM-4. Degradation is graded, one-way, and time-bounded.** Normal leads to a fallback mode with a declared maximum duration, then to a minimal-risk condition. There is no automatic return to normal without revalidation (the same rule as Autoware's comfortable stop to emergency stop).
- **BM-5. Request to intervene, with a no-response path.** A human is asked to take over when the first degradation begins, with a bounded time to respond. No answer moves the system to the next level, never back.
- **BM-6. Reconnection is a new session.** After a lost link, control resumes only after full re-authentication (S-5) and a reconciliation of what happened meanwhile. Outcomes of that period are reported as unknown until proven (LTC `UNKNOWN_AFTER_RESTART`).
- **BM-7. Blind modes are drilled** on a schedule, in production conditions (U-7).

**The boundary for Spark:** no process on the DGX may sit in the loop that keeps a person safe. Spark may observe that loop and advise it. The loop itself runs on an independent controller with its own sensors and declared blind modes. Spark never flies anything blind; it makes sure whatever flies has a declared way to fly blind.

## 9. Expansion log

Add entries at the end; never rewrite earlier ones.

| Date | Entry | By | Summary |
| --- | --- | --- | --- |
| 2026-09-29 | Reopened | Owner | PD-01 reopened for review; the taxonomy discussed as three families with two levels |
| 2026-09-29 | Consequence axis | Owner, Claude | "What if a life depended on it": consequence of failure separated from capability |
| 2026-09-29 | Escalation | Owner, Claude | Blind daemons let the orchestrator know, so inference can propose a fix; proposals are never authority, and escalation never extends `T` |
| 2026-09-29 | Scripts | Owner | The same invariants apply to scripts (PD-01.8) |
| 2026-09-29 | Authentication | Owner, Claude | Connecting to systems with their own safety handoffs: authentication and handshakes never degrade (S-5, S-7, section 6) |
| 2026-09-29 | Prior-art review | Owner, Claude | Robotics, industrial, flight, automotive, operations and AI-agent sources reviewed (`PRIOR-ART-REVIEW.md`); twelve amendments proposed (section 9a); HF-28 description corrected |
| 2026-09-29 | Universal contract | Owner, Claude | Alignment with the L0 to L4 stack and rules U-1 to U-3; four further considerations (U-4 temporal honesty, adding S-8; U-5 stop and revocation; U-6 cumulative bounds, measured; U-7 assurance of the assurance), PD-64 to PD-68 in `ADJUDICATION-PLUG-AND-PLAY.md` section 9 |
| 2026-09-30 | Blind modes | Owner, Claude | Life depending on a running process: blind is a declared mode, run by the side that still works (BM-1 to BM-7, section 9b, PD-01.9); limit-interaction findings and the integration plan in `INTEGRATION-REVIEW.md` |
