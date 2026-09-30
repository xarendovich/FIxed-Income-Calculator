# Break Glass for external connections, and operator-gated recovery

- **Status:** PROPOSED. Nothing here is built, and nothing changes behaviour. Every requirement is PENDING under sub-decisions PD-01.10 to PD-01.14 (`PD-01-DAEMON-CLASSES.md` §7). Today's daemons are observers (class 1a): they have no break glass, and their blind mode stays "fail closed and call a human".
- **Revision:** r4.2, 2026-09-30, by Claude.
- **Asked:** the owner proposed a "Break Glass Protocol" with three mechanisms:
  - Dormant Egress Relaxation;
  - a Sovereign Survival Socket;
  - Pre-Signed Capability Envelopes.

  The owner then added three controls:
  - an emergency epoch with fencing;
  - an independent two-signal witness;
  - an Emergency Flight Recorder with mandatory reconciliation.

  Finally the owner added an operator-gated recovery boundary: `SAFE_TO_ISOLATE`, a restart that starts a new epoch, and `REJOIN_PROBATION`.

  Two constraints come with it:
  - "An AI model cannot unilaterally decide that an emergency exists." The owner cites Governing Principle 10: "Do not let model judgment automatically confer state-mutating authority."
  - The mechanisms "should never be used for internal spark-core systems, only used to help spark-core connect to external systems when absolutely necessary."

This document evaluates all of it against what is recorded here: S-1 to S-8, section 6's connector gate, BM-1 to BM-7, and the Blind Forester's BF-1 to BF-6. It merges the result into one design with numbered requirements.

## 1. Verdict in brief

| Proposal | Verdict | Why, in one line |
| --- | --- | --- |
| Emergency is a deterministic, pre-authorized reflex; no model decides | **Adopt** (BG-1) | It is S-6 applied to the most dangerous moment |
| External systems only; never internal spark-core | **Adopt, and enforce it mechanically** (BG-2) | A written rule alone would be one mistaken manifest away from an internal back door |
| Dormant Egress Relaxation | **Adopt, modified** (BG-5) | Landlock cannot be relaxed once applied, so the egress must already belong to a separate, dormant relay |
| Sovereign Survival Socket, raw and unauthenticated in `/tmp` | **Reject as first written.** **Adopt the owner's revised form**, with two additions (BG-6) | Unauthenticated contradicts S-5, and `/tmp` can be squatted. The revision is epoch-bound, in a protected directory, with an identified peer, and destroyed at fence |
| Pre-Signed Capability Envelopes | **Adopt, with conditions** (BG-7) | This is BF-4 made concrete. The envelope *is* the pre-authorization, so nothing is bypassed |
| Emergency epoch and fencing | **Adopt; the most important addition** (BG-8 to BG-11) | It answers "who ends emergency authority, and how is that proven?" |
| Independent witness, two signals | **Adopt, with an independence test** (BG-12 to BG-14) | Without it, whoever can stop a heartbeat can manufacture an emergency |
| Emergency Flight Recorder and mandatory reconciliation | **Adopt, write-ahead** (BG-15 to BG-18) | Break glass bends normal assumptions, so its record must be stronger than normal logging |
| Operator-gated recovery: `SAFE_TO_ISOLATE`, a new epoch, `REJOIN_PROBATION` | **Adopt as first-class states** (OR-1 to OR-8) | "The emergency has ended" and "ready for normal operation" are different events |

**Added by this review** (each explained below):
1. **Two tiers.** A *signal* tier (an emergency beacon) and an *act* tier. The owner's external-only rule places the act tier in class 3b, which is deferred to kernel v2. The signal tier can come much sooner.
2. **Fencing must reach the external target.** Advancing an epoch on the DGX does not stop a partitioned orchestrator from commanding the same external system.
3. **Every boot opens a new epoch.** Then emergency authority dies even if power is cut before the fence completes.
4. **A rollback defence.** Restoring an old disk image would otherwise restore an old epoch.
5. **An unplanned restart mid-emergency ends break glass.** It does not end the survival loop. This is stated as a deliberate cost.
6. **Emergency authority has a hard maximum duration,** even if the orchestrator never returns.
7. **The recovery panel shows evidence, not claims.** "Emergency cause: cleared" becomes "trigger not observed since …".

## 2. The premise and the scope

### 2.1 A reflex, not a judgment

**BG-1. Only mechanical signals can start an emergency.** The trigger is a fixed Boolean rule over a closed list of mechanical signals: heartbeat absent for a declared duration, watchdog pre-timeout, a declared sensor threshold. The rule is declared in the manifest, bound to its digest and drilled.

No model output, proposal, log text or observed free text is an input to the trigger, to the witness or to the choice of action. An inference engine may *explain* an emergency afterwards (class 2b). It never starts one, extends one or picks what happens in one.

This is Governing Principle 10 as the owner quoted it, and S-6 ("proposals are not authority") at the moment it matters most.

### 2.2 External only, enforced

**BG-2. Break glass reaches only declared external targets.**

A break-glass target must be a connection declared in the manifest (section 6: name, protocol and version, credential reference, conformance suite) and marked `scope: external`. This is where `network.mode: named` (PD-42) attaches.

The manifest validator refuses any break-glass declaration whose target is internal. For this rule, internal means any of:
- loopback, link-local, or the host's own addresses;
- any Unix socket or path under Spark's directories;
- the orchestrator, the registry, the activation register (PD-40) or any other Spark control-plane endpoint;
- another daemon's ledger, unit or socket;
- anything in the spark-core zone, once zones exist (A-11, IEC 62443 zones and conduits).

**BG-3. The relay checks the scope again at connect time.** The validator sees names; the relay sees addresses. A name that resolves to an internal address at connect time is refused, so a changed DNS record cannot turn an external target into an internal one. The relay pins the address it validated and records it in the flight recorder.

**BG-4. Break glass never touches Spark's own confinement or authentication.** It never widens the worker's Landlock domain, seccomp filter or systemd sandbox (S-4). It never changes a ledger rule. It never relaxes any internal authentication (S-5). The emergency socket's commands address the break-glass state and the relay only, never an internal subsystem.

Inside Spark, loss of the control plane is handled by the rules that already exist:
- fail closed (observers);
- the Blind Forester's survival loop (active classes);
- escalation.

It is never handled by break glass.

*Analogy:* a fire door with a crash bar. It opens outward without a key, it sounds an alarm when used, and it does not open any office on the way out.

### 2.3 Where this sits on the capability ladder: two tiers

The owner's scope rule has a consequence that is worth stating plainly. Under PD-01 §3, an action whose effect is external is class **3b** (Actuator), whatever its size, and 3b is deferred to kernel v2 and a certification path (PD-01.6). Break glass that *acts* on an external system therefore cannot be built before 3b.

Not all break glass acts. Splitting it into two tiers lets the useful half come much sooner:

| Tier | What it does | Class | Needs | When |
| --- | --- | --- | --- | --- |
| **BG-S (signal)** | Sends a fixed, typed distress message (`spark-escalation/1`, no free text) to one declared external endpoint, such as a pager service or a secondary controller. It is outbound only and cannot be commanded | 1b over a declared connector (open question 3 in PD-01 §8) | PD-01.3, the connector gate (PD-01.7), the witness, the flight recorder, epochs | After PD-01.3 and PD-01.7 |
| **BG-A (act)** | Executes a pre-signed survival manoeuvre on an external system | 3b | Everything for BG-S, plus envelopes, the supervisor split (PD-72), the activation register (PD-40) and a certification path | Kernel v2 |

*Analogy:* an aircraft's emergency locator transmitter. It fires on a physical trigger (a g-switch) and transmits a fixed signal that nobody can command. That is BG-S. Flying the aircraft is BG-A.

## 3. The three mechanisms

### 3.1 Dormant egress: the relay is pre-granted, never relaxed

**The problem as proposed.** "The supervisor relaxes Landlock to a pre-validated emergency endpoint" cannot be done:
- A Landlock domain can only gain restrictions; nothing can lift one once it is applied (`ADJUDICATION-AP.md`).
- Landlock's network rules (ABI 4 and later) filter TCP bind and connect by *port* only. They cannot name a host.

**BG-5. The emergency egress belongs to a separate break-glass relay, pre-granted and dormant.**
- The relay is its own unit, with systemd `IPAddressDeny=any` and `IPAddressAllow=` set to exactly the declared endpoint's addresses. It has no other egress.
- It holds that egress permanently but refuses to use it outside an active, witnessed emergency epoch. So "dormant" is a policy check inside a small, separately tested program, not a permission that gets granted at the worst moment.
- The worker's confinement never changes. The worker can reach the relay only through the supervisor, never directly.
- "Restore egress restrictions" at fence means the relay stops (its process exits). An exited process is the one state the owner's recovery panel can verify from outside.

The alternative, starting the relay only when break glass trips, removes the standing egress but needs something privileged enough to start units at the worst moment. The dormant form is recommended; the choice is recorded in PD-01.11.

**"Bypassing the two-phase tool gate."** The act tier does not bypass authorization; it moves it earlier. Phase one (approval) happened when a human approved the runbook from which the envelope was signed (BG-7). Phase two (the check before execution) is the relay's verification of the envelope, the epoch and the witness decision. Both phases still happen. The signal tier needs no tool gate, because it cannot act.

### 3.2 The survival socket: identified, not unauthenticated

**The problem as first written:** `/tmp/spark-emergency.sock`, raw and unauthenticated.
- **Squatting.** `/tmp` is world-writable. Any local user can create that path first. The sticky bit then stops Spark from removing it, and the "controller" talks to an impostor.
- **Anyone can connect.** Any local process, including a compromised one, can connect to an unauthenticated socket.
- **Invisible under the sandbox.** Under the unit's `PrivateTmp=`, a socket in `/tmp` is not visible to anyone outside the unit, so a technician could not find it anyway.
- **A matched pair.** Combined with a trigger an attacker can induce (stop the heartbeat), an unauthenticated control socket gives an attacker the emergency on demand and a command port during it.
- **The invariants.** It contradicts S-5 (authentication never degrades) and BM-6 (reconnection is a new, fully authenticated session).

**The owner's revision** fixes the dangerous parts. The socket:
- exists only during a valid emergency epoch;
- sits in a protected runtime directory;
- accepts only an identified local controller;
- is destroyed at fence.

**BG-6 (the revision, with two additions).**
- **Where it lives.** `/run/spark-emergency/<daemon>/`, mode 0750, owned by the supervisor's user and a dedicated `spark-emergency` group. It is created when the epoch opens and removed at fence. Clients that hold the old path fail at the next connect.
- **Who may connect.** The kernel supplies the peer's user, group and process ID on every connection (`SO_PEERCRED`), so a declared controller identifies itself with **no friction**: no password, no token exchange, nothing to forget under stress. A connection from any other identity is refused and recorded.
- **Addition 1: a closed, mostly read-only command set.** The commands are `status`, `hold`, `safe-stop` and `acknowledge`. `status` is free. The others are recorded in the flight recorder before they take effect. No command executes arbitrary input, and none addresses an internal subsystem (BG-4).
- **Addition 2: a human at the machine needs presence plus a key.** A technician who is not a declared process proves physical presence with a pre-provisioned hardware key checked locally, with no network needed. That is the "sovereign" property the owner wanted: it works when every network and the orchestrator are down. Low friction comes from pre-provisioned credentials, never from having none.
- **At fence,** the socket is removed and every open session is closed. A session belongs to one epoch (LTC-10 fencing).

### 3.3 Pre-signed envelopes: the pre-authorization itself

**BG-7. A survival-manoeuvre envelope is a digest-bound, single-use, epoch-bound ticket.**

**Contents:**
- `envelope_id`, a nonce;
- `parent_epoch` (see BG-9);
- the daemon's manifest and code digests;
- the declared external connection it targets (BG-2);
- one action from the manifest's closed survival list, with parameter bounds;
- `not_before` and `not_after`;
- `max_uses`, normally 1;
- the reference of the human approval it was issued under.

It never holds a secret.

**Issued only from a human-approved runbook.**
- The orchestrator signs only envelopes whose action template a human approved at activation (BF-4, PD-40). The approval is what carries authority; the orchestrator's signature is a delivery mechanism.
- This keeps Principle 10 intact even though the orchestrator may itself use a model: the model can choose *when* to refresh an envelope, never *what* an envelope permits.

**Verified by the supervisor and the relay, never the worker** (BF-1).
- The verification key comes from the activation register.
- The signing key is never on the worker's side of the host.

**Spent write-ahead.** The envelope's use is recorded in the flight recorder *before* the action runs, so a crash and restart cannot replay it. If the relay crashes after the action is sent but before it is acknowledged, the outcome is recorded as unknown, never as success (LTC `UNKNOWN_AFTER_RESTART`).

**Accepted by the far side, where it has a protocol.** Under S-7, the external system's own safety protocol wins. A survival manoeuvre must be one that the external system's operators have agreed to in advance, as air traffic control knows what a pilot squawking 7600 will do: the pilot flies the pre-filed lost-communications procedure.

## 4. The controls that bound them

### 4.1 Emergency epochs and fencing (EEF)

The owner's invariant: **emergency authority cannot survive the emergency that created it, nor the recovery boundary that ended it.**

**BG-8. Every grant of authority belongs to exactly one epoch.**
- An epoch is a counter that only ever increases, stored durably in the supervisor's state.
- Every emergency capability carries its epoch, the daemon ID, the activation reason, `expires_at` and `max_uses`. This covers envelopes, socket sessions, relay activation and the witness's decision.
- Every verifier reads the *current* epoch from durable state at each use, never from a cached copy. It refuses anything from another epoch.

**BG-9. What opens a new epoch.**
- **Break glass tripping.** Emergency epoch `N+1` records its parent, normal epoch `N`.
- **The fence.** It writes epoch `N+2`.
- **Every boot, whatever the reason** (this review's addition). On a restart after a power cut in the middle of break glass, the supervisor's first durable act is to write a new epoch. The interrupted emergency is then fenced by the restart, even though its fence never ran. Its outcomes are marked unknown, and the start goes to quarantined recovery (OR-2), not to probation. This makes the owner's invariant hold even when power is cut *before* `SAFE_TO_ISOLATE`, not only after it.

**Envelopes and epochs.** Envelopes are signed during normal epoch `N` and name it as `parent_epoch`. They are valid only in an emergency epoch whose recorded parent is `N`.

Two consequences follow:
- **Re-issue after each start.** After any restart, envelopes from before the restart are dead, and the orchestrator re-issues them when probation completes. Until then, break glass has no act tier.
- **An unplanned restart during an emergency ends break glass.** The Blind Forester's survival loop still resumes, as PD-70 requires, because survival mode is the daemon's own pre-validated behaviour. Break glass is *extra* authority, and that does not survive a restart. What protects the payload from then on is the far side's own procedure (BF-5, BM-2).

  This is the conservative choice. The alternative (an emergency that was fenced by a crash may trip again with its old envelopes) is weaker, and it is recorded in PD-01.12 for the owner.

**BG-10. Fencing must reach the external target (split brain).** The owner is right that the orchestrator may be alive but partitioned. The daemon's epoch then stops the *daemon's* stale authority, but not the orchestrator's: while the daemon acts in epoch 419, the orchestrator may still send epoch-418 commands to the same external system. The standard answer is a fencing token that the *resource* checks (Chubby's sequencers; Kleppmann's "How to do distributed locking").
- Every command to a break-glass target, normal or emergency, passes through one connector gate. The gate enforces "the highest epoch wins": once 419 is open, commands carrying 418 are refused; after the fence to 420, commands carrying 419 are refused.
- If the external system can be reached by another path that bypasses the gate, or cannot check an epoch itself, break glass must not be declared for it. The validator requires the connection to declare `single_path: true`, and the gate's conformance suite tests it.

**BG-11. Rollback defence.** Restoring an older disk image would restore an older epoch, and with it envelopes that had died. Three defences, from minimal to strongest:
1. Envelopes carry a short wall-clock `not_after`.
2. The witness keeps its own copy of the epoch, and the two copies must agree before the witness answers yes.
3. Where the host has one, the epoch is held in a hardware monotonic counter (a TPM NV counter).

### 4.2 The independent witness

**BG-12. Break glass trips only when a witness agrees.** The rule is two independent signals at the least, or 2-of-3 where a physical signal exists, as in the owner's example. This is the voting architecture of IEC 61508's 2oo3 systems.

The witness answers exactly one question: *may emergency mode begin, yes or no?*

It:
- chooses no action;
- generates no command;
- reads no prompt;
- expands no capability;
- never sees free text.

Its "yes" is itself an epoch-bound capability (BG-8). It is recorded, and it is rate-limited (at most a declared number of trips per day). A "no" is recorded too, and the daemon then stays under its own blind rules.

Separation of roles:
- the orchestrator decides normal actions;
- the daemon executes permitted actions;
- the witness authorizes the move into the emergency state.

**BG-13. Independence is shown, not assumed** (U-7, BM-3).
- **Shared cause means one signal.** Two signals that one fault can produce are one signal: for example, both measured by the supervisor, or both carried over the same network link.
- **The witness's own base.** It runs on hardware or at least a process tree that shares as little as possible with the supervisor, with its own clock source.
- **A hardware watchdog as a signal.** On expiry, a hardware watchdog normally *resets the host*, which ends everything. The usable signal is its pre-timeout warning, or an external device that watches the link.
- **The physical signal** comes from a declared sensor, never from a model or a log.
- **Audit.** The independence claim is part of the safety case and is audited (A-10).

**BG-14. Stopping a heartbeat must not buy an attacker anything they want.** The witness raises the cost of a false trip; it cannot remove it. The design limits what a forced trip yields:
- the signal tier sends a fixed message;
- the act tier can do only what pre-signed, single-use, bounded envelopes allow;
- the socket admits only declared identities;
- every trip is recorded and counted.

### 4.3 The Emergency Flight Recorder (EFR)

**BG-15. A separate, append-only, hash-chained record, written by the supervisor.** It is separate from the daemon's ledger and uses the same format (JCS records, a chain, one fsync per record). The worker has no write access to it. It holds the owner's events:
- `EmergencyActivation`: the triggers, the witness decision, the last orchestrator sequence number;
- `EmergencyCapabilityUsed`: the envelope and capability hash, the target, the result;
- `EmergencyEgressOpened`: the resolved address, the policy, bytes sent and received;
- `EmergencySessionOpened`: the interface and the peer's identity or proof of presence;
- `EmergencyRecovered`: the recovered orchestrator epoch, the final emergency sequence number.

This review adds `EmergencyFenced`, `EmergencyExpired` and `OperatorDecision`. Each record states its time three ways: the boot ID, a monotonic clock and a wall clock. A monotonic clock alone restarts at every boot and cannot be compared across restarts (S-8).

**BG-16. Write-ahead: no record, no action.** An emergency action is recorded as an intent before it runs, and its outcome after, or `UNKNOWN` after a crash. So that "no record" never happens because the disk is full, the recorder's space is preallocated at activation, sized for the declared maximum trips multiplied by the maximum record size (A-12, freedom from interference on disk).

**BG-17. Sealed and anchored.** At fence, a seal record carries the head of the chain. At reconciliation, the head hash is sent to the orchestrator and to the witness. Anyone who later edits the recorder breaks a hash that someone else already holds. The seal is checked by a verifier that does not import `spark_daemon` (B-6, U-7). As with a ship's voyage data recorder, the record is more valuable than the moment, so it is protected as the more valuable thing.

**BG-18. No secrets and no observed free text in the recorder.** The same rule applies as for the action ledger in section 6.

## 5. The lifecycle: one state machine

The owner's two drafts, the lifecycle with `SAFE_TO_ISOLATE` and the one with `QUARANTINED_RECOVERY`, describe the same stretch in different detail. They merge as follows. "Quarantined recovery" is the stretch from the fence to `SAFE_TO_ISOLATE`.

```text
NORMAL (epoch N)
  │ trigger rule true (mechanical signals only, BG-1)
  ▼
BREAK_GLASS_PENDING ──── witness NO / rate limit ───► stays under its own blind rules (fail closed, or Blind Forester); recorded
  │ witness YES (BG-12)
  ▼
BREAK_GLASS_ACTIVE (epoch N+1)            relay armed, socket created, envelopes with parent N usable
  │                    │
  │ authenticated      │ max duration reached, orchestrator never returned (OR-5)
  │ recovery (OR-1)    ▼
  │               EMERGENCY_EXPIRED ─► fence ─► the far side's procedure; human
  ▼
RECOVERY_DETECTED
  │ FENCE: write epoch N+2 first; then stop the relay, remove the socket,
  │        spend the remaining envelopes, seal the flight recorder (OR-2)
  ▼
QUARANTINED_RECOVERY (epoch N+2)          observation only; no normal commands to affected targets
  │ reconcile: replay the recorder to the orchestrator; re-observe the actual external state;
  │            unknown outcomes stay unknown (OR-3)
  ▼
SAFE_TO_ISOLATE ───────────────── HUMAN BOUNDARY (OR-4) ─────────────────┐
  │ human: disconnect / restart                                       │ human: hold for inspection
  ▼                                                                   ▼
OFFLINE / ISOLATED                                            HELD (no timeout back to normal)
  │ boot: new boot_id, new runtime nonce, epoch N+3 (BG-9)
  ▼
REJOIN_PROBATION (OR-6)                   no break glass; restricted capability
  │ every check passes, and accepted cycles as the declared rule requires (BF-6)
  ▼
NORMAL (epoch N+3); envelopes re-issued with parent N+3

Any crash or power loss from BREAK_GLASS_ACTIVE onward → boot → new epoch, FENCED_BY_RESTART,
outcomes UNKNOWN → QUARANTINED_RECOVERY (never straight to probation).
```

**OR-1. Recovery is authenticated.** "The orchestrator is back" is a full new session (S-5, BM-6). A forged recovery message cannot end an emergency, because ending one early could drop the payload.

Fencing *authority* early is always permitted, because it moves in the safe direction for authority. The survival loop, however, is not ended by recovery. It ends only through OR-6 and BF-6.

**OR-2. The fence comes first, and it is one durable write.** The owner's diagram lists four fence steps:
- revoke the envelopes;
- close the socket;
- restore egress;
- seal the recorder.

The order matters. Writing epoch `N+2` *is* the revocation, because every verifier checks the current epoch (BG-8). The other steps are cleanup that the new epoch has already made harmless. So a crash halfway through a fence leaves no live authority.

**OR-3. Reconciliation is a required transition, not a log step.** The orchestrator's model of reality may be stale (the owner's example: it believes `valve_closed = true` when the emergency opened it). Reconciliation therefore:
- replays the flight recorder to the orchestrator;
- *re-observes* the external state, trusting neither model;
- leaves every unconfirmed outcome as unknown.

Until reconciliation completes, the gate refuses the orchestrator's normal commands to any target the emergency touched.

**OR-4. `SAFE_TO_ISOLATE` is a supervisor state, and the dashboard only presents it.**
- **When the human crossing is required.** The deployment declares `recovery_gate: operator` or `automatic`. `critical` consequence requires `operator`.
- **The daemon never crosses the boundary itself.** With `recovery_gate: automatic`, the crossing is a rule the human approved at activation, and it is recorded.
- **Nothing times out back towards normal.** `HELD` has no timeout. A request to intervene that goes unanswered moves the system deeper, never back (BM-5).
- **The decision is recorded.** The human's choice is authenticated (S-5) and recorded as `OperatorDecision`: who, when, and which option.
- **Precedent.** ISO 13850 and IEC 60204-1 require that resetting an emergency stop does not by itself restart the machine. The human boundary is the same rule, and the physical disconnect is lockout and tagout.

**OR-5. Emergency authority has a hard maximum duration.** The manifest declares it, and it is measured on the monotonic clock from the trip. When it passes, the supervisor fences the emergency even if the orchestrator never returns, and hands over to the far side's procedure. Emergency authority must not outlive its reason just because nobody came back (BM-4: graded, one-way, time-bounded).

**OR-6. `REJOIN_PROBATION` has a closed checklist.**
- The registry and orchestrator are reachable.
- Normal authentication is valid.
- The Landlock policy is loaded (confirmed from the process, not assumed).
- The ledger and flight recorder verify, using the independent verifier.
- External and actuator state has been re-observed.
- The event sequence is synchronized.
- The declared number of accepted cycles has been reached (BF-6).

During probation there is no break glass (no valid envelopes exist), and capability is limited to observing and alerting (1a and 1b). A failure during probation goes back to quarantined recovery. Another emergency during probation is handled by the far side's procedure alone. This is a stated cost, recorded in PD-01.14.

**OR-7. The panel shows evidence, not verdicts.** The owner's panel is right to avoid a single "SAFE". Three rows should also say only what software can know:

| The owner's row | Proposed wording | Why |
| --- | --- | --- |
| Emergency cause: CLEARED | Trigger condition: not observed for 14 min | Software sees the trigger go away, not the cause. The cause is a human finding (S-1) |
| Physical state snapshot: COMPLETE | Physical state: captured 09:41:07Z (3 min ago) | A snapshot has an age (S-8) |
| Orchestrator state: RECONCILED | Reconciled: 7 confirmed, 1 UNKNOWN | Unknowns are shown, never folded into "reconciled" |

Each row is computed from the ledger and the flight recorder by the independent verifier, not reported by the supervisor about itself (U-7). The status line reads "software conditions for isolation are met", which is the owner's own narrower, auditable assertion.

**OR-8. Entry is fast and exit is slow, on purpose.**
- **Entry:** automatic, deterministic and fast.
- **Exit:** deterministic fencing, then reconciliation, then the human boundary where declared, then fresh initialization, then probation.

Broad authority is never restored just because a heartbeat came back.

**The human boundary also closes the Blind Forester loop.** BF-6 requires revalidation before leaving survival mode. This review recommends that the same path, from quarantined recovery through probation, be the only exit from *any* survival or emergency mode (PD-01.14).

## 6. How it fits what exists

| Existing piece | Relationship |
| --- | --- |
| Blind Forester (PD-01.9, BF-1 to BF-6) | Survival *mode* is the daemon's own pre-validated behaviour and resumes across restarts (PD-70). Emergency *authority* is extra and dies with its epoch. The two are kept apart on purpose |
| PD-70 heartbeat | Written to the daemon's ledger for someone to pull; there is no acknowledgement path. A "heartbeat lost" trigger needs the orchestrator to acknowledge heartbeats where the supervisor can read them. That is new |
| Supervisor/worker split (PD-72) | Required. The supervisor holds the epoch, runs the fence, verifies envelopes and writes the flight recorder (BF-1) |
| Connector gate (PD-01.7, A-1, A-11) | The relay is the gate's emergency form. The black-channel error model (A-1) and the conduit definitions (A-11) apply to it |
| Activation register (PD-40), stop and revocation (U-5, PD-66) | The register holds the approvals, the verification keys and revocations. The fence is a revocation with a tested meaning |
| `network.mode: named` (PD-42) | Break-glass targets are named connections with `scope: external` and `single_path: true` |
| Independence (U-7, BM-3), freedom from interference (A-12) | Witness independence, the independent seal verifier, and the recorder's preallocated space |
| Today's code | Nothing changes. Observers declare no break glass. The event names above are proposals; reserving them would be a contract change, best batched with PD-35 |

## 7. Edge cases

| Case | What happens |
| --- | --- |
| Heartbeat stopped deliberately by an attacker | The witness's second signal is absent, so nothing trips. Even if both signals are forced, the attacker gets only a fixed beacon or single-use bounded envelopes, and the trip is recorded (BG-14) |
| Orchestrator alive but partitioned | The gate's "highest epoch wins" refuses its epoch-`N` commands to break-glass targets during the emergency (BG-10) |
| Power cut before `SAFE_TO_ISOLATE` | The boot writes a new epoch first, so emergency authority is dead. Outcomes are `UNKNOWN`, and the start goes to quarantined recovery (BG-9) |
| Crash between sending an action and its acknowledgement | The intent is already recorded; the outcome is `UNKNOWN`; reconciliation re-observes (BG-16, OR-3) |
| Stolen envelope | Useless outside its parent's emergency epoch, after `not_after`, after one use, or outside the gate (BG-7, BG-8) |
| Old disk image restored | `not_after`, the witness's copy of the epoch, and a hardware counter where available (BG-11) |
| DNS changed to point at an internal address | The relay refuses at connect time (BG-3) |
| The orchestrator never returns | The maximum duration fences the emergency, and the far side's procedure takes over (OR-5) |
| A second emergency during probation | No break glass; the far side's procedure. A stated cost (OR-6) |
| A human pulls the power at `SAFE_TO_ISOLATE` | Authority was already dead at the fence. That is the owner's point, and the design keeps it |
| The recorder cannot write | No action. Preallocation makes this a hardware fault, not a full disk (BG-16) |
| A model "concludes" there is an emergency | Not an input to anything that trips (BG-1). At most, the orchestrator can page a human, which is a 1b alert |

## 8. Build order

Nothing here comes before the pieces it stands on. In order:
1. PD-01.3: the Sentinel and `spark-escalation/1`.
2. PD-72: the supervisor split.
3. PD-01.7: the connector gate and its conformance suite.
4. **Then BG-S:** the beacon tier, with epochs, the witness, the flight recorder and the recovery states. This is where the state machine is first built and drilled (BM-7).
5. PD-40: the activation register.
6. **BG-A** with class 3b, in kernel v2.

The recovery states (section 5) are worth building with BG-S, even though the beacon cannot act. That way they are proven before anything that acts depends on them.

## 9. Sources

- ISO 13850 and IEC 60204-1: an emergency-stop reset must not by itself restart the machine.
- Lockout and tagout: OSHA 29 CFR 1910.147.
- IEC 61508: MooN voting architectures (2oo3).
- Fencing tokens: Burrows, "The Chubby lock service" (sequencers); Kleppmann, "How to do distributed locking" (2016).
- Lost-communications procedure: 14 CFR 91.185, transponder code 7600.
- Emergency locator transmitters; flight and voyage data recorders.
- Linux: Landlock, `SO_PEERCRED`, watchdog pre-timeout; systemd `IPAddressAllow=`.
- LTC-10 (fencing by incarnation) and `UNKNOWN_AFTER_RESTART`.
- The other sources are those already cited in `PRIOR-ART-REVIEW.md`.
