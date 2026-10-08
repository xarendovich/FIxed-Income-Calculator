# Roadmap: what remains, and the kernel v2 track

- **As of:** 2026-10-08, contract 5.0.0 at `6aa8671` (HF-43/44 fixed, verification pass done).
- **What this can and cannot say.** This repository holds the daemon pattern. It does not hold the Spark kernel. The kernel v2 material visible here is: the owner's statement that the daemon subsystem "doesn't have a spot in the roadmap but will have one by kernel v.2" (PD-31), the Kernel v0.2 Stage A review (cross-checked in r3.1), and the decisions this pattern recorded for kernel v2 (PD-54 to PD-62). I have **not** seen a kernel v2 roadmap or schedule, so the "on track" judgement below is about what the pattern must deliver to kernel v2, not about kernel v2's own timeline. If the kernel repository exists, it can be attached to this session and the second half filled in from source.

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

## 3. The kernel v2 track, as recorded here

The pattern's job for kernel v2 was to fix the interface the authoring subsystem will plug into, before that subsystem exists, so that it inherits "a published, versioned, test-pinned contract and an evidence path that it cannot shortcut" (DAEMON-CONTRACT §11). **That part is delivered**, at 5.0.0, with less surface than when it was promised (eleven seams to six boundaries).

What kernel v2 itself still has to decide is unchanged since r3.4, and every item is still **PENDING** with the owner. The order recorded then still holds:

| Order | Decision | What it settles | Blocks |
| --- | --- | --- | --- |
| 1 | **PD-54** a roadmap slot and an owner for resident components | the daemon subsystem gets a WBS number: an observe-only plane below the fixed kernel; skeleton in the fixed tier, manifests and `daemon.py` pluggable | everything below |
| 2 | **PD-57** one unit generator for every resident component | which generator (this pattern's `unitgen.py` or WBS 4.0's) owns units; the other becomes its conformance reference | installing anything beyond the pilot |
| 2 | **PD-58** the off-host anchor | one place, one writer, one schedule for every ledger's chain head | residual 5 here; the Observer's equivalent limit |
| 3 | **PD-56** how the Observer and daemons share one durable writer | vendored pinned copy now; kernel-owned library decided at v2 | PD-34 |
| 4 | **PD-61** how untrusted evidence (a digest) reaches a model | one kernel rule: user/tool turn, delimited, staleness-checked, never a system prompt | any model reading a digest |
| 4 | **PD-62** producer identity never raises trust | a model-written candidate faces the same suite and the same Class C activation | any model writing a candidate; the authoring slot (PD-31) |
| 5 | **PD-55** a shared exit-reason taxonomy | needs the Observer plus one installed daemon as evidence | — |
| 5 | **PD-59** stay with the Python standard library | revisit trigger: a second kernel interface needing `ctypes` | — |
| 5 | **PD-60** whether any resident component may act or send | keep `act` and `named` reserved through v2 | — |

Already on kernel v2's list from earlier rounds: PD-39 (KECC change classes and a support horizon per published contract), PD-40 (the activation register), PD-41 (reserved human-correction event names), PD-46/47 (L0 in X1 with this pattern as pilot 0), PD-50 (no zero-touch activation for resident components).

## 4. On track?

**For what the pattern owes kernel v2: yes, and early.** The boundary, the contract and the evidence path exist, are frozen-candidate, reviewed, and simpler than promised. Nothing kernel v2 needs from this side is missing; the open items here (freeze, 5.0.1 wording, V-1, the DGX gate) are hardening of a delivered thing, not prerequisites for v2 planning.

**For kernel v2's own decisions: not started**, by the evidence here. PD-54 to PD-62 are all PENDING, and PD-54 (an owner and a slot) gates the rest. The practical sequence is unchanged: close the DGX gate so the kernel decisions rest on a real deployment fact (PD-57 and PD-58 are "deployment facts before any daemon is installed"), then rule PD-54, then the rest in the recorded order.

**What would change this assessment:** seeing the kernel v2 roadmap itself. If it exists in another repository, attach it and this section gets filled in from source rather than from what the pattern recorded about it.
