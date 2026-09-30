# Adjudication of the r4.2 to r4.4 improvements (r4.5)

- **Asked:** the owner asked to "review and adjudicate these improvements": everything proposed in the combined review package (`REVIEW-PACKAGE-COMBINED.md`). That means the amendments to the four settled rulings and the open decisions in Packages A, B and C.
- **Authority:** adjudicated by Claude at the owner's request on 2026-09-30.
  - The owner's Class C authority is unchanged: each verdict stands unless the owner overturns it.
  - Two items interpret the owner's *own* rulings. They are marked **OWNER** and take effect only when the owner confirms them.
  - Nothing here activates a daemon, unlocks a class or changes the LTC.
- **Method:** each item was reviewed again with fresh eyes before a verdict, including the recommendations this repository made itself. Four of them are changed below (§3). Two findings were turned from "traced" or "not yet seen" into reproduced defects and fixed with tests (HF-34, HF-35). Every verdict names its evidence.
- **Verdict vocabulary:** ADOPTED, ADOPTED WITH MODIFICATION, DEFERRED (with a trigger), REJECTED. BUILT means done in this revision, with tests.

## 1. Summary

| Group | Adopted | With modification | Deferred | Rejected | Built in r4.5 |
| --- | --- | --- | --- | --- | --- |
| Settled-ruling amendments (5) | 3 (one needs the OWNER) | 1 (OWNER) | 1 | 0 | PD-70's clock basis (HF-34) |
| Package A: transport contract (5, plus the LTC markup) | 4 | 2 | 0 | 0 | — |
| Package B: Break Glass (5) | 4 | 1 | 0 | 0 | — (blocked on PD-72, PD-01.7, PD-40) |
| Package C (11 lines) | 9 | 2 | A-8, A-9, A-10 (inside one line) | 0 | DB-20 (HF-35, found during this review) |
| Seams N-1 to N-14 | adopted as the required test list for their decisions | | | | |

Nothing is rejected: the second look changed four recommendations instead (PD-83, PD-85, PD-01.13 and the Phase 1 staleness checker), and tightened PD-76.

## 2. Two defects found and fixed while adjudicating

**HF-34 (High): a backward wall-clock step brought HF-32 back.** The r4.4 brief traced this; r4.5 reproduced it.
- **Reproduction:** a daemon blind on every cycle was killed and restarted every second against a 1.5 s limit, with its wall clock one hour behind after the first run.
  - On r4.4, all six restarts inherited 0 ms and `SENSE_BLIND` never fired.
  - Control, with the real clock: `SENSE_BLIND` on the second start.
  - Evidence: `evidence/r4.5/clock_step_before.txt`.
- **Fix (PD-70's amendment, adopted in §3.1):**
  - `DAEMON_START`, `DAEMON_HEARTBEAT` and `DAEMON_STOP` carry `boot_id`, `boottime_ms` (`CLOCK_BOOTTIME`) and `blind_since_boottime_ms`.
  - A restart within one boot measures inherited blindness on that clock.
  - Across a reboot the wall clock is used if it moved forward. Otherwise blind-at-the-limit is assumed, which leaves the one reacquisition cycle.
  - `clock_basis` is recorded in `DAEMON_START` and `SENSE_BLIND`.
  - A daemon event carries no boot time, so it is anchored at the preceding stamp. That can over-state blindness by at most `T`/2, in the safe direction.
- **Contract 3.1.0** (additive: no existing daemon can newly fail).
- **Tests** in `tests/test_blind.py`:
  - `test_a_backward_clock_step_does_not_hide_blindness` fails on r4.4.
  - `test_start_heartbeat_and_stop_carry_the_boot_stamp` fails on r4.4.
  - `test_across_a_reboot_the_wall_clock_is_used_and_a_backward_one_assumes_the_worst`.
  - `test_an_event_after_the_last_stamp_is_anchored_at_that_stamp`.
- After the fix, the reproduction fires `SENSE_BLIND` with the clock one hour behind (`clock_step_after.txt`).
- **Residual:** within a process the blind clock still runs on `CLOCK_MONOTONIC`, which does not count suspend. Across restarts it now counts suspend. A DGX server does not suspend, so this is recorded rather than changed.

**HF-35 (High): the battery passed a daemon that never observes anything.**
- **Found by** reviewing the battery for this adjudication: no check exercises the blind period. A daemon whose every cycle is unsettled records no error and no event.
- **Reproduction:** with its digest disabled, the fixture `tests/fixtures/daemons/neverseeing` got **RESULT: PASS** on r4.4 (`evidence/r4.5/neverseeing_battery_before.txt`). This is the same class of defect as HF-16 (r3), where the battery passed a daemon that failed every cycle.
- **Fix:** a new check, **DB-20 "observes within a blind limit"**.
  - It runs the daemon with a blind limit of five test intervals (1.0 s) over 12 cycles.
  - It requires accepted cycles in the heartbeats and no `SENSE_BLIND`.
  - It is a new ID, not an edit of DB-04, under PD-78's rule. DB-19 stays reserved for the direct memory reading (PD-76).
- **Tests:**
  - `test_handoff.py::BatteryCatchesAlwaysFailingDaemonsTests::test_a_daemon_that_never_observes_fails_db20` fails on r4.4.
  - `test_misc.py` now pins the check list: 19 checks, ending DB-18, DB-20.
- **Result:** all four reference daemons pass DB-20 (10 accepted cycles each).

## 3. Verdicts

### 3.1 Amendments to settled rulings

| Item | Verdict | Conditions, and why |
| --- | --- | --- |
| **PD-63: exit-code split** (`EXIT_POLICY` ≠ `SENSE_BLIND`) | **ADOPTED**, built with contract 4.0.0 | Keep `SENSE_BLIND` at 78 (the Observer's code). Choose `EXIT_POLICY`'s new code from the Observer's exit table under PD-35, once that table is read, not before. **Condition (seam N-12):** one exit-code table generates the runtime constants, the unit's `RestartPreventExitStatus`, and the notifier's mapping; a test fails if any drifts |
| **PD-63: where `T` is set** | **DEFERRED** until PD-40 is built | No evidence of a problem today. Two places for one number is a cost worth paying only when per-host tuning is needed |
| **PD-70: clock basis** | **ADOPTED and BUILT** (HF-34, contract 3.1.0) | Reproduced, fixed, tested (§2) |
| **PD-01.9: scope clarification** | **ADOPTED as the working interpretation; OWNER** | It interprets the owner's own ruling, so it needs the owner's word. Until then this repository designs to it: the Blind Forester does not unlock `critical`; BF-5 is mandatory where a person could be harmed; it governs survival *mode*, never emergency *authority*; PD-72 is a prerequisite |
| **PD-76: mechanism wording** | **ADOPTED WITH MODIFICATION; OWNER** (it rewords the owner's ruling) | Adopt "a direct cgroup peak: v2 `memory.peak` or v1 `memory.max_usage_in_bytes`". **Modification:** a v1 reading may decide PASS or FAIL only against a limit stated for v1 hosts, because v1 counts page cache differently. On the DGX (v2), only v2 readings are admissible. The battery's privileged step, creating a cgroup and moving the process into it, goes in a small separate helper with its own test, not inline (seam N-4) |

### 3.2 Package A: the transport contract

| Item | Verdict | Conditions, and why |
| --- | --- | --- |
| **PD-82** Bind, do not embed | **ADOPTED** | The LTC's own scope (LTC-01, "not a seventh pillar") and this repository's PD-52 agree |
| **PD-83** Truncated reads raise | **ADOPTED WITH MODIFICATION** | **Change from the recommendation:** in contract 4.0.0, truncation *always* raises `TooLarge`. The opt-in "partial result" type is **not** built until a real daemon needs explicit partial semantics. No reference daemon does: `dir-watch` and `git-watch` both now fail the cycle. This removes seam N-10 instead of guarding it |
| **PD-84** Register uses the LTC binding vocabulary | **ADOPTED** | A design rule for PD-40, with H02's three modifications |
| **PD-85** Declared pacing | **ADOPTED WITH MODIFICATION** | **Change from the recommendation:** adopt the `pacing` field, but keep **today's bounded random jitter as the default**. Deterministic spread is available and recommended for new daemons. There is no measured problem with the current jitter on a host with four daemons: it is already zero in test mode, so it does not hurt testability. Changing every daemon's timing in a batch for a benefit nobody has measured is churn. Revisit when a host runs enough daemons to measure phase collisions |
| **PD-86** Retry bounds hold over time | **ADOPTED** | Evidence: HF-28 |
| **LTC-H01 to H03, this repository's markup** | **ADOPTED for submission** to the LTC reviewers | This repository cannot rule on the LTC. It forwards its markup (a total-time deadline for streams, digest-named trust evidence, bounds that hold over time) |

### 3.3 Package B: Break Glass and operator-gated recovery

| Item | Verdict | Conditions, and why |
| --- | --- | --- |
| **PD-01.10** Premise, scope, tiers | **ADOPTED** | The beacon tier (BG-S) comes after PD-01.3, PD-72 and PD-01.7. The acting tier (BG-A) is deferred with 3b to kernel v2 |
| **PD-01.11** Mechanisms | **ADOPTED** | A dormant relay; the owner's revised socket; single-use, epoch-bound envelopes |
| **PD-01.12** Epochs, conservative restart rule | **ADOPTED** | Since HF-34, every record carries `boot_id`, which is the natural carrier for the epoch-per-boot rule (BG-9). Build the epoch on it |
| **PD-01.13** Witness and flight recorder | **ADOPTED WITH MODIFICATION** | **Change:** where a person could be harmed (`critical`, still refused), the witness is separate hardware. At `elevated`, it is a separate process tree with its own clock source, and its independence is audited (U-7). "Separate process tree" alone is not enough evidence of independence at `critical` |
| **PD-01.14** Recovery boundary | **ADOPTED** | It is also the Blind Forester's only exit, which completes BF-6 |

### 3.4 Package C

| Item | Verdict | Conditions, and why |
| --- | --- | --- |
| **BF-1 to BF-6** | **ADOPTED**, BF-5 mandatory | Each follows from a recorded invariant |
| **PD-72** Supervisor/worker split | **ADOPTED as a precondition**; built with the first active class | BF-1, LTC-14 and Package B all rest on it |
| **PD-01.1, PD-01.2** Classes; consequence field | **ADOPTED** | The consequence field goes in contract 4.0.0 |
| **PD-01.3** Sentinel | **ADOPTED WITH MODIFICATION** | Ship it with ISA-18.2's minimum (A-5): one `SENSE_DEGRADED` record per blind streak, a rate limit, a defined response. Without that, alerts turn into fatigue |
| **PD-69** Limits checked against each other | **ADOPTED** | Includes time windows (PD-86). DB-15's budget table is Phase 1 |
| **PD-71** (remainder) | **ADOPTED**, with DB-19 | — |
| **PD-75 to PD-81** Numbering | **ADOPTED: alias** | — |
| **C-1** Battery exit codes | **ADOPTED**: 0/3/4 in contract 4.0.0 | Its meanings change, so it moves with every other exit-code change, once |
| **`INCOMPLETE` in CI** | **ADOPTED** | Require PASS where a direct reading is possible; `INCOMPLETE` elsewhere only with a stated environment reason |
| **Phase 1** | **ADOPTED WITH MODIFICATION** | **Change (seam N-5):** the diverse staleness checker detects *change*. It records the ledger's size and head hash, and checks on its own monotonic clock that they changed within `T`. It does not compare a file's age against the wall clock, which has the HF-34 weakness |
| **PD-64 to PD-68; A-1 to A-12** | **ADOPTED** (PD-64 to PD-68; A-1, A-2, A-4, A-5, A-7, A-11, A-12). **ADOPTED WITH CONDITION** A-6: a shelving window extends `T` only by a human or a pre-approved rule, bounded and recorded (S-3). A-3 comes with PD-01.2. **DEFERRED:** A-8 with PD-01.5; A-9 and A-10 with the first active class or a certification path | — |

### 3.5 Seams N-1 to N-14

**ADOPTED as required tests.** Each seam's "what a reviewer should check" becomes a test that must exist when its decision is built. `BATTERY-REVIEW-R4.5.md` §5 lists them as planned tests T-N1 to T-N14. Seam N-10 is removed by PD-83's modification.

## 4. What this changes in the order of work

1. **Done in r4.5:** HF-34 (contract 3.1.0) and HF-35 (DB-20).
2. **Owner confirms:** PD-01.9's scope, and PD-76's wording.
3. **Phase 1,** with the change-detecting staleness checker.
4. **DB-19** (direct memory reading), with the separate cgroup helper and the CI rule.
5. **Contract 4.0.0:**
   - the exit-code split (PD-63);
   - battery codes 0/3/4 (C-1, PD-81);
   - truncation always raises (PD-83);
   - the `pacing` field (PD-85);
   - the consequence field (PD-01.2);
   - PD-35, PD-39 and PD-49;
   - a migration hint for schema 3.
6. **PD-40, PD-72, PD-01.7,** then Package B's beacon tier.
