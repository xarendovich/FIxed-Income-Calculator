# Spark daemon pattern: review package (r4.8)

- **For:** reviewers (people or models). This document stands alone. Every file it names is in `spark-daemon-pattern/` on branch `claude/clever-bardeen-qqkxez`, for reviewers who can read the repository.
- **Status:** FOR REVIEW. It authorizes no activation, no class unlock and no LTC change. Verdicts marked "Claude, subject to the owner" stand unless the owner overturns them. Items marked **OWNER** need the owner's own word.
- **Revision:** r4.8, 2026-10-06, by Claude.
- **State at r4.8:**
  - contract 3.1.0 (unchanged since r4.5);
  - 214 of 214 self-tests pass, none skipped (Python 3.11.15, with `jsonschema` 4.26 for the four schema-agreement tests; `evidence/r4.8/selftests.txt`);
  - the four reference daemons pass all 19 battery checks (DB-01 to DB-18 and DB-20) on the build workspace, Landlock ABI 7 (`evidence/r4.8/battery-examples.txt`);
  - the pattern has been ported to its **first pilot project** and merged there, with one observer daemon (§2).
- **Supersedes:** the status and seam tables of `REVIEW-PACKAGE-COMBINED.md` (r4.4) and `REVIEW-PACKAGE-R4.5.md` (r4.5). The detail in those documents still holds unless this one says otherwise; it is cited, not repeated.

**How to mark:** for each decision, seam, gap or question, give AGREE, CHANGE or REJECT, with notes. Say which kind of evidence you rely on: **reproduced**, **measured**, **traced** (to code) or **opinion**. §8 lists the questions this review most needs answered.

**Reading order:**
1. §1 and §2: what changed since r4.5, and what the first deployment teaches.
2. §3 to §5: where every decision stands, what is owed to the owner, and the seams. **This is the main request.**
3. §6 and §7: the battery and the self-test list.
4. For detail: `HARDENING-REVIEW-R4.6.md`, `ADJUDICATION-R4.5.md`, `BATTERY-REVIEW-R4.5.md`, `SQLITE-LEDGER-REVIEW.md`, `BREAK-GLASS.md`, `LTC-INTEGRATION-REVIEW.md`, `PD-01-DAEMON-CLASSES.md`, `HARDENING.md`.

## 1. What changed since r4.5

| Revision | Change | Kind | Evidence |
| --- | --- | --- | --- |
| r4.6 | **HF-36 (Medium).** The Landlock domain granted execute wherever it granted read, including the output directory. A binary written there by a compromised daemon, or dropped by someone else into a watched folder, could be run if the in-process layers were bypassed. Execute now stays on system paths only | Defect, fixed | **Reproduced** under ABI 7 (`evidence/r4.6/exec_before.txt`); `tests/test_landlock.py::test_nothing_the_daemon_can_write_or_merely_reads_can_be_executed` |
| r4.6 | **PD-93 to PD-98** adjudicated: no disassembly gate (digest-pinned executables instead); an activation-time ELF provenance check; containment of untrusted text; credential stores denied; the supervisor alone holds the ledger; F-1 closed by a check after opening | Decisions (Claude, subject to the owner) | `HARDENING-REVIEW-R4.6.md` §4 |
| r4.6 | **F-1 recorded.** A directory swapped for a symlink between `ctx.read_text`'s path check and its open can reach a denied subtree inside a declared read. systemd's `InaccessiblePaths=` still blocks it under the unit; outside the unit it does not | Weakness, **fix planned** | **Simulated** deterministically (`evidence/r4.6/read_race_simulation.txt`) |
| r4.7 | **HF-37 (Medium).** Precheck bound the original manifest instead of the workspace copy. A daemon with an absolute `output_dir` failed precheck ("no ledger") while the battery passed it 19 of 19. Found while porting the pattern to the pilot | Defect, fixed | **Reproduced** by the pilot's own daemon; `tests/test_handoff.py::test_an_absolute_output_dir_prechecks_like_the_battery` |
| r4.8 | **Two unit options from the pilot.** `unit --require-path` adds `ConditionPathExists=`, so a daemon writing to a locked drive is skipped rather than stopped fail-closed. `unit --part-of` adds `PartOf=`, `After=` and `WantedBy=` another unit, so a daemon whose open ledger would keep a drive busy stops before the unit that owns the drive and starts with it. The unit name is checked, so it cannot inject a directive | Feature, from deployment | `tests/test_misc.py::test_a_required_path_becomes_a_start_condition`, `test_part_of_binds_the_unit_to_another` |
| r4.8 | `proc.SYSTEM_PATH` replaces the command search path written out in 17 places; `tests/conftest.py`; git-watch's Git calls are counted by the parser, not by text | Hygiene | 214 of 214 self-tests |

## 2. The first deployment, and what it teaches

The pattern was copied into one pilot project, not linked to it. The pilot runs one class-1a observer, `pressure-watch`. It records when a single host's available memory, swap in use or drive free space crosses that project's own soak limits, and nothing else. All figures below come from that copy. The pilot's name and paths are deliberately left out of this repository.

**What the pilot's owner decided** (their decisions D-1 to D-4, 2026-10-01):

| # | Question | Decision | What it means for the pattern |
| --- | --- | --- | --- |
| D-1 | The generated unit restarts on a crash; that project's own units never restart | **Keep the restart**, and tell the owner about every restart and every stop that needs a person, in the project's chat | The restart policy (`RestartPreventExitStatus=2 65 73 78`, 5 starts in 300 s) survived contact with a stricter house rule, **on condition of a notifier**. That is Phase 1's `OnFailure=` notifier, built by the pilot, not here (seams N-19, N-22) |
| D-2 | Where the unit lives | **Generated on the host** at install time; committed only after a soak | The generator, not a checked-in unit, is the source of truth |
| D-3 | The observer overlaps an existing 5-minute sampler | **Keep both** during the pilot | — |
| D-4 | `strace` on the host | **Installed**, so DB-08 and DB-09 can run there | A host battery can now reach PASS rather than INCOMPLETE (G-7) |

**How the pilot activates it.** The project's installer runs on setup day and on every update. For a fixed list of observers (today only `pressure-watch`) it:
1. runs the full battery **on the host, as the service user**;
2. on PASS only, generates the unit with `--require-path` (the drive's marker) and `--part-of` (the project's stack unit), installs and enables it;
3. on anything else, installs nothing, disables a unit installed before, and warns with the report's path.

The owner's Class C decision has therefore been **delegated to a battery PASS on the host**, rerun at each update. This is a design choice the pilot's owner made with the facts in front of them. It also makes three of this repository's open items load-bearing sooner than planned: PD-40 (the register), PD-93 (digest-pinned executables) and G-5 (a PASS bound to its host) (seam N-20).

**What is still not known:** the first host battery and the first `DAEMON_START` under systemd as PID 1 have **not been reported back** to this repository. G-6 ("never run under systemd PID 1") stays open until that `DAEMON_START.landlock.status` is read and is `"enforced"`.

## 3. Where every decision stands

Status labels: **Built** (in code, with tests); **Partial**; **Designed** (documents only); **Planned** (adjudicated, scheduled, not started). "Owner" means the decision needs the owner's word before it is final.

### 3.1 Built

| Item | Where | Since |
| --- | --- | --- |
| PD-63 blind period, `SENSE_BLIND` exit 78, never restarted | `runtime.py`, `context.py`, `unitgen.py` | r3.5 |
| PD-70 blindness across restarts, on `CLOCK_BOOTTIME` within a boot (HF-34) | `runtime.py` (`_inherited_blindness`, `_boot_stamp`) | r4.0, r4.5 |
| DB-20 the daemon observes (HF-35) | `battery.py` (`db20`) | r4.5 |
| HF-33 capped listing fails the cycle (`dir-watch` only) | `examples/dir-watch/daemon.py` | r4.3 |
| HF-36 execute only on system paths | `landlock.py` | r4.6 |
| HF-37 precheck binds the workspace manifest | `handoff.py` | r4.7 |
| PD-95 containment of untrusted text in the digest | `render.py`, DB-10 | r3 |
| `--require-path`, `--part-of` | `unitgen.py`, `cli.py` | r4.8 |
| Contract 3.1.0 | `contract.py`, `contract/` | r4.5 |

### 3.2 Planned, in the order adjudicated

| # | Item | Lands in | Notes |
| --- | --- | --- | --- |
| 1 | **F-1** check after opening, plus `O_NOFOLLOW` on the ledger, lock and digest opens (PD-98) | `context.py`, `guard.py`, `ledger.py`, `runtime.py` | Next. The only weakness shown and not yet closed |
| 2 | **PD-93** each allowlisted executable's SHA-256 in `DAEMON_START.tools` | `runtime.py` | Additive. Becomes urgent where activation is automated (§2, N-20) |
| 3 | **Phase 1:** an `OnFailure=` notifier; a change-detecting staleness checker (N-5); a budget report | new | The pilot built its own notifier (D-1); compare before building (Q-3) |
| 4 | **DB-19** direct cgroup memory peak, with a separate helper, and the CI rule for INCOMPLETE | `battery.py` + helper | **OWNER** wording (PD-76). Replaces DB-14's memory half (38 % under-read, measured r4.4) |
| 5 | **Contract 4.0.0** in one batch: exit split (PD-63), battery codes 0/3/4 (C-1), truncation always raises (PD-83), `pacing` with jitter as default (PD-85), consequence field (PD-01.2), credential stores and secret-shaped names denied (PD-96), PD-35/39/49, schema-3 migration hint | many | One migration for every author (N-13) |
| 6 | **DB-21** budget table (PD-69, PD-86); **DB-22** independent ledger verifier (B-6); **DB-23** worst-case fixtures (PD-71); **PD-94** executable provenance check | `battery.py`, a verifier outside the package | — |
| 7 | **PD-40** activation register; **PD-72** supervisor/worker split (with PD-97); **PD-01.7** connector gate | new | Preconditions for every class above 1a |
| 8 | **Break Glass beacon tier** (PD-01.10 to PD-01.14, BG-S) | new | After PD-01.3, PD-72 and PD-01.7. The acting tier (BG-A, class 3b) is deferred with kernel v2 |

### 3.3 Designed, or adopted as rules, with nothing to build yet

- **Package A (transport):** PD-82 bind, do not embed; PD-84 register vocabulary; PD-86 retry bounds over time; LI-1 to LI-14; the LTC-H01 to H03 markup, forwarded to the LTC reviewers.
- **SQLite ledgers:** PD-87 observers never govern; PD-88 one set of hash-chain rules; PD-89 wake is a hint; PD-90 UDS profile guidance; PD-92 the three-ledger split by writer. **Deferred:** PD-91, until the kernel's ledger exists and the read-only WAL problem is solved on the target's SQLite.
- **Classes:** PD-01.1 to PD-01.3; BF-1 to BF-6 (BF-5 mandatory); A-1 to A-12 (A-8 to A-10 deferred).
- **Deferred:** WASI with fuel (PD-73, PD-74); where `T` is set (PD-63, until PD-40).

## 4. What is owed to the owner

| Item | What is needed | Blocks |
| --- | --- | --- |
| **PD-01.9 scope** (OWNER) | Confirm: the Blind Forester does not unlock `critical`; BF-5 is mandatory where a person could be harmed; it governs survival mode, never emergency authority; PD-72 is a prerequisite | Every active class |
| **PD-76 wording** (OWNER) | Confirm "a direct cgroup peak: v2 `memory.peak` or v1 `memory.max_usage_in_bytes`", with a v1 reading admissible only against a v1 limit | DB-19 |
| **Delegated verdicts** | Ratify or overturn: r4.5 (`ADJUDICATION-R4.5.md`: settled-ruling amendments, Packages A to C, PD-87 to PD-92) and r4.6 (PD-93 to PD-98) | Nothing today; each stands until overturned |
| **The host report** | The first host battery report and the first `DAEMON_START` from the pilot | G-5, G-6 |

## 5. Seams

A seam is a boundary where two parts meet and each assumes something about the other. Every defect found since r3.5 sat on one. Test each with: "What does each side assume about the other, and what happens when that assumption is false?"

**N-1 to N-14** (`REVIEW-PACKAGE-COMBINED.md`, reviewer focus) and **N-15 to N-18** (`SQLITE-LEDGER-REVIEW.md` §5) stand as adopted, each becoming a required test when its decision is built. N-10 was removed by PD-83's modification.

**New at r4.8, from the first deployment.** Each was found by reading the pilot's code (**traced**). None has been reproduced.

| # | Seam | The assumption that could fail | What a reviewer should check |
| --- | --- | --- | --- |
| **N-19** | **A ledger consumer outside the pattern** (the pilot's notifier) | The notifier reads each daemon's ledger to report restarts. It reads a payload flag (`previous_run_ended_cleanly`) without verifying the hash chain. It reports stops from systemd's unit state, which is independent | Can a malformed or edited record make a false "restarted" notice, or hide a real one? Should every consumer either verify the chain (DB-22's independent verifier, once built, is the natural library) or treat what it reads as a hint only? The notice text is fixed words and checked fields, never the daemon's own text: confirm that holds for every field |
| **N-20** | **Automated activation** (the pilot's installer; PD-40, PD-93, G-5) | The code the battery judged is the code the unit runs | `DAEMON_START` records `manifest_sha256` and `daemon_code_sha256`, but **nothing compares them** with the PASS report's digests. A repository change that reaches the host by any path other than the installer runs at the next restart with no battery. Should the runtime, or a pre-start step, refuse a start whose digests differ from the last PASS on this host? This is PD-40's job; is a minimal check owed before PD-40? |
| **N-21** | **`PartOf=` and the meaning of a heartbeat** (r4.8) | "Heartbeats prove it was watching while nothing changed" | When the owning unit stops, the daemon stops cleanly, so no blindness is inherited at the next start (correct under PD-70). The ledger shows the gap as `DAEMON_STOP` to `DAEMON_START`. Does every reader of "coverage" treat that gap as **not watching**, rather than as quiet? |
| **N-22** | **`ConditionPathExists=` and silence** (r4.8) | A skipped unit is harmless | A condition failure is not a unit failure: systemd records it, but nothing is written to the ledger and the pilot's notifier, which looks for failed units, says nothing. A drive left locked means the observer silently never starts. Does the staleness checker (Phase 1, N-5) cover "never started", and not only "stopped changing"? |
| **N-23** | **Two copies of one pattern** (no link allowed) | A fix in one copy reaches the other | r4.8 brought the pilot's changes back by hand. HF-36 and HF-37 went the other way by hand. Is a per-revision parity list (change, in source?, in pilot?) enough, or should each revision's tests run against both copies? |

## 6. The conformance battery at r4.8

The checks are unchanged since r4.5: **DB-01 to DB-18 and DB-20**; DB-19 is reserved for PD-76. Each check's method, its limits ("cannot catch") and whether it is independent of the code it judges are in `REVIEW-PACKAGE-R4.5.md` Part 1 §2, which still holds. Two changes in what is known:

- **DB-08 and DB-09** can now run on the pilot host (D-4).
- **The battery is now an activation gate** in the pilot (§2). That raises the cost of the known gaps:

| # | Gap (from r4.5) | Change at r4.8 |
| --- | --- | --- |
| G-1 | Memory read per process (DB-14), 38 % low | Unchanged. DB-19 waits on the PD-76 wording (OWNER) |
| G-2 | Ledger checks use the skeleton's own verifier | Unchanged; **N-19** adds a second reason for DB-22 |
| G-3 | Limits not checked against each other | Unchanged (DB-21) |
| G-4 | Fixtures are small | Unchanged (DB-23) |
| G-5 | A PASS is host-specific but does not say so | **Raised.** In the pilot the PASS is produced on the host it gates, which helps, but the report still does not bind a host fingerprint |
| G-6 | Never run under systemd PID 1 | **Pending the pilot's host report** (§4) |
| G-7 | Two checks need tools | **Closed on the pilot host** (D-4); open elsewhere |
| G-8 | Power-loss durability not provable by SIGKILL | Unchanged |

**Planned checks** are as listed in `REVIEW-PACKAGE-R4.5.md` Part 1 §4 (DB-19, DB-21, DB-22, DB-23, DB-2x), plus PD-94's executable provenance check. **Planned self-tests** are as listed in its §5 (T-EX1, T-EX2, T-TR1, T-PC1, T-ST1, T-CG1, T-N1 to T-N14), plus one per new seam: T-N19 to T-N23.

## 7. The self-test list

214 tests at r4.8, generated from the test files by `evidence/r4.5/testlist.py` into `evidence/r4.8/testlist.md`, which is appended below. "Cites" lists the defect (HF), decision (PD), battery check (DB) and rule identifiers each test names. New since r4.5: the HF-36 and HF-37 tests (r4.6, r4.7) and the two unit-option tests (r4.8).

All 214 ran on this revision; none was skipped (`evidence/r4.8/selftests.txt`). Without `jsonschema` installed, the four schema-agreement tests skip by name and say so.

## 8. Questions for the reviewer

1. **N-20:** with activation automated in the pilot, should a minimal "digests match the last PASS on this host" check come **before** PD-40, or is it acceptable to wait for the register?
2. **N-19:** should the pattern publish a small, independent ledger reader (DB-22's verifier) for consumers, and require consumers to use it?
3. **D-1 and Phase 1:** the pilot built its own restart and stop notifier. Should Phase 1's `OnFailure=` notifier be built here as the reference, or should the pattern only specify what a notifier must report (stops by reason, restarts by unclean start, condition skips)?
4. **N-22:** is "never started" a staleness case the Phase 1 checker must detect, and against what clock?
5. **F-1:** the adopted fix opens the file, asks the kernel which file the descriptor actually names, and re-checks that path against the policy before reading a byte. Is that sufficient, or should reads go through `openat2` with `RESOLVE_NO_SYMLINKS` where the kernel supports it, with the post-open check as a fallback? (The audit hook judges relative paths, which is why a per-component `O_NOFOLLOW` walk was not chosen: `HARDENING-REVIEW-R4.6.md` §3.8.)
6. **G-5:** which host facts must a PASS bind (kernel, Landlock ABI, cgroup version, systemd version, tool versions) before an installer may act on it?
7. Which of the 214 self-tests would you delete as redundant, and which property has no test at all?

## Markup table

| # | Item | AGREE / CHANGE / REJECT | Evidence type | Notes |
| --- | --- | --- | --- | --- |
| 1 | §1 changes since r4.5 | | | |
| 2 | §2 lessons from the first deployment | | | |
| 3 | §3.2 order of planned work | | | |
| 4 | §4 owner items | | | |
| 5 | N-19 | | | |
| 6 | N-20 | | | |
| 7 | N-21 | | | |
| 8 | N-22 | | | |
| 9 | N-23 | | | |
| 10 | §6 gap changes | | | |
| 11 | Q-1 to Q-7 | | | |

## Appendix: the self-test list

<!-- generated: 214 tests -->

#### `tests/test_blind.py` (29 tests)

Blind period (r3.5): ctx.unsettled(), blind_limit_seconds and SENSE_BLIND, and the unit

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `BlindPeriodTests` | `test_unsettled_is_neither_an_event_nor_an_error` | Unsettled is neither an event nor an error. |  |
| `BlindPeriodTests` | `test_always_unsettled_stops_at_the_limit` | Always unsettled stops at the limit. |  |
| `BlindPeriodTests` | `test_failed_cycles_count_towards_the_limit` | r2's silent-outage shape: a daemon failing every cycle kept pinging the watchdog forever. |  |
| `BlindPeriodTests` | `test_an_accepted_cycle_resets_the_clock` | Busy for about twice the limit in total, but never for a whole limit at once. |  |
| `BlindPeriodTests` | `test_blindness_survives_a_restart_loop` | HF-32: killed and restarted every 1.0 s against a 1.5 s limit. On r3.9 no run ever reached the limit; now the second run inherits the first run's blindness. | HF-32 |
| `BlindPeriodTests` | `test_a_backward_clock_step_does_not_hide_blindness` | On r4.4 every restart after a one-hour backward step inherited 0 ms, and six kill-restarts (6 s blind against a 1.5 s limit) never reached SENSE_BLIND (evidence/r4.5/). | HF-34 |
| `BlindPeriodTests` | `test_start_heartbeat_and_stop_carry_the_boot_stamp` | Start heartbeat and stop carry the boot stamp. | HF-34 |
| `BlindPeriodTests` | `test_across_a_reboot_the_wall_clock_is_used_and_a_backward_one_assumes_the_worst` | Across a reboot the wall clock is used and a backward one assumes the worst. | HF-34 |
| `BlindPeriodTests` | `test_an_event_after_the_last_stamp_is_anchored_at_that_stamp` | A daemon event carries no boot time; the preceding stamp bounds it from below (safe side). | HF-34 |
| `BlindPeriodTests` | `test_a_clean_stop_does_not_reset_blindness` | A clean stop does not reset blindness. | HF-32 |
| `BlindPeriodTests` | `test_a_restart_past_the_limit_gets_one_reacquisition_cycle` | A restart past the limit gets one reacquisition cycle. | HF-32 |
| `BlindPeriodTests` | `test_heartbeats_tell_quiet_from_dead` | A healthy daemon that sees no change still leaves evidence every limit/2. | HF-32 |
| `BlindPeriodTests` | `test_a_malformed_reason_is_a_sense_failure` | A malformed reason is a sense failure. |  |
| `BlindPeriodTests` | `test_the_override_is_ignored_outside_test_mode` | Outside test mode the manifest's 600 s applies, and the interval is the manifest's |  |
| `BlindLimitManifestTests` | `test_required_with_no_default` | Required with no default. |  |
| `BlindLimitManifestTests` | `test_bounded_both_ways` | Bounded both ways. |  |
| `BlindLimitManifestTests` | `test_at_least_three_intervals` | At least three intervals. |  |
| `BlindLimitManifestTests` | `test_version_1_manifest_gets_a_migration_hint` | Version 1 manifest gets a migration hint. |  |
| `GitWatchUnsettledTests` | `test_moving_refs_are_unsettled` | Moving refs are unsettled. |  |
| `GitWatchUnsettledTests` | `test_a_changing_worktree_is_unsettled` | A changing worktree is unsettled. |  |
| `GitWatchUnsettledTests` | `test_output_over_the_cap_fails_the_cycle` | Before r3.5 this was accepted as "dirty: null" every cycle, so it never reached the limit. |  |
| `GitWatchUnsettledTests` | `test_a_still_repository_is_observed` | A still repository is observed. |  |
| `DirWatchCapacityTests` | `test_a_folder_over_the_cap_fails_the_cycle` | A folder over the cap fails the cycle. | HF-33 |
| `DirWatchCapacityTests` | `test_adding_a_file_over_the_cap_never_reports_a_removal` | Adding a file over the cap never reports a removal. | HF-33 |
| `DirWatchCapacityTests` | `test_at_the_cap_the_inventory_is_complete` | At the cap the inventory is complete. | HF-33 |
| `NoRestartTests` | `test_fail_closed_exits_are_never_restarted` | Fail closed exits are never restarted. |  |
| `NoRestartTests` | `test_a_stop_outlasts_one_watchdog_period` | r3.9 (HF-31): a stop is honoured between cycles; a fixed 30 s SIGKILLed slow cycles. | HF-31 |
| `NoRestartTests` | `test_git_watch_worst_case_sense_fits_half_the_watchdog` | r3.9 (HF-30): six Git calls per worktree cycle since r3.5, each up to step_timeout. | HF-30 |
| `NoRestartTests` | `test_uncertain_commit_stays_restartable` | Uncertain commit stays restartable. |  |

#### `tests/test_canonical.py` (9 tests)

Canonical JSON: golden vector, refusals, determinism, strict parsing.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `CanonicalTests` | `test_golden_vector` | Golden vector. |  |
| `CanonicalTests` | `test_genesis_is_64_zeros` | Genesis is 64 zeros. |  |
| `CanonicalTests` | `test_key_order_matches_rfc8785_not_code_point_order` | Key order matches rfc8785 not code point order. |  |
| `CanonicalTests` | `test_key_order_nested` | The reordering must apply at every depth, not only the top level. |  |
| `CanonicalTests` | `test_allowed_conversions` | Allowed conversions. |  |
| `CanonicalTests` | `test_refusals` | Refusals. |  |
| `CanonicalTests` | `test_lone_surrogate_refused` | Lone surrogate refused. |  |
| `CanonicalTests` | `test_independent_of_hash_seed` | Independent of hash seed. |  |
| `CanonicalTests` | `test_strict_loads` | Strict loads. |  |

#### `tests/test_guard.py` (8 tests)

Guard: output-directory checks, inventory, path policy, and the audit hook in a child process.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `OutputDirTests` | `test_creates_0700_tree` | Creates 0700 tree. |  |
| `OutputDirTests` | `test_refuses_loose_permissions` | Refuses loose permissions. |  |
| `OutputDirTests` | `test_refuses_symlink` | Refuses symlink. |  |
| `OutputDirTests` | `test_stray_temp_files_removed` | Stray temp files removed. |  |
| `OutputDirTests` | `test_inventory_reports_foreign_files` | Inventory reports foreign files. |  |
| `AuditHookTests` | `test_enforce_mode_blocks_every_forbidden_operation` | Enforce mode blocks every forbidden operation. |  |
| `AuditHookTests` | `test_record_mode_reports_without_blocking` | Record mode reports without blocking. |  |
| `AuditHookTests` | `test_policy_readable` | Policy readable. |  |

#### `tests/test_handoff.py` (32 tests)

The handoff layer: the published contract, the manifest JSON Schema, candidate envelopes,

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `PublishedContractTests` | `test_committed_contract_matches_the_running_rules` | Committed contract matches the running rules. |  |
| `PublishedContractTests` | `test_committed_schema_matches_the_running_validator` | Committed schema matches the running validator. |  |
| `PublishedContractTests` | `test_contract_version_is_pinned_to_its_sha` | Changing any enforced rule changes contract_sha256. That must come with a new CONTRACT_VERSION and a new line in contract/versions.json, never a silent edit. |  |
| `PublishedContractTests` | `test_contract_is_identical_on_every_installed_python` | Contract is identical on every installed python. |  |
| `PublishedContractTests` | `test_contract_names_every_ctx_method_and_battery_check` | Contract names every ctx method and battery check. |  |
| `PublishedContractTests` | `test_describe_cli` | Describe cli. |  |
| `SchemaAgreementTests` | `test_schema_is_valid_draft_2020_12` | Schema is valid draft 2020 12. |  |
| `SchemaAgreementTests` | `test_every_shipped_manifest_passes_both` | Every shipped manifest passes both. |  |
| `SchemaAgreementTests` | `test_mutations_are_refused_by_both` | Mutations are refused by both. |  |
| `SchemaAgreementTests` | `test_cross_field_rules_are_manifest_py_only_and_declared` | Cross field rules are manifest py only and declared. |  |
| `EnvelopeTests` | `test_scaffold_envelope_is_exact` | Scaffold envelope is exact. |  |
| `EnvelopeTests` | `test_a_candidate_cannot_certify_itself` | A candidate cannot certify itself. |  |
| `EnvelopeTests` | `test_files_changed_after_sealing` | Files changed after sealing. |  |
| `EnvelopeTests` | `test_other_major_version_is_incompatible` | Other major version is incompatible. |  |
| `EnvelopeTests` | `test_same_version_other_rules_is_a_mismatch` | Same version other rules is a mismatch. |  |
| `EnvelopeTests` | `test_same_major_other_minor_is_a_warning` | Same major other minor is a warning. |  |
| `EnvelopeTests` | `test_bad_producer_kind` | Bad producer kind. |  |
| `EnvelopeTests` | `test_envelope_cli_round_trip` | Envelope cli round trip. |  |
| `ValidateJsonTests` | `test_structured_diagnostics` | Structured diagnostics. |  |
| `ValidateJsonTests` | `test_manifest_diagnostics_carry_the_field` | Manifest diagnostics carry the field. |  |
| `ValidateJsonTests` | `test_text_mode_is_unchanged_for_people` | Text mode is unchanged for people. |  |
| `PrecheckTests` | `test_every_reference_daemon_prechecks_ok` | Every reference daemon prechecks ok. |  |
| `PrecheckTests` | `test_impure_daemon_fails_fast_and_skips_the_run` | Impure daemon fails fast and skips the run. |  |
| `PrecheckTests` | `test_policy_violation_is_caught_by_the_short_run` | Policy violation is caught by the short run. |  |
| `PrecheckTests` | `test_precheck_never_says_pass` | Precheck never says pass. |  |
| `PrecheckTests` | `test_an_absolute_output_dir_prechecks_like_the_battery` | r4.7 (HF-37): the battery and precheck both rewrite an absolute output_dir into their workspace, but precheck bound the original manifest: "no ledger", and the rewritten folder reported as a write outside the output … | HF-37 |
| `ScaffoldTests` | `test_refuses_a_non_empty_folder_and_bad_names` | Refuses a non empty folder and bad names. |  |
| `ScaffoldTests` | `test_scaffold_passes_validate_and_user_unit_variant` | Scaffold passes validate and user unit variant. |  |
| `SymlinkStatTests` | `test_planted_symlink_is_a_link_not_a_violation` | Planted symlink is a link not a violation. | HF-17 |
| `SymlinkStatTests` | `test_following_reads_still_judge_the_target` | Following reads still judge the target. | HF-17 |
| `BatteryCatchesAlwaysFailingDaemonsTests` | `test_a_daemon_that_errors_every_cycle_fails_db04` | r2 passed the raiser fixture: its sense() fails every cycle in the battery's workspace (no ~/data/mode.txt), yet the process exits 0 with a valid chain. | DB-04, HF-16 |
| `BatteryCatchesAlwaysFailingDaemonsTests` | `test_a_daemon_that_never_observes_fails_db20` | r4.5 (HF-35): every cycle unsettled means no DAEMON_ERROR and no event; with its digest disabled, r4.4's battery reported RESULT: PASS for it. | DB-04, DB-20, HF-35 |

#### `tests/test_hardening.py` (40 tests)

Regression tests for the r3 hardening: every escape or crash found in the r2 review has a

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `ManifestHardeningTests` | `test_interpreter_families_and_command_runners_are_refused` | Interpreter families and command runners are refused. | HF-11 |
| `ManifestHardeningTests` | `test_harmless_commands_still_allowed` | Harmless commands still allowed. |  |
| `ManifestHardeningTests` | `test_dot_segments_and_trailing_slashes_refused` | Dot segments and trailing slashes refused. | HF-14 |
| `ManifestHardeningTests` | `test_output_dir_must_not_contain_a_protected_tree` | r2 accepted "~": Landlock and ReadWritePaths= would then grant write access to the whole home directory, ~/.ssh and ~/spark-core included. | HF-12 |
| `ManifestHardeningTests` | `test_output_dir_inside_the_manifests_own_deny_is_refused` | Output dir inside the manifests own deny is refused. |  |
| `ManifestHardeningTests` | `test_trailing_newlines_are_refused` | r2 used re.match with "$", which also matches before a final newline: a name of "x\n" split the generated unit's Description= line in two. | HF-13 |
| `ManifestHardeningTests` | `test_to_dict_is_bounded_and_strict` | To dict is bounded and strict. |  |
| `GitHardeningTests` | `test_refusals` | Refusals. |  |
| `GitHardeningTests` | `test_allowed` | Allowed. |  |
| `GitHardeningTests` | `test_diff_subcommands_get_no_ext_diff_and_no_textconv` | Diff subcommands get no ext diff and no textconv. | HF-08, HF-19, PD-25 |
| `GitHistoryIntegrityTests` | `test_repository_gpg_program_never_runs` | Repository gpg program never runs. |  |
| `GitHistoryIntegrityTests` | `test_replace_refs_do_not_forge_history` | Replace refs do not forge history. |  |
| `GitHistoryIntegrityTests` | `test_grafts_do_not_rewrite_ancestry` | Grafts do not rewrite ancestry. |  |
| `GitInjectionRuntimeTests` | `test_alias_injection_through_ctx_git_is_refused` | Alias injection through ctx git is refused. |  |
| `GitInjectionRuntimeTests` | `test_git_through_ctx_run_is_refused` | Git through ctx run is refused. | HF-02 |
| `GitInjectionRuntimeTests` | `test_read_only_git_still_works` | Read only git still works. |  |
| `PurityHardeningTests` | `test_frame_escape_to_real_builtins` | Frame escape to real builtins. | HF-04 |
| `PurityHardeningTests` | `test_traceback_frame_escape` | Traceback frame escape. | HF-04 |
| `PurityHardeningTests` | `test_private_attribute` | Private attribute. | HF-03 |
| `PurityHardeningTests` | `test_aliasing_a_forbidden_builtin` | Aliasing a forbidden builtin. | HF-05 |
| `PurityHardeningTests` | `test_dynamic_attribute_helpers` | Dynamic attribute helpers. | HF-06 |
| `PurityHardeningTests` | `test_format_string_reaching_a_dunder` | Format string reaching a dunder. | HF-06 |
| `PurityHardeningTests` | `test_exit_and_quit` | Exit and quit. |  |
| `PurityHardeningTests` | `test_import_time_calls` | Import time calls. |  |
| `PurityHardeningTests` | `test_entry_points_cannot_be_reassigned_or_redefined` | Entry points cannot be reassigned or redefined. |  |
| `PurityHardeningTests` | `test_allowed_idioms_still_pass` | Allowed idioms still pass. |  |
| `PurityHardeningTests` | `test_every_shipped_daemon_is_still_pure` | Every shipped daemon is still pure. |  |
| `RuntimeHardeningTests` | `test_system_exit_in_sense_is_recorded_not_a_silent_exit` | r2: `raise SystemExit(0)` ended the process with exit 0 and no DAEMON_STOP, so systemd's Restart=on-failure would never have restarted it. | HF-09 |
| `RuntimeHardeningTests` | `test_undeclared_command_request_fails_closed_even_when_swallowed` | Undeclared command request fails closed even when swallowed. | HF-10 |
| `RuntimeHardeningTests` | `test_rejected_duplicate_launch_touches_nothing` | r3.2 (WBS 3.1 §4.2): the second instance cleaned tmp/ before taking the lock and deleted the running instance's in-flight files (for git-watch, its index copy). | HF-25 |
| `RuntimeHardeningTests` | `test_torn_first_write_still_starts_the_ledger_with_daemon_start` | r3.2 (WBS 3.0 r4's pre-baseline correction): a crash during the first-ever append left bytes but no complete record; the next start wrote LEDGER_TAIL_QUARANTINED as seq 1, a ledger the battery's own DB-04 then … | DB-04, HF-26 |
| `RuntimeHardeningTests` | `test_policy_is_immutable` | Policy is immutable. | HF-03 |
| `CpuAccountingTests` | `test_cpu_includes_commands_the_daemon_ran` | Cpu includes commands the daemon ran. | HF-18 |
| `UnitHardeningTests` | `test_unit_allows_the_landlock_syscalls` | Unit allows the landlock syscalls. | HF-07 |
| `UnitHardeningTests` | `test_resolve_syscall_filter` | Resolve syscall filter. | HF-07 |
| `UnitHardeningTests` | `test_unsafe_paths_are_refused` | Unsafe paths are refused. | HF-22 |
| `RenderLabelTests` | `test_label_with_trailing_newline_is_refused` | Label with trailing newline is refused. | HF-13 |
| `RenderEfficiencyTests` | `test_binary_search_matches_the_linear_scan` | Binary search matches the linear scan. | HF-21 |
| `CliRobustnessTests` | `test_verify_with_a_bad_manifest_reports_instead_of_crashing` | Verify with a bad manifest reports instead of crashing. |  |
| `CliRobustnessTests` | `test_battery_with_invalid_json_reports_instead_of_crashing` | Battery with invalid json reports instead of crashing. |  |

#### `tests/test_landlock.py` (13 tests)

Landlock (AP-01): ABI detection, the pure helpers, and real enforcement in forked

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `AbiTests` | `test_abi_version_is_an_int` | Abi version is an int. |  |
| `AbiTests` | `test_this_workspace_kernel_supports_landlock` | Documents the environment this suite runs in; the DGX (aarch64, a different kernel) must be checked separately per ADJUDICATION-AP.md. |  |
| `PureHelperTests` | `test_fs_access_mask_grows_with_abi` | Fs access mask grows with abi. |  |
| `PureHelperTests` | `test_fs_access_for_file_drops_directory_only_rights` | Fs access for file drops directory only rights. |  |
| `PureHelperTests` | `test_fs_access_for_directory_keeps_every_requested_right` | Fs access for directory keeps every requested right. |  |
| `PureHelperTests` | `test_gaps_finds_a_denied_path_inside_a_granted_read` | Gaps finds a denied path inside a granted read. |  |
| `PureHelperTests` | `test_gaps_empty_when_reads_and_denies_do_not_overlap` | Gaps empty when reads and denies do not overlap. |  |
| `SupervisorDomainTestModeTests` | `test_unavailable_landlock_is_non_fatal_in_test_mode` | Unavailable landlock is non fatal in test mode. |  |
| `SupervisorDomainTestModeTests` | `test_unavailable_landlock_refuses_outside_test_mode` | Unavailable landlock refuses outside test mode. |  |
| `EnforcementTests` | `test_supervisor_domain_blocks_denied_reads_and_outside_writes` | Supervisor domain blocks denied reads and outside writes. |  |
| `EnforcementTests` | `test_nested_child_domain_can_only_narrow_never_widen` | Nested child domain can only narrow never widen. | AP-04 |
| `EnforcementTests` | `test_nothing_the_daemon_can_write_or_merely_reads_can_be_executed` | r4.6 (HF-36): the domain granted EXECUTE on the output directory and on every declared read path, so if the in-process layers were bypassed, a binary written into the output directory, or dropped by someone else into … | HF-36 |
| `EnforcementTests` | `test_output_directory_gets_every_right_the_abi_handles` | Output directory gets every right the abi handles. |  |

#### `tests/test_layers.py` (4 tests)

Framework / services boundary (r3.4, PD-53; a Python reading of the Asterinas framekernel

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `LayerBoundaryTests` | `test_service_modules_import_nothing_that_reaches_the_os` | Service modules import nothing that reaches the os. |  |
| `LayerBoundaryTests` | `test_service_modules_never_call_open` | Service modules never call open. |  |
| `LayerBoundaryTests` | `test_ctypes_only_where_declared` | Ctypes only where declared. |  |
| `LayerBoundaryTests` | `test_exactly_one_truncate_site` | r4's audit rule: truncation destroys bytes, so it happens in one reviewed place (ledger.recover, after the torn tail is copied to quarantine). |  |

#### `tests/test_ledger.py` (16 tests)

Ledger: append and verify, every corruption category, torn tails, uncertain commits.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `LedgerTests` | `test_append_and_verify` | Append and verify. |  |
| `LedgerTests` | `test_flipped_byte` | Flipped byte. |  |
| `LedgerTests` | `test_swapped_lines` | Swapped lines. |  |
| `LedgerTests` | `test_missing_line` | Missing line. |  |
| `LedgerTests` | `test_non_canonical_line` | Non canonical line. |  |
| `LedgerTests` | `test_foreign_daemon` | Foreign daemon. |  |
| `LedgerTests` | `test_newer_schema_fails_explicitly` | Newer schema fails explicitly. |  |
| `LedgerTests` | `test_overlong_line` | Overlong line. |  |
| `LedgerTests` | `test_corrupt_last_complete_line_is_not_a_torn_tail` | Corrupt last complete line is not a torn tail. |  |
| `LedgerTests` | `test_torn_tail_garbage_is_quarantined` | Torn tail garbage is quarantined. |  |
| `LedgerTests` | `test_complete_record_without_newline_is_quarantined` | Complete record without newline is quarantined. |  |
| `LedgerTests` | `test_two_torn_tails_never_overwrite` | Two torn tails never overwrite. |  |
| `LedgerTests` | `test_prepare_refuses_before_writing` | Prepare refuses before writing. |  |
| `LedgerTests` | `test_fsync_failure_is_uncertain` | Fsync failure is uncertain. |  |
| `LedgerTests` | `test_short_write_is_uncertain` | Short write is uncertain. |  |
| `LedgerTests` | `test_streaming_pass_memory_does_not_depend_on_length` | Streaming pass memory does not depend on length. |  |

#### `tests/test_manifest.py` (15 tests)

Manifest: the example validates; every refusal category is enforced.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `ManifestTests` | `test_example_is_valid` | Example is valid. |  |
| `ManifestTests` | `test_hash_ignores_key_order_and_whitespace` | Hash ignores key order and whitespace. |  |
| `ManifestTests` | `test_unknown_keys_refused_at_every_level` | Unknown keys refused at every level. |  |
| `ManifestTests` | `test_missing_key` | Missing key. |  |
| `ManifestTests` | `test_acting_daemons_refused` | Acting daemons refused. |  |
| `ManifestTests` | `test_network_refused` | Network refused. |  |
| `ManifestTests` | `test_forbidden_commands` | Forbidden commands. |  |
| `ManifestTests` | `test_reads_inside_denied_paths` | Reads inside denied paths. |  |
| `ManifestTests` | `test_output_dir_placement` | Output dir placement. |  |
| `ManifestTests` | `test_reserved_event_types` | Reserved event types. |  |
| `ManifestTests` | `test_step_timeout_bounded_by_watchdog` | Step timeout bounded by watchdog. |  |
| `ManifestTests` | `test_system_unit_needs_non_root_user` | System unit needs non root user. |  |
| `ManifestTests` | `test_paths_limited_to_safe_characters` | Paths limited to safe characters. |  |
| `ManifestTests` | `test_purpose_cannot_carry_systemd_specifiers` | Purpose cannot carry systemd specifiers. |  |
| `ManifestTests` | `test_floats_refused` | Floats refused. |  |

#### `tests/test_misc.py` (14 tests)

Unit generator, purity check, battery and source hygiene.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `UnitgenTests` | `test_example_unit_carries_every_required_directive` | Example unit carries every required directive. |  |
| `UnitgenTests` | `test_lint_detects_a_removed_directive` | Lint detects a removed directive. |  |
| `UnitgenTests` | `test_user_units_carry_a_warning` | User units carry a warning. |  |
| `UnitgenTests` | `test_a_required_path_becomes_a_start_condition` | r4.8 (from the first pilot): with a locked drive, a daemon whose output is on it must be skipped by systemd, not stopped fail-closed (78, never restarted). |  |
| `UnitgenTests` | `test_part_of_binds_the_unit_to_another` | r4.8 (from the first pilot): an open ledger keeps a drive busy, so a daemon writing to a drive another unit owns must stop before it and start with it, never at boot. |  |
| `UnitgenTests` | `test_install_plan_is_text_only` | Install plan is text only. |  |
| `PurityTests` | `test_good_module` | Good module. |  |
| `PurityTests` | `test_example_is_pure` | Example is pure. |  |
| `PurityTests` | `test_each_forbidden_construct` | Each forbidden construct. |  |
| `PurityTests` | `test_required_functions_and_arity` | Required functions and arity. |  |
| `BatteryTests` | `test_example_passes_or_is_incomplete_never_fails` | Example passes or is incomplete never fails. | DB-18, DB-19, DB-20, HF-35, PD-76 |
| `BatteryTests` | `test_impure_daemon_fails` | Impure daemon fails. | DB-02, DB-03 |
| `RuntimeStructureTests` | `test_gc_collect_runs_once_per_cycle_not_inside_the_sleep_loop` | Structural guard for the multi-daemon GC-forcing addition (r2): gc.collect() must be called exactly once per cycle of the outer loop, and not inside the inner sleep loop (which would call it many times per cycle, … |  |
| `SourceHygieneTests` | `test_no_raw_invisible_characters_in_source` | Trojan-source guard: bidi, zero-width and control characters appear only as escapes. |  |

#### `tests/test_proc.py` (6 tests)

Hardened subprocess: allowlist, scrubbed environment, bounds, timeout, and the F4 index-copy fix.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `ProcTests` | `test_only_allowlisted_commands_run` | Only allowlisted commands run. |  |
| `ProcTests` | `test_output_is_bounded` | Output is bounded. |  |
| `ProcTests` | `test_timeout_kills_the_group` | Timeout kills the group. |  |
| `ProcTests` | `test_git_ignores_user_configuration` | Script-board F5: color.ui=always and diff.external in the user's config must not leak. | F5 |
| `ProcTests` | `test_inherited_git_selection_variables_cannot_redirect_git` | r4.0 (hardening review, item 2): GIT_DIR, GIT_WORK_TREE and GIT_OBJECT_DIRECTORY in the daemon's own environment must not redirect a call to a decoy repository. Every child starts from SAFE_ENV (an allowlist), so … |  |
| `ProcTests` | `test_index_copy_leaves_the_real_index_untouched` | Script-board F4: porcelain `git diff` rewrites .git/index; the index copy does not. | F4 |

#### `tests/test_render.py` (10 tests)

Rendering: hostile values stay inside their blocks; structure never changes; bounds hold.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `RenderTests` | `test_hostile_corpus_is_inert` | Hostile corpus is inert. |  |
| `RenderTests` | `test_structure_invariance_property` | Structure invariance property. |  |
| `RenderTests` | `test_escapes_are_visible_and_unambiguous` | Escapes are visible and unambiguous. |  |
| `RenderTests` | `test_multiline_values_are_prefixed` | Multiline values are prefixed. |  |
| `RenderTests` | `test_fence_longer_than_any_backtick_run` | Fence longer than any backtick run. |  |
| `RenderTests` | `test_labels_are_validated` | Labels are validated. |  |
| `RenderTests` | `test_values_are_type_checked` | Values are type checked. |  |
| `RenderTests` | `test_size_bound_truncates_with_marker` | Size bound truncates with marker. |  |
| `RenderTests` | `test_stamp_is_validated` | Stamp is validated. |  |
| `RenderTests` | `test_deterministic` | Deterministic. |  |

#### `tests/test_runtime.py` (18 tests)

Runtime end to end: lifecycle, errors, fail-closed policy, the watchdog, refusals.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `RuntimeTests` | `test_lifecycle_and_digest` | Lifecycle and digest. |  |
| `RuntimeTests` | `test_hostile_value_is_contained_in_the_digest` | Hostile value is contained in the digest. |  |
| `RuntimeTests` | `test_errors_are_bounded_and_streaks_suppressed` | Errors are bounded and streaks suppressed. |  |
| `RuntimeTests` | `test_error_streak_clears_on_success` | Error streak clears on success. |  |
| `RuntimeTests` | `test_policy_violation_fails_closed_even_when_swallowed` | Policy violation fails closed even when swallowed. |  |
| `RuntimeTests` | `test_undeclared_event_type_is_refused` | Undeclared event type is refused. |  |
| `RuntimeTests` | `test_float_payload_is_refused` | Float payload is refused. |  |
| `RuntimeTests` | `test_test_mode_refused_under_systemd` | Test mode refused under systemd. |  |
| `RuntimeTests` | `test_impure_code_never_starts` | Impure code never starts. |  |
| `RuntimeTests` | `test_unsafe_output_directory_is_refused` | Unsafe output directory is refused. |  |
| `RuntimeTests` | `test_foreign_files_are_reported_not_touched` | Foreign files are reported not touched. |  |
| `RuntimeTests` | `test_corrupt_ledger_refuses_and_changes_nothing` | Corrupt ledger refuses and changes nothing. |  |
| `RuntimeTests` | `test_watchdog_pings_stop_while_an_observation_hangs` | No background pinger: a blocked sense() starves the watchdog, so systemd would restart it. |  |
| `RuntimeTests` | `test_jitter_bound_is_zero_in_test_mode` | Jitter bound is zero in test mode. |  |
| `RuntimeTests` | `test_jitter_bound_is_a_fraction_of_the_interval` | Jitter bound is a fraction of the interval. |  |
| `RuntimeTests` | `test_jitter_bound_is_capped_for_long_intervals` | 86400s (the manifest maximum) times 5% would be 4320s; it must not drift by hours. |  |
| `RuntimeTests` | `test_jitter_disabled_in_test_mode_and_recorded_for_a_real_run` | Jitter disabled in test mode and recorded for a real run. |  |
| `RuntimeTests` | `test_example_daemon_runs` | Example daemon runs. |  |
