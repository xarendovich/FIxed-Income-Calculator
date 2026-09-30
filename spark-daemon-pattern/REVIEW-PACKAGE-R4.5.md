# Spark daemon pattern: secondary review package (r4.5)

- **For:** secondary reviewers (people or models). This document stands alone: every file it names is in `spark-daemon-pattern/` on branch `claude/clever-bardeen-qqkxez`, for reviewers who can read the repository.
- **State at r4.5:**
  - contract 3.1.0;
  - 210 of 210 self-tests pass;
  - the four reference daemons pass all 19 battery checks (DB-01 to DB-18 and the new DB-20).
- **What to review first:** Part 1, the conformance battery and the test list, is the main request. Part 2 records the adjudication it builds on, including the two defects fixed in this revision (HF-34 and HF-35). Part 3 reviews the SQLite three-ledger recommendations.

**How to mark:** for each check, planned check, planned test or decision, give AGREE, CHANGE or REJECT. Say which evidence type you are relying on: reproduced, measured, traced or opinion. Part 1 §6 lists the questions this review most needs answered.

## Part 1: Conformance battery review and test list

- **Status:** FOR SECONDARY REVIEW. It describes the battery and self-tests as they are at r4.5 (contract 3.1.0), what each check cannot catch, the gaps, and the planned checks and tests that the adjudication (`ADJUDICATION-R4.5.md`) calls for. Every number here was produced on this revision.
- **Revision:** r4.5, 2026-09-30, by Claude.
- **Evidence at this revision:**
  - 210 of 210 self-tests pass (Python 3.12, `-W error::ResourceWarning`).
  - The four reference daemons pass all 19 battery checks: 18 original plus DB-20.
  - The contract check is up to date (3.1.0).
  - The workspace has Landlock ABI 7, `strace` and `systemd-analyze`; it has no systemd PID 1, and its cgroup is v1.

**Two kinds of evidence, kept apart on purpose:**
- **The battery** judges *one daemon*. It runs that daemon's real code, in child processes, in a disposable workspace, and its PASS is the only admissible evidence for activating that daemon.
- **The self-tests** judge *the skeleton*: properties every daemon inherits, such as the ledger format, the blind clock, purity and the unit generator.

A property belongs in the battery when a daemon's own code or manifest can break it. Otherwise it belongs in the self-tests. Reviewers should challenge any item they think sits on the wrong side of that line.

### 1. Code map

What each module does, its size in lines, and where the adjudicated work will land.

| Module | Lines | Responsibility | Adjudicated change that lands here |
| --- | --- | --- | --- |
| `manifest.py` | 432 | Closed-schema manifest and validation; `blind_limit_seconds` range | Contract 4.0.0: `pacing` (PD-85), consequence level (PD-01.2), schema-3 migration hint |
| `runtime.py` | 576 | Start-up order, the cycle loop, the blind clock, heartbeats, stop | **r4.5:** boot-time basis for inherited blindness (HF-34). Contract 4.0.0: pacing |
| `context.py` | 183 | The only I/O a daemon may perform (`read_text`, `list_dir`, `stat`, `run`, `git`, `unsettled`) | PD-83: truncation always raises `TooLarge` |
| `proc.py` | 217 | Bounded, allowlisted subprocesses; the Git hardening profile | — |
| `guard.py` | 258 | Path policy, output-directory checks, the audit hook | — |
| `landlock.py` | 246 | Kernel confinement (the only `ctypes` besides probes) | — |
| `purity.py` | 215 | Static purity check before import | — |
| `ledger.py` | 256 | Append-only JCS hash chain; recovery; quarantine | An independent verifier is planned *outside* this package (B-6) |
| `canonical.py` | 118 | Canonical JSON and hashing | Shared rules for any other ledger format (see `SQLITE-LEDGER-REVIEW.md`) |
| `render.py` | 176 | Structure-safe digest | — |
| `unitgen.py` | 250 | The systemd unit and its lint | The exit-code table (N-12, PD-63's split) |
| `notify.py` | 23 | `sd_notify` | — |
| `contract.py` | 374 | The machine-readable author contract | 3.1.0 now; 4.0.0 batched |
| `battery.py` | 800 | DB-01 to DB-18 and DB-20 | **r4.5:** DB-20. Planned: DB-19, DB-21, DB-22 (§4) |
| `probes.py` | 266 | Child-process probes the battery uses | — |
| `handoff.py`, `scaffold.py`, `cli.py`, `paths.py`, `__init__.py` | 684 | Candidate envelopes, validate and precheck, command line, constants | Exit codes: C-1 and PD-63's split, in contract 4.0.0 |

### 2. The checks, one by one

"Cannot catch" is the list a secondary reviewer should attack. **Independent** says whether the check uses code other than the skeleton code it judges.

| ID | What it checks, and how | Cannot catch | Independent? | Environment |
| --- | --- | --- | --- | --- |
| DB-01 | The manifest validates against the closed schema | Limits that are valid alone but wrong together (U-8). Only a few cross-checks exist today (planned DB-21) | Uses the skeleton's validator | Any |
| DB-02 | The static purity check of `daemon.py` | Routes the syntactic rules do not know (residual risk 2 in `HARDENING.md`). Landlock is the backstop, which DB-17 proves | Skeleton's checker | Any |
| DB-03 | Confinement: 5 cycles, then a snapshot comparison of the workspace `HOME` and the daemon folder; no audit-hook events | Writes outside the snapshot roots (Landlock and DB-17 cover these); behaviour that needs inputs the fixture lacks | Snapshot is independent; the audit count comes from the skeleton | Any |
| DB-04 | Ledger integrity and provenance after a normal run: verifies, starts with `DAEMON_START`, ends with `DAEMON_STOP`, only declared types, **zero `DAEMON_ERROR`**, digests match | A daemon that never accepts a cycle (now DB-20, HF-35). Verifier defects, because it uses the skeleton's own verifier (B-6) | **No** | Any |
| DB-05 | Crash and restart: 8 SIGKILLs (3 with `--quick`) at seeded random points; the chain verifies; every restart is flagged unclean; quarantine files match records | Power loss: SIGKILL leaves the page cache intact, so an fsync that never happened cannot be seen | Skeleton's verifier | Any |
| DB-06 | Torn tails (garbage; a complete record without its newline) are quarantined byte for byte and recorded | Other tear shapes (a fuzz corpus is a candidate addition) | Skeleton's verifier | Any |
| DB-07 | One flipped byte in a middle record: the daemon refuses to start (65) and changes nothing | Only one corruption shape | Skeleton's verifier | Any |
| DB-08 | Injected fsync `EIO`: exit 70 at once, and a restart recovers | — | `strace` fault injection | **Needs `strace`**, otherwise UNKNOWN, so INCOMPLETE |
| DB-09 | Injected `ENOSPC` on write: exit 70, and a restart recovers | A real full disk with partial writes | `strace` | **Needs `strace`** |
| DB-10 | The digest stays structure-safe under hostile values (seeded probe) | Daemons without a digest (N/A) | Probe | Any |
| DB-11 | The notify protocol against a stand-in `NOTIFY_SOCKET`: `READY` after `DAEMON_START`, watchdog pings, `STOPPING` | Real systemd behaviour (it has never run under PID 1) | Stand-in socket | Any |
| DB-12 | A second instance is refused (73); SIGTERM gives `DAEMON_STOP` and exit 0 | — | — | Any |
| DB-13 | The audit hook blocks the fixed list of forbidden operations for this manifest | Operations not on the list | Probe, in-process | Any |
| DB-14 | Resource budget: projected CPU per cycle, and peak memory as `max(wait4 ru_maxrss, DAEMON_STOP RUSAGE_SELF)` over 21 cycles | **Memory held by a daemon while its child runs: measured 38 % low** (`evidence/r4.4/`). Worst-case inputs (fixture data is small, PD-71) | Partly | Any. **To be replaced by DB-19** (PD-76) |
| DB-15 | The unit carries every required directive; `systemd-analyze security --offline` exposure is under the threshold; start-up syscalls are allowed by the unit's seccomp filter | The unit's behaviour under a real service manager | `systemd-analyze` | **Needs `systemd-analyze`**, otherwise UNKNOWN |
| DB-16 | Verification is read-only and repeatable: two runs agree, bytes and mtime unchanged | "Two runs agree" runs the same code twice (B-6) | **No** | Any |
| DB-17 | With the audit hook in record-only mode, the kernel alone blocks a forbidden write, a denied read and a TCP connect | Kernels without Landlock (the check becomes UNKNOWN; the runtime refuses to start there outside test mode) | Kernel | Landlock ABI dependent |
| DB-18 | `DAEMON_START.landlock` is present and matches this host's ABI and the manifest's gaps | Another host's ABI: a PASS here says nothing about the DGX's ABI (B-5) | — | Host specific |
| **DB-20** (r4.5) | **The daemon observes:** 12 cycles against a blind limit of five test intervals (1.0 s); accepted cycles appear in the heartbeats; no `SENSE_BLIND` | A daemon that observes the fixture but not production inputs (PD-71). Slow first cycles longer than 1.0 s would be reported as blind (none of the reference daemons comes close) | Uses the runtime's own counters | Any |

**Verdict rule:**
- PASS only when nothing FAILs and nothing is UNKNOWN;
- FAIL if anything FAILs;
- INCOMPLETE otherwise.

Exit codes are PASS 0, INCOMPLETE 3, FAIL 1. They change to 0/3/4 in contract 4.0.0 (C-1). The report binds the manifest digest, the code digest and the contract identity.

### 3. Gaps, in order of risk

| # | Gap | Consequence | Plan |
| --- | --- | --- | --- |
| G-1 | **Memory is read per process** (DB-14) | A daemon can pass while its cgroup exceeds `MemoryMax=` (38 % under-read, measured) | DB-19 (PD-76), with a separate cgroup helper |
| G-2 | **The ledger checks are not independent** (DB-04, DB-16) | A verifier defect passes its own output | DB-22: a verifier that does not import `spark_daemon`, cross-checked with a second JCS implementation |
| G-3 | **Limits are not checked against each other** in the battery | HF-28, HF-30, HF-31 and HF-32 were all limit interactions | DB-21: the budget table (PD-69), including time windows (PD-86) |
| G-4 | **The fixtures are small** | Worst-case behaviour (a huge repository, a full inbox) is never exercised | Worst-case fixtures per observation type, plus a scaling series (PD-71). The first such fixture is `dir-watch` over its cap (HF-33) |
| G-5 | **PASS is host-specific but does not say so** | A PASS on this workspace is read as a PASS on the DGX | Bind a host fingerprint (kernel, Landlock ABI, cgroup version, tool versions) into the report (B-2); host-caused N/A counts as an owed environment |
| G-6 | **Never run under systemd PID 1** | DB-11 and DB-15 simulate | First DGX step: install one reference daemon and confirm `DAEMON_START.landlock.status == "enforced"` |
| G-7 | **Two checks need tools** (`strace`, `systemd-analyze`) | Without them the verdict is INCOMPLETE, which is correct, but CI must decide what that means | The adjudicated CI rule (`INCOMPLETE` only with a stated environment reason) |
| G-8 | **Power-loss durability is not provable by SIGKILL** | Missing fsyncs would pass DB-05 | DB-08 covers fsync *errors*. A write-order trace of fsync placement is a candidate check |

### 4. Planned checks

New IDs are never edited in place (PD-78).

| ID | Check | From | When |
| --- | --- | --- | --- |
| **DB-19** | A direct cgroup peak (v2 `memory.peak`, or v1 `memory.max_usage_in_bytes` against a v1 limit) for a fresh cgroup holding the daemon and every child from its first instruction. Replaces DB-14's memory half; DB-14 is retired and its CPU half carried over | PD-76 (owner), `DECISION-BRIEF.md` §2.4 | Next |
| **DB-21** | A budget table: worst-case cycle against the watchdog (at most half), stop timeout against one watchdog period, blind limit against intervals, and retry bounds over time | PD-69, PD-86, U-8 | With Phase 1's budget report |
| **DB-22** | Independent ledger verification: a separate verifier and a second JCS implementation agree with the skeleton's | B-6, U-7 | Phase 2 |
| DB-23 | Worst-case fixtures per observation type: over-capacity input fails the cycle and never records a partial result | PD-71, HF-33, PD-83 | With DB-19 |
| DB-2x | The LTC provider-neutral suite for each declared connection | PD-82, LI-11 | With the first request channel |

### 5. Planned self-tests (skeleton properties)

| ID | Test | From |
| --- | --- | --- |
| T-EX1 | One exit-code table drives the runtime constants, `RestartPreventExitStatus` and the notifier mapping; the test fails if any of them drifts | PD-63 split, N-12 |
| T-EX2 | `EXIT_POLICY` and `EXIT_SENSE_BLIND` differ, and both are never restarted | PD-63 split |
| T-TR1 | `ctx.run`, `ctx.git` and `ctx.list_dir` raise `TooLarge` one byte or one entry past the cap, and succeed exactly at it | PD-83 (modified) |
| T-PC1 | `pacing: BOUNDED_JITTER` stays within its bound; `DETERMINISTIC_SPREAD` gives the same phase on restart; `FIXED_PHASE` injects nothing; watchdog pings continue through any offset | PD-85, N-11 |
| T-ST1 | The staleness checker flags a ledger that stops changing within `T`, using its own monotonic clock, and is unaffected by a wall-clock step | Phase 1 (modified), N-5 |
| T-CG1 | The cgroup helper places the child before `exec`, cleans up on every exit path, and records the cgroup version | DB-19, N-4 |
| T-N1 to T-N14 | One test per seam when its decision is built: supervisor/worker (N-1), register binding (N-2), declared connection and LTC bounds (N-3), cgroup (N-4), staleness (N-5), inbound acknowledgement never carries commands (N-6), gate epoch fencing (N-7), witness independence (N-8), recorder anchoring (N-9), pacing and watchdog (N-11), exit table (N-12), schema-3 migration (N-13), survival mode never read as authority (N-14). N-10 is removed by PD-83's modification | `REVIEW-PACKAGE-COMBINED.md`, reviewer focus |

### 6. Questions for the secondary reviewer

1. Is any check on the wrong side of the battery/self-test line (§ introduction)?
2. DB-20 uses a 1.0 s blind limit over 12 cycles. Is there a legitimate daemon whose first accepted cycle takes longer in the battery workspace, and should the limit scale with the manifest's `step_timeout_seconds`?
3. Is `max(ru_maxrss)` ever the *right* basis once DB-19 exists, for example for a daemon with no children?
4. Should DB-05 add a power-loss simulation (for example a `dm-flakey` or fsync-trace check), or is DB-08 plus code review enough?
5. For G-5: which host facts must a PASS bind before it can be used on another machine?
6. Which of the 210 self-tests below would you delete as redundant, and which property has no test at all?

### 7. The self-test list

Generated from the test files at r4.5 (`evidence/r4.5/testlist.py`). "Cites" lists the defect (HF), decision (PD), battery check (DB) and rule identifiers each test names in its body or docstring.
<!-- generated: 210 tests -->

##### `tests/test_blind.py` (29 tests)

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

##### `tests/test_canonical.py` (9 tests)

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

##### `tests/test_guard.py` (8 tests)

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

##### `tests/test_handoff.py` (31 tests)

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
| `ScaffoldTests` | `test_refuses_a_non_empty_folder_and_bad_names` | Refuses a non empty folder and bad names. |  |
| `ScaffoldTests` | `test_scaffold_passes_validate_and_user_unit_variant` | Scaffold passes validate and user unit variant. |  |
| `SymlinkStatTests` | `test_planted_symlink_is_a_link_not_a_violation` | Planted symlink is a link not a violation. | HF-17 |
| `SymlinkStatTests` | `test_following_reads_still_judge_the_target` | Following reads still judge the target. | HF-17 |
| `BatteryCatchesAlwaysFailingDaemonsTests` | `test_a_daemon_that_errors_every_cycle_fails_db04` | r2 passed the raiser fixture: its sense() fails every cycle in the battery's workspace (no ~/data/mode.txt), yet the process exits 0 with a valid chain. | DB-04, HF-16 |
| `BatteryCatchesAlwaysFailingDaemonsTests` | `test_a_daemon_that_never_observes_fails_db20` | r4.5 (HF-35): every cycle unsettled means no DAEMON_ERROR and no event; with its digest disabled, r4.4's battery reported RESULT: PASS for it. | DB-04, DB-20, HF-35 |

##### `tests/test_hardening.py` (40 tests)

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

##### `tests/test_landlock.py` (12 tests)

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
| `EnforcementTests` | `test_output_directory_gets_every_right_the_abi_handles` | Output directory gets every right the abi handles. |  |

##### `tests/test_layers.py` (4 tests)

Framework / services boundary (r3.4, PD-53; a Python reading of the Asterinas framekernel

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `LayerBoundaryTests` | `test_service_modules_import_nothing_that_reaches_the_os` | Service modules import nothing that reaches the os. |  |
| `LayerBoundaryTests` | `test_service_modules_never_call_open` | Service modules never call open. |  |
| `LayerBoundaryTests` | `test_ctypes_only_where_declared` | Ctypes only where declared. |  |
| `LayerBoundaryTests` | `test_exactly_one_truncate_site` | r4's audit rule: truncation destroys bytes, so it happens in one reviewed place (ledger.recover, after the torn tail is copied to quarantine). |  |

##### `tests/test_ledger.py` (16 tests)

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

##### `tests/test_manifest.py` (15 tests)

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

##### `tests/test_misc.py` (12 tests)

Unit generator, purity check, battery and source hygiene.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `UnitgenTests` | `test_example_unit_carries_every_required_directive` | Example unit carries every required directive. |  |
| `UnitgenTests` | `test_lint_detects_a_removed_directive` | Lint detects a removed directive. |  |
| `UnitgenTests` | `test_user_units_carry_a_warning` | User units carry a warning. |  |
| `UnitgenTests` | `test_install_plan_is_text_only` | Install plan is text only. |  |
| `PurityTests` | `test_good_module` | Good module. |  |
| `PurityTests` | `test_example_is_pure` | Example is pure. |  |
| `PurityTests` | `test_each_forbidden_construct` | Each forbidden construct. |  |
| `PurityTests` | `test_required_functions_and_arity` | Required functions and arity. |  |
| `BatteryTests` | `test_example_passes_or_is_incomplete_never_fails` | Example passes or is incomplete never fails. | DB-18, DB-19, DB-20, HF-35, PD-76 |
| `BatteryTests` | `test_impure_daemon_fails` | Impure daemon fails. | DB-02, DB-03 |
| `RuntimeStructureTests` | `test_gc_collect_runs_once_per_cycle_not_inside_the_sleep_loop` | Structural guard for the multi-daemon GC-forcing addition (r2): gc.collect() must be called exactly once per cycle of the outer loop, and not inside the inner sleep loop (which would call it many times per cycle, … |  |
| `SourceHygieneTests` | `test_no_raw_invisible_characters_in_source` | Trojan-source guard: bidi, zero-width and control characters appear only as escapes. |  |

##### `tests/test_proc.py` (6 tests)

Hardened subprocess: allowlist, scrubbed environment, bounds, timeout, and the F4 index-copy fix.

| Class | Test | What it pins | Cites |
| --- | --- | --- | --- |
| `ProcTests` | `test_only_allowlisted_commands_run` | Only allowlisted commands run. |  |
| `ProcTests` | `test_output_is_bounded` | Output is bounded. |  |
| `ProcTests` | `test_timeout_kills_the_group` | Timeout kills the group. |  |
| `ProcTests` | `test_git_ignores_user_configuration` | Script-board F5: color.ui=always and diff.external in the user's config must not leak. | F5 |
| `ProcTests` | `test_inherited_git_selection_variables_cannot_redirect_git` | r4.0 (hardening review, item 2): GIT_DIR, GIT_WORK_TREE and GIT_OBJECT_DIRECTORY in the daemon's own environment must not redirect a call to a decoy repository. Every child starts from SAFE_ENV (an allowlist), so … |  |
| `ProcTests` | `test_index_copy_leaves_the_real_index_untouched` | Script-board F4: porcelain `git diff` rewrites .git/index; the index copy does not. | F4 |

##### `tests/test_render.py` (10 tests)

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

##### `tests/test_runtime.py` (18 tests)

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

## Part 2: Adjudication of the r4.2 to r4.4 improvements

- **Asked:** the owner asked to "review and adjudicate these improvements": everything proposed in the combined review package (`REVIEW-PACKAGE-COMBINED.md`). That means the amendments to the four settled rulings and the open decisions in Packages A, B and C.
- **Authority:** adjudicated by Claude at the owner's request on 2026-09-30.
  - The owner's Class C authority is unchanged: each verdict stands unless the owner overturns it.
  - Two items interpret the owner's *own* rulings. They are marked **OWNER** and take effect only when the owner confirms them.
  - Nothing here activates a daemon, unlocks a class or changes the LTC.
- **Method:** each item was reviewed again with fresh eyes before a verdict, including the recommendations this repository made itself. Four of them are changed below (§3). Two findings were turned from "traced" or "not yet seen" into reproduced defects and fixed with tests (HF-34, HF-35). Every verdict names its evidence.
- **Verdict vocabulary:** ADOPTED, ADOPTED WITH MODIFICATION, DEFERRED (with a trigger), REJECTED. BUILT means done in this revision, with tests.

### 1. Summary

| Group | Adopted | With modification | Deferred | Rejected | Built in r4.5 |
| --- | --- | --- | --- | --- | --- |
| Settled-ruling amendments (5) | 3 (one needs the OWNER) | 1 (OWNER) | 1 | 0 | PD-70's clock basis (HF-34) |
| Package A: transport contract (5, plus the LTC markup) | 4 | 2 | 0 | 0 | — |
| Package B: Break Glass (5) | 4 | 1 | 0 | 0 | — (blocked on PD-72, PD-01.7, PD-40) |
| Package C (11 lines) | 9 | 2 | A-8, A-9, A-10 (inside one line) | 0 | DB-20 (HF-35, found during this review) |
| Seams N-1 to N-14 | adopted as the required test list for their decisions | | | | |

Nothing is rejected: the second look changed four recommendations instead (PD-83, PD-85, PD-01.13 and the Phase 1 staleness checker), and tightened PD-76.

### 2. Two defects found and fixed while adjudicating

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

### 3. Verdicts

#### 3.1 Amendments to settled rulings

| Item | Verdict | Conditions, and why |
| --- | --- | --- |
| **PD-63: exit-code split** (`EXIT_POLICY` ≠ `SENSE_BLIND`) | **ADOPTED**, built with contract 4.0.0 | Keep `SENSE_BLIND` at 78 (the Observer's code). Choose `EXIT_POLICY`'s new code from the Observer's exit table under PD-35, once that table is read, not before. **Condition (seam N-12):** one exit-code table generates the runtime constants, the unit's `RestartPreventExitStatus`, and the notifier's mapping; a test fails if any drifts |
| **PD-63: where `T` is set** | **DEFERRED** until PD-40 is built | No evidence of a problem today. Two places for one number is a cost worth paying only when per-host tuning is needed |
| **PD-70: clock basis** | **ADOPTED and BUILT** (HF-34, contract 3.1.0) | Reproduced, fixed, tested (§2) |
| **PD-01.9: scope clarification** | **ADOPTED as the working interpretation; OWNER** | It interprets the owner's own ruling, so it needs the owner's word. Until then this repository designs to it: the Blind Forester does not unlock `critical`; BF-5 is mandatory where a person could be harmed; it governs survival *mode*, never emergency *authority*; PD-72 is a prerequisite |
| **PD-76: mechanism wording** | **ADOPTED WITH MODIFICATION; OWNER** (it rewords the owner's ruling) | Adopt "a direct cgroup peak: v2 `memory.peak` or v1 `memory.max_usage_in_bytes`". **Modification:** a v1 reading may decide PASS or FAIL only against a limit stated for v1 hosts, because v1 counts page cache differently. On the DGX (v2), only v2 readings are admissible. The battery's privileged step, creating a cgroup and moving the process into it, goes in a small separate helper with its own test, not inline (seam N-4) |

#### 3.2 Package A: the transport contract

| Item | Verdict | Conditions, and why |
| --- | --- | --- |
| **PD-82** Bind, do not embed | **ADOPTED** | The LTC's own scope (LTC-01, "not a seventh pillar") and this repository's PD-52 agree |
| **PD-83** Truncated reads raise | **ADOPTED WITH MODIFICATION** | **Change from the recommendation:** in contract 4.0.0, truncation *always* raises `TooLarge`. The opt-in "partial result" type is **not** built until a real daemon needs explicit partial semantics. No reference daemon does: `dir-watch` and `git-watch` both now fail the cycle. This removes seam N-10 instead of guarding it |
| **PD-84** Register uses the LTC binding vocabulary | **ADOPTED** | A design rule for PD-40, with H02's three modifications |
| **PD-85** Declared pacing | **ADOPTED WITH MODIFICATION** | **Change from the recommendation:** adopt the `pacing` field, but keep **today's bounded random jitter as the default**. Deterministic spread is available and recommended for new daemons. There is no measured problem with the current jitter on a host with four daemons: it is already zero in test mode, so it does not hurt testability. Changing every daemon's timing in a batch for a benefit nobody has measured is churn. Revisit when a host runs enough daemons to measure phase collisions |
| **PD-86** Retry bounds hold over time | **ADOPTED** | Evidence: HF-28 |
| **LTC-H01 to H03, this repository's markup** | **ADOPTED for submission** to the LTC reviewers | This repository cannot rule on the LTC. It forwards its markup (a total-time deadline for streams, digest-named trust evidence, bounds that hold over time) |

#### 3.3 Package B: Break Glass and operator-gated recovery

| Item | Verdict | Conditions, and why |
| --- | --- | --- |
| **PD-01.10** Premise, scope, tiers | **ADOPTED** | The beacon tier (BG-S) comes after PD-01.3, PD-72 and PD-01.7. The acting tier (BG-A) is deferred with 3b to kernel v2 |
| **PD-01.11** Mechanisms | **ADOPTED** | A dormant relay; the owner's revised socket; single-use, epoch-bound envelopes |
| **PD-01.12** Epochs, conservative restart rule | **ADOPTED** | Since HF-34, every record carries `boot_id`, which is the natural carrier for the epoch-per-boot rule (BG-9). Build the epoch on it |
| **PD-01.13** Witness and flight recorder | **ADOPTED WITH MODIFICATION** | **Change:** where a person could be harmed (`critical`, still refused), the witness is separate hardware. At `elevated`, it is a separate process tree with its own clock source, and its independence is audited (U-7). "Separate process tree" alone is not enough evidence of independence at `critical` |
| **PD-01.14** Recovery boundary | **ADOPTED** | It is also the Blind Forester's only exit, which completes BF-6 |

#### 3.4 Package C

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

#### 3.5 Seams N-1 to N-14

**ADOPTED as required tests.** Each seam's "what a reviewer should check" becomes a test that must exist when its decision is built. `BATTERY-REVIEW-R4.5.md` §5 lists them as planned tests T-N1 to T-N14. Seam N-10 is removed by PD-83's modification.

### 4. What this changes in the order of work

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

## Part 3: Review of the SQLite three-ledger recommendations

- **Status:** REVIEW AND ADJUDICATION. Decisions PD-87 to PD-92 are adjudicated by Claude at the owner's request, subject to the owner (`ADJUDICATION-R4.5.md` sets out that authority). Nothing here changes code. Four probes were run on this host (`evidence/r4.5/`); every other statement is marked as reasoned.
- **Revision:** r4.5, 2026-09-30, by Claude.
- **Asked:** the owner asked for a review of recommendations "for hardening and improving the daemon pattern's code map":
  - an append-only SQLite event store split into three ledgers (ingestion and events; memory reconciliation; external metadata), for an offline runtime kernel watched by a Git-based observer daemon;
  - `systemd.path` watchers on the SQLite write-ahead log, instead of polling;
  - Unix-domain sockets for all IPC, with POSIX permissions;
  - an application-layer SHA-256 hash chain, with a schema, an ingestion protocol and a verification loop.

### 1. Verdict in brief

The direction is sound, and much of it matches rules this repository already enforces for its own ledgers:
- one writer per ledger;
- append-only records;
- a hash chain from a genesis of 64 zeros;
- verification by a passive observer.

Five points need changing before the design is safe. Three of them are shown by probes on this host:

| # | Point | Evidence | Change |
| --- | --- | --- | --- |
| 1 | **The proposed `payload JSON` column changes what is stored.** A declared type of `JSON` gets NUMERIC affinity in SQLite, so the text `123` is stored as the integer 123, and `1.50` as the real 1.5. The bytes the verifier reads are not the bytes that were hashed | **Measured** (`sqlite_probes.txt`, SQLite 3.45.1) | Declare `payload TEXT NOT NULL CHECK (json_valid(payload))` in a `STRICT` table, which kept `1.50` as text in the probe. Verify the stored bytes; never re-serialize |
| 2 | **Plain concatenation is ambiguous.** `previous_hash + timestamp + event_type + canonical_json(payload)`: event type `A_B` with payload `{"x":1}`, and event type `A_` with payload `B{"x":1}`, give the same input and the same hash | **Measured** (`sqlite_probes.txt`) | Hash the canonical (JCS) encoding of the whole record as one object, including a sequence number and the previous hash. That is what this repository's ledger does (`canonical.py`, `ledger.py`) |
| 3 | **A read-only observer cannot read the ledger after the writer closes.** While the writer is open, a confined reader (uid 65534, directory not writable) reads fine. When the last writer closes, SQLite removes `-wal` and `-shm`, and the reader fails with "attempt to write a readonly database". The observer loses sight of the ledger exactly when it matters most: after the kernel stopped or crashed | **Measured** (`sqlite_readonly_wal_probe.txt`) | The writer keeps the WAL files persistent (`SQLITE_FCNTL_PERSIST_WAL`), or the observer verifies a snapshot the writer exports, or reads with `immutable=1` only on a copy. Solve this before any read-only observer is built (PD-91) |
| 4 | **A hash chain alone does not stop someone who can write the file.** "Any altered bit breaks the link" is true only for an attacker who does not recompute the rows after the edit. Whoever can write the database can rewrite the chain from the edit onward | Reasoned (and the r4 known limit in WBS 3.0) | Anchor the head hash somewhere the writer cannot rewrite: the observer's Git commits, pushed off-host (PD-58). Also add `BEFORE UPDATE` and `BEFORE DELETE` triggers that abort, against accidents, not attackers |
| 5 | **The observer must not govern canonical memory.** "The passive observer daemon reads this ledger to resolve conflicts and govern the canonical memory state", and it "halts memory reconciliation" on corruption. Both are actions. An observer that resolves conflicts is at least an Advisor (class 2a), and halting is class 3 | Against recorded rules: PD-01 §3, S-6, and the Observer's "observation never authorizes action" | The observer verifies, reports and proposes. The kernel, or a gate, writes supersession records and decides to halt. The observer's corruption finding is an escalation record the kernel acts on (PD-87) |

### 2. The three ledgers

**Adopted with modification (PD-92): split by writer authority, and name each writer.** Separating operational metadata from semantic memory is right. What makes a split safe is that each ledger has exactly one writer, and that no component can write a ledger it should only read.

| Ledger | Proposed | Adjudicated |
| --- | --- | --- |
| Ingestion and events | Written only by the active kernel | **Agree.** The kernel is the single writer. Commits use `BEGIN IMMEDIATE`; in WAL mode `EXCLUSIVE` behaves the same. Each commit uses `synchronous=FULL`, because `NORMAL` in WAL mode keeps the database consistent but can lose the last commits on power loss, and a ledger that loses acknowledged records is not a ledger |
| Memory reconciliation | Read by the observer, which "resolves conflicts and governs" | **Modified.** The single writer is the kernel or a governance gate. The observer reads it, verifies supersession paths and proposes resolutions (class 2a). Validity intervals follow S-8 and the HF-34 lesson: they state their clock basis, and order comes from sequence, never from wall-clock timestamps |
| External metadata | Written by WASM plugins "via its socket" | **Agree, made explicit.** A small ledger service owns the file and is its single writer. Plugins send requests over a socket and never hold the file, so the service, not the plugin, computes the chain |

**Row rules for all three**, which are this repository's ledger rules applied to SQLite (PD-88):
- a `STRICT` table;
- `seq INTEGER PRIMARY KEY`, contiguous and checked by the verifier;
- `record TEXT NOT NULL`, holding the exact canonical bytes that were hashed;
- `previous_hash` and `current_hash`, each `CHECK (length(x) = 64 AND x NOT GLOB '*[^0-9a-f]*')`;
- `current_hash UNIQUE`;
- triggers that abort any `UPDATE` or `DELETE`;
- genesis 64 zeros;
- timestamps are informational and never order anything.

### 3. Event-driven wake with `systemd.path`

**Adopted with modification (PD-89): an event is a hint that shortens the wait; it is never the liveness signal.**

- **A path unit starts a unit; it cannot wake a resident process.** If the observer is a long-running daemon (this pattern's shape), a path unit's activation does nothing while the service is already running. It suits a oneshot script, not a resident daemon.
- **Events are coalesced and can be dropped.** Writes that arrive while the triggered unit is still running are coalesced. Path units are rate-limited in recent systemd (`TriggerLimitIntervalSec=`, `TriggerLimitBurst=`): when the limit trips, the unit fails and stops triggering. inotify itself can overflow its queue. So completeness can never rest on events.
- **Silence and death look the same.** With push-only wake, "nothing was written" and "the writer is dead" produce the same silence. This pattern's blind period and heartbeat exist to tell those apart (PD-63, PD-70), and they need a periodic cycle.
- **"The exact millisecond" overstates it.** A write to `-wal` is not yet a commit, so a woken reader can see nothing new (reasoned). In this pattern that case is `ctx.unsettled`: nothing stable was read.

**Design:**
- The resident daemon keeps its poll interval as its **floor**: the most it can be late, and its liveness proof.
- A path unit starts a tiny oneshot that sends the daemon a wake signal, and the daemon's sleep ends early.
- A missed or coalesced event costs at most one interval.
- Nothing inside the skeleton needs inotify. That matters because `ctypes` is confined to `landlock.py` and `probes.py` (`tests/test_layers.py`).

Build it when a daemon needs latency below its interval. No reference daemon does today.

### 4. Unix-domain sockets for IPC

**Adopted as execution-profile guidance, not a contract rule (PD-90).**

- **It belongs to the execution profile, not the contract.** Under the transport contract, the IPC mechanism is a provider and execution-profile choice (LTC-02 forbids a contract that requires one primitive, and PD-82 binds the pattern to the LTC at its edges). "Use UDS" therefore goes in the execution profile, not in the LTC or the daemon contract.
- **Pathname sockets only.** Put them in a protected directory and check the peer with `SO_PEERCRED`, as Package B's socket does (BG-6). Avoid abstract-namespace sockets: they have no file permissions. Where they cannot be avoided, Landlock ABI 6 and later can scope them.
- **POSIX permissions cannot separate plugins inside one process.** "A WASM plugin has write access to the metadata ledger via its socket but is physically denied the reconciliation ledger" holds only if each plugin runs as its own process with its own uid. Plugins hosted inside one runtime process share its uid and its open sockets. There the separation has to come from the WASM host's capability handles (WASI), not from file permissions (reasoned).
- **The latency claim is overstated.** UDS avoids the TCP/IP stack and is faster for small messages, but for bulk ingestion the difference is modest. The strong argument for UDS is security: no network exposure, kernel-checked peer identity, and Landlock can deny TCP entirely (DB-17 shows it does for daemons today).

### 5. What changes in the daemon pattern's code map

| Recommendation | Where it would land | When |
| --- | --- | --- |
| Hash-chain rules shared by every Spark ledger (PD-88) | `canonical.py` stays the reference encoding; the planned independent verifier (DB-22) is written to verify both this JSONL format and a SQLite table built to PD-88 | With DB-22 |
| A read-only SQLite capability for daemons (PD-91) | `context.py` gains `ctx.sqlite_rows(db, sql, params, max_rows)`: `SELECT` only, `mode=ro` URI, `PRAGMA query_only`, a progress-handler time limit, a row cap that raises `TooLarge` (PD-83), and bounded read transactions (a long read blocks checkpoints and grows the WAL). `manifest.py` declares the database paths. `landlock.py` needs read access to the database and its `-wal` and `-shm` | Deferred until the kernel's ledger exists and point 3 is solved |
| A reference observer for a SQLite chain | A fifth reference daemon, `sqlite-ledger-watch`, class 1a: it verifies, records the head and anchors it, and escalates on corruption. It never halts anything itself | With PD-91 |
| Wake hints (PD-89) | `runtime.py`: a signal ends the sleep early; `unitgen.py`: an optional path unit and oneshot; `manifest.py`: `trigger.wake_on` paths, with the interval required as the floor | When a daemon needs it |
| This pattern's own ledgers | **No change.** They stay JSONL with one fsync per record (PD-02, PD-56). Two formats are acceptable if one independent verifier checks both | — |

**New seams**, which join the reviewer-focus list:
- **N-15:** the observer, the WAL files and Landlock (point 3).
- **N-16:** the ledger service and the plugin sockets (single writer).
- **N-17:** the wake hint and the poll floor, where an event must never replace the liveness cycle.
- **N-18:** a head anchor that the kernel cannot rewrite.

### 6. Decisions

**PD-87. Observers verify and propose; they never govern.** The reconciliation ledger's writer is the kernel or a gate. The observer's corruption finding is an escalation the kernel acts on.
Verdict: **ADOPTED** (design rule for the kernel's memory store).

**PD-88. One set of hash-chain rules for every Spark ledger, SQLite included** (§2): canonical encoding of the whole record, stored bytes verified as stored, a `STRICT` schema, abort triggers, `synchronous=FULL`, sequence-ordered, head anchored off the writer's reach.
Verdict: **ADOPTED.**

**PD-89. Event wake is a hint; the interval is the floor** (§3).
Verdict: **ADOPTED WITH MODIFICATION** (hint via signal, not path-unit activation of a resident daemon). Build when needed.

**PD-90. UDS in execution profiles, with peer credentials; per-plugin separation needs per-plugin processes or capability handles** (§4).
Verdict: **ADOPTED as profile guidance.**

**PD-91. A read-only SQLite capability and a reference observer.**
Verdict: **DEFERRED.** Triggers: the kernel's ledger exists, and point 3 is solved and probed under Landlock on the DGX's SQLite version.

**PD-92. The three-ledger split, by writer authority, with each writer named** (§2).
Verdict: **ADOPTED WITH MODIFICATION.**
