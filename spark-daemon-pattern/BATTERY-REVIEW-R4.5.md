# Conformance battery review and test list (r4.5), for secondary review

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

## 1. Code map

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

## 2. The checks, one by one

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

## 3. Gaps, in order of risk

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

## 4. Planned checks

New IDs are never edited in place (PD-78).

| ID | Check | From | When |
| --- | --- | --- | --- |
| **DB-19** | A direct cgroup peak (v2 `memory.peak`, or v1 `memory.max_usage_in_bytes` against a v1 limit) for a fresh cgroup holding the daemon and every child from its first instruction. Replaces DB-14's memory half; DB-14 is retired and its CPU half carried over | PD-76 (owner), `DECISION-BRIEF.md` §2.4 | Next |
| **DB-21** | A budget table: worst-case cycle against the watchdog (at most half), stop timeout against one watchdog period, blind limit against intervals, and retry bounds over time | PD-69, PD-86, U-8 | With Phase 1's budget report |
| **DB-22** | Independent ledger verification: a separate verifier and a second JCS implementation agree with the skeleton's | B-6, U-7 | Phase 2 |
| DB-23 | Worst-case fixtures per observation type: over-capacity input fails the cycle and never records a partial result | PD-71, HF-33, PD-83 | With DB-19 |
| DB-2x | The LTC provider-neutral suite for each declared connection | PD-82, LI-11 | With the first request channel |

## 5. Planned self-tests (skeleton properties)

| ID | Test | From |
| --- | --- | --- |
| T-EX1 | One exit-code table drives the runtime constants, `RestartPreventExitStatus` and the notifier mapping; the test fails if any of them drifts | PD-63 split, N-12 |
| T-EX2 | `EXIT_POLICY` and `EXIT_SENSE_BLIND` differ, and both are never restarted | PD-63 split |
| T-TR1 | `ctx.run`, `ctx.git` and `ctx.list_dir` raise `TooLarge` one byte or one entry past the cap, and succeed exactly at it | PD-83 (modified) |
| T-PC1 | `pacing: BOUNDED_JITTER` stays within its bound; `DETERMINISTIC_SPREAD` gives the same phase on restart; `FIXED_PHASE` injects nothing; watchdog pings continue through any offset | PD-85, N-11 |
| T-ST1 | The staleness checker flags a ledger that stops changing within `T`, using its own monotonic clock, and is unaffected by a wall-clock step | Phase 1 (modified), N-5 |
| T-CG1 | The cgroup helper places the child before `exec`, cleans up on every exit path, and records the cgroup version | DB-19, N-4 |
| T-N1 to T-N14 | One test per seam when its decision is built: supervisor/worker (N-1), register binding (N-2), declared connection and LTC bounds (N-3), cgroup (N-4), staleness (N-5), inbound acknowledgement never carries commands (N-6), gate epoch fencing (N-7), witness independence (N-8), recorder anchoring (N-9), pacing and watchdog (N-11), exit table (N-12), schema-3 migration (N-13), survival mode never read as authority (N-14). N-10 is removed by PD-83's modification | `REVIEW-PACKAGE-COMBINED.md`, reviewer focus |

## 6. Questions for the secondary reviewer

1. Is any check on the wrong side of the battery/self-test line (§ introduction)?
2. DB-20 uses a 1.0 s blind limit over 12 cycles. Is there a legitimate daemon whose first accepted cycle takes longer in the battery workspace, and should the limit scale with the manifest's `step_timeout_seconds`?
3. Is `max(ru_maxrss)` ever the *right* basis once DB-19 exists, for example for a daemon with no children?
4. Should DB-05 add a power-loss simulation (for example a `dm-flakey` or fsync-trace check), or is DB-08 plus code review enough?
5. For G-5: which host facts must a PASS bind before it can be used on another machine?
6. Which of the 210 self-tests below would you delete as redundant, and which property has no test at all?

## 7. The self-test list

Generated from the test files at r4.5 (`evidence/r4.5/testlist.py`). "Cites" lists the defect (HF), decision (PD), battery check (DB) and rule identifiers each test names in its body or docstring.
<!-- generated: 210 tests -->

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

#### `tests/test_handoff.py` (31 tests)

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

#### `tests/test_landlock.py` (12 tests)

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

#### `tests/test_misc.py` (12 tests)

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
