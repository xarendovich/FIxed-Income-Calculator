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
