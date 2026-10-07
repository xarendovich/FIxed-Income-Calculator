# Cut 1: full R-2 and E-2. Test and battery accounting

Baseline `a3599fd`: 267 self-tests (`tests-baseline-a3599fd.txt`). After cut 1: 245 (`tests-cut1.txt`). 29 IDs left, 7 arrived: 267 − 29 + 7 = 245. Every change is listed below. Reproduce the list with `python3 -B evidence/v5/list_tests.py | diff evidence/v5/tests-baseline-a3599fd.txt -`.

## Retired with the code they tested (22)

| Test ID | Why |
| --- | --- |
| `test_proc.ProcTests.*` (7: `test_an_exception_mid_wait_still_kills_the_group`, `test_git_ignores_user_configuration`, `test_index_copy_leaves_the_real_index_untouched`, `test_inherited_git_selection_variables_cannot_redirect_git`, `test_only_allowlisted_commands_run`, `test_output_is_bounded`, `test_timeout_kills_the_group`) | `proc.py` is deleted (R-2). HF-40's mechanism, two deadlines meeting, no longer exists (E-2) |
| `test_hardening.GitHardeningTests.*` (3: `test_allowed`, `test_refusals`, `test_diff_subcommands_get_no_ext_diff_and_no_textconv`) | `ctx.git` is deleted. HF-01, HF-19 |
| `test_hardening.GitHistoryIntegrityTests.*` (3: `test_grafts_do_not_rewrite_ancestry`, `test_replace_refs_do_not_forge_history`, `test_repository_gpg_program_never_runs`) | `ctx.git` is deleted. HF-24 |
| `test_hardening.GitInjectionRuntimeTests.*` (3: `test_alias_injection_through_ctx_git_is_refused`, `test_git_through_ctx_run_is_refused`, `test_read_only_git_still_works`) | `ctx.run` and `ctx.git` are deleted. HF-01, HF-02 |
| `test_blind.GitWatchUnsettledTests.*` (4) | `git-watch` left the reference set. HF-29 |
| `test_blind.NoRestartTests.test_git_watch_worst_case_sense_fits_its_cycle_budget` | `git-watch` left the reference set. HF-30 |
| `test_budget.CycleDeadlineTests.test_a_command_gets_only_the_time_left` | No command to give the time left to (E-2). The three other deadline tests (busy loop, blocking read, swallowed alarm) stay |
| `test_landlock.EnforcementTests.test_a_declared_command_runs_and_nothing_else_does` | No command can be declared. Execute is granted nowhere, which `test_nothing_the_daemon_can_write_or_merely_reads_can_be_executed` checks |

## Replaced or renamed (7 left, 7 arrived)

| Left | Arrived | Note |
| --- | --- | --- |
| `test_manifest.ManifestTests.test_forbidden_commands` | `test_manifest.ManifestTests.test_commands_refused` | Any `commands` key is refused with its migration |
| `test_hardening.ManifestHardeningTests.test_interpreter_families_and_command_runners_are_refused`, `test_harmless_commands_still_allowed` | `test_hardening.ManifestHardeningTests.test_a_commands_list_is_refused_with_its_migration` | HF-11 |
| `test_hardening.RuntimeHardeningTests.test_undeclared_command_request_fails_closed_even_when_swallowed` | `test_hardening.RuntimeHardeningTests.test_ctx_offers_no_way_to_run_a_program` | HF-10: nothing to request, and asking starts no process |
| `test_hardening.CpuAccountingTests.test_cpu_includes_commands_the_daemon_ran` | `test_hardening.CpuAccountingTests.test_cpu_includes_any_child_process` | HF-18 kept: a regression that spawned a child would still show in DB-14 |
| `test_landlock.EnforcementTests.test_the_loader_residual_is_recorded_not_hidden` | `test_landlock.EnforcementTests.test_the_loader_residual_is_closed` | The r4.11 R-2b residual: with execute granted nowhere, the ELF loader is refused like any other program |
| (none) | `test_budget.MigrationTests.test_a_contract_4_manifest_is_told_commands_are_gone`, `test_commands_are_refused_under_the_current_schema_too` | Schema 3 → 4 migration |

## Changed in place (same ID)

- `test_runtime.RuntimeTests.test_watchdog_pings_stop_while_an_observation_hangs`: the hang fixture blocks on a FIFO with no writer until a 3-second cycle alarm. Before, it ran `sleep 3`. The assertion is unchanged: no ping for more than 2.5 s.
- `test_budget.MigrationTests.test_a_contract_3_manifest_is_told_exactly_what_changed`: the schema line now names contract 5.0.0. The four field lines still name 4.0.0, the contract that removed them.
- `test_handoff.PublishedContractTests.test_contract_version_is_pinned_to_its_sha`: accepts an unrecorded `-dev` version whose digest matches no published contract (ADJUDICATION-V5.md §3).
- `test_handoff.PublishedContractTests.test_contract_names_every_ctx_method_and_battery_check`: `run` and `git` are gone from the expected methods.
- `test_landlock` helpers: `_exec` closes the child's stdout and stderr. It no longer opens `/dev/null`, whose write grant served only commands and was removed with them, along with the audit hook's `/dev/null` exemption.

## Battery

Reference set: four daemons at `a3599fd`, three now. `git-watch` is retired (R-2) and remains in history at `a3599fd`. The check IDs and their meanings are unchanged: 22 checks per daemon, DB-19, DB-21 and DB-23 reserved. 4 × 22 → 3 × 22. The reduction is the retired daemon only. On each of the three: 21 PASS and DB-24 N/A (no candidate envelope given), exactly as at `a3599fd`. DB-22, the two-verifier cross-check, agrees on every daemon. Runs: `cut1/selftests.txt` (245 of 245, none skipped) and `cut1/battery-*.txt`.
