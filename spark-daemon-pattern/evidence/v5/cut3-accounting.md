# Cut 3: no manifest copy, and an explicit harness in place of test mode. Accounting

After cut 2: 248 self-tests (`tests-cut2.txt`). After cut 3: 248 (`tests-cut3.txt`). 8 IDs left and 8 arrived. Every change is listed below. Reproduce the list with `python3 -B evidence/v5/list_tests.py | diff evidence/v5/tests-cut2.txt -`.

## What changed

- **No test mode (E-6, E-12).** `run` reads no test switch from the environment. `SPARK_DAEMON_TEST`, `SPARK_DAEMON_TEST_INTERVAL_MS`, `..._BLIND_LIMIT_MS`, `..._CYCLE_BUDGET_MS` and `SPARK_DAEMON_AUDIT` are gone. A separate entry, `spark-daemon harness`, takes explicit parameters: `--interval-ms`, `--blind-limit-ms`, `--cycle-budget-ms`, `--output-dir`, and `--audit record` (DB-03 and DB-17 only, as adjudicated). Jitter is off under it. Its parameters are recorded in `DAEMON_START.harness`, which replaces `test_overrides`; a production run records `null`. The harness refuses to run inside a systemd service.
- **No unqualified start, and no missing-Landlock tolerance, anywhere.** `run` and `harness` both need the three digests, and both refuse to start without Landlock (PD-15). The battery and the self-tests pass the digests of the files as they are.
- **No manifest copy (E-3).** The battery runs the manifest and `daemon.py` where they are. An absolute `output_dir` is redirected by one recorded harness parameter, an output directory inside the workspace, which must pass the manifest's own placement rules. The report records the harness parameters (`harness`) in place of `workspace_manifest_sha256` and `rewrites`. The battery no longer changes its own process's environment: `guard.Policy` and the unit generator take the home explicitly.
- The contract did not change in this cut: `contract/daemon-contract.json` is identical.

## Check IDs: no meaning changed (stop condition checked)

- **DB-03** still runs the daemon with the audit hook in record-only mode and checks that no file outside the output directory changed. It now snapshots the daemon's own folder, which is where it runs.
- **DB-17**'s probe still reports N/A on a kernel without Landlock. The daemon itself now refuses to start there, so the other run checks fail on such a kernel, where before they passed under test mode. That is the intended consequence of removing the tolerance, not a new meaning for any check.
- **DB-16**, **DB-10**, **DB-13** and **DB-17** probes get the harness output directory when one is recorded.
- The evidence lines of all 22 checks on the three reference daemons are compared with cut 2 in `cut3/battery-*.txt`.

## Renamed or rewritten

| Left | Arrived | Note |
| --- | --- | --- |
| `test_blind.BlindPeriodTests.test_the_override_is_ignored_outside_test_mode` | `test_blind.BlindPeriodTests.test_a_production_run_takes_no_harness_parameter` | `run` rejects a harness option, and the 4.x environment switches do nothing |
| `test_landlock.SupervisorDomainTestModeTests.test_unavailable_landlock_refuses_outside_test_mode` | `test_landlock.SupervisorDomainUnavailableTests.test_unavailable_landlock_always_refuses` | |
| `test_landlock.SupervisorDomainTestModeTests.test_unavailable_landlock_is_non_fatal_in_test_mode` | retired | The tolerance it tested is removed |
| `test_qualify.RuntimeGateTests.test_an_unqualified_start_is_refused_outside_test_mode` | `test_qualify.RuntimeGateTests.test_an_unqualified_start_is_refused_everywhere` | `run` and `harness` |
| `test_qualify.RuntimeGateTests.test_changed_code_is_refused_even_in_test_mode` | `test_qualify.RuntimeGateTests.test_changed_code_is_refused_everywhere` | `run` and `harness` |
| `test_runtime.RuntimeTests.test_jitter_bound_is_zero_in_test_mode` | `test_runtime.RuntimeTests.test_jitter_bound_is_zero_under_the_harness` | |
| `test_runtime.RuntimeTests.test_jitter_disabled_in_test_mode_and_recorded_for_a_real_run` | `test_runtime.RuntimeTests.test_jitter_off_under_the_harness_and_recorded_for_a_real_run` | |
| `test_runtime.RuntimeTests.test_test_mode_refused_under_systemd` | `test_runtime.RuntimeTests.test_the_harness_is_refused_under_systemd` | |

## New

- `test_runtime.RuntimeTests.test_the_harness_output_dir_follows_the_manifests_rules`

## Changed in place (same ID)

- `test_judge.NoStandaloneJudgeTests.test_reports_bind_the_manifest_as_written` asserts that the run checks bind the same manifest, that the harness output directory is recorded, and that nothing is written beside the daemon.
- Tests that set the 4.x environment switches now pass harness parameters through `helpers.Sandbox.argv`. The assertions on `DAEMON_START.test_overrides` read `DAEMON_START.harness`.

## Battery

Three reference daemons × 22 checks, IDs unchanged. Runs: `cut3/battery-*.txt`; self-tests: `cut3/selftests.txt`.
