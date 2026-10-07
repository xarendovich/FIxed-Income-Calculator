# Cut 2: profiles, one facts-only report, the unit as a projection, the gates. Accounting

After cut 1: 245 self-tests (`tests-cut1.txt`). After cut 2: 248 (`tests-cut2.txt`). 19 IDs left and 22 arrived: 245 − 19 + 22 = 248. Every change is listed below. Reproduce the list with `python3 -B evidence/v5/list_tests.py | diff evidence/v5/tests-cut1.txt -`.

## What changed

- **Profiles nest in the order they run:** precheck (DB-01, DB-02, DB-24, DB-25) ⊂ validate (+ DB-03, DB-04) ⊂ battery (every check). Before, the first two names were the other way round. The CLI says so on stderr for one release (`judge.RENAMED`). The check sets are unchanged; only the names moved.
- **One report shape**, `spark-daemon-report/1`, for all three profiles. It replaces `spark-daemon-validate/2`, `spark-daemon-precheck/2`, `spark-daemon-battery/2` and `spark-daemon-qualification/1`. It holds facts only. `result`, `qualifying`, `valid`, `error_count`, `warning_count` and `activation_evidence` are gone; `judge.conclusions(report)` derives the verdict, whether the report qualifies, and why not (E-10).
- **The unit is a projection.** The report binds every input of the unit generator: the manifest as written (the object), its path and digest, `daemon.py`'s digest, the contract digest, the skeleton version, the pattern root, the interpreter, the unit options and `SPARK_DAEMON_HOME`. It also binds the generated unit's digest. `unit --report R` reproduces the unit from the report alone, and refuses if the generator no longer reproduces it (E-11).
- **The gates without duplicated predicates.** Installing (`unit --report`) checks that the report qualifies and was made on this host. The runtime checks at every start that the manifest, `daemon.py` and contract digests are those the unit's `ExecStart` carries (unchanged). `battery --emit-unit`, the qualification record and the `qualified` command are removed.
- **HF-41 fixed** (`HARDENING.md`): the emitted unit had named the battery's disposable workspace as the daemon's home.

## Check IDs: no meaning changed (stop condition checked)

- **DB-03** still runs 2 cycles in the short-run profile and 5 in the battery. The short-run profile is now named validate.
- **DB-15** ("unit hardening") now scores the unit DB-25 generated before the workspace existed, which is the unit the report binds and `unit --report` reproduces (`test_the_projection_is_the_unit_the_battery_scored`). Before, it scored a unit generated from the workspace copy under the workspace's home. Since r4.11 its documented meaning has been "judge the exact unit that is emitted", which the old code did not do (HF-41). The check, its threshold and its evidence format are the same. On the three reference daemons the exposure score is unchanged (`cut2/battery-*.txt` against `cut1/battery-*.txt`).
- **DB-25** now generates the unit with the expected digests in `ExecStart` (the installable unit) when `daemon.py` can be identified. The directive and timing rules are the same.

## Renamed or rewritten (same subject, new ID)

| Left | Arrived | Note |
| --- | --- | --- |
| `test_handoff.ValidateJsonTests.*` (3) | `test_handoff.PrecheckJsonTests.*` (3, same method names) | The static profile is now `precheck`; the report is `spark-daemon-report/1` |
| `test_handoff.PrecheckTests.test_every_reference_daemon_prechecks_ok` | `test_handoff.ValidateRunTests.test_every_reference_daemon_validates` | The short run is now `validate` |
| `test_handoff.PrecheckTests.test_impure_daemon_fails_fast_and_skips_the_run`, `test_policy_violation_is_caught_by_the_short_run` | Same method names under `ValidateRunTests` | |
| `test_handoff.PrecheckTests.test_precheck_is_never_qualifying` | `test_handoff.ValidateRunTests.test_validate_is_never_qualifying` | Derived by `conclusions`, not read from a stored field |
| `test_handoff.PrecheckTests.test_an_absolute_output_dir_prechecks_like_the_battery` | `test_handoff.ValidateRunTests.test_an_absolute_output_dir_validates_like_the_battery` | HF-37 |
| `test_judge.RegistryTests.test_validate_starts_no_process_and_precheck_needs_no_extra_tool` | `test_judge.RegistryTests.test_precheck_starts_no_process_and_validate_needs_no_extra_tool` | |
| `test_judge.RegistryTests.test_only_the_full_battery_qualifies` | `test_qualify.ConclusionTests.test_only_a_complete_full_battery_pass_qualifies` | Moved: qualifying is now derived from a report, not a property of a judgement |
| `test_qualify.EmitRefusalTests.test_only_a_qualifying_pass_gets_past_the_first_checks` | `test_qualify.ProjectedUnitTests.test_a_report_that_does_not_qualify_is_refused` | |
| `test_qualify.EmitRefusalTests.test_a_failed_battery_emits_nothing` | `test_qualify.ProjectedUnitTests.test_a_failed_battery_is_not_installable` | |
| `test_qualify.EmitRefusalTests.test_quick_cannot_emit` | covered by `test_a_report_that_does_not_qualify_is_refused` (quick) and `ConclusionTests` | `--emit-unit` no longer exists |
| `test_qualify.QualifiedUnitTests.test_a_pass_emits_exactly_the_unit_and_its_record` | `test_qualify.ProjectedUnitTests.test_a_qualifying_report_projects_its_unit` | No record any more |
| `test_qualify.QualifiedUnitTests.test_the_unit_binds_manifest_code_and_contract` | `test_qualify.ProjectedUnitTests.test_the_unit_binds_manifest_code_contract_and_inputs` | |
| `test_qualify.QualifiedUnitTests.test_a_record_from_another_host_fails` | covered by `test_a_report_that_does_not_qualify_is_refused` (other host) | |
| `test_qualify.QualifiedUnitTests.test_changing_a_unit_option_invalidates_it` | `test_qualify.ProjectedUnitTests.test_an_edited_unit_input_does_not_reproduce`, `test_unit_inputs_cannot_be_changed_at_install_time` | |
| `test_qualify.QualifiedUnitTests.test_changed_code_fails_the_gate` | `test_qualify.ProjectedUnitTests.test_changed_code_is_the_runtimes_refusal_not_the_installers` | The predicate now lives in one gate only, the runtime |
| `test_qualify.QualifiedUnitTests.test_a_preview_cannot_pass_the_gate` | retired | There is no unit-bytes gate to pass. A preview carries no digests, and the runtime refuses it outside test mode (`RuntimeGateTests.test_an_unqualified_start_is_refused_outside_test_mode`) |

## New

- `test_judge.RegistryTests.test_the_renamed_profiles_say_so`
- `test_qualify.ConclusionTests.test_no_conclusion_is_stored`, `test_a_stored_result_field_changes_nothing`
- `test_qualify.ProjectedUnitTests.test_the_projection_is_the_unit_the_battery_scored`, `test_the_unit_names_the_authors_home_not_the_workspace` (HF-41)

## Battery

Three reference daemons × 22 checks, IDs unchanged. Runs: `cut2/battery-*.txt`; self-tests: `cut2/selftests.txt`.
