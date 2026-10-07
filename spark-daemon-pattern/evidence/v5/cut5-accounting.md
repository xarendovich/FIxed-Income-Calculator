# Cut 5: six invariants, fewer commands, R-5c deferred, contract 5.0.0. Accounting

After cut 4: 255 self-tests (`tests-cut4.txt`). After cut 5: 261 (`tests-cut5.txt`). 1 ID left and 7 arrived: 255 − 1 + 7 = 261. Reproduce the list with `python3 -B evidence/v5/list_tests.py | diff evidence/v5/tests-cut4.txt -`.

## What changed

- **E-8, six invariants.** `contract.INVARIANTS` (published as `contract.invariants`; table in `DAEMON-CONTRACT.md` §5a). Each names where it is enforced, the battery checks that hold it and the self-tests that hold it, and `tests/test_invariants.py` fails if any of them stops existing. Together they absorb owner invariants 1 to 7 and INV-1 to INV-9 exactly once; the owner's eighth is the preamble. One correction to r4.11's E-8 table: owner 6 (`cycle_budget_seconds` constrains the whole cycle) belongs to I-6 (bounded), not I-3 (blindness).
- **E-9, fewer commands.** Caller audit: `verify` was called by this repository's README, by DB-16 and by one self-test; the pilot's README names it for its own 3.1.0 copy. `verify` is now `status --verify-only`. It prints `verify`'s line and `RESULT:` with the same exit codes, from the independent verifier cross-checked against the primary one (a disagreement is `VERIFIERS_DISAGREE`, FAIL). `scaffold` and `envelope` moved to the authoring tool, `bin/spark-daemon-author` (`spark_daemon/author.py`). For one release the runtime CLI says where each went (exit 2).
- **R-5c recorded as deferred**, with its trigger (README, Known limits; `ADJUDICATION-V5.md`).
- **Contract 5.0.0**, recorded in `contract/versions.json`: `2d080b407b6c82b230563670746f4a615b2c96660ec68f4509574562c282001d`. Skeleton version 0.5.0.

## Check IDs: no meaning changed (stop condition checked)

- **DB-16** ("read-only verification") runs the reader command twice and checks that the two runs agree and that the ledger's bytes and mtime are unchanged. The reader command is now `status --verify-only`, which prints the same line; the check and its evidence format are the same.

## Renamed or new

| Left | Arrived | Note |
| --- | --- | --- |
| `test_hardening.CliRobustnessTests.test_verify_with_a_bad_manifest_reports_instead_of_crashing` | `test_hardening.CliRobustnessTests.test_verify_only_with_a_bad_manifest_reports_instead_of_crashing` | Same behaviour, new command |
| (none) | `test_hardening.CliRobustnessTests.test_verify_only_prints_what_verify_printed`, `test_moved_commands_say_where_they_went` | |
| (none) | `test_invariants.InvariantTests.*` (3) | |
| (none) | `test_misc.BatteryWorkdirTests.test_a_long_workdir_does_not_crash_the_notify_check` | HF-42, found by the clean-checkout rerun |

## Changed in place

- `test_handoff.EnvelopeTests.test_envelope_cli_round_trip` uses `spark-daemon-author envelope`, and checks that `spark-daemon envelope` says where it went.
- The scaffold's envelope names its producer `spark-daemon-author scaffold`.

## Battery

Three reference daemons × 22 checks, IDs unchanged. Runs: `cut5/battery-*.txt`; self-tests: `cut5/selftests.txt`. The final rerun from a clean checkout is in `clean-checkout/`.
