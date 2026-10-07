# Cut 4: one canonical PathPolicy. Accounting

After cut 3: 248 self-tests (`tests-cut3.txt`). After cut 4: 255 (`tests-cut4.txt`). Nothing left, 7 arrived (`tests/test_pathpolicy.py`). Reproduce the list with `python3 -B evidence/v5/list_tests.py | diff evidence/v5/tests-cut3.txt -`.

## What changed

- `spark_daemon/pathpolicy.py`: `PathPolicy`, one frozen value per manifest, expanded once against an explicit home: `output_dir`, `reads` and `deny`. Each enforcement layer is a projection of it:
  - **Python** (`guard.Policy`, ctx and the audit hook): `PathPolicy.denied`, `may_read`, `may_write`. `guard.Policy` now holds a `PathPolicy` (`paths`) plus the notify target, and the audit hook captures the same frozen value.
  - **Kernel** (`landlock.py`): `pathpolicy.grants(policy, extra_reads)`. Landlock keeps only its access masks and existence checks; `landlock.gaps` is `pathpolicy.gaps`.
  - **Unit** (`unitgen.py`): `pathpolicy.unit_paths(PathPolicy.of(m, home=...))` gives `ReadWritePaths`, `ReadOnlyPaths` and `InaccessiblePaths`.
- `pathpolicy.agreement(policy, unit_text, extra_reads)` reads the generated unit back and checks it against the Python and kernel projections. It covers what is writable, what is readable, what is inaccessible, no grant inside a denied path, no gap, and `denied()` on every denied path. It is run on every shipped manifest: 3 reference daemons and 8 fixtures.
- **Unchanged on purpose:** the enforcement stays diverse; only the policy is single. A path refused by more than one layer has the same outcome as before (a Python refusal is a policy violation, exit 78), pending the owner's ruling on E-4. The contract did not change.
- `guard.count_violation` is removed: its only caller was the command allowlist, removed in cut 1.

## One finding while writing the agreement test

A first version checked the Python projection only through `may_read` and `may_write`. Since the no-gaps rule (R-1) keeps every read and the output directory clear of denied paths, a `denied()` that returned False for everything passed that version (`test_a_drifted_python_check_is_caught` failed first). The audit hook relies on `denied()` for opens anywhere, not only under a read, so the agreement now checks `denied()` on every denied path. No shipped code had the defect; the test would have missed it.

## Check IDs

No check changed. The evidence lines of all 22 checks on the three reference daemons are compared with cut 3 in `cut4/battery-*.txt`.

## Battery

Three reference daemons × 22 checks, IDs unchanged. Runs: `cut4/battery-*.txt`; self-tests: `cut4/selftests.txt`.
