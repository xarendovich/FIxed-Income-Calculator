# Review package: contract 5.0.0 (r4.12)

- **Status:** FOR REVIEW. The five cuts the owner authorized (`ADJUDICATION-V5.md` §4) are implemented, in order, one commit each. Nothing here activates anything.
- **Source:** GitHub `xarendovich/fixed-income-calculator`, branch `claude/clever-bardeen-qqkxez`, directory `spark-daemon-pattern/`. Baseline `a3599fd` (r4.11); cuts `4f0db7d`, `bf4c0e4`, `9f9cf56`, `1185daa`, and the cut 5 commit that carries this page.
- **Revision:** r4.12, 2026-10-07, by Claude.

## 1. Result in one table

| | r4.11 (`a3599fd`) | Contract 5.0.0 |
| --- | --- | --- |
| Contract | 4.0.0 `dfdb9f1b…` | **5.0.0 `2d080b40…`**, recorded in `contract/versions.json` |
| Seams a reviewer must hold | 11 | **6 boundaries**, 5 once packaging replaces vendoring (§2) |
| Invariants | 8 owner + 9 INV, overlapping | **6**, each enforced in one place and naming its checks and tests (§3) |
| Runtime CLI commands | 15, of which 12 public | **12, of which 9 public** (describe, schema, precheck, validate, battery, unit, run, harness, status) and 3 internal probes; `scaffold` and `envelope` moved to `spark-daemon-author`; `verify` and `qualified` removed |
| Report schemas | validate/2, precheck/2, battery/2, qualification/1 | **one**, `spark-daemon-report/1`, facts only |
| Ways a daemon starts | `run` (+ ambient test mode, unqualified starts, tolerance of missing Landlock) | `run`; `harness` with explicit, recorded parameters. Both need the digests and Landlock |
| Programs a daemon may run | an allowlist, with loader residual | **none**; execute granted nowhere |
| Self-tests | 267 of 267 | **261 of 261**, none skipped; every change explained by ID (`evidence/v5/cut1-accounting.md` to `cut5-accounting.md`) |
| Battery | 4 × 22 (21 PASS, DB-24 N/A) | **3 × 22** (21 PASS, DB-24 N/A); `git-watch` retired with R-2 |

## 2. The six boundaries left

| # | Boundary | Why it stays |
| --- | --- | --- |
| 1 | **Author ↔ judge.** A candidate (manifest, `daemon.py`, envelope) against one registry of checks in three nested profiles, writing one facts-only report | A candidate cannot certify itself |
| 2 | **Judge ↔ installer ↔ runtime.** The unit is a projection of a qualifying report. Installing checks qualification and host; the runtime checks the files' digests at every start. Neither repeats the other's predicate | Qualification is evidence; activation is a person's decision |
| 3 | **Daemon code ↔ the world.** One `PathPolicy`, projected into the Python checks, the Landlock grants and the unit's path directives, with an agreement test on every shipped manifest | Enforcement stays diverse; only the policy is single |
| 4 | **Writer ↔ readers.** One ledger writer, two independent verifiers compared by DB-22, one interpreter for start-up and `status` | Verification diversity found HF-38; interpretation stays single |
| 5 | **Production ↔ harness.** `run` reads no test switch; `harness` takes explicit parameters, records them, and refuses to run under systemd | Tests need short limits; production must not be able to ask for them |
| 6 | **Pattern ↔ a project that copies it.** The rename map and three-hash binding (`vendoring/`) | Until packaging with neutral names (R-8) |

Removed: commands and their timeout plumbing (seams 1 and 2), checkpointed start-up (seam 5, deferred with a trigger), and the manifest copy (seam 8).

## 3. The six invariants

Published in the contract (`spark-daemon describe`, `contract.invariants`) and tabled in `DAEMON-CONTRACT.md` §5a:

- **I-1** Observe only, and the kernel enforces it.
- **I-2** Never record a guess.
- **I-3** Blindness surfaces within the limit.
- **I-4** The ledger is the one source of truth.
- **I-5** It runs only what was judged, and judging is not activation.
- **I-6** Bounded.

The owner's eighth invariant ("nothing relaxes …") is the preamble. `tests/test_invariants.py` fails if any named check or test stops existing.

## 4. What the cuts found

- **HF-41 (Medium), fixed in cut 2.** The unit `battery --emit-unit` wrote named the battery's disposable workspace as the daemon's home, in `Environment=SPARK_DAEMON_HOME=` and `ReadWritePaths=`, and expanded every `~` path there. It was generated after the workspace had changed the battery process's environment. It is visible in the committed r4.11 sample. Reproduction and fix: `evidence/v5/hf41_*`. The pilot was never exposed (it runs 3.1.0, which has no `--emit-unit`). The root cause, an ambient environment change, is gone with cut 3.
- **A blind spot in a first agreement test, cut 4.** Because the no-gaps rule keeps reads clear of denied paths, a Python `denied()` that allowed everything would have passed a check made only through `may_read` and `may_write`. The audit hook relies on `denied()` directly, so the agreement now checks it. No shipped code had the defect.
- **HF-42 (Low), found by the clean-checkout rerun.** DB-11 bound its notify socket inside the workspace, so a long `--workdir` pushed the path past AF_UNIX's 108 bytes, crashed the check and failed the battery for a daemon with nothing wrong. Present since r3; it never showed because every earlier run used a short workdir. Fixed, with a regression test (`evidence/v5/hf42_before.txt`). The final rerun from a clean checkout is in `evidence/v5/clean-checkout/`.
- **A correction to r4.11's E-8 table.** Owner invariant 6 (the cycle budget) belongs to I-6, not I-3.

## 5. Stop conditions: none triggered

| Condition | How it was checked |
| --- | --- |
| Full R-2 requires weakening confinement | Confinement got stricter: execute is granted nowhere, and the `/dev/null` grant and exemption are gone. DB-13, DB-17 and the Landlock tests pass; the loader residual is closed (`test_the_loader_residual_is_closed`) |
| A supposedly command-free daemon depends on a process | Every remaining reference daemon and fixture runs with spawn refused by the audit hook and execute refused by the kernel. The hang fixture now blocks on a FIFO instead of `sleep` |
| Removing the timeout plumbing breaks the whole-cycle budget | The busy-loop, blocking-read and swallowed-alarm deadline tests pass; only the command variant retired |
| Report unification changes a check ID's meaning | Every check's evidence line compared cut to cut on all three reference daemons, apart from timings and hashes. DB-15 and DB-16 are discussed in the cut 2 and cut 5 accountings |
| The two ledger verifiers disagree | DB-22 agrees on every run; `status --verify-only` now fails on any disagreement |

## 6. Migrating a daemon to 5.0.0 (from 3.1.0 or 4.0.0)

1. Set `manifest_schema` to `spark-daemon-manifest/4`, and delete `commands`. A daemon that needed a program is not a generic daemon. From 3.1.0, also delete `deny`, `watchdog_seconds` and `step_timeout_seconds`, and add `cycle_budget_seconds`. The validator lists every one of these, field by field.
2. Replace `ctx.run` and `ctx.git` reads with file reads.
3. `precheck` is now the static checks, and `validate` adds the short run (the names swapped).
4. Run the battery with the unit inputs (`--python`, `--require-path`, `--part-of`), then `unit --report <report> --out DIR`. `--emit-unit`, the qualification record and `qualified` are gone.
5. Change `verify` to `status --verify-only`, and `scaffold` and `envelope` to `spark-daemon-author`.
6. Tests and harnesses: the `SPARK_DAEMON_TEST*` and `SPARK_DAEMON_AUDIT` variables do nothing. Use `spark-daemon harness` with explicit options and the digests.

## 7. Open for the owner

1. **The first DGX run (G-6).** Install `meminfo-watch` under systemd as PID 1 and confirm `DAEMON_START.landlock.status == "enforced"`. Recommended now, at the pilot's 3.1.0, as adjudicated.
2. **E-4:** what a path refused by more than one layer should be. Today it is a policy violation, exit 78.
3. **R-8:** whether the pilot counts as the second real consumer that would justify neutral names and packaging.
4. **Kernels without Landlock.** Since contract 5 the harness refuses to start there, so the run checks fail where they used to pass under test mode. CI and every developer kernel need Landlock (ABI 1 or later).
5. **R-5c trigger:** measure start-up verification on the DGX at the daemon's real record rate.

## 8. Questions for reviewers

1. Is any of the six boundaries still removable without weakening a guarantee? Packaging (boundary 6) is the known candidate.
2. Does any invariant now hold through a single check that a refactor could quietly drop? `test_invariants.py` checks only that the checks and tests exist, not that they still test the invariant.
3. Is `status --verify-only` the right home for `verify`, or should integrity be a field every `status` carries, with no flag?
4. Should `harness` live outside the runtime CLI, like the authoring tool, so a production install cannot even name it?
