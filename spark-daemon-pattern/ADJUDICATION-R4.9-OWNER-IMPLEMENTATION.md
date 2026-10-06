# Implementation record: the owner's r4.9 adjudication (r4.11)

- **Implements:** `ADJUDICATION-R4.9-OWNER.md`, the owner-approved decisions on R-1 to R-8, within the bounded package that handoff authorizes. No daemon class is unlocked, nothing is activated, and Break Glass and LTC authority are unchanged.
- **Revision:** r4.11, 2026-10-06, by Claude.
- **Candidate (code):** `55bb8b4e654737ea3a23f7d234d7daa0b1113924` on branch `claude/clever-bardeen-qqkxez`. This record and the evidence files were committed on top of it and change no code.
- **Evidence at the candidate:**
  - 266 of 266 self-tests pass, none skipped (Python 3.11.15, `jsonschema` 4.26; `evidence/r4.11/selftests.txt`).
  - The four reference daemons pass all 22 battery checks (`evidence/r4.11/battery-examples.txt`).
  - `git diff --check` is clean.
  - A sample qualified unit, with its record and the installer gate's verdict, is in `evidence/r4.11/qualified-sample/`.
- **Contract:** 4.0.0 (`dfdb9f1b484d842d4e4f5d1684917371a7106b0bd68ebd06d388ab88f49b8ea6`). Manifest schema 3; validate, precheck and battery report schemas 2.

## 1. The rulings, as built

| ID | Ruling | Built as | Where | Acceptance evidence (tests) |
| --- | --- | --- | --- | --- |
| **R-7** | APPROVE | One check registry, one state vocabulary, one verdict rule, one exit-code mapping. `validate`, `precheck` and `battery` are profiles; `handoff.py` and `battery.main` only shape reports. Precheck's PC-01..04 are gone. New IDs, no meaning edited: DB-24 (candidate envelope) and DB-25 (unit directives and derived timings) | `spark_daemon/judge.py` | `tests/test_judge.py`: one invariant, same ID and reason in every profile; a mutation of shared judge logic caught by all three profiles; verdicts come only from the judge |
| **R-3** | APPROVE WITH GUARD | `battery --emit-unit DIR` writes a unit and its qualification record on a qualifying PASS only (full battery, not `--quick`). Unit options are battery inputs. `spark-daemon qualified` is the installer's gate. The runtime refuses a digest mismatch, and outside test mode a start that carries no digests. `unit` is a marked preview | `spark_daemon/qualify.py`, `unitgen.py`, `runtime.py`, `cli.py` | `tests/test_qualify.py`: failed, incomplete and `--quick` batteries emit nothing; a preview fails the gate; a changed option invalidates the unit; changed code fails the gate and the runtime; a record from another host fails; nothing installs or enables |
| **R-1** | APPROVE WITH MODIFICATION | `deny` removed from schema 3 outright. A schema-3 manifest that has it is refused with the reason. A schema-2 manifest is told field by field what changed. No read may contain an always-denied path (no gaps) | `spark_daemon/manifest.py` | `tests/test_hardening.py` (`test_a_deny_list_is_refused_with_its_migration`, `test_no_read_may_contain_an_always_denied_path`); `tests/test_budget.py::MigrationTests` |
| **R-2b** | APPROVE | Execute only on each declared executable and its ELF loader. A daemon with no command gets execute nowhere. In-process, `proc` and the audit hook allow exactly the declared executables' resolved paths | `spark_daemon/landlock.py` (`execute_paths`, `elf_interpreter`) | `tests/test_landlock.py`: a command-free domain runs nothing, system tools included; native modules still import; a declared command runs and an undeclared one is refused. **Residual in §2.1** |
| **R-6** | APPROVE | `cycle_budget_seconds` is the one declared cycle time. Each cycle has one monotonic deadline: `ctx` calls get the time left, and an alarm interrupts sense and decide at the deadline. The ledger write is never interrupted, and a caught alarm does not save the cycle. Watchdog, stop and start timeouts are derived in one place, and DB-25 fails a unit that contradicts them | `manifest.py`, `context.py`, `runtime.py`, `unitgen.py`, `judge.py` | `tests/test_budget.py`: a pure-Python loop, a blocking FIFO read, a slow command and a swallowed alarm each fail within the deadline; overruns count as blind time; unit timings cannot contradict the budget |
| **R-4** | APPROVE WITH SPLIT | One pure interpreter (`semantics.py`), shared by start-up and the new `status` command. An independent verifier (`verifier/ledger_verify.py`) imports nothing from the package, encodes RFC 8785 by hand and reports facts only. `status` interprets only a chain it verified intact. DB-22 runs both verifiers | `spark_daemon/semantics.py`, `status.py`, `verifier/`, `battery.py` (`db22`) | `tests/test_independent.py`: start-up and status agree on the same facts; each verifier's mutation is caught by the other; a hash-invalid chain never reaches the interpreter; a copy of the package whose interpreter forgets inherited time fails the HF-32 restart-loop behaviour |
| **R-5** | REJECT | Not built | — | — |
| **R-5b** | DO NOT FREEZE | Not built; no threshold decides integrity | — | — |
| **R-5c** | APPROVE AS TARGET | **Gated; not built** (§2.2). Full start-up verification remains normative | — | — |
| **R-8b** | APPROVE WITH MODIFICATION | A deterministic rename map, and a copy bound by the source-tree, map and transformed-tree hashes (`VENDORED.json`); `--check` proves a vendored tree is exactly that copy | `vendoring/vendor.py`, `vendoring/example-rename-map.json` | `tests/test_vendor.py`: deterministic; one source byte or one map change moves the right hashes; a copy renamed by the map checks out and runs under its new names with the same contract version |
| **R-2, R-8** | DEFER | Not built | — | — |

## 2. For the owner

### 2.1 R-2b: one residual

**Evidence:** `evidence/r4.11/exec_grant_probe.txt`; pinned by `tests/test_landlock.py::test_the_loader_residual_is_recorded_not_hidden`.

Running a dynamically linked program needs execute on its ELF loader as well as on the program, because the kernel opens the loader for execute. But the loader can also be run as a program in its own right, on any binary the daemon may read. Under a domain that granted execute only on `git` and its loader, running the loader on `/usr/bin/true` ran it.

**What holds today:**
- **A daemon with no command:** the ruling holds completely, at the kernel. That is every reference daemon but `git-watch`, and the pilot's daemon.
- **A command-using daemon, at the Python level:** the ruling holds; `proc` and the audit hook allow only the declared executables.
- **A command-using daemon, at the kernel:** code that had escaped Python could use the loader to run any readable binary.

**Ways to close it, for decision:**
1. **Commands run from outside the confined process.** This is the supervisor/worker split (PD-72): the worker asks, and a broker that validates the arguments executes. It needs PD-72.
2. **R-2 in full:** no programs in the core.
3. **Narrower read grants on `/usr`.** This reduces the binaries reachable through the loader but does not close the route, because `/usr/lib` holds executables too. Not recommended as a fix.

Until one of these is chosen, `git-watch` is the only reference daemon the residual applies to.

### 2.2 R-5c: no trustworthy checkpoint anchor exists, so the phase stops here

As the handoff directs, the coding pass looked first for an existing authority that could bind a checkpoint outside the unverified suffix. It found none:

| Candidate | Why it is not an anchor |
| --- | --- |
| A file beside the ledger | The daemon can write it. It would be a performance cache, not a trust anchor (handoff §4) |
| The qualified unit or its record (R-3) | Written once at qualification and static; a checkpoint must move as the ledger grows |
| Off-host anchoring of chain heads (PD-58) | Not built |
| An independent verifier running under its own identity, writing where the daemon cannot | Does not exist. Creating it (the identity, a root-owned location, a timer, installation) is a new authority subsystem |

**Result:** start-up still verifies the whole chain, and `status` always verifies in full, so nothing is ever reported as deferred.

**For context (r4.9):** verification runs at about 28,000 records a second on the build workspace. Start-up reaches the 60 s timeout after decades for pressure-watch's record rate, but after months for a daemon writing every 5 seconds. The natural next step is the fourth candidate, but it is a decision for the owner.

### 2.3 Profile nesting: a wording to confirm

The handoff says "`precheck` stays fast, offline, and dependency-free. `validate` is a larger deterministic profile."

The implementation keeps the existing public meanings:
- `validate` is static and runs in milliseconds;
- `precheck` adds a short confined run and the ledger check, so validate ⊂ precheck ⊂ battery.

Both stated properties hold: precheck needs no tool beyond Python and no network, and validate is deterministic. Making validate the larger profile would swap what two published commands do. **Confirm the nesting, or rule that they swap.**

### 2.4 Changes a reviewer should know about

- **Phase order.** Phase D (the independent verifier and DB-22) was built before Phase C. DB-22 is a new battery check, and adding it after 4.0.0 would have needed a second major version. As built, every breaking change is in the one 4.0.0 cut.
- **precheck says PASS.** With one result vocabulary, precheck's "OK" became "PASS", reported with `qualifying: false`. The old safeguard, that precheck never says PASS, is now enforced mechanically: only a qualifying battery PASS emits a unit, and the runtime refuses any other start.
- **HF-38 (Low), fixed.** Found while writing the independent verifier: a first record whose `seq` was the JSON boolean `true` verified as seq 1 (`True == 1` in Python). The first draft of the independent verifier had the same blind spot. Both now require an int, and DB-22 checks it on every daemon.
- **HF-39 (Low), fixed.** Found while building R-3: the battery report bound its workspace copy of the manifest. For a daemon with an absolute `output_dir` (the pilot's), that copy is rewritten, so the report named a digest the installed manifest never has, and a qualified unit would never have started. Reports now bind the manifest as written and name the workspace copy separately. `tests/test_judge.py::NoStandaloneJudgeTests::test_reports_bind_the_manifest_as_written`.
- **Run checks are SKIPPED once a static check fails.** The battery used to run them and report FAIL. The verdict is the same, and the battery is faster.
- **Battery IDs.** DB-19, DB-21 and DB-23 stay reserved. DB-22, DB-24 and DB-25 are new.

### 2.5 What the pilot project must change to adopt 4.0.0 (its decision, in its repository)

1. **Manifest:** schema 3. Delete `deny`, `watchdog_seconds` and `step_timeout_seconds`; add `cycle_budget_seconds`. The validator explains each change.
2. **Installer:** run `battery ... --emit-unit DIR` with the unit options, check the result with `qualified`, and install the emitted unit. `unit` now produces a preview, which the runtime refuses to start outside test mode.
3. **Tests:** its precheck test expects "PASS", not "OK".
4. **Notifier:** read daemons through `status` or a copy of `verifier/ledger_verify.py` instead of parsing the ledger (N-19); this also removes its byte-offset dependency.
5. **Copy:** re-vendor with `vendoring/vendor.py` and the project's own rename map, and keep the `VENDORED.json` binding.

## 3. The handoff's invariants, as preserved

| # | Invariant | How it holds |
| --- | --- | --- |
| 1 | A battery PASS is qualification evidence, not permission | `qualify.py` installs and enables nothing; the record and the gate say so; activation is unchanged |
| 2 | No class above the authorized one is unlocked | `daemon_class` still admits `observe` only |
| 3 | A command declaration is an allowlist, not a Boolean flag | Execute only on the declared executables and their loaders; exact in-process. Residual in §2.1 |
| 4 | Start-up and `status` share interpretation; verification stays diverse | `semantics.py` is shared; `ledger.py` and `verifier/ledger_verify.py` are independent, and DB-22 compares them |
| 5 | Integrity uncertainty is visible | Nothing is deferred: full verification at start-up and in `status`. A broken chain is `CORRUPT` and never interpreted |
| 6 | `cycle_budget_seconds` constrains the whole cycle | Deadline set at cycle entry; the alarm covers sense and decide, and the digest gets what is left |
| 7 | Activation remains a separate authority decision | Unchanged |
| 8 | Nothing relaxes confinement, blindness, authentication or proposal-versus-authority | Confinement tightened (execute removed from command-free daemons; no gaps); blindness rules unchanged and now shared |

## 4. Freeze and qualification gate (handoff §5)

| Gate item | Status |
| --- | --- |
| Contract and schemas at 4.0.0 where the breaking semantics need it | Done: contract 4.0.0, manifest schema 3, report schemas 2, `contract/versions.json` |
| All earlier tests green, or migrated with stable replacement IDs | Done: 266 of 266. Precheck's PC-IDs were replaced by registry IDs; no DB-ID changed meaning |
| New focused tests pass | Done (§1) |
| Mutation and adversarial tests for the command boundary, unified judge, cycle deadline, shared interpreter and checkpoint continuity | Done for the first four. Checkpoint continuity: **not applicable**, since R-5c is gated (§2.2) |
| An installable unit comes only from a complete battery PASS | Done (R-3 tests) |
| A battery PASS still cannot activate | Done |
| No class unlock | Done |
| `git diff --check` | Clean |
| Exact candidate SHA and test evidence recorded | This record; `evidence/r4.11/` |

**The package is closed except for two items for the owner:** the R-2b residual (§2.1) and the R-5c anchor (§2.2). Neither blocks the freeze of what is built.
