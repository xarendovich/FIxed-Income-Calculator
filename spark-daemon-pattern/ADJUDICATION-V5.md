# Adjudication: the v5 simplification (r4.12)

- **Asked:** the owner asked Claude to review and adjudicate a set of recommendations, made after the r4.11 design review, to reduce the eleven seams to about six intentional boundaries. The owner then authorized the cuts, in a fixed order and with stop conditions (§4).
- **Authority:** the owner's authorization covers the cuts listed in §4. The verdicts below are Claude's, subject to the owner. Where a verdict modifies a recommendation, the modification is stated with its evidence.
- **Baseline:** `a3599fd`: 267 of 267 self-tests (`evidence/v5/tests-baseline-a3599fd.txt`); four reference daemons × 22 of 22 battery checks; contract 4.0.0.
- **Revision:** r4.12, 2026-10-07, by Claude. **Implemented** in five commits (`4f0db7d`, `bf4c0e4`, `9f9cf56`, `1185daa`, and the contract 5.0.0 commit); see `REVIEW-PACKAGE-V5.md` and `evidence/v5/`.

## 1. Verdicts

| Item | Recommendation | Verdict | Conditions and evidence |
| --- | --- | --- | --- |
| Seam 1 | Full R-2: the generic core runs no programs | **ADOPT** | Evidence: `proc.py` serves only `ctx.run`, `ctx.git` and start-up's Git version probe; every other module only borrows its PATH constant. Every remaining reference daemon and the pilot's daemon declare no command. `git-watch` leaves the reference set (it remains in history at `a3599fd`). **No command-backed extension is built in v5.** A future Repository Observer needs one, designed with commands run outside the confined process (PD-72) |
| Seam 2 | E-2: one in-process deadline | **ADOPT** | Falls out of R-2: `ctx`'s per-call time left existed only for commands. One monotonic cycle alarm stays; the watchdog and stop timeouts stay derived outer controls |
| Seam 3 | Keep one interpreter, two callers | **KEEP** | — |
| Seam 4 | Keep two verifiers; one `VerifiedFacts` shape | **KEEP, wording modified** | The independent verifier must not import the package, so `VerifiedFacts` is a **documented shape** both emit, not a shared type. DB-22 compares the two |
| Seam 5 | R-5c deferred with a trigger | **ADOPT** | Start-up fully verifies the ledger; checkpoint acceleration is not part of the v5 contract. **Trigger:** a measurement on the DGX projects start-up verification at the daemon's real record rate to more than half its start timeout (r4.9 measured about 28,000 records a second on the build workspace) |
| Seams 6, 10 | E-11: the unit is a projection; qualification and admission without duplicated predicates | **ADOPT WITH CONDITION** | A projection is pure only if the report binds **every** input of the unit generator. Traced in `unitgen.generate`: the manifest path and digest, `daemon.py`'s digest, the contract digest, the skeleton version, the pattern root, the interpreter, `SPARK_DAEMON_HOME` (written into the unit), and the unit options. **Installing** checks that the report qualifies and was made on this host, then writes the unit. **The runtime** checks, at every start, that the files and contract are those the unit names. Neither repeats the other's predicate |
| Seam 7 | Keep vendoring for v5; then a pinned local package | **KEEP for v5; packaging ADOPT WITH CONDITION** | A package removes the rename only if the names a host sees are neutral or configurable: the package name, the CLI name, the environment prefix and the unit prefix. The pilot forbids the word "spark" in its paths and names its units and settings by its own conventions. So the packaging step must come with R-8 (neutral names) or name parameters. Whether the pilot counts as R-8's "second real consumer" is the owner's call |
| Seam 8 | E-3: the battery never copies or rewrites the manifest | **ADOPT** | The battery runs the manifest and code where they are; an absolute `output_dir` is redirected by one recorded harness parameter |
| Seam 9 | E-6/E-12: no ambient test mode | **ADOPT WITH MODIFICATION** | Production `run` reads no test switch. A separate harness entry takes explicit, recorded parameters: interval, blind limit, cycle budget, output root, jitter off. Unqualified starts and tolerance of missing Landlock are removed. **Modification:** record-only audit stays as a harness parameter, used by DB-03 and DB-17 only. DB-17's meaning is "the kernel alone blocks with the audit hook record-only"; removing it would delete the battery's proof of kernel enforcement, and change a check's meaning, which is a stop condition. It relaxes nothing the kernel enforces, and the production entry cannot select it |
| Seam 11 | One canonical `PathPolicy`, projected into Python, Landlock and systemd | **ADOPT** | Enforcement diversity stays. The outcome for a path refused by more than one layer is unchanged (a Python refusal is a policy violation, exit 78) unless the owner rules on E-4 |
| E-10 | Store facts, not conclusions | **ADOPT, scope clarified** | Applies to a conclusion that is a pure function of facts **in the same artifact**: a report's result, `qualifying`, `valid`. Those are derived on read. It does not apply to ledger records of what the runtime observed or decided at the time (a heartbeat's `mode`, `SENSE_BLIND`, `inherited_blind_ms`, `clock_basis`), which cannot be recomputed later. The printed `RESULT:` line stays as presentation, since installers read it |
| E-11 | The unit is a projection | **ADOPT** | See seams 6 and 10 |
| E-12 | No ambient test-mode authority | **ADOPT WITH MODIFICATION** | See seam 9 |
| Profiles | PRECHECK ⊂ VALIDATE ⊂ BATTERY | **ADOPT** | PRECHECK is the static checks (DB-01, DB-02, DB-24, DB-25); VALIDATE adds the short confined run and the ledger check (DB-03, DB-04); BATTERY is every check. Three immutable ID sets over one registry. The swapped command names print a migration notice for one release |
| E-8 | Six invariants | **ADOPT** | As proposed in r4.11 §5 |
| E-9 | `verify` into `status --verify-only`; authoring commands out of the runtime CLI | **ADOPT after a caller audit** | Known callers of `verify`: this repository's README and tests, and the pilot's README |
| Pilot | Skip 3.1.0 → 4.0.0; move straight to v5 | **ADOPT, with one addition** | **Do not wait for v5 for the first DGX run.** The pilot's installer already qualifies and installs pressure-watch at 3.1.0 on every `fb update`. Its first run as a service under the real host systemd (systemd as PID 1) answers G-6, which no contract version changes |

**Result:**
- **Removed:** seams 1, 2, 5 and 8.
- **Kept on purpose:** seams 3 and 4.
- **Simplified into one boundary:** seams 6 and 10.
- **Simplified:** seams 9 and 11.
- **Kept for this cut:** seam 7.

That leaves six intentional boundaries, and five once packaging replaces vendoring.

## 2. What v5 deliberately does not change

- **Verification diversity:** two independent ledger verifiers, compared by DB-22. HF-38 is the evidence.
- **Enforcement diversity:** Python checks, Landlock and the systemd sandbox. What becomes single is the policy they enforce.
- **Qualification and activation stay separate.** A qualifying report is evidence; installing and enabling remain a person's decision.

## 3. Contract versioning across the cuts

Contract 4.0.0 is published. While the cuts are under way, the contract reports `5.0.0-dev`, which is never recorded in `contract/versions.json`. The pin test accepts an unrecorded `-dev` version but still requires the committed contract files to match the running rules. The last cut sets `5.0.0` and records its digest. Authors migrate once, from 4.0.0 or 3.1.0, to 5.0.0.

## 4. The authorized order, and the stop conditions

**The order:**
1. Full R-2 (with E-2).
2. Profiles, one report shape, facts only, the unit as a projection, and the gates without duplicated predicates.
3. No manifest copy, and the explicit harness in place of test mode.
4. One `PathPolicy`.
5. Six invariants and fewer commands; R-5c recorded as deferred; contract 5.0.0.

**After each semantic cut:** the focused tests and `git diff --check`. **After each commit:** the full self-test suite and every applicable reference-daemon battery. **At the end:** a rerun from a clean checkout.

**Counts:** every reduction in the test or battery-check count is explained by an explicit retired or moved test ID or check ID (`evidence/v5/`). Counts are not preserved artificially.

**Stop, and keep the smallest failing fixture, if:**
- R-2 requires weakening confinement;
- a supposedly command-free daemon depends on running a process;
- removing the timeout plumbing breaks the whole-cycle budget;
- unifying the reports changes the meaning of a check ID;
- the two ledger verifiers disagree.
