# Spark daemon pattern and conformance battery — review and r3 delta (v0.1)

**Status:** DRAFT for adjudication. **PD-24 is ADOPTED, option (a) (owner, 2026-09-29).** Every other decision in §7 is `PENDING`; the owner directed on 2026-09-29 that PD-23 and PD-25 to PD-29 be implemented as documentation and schema without waiting for DGX measurements (done in README r3 and its `registry/`), with their behaviour still subject to normal conformance testing before activation. These are recommendations for the human's Class C ruling.
**Date:** 2026-09-29 · **Author:** Claude · **Review type:** documentation and architecture review, with a proposed delta
**Reviewed:** `spark-daemon-pattern-v0.1-README.md` (r2, implemented) and `spark-daemon-pattern-v0.1-adjudication-AP.md` (2026-09-27)
**Read against:** Spark Probe Standard SPS-1 v0.2; the memory-plugin taxonomy v1.0; the cross-track stress test v0.11; the Steps 9-10-X1 WBS roadmap; the WBS 3.1 entry-ceiling handoff (r5 at the time of review; r7 re-checked for v0.1.1, EC-1/EC-2/EC-7/D-6 unchanged in substance); the kernel/systemd/DGX facts note (2026-09-29)
**Not reviewed:** the code. `battery.py`, `probes.py`, `landlock.py`, `guard.py` and the rest of the package are not in the Project. Every statement below about what the battery *does* comes from the two documents, not from running it. Where that matters the text says so.
**Memory:** off in this session; only Project documents were used.

---

## 0. Summary

The battery's core design is sound and should not be rewritten: a closed manifest declares, a fixed skeleton guarantees, a battery verifies the real daemon in a disposable workspace, and any `UNKNOWN` makes the run `INCOMPLETE`. That already matches SPS-1's principle P2 and is the strongest reason to keep it.

What has changed since r2 is the frame around it. Three things follow.

1. **Two gate-semantics problems (MAJOR).** DB-14 checks memory against a cap with what the README calls "peak RSS", which is a lower bound on what `MemoryMax` enforces (B-1). And "only PASS allows activation" together with "N/A does not block a PASS" lets a PASS stand for less than activation needs: it is not bound to the target platform or to hashes, and an N/A can hide a host where the installed unit would refuse to start (B-2).
2. **The battery is outside the feedback loop.** The README describes its report as "every check, the seed and the environment"; it names no WBS item, no lineage, no clause traces and no quantity basis. A battery run cannot be read by the worksheet, so it cannot say "this check regressed for this daemon" (B-3, B-4). A reading across daemons ("the same check fails for several of them, so suspect the skeleton") is the highest-value one, and it needs a small reader addition as well as an adapter (§4).
3. **The battery is the first instance of a reusable template.** Extract the template (registry, verdict rule, report projection, self-verification) and keep `DB-01` to `DB-18` as its daemon instance. A memory-plugin instance for Step 10 is a natural second one, but it is a sketch here only, because the X1 plugin manifest and the WASM runtime are both undecided (§5).

Seven findings are minor or notes: stale Landlock ABI expectations (B-5), a possibly non-independent ledger verifier (B-6), check-ID stability under the planned process split (B-7), exit-code and home-directory overlaps with SPS-1 (B-8, B-9).

---

## 1. The battery as documented

| Piece | What the documents say |
|---|---|
| Split of roles | Manifest declares safety; skeleton supplies every mechanism; battery verifies the real daemon before activation (README, opening) |
| Runner | Disposable temporary folder with its own `HOME`; never touches the real output directory, `~/spark-core` or `~/spark-governance`; about 15 s (README, "Try it") |
| Checks | 18: `DB-01` to `DB-18`. Statuses `PASS`, `FAIL`, `UNKNOWN`, `N/A` (README §3) |
| Verdict | `PASS` only if nothing fails and nothing is `UNKNOWN`; otherwise `FAIL` or `INCOMPLETE`. `N/A` does not block a `PASS`. Only `PASS` allows activation (README §3, PD-11) |
| Report | `spark-daemon-battery/1`: every check, the seed, the environment |
| Faults | Seeded random SIGKILLs (`DB-05`); `strace` injection of `fsync` EIO and `write` ENOSPC (`DB-08`, `DB-09`); missing `strace` or `systemd-analyze` gives `UNKNOWN`, never `PASS` |
| Companions | 105 skeleton self-tests and 7 fixture daemons, some deliberately bad (README) |
| Evidence | `meminfo-watch`: 18 of 18. `opener` fails `DB-02` and every runtime check. `sneaky` stops itself with exit 78. One host: x86_64, kernel 6.18.44, Landlock ABI 7. **Not run on the DGX** (README, "Evidence") |
| Layers | Purity (static), audit hook (in process), systemd sandbox, and since r2 Landlock (kernel). The README says plainly what each cannot catch: a table for the first three, "Known limits" for Landlock |
| Unbuilt | AP-04 supervisor/worker split (S1 to S7) is design only |

Three things the documents do well and this review keeps: the "Known limits" section is candid; the README corrects a follow-up submission's "4 MiB" ledger constant against the project's own record ("A correction, for the record", which finds the schema ceiling at 1 MiB and left open); and every decision is `PENDING` until the human rules.

---

## 2. What is new since r2

| New input | Source | Consequence for the battery |
|---|---|---|
| Keys for a feedback loop: `wbs_id`, `parent_subject_sha256`, per-clause `TRACE`, `Quantity` with basis, `limit`, `predicted`; an append-only ledger; a 15-reason advisory worksheet | SPS-1 v0.2 §5, §6 | Battery runs should be readable by that reader (B-3, B-4) |
| Measurement basis: a lower-bound proxy can show a limit exceeded, never met | SPS-1 v0.2 §6.2 | DB-14's memory reading (B-1) |
| Proposed memory metric: cgroup `memory.peak` of a fresh transient scope, which contains the process "and every child it spawns" (EC-1); process RSS is only a "lower-bound cross-check"; authority for a frozen number comes from the DGX run only (all `PENDING`) | 3.1 handoff EC-1, EC-2, EC-7 | Same |
| DGX OS kernels and Landlock ABIs: 7.2.3 kernel 6.11 (ABI 5), 7.3.1 kernel 6.14 (ABI 6), 7.4.0 and 7.5.0 kernel 6.17 (ABI 7), 7.6.0 (2026-09-14) kernel 7.0 (ABI 8). The ABI column is inferred, not an NVIDIA statement | Kernel facts note §2, §3 | AP-01's "ABI 7 is expected" is out of date (B-5) |
| Memory plugins are classified by three axes; a Titans-class system is worked through as `MODEL_COUPLED`, `ENTANGLED`, `CONTINUOUS_PARAMETRIC`, never canonical (stress test Finding 1), with full rebuild the only valid purge path; the taxonomy also says it is "not a Step 10 plugin" but a Step 15 question; "the plugin declares, the host decides" is a recommendation | Memory taxonomy §1, §2, §5; stress test Finding 1 | A second battery instance for Step 10 plugins (§5.3) |
| Verification-tier ladder: Titans/ATLAS-class update math at Tier 3 (runtime monitor), containment boundary at Tier 2 (ATLAS is named in the roadmap's tier table and the WP8.6 handoff, not in the taxonomy) | Steps 9-10-X1 roadmap | Where a Titans-class check set would live: Step 15 containment, not the plugin battery (§5.3) |
| No shared daemon exit taxonomy until a demonstrated multi-daemon need and a separate decision (D-6) | 3.1 handoff, "already settled" | B-8 |
| X1 defers the WASM runtime and the signing stack; the daemon pattern deferred WASI (AP-02) | Integration Handoff; AP-02 | The memory instance cannot yet have a runner adapter (§5.2) |
| The supervisor/worker split (AP-04, S1 to S7) is designed and unbuilt | AP doc | B-7 |

---

## 3. Findings

Severity uses SPS-1's vocabulary. Neither MAJOR is a live failure: nothing in the documents shows the reference daemon is unsafe. Both are cases where the evidence a `PASS` carries is weaker than the gate it opens.

| ID | Severity | Finding |
|---|---|---|
| B-1 | MAJOR | DB-14 compares a lower-bound memory reading with the cap that `MemoryMax` enforces |
| B-2 | MAJOR | A `PASS` is not bound to the target platform or to hashes, and `N/A` can pass on a host where the installed unit would refuse to start |
| B-3 | MINOR | The battery report cannot enter the ledger or the worksheet |
| B-4 | MINOR | No traceability from checks to guarantees and clause IDs, and no record of what makes each check fail |
| B-5 | MINOR | The Landlock ABI expectation is one number and out of date; some planned checks depend on the ABI |
| B-6 | MINOR | The documents do not say whether the ledger check uses an oracle independent of the skeleton's own verifier |
| B-7 | MINOR | Check IDs and check meanings need a stability rule before the process split lands |
| B-8 | NOTE | Exit codes: the `battery` command's are undocumented, and SPS-1 describes daemon codes as if shared |
| B-9 | NOTE | Two decisions on where the tools live, and two implementations of the `spark-core` path rule |

### B-1 (MAJOR) — DB-14's memory basis

| Field | Content |
|---|---|
| **Finding** | README DB-14: "Self-measured CPU per cycle, projected to the real interval, and peak RSS within the manifest". PD-12 repeats it. The manifest's `resources` block, including `memory_max_mb`, is "written into the unit" (README §1). The README does not name `MemoryMax`; I take `memory_max_mb` to become it, and the facts note §4 says the OOM killer is invoked inside the unit when usage cannot be contained under that limit. |
| **Affected invariant** | A `PASS` at DB-14 should mean the daemon fits the cap it will run under. |
| **Failure sequence** | (1) A daemon's own peak RSS reads under its cap, so DB-14 passes. (2) Under the unit, the limit applies to the cgroup, whose `memory.current` counts "the cgroup and descendants — page cache, anonymous memory, kernel data structures, socket buffers" (kernel facts §1), and whose scope contains the process "and every child it spawns" (3.1 EC-1). (3) A daemon near its cap, or one whose Git child is large, can be killed by the OOM killer in a run the battery called conformant. The README's own correction paragraph after PD-22 notes that combined supervisor and worker RSS "runs 25-35 MB depending on accounting method", the same sensitivity; the AP document's IF-01 section gives about 25.6 MB (`Pss`/`Private_Dirty`) to about 34.5 MB (naive sum). |
| **Why it is not yet a live defect** | The reference daemon peaks near 19 to 20 MiB against 64 MiB, a wide margin. And it depends on what DB-14 actually reads: if it includes children, the gap is smaller. I cannot tell from the documents. |
| **Smallest correction** | Label the reading's basis in the report. Under SPS-1, a process RSS is `PROXY_LOWER_BOUND`: at or below the cap it is `UNDECIDED`, above the cap it is `OVER`. Add a direct reading, cgroup `memory.peak` of a fresh transient scope (3.1 EC-2: `systemd-run --user --scope -p MemoryAccounting=yes`), which is `DIRECT`. When EC-1 and EC-2 are ruled, an RSS-only run leaves the memory half of DB-14 `UNKNOWN` (`TOOL_MISSING`), so the run is `INCOMPLETE`. |
| **Consequence to state plainly** | Under that rule the reference daemon's 18 of 18 `PASS` on this workspace would read `INCOMPLETE` at DB-14 until it is run in a scope on a host with systemd. That is the rule working, not a regression. |
| **Complexity introduced** | One extra measurement per run, and an SPS-1 change: a `US` (microsecond) unit for CPU per cycle, since SPS-1 v0.2 has only BYTES, KIB, MS and COUNT and a few milliseconds per cycle does not survive integer milliseconds (§6.7, PS-17). The memory decision is PD-24. |
| **Cross-references (v0.1.1)** | Handoff r7 Option G says DB-14 "is already Option A applied per daemon". Under EC-1 it is the RSS proxy that Option A retires, so that sentence is the finding here restated as if resolved; the alignment note `spark-probe-v3-alignment-v0.2.md` (V3-8) recommends dropping it. The same note's V3-1 makes the `PROXY_LOWER_BOUND` label in this row's correction **provisional** until 3.1 Phase 2 tests the direction of the RSS reading against the cgroup peak: an RSS sum over-counts shared pages while the cgroup adds page cache, so the reading's side is an empirical question. The correction stands; the label is a hypothesis the run must test, not a settled fact. |

### B-2 (MAJOR) — What a `PASS` binds, and what `N/A` can hide

| Field | Content |
|---|---|
| **Finding** | README §3: "N/A does not block a PASS (DB-17 when Landlock is unavailable)". PD-11: "only PASS allows activation". PD-15's recommendation: Landlock "required for installed units; best-effort only in test mode". The battery runs in test mode (its knobs, such as record-only audit and a 200 ms interval, exist only there, PD-10). |
| **Affected invariant** | A `PASS` is the necessary condition for activation; it must describe the host and artifact the unit will run on. Measurement does not confer authority (SPS-1 P3): the human's Class C ruling does. |
| **Failure sequence** | (1) The battery runs on a host without Landlock. DB-17 is `N/A`, everything else passes, the verdict is `PASS`. (2) Someone installs the unit. (3) By PD-15's recommendation, start-up refuses (exit 78). A `PASS` preceded an activation that cannot start. On the DGX kernels in the facts note (ABI 5 to 8) DB-17 cannot be `N/A`, so this bites on other hosts. |
| **Second gap** | The README says the battery has run on one x86_64 workspace ("Not yet run on the DGX Spark (aarch64)") and says nothing that stops that `PASS` counting for a DGX activation. SPS-1's `NOT_ON_TARGET` reader rule (PS-19, following 3.1 EC-7) is the model for this. As drafted, PS-19 recommends APPROVE for `MEASURE` and `OBSERVE` and DEFER for the other kinds, and is `PENDING`, so a `GUARD` battery needs it extended (PD-23). |
| **Third gap** | The README does not say what a `PASS` is bound to. A `PASS` earned for one manifest, code tree or skeleton version should not survive a change to any of them. |
| **Smallest correction** | (a) Reword PD-11: `PASS` is **necessary, never sufficient**; `FAIL` and `INCOMPLETE` block. (b) Treat an `N/A` that stems from the host as an owed environment (SPS-1's `owed_environments`), which makes the run `INCOMPLETE` for activation; keep `N/A` for checks that are structurally inapplicable to the manifest. (c) Bind each verdict to the hashes of the daemon tree, the manifest, the battery registry, the battery code and the host fingerprint, and require a `PASS` on the target platform. (d) State what happens when a check itself crashes: SPS-1 records `ERROR`, which is inconclusive and never a `FAIL`. The README lists no `ERROR` status. |
| **Complexity introduced** | Small. A host-caused inability can already be recorded in SPS-1 v0.2 as an `UNKNOWN` case with reason `ENVIRONMENT_OWED`, which is inconclusive and so makes the run `INCOMPLETE`. What v0.2 does not do is make an off-target or proxy-only reading block: those surface only as advisory worksheet rows (`NOT_ON_TARGET`, `LIMIT_UNDECIDED`). Making them block is an SPS-1 rule change (§6.7). |

### B-3 (MINOR) — The report cannot enter the ledger

The battery's native report and `spark-probe/1` share the idea (statuses, `INCOMPLETE` on `UNKNOWN`, seed, environment). The README enumerates the battery report only as "every check, the seed and the environment", so I cannot say which keys overlap; it names none of the ledger keys (WBS item, parent hash, contract hash, clause traces). Consequences: a daemon's battery history cannot be read as "did DB-05 pass at the previous code hash and fail at this one" (`REGRESSION`), and cross-daemon recurrence cannot be seen. The gaps in vocabulary:

| Battery | SPS-1 | Handling proposed |
|---|---|---|
| `PASS` / `FAIL` / `UNKNOWN` | same | direct |
| `N/A` | none | B-2(b): host-caused becomes an owed environment |
| (crash not defined) | `ERROR` | B-2(d) |
| `PASS` / `FAIL` / `INCOMPLETE` verdict | `completeness` plus `subject_status`, exit `0` / `3` / `4` | a `FAIL` is a reproduced defect (`REPRODUCED`), so exit 3; `INCOMPLETE` is exit 4. SPS-1 §6.1 says 4 "takes precedence over 3"; the README says only "FAIL or INCOMPLETE", so a run with both a `FAIL` and an `UNKNOWN` needs the same rule stated (PD-23) |
| `DB-nn` | `case_id`, requirement IDs | §4 |
| seed, environment | `seed`, `python_minor`, `arch`, `kernel_minor` | direct |
| no `wbs_id`, parent hash, contract hash | required or nullable keys | §4 |

Smallest correction: an adapter that projects a battery report into `spark-probe/1` records, and a mapping table in the README. Do not merge the two schemas now (PD-25). The daemon report carries fields SPS-1 does not need.

### B-4 (MINOR) — Traceability and detection

The README's guarantee table cites clause IDs (`LG2` to `LG4`, `FL1`, `RC1` to `RC4`, `VR6`, `RC7`, `CS1` to `CS9`, `ES5`, `FS1`, `FS2`, `FL4`) and other sources. The check table does not say which guarantee or clause each check exercises. As a result the documents cannot answer "which WBS 3.0 clauses has no battery check" (SPS-1's `UNEXERCISED_REQUIREMENT`). Guarantees the README lists that no check in its table names:

- errors recorded as category and exception class only (PD-07, `FL4`);
- tool versions in `DAEMON_START` (proposal B2, `SC9`); DB-04 checks manifest and code hashes only;
- output directory mode, ownership, symlink refusal and foreign files reported, never touched (`FS1`, `FS2`, proposal B3); DB-03 checks that a run produces zero events and changes no file outside the output directory;
- bounded command output and process-group kill (v0.3 §3.8, `FL4`), and Git hardening (F3 to F5); the README's self-test categories cover these;
- canonical JSON, the pinned golden vector, no floats (`CS1` to `CS9`); DB-04 verifies the chain only;
- the bound on streaming-verification memory (`VR6`, `RC7`); self-tests cover it;
- start-up order beyond `READY=1` after `DAEMON_START` (`RC1`); jitter and GC forcing (PD-21, PD-22).

Some of these are properly self-test territory: the battery checks a *given daemon* under the skeleton, the self-tests check the skeleton. The finding is that the documents do not say which is which.

Second half: only two of the seven fixtures are described. `opener` "fails DB-02 and every runtime check"; `sneaky` is a `FAIL` that stops itself with exit 78 on its first cycle, but the README names no check for it (DB-13 or DB-03 could each catch it). The README does not say which check each of the other five turns red. A check never seen to fail proves little; SPS-1 makes this a hard rule only for `PROVE` runs (§3.1, §3.4), so extending it to a `GUARD` battery is this review's recommendation, not an SPS-1 requirement (PD-26). Smallest correction: add `Exercises` and `Detected by` columns (§6.1, Table T-1).

### B-5 (MINOR) — The Landlock ABI is a host variable

README ("Evidence") says ABI 7 "is also expected" on the DGX; AP-01's section says DGX OS 7.4.0 (kernel 6.17) gives ABI 7. The facts note (2026-09-29) lists DGX OS 7.6.0 (2026-09-14) on kernel 7.0, ABI 8 (inferred), and older releases at ABI 5 and 6. Consequences:

- Signal scoping needs ABI 6 (facts note §2: "abstract UNIX socket and signal scoping"). AP-04's assessment expects a worker that "from ABI 6 cannot signal the supervisor", and the AP Landlock evidence table shows signalling blocked at ABI 7 and allowed at ABI 4, 3 and 1. On DGX OS 7.2.3 (ABI 5, inferred) that protection would be absent. S7's list of new checks (worker hang, crash, garbage and oversize messages, restart limit, "worker cannot write the output directory") has no signal check, so a signal check is a proposed addition, conditional on ABI 6 or later.
- DB-17's `N/A` floor (ABI below 2) cannot occur on any listed DGX OS kernel, so that branch is effectively test-only there.
- "UDP is never covered" is time-bound. The facts note lists ABIs 10 to 11 as adding "UDP controls; `NO_NEW_PRIVS` flag" (kernel versions not confirmed); ABI 9 (kernel 7.1) adds a pathname UNIX socket restriction.

Smallest correction: replace the single expectation with the table, mark the ABI column as inferred (confirm on the box), and give each check a `Requires` entry (for example DB-17's TCP part needs ABI 4, signal scoping needs ABI 6).

### B-6 (MINOR) — Independence of the ledger check

`DB-04` "Chain verifies" and `DB-16` "Two verifications agree" both appear to use the skeleton's own `verify`. If so, a defect in that verifier makes DB-04 pass vacuously, and agreement between two runs of one verifier is not independence. SPS-1 requires independent oracles for `MEASURE` fixtures (§3.4, §3.7); the same principle is the reason to ask here, though it is not a rule SPS-1 imposes on a `GUARD` battery. AP-03 (itself `PENDING`) argues, on a cross-check against Node's `JSON.stringify`, that any RFC 8785 implementation plus a SHA-256 tool can verify the chain; its third-party-library cross-check (J2) is still pending. On that argument a small independent verifier looks cheap. **I cannot tell from the documents whether one exists**; the correction is a question for the code owner, then an independent check if the answer is no.

### B-7 (MINOR) — Check IDs and the process split

AP-04's S7 adds checks (worker hang, crash, garbage and oversize messages, restart limit, "worker cannot write the output directory"). AP-04's "Costs (measured)" says "The watchdog-starvation self-test changes meaning and must be rewritten", and I expect DB-03, DB-11, DB-12, DB-13 and DB-17 to change scope with two processes (my inference from what each check covers, not a statement in the documents). SPS-1 was written because IDs drift (I3). Recommendation: check IDs are immutable and never reused; new checks take the next number; a check whose meaning changes gets a new ID and the old one is retired with a reason; the check registry is hashed, and that hash is the SPS-1 `contract_sha256`, so a redefinition resets history instead of producing a false regression.

### B-8 (NOTE) — Exit codes

The README lists daemon exit codes (0, 2, 65, 70, 73, 78) and AP-04 proposes 75, but does not document the `battery` command's own. SPS-1 PS-05 says probes exit `0`, `3` or `4`, and that `2`, `65`, `70`, `73`, `78` "keep their X1 and daemon meanings". The project's settled position D-6 (3.1 handoff, "already settled") is *no shared daemon exit taxonomy* until a demonstrated multi-daemon need and a separate decision. PS-05 already ends "Rule together with the 3.0B/3.1 handoff Q8 (is X1 the one taxonomy for all daemons)", so the tension is known; this finding adds that D-6 appears to answer Q8 for now (my reading; the handoff is `PENDING` throughout). Recommendation: document the `battery` command as 0 `PASS`, 3 `FAIL`, 4 `INCOMPLETE`; describe 3 and 4 as a probe-tool convention, not as part of a daemon taxonomy; and word PS-05 so it does not imply one.

### B-9 (NOTE) — Home directory and path rule

PD-14 (where the pattern lives: `~/spark-tools` or `~/spark-governance/tools`) and SPS-1's PS-12 (`~/spark-governance/tools/probe/`) are the same question; PD-14 already says to decide it together with the script board's Home question. Separately, the daemon pattern's rule (a fixed base deny list that includes `~/spark-core/data`, and `output_dir` "never inside `~/spark-core`", README §1; enforced in `guard.py` per the README) and `probe_ledger.refuse_spark_core` (any path component named `spark-core`) both implement a rule about `spark-core` for different purposes (a daemon's runtime reads versus development probes). That is two definitions of one thing (SPS-1 P9); a one-line note that each stays in its own scope is enough for now.

---

## 4. Mapping a battery run onto `spark-probe/1`

The battery is a `GUARD`-kind probe family. Almost every field below already exists in SPS-1 v0.2. Three do not, or do not behave as needed, and they are listed after the table (a to c).

| SPS-1 field | Battery value |
|---|---|
| `probe_kind` | `GUARD` |
| `wbs_id` | the daemon's WBS item |
| `probe_id` | the battery run identifier (SPS-1's SLUG grammar: lower-case letters, digits and hyphens); it is the run's identity, and `DUPLICATE_RUN` keys on it |
| `review_ref` | the review or adjudication the run belongs to (SPS-1 §3.2), not the run. The reader counts distinct `review_ref` values, so a per-run value would make every run its own review |
| `subject_sha256` | the daemon tree hash: code plus manifest (SPS-1 PS-09 tree hash) |
| `parent_subject_sha256` | the previous daemon tree, so a fix is linked to what it fixed |
| `contract_sha256` | the hash of the check registry (§6.1, Table T-1). The registry is the contract |
| `probe_sha256` | the hash of `battery.py` and `probes.py` |
| Cases | one `GUARD` case per check. `case_id` follows SPS-1's grammar (lower-case letters, digits and underscores, 3 to 64 characters), so `DB-05` becomes `db_05` |
| `requirements` on each case | at least one requirement ID per case (SPS-1's registry rule for `GUARD` and `COVERAGE`), each matching SPS-1's `REQ_ID` grammar (a capital letter first, then letters, digits, dots and hyphens; at most 24 characters). Where a check has no external clause ID, the registry gives it its own (`DB-05`); free-text sources in T-1's `Exercises` column stay notes |
| `seed`, `python_minor`, `arch`, `kernel_minor` | as recorded today |
| `owed_environments` | one per host-caused inability, per missing tool, and one when the run is not on the target platform |
| `mutation_total` / `mutation_killed` | the number of registry checks / the number for which a failing fixture or skeleton mutant is recorded (T-1, `Detected by`). SPS-1 requires these only for `PROVE`; here they are informational (PD-26) |
| `Quantity` | `peak-kib` (`DIRECT`, from a scope); `peak-rss-kib` (`PROXY_LOWER_BOUND`, from process RSS) under its own `quantity_id`; `cpu-us-per-cycle` (see b); `landlock-abi` (`COUNT`, `DIRECT`), so the ABI needs no new field |
| `completeness` / exit | `INCOMPLETE` when any case is `INCONCLUSIVE` (see a). Exit `0` / `3` / `4`; 4 takes precedence over 3 |

Three points that are not free:

- **(a) What makes a run `INCOMPLETE`.** In SPS-1 v0.2 a report is `INCOMPLETE` for an `INCONCLUSIVE` case, missing provenance, a missing fidelity `PASS`, a `PROVE` mutation gap, or a `MEASURE` run with no quantity. Owed environments, off-target runs and proxy-only readings do not change it; they appear as advisory worksheet rows. So a host-caused `N/A` must be recorded as an `UNKNOWN` case with reason `ENVIRONMENT_OWED` to reach `INCOMPLETE` today. Making off-target and proxy-only readings block is an SPS-1 change (§6.7).
- **(b) The `US` unit.** `cpu-us-per-cycle` needs a microsecond unit; SPS-1 v0.2 has BYTES, KIB, MS and COUNT. Listed in §6.7.
- **(c) One reading per quantity point.** A quantity point is (`quantity_id`, `at_id`, `at`, `condition`); basis is not part of it. Two readings at the same point in one report are rejected (`duplicate_quantity_point`), and across runs a later reading supersedes an earlier one (deliberately: a direct measurement replaces the proxy it follows). So a later RSS-only run would overwrite an earlier `DIRECT` reading. That is why the RSS reading needs its own `quantity_id`. SPS-1 §10.6's example has the same collision and should be corrected in v0.3.

What the reader then gives without new code:

- `REGRESSION` and `CORRECTION_FAILED`: per check, per daemon lineage. A lineage is the same subject hash or a `parent_subject_sha256` chain, so two different daemons never share one.
- `OWED_EVIDENCE` and `NOT_ON_TARGET`: the honest state of "not yet run on the DGX", per daemon. `NOT_ON_TARGET` is a reader rule that PS-19 recommends for `MEASURE` and `OBSERVE`; extending it to `GUARD` is part of PD-23.
- `UNEXERCISED_REQUIREMENT`: the WBS 3.0 clauses no check exercises, if the caller declares the clause list.
- `LIMIT_EXCEEDED`, `LIMIT_UNDECIDED`, `LOW_HEADROOM`: DB-14, with the proxy rule of B-1.

What it does **not** give without a change:

- `RECURRING_CLASS` and `REQUIREMENT_HOTSPOT` read `FINDING` and `REQUIREMENT` records, which exist only for findings. A report made of `GUARD` cases produces `TRACE` records and neither. To feed them, each failed check would have to emit a finding (with a `defect_class` and requirements).
- The **cross-daemon** reading: a check that fails for several daemons is a skeleton defect candidate. The reader follows a clause only within one lineage (`requirement_histories` is keyed by lineage, contract hash and requirement), so it cannot show this. It needs a small addition: group failing `TRACE` records by (contract hash, requirement) across lineages. It is not in v0.2. Recommendation: build it when at least two daemons have battery history (PD-25(b)).

Not needed and not proposed: merging the schemas, moving the battery into the probe kit, or making the daemon package depend on the kit. Both are standard-library only, and an adapter keeps them independent (PD-25).

---

## 5. The template, and what instances it could have

### 5.1 The battery template (an SPS-1 profile)

| Part | Rule |
|---|---|
| Subject class and manifest | A closed-schema manifest in which the subject declares what it claims. The host, not the subject, decides what a declaration entitles it to (the memory taxonomy §5 recommends "the plugin declares, the host decides") |
| Check registry | Immutable IDs. Per check: title, `Exercises` (guarantee rows and clause IDs), `Requires` (tools, kernel features, ABI), `Detected by` (a failing fixture or skeleton mutant), status rules |
| Runner adapter | Disposable workspace with its own `HOME`, seeded faults, no egress, standard library. Adapter-specific: it is the part that differs per subject class |
| Verdict rule | `UNKNOWN`, owed environments, off-target runs and proxy-only readings make `INCOMPLETE`. `PASS` is necessary, never sufficient. `FAIL` and `INCOMPLETE` block |
| Report | Native report plus the projection of §4 |
| Self-verification | Recommended (PD-26): every check has a demonstrated failing case, and the registry is mutation-checked like SPS-1's kit. SPS-1 itself requires this only for `PROVE` runs |
| Boundaries | SP-10; no read or write under `~/spark-core` before WBS 4.1; content-free records; closed vocabularies |

### 5.2 Instance A: daemon conformance (`DB-01` to `DB-18` and S7's additions)

Keep it. Change per B-1 to B-7. The runner adapter is the existing `battery.py`.

### 5.3 Instance B: memory-plugin conformance (a sketch; deferred)

The taxonomy recommends a design rule (§5, `PENDING`): a plugin declares `coupling`, `separability` and `write_discipline`, and the host decides what that entitles it to. A battery would verify the declaration by behaviour. Scope matters here. The taxonomy says a Titans-class system is "not a Step 10 plugin" and belongs to the Step 15 transformer-selection question, and that a `MODEL_COUPLED` system should not be evaluated as a memory plugin at all. So this instance is for Step 10 plugins (`EXTERNAL` coupling; `DETERMINISTIC` or `INFERENCE_MEDIATED` write discipline). Candidate checks, none specified:

| Candidate | Verifies |
|---|---|
| `MB-01` | The manifest declares all three axes; a missing one is `UNKNOWN`, never assumed |
| `MB-02` | Declared `SEPARABLE`: retracting one item removes it and leaves the others unchanged (compare with a rebuild from the remaining set) |
| `MB-03` | Declared `DETERMINISTIC`: the same inputs give the same index hash |
| `MB-04` | Declared `INFERENCE_MEDIATED`: its output is a proposal until accepted; no canonical write bypasses the gate |
| `MB-05` | Retraction followed by a full rebuild from the authorized revision set leaves no trace of the retracted item on probe queries (a proxy: absence on queries is a lower bound on leakage, so this can show a leak and never prove absence) |
| `MB-06` | Every derived record traces to its contributing revision IDs (Step 10's lineage invariant) |
| `MB-07` | Hostile payload text is data: no instruction following, no retention beyond what is declared (SPS-1 P4; the `ERR_CONTENT_RETENTION` class) |
| `MB-08` | The confinement checks (no egress, no writes outside the output) apply to the plugin's runner |
| `MB-09` | Resource budget, measured with the same basis rules as B-1 |

A Titans/ATLAS-class system is outside this instance. The roadmap's verification-tier ladder treats it as a Step 15 subsystem: cache-only, never canonical, purge by rebuild for the containment boundary (Tier 2), plus a runtime monitor on the update math (Tier 3). If Step 15 ever adopts one, its containment checks are natural `GUARD` cases in the same template (for example "nothing derived from the model-coupled state is read as canonical"), under the roadmap's own rule. The roadmap also says heavy verification ahead of the Step 15 decision would be disproportionate, and that decision (XC.3) is still `PENDING`.

Why deferred: the X1 plugin manifest is not final, the WASM runtime is not chosen (Integration Handoff; AP-02), so there is no runner adapter to specify. The template and the registry columns can be settled now; the instance cannot. A natural home when the time comes is the Step 10 candidate or shadow evaluation before promotion, with the authority to promote staying with the Step 9 and 10 controls, not with the battery.

---

## 6. Proposed r3 delta to the two documents

Concrete edits, to apply on adoption. *v0.1.2:* applied on 2026-09-29 as documentation and schema (README r3 and its `registry/`, AP document r3 edits) at the owner's direction; only PD-24 is adopted, and no battery code has changed.

### 6.1 README §3: the check table gains `Exercises`, `Requires`, `Detected by` (Table T-1)

`Exercises` is **my reading of the README's own guarantee table**, and needs confirming against `battery.py`. `Requires` is filled only where the README or the facts note says a tool or ABI is needed (and marked where I assumed). `Detected by` is blank for the author to fill, apart from `opener`, which the README says fails DB-02. `Exercises` entries are notes; to enter the ledger, each check also needs at least one `REQ_ID`-shaped requirement (§4), for which its own ID (`DB-05`) will do.

| ID | Check | Exercises (inferred) | Requires | Detected by |
|---|---|---|---|---|
| DB-01 | Manifest validates | manifest schema (README §1) | — | *(author)* |
| DB-02 | Purity | PD-05 | — | `opener` |
| DB-03 | Confinement | v0.3 §6.2; `FS1`, `FS2` in part | — | *(author)* |
| DB-04 | Ledger integrity and provenance | `LG2` to `LG4`; B2, `SC9` in part | — | *(author)* |
| DB-05 | Crash and restart | Observer WBS 3.2 | — | *(author)* |
| DB-06 | Torn tails | `RC2` to `RC4` | — | *(author)* |
| DB-07 | Corrupt ledger | `RC2` to `RC4` | — | *(author)* |
| DB-08 | fsync failure | `LG2` to `LG4`, `FL1` | `strace` | *(author)* |
| DB-09 | Disk full | `LG2` to `LG4`, `FL1` | `strace` | *(author)* |
| DB-10 | Digest containment | `ES5` | — | *(author)* |
| DB-11 | Notify protocol | `RC1`; proposal C1 | — | *(author)* |
| DB-12 | Single instance, SIGTERM | Observer WBS 3.1 | — | *(author)* |
| DB-13 | Audit hook | PD-06 | — | *(author; `sneaky` may be this one)* |
| DB-14 | Resource budget | PD-12 | cgroup scope for the direct memory reading | *(author)* |
| DB-15 | Unit hardening | v0.3 §4; dev-agents C11 | `systemd-analyze` | *(author)* |
| DB-16 | Read-only verification | verification is read-only | — | *(author)* |
| DB-17 | Landlock enforces alone | AP-01 L5 | ABI 2 (TCP part ABI 4) | *(author)* |
| DB-18 | `DAEMON_START.landlock` is honest | AP-01 L4 | ABI 2 *(assumed)* | *(author)* |

### 6.2 README §3: verdict paragraph and PD-11

Before: "The verdict is PASS only if nothing fails and nothing is UNKNOWN; otherwise FAIL or INCOMPLETE. N/A does not block a PASS (DB-17 when Landlock is unavailable)." and PD-11: "INCOMPLETE whenever any check is UNKNOWN; only PASS allows activation."

After (proposed): The verdict is `PASS` only if nothing fails, nothing is `UNKNOWN`, no environment is owed, no reading is proxy-only against a limit, and the run is on the target platform. `N/A` is reserved for a check that is structurally inapplicable to the manifest. A host-caused inability to run a check is an owed environment and makes the run `INCOMPLETE`. `PASS` is necessary for activation and never sufficient; the human's Class C ruling remains. Each verdict names the hashes it binds: daemon tree, manifest, check registry, battery code, host fingerprint.

### 6.3 README: DB-14 row and PD-12

Add: memory is reported with its basis. Process RSS is `PROXY_LOWER_BOUND`. A cgroup `memory.peak` reading in a fresh transient scope is `DIRECT` (3.1 EC-2, once ruled). Report CPU per cycle in microseconds.

### 6.4 README "Evidence" and AP-01 "Expected ABI on the DGX Spark"

Replace "ABI 7 is also expected" with the table:

| DGX OS | Kernel | Landlock ABI (inferred) |
|---|---|---|
| 7.2.3 | 6.11 | 5 |
| 7.3.1 | 6.14 | 6 |
| 7.4.0, 7.5.0 | 6.17 | 7 |
| 7.6.0 (2026-09-14) | 7.0 | 8 |

Add "the mapping is inferred from the kernel ladder, not an NVIDIA statement; confirm on the box", and change "UDP is never covered" to "no ABI up to 9 covers UDP; the facts note lists ABIs 10 to 11 as adding UDP controls and a `NO_NEW_PRIVS` flag (kernel versions unconfirmed)". Source: kernel facts note §2 and §3.

### 6.5 README: new short section, "Battery report and the probe standard"

The mapping table of §4, and the exit codes of the `battery` command (0 `PASS`, 3 `FAIL`, 4 `INCOMPLETE`), described as a probe-tool convention (B-8).

### 6.6 AP document: AP-04 and S7

State in AP-04's assessment that "worker cannot signal the supervisor" requires ABI 6 or later, and add a conditional signal check to S7's list (B-5). New checks take the next free numbers (B-7). The proposed exit code 75 is recorded as a proposal, not part of a shared taxonomy (B-8).

### 6.7 SPS-1 (for a v0.3 of that document)

| Change | Reason |
|---|---|
| §5.4 ("How a WBS item plugs in") gains a row: "a conformance battery (a registry of checks per subject class)": kind `GUARD`, mapping of §4 | B-3 |
| PS-17 gains the unit `US` (microseconds). A ratio unit is not proposed until an instance needs one | B-1 |
| §6.1: an owed environment, an off-target run and a proxy-only reading against a limit each make a run `INCOMPLETE` for kinds that gate an activation (today they are advisory worksheet rows), and a host-caused inability is recorded as `UNKNOWN` with reason `ENVIRONMENT_OWED` | B-2; §4(a) |
| PS-19 extended from `MEASURE` and `OBSERVE` to `GUARD` batteries that gate an activation | B-2 |
| §10.6 example: give the RSS reading its own `quantity_id`, since two readings at one quantity point collide | §4(c) |
| PS-05 rewording: probe exit codes 3 and 4 are a probe-tool convention; daemon codes are not described as a shared taxonomy | B-8 (D-6) |
| PS-12 and PD-14 to be ruled together | B-9 |
| Reader: a cross-lineage view of failing `TRACE` records per (contract hash, requirement), for the "same check fails for several daemons" reading. Deferred until two daemons have battery history | §4; PD-25(b) |

---

## 7. Decisions for adjudication

Numbering continues from the README (PD-22).

**PD-23. Battery verdict semantics.** `PASS` is necessary, never sufficient; host-caused `N/A` is an owed environment and makes the run `INCOMPLETE`; a verdict is bound to the daemon tree, manifest, registry, battery code and host fingerprint, and activation requires a `PASS` on the target platform. A run with both a `FAIL` and an `UNKNOWN` exits 4, as SPS-1 §6.1 says. Rewords PD-11, and needs the SPS-1 changes in §6.7 (owed, off-target and proxy-only readings blocking; PS-19 extended to `GUARD`).
Recommendation: APPROVE. Decision: PENDING (documentation/schema implemented in README r3, 2026-09-29, at the owner's direction; behaviour needs conformance testing before activation)

**PD-24. DB-14's memory basis.** Report the basis. Options: (a) strict: a process-RSS reading is `PROXY_LOWER_BOUND`, and activation requires a `DIRECT` cgroup reading from a fresh scope, so RSS-only runs are `INCOMPLETE`; (b) lenient: keep RSS as the gate and add a margin rule (for example RSS at most half the cap counts as within).
Recommendation: (a), once 3.1 EC-1 and EC-2 are ruled. Until then, label the basis and change nothing else. Option (b) replaces evidence with a margin and I would not recommend it for activation. **Decision: ADOPTED (a), owner, 2026-09-29.** The condition was met the same day (3.1 EC-1 ADOPT, EC-2 ADOPT CONDITIONALLY). Recorded in README r3; under the PD-26 rule the change of basis retires DB-14 and adds DB-19. The proxy label stays provisional until 3.1 Phase 2 tests the RSS reading's direction (3.1 handoff §10D D-2). Not yet implemented in `battery.py`.

**PD-25. Report convergence.** (a) Ship an adapter from `spark-daemon-battery/1` to `spark-probe/1` records and a mapping table; do not merge the schemas. (b) Add the cross-lineage reader view (§4) once at least two daemons have battery history.
Recommendation: APPROVE (a); DEFER (b) and any convergence. Decision: PENDING (documentation/schema implemented in README r3, 2026-09-29, at the owner's direction; behaviour needs conformance testing before activation)

**PD-26. Registry rules.** Check IDs are immutable and never reused. Each check carries `Exercises`, `Requires` and `Detected by`, and at least one requirement ID. Each check has a demonstrated failing fixture or skeleton mutant (a recommendation of this review; SPS-1 requires it only for `PROVE`). The registry hash is the SPS-1 `contract_sha256`.
Recommendation: APPROVE. Decision: PENDING (documentation/schema implemented in README r3, 2026-09-29, at the owner's direction; behaviour needs conformance testing before activation)

**PD-27. The template and its instances.** Adopt the template of §5.1 as an SPS-1 profile with the daemon battery as instance A. Defer the Step 10 memory-plugin instance (§5.3) until the X1 plugin manifest and the WASM runtime are decided. A Titans-class system is not an instance of it; any containment checks belong to Step 15.
Recommendation: APPROVE the template; DEFER instance B. Decision: PENDING (documentation/schema implemented in README r3, 2026-09-29, at the owner's direction; behaviour needs conformance testing before activation)

**PD-28. Landlock ABI as a host variable.** Record the ABI table (§6.4) as inferred, give each check a `Requires` entry, and treat an unavailable required feature as an owed environment, consistent with PD-15.
Recommendation: APPROVE. Decision: PENDING (documentation/schema implemented in README r3, 2026-09-29, at the owner's direction; behaviour needs conformance testing before activation)

**PD-29. Exit codes of the `battery` command.** 0 `PASS`, 3 `FAIL`, 4 `INCOMPLETE`, documented; described as a probe-tool convention consistent with D-6.
Recommendation: APPROVE. Decision: PENDING (documentation/schema implemented in README r3, 2026-09-29, at the owner's direction; behaviour needs conformance testing before activation)

**Ruled together, not new:** PD-14 and SPS-1 PS-12 (where the tools live).

---

## 8. Limits of this review

- **Documents only.** I did not read or run the code. Findings B-1, B-4 and B-6 hinge on what `battery.py` does; the text says "cannot tell from the documents" where that applies.
- **Inferred mappings.** The `Exercises` column of Table T-1, and the ABI column of §6.4, are inferences. The ABI table comes from the facts note, which itself marks the column as a mapping.
- **Dependencies not yet ruled.** B-1's strict option depends on 3.1 EC-1 and EC-2, both `PENDING`.
- **Sketch only.** The memory battery is a design intent. It has no runner and no manifest to check against.
- **Not run.** Nothing here has run on the DGX, and nothing in the delta has been implemented or tested.
- **Verification.** An independent agent checked about 150 claims in an earlier version of this draft against the Project documents and the probe kit's code and found 24 discrepancies (12 wrong facts, 3 wrong section or scope attributions, 9 imprecise wordings); 4 claims could not be checked. I confirmed the four that concern the kit's code (report completeness rules, requirement and finding records, case-ID grammar, quantity points) against the code, and corrected the draft. The corrected text has not been checked a second time.
- **Cited pending material.** Several sources cited here are themselves `PENDING` (the 3.1 handoff, AP-03, PS-19, the memory taxonomy's recommendations). They are cited as positions, not rulings.

---

## Revision history

| Revision | Date | By | Change |
|---|---|---|---|
| v0.1 | 2026-09-29 | Claude | First draft: review of README r2 and the AP adjudication against SPS-1 v0.2, the memory taxonomy, the 3.1 handoff and the kernel facts note; findings B-1 to B-9; template; r3 delta; PD-23 to PD-29. Checked by an independent agent; 24 discrepancies corrected before publication (§8) |
| v0.1.1 | 2026-09-29 | Claude | Handoff reference updated r5 → r7 (substance of the cited items unchanged); B-1 cross-referenced to r7 Option G and to the Probe v3 alignment note (V3-1: basis label provisional; V3-8: Option G wording). No finding or decision changed |
| v0.1.2 | 2026-09-29 | Claude | Recorded PD-24(a) as ADOPTED by the owner. Recorded that PD-23 and PD-25 to PD-29 are implemented as documentation and schema (README r3, `registry/`, AP document r3 edits for §6.4 and §6.6) with rulings still PENDING. SPS-1 §6.7 items now implemented in the kit as v0.3 (unit `US`; owed, off-target and proxy-undecided readings make an activation-gating run `INCOMPLETE`, which extends PS-19's effect to `GUARD`; the §10.6 collision is avoided by separate quantity IDs). No finding changed |

## Decision log

| Date | ID | Decision | By | Notes |
|---|---|---|---|---|
| 2026-09-29 | PD-24 | ADOPT (a) | Owner | 3.1 EC-1/EC-2 ruled the same day; README r3 records DB-14 → DB-19 |
