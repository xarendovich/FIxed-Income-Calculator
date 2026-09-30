# Reconciliation with the v0.1 line (r4.1)

- **Status:** RECORD AND RECOMMENDATIONS.
  - One owner ruling from the other line is recorded here under an alias: PD-76, that line's PD-24(a), ADOPTED.
  - The AP adjudication's r3 edits are adopted.
  - One stale fact is corrected: the single expected Landlock ABI.
  - Nothing else changes behaviour. The alias numbering and the conflicts in section 4 need the owner's confirmation.
- **Revision:** r4.1, 2026-09-30, by Claude.
- **Asked:** the owner moved three files from another project "in case this information hasn't been absorbed". It had not been: this repository had none of that line's r3 content.

## 1. Two lines from one r2

Both lines start from the same r2 of 2026-09-27 (Landlock, JCS key order, the 64 MB floor, jitter, GC forcing; 105 self-tests).

| | **v0.1 line** (the moved files) | **This repository** |
| --- | --- | --- |
| Its "r3" | Documentation and schema only (2026-09-29): a battery check registry, the proposed verdict rule, the SPS-1 mapping, the ABI table. "No code in `spark_daemon/` changed" | Code (2026-09-27): 22 defects fixed, the handoff contract, three more reference daemons |
| Afterwards | — | r3.1 to r4.0: Spark cross-checks, the blind period, PD-01 reopened, prior-art review, integration review, heartbeat. Contract 3.0.0, 202 self-tests |
| Battery | r2 battery, with a registry beside it that the battery does not yet read | r2 battery plus r3 fixes (for example DB-04's zero-error rule, HF-16); no registry |
| Owner rulings | **PD-24(a)** (memory basis) | PD-63, PD-01.9, PD-70 |

The two lines use the label "r3" for different things. In this repository, "r3" always means this repository's code revision. The other line's is "the v0.1 line's r3".

## 2. Decision numbers collide; proposed alias

Both lines continued the numbering from PD-22 independently:

| Number | This repository (`DAEMON-CONTRACT.md`) | v0.1 line (battery review §7) |
| --- | --- | --- |
| PD-23 | The contract is the only normative author interface | Battery verdict semantics |
| PD-24 | Contract versioning (PENDING) | DB-14's memory basis (**ADOPTED (a), owner**) |
| PD-25 | Git `safe.directory` scoping | Report convergence (adapter to `spark-probe/1`) |
| PD-26 | The candidate envelope and the no-authority rule | Registry rules (immutable check IDs) |
| PD-27 | Evidence lanes | The battery template and its instances |
| PD-28 | A clean battery run records zero `DAEMON_ERROR` | Landlock ABI as a host variable |
| PD-29 | Contract 1.0.0's rule set | Exit codes of the `battery` command |

Unresolved, "PD-24 adopted" could be read as adopting this repository's contract-versioning decision.

**Proposed alias.** Neither line's documents are rewritten. In this repository, the v0.1 line's decisions take the next free numbers, and every cross-reference names both.

| This repository | = v0.1 line | Status |
| --- | --- | --- |
| **PD-75** | PD-23: battery verdict semantics | PENDING |
| **PD-76** | PD-24: DB-14's memory basis | **ADOPTED (a), owner, 2026-09-29** |
| **PD-77** | PD-25: report convergence | PENDING |
| **PD-78** | PD-26: registry rules | PENDING |
| **PD-79** | PD-27: template and instances | PENDING |
| **PD-80** | PD-28: Landlock ABI as a host variable | PENDING |
| **PD-81** | PD-29: exit codes of the `battery` command | PENDING |

The alternative is to renumber this repository's PD-23 to PD-31. Those numbers are referenced across more documents, and none carries a ruling, so either direction works. The owner should choose once.

## 3. The owner's ruling PD-76 (= v0.1 PD-24(a)) and what it means here

**Ruling:** memory at the budget check needs a *direct* reading: cgroup `memory.peak` of a fresh transient scope containing the daemon and every child. A process-RSS reading is a proxy that never decides, and an RSS-only run is `INCOMPLETE`. Under that line's registry rule, DB-14 is retired and **DB-19** added. The proxy label stays provisional until WBS 3.1 Phase 2 tests the direction of the RSS reading.

**Here:**
- **Not built in either line.** That line says so; this repository's `battery.py` still decides DB-14 on process RSS.
- **Confirmed from the code** (the review could only ask): DB-14 takes `max(wait4 ru_maxrss, DAEMON_STOP's RUSAGE_SELF)`. That is the peak of the largest single process, daemon or child, never a sum, never page cache and never the cgroup. The ruling's premise holds.
- **Consequence of building it here.** On this workspace (a container with no systemd user manager), the direct reading cannot be taken. Every battery run would read `INCOMPLETE` at DB-19, and the Makefile and CI targets that require `RESULT: PASS` would need to accept that. The review calls this "the rule working, not a regression". It is the first thing to settle before building.
- **My PD-71** (integration review) reached the same conclusion independently from the DB-14 comment. The memory-basis part of PD-71 is **superseded by PD-76**. Its remaining part, a named worst-case fixture per observation type and a scaling series to derive `N_max`, is not covered by PD-76 and stays PENDING.
- **The DB-14 comment's "is already Option A applied per daemon".** The battery review traces this to handoff r7's Option G and notes the alignment note V3-8 recommends dropping the sentence. The review in `INTEGRATION-REVIEW.md` §2 agrees.

## 4. Conflicts that need a decision

| # | Conflict | Why it matters | Recommendation |
| --- | --- | --- | --- |
| C-1 | **Battery exit codes.** Here: PASS 0, INCOMPLETE 3, FAIL 1. v0.1 line (PD-81 = its PD-29, pending): PASS 0, FAIL 3, INCOMPLETE 4, with 4 taking precedence | A tool following the SPS-1 convention would read this repository's INCOMPLETE (3) as FAIL, and this repository's FAIL (1) as nothing it knows | Adopt the SPS-1 convention (0/3/4) with PD-81, in the same contract change as the other exit-code work (PD-35). Until then, document both |
| C-2 | **Decision numbers** (section 2) | Ambiguity in the decision log | Confirm the alias, or choose to renumber this repository's |
| C-3 | **The registry exists only in the other line**, and this repository's battery has changed since r2 (DB-04's zero-error rule and others) | Its `passes_when` texts describe the r2 battery, so importing it unchanged would misdescribe this one | Move `registry/` over, then reconcile it against this `battery.py` under PD-78's rule: a check whose meaning changed is retired and replaced, never edited in place |
| C-4 | **Two "r3"s** | Confusing citations | Settled by the naming in section 1 |

## 5. The battery review's findings against this repository

The review was written from documents only. This repository has the code, so several of its open questions can now be answered.

| Finding | Status here |
| --- | --- |
| **B-1** (MAJOR): DB-14's memory basis | Confirmed from code (section 3). Ruled by PD-76, not yet built |
| **B-2** (MAJOR): what a PASS binds; N/A can hide a refusing host | Partly addressed. The battery report binds the manifest, the contract and, since r3.3 (PX-01), the code hash. Still missing: binding to the host fingerprint, the target-platform requirement, and host-caused N/A counted as an owed environment (PD-75, PD-80). Converges with U-7 (evidence expiry) and PD-40 (digest-bound activation) |
| **B-3**: the report cannot enter the SPS-1 ledger | Open. PD-77's adapter is now possible, because this repository's `battery.py` defines the report's keys, which the review could not read |
| **B-4**: traceability; which check catches each fixture | Answered in part. `sneaky` is caught first by **DB-03** (its confined run exits 78), then by every runtime check, not by DB-13 as the review guessed. Traceability converges with this repository's r3.1 recommendation (`ADJUDICATION-SPARK-SOURCES.md` §3) |
| **B-5**: Landlock ABI is a host variable | Fixed as documentation: `ADJUDICATION-AP.md` now carries the ABI table, and the README's "ABI 7 is also expected" is replaced |
| **B-6**: is the ledger check independent? | **Answered: no.** DB-04 and DB-16 both use the skeleton's own verifier, and DB-16's "two verifications agree" runs the same code twice. This converges with U-7 (independence must be shown). Recommendation: a small verifier that does not import `spark_daemon`, cross-checked against a second JCS implementation (AP-03's J2) |
| **B-7**: check-ID stability before the process split | Open; PD-78's rule. This repository has kept every DB number, but DB-04's meaning changed in r3 (zero `DAEMON_ERROR`), which under PD-78 would have meant a new ID |
| **B-8**: exit codes | Conflict C-1 |
| **B-9**: where the tools live; two `spark-core` path rules | Open (PD-14). Unchanged |

## 6. What changes in the integration plan

Phase 2 of `INTEGRATION-REVIEW.md` §4 gains:
- **PD-76** (DB-19, the direct memory reading), together with a decision on how this workspace and CI treat the resulting `INCOMPLETE` (section 3);
- **PD-75** (verdict semantics) and **PD-81** (battery exit codes, conflict C-1), in the same contract change as PD-35;
- **PD-78** (registry rules), with the other line's `registry/` imported and reconciled (C-3);
- an independent ledger verifier (B-6 with U-7).
