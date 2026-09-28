# Daemon pattern × Spark handoffs — cross-check for adjudication (r3.1)

- **Status:** DRAFT FOR ADJUDICATION, a companion to `README.md`, `HARDENING.md` and `DAEMON-CONTRACT.md`. One item (SX-01) is implemented because it is a reproduced security defect. Everything else is a recommendation, and PD-32 to PD-42 are PENDING.
- **Revision:** r3.1, 2026-09-28, by Claude.
- **Question asked:** is there language, contract or architectural design in the other Spark documents, including the completed WBS sections, that the daemon pattern should copy?
- **Short answer:** yes, and more than borrowing. The daemon pattern's ledger and recovery code were drafted from the WBS 3.0 spec **r2**. Since then r3 was adjudicated (2026-09-26) and 3.0C.1 was frozen (2026-09-27), and both changed rules this pattern cites by ID. The Spark handoffs also closed a Git code-execution path that this pattern had not (SX-01, now fixed), and WBS 3.0E.1 had already reviewed this pattern once (section 4).

## Sources

| Source | How read | Relevance |
| --- | --- | --- |
| WBS 3.0 Artifact Writers boundary spec, **r3 adjudicated** (upload) | In full | Normative source for the ledger rules the README cites (CS, LG, VR, RC, DG, ES, FS, FL) |
| WBS 3.0C.1 Durable Ledger Append Boundary, **FROZEN** at `f5ed529` (upload) | In full | The Observer's writer now exists: PD-02's replacement trigger |
| WBS 3.0A.2 secondary reliability review handoff (upload) | In full | Read-side contract closure, torn-tail surface |
| WBS 3.0E.1 Adjudicated Seam Contract (doc) | §1–4 and §8–14 in full; §5–7 skimmed | Its §8, §11 and §14 review this daemon pattern directly |
| WBS 2.5 Baseline & HEAD Events handoff (doc) | In full | The hardened Git profile (A1, E1–E16) |
| WBS 3.0 Artifact Writers handoff, r2-era (doc) | In full | Evidence W1–W12; consumer contract |
| Observer Handoff & Roadmap v0.2 (doc) | In full | Isolation corrections; self-referential safeguards (§6) |
| Observer Improvement Proposal (doc) | In full | A1–E4; the origin of several PDs here |
| Observer Closing Executive Brief (doc) | In full | Egress threat model; single escaping path |
| Spark Script Repository board (doc) | In full | SC1–SC12, relaxations R1–R8, tiers |
| WBS 2.5D3 HeadTransitionCandidate review; WBS25D handoff review (doc and both .docx uploads) | In full | Fixture discipline (the .docx files are exports of the WBS25D review) |
| Spark Core Universal Harness (H-Track) v0.1 (doc) | In full | Digest-bound tickets, declared safe state |
| Kernel v0.2 Stage A review (page) | In full | KECC change classes; K2 requirement families |
| Step 8.5 Architecture Review (page) | **Not read** | Memory reconciliation; outside the daemon pattern's scope |
| `xarendovich/FirstBorn` handoff package | In full (previous turn) | Operational patterns; see section 6 |

## 1. Fixed in this revision

**SX-01. Adopt the Observer's WBS 2.5 hardened Git profile in `ctx.git` (security, reproduced).**
WBS 2.5 A1 and evidence E1–E3 showed that a repository's own configuration can run a program during `git log`, and can forge history under genuine SHAs. The r3 `ctx.git` had inherited only part of that profile. Against r3:

- **E1:** with repo-local `log.showSignature=true` and `gpg.program` set, `ctx.git(repo, ["log", ...])` and `["show", ...]` **ran the configured program**. `rev-list` did not.
- **E2:** a `refs/replace/*` entry made `ctx.git` report a forged subject under the real SHA, and `rev-list --count` returned 1 instead of 3.
- **E3:** `.git/info/grafts` made `rev-list --count` return 1 instead of 3.

The fix adds `-c log.showSignature=false -c log.mailmap=false` to every call (command-line config outranks repository config), plus `GIT_NO_REPLACE_OBJECTS=1`, `GIT_GRAFT_FILE=/dev/null` and `GIT_NO_LAZY_FETCH=1`. Tests are in `tests/test_hardening.py::GitHistoryIntegrityTests`. All four cases fail without the fix and pass with it. Each fixture first proves that plain `git` is affected, so the test cannot pass vacuously. The contract moves to **1.0.1**: the enforced Git environment changed, but no rule a daemon must satisfy did.

## 2. Conformance against WBS 3.0 r3 and 3.0C.1

The README credits these rules to "WBS 3.0". The column "today" was checked by reading `spark_daemon/ledger.py` and `runtime.py`. Items marked *by reading* were not run.

| ID | r3 / C1 rule | Daemon pattern today | Impact | Recommendation |
| --- | --- | --- | --- | --- |
| SC-01 | **C2 / RC1 step 1:** startup opens and reads; it never issues a speculative or retry `fsync` | `ledger.recover()` fsyncs the ledger and the directory before verifying (the r2 protocol, now r3 Appendix A) | After an `fsync` failure, the restart's `fsync` can report success on pages Linux already dropped: the exact false success FL1 exists to prevent | Remove both startup fsyncs (PD-32) |
| SC-02 | **C3 / RC3:** crash-idempotent quarantine with a pending receipt; restart completes the missing step exactly once (AT-38) | Quarantine file, then truncate, then the `LEDGER_TAIL_QUARANTINED` record is appended later, in `run()`. A kill between truncate and append leaves a quarantine file that no record references, forever. The r2-era rule "record unreferenced quarantine files at the next start" is not implemented either (*by reading*) | A repair that the ledger never records | Adopt the receipt protocol, or at minimum detect unreferenced quarantine files at start (PD-32) |
| SC-03 | **RC5 / A.2:** a non-empty ledger whose first record is not the origin record refuses to start | Checked only by the battery (DB-04), not by `recover()` | A ledger that starts mid-history is accepted at run time | Enforce in `recover()` (PD-32) |
| SC-04 | **FS6 / C7 / C1:** existing artifacts must be regular files, owned, at most 0600; opened with `O_NOFOLLOW` and checked with `fstat`; C1 pins `(st_dev, st_ino)` across preflight and open | The output and quarantine directories are checked (FS2 holds). The ledger, digest and quarantine files are opened by path without `O_NOFOLLOW` or `fstat`, and there is no identity pinning | A same-user symlink or replacement inside the 0700 directory is followed | Adopt FS6 and C1's identity binding (PD-32) |
| SC-05 | **C4 / FL5 / DG1 / DG5 / DG6:** the digest derives only from committed ledger state; a failed refresh stops the cycle; startup regenerates it unconditionally | The digest renders `digest(snapshot, recent)` from the in-memory snapshot, which may hold nothing committed; a failure is logged and the loop continues; no startup regeneration | Two different digest semantics under one name | Owner's call: conform, or record a deliberate divergence (PD-33) |
| — | **Already conforming** | LG2 one-write append; LG4/FL1 exit on any write or `fsync` error with no retry; RC2 torn tail = every byte after the last LF, reported as offset and length (A.2 PR-1); RC4 corrupt complete line refuses with nothing changed; VR5 bounded reports; CS golden vector; read-side value domain equals write-side (A.2 PR-2: strict parse, canonical round trip, safe-int range); C1 "refuse further appends after an uncertain write" (the process exits) | — | Credit in the README |

**PD-02 has partly triggered.** PD-02 says `ledger.py` is provisional until "the Observer's own writer exists". `observer_writer.py` (3.0C.1) is now frozen, and 3.0A (`observer_ledger.py`, `observer_verify.py`) is in review. Recovery (3.0B) is not written yet (PD-34).

## 3. Language and contract to borrow

| From | What | Where it goes |
| --- | --- | --- |
| r3 §1 | "WBS 3.0 owns **durability, not meaning**. Scope documents should say '3.0 persists…', never '3.0 decides…'." | The contract's first line: *the skeleton owns durability and confinement, not meaning; the daemon owns meaning and nothing else.* |
| r3 "Who decides" | Class C decides; after implementation, pass or fail is **Class B**, decided by deterministic tests an LLM may run but is not the authority behind. "Convergence is a signal; only a recorded decision freezes an item." | README decision section; `AUTHORITY` in the contract |
| r3 decision values | APPROVE / MODIFY / REJECT / DEFER, each with a stated consequence; `Recommendation:` and `Decision:` lines; apply MODIFY and REJECT to the text, then re-run the reference check | Replace the pattern's APPROVE/DEFER-only PD format (PD-39) |
| r3 §10 step 3 | "Every referenced ID is defined, every requirement is named by at least one test, and every test belongs to a package" | A traceability test over README guarantees, DB checks and tests (also FirstBorn item 4) |
| r3 G0 | A read-only extraction gate before any source edit | Step 0 of the authoring loop (`DAEMON-CONTRACT.md` §7): `describe` is the G0 of a candidate |
| r3 IG5 / C3 lesson | Full-block replacement; anchor insertions on the first decorator, not the `def` line; a pre-commit check running focused and full suites plus `git diff --check` | Contributor guidance and a pre-commit hook for this repository |
| Kernel v0.2 KECC | Change classes **C0 clarification, C1 additive, C2 transformable (with an adjacent upcaster), C3 breaking**; F-04: shrinking a declared support horizon is C3 | Replace the invented semver wording for `contract_version`; add a declared support horizon of accepted contract versions (PD-39) |
| Kernel v0.2 K2-SPEC | BCP 14 normative language (MUST / SHOULD / MAY) | Contract text |
| Script board SC2 | `RESULT:` line, exits 0/1/2/3, `--json` tagged with a schema | `validate --json` and `precheck` already follow it; keep one shape across tools |
| Script board tiers | T0 Observe … TS Spark-callable; "a capability declared, never authority granted" | Declare every daemon as **T0** in the contract; TS only through one read-only tool (Closing Brief §6.2) |
| Script board / Observer Proposal C3, D1 | Evidence scrub; periodic off-host anchoring of the chain head; one verified, escaping read library | `spark-daemon verify` on a timer; the activation register records chain heads off-host; a read-only reader built on the verifier and renderer |
| WBS 3.0 writers §6 | The digest goes into a user or tool turn inside delimiters, never a system prompt; consumers check the stamp for staleness | Add to the contract's digest section |
| Observer v0.2 §6.6 / r3 BL3 | Corrections only through a future human-only `ANNOTATION` event; never edit history | Reserve `ANNOTATION` now so no daemon can declare it (PD-41) |
| Closing Brief §6.3 / board R2, C15 | Egress needs a named destination, a threat model, network-layer allowlisting, a rate limit and a byte-exact copy of what was sent | The precondition text for ever un-reserving `network.mode: named` (PD-42) |
| H-Track §5, §8 | An `ExecutionTicket` binds approval to a **digest of the exact proposal** and is re-checked at dispatch, which defeats swap-after-approval | Digest-bound activation: the runtime refuses to start when the manifest or code SHA differs from the approved one (PD-40) |
| H-Track §9 | Every device declares a safe state | Not needed for observe-only daemons; required before `daemon_class: act` is ever considered (PD-01) |
| Kernel review §9 | RATS (RFC 9334) roles; Trust On First Use | Reference points for X1 trust of candidate producers in kernel v2, not now |

## 4. What WBS 3.0E.1 already said about this pattern

E.1 §8, §11 and §14 reviewed this pattern while scoping the Observer's WBS 3.1. It notes that the pattern "is a draft and is not binding on the Observer". Its points, with a reply to each:

| ID | E.1 finding | Reply | Recommendation |
| --- | --- | --- | --- |
| EX-01 | **Exit-code collision (V-12, D-6).** The pattern uses 70 for "uncertain commit", X1 uses 74; X1's 70 is invariant violation; 75 means "post-commit render failure" in X1 but "restart limit exceeded" in the AP-04 draft (S5) | Confirmed: `EXIT_UNCERTAIN_COMMIT = 70` | One shared table, as E.1 recommends: uncertain commit → 74 (restartable), 70 = invariant violation, 75 = post-commit render failure, AP-04's restart limit → 69. A C3 contract change, since `exit_codes` is in the contract (PD-35) |
| EX-02 | PD-21 "pings the watchdog on its own fixed cadence, which would mask a hung main loop" | Not as implemented: every ping comes from the main thread, including those inside the sleep loop, and a hang in `sense()` stops them (the runtime's watchdog-starvation test). Sleep-loop pings are needed because an interval may exceed `WatchdogSec` | Record this reply. Keep E.1's stricter rule for the Observer |
| EX-03 | PD-22 `gc.collect()`: "the claimed benefit did not reproduce" | Accepted: r2's evidence was cost, not benefit | Change PD-22's recommendation from APPROVE to DEFER until RSS data exists from the DGX (PD-36) |
| EX-04 | Lock descriptor must never reach a child (V-16) | Conforms today: `O_CLOEXEC`, `close_fds=True`, no fork | Add E.1's structure test forbidding `close_fds=False`, `pass_fds`, `set_inheritable` and `dup2` on the lock (PD-37) |
| EX-05 | Signal matrix (§14.1): handlers installed first; a stop is latched; recovery completes before exit | **Gap:** the SIGTERM handler is installed after recovery. A SIGTERM during start-up takes the default action and can kill quarantine mid-way (compounds SC-02) | Install handlers first and latch (PD-37) |
| EX-06 | Sense, gate, commit; every durable prefix of a cycle is valid (§9, §14.3) | A stop during `sense()` today still commits that cycle's events. That is harmless for observe-only daemons | Add the gate so both subsystems share one lifecycle (PD-37) |
| EX-07 | Watchdog bound: a total sense deadline checked between steps (§14.6) | **Gap:** only per-command timeouts. `git-watch` could spend 4 × 20 s in one cycle | A per-cycle ctx budget (for example half of `watchdog_seconds`) enforced across `run`/`git` calls (PD-38) |
| EX-08 | Operational log contract (§14.5) | Conforms: stderr only, bounded, exception class never message, collapsed repeats | — |
| EX-09 | `CPUWeight` only with watchdog headroom | Conforms: the `step_timeout` ≤ watchdog/2 rule plus DB-11's ping-gap check | — |

## 5. What the Spark documents confirm the pattern already does well

Worth recording so it is not relitigated: observe-only with no path to action (v0.2 §6.2, H-Track "declared, never authorized"); structure-safe, not prompt-safe rendering (r3 ES5 scope note); every limit and budget stated in bytes and checked; the full chain verified at every start (r3 VR6, A2 defect 1); no checkpoint file (r3 K1); tool versions on every start (Proposal B2); foreign files reported, never touched (Proposal B3); watchdog from the main loop (Proposal C1); inotify reserved as a wake-up only (C2); the `<1%` / 64 MB budget (A2); and the no-authority candidate rule, which is Step 9's "observation is not activation" applied to authorship.

## 6. The FirstBorn package (reviewed separately)

The earlier review of `xarendovich/FirstBorn` found thirteen borrowable items. Three overlap with this cross-check: the traceability map (section 3), a mount guard before writing (it strengthens SC-04), and the activation register (it carries PD-40). The others stand as listed there: cloud-versus-device tiers, `[VERIFY]` IDs, dependency classes, preflight, the soak ladder, the evidence bundle, update and rollback, and committed valid and invalid manifest examples. **Separation:** FirstBorn's own rules forbid the word "spark" in its paths and any read outside its drive. Patterns flow one way only, and nothing links the two projects.

## 7. Decisions for adjudication

Continuing from PD-31. Each item is written in the r3 format this document recommends adopting.

**PD-32. Conform ledger recovery to WBS 3.0 r3 while `ledger.py` remains** (SC-01 no startup `fsync`; SC-02 crash-idempotent quarantine; SC-03 origin-record check; SC-04 artifact safety and identity binding).
Recommendation: APPROVE. Decision: PENDING

**PD-33. Digest semantics** (SC-05). (a) Conform to r3: derive the digest only from committed records, stop the cycle on failure, regenerate at start. (b) Record a deliberate divergence: a daemon digest is a "current observation" view, stamped with the ledger head, with failures non-fatal. Option (b) keeps `digest(snapshot, recent)` meaningful. Option (a) makes a daemon digest provable from the ledger.
Recommendation: MODIFY toward (b): keep the current semantics, name them in the contract, and add the stamp's snapshot-cycle number. Decision: PENDING

**PD-34. Converge on the Observer's writer.** When WBS 3.0B freezes, replace `ledger.py` with `observer_ledger.py` / `observer_writer.py`, leaving a thin adapter for the daemon's envelope, so there is one implementation, as PD-02 intended. Until then PD-32 keeps the rules aligned.
Recommendation: APPROVE. Decision: PENDING

**PD-35. One exit-code table across Spark daemons** (EX-01, E.1 D-6).
Recommendation: APPROVE, with the numbering in EX-01; a KECC-C3 contract change, made once, before any daemon is installed. Decision: PENDING

**PD-36. Revise PD-22** (`gc.collect()` per cycle) from APPROVE to DEFER until DGX RSS data shows a benefit (EX-03).
Recommendation: DEFER. Decision: PENDING

**PD-37. Adopt the WBS 3.1 lifecycle rules** for the daemon runtime: handlers installed first with stops latched; sense, gate, commit; the lock-inheritance structure test (EX-04 to EX-06).
Recommendation: APPROVE. Decision: PENDING

**PD-38. A per-cycle sense budget** across all ctx calls (EX-07).
Recommendation: APPROVE; a KECC-C1 addition. Decision: PENDING

**PD-39. Adopt the house vocabulary**: KECC change classes C0 to C3 for `contract_version`, with a declared support horizon (shrinking it is C3), and the r3 APPROVE/MODIFY/REJECT/DEFER decision format for every PD.
Recommendation: APPROVE. Decision: PENDING

**PD-40. Digest-bound activation** (H-Track ticket pattern): an activation register holds the approved manifest and code SHA-256 per daemon, and the runtime refuses to start (78) on any mismatch. The same register records chain heads for off-host anchoring (Proposal C3).
Recommendation: APPROVE; build it with the FirstBorn-style register. Decision: PENDING

**PD-41. Reserve human-correction event names** (`ANNOTATION`, and any future correction type) so no daemon can declare them.
Recommendation: APPROVE; a KECC-C3 change in form, with no existing daemon affected. Decision: PENDING

**PD-42. Record the egress preconditions now** for any future un-reserving of `network.mode: named` (Closing Brief §6.3, board R2 and C15).
Recommendation: APPROVE (record only). Decision: PENDING

## 8. Recommended order

1. Rule on PD-35 (exit codes) and PD-39 (vocabulary) first. Both change the contract's form, and doing them once avoids two contract versions.
2. PD-32 and PD-37: small, testable, and they close the only reproduced-by-reading durability gaps.
3. PD-40 together with the activation register, before the first daemon is installed on the DGX.
4. PD-34 when WBS 3.0B freezes.
