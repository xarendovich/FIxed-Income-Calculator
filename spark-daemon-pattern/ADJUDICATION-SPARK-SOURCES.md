# Daemon pattern × Spark handoffs — cross-check for adjudication (r3.2)

- **Status:** DRAFT FOR ADJUDICATION, a companion to `README.md`, `HARDENING.md` and `DAEMON-CONTRACT.md`. Three items are implemented because each is a reproduced defect: SX-01 (security, r3.1); SX-02 (a duplicate launch deleted the running instance's files) and SX-03 (a torn first write produced a ledger the battery rejects), both r3.2. Everything else is a recommendation, and PD-32 to PD-45 are PENDING. r3.5 adds the blind period (section 4c, PD-63), implemented at the owner's request.
- **Revision:** r3.2, 2026-09-28, by Claude. r3.2 adds the WBS 3.1 final adjudicated contract (section 4a) and the WBS 3.0 r4 closeout (section 4b), revises PD-34, PD-35 and PD-37 to follow them, and adds PD-43 to PD-45.
- **Question asked:** is there language, contract or architectural design in the other Spark documents, including the completed WBS sections, that the daemon pattern should copy?
- **Short answer:** yes, and more than borrowing. The daemon pattern's ledger and recovery code were drafted from the WBS 3.0 spec **r2**. Since then r3 was adjudicated (2026-09-26) and 3.0C.1 was frozen (2026-09-27), and both changed rules this pattern cites by ID. The Spark handoffs also closed a Git code-execution path that this pattern had not (SX-01, now fixed), and WBS 3.0E.1 had already reviewed this pattern once (section 4).

## Sources

| Source | How read | Relevance |
| --- | --- | --- |
| WBS 3.0 Artifact Writers boundary spec, **r3 adjudicated** (upload) | In full | Normative source for the ledger rules the README cites (CS, LG, VR, RC, DG, ES, FS, FL) |
| WBS 3.0C.1 Durable Ledger Append Boundary, **FROZEN** at `f5ed529` (upload) | In full | The Observer's writer now exists: PD-02's replacement trigger |
| WBS 3.0A.2 secondary reliability review handoff (upload) | In full | Read-side contract closure, torn-tail surface |
| WBS 3.1 Observer Self-Protection, **final adjudicated contract** (upload, 2026-09-28) | In full | Singleton lease, signal contract, sense → gate → commit, exit reasons; its D-6 overrides the EX-01 recommendation |
| WBS 3.0 **r4** Recovery Correction closeout, frozen at `d2785406` (upload, PDF, 2026-09-28) | In full | Pre-baseline recovery; receipt-first quarantine; the Observer's writer and recovery are now frozen, which moves PD-34 |
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

**SX-02. A rejected duplicate launch deleted the running instance's temp files (WBS 3.1 §4.2, reproduced).**
WBS 3.1 §4.2: "A rejected duplicate launch MUST have no repository-observation or artifact-writing side effects." Reading the start-up order against that sentence found a violation. `guard.prepare_output_dir()` ran **before** the lock and deleted every file in `tmp/` plus any `.digest-*` temp in the output directory. It did this as crash cleanup, but a second launch did it too, before it discovered the lock was held, so it removed the files the running instance was writing (the index copy and digest temps). Reproduced: a first instance running, a file placed in its `tmp/`, a second launch exits 73 and the file is gone.
The fix splits the function. `prepare_output_dir()` now only creates and checks directories. `remove_stray_temp_files()` does the cleanup and is called only after the lock is held. The test is `tests/test_hardening.py::RuntimeHardeningTests::test_rejected_duplicate_launch_touches_nothing`: it fails without the fix and passes with it. No contract change: this restores what the README already claimed.
Residue, recorded not fixed: on a first-ever start the unlocked phase still creates the output directory, `quarantine/`, `tmp/` and `daemon.lock`. On a duplicate launch all four already exist, so nothing changes. D-5's objection to creating state just to take the lock is the subject of PD-44.

**SX-03. A torn first-ever write produced a ledger the pattern's own battery rejects (WBS 3.0 r4, reproduced).**
The r4 closeout describes a contradiction in r3. A crash during the very first append leaves bytes but no complete record. Treated as an ordinary torn tail, the recovery record becomes seq 1, which breaks the rule that the ledger starts with its origin record. The daemon pattern had the same defect. Reproduced: with a 41-byte torn first write, the next start wrote `LEDGER_TAIL_QUARANTINED` as seq 1 and `DAEMON_START` as seq 2, and DB-04 then failed that ledger ("first record is not DAEMON_START"). The failure was permanent, since every later run appends to the same ledger. That run's `DAEMON_START` also said `previous_run_ended_cleanly: null` ("no previous run"), though a previous run had started and crashed.
The fix follows r4's adopted order. The origin record stays seq 1 and the recovery record comes after it: `DAEMON_START`, then `LEDGER_TAIL_QUARANTINED`, now for every torn tail, so every run begins with `DAEMON_START`. `previous_run_ended_cleanly` is `false` in this case. The test is `tests/test_hardening.py::RuntimeHardeningTests::test_torn_first_write_still_starts_the_ledger_with_daemon_start`: it fails without the fix and passes with it. No contract change, because the contract does not fix the order of lifecycle records. r4's receipt-first protocol, which makes the quarantine crash-idempotent, is still SC-02's subject and is not adopted here.

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

## 4a. What the WBS 3.1 final contract changes

WBS 3.1 (final, 2026-09-28) freezes the rules E.1 proposed for the Observer's process lifecycle. It binds the Observer only: §10 says "Generic daemon patterns do not control these Observer codes in v0.3", and D-6 declines to renumber the Observer for this pattern. Its rules are still the house design for a long-running Spark process, so this section compares the runtime against each one. The "today" column was checked by reading `runtime.py`, apart from WX-01, which was run.

| ID | WBS 3.1 rule | Daemon pattern today | Recommendation |
| --- | --- | --- | --- |
| WX-01 | §4.2: the lease comes before any artifact mutation; a rejected duplicate launch has no side effects | Violated until r3.2 (SX-02). **Fixed**; only idempotent directory creation stays before the lock | — |
| WX-02 | §4.1, §4.3: `flock` on a descriptor that is close-on-exec, never inherited; no PID files | Conforms: `flock(LOCK_EX|LOCK_NB)`, `O_CLOEXEC`, no PID file, children spawned with `close_fds=True` | E.1's structure test (PD-37) |
| WX-03 | D-5, §4.4: the anchor is a **pre-existing directory** outside the observed repository; its path is checked against the locked inode at every safe point; loss fails closed | The anchor is the file `daemon.lock` inside the output directory, created on demand. It is never re-checked: if the output directory is renamed away and recreated, a second instance can lock a new inode and both run | Lock the output directory itself (it must already exist and pass FS2), and check `(st_dev, st_ino)` of the path against the locked descriptor once per cycle, exiting 78 on a mismatch (PD-44) |
| WX-04 | §5, §3: handlers do nothing but set a plain stop flag and wake the loop; installed before the lock; `SIGTERM`, `SIGINT` **and `SIGHUP`** are equal; `READY` wakes **immediately** | Handlers only set state (conforms). But they are installed after recovery (EX-05); `SIGHUP` is not handled, so it kills the process with the default action; and the sleep loop polls in slices of at most `ping_every`, so a stop waits up to that long instead of waking | Install first, add `SIGHUP`, wake through `signal.set_wakeup_fd` plus `select` (PD-37) |
| WX-05 | §6, §7: sense → gate → commit; a stop seen at the gate discards the intent; every durable prefix of a cycle is valid | No gate (EX-06). Each event is its own append, so every prefix is a valid ledger state already | Add the gate (PD-37); the prefix rule needs no change |
| WX-06 | §8: a total sense deadline checked between bounded steps; the main loop is the only ping source; no heartbeat thread | Main loop only, no thread (conforms). No total deadline (EX-07) | PD-38 as written |
| WX-07 | §11: refuses to run as root (`ROOT_REFUSED`, 78) | No check. The unit generator never runs as root, but a manual `spark-daemon run` as root would start | Refuse `geteuid() == 0` with 78 before the lock; a KECC-C1 addition (PD-43) |
| WX-08 | §3: no synthetic stop event on any shutdown path (`OBSERVER_STOP` is forbidden) | **Deliberate divergence:** the skeleton writes `DAEMON_STOP` with CPU and peak memory, and `DAEMON_START.previous_run_ended_cleanly` is derived from it. It is a lifecycle record, not a repository-history event, which is the class 3.1 forbids | Keep; record as a divergence (PD-45) |
| WX-09 | D-4: pings only after a cycle completes or is safely abandoned | **Deliberate divergence** (EX-02): the sleep loop also pings, because a daemon's interval may exceed `WatchdogSec`. Every ping still comes from the main thread | Keep; record as a divergence (PD-45) |
| WX-10 | §10: symbolic reasons authoritative inside the process; one total boundary function maps them to codes; the mapping need not be one-to-one | Numeric constants returned directly from many places | Adopt the symbolic-reason form; see PD-35 for the numbers |
| WX-11 | §9: operational logs are non-authoritative, bounded, stderr only; logging failure never changes an exit reason | Conforms (EX-08) | — |

## 4b. What the WBS 3.0 r4 closeout changes

r4 (frozen at `d2785406`, 410 tests) corrects WBS 3.0 recovery for the case before the first record exists and closes the Observer's recovery work. Its effect on this pattern follows. RX-01 and RX-04 were run; the rest were checked by reading.

| ID | r4 rule | Daemon pattern | Recommendation |
| --- | --- | --- | --- |
| RX-01 | §2, §5.3: the origin record is seq 1 and the recovery record follows it; recovery never fabricates the origin record | Violated until r3.2 (SX-03). **Fixed** | — |
| RX-02 | §5.2, §5.5: receipt-first preservation. The receipt is durable before the fragment is named; the fragment is hard-linked no-replace; identity, hash and length are revalidated before truncation; a match key of five fields finds the recovery event; exactly one `ftruncate` site | Copy, then truncate, then append later, with no receipt (SC-02) | r4 is the concrete protocol SC-02 asked for. PD-32 should adopt it by reference rather than design its own |
| RX-03 | §5.2: `O_NOFOLLOW` descriptor; `fstat` type, owner, exact 0600, dev, ino, `st_nlink == 1`; bytes read from that same descriptor | Opened by path (SC-04) | Add the `nlink` rule to SC-04; a hard link to the ledger is otherwise undetected |
| RX-04 | §5.6: no ledger, but a digest or quarantine fragment present, means ledger loss and fails closed | Reproduced: with the ledger deleted and a fragment in `quarantine/`, the runtime starts a new chain at seq 1 with `previous_run_ended_cleanly: null` and exits 0 | Refuse (65) when the ledger is absent but quarantine fragments exist, a KECC-C1 addition (PD-32) |
| RX-05 | §9 known limit: deleting all of `history/` still looks like a first run until an off-host commitment exists | The same for the output directory | Already PD-40's off-host chain-head record; r4 confirms it is the only remedy |
| RX-06 | §5.1: the first-run open path creates neither the ledger nor the digest | The ledger is created by the first append (conforms). The digest is rendered after every cycle | Nothing to change |

**PD-34 has triggered.** The Observer's writer (3.0C.1) and its recovery (r4, R4.2C) are both frozen, and r4 found no need for a separate `observer_recovery.py`. The condition PD-34 waited for ("when WBS 3.0B freezes") has in effect been met. Two things still stand in the way of a drop-in replacement. First, the Observer's writer is shaped around `OBSERVER_BASELINE` and its own event types, so the adapter PD-34 describes has to be written. Second, the Observer is on a different host path (`~/spark-governance`), and this pattern must not import from it until someone decides how the code is shared.

## 4c. Blind period (r3.5, implemented at the owner's request)

**Source:** the owner's analysis of the Observer's blind-period rule. As described there: under D-7 the Observer keeps the monotonic time of its last accepted cycle. Unstable samples, repeated gate rejection, Git-output ceilings and parser entry ceilings leave it "alive but blind". Within `T` it keeps pinging the watchdog; beyond `T` it exits `SENSE_BLIND` (78, no restart). `T` is injected deployment configuration, tuned above the longest legitimate unsettled period on the host. **Caveat:** D-7 itself was not among the documents read for this review. The description above is the owner's summary, and the pattern follows it as described.

**What the pattern now does:**
- **`ctx.unsettled(reason)`.** `sense()` returns it when what it read was not stable. The cycle is abandoned before `decide()`, with no event, no error and no new digest. This is the Observer's "abandoned before the gate", expressed as a return value, so daemon code cannot swallow it by accident.
- **Required `blind_limit_seconds`.** Bounded 60 to 86400 and at least 3 poll intervals, with no default, so infinite patience cannot be written down.
- **Counted towards the limit:** unsettled cycles and failed cycles. Start-up counts as accepted, and the clock is monotonic.
- **Not counted:**
  - A commit failure, which already exits 70 at once.
  - A digest failure: the events were committed, so the cycle was accepted.
- **At the limit:** `DAEMON_ERROR` with category `SENSE_BLIND`, carrying `blind_ms`, `limit_ms`, `unsettled_cycles`, `failed_cycles`, `last_cause` and `last_cause_kind`. Then exit 78, and no `DAEMON_STOP`, because it is not a clean stop.
- **Restarts:** the unit never restarts 78 (HF-28; before this, "no restart" was not true for any 78).
- **`git-watch` as the reference use:**
  - A repository that moves between two reads is unsettled (`REPO_CHANGING`).
  - Output over its cap, or a timeout, is a failed cycle rather than a false "unavailable" (HF-29). This is the "Git-output ceiling" cause, which no longer hides from the limit.

**Deliberate choices, for the adjudicator:**
1. **`T` lives in the manifest, not in a unit override.** The analysis calls `T` deployment configuration. In this pattern the manifest *is* the approved deployment configuration: it is hashed into `DAEMON_START`, and PD-40's register binds activation to that hash. Tuning `T` for the DGX means changing the approved manifest, which is visible. An environment override in the unit would be an unhashed knob on a safety limit.
2. **`SENSE_BLIND` is a `DAEMON_ERROR` category, not yet a symbolic exit reason.** The exit number matches the Observer's (78, no restart). The symbolic-reason form arrives with PD-35.
3. **Unsettled cycles leave no record unless the limit trips.** The analysis calls intermediate states "transient state, not events", and the digest's older stamp already shows a consumer that nothing new was accepted. The alternative is one record at the start of each unsettled streak.

**PD-63. Adopt the blind period as specified above** (contract 2.0.0, manifest schema 2). This covers choices 1 and 3, and choice 2 until PD-35 lands.
Recommendation: APPROVE. The code is in place because the owner asked for it; this records the ruling. Decision: PENDING

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

**PD-34. Converge on the Observer's writer.** Replace `ledger.py` with `observer_ledger.py` / `observer_writer.py` from the frozen r4 commit `d2785406`, leaving a thin adapter for the daemon's envelope and origin record, so there is one implementation, as PD-02 intended. r3.2: the trigger has been met (section 4b). What remains is choosing how the two share code, which is a Class C decision about repository layout. Until then PD-32 keeps the rules aligned, by reference to r4 where r4 is more specific.
Recommendation: APPROVE; decide the sharing mechanism first. Decision: PENDING

**PD-35. Exit reasons for this pattern** (EX-01, WX-10; revised in r3.2 after WBS 3.1 D-6).
r3.1 recommended one shared table across Spark daemons. WBS 3.1 D-6 has since ruled MODIFY: the Observer keeps its X1 table locally, and a shared taxonomy "requires demonstrated multi-daemon need and a separate cross-cutting decision". That ruling is the Observer's, and this document does not reopen it. The pattern can still avoid the collision on its own side, with no cross-cutting decision. It would adopt 3.1's symbolic-reason form (WX-10) and, where a reason means the same thing as an Observer reason, use the Observer's number: `UNCERTAIN_COMMIT` 70 → **74**, 70 reserved for `INVARIANT_VIOLATION`, `ROOT_REFUSED` and a lost anchor → 78. The pattern owns this table; it is not a shared one.
Recommendation: MODIFY (was APPROVE of a shared table): align voluntarily as above, as one KECC-C3 contract change made before any daemon is installed. Decision: PENDING

**PD-36. Revise PD-22** (`gc.collect()` per cycle) from APPROVE to DEFER until DGX RSS data shows a benefit (EX-03).
Recommendation: DEFER. Decision: PENDING

**PD-37. Adopt the WBS 3.1 lifecycle rules** for the daemon runtime (EX-04 to EX-06, WX-02, WX-04, WX-05): handlers installed before the lock, with stops latched; `SIGHUP` treated like `SIGTERM`; an immediate wake through a wakeup descriptor rather than polling slices; sense, gate, commit; the lock-inheritance structure test. WBS 3.1 is now final, so these are frozen rules, not E.1 proposals.
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

**PD-43. Refuse to run as root** (WX-07, WBS 3.1 §11): `geteuid() == 0` exits 78 before the lock is taken.
Recommendation: APPROVE; a KECC-C1 addition. Decision: PENDING

**PD-44. Lock anchor and anchor continuity** (WX-03, WBS 3.1 D-5 and §4.4): lock the pre-existing output directory instead of a `daemon.lock` file created on demand, and check the path's identity against the locked descriptor once per cycle, exiting 78 on a mismatch.
Recommendation: APPROVE. Decision: PENDING

**PD-45. Record two deliberate divergences from WBS 3.1** (WX-08 `DAEMON_STOP`; WX-09 sleep-loop pings), each with its reason, so the next review does not find them as defects.
Recommendation: APPROVE (record only). Decision: PENDING

## 8. Recommended order

1. Rule on PD-35 (exit codes) and PD-39 (vocabulary) first. Both change the contract's form, and doing them once avoids two contract versions.
2. PD-32, PD-37, PD-43 and PD-44: small and testable. They close the durability gaps found by reading and bring the runtime in line with the now-final WBS 3.1 lifecycle.
3. PD-40 together with the activation register, before the first daemon is installed on the DGX.
4. PD-34 when WBS 3.0B freezes.
