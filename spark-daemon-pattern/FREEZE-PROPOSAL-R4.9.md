# Freeze proposal: the smallest stable core (r4.9)

- **Status:** PROPOSED, for the owner and for review. It changes no code. It recommends ten decisions (PD-99 to PD-108). Five of them withdraw or shrink work this repository itself recommended earlier; each one says so.
- **Revision:** r4.9, 2026-10-06, by Claude.
- **Asked:** the owner asked to close the seams and invariants within the scope and boundaries found so far, to find assumptions, responsibilities or boundaries that are misplaced or more complex than they need to be, and to push for the smallest stable design that can be frozen without future architectural debt.
- **Evidence:** every claim is **traced** to code or **measured** here (one new probe, `evidence/r4.9/ledger_growth_probe.txt`), unless it says **opinion**.

## 1. Answer in brief

**Freeze a class-1a observer core, and nothing else.** The design has grown to about 100 pending decisions and 23 seams. Most of that comes from planning for active classes, Break Glass, the transport contract and SQLite ledgers. None of these exists, and none needs to change the observer core when it arrives, *provided the core's four contracts are fixed now and extensions may only consume them*.

**The frozen core is four contracts plus one activation chain:**

| Contract | What is frozen | Already built? |
| --- | --- | --- |
| **Manifest** | The closed schema, with one new rule: no denied path inside a granted read (§3.1) | Yes, plus the rule |
| **Author API** | `sense(ctx)`, `decide(prev, snapshot)`, `digest(...)`; `ctx` read-only; truncation always raises | Yes, plus PD-83 |
| **Ledger** | The record format, the hash chain, the lifecycle records and their meanings; a reader identifies its position by `seq` | Yes |
| **Battery verdict** | The report's `result` field and the final `RESULT:` line, bound to the manifest and code digests and to host facts | Yes, plus host facts |
| **Activation chain** | A battery PASS on the host → a unit that pins those digests → a runtime that refuses anything else | Partly: the unit records the manifest digest in a comment and enforces nothing |

**Three boundaries are drawn in the wrong place today:**
1. **Policy is enforced twice, and the copies differ.** Python path checks, the audit hook, Landlock and the unit's `InaccessiblePaths=` all enforce file policy. They disagree in exactly one case: a denied path inside a granted read (a "gap"). That one case created F-1 and its planned fix. **Forbid gaps, and the kernel alone expresses the whole policy** (§3.1).
2. **Consumers re-derive the ledger's meaning.** The pilot's notifier reads the ledger without verifying it, works out "restarted" from a payload flag, and tracks a byte offset. That knowledge belongs to the pattern. **Ship one independent reader and one `status` command, and require consumers to use them** (§3.5).
3. **Activation lives in a register that does not exist.** Meanwhile, the pilot has automated activation with no digest check at all. **Make the generated unit the activation record** (§3.4).

**Eight planned items should be dropped, deferred or left unfrozen** (§4). The five to drop outright: the exit-code split, the battery exit-code change (C-1), the `pacing` field, the secret-name heuristic, and the separate Phase 1 notifier and staleness checker. Each adds surface and a seam, and the core is safe without it.

**What remains before a freeze is about nine small pieces of work** (§6), and one owner ruling, PD-76's wording for DB-19.

## 2. What the freeze must not break

Every proposal below was checked against these. A proposal that breaks one is not in this document.

| Must hold | Source | Status under this proposal |
| --- | --- | --- |
| Blind period: no accepted cycle within `T` → `SENSE_BLIND`, exit 78, never restarted | PD-63 (owner) | Kept unchanged |
| Blindness survives restarts and clock steps | PD-70 (owner), HF-32, HF-34 | Kept unchanged. Examined for simplification; see §5 |
| Class 1a fails closed; the Blind Forester governs survival mode only in active classes | PD-01.9 (owner) | Unaffected: active classes are outside the core |
| A direct cgroup memory reading decides the memory bound | PD-76 (owner; wording OWNER) | Kept: DB-19 is in the freeze cut |
| Break Glass is never used for internal systems, only to reach external systems | Owner, Break Glass | Strengthened: the core has no inbound or outbound channel at all |
| "An AI model cannot unilaterally decide that an emergency exists" | Owner, Break Glass | Unaffected: no authority exists in the core |
| What the pilot already relies on: `ledger.jsonl` and its record format, the battery's `RESULT:` line, `unit --require-path` and `--part-of` | Pilot deployment | Kept. One deliberate change: unit generation requires a PASS report (§3.4) |
| S-1 to S-4 and S-8 | `PD-01-DAEMON-CLASSES.md` §2 | Each becomes a core invariant with a named check (§7) |

## 3. Boundaries to move or remove

### 3.1 One policy, expressed once, by the kernel (PD-100)

**Today.** A read is checked four times:
- by `guard.Policy.readable` in `ctx`;
- by the audit hook;
- by Landlock;
- under systemd, by `InaccessiblePaths=`.

Landlock can grant a directory but cannot carve a denied subtree out of it. So for a gap (a denied path inside a granted read), only the Python checks and `InaccessiblePaths=` stand in the way. F-1 is exactly that window: a directory swapped for a symlink between the Python check and the open (simulated, `evidence/r4.6/read_race_simulation.txt`). The planned fix is a check after opening, designed around the audit hook.

**Proposal.** Manifest validation refuses any denied path, base or declared, that lies inside a declared read. Then:
- Landlock's grant is exactly the policy: the declared reads, plus system code (`/usr`, `/lib`, `/lib64`, `/etc/ld.so.cache`, the interpreter's prefix) and the daemon's own folder (traced: `landlock.system_read_paths`, `runtime.py`);
- **F-1 closes by construction.** A swapped symlink can only lead to a path the kernel already refuses;
- the `deny` list stops being an enforcement mechanism and becomes a **validation constraint**: "no read may cover these";
- `landlock.gaps()` returns `[]` for every valid manifest. `DAEMON_START.landlock.gaps` and DB-18 stay, as a check that this remains true.

**What it breaks.** Nothing shipped: no reference daemon, test fixture or pilot manifest has a gap (traced: `landlock.gaps()` over every shipped manifest). A daemon wanting "this repository except that folder" must declare narrower reads. That is the honest form of the policy anyway.

**Work it removes:** the F-1 post-open check, and `O_NOFOLLOW` on output-directory opens (PD-98). The output directory is 0700 and owned by the daemon's user, so only that user could plant a link there, and that user can already write the ledger.

### 3.2 The kernel is the safety boundary; purity and the audit hook are diagnostics (PD-101)

**Today.** S-4 lists four confinement layers as if they were equal: the purity check, the audit hook, Landlock and the systemd sandbox. The first two run inside the process they police. A daemon that escapes Python's rules (residual risk 2 in `HARDENING.md`) bypasses both.

**Proposal.** S-4 is restated as **INV-1** (§7). The safety claim rests on Landlock (including execute-denial, HF-36) and the systemd sandbox. The purity check and the audit hook stay, unchanged, as **author diagnostics and a tripwire**. They fail fast with a readable reason, they record `POLICY_VIOLATION`, and they exit 78. A gap in them becomes a diagnostic defect, not a safety defect. DB-17 already proves that the kernel blocks without them.

**Why it matters for a freeze.** Reviewers stop treating every new Python escape route as a breach of the boundary. The safety case shrinks to what the kernel enforces.

### 3.3 Exit codes say how to restart; the ledger says why (PD-102)

**Today.** Exit 78 means both "policy violation" and `SENSE_BLIND`. r4.5 adopted a split into two codes (an amendment to PD-63), which created seam N-12: four consumers of one table.

**Proposal: withdraw the split.** An exit code only needs to select a restart class:

| Class | Codes | Unit behaviour |
| --- | --- | --- |
| Clean | 0 | Stopped |
| Needs a person | 2, 65, 73, 78 | Never restarted (`RestartPreventExitStatus=`, generated from `NO_RESTART_EXIT_CODES`) |
| Retry | 70, crash or signal | Restarted, within the start limit |

The reason is already recorded as a symbolic `DAEMON_ERROR.category` (`SENSE_BLIND`, `POLICY_VIOLATION`, ...) before the exit. That meets PD-35's "symbolic exit reasons" without touching the numbers. The pilot's notifier already reads the reason this way. N-12 closes: there is one table, already single-sourced in `unitgen.py`.

*This withdraws a recommendation this repository made (the PD-63 split, r4.5 §3.1).*

### 3.4 The unit is the activation record (PD-104)

**Today:**
- PD-40 plans a register that holds approved digests, with the runtime refusing a mismatch. It is not built.
- PD-50 says there is no zero-touch activation.
- The pilot runs the battery on the host and installs on PASS, on every update. That is reasonable, but nothing checks that the code the unit runs at its next start is the code the battery judged (N-20).
- `DAEMON_START` records `manifest_sha256` and `daemon_code_sha256`, and nothing compares them with anything.

**Proposal.** Make the chain mechanical, with no new component:
1. **The battery report** already binds the canonical-manifest digest and the code file's digest. It gains **host facts**: kernel release, machine, Landlock ABI, cgroup version, systemd version and Python version. Some of these are in `environment` today.
2. **`unit --report PATH`** refuses unless the report says PASS, its digests match the files now on disk, and its host facts match this host. It writes the digests into `ExecStart=` as `--expect-manifest-sha256` and `--expect-code-sha256`.
3. **The runtime** computes both digests at start, as it already does. On a mismatch it records `DAEMON_ERROR ACTIVATION_MISMATCH` and exits 78 (never restarted).
4. **The list of activated daemons lives in version control**, reviewed like any change. The pilot's `OBSERVERS` list in its installer is already exactly this.

**What this closes:** N-20 (code reaching the host by another path cannot run), G-5 (a PASS cannot be reused on a different host), and N-2's question of which digest (both: the canonical manifest and the code file's bytes). PD-50 is met in its "guided" form: a person starts the run, a reviewed list names the daemon, the battery decides, and the unit enforces the result.

**What is left of PD-40:** revocation is disabling the unit, and the history is the version-control log. A register for many daemons, evidence validity periods and off-host chain anchoring become an extension for higher classes.

**Deliberate break:** the pilot's installer must pass `--report` once it adopts contract 4.0.0.

### 3.5 One independent reader, one interpretation (PD-105)

**Today:**
- Four planned or existing parts each read the ledger in their own way:
  - the skeleton's start-up recovery;
  - DB-04 and DB-16, using the skeleton's own verifier, which is not independent (G-2);
  - a planned Phase 1 staleness checker that must *not* import the skeleton (U-7);
  - the pilot's notifier.
- The notifier re-implements pattern knowledge without verifying the chain:
  - "restarted" means a `DAEMON_START` with `previous_run_ended_cleanly: false`;
  - its position is a byte offset;
  - the stop reason comes from the last `DAEMON_ERROR`.

**Proposal:**
- **`ledger_reader.py`:** a single standard-library file of about 150 lines that does **not** import `spark_daemon`. It is written separately from `ledger.py`, so the writer and the reader are diverse implementations of one format. It verifies the chain and returns records with their `seq`. Consumers in other projects **copy** it (it is a file, not a dependency), which keeps the no-link rule.
- **`status --manifest M [--json]`**, built on that reader. It derives the daemon's state from the ledger, and is the one place that interpretation lives:
  - `observing` / `quiet` (heartbeats arriving, with or without events);
  - `blind` (in a blind streak);
  - `stopped: <reason>` (from the last `DAEMON_ERROR`, or clean);
  - `not watching` (no heartbeat within `T`, measured on `CLOCK_BOOTTIME` when the last record is from this boot, which no wall-clock step moves);
  - `never started`;
  - plus coverage: every `DAEMON_STOP` → `DAEMON_START` gap is reported as **not watching**, never as quiet.
- **DB-22** compares this reader with the skeleton's verifier over the battery's ledgers.
- **Consumer rule:** anything that acts on a ledger, including a notice, uses `status` or the reader. Anything else is a hint and is labelled one.

**What this closes:**
- N-19 (unverified consumer);
- N-21 (a stop gap read as quiet);
- N-22 (a unit skipped by its condition: `status`, run by a timer that has no such condition, reports `not watching`);
- N-5 (staleness on the wall clock: within a boot it uses `CLOCK_BOOTTIME`, which every process shares);
- G-2 (verifier independence).

**What it replaces:** Phase 1's `OnFailure=` notifier and its separate staleness checker. A timer that runs `status` and alerts on any state other than `observing` or `quiet` does both jobs. *This shrinks Phase 1 as this repository recommended it.*

### 3.6 Ledger growth: fix the rule now, build when measured (PD-106)

**Measured** (`evidence/r4.9/ledger_growth_probe.txt`, this workspace, Python 3.11): start-up verification is linear, at about 28,000 heartbeat-sized records per second:

| Records | Size | Start-up verification |
| --- | --- | --- |
| 10,000 | 5.8 MB | 0.38 s |
| 50,000 | 28.9 MB | 1.51 s |
| 200,000 | 115.9 MB | 7.05 s |

The unit allows `TimeoutStartSec=max(60, watchdog_seconds)`. A daemon writing about 200 records a day (pressure-watch's heartbeats and a few events) reaches 60 s after about 20 years. A daemon writing one record every 5-second cycle would reach it in about three months. The DGX's speed is not measured. Start-up time therefore grows without bound (U-6, PD-67), and segmenting the ledger later would break any consumer that tracks byte offsets, as the pilot's notifier does.

**Proposal: freeze the rule now; build it when a projection calls for it:**
- A reader identifies its position by `seq`, never by byte offset (`status` and the reader do).
- When segmentation is built, the chain continues across files. A full segment is renamed `ledger-<first seq>.jsonl` and is read-only. `ledger.jsonl` stays the current segment, and its first record's `prev_sha256` is the sealed segment's head. Start-up verifies only the current segment and that one link. `verify --all` checks everything.
- DB-21's budget table projects start-up verification time at the daemon's measured record rate. It flags at half the start timeout, which is the trigger for building segmentation.

The record format does not change, so the ledger schema stays `spark-daemon-ledger/1`.

## 4. Planned work to drop

| Item | Was | Why drop it | Replaced by |
| --- | --- | --- | --- |
| Exit-code split (PD-63 amendment) | Adopted r4.5, contract 4.0.0 | The reason is already in the ledger; the split only creates a fourth table to keep in sync (N-12) | §3.3 |
| Battery exit codes 0/3/4 (C-1) | Adopted r4.5, contract 4.0.0 | Changes meanings to match another line's registry that has not been imported. Consumers read `RESULT:`, as the pilot's installer does | Freeze `result` and `RESULT:`; the exit code is only 0 or non-zero (PD-103) |
| `pacing` manifest field (PD-85) | Adopted with modification r4.5 | No measured phase collision; the default stays jitter anyway; adds a field, three modes and seam N-11 | Internal jitter, not part of the contract |
| Secret-shaped names refused by `ctx` (part of PD-96) | Adopted with modification r4.6 | A name heuristic gives false refusals and false comfort. Under §3.1, credential stores are denied by path and no read may cover them | Credential-store paths in `BASE_DENY` (kept) |
| F-1 post-open check; `O_NOFOLLOW` on output opens (PD-98) | Planned next | Closed by construction (§3.1); output opens add nothing under 0700 ownership | §3.1 |
| Phase 1 `OnFailure=` notifier and separate staleness checker | Adopted with modification r4.5 | Two new components, each reading the ledger its own way | `status` plus a timer (§3.5) |
| Consequence field (PD-01.2) in 4.0.0 | Adopted | Meaningful only above class 1a; an optional field with a default can be added later in a minor version | Deferred to the first active class |
| Candidate envelope as a frozen contract (`spark-daemon-candidate/1`) | Built | Authoring tooling, not a safety contract. Freezing it adds surface for no protection: the battery binds the digests anyway | Kept as unfrozen tooling, like `scaffold` |

## 5. Examined and kept as they are

Simplicity is not the only test. These were examined, and each part of them is load-bearing:
- **The blind clock** (monotonic in-process; `CLOCK_BOOTTIME` within a boot; the wall clock across a reboot when it moved forward; otherwise worst case). Each branch is a reproduced defect: HF-28, HF-32 and HF-34. Removing the wall-clock branch would make every reboot cost a daemon its whole blind allowance; removing the boot-time branch brings HF-34 back.
- **Heartbeats every `T`/2.** They are the only way the ledger can say "watching, nothing changed" (S-8, T-6).
- **Torn-tail quarantine, one fsync per record, refuse to start on a corrupt chain.** Each comes from a reproduced defect or a stated durability rule.
- **The digest.** It is a view, never authoritative, and its failure never touches the ledger.
- **`--require-path` alongside `--part-of`.** The pilot's stack unit is never started at boot and carries its own `ConditionPathExists=`. But systemd checks conditions when a job runs, not when dependent units are queued, so a wanted unit is still started when the stack is skipped. Without the daemon's own condition, that case would be a loud stop with a false alarm. The silence it leaves is closed by `status` (§3.5).

## 6. The freeze cut: what remains, in order

| # | Work | Contract change | Size | Closes |
| --- | --- | --- | --- | --- |
| 1 | No-gaps validation rule (PD-100) | 4.0.0 (stricter validation) | Small | F-1, the gap seam |
| 2 | Truncation always raises in `ctx.run`, `ctx.git`, `ctx.list_dir` (PD-83) | 4.0.0 | Small | N-10, an S-2 footgun |
| 3 | Credential-store paths in `BASE_DENY` (PD-96, path part) | 4.0.0 (data) | Small | — |
| 4 | Host facts in the battery report; `unit --report`; runtime `--expect-*` (PD-104) | 4.0.0 (unit generation needs a report) | Medium | N-2, N-20, G-5 |
| 5 | `ledger_reader.py`, `status`, DB-22 (PD-105) | Additive | Medium | N-5, N-19, N-21, N-22, G-2 |
| 6 | DB-19, a direct cgroup memory peak, with its helper and the CI rule for INCOMPLETE | Battery check | Medium | G-1, N-4. **Needs the OWNER's PD-76 wording** |
| 7 | DB-21, a budget table, including the start-up verification projection (PD-69, PD-86, PD-106) | Battery check | Medium | G-3, U-6 |
| 8 | A schema-3 migration hint, the contract 4.0.0 release and a tag | Release | Small | N-13 |
| 9 | The first host report from the pilot: `DAEMON_START.landlock.status == "enforced"` under systemd as PID 1 | None | Owner, on the host | G-6 |

**Then freeze: core 1.0.** After that, a change to a frozen contract is a major version and needs the owner. Extensions add capabilities beside the core.

**Explicitly not before the freeze, and needing no core change later:**
- **PD-72 supervisor/worker split.** It is internal. The author API and the ledger stay as they are, and the supervisor keeps the ledger (PD-97).
- **PD-01.3 Sentinel.** `SENSE_DEGRADED` is a new reserved record type: additive, a minor version.
- **Above class 1a:** the PD-40 multi-daemon register, PD-01.7, Break Glass, the Blind Forester and the LTC bindings (PD-82 to PD-86). The core has no channel for them to change; they arrive as separate processes or as additive manifest fields that a 1a daemon may not use.
- **The SQLite ledgers (PD-87 to PD-92).** They belong to the kernel's memory store, not to this pattern; PD-88 already says they follow the same chain rules.
- **WASI (PD-73, PD-74)** and **G-8**, a power-loss simulation, which is accepted as a stated limit, with DB-08 covering fsync errors.

## 7. Core invariants (frozen), each with its check

The core inherits a long list of identifier families (S, U, BM, BF, BG, OR, LI, A, N, G, T). For the frozen core they reduce to nine invariants. An invariant with no check is not frozen.

| ID | Invariant | Sources | Enforced by |
| --- | --- | --- | --- |
| **INV-1** | Observe only: no network; writes only in `output_dir`; execution only of allowlisted system commands. **The kernel enforces it** | S-4 (restated), HF-36, PD-95 | Landlock and the unit; DB-15, DB-17; `test_landlock.py` |
| **INV-2** | The policy is fully expressible by the kernel: no denied path inside a granted read | New (PD-100) | Manifest validation; DB-18 (`gaps == []`) |
| **INV-3** | Never record a guess: only accepted cycles produce events; truncated or unsettled reads raise or abandon the cycle | S-2, PD-63, PD-83, HF-29, HF-33 | `ctx`; T-TR1; `test_blind.py` |
| **INV-4** | Blindness surfaces within `T`, across restarts and clock steps, and the unit never restarts it | S-1, S-3, PD-63, PD-70, HF-28, HF-32, HF-34 | Runtime; DB-20; `test_blind.py` |
| **INV-5** | The ledger is append-only, hash-chained and canonical, with one fsync per record. Corruption means refusing to start and changing nothing. Positions are by `seq` | PD-88, PD-106, HF-26 | `ledger.py`; DB-04 to DB-07, DB-16, DB-22 |
| **INV-6** | Time is honest: no gap is quiet. A stop-to-start interval, or a missing heartbeat, is reported as not watching | S-8, U-4, T-6 | Heartbeats; `status`; DB-20 |
| **INV-7** | It runs only what was judged: the unit pins the digests of a PASS on this host, and the runtime refuses anything else | PD-40 (core part), PD-50, PD-104 | `unit --report`; runtime; a new test |
| **INV-8** | Restart follows the exit class; the reason lives in the ledger | PD-63, PD-35, PD-102, HF-28 | `NO_RESTART_EXIT_CODES` → the unit; `test_fail_closed_exits_are_never_restarted` |
| **INV-9** | Bounded: the limits are checked against each other; memory is measured on the cgroup; start-up stays within its timeout as the ledger grows | U-6, U-8, PD-69, PD-76, PD-106 | DB-14 → DB-19; DB-21 |

## 8. Every seam, closed, moved or removed

| Seam | Outcome | How |
| --- | --- | --- |
| N-1 supervisor/worker | **Moved** | Extension; internal split later, no contract change |
| N-2 register binding | **Closed** | PD-104: canonical manifest and code file digests, both pinned |
| N-3 declared connection and LTC | **Moved** | No connections in the core |
| N-4 battery and cgroup | **Open, in the cut** | DB-19 (item 6) |
| N-5 staleness on the wall clock | **Closed** | `status` on `CLOCK_BOOTTIME` within a boot |
| N-6 inbound acknowledgement | **Removed from the core** | The core has no inbound channel |
| N-7 to N-9 relay, witness, recorder | **Moved** | Break Glass extension |
| N-10 partial result loses its flag | **Removed** | PD-83 (r4.5) |
| N-11 pacing and watchdog | **Removed** | No `pacing` field |
| N-12 exit table | **Closed** | Three classes, one table (PD-102) |
| N-13 4.0.0 migration | **Closed by the cut** | Small batch plus a hint (item 8) |
| N-14 survival mode and authority | **Moved** | Neither exists in the core |
| N-15 to N-18 SQLite | **Moved** | Kernel memory store |
| N-19 unverified consumer | **Closed** | Reader and consumer rule (PD-105) |
| N-20 automated activation | **Closed** | PD-104 |
| N-21 stop gap read as quiet | **Closed** | `status` coverage |
| N-22 condition skip is silent | **Closed** | `status` run by a timer without the condition |
| N-23 two copies | **Closed by direction** | PD-108 |

## 9. Decisions

| ID | Decision | Recommendation |
| --- | --- | --- |
| **PD-99** | **Freeze scope.** The frozen core is the class-1a observer: four contracts and the activation chain (§1). Extensions may consume them and may add record types or optional fields in minor versions. They may never change an existing meaning | ADOPT |
| **PD-100** | **No gaps.** No denied path inside a granted read. Withdraws the F-1 post-open fix and PD-98's `O_NOFOLLOW` work | ADOPT |
| **PD-101** | **The kernel is the boundary.** S-4 restated as INV-1; purity and the audit hook are diagnostics and a tripwire | ADOPT |
| **PD-102** | **Exit codes encode restart class only.** Withdraws the PD-63 exit-split amendment; PD-35 is met by ledger categories | ADOPT; the owner may wish to confirm, since it touches an amendment to the owner's ruling |
| **PD-103** | **The battery verdict contract** is the report's `result` and the `RESULT:` line; the exit code is only 0 or non-zero. Withdraws C-1 | ADOPT |
| **PD-104** | **The unit is the activation record:** `unit --report`, runtime `--expect-*`, host facts. The activation list lives in version control. Replaces PD-40's core part; meets PD-50 in its guided form | ADOPT |
| **PD-105** | **One independent reader and one `status` command; consumers use them.** Replaces Phase 1's notifier and staleness checker; DB-22 uses the reader | ADOPT |
| **PD-106** | **Ledger positions by `seq`; the segmentation rule is fixed now and built at DB-21's trigger** | ADOPT |
| **PD-107** | **Contract 4.0.0 shrinks to** PD-83, PD-100, the credential-store paths, PD-104 and a migration hint. Defers `pacing`, the consequence field and the secret-name heuristic | ADOPT |
| **PD-108** | **The core flows one way.** Changes land in this repository first; a vendored copy is regenerated by a mechanical rename and records the core version it came from. A local change in a copy is upstreamed before the next re-vendor | ADOPT |

**For the pilot (not this repository's to change; for its owner):**
- **After 4.0.0:**
  - pass `--report` to `unit`;
  - replace the notifier's ledger parsing with a copy of `ledger_reader.py` and `status`, which also removes its byte-offset dependency.
- **Until then:** its notifier should treat what it reads as a hint (N-19).
