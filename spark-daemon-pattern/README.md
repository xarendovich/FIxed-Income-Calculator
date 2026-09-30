# Spark daemon pattern — v0.3 (r3) draft for review

- **Status:** DRAFT FOR ADJUDICATION. Nothing here is approved, installed or running anywhere. r3 is implemented and self-tested: 170 of 170 tests pass on Python 3.10, 3.11, 3.12 and 3.13, and all four reference daemons pass 18 of 18 battery checks on this workspace's kernel (Landlock ABI 7). That is evidence for the human's Class C ruling, not the ruling itself. Every PD (PD-01 to PD-31) is still PENDING until recorded in the decision log.
- **r3.1 (2026-09-28):** cross-checked against the Spark handoffs (WBS 3.0 r3, 3.0C.1, 3.0A.2, 3.0E.1, WBS 2.5, Observer v0.2, the script board, H-Track, Kernel v0.2 Stage A). One Git code-execution path closed (HARDENING.md HF-24, contract 1.0.1); conformance gaps against WBS 3.0 r3 and PD-32 to PD-42 in `ADJUDICATION-SPARK-SOURCES.md`.
- **r3.5 (2026-09-29):** a blind-period limit. A daemon that sees nothing it can accept (unsettled or failed cycles) for `blind_limit_seconds` stops with `SENSE_BLIND` (78) for a human, instead of pinging the watchdog over an empty ledger. The unit no longer restarts fail-closed exits (HF-28). Contract 2.0.0, manifest schema 2.
- **r3.3 (2026-09-29):** reviewed the proposed plug-and-play contract stack (L0 component envelope and layers) against this pattern (`ADJUDICATION-PLUG-AND-PLAY.md`). The pattern is already a working L0 instance for resident components. The battery report now names the code it tested (HF-27). PD-46 to PD-52.
- **r3 in one line:** a review that reproduced and fixed 22 defects, including two confinement escapes (`HARDENING.md`), plus a published, versioned daemon contract and a candidate handoff that carries no authority (`DAEMON-CONTRACT.md`).
- **Revision:** r3, 2026-09-27 (see the revision history; r2 below for context). r2, 2026-09-27, drafted by Claude from Observer v0.3, the WBS 3.0 spec (r2), the Observer Improvement Proposal and the Spark Script Repository board. r2 adjudicates four external proposals (`ADJUDICATION-AP.md`, PD-15 to PD-20) plus two further ones submitted the same day (polling jitter and per-cycle GC forcing, PD-21 to PD-22), and implements the parts with a clear draft verdict: Landlock (AP-01), JCS key ordering (AP-03/J1), the 64 MB memory floor (IF-01/PD-20), polling jitter and GC forcing. The out-of-process supervisor/worker split (AP-04, S1-S7) stays a design only in `ADJUDICATION-AP.md` — none of it is built yet.
- **Adjudicated by:**
- **Adjudicated on:**

A daemon built from this pattern has three parts. A **manifest** declares everything about its safety in a closed schema. A fixed **skeleton** supplies every safety mechanism, so the daemon's author writes only what to observe. A **conformance battery** runs the real daemon through eighteen checks before anyone may activate it. v1 admits only daemons that observe and record; nothing in this package installs, enables or starts a service, and `spark-new daemon` is deliberately not built yet.

Whoever writes a daemon, whether a person, a script or a model, builds against one published **contract** (`spark-daemon describe`, `contract/`) and hands back a **candidate** (`manifest.json`, `daemon.py`, `candidate.json`). A candidate carries no authority: validate and precheck are fast feedback, the battery's PASS is the only admissible evidence, and activation stays a human Class C decision (section 4, `DAEMON-CONTRACT.md`).

## What is in the folder

```text
spark-daemon-pattern/
  ADJUDICATION-AP.md        draft adjudication of external proposals AP-01..AP-04 (r2)
  HARDENING.md              r3 review: 22 reproduced defects, fixes, tests, residual risks
  DAEMON-CONTRACT.md        r3 handoff design: contract, candidate envelope, evidence lanes, PD-23..PD-31
  ADJUDICATION-SPARK-SOURCES.md  r3.1/r3.2 cross-check against the Spark handoffs (incl. WBS 3.1, 3.0 r4): PD-32..PD-45
  ADJUDICATION-PLUG-AND-PLAY.md  r3.3-r3.8 plug-and-play stack, framework/services boundary, kernel v2 agenda,
                            universal-contract alignment and U-1..U-7: PD-46..PD-68
  PD-01-DAEMON-CLASSES.md   r3.6 PD-01 reopened: daemon classes, consequence levels, safety invariants (open for expansion)
  HARDENING-REVIEW-R4.6.md  r4.6 review of pre-execution screening and runtime boundaries: HF-36, F-1, PD-93..PD-98
  REVIEW-PACKAGE-R4.5.md    r4.5 the three r4.5 documents in one, for secondary reviewers (PDF in handoff/)
  ADJUDICATION-R4.5.md      r4.5 adjudication of the r4.2-r4.4 improvements (delegated by the owner); HF-34, HF-35
  BATTERY-REVIEW-R4.5.md    r4.5 conformance battery review, code map, gaps, planned checks, full self-test list
  SQLITE-LEDGER-REVIEW.md   r4.5 review of the three-ledger SQLite store, systemd.path wake and UDS IPC: PD-87..PD-92
  REVIEW-PACKAGE-COMBINED.md r4.4 the handoff and the decision brief in one document, led by a reviewer focus:
                            what each decision has built and still needs, and new seams N-1..N-14
  DECISION-BRIEF.md         r4.4 handoff: the settled rulings revisited (keep all; one amendment or clarification each),
                            and every open choice with options, pros, cons and a recommendation
  HANDOFF-REVIEW-PACKAGE.md r4.3 self-contained handoff for independent reviewers: packages A (LTC), B (Break Glass),
                            C (other open decisions), owner rulings, markup template
  handoff/                  the handoff and the decision brief rendered as PDF (the Markdown files are the source)
  LTC-INTEGRATION-REVIEW.md r4.3 the Local Transport Contract and the pattern: bind, do not embed; LI-1..LI-14,
                            markup on LTC-H01..H03, HF-33: PD-82..PD-86
  BREAK-GLASS.md            r4.2 Break Glass for external connections only; epochs, witness, flight recorder,
                            operator-gated recovery (SAFE_TO_ISOLATE, REJOIN_PROBATION): PD-01.10..PD-01.14
  PRIOR-ART-REVIEW.md       r3.7 review of robotics, industrial, flight, automotive, operations and AI-agent practice: A-1..A-12
  RECONCILIATION-V01-LINE.md  r4.1 the parallel v0.1 line: PD number alias (PD-75..PD-81), owner ruling PD-76, conflicts
  sources/v0.1-line/        that line's README r3, AP adjudication r3 and battery review v0.1.2, verbatim
  INTEGRATION-REVIEW.md     r3.9/r4.0 limits in conflict (HF-28..HF-32), DB-14 comment review, blind modes, integration plan,
                            owner rulings (PD-01.9, PD-70), hardening review: PD-69..PD-74
  Makefile                  test | contract | check-contract | validate/precheck/battery-examples | evidence
  bin/spark-daemon          entry point; works under python3 -I -B (isolated, no bytecode)
  contract/                 generated, never hand-edited: daemon-contract.json, manifest.schema.json,
                            versions.json (append-only contract_version -> contract_sha256)
  spark_daemon/
    contract.py             the daemon contract and the manifest JSON Schema, from the enforced rules (r3)
    handoff.py              candidate envelopes, validate --json, precheck (r3)
    scaffold.py             a blank page that passes the battery (r3)
    manifest.py             the closed-schema manifest and its validator
    runtime.py              the skeleton: start-up order, cycle loop, lifecycle events
    ledger.py               append-only hash-chained ledger, recovery, torn-tail quarantine
    canonical.py            canonical JSON and hashing
    render.py               containment renderer and digest (WBS 3.0 ES5)
    guard.py                path policy, output-directory checks, in-process audit hook
    landlock.py             kernel-enforced confinement via the Landlock LSM (ctypes, r2, AP-01)
    context.py              the read-only ctx object: the daemon's only I/O
    proc.py                 hardened, bounded subprocess and Git helper
    notify.py               systemd READY / WATCHDOG / STOPPING notifications
    purity.py               static check of the daemon's code before import
    unitgen.py              sandboxed systemd unit and human install plan (text only)
    battery.py, probes.py   the conformance battery and its child-process probes
    cli.py                  describe | schema | scaffold | envelope | validate | precheck |
                            battery | unit | run | verify (+ internal probes)
  examples/                 four reference daemons, one idiom each (DAEMON-CONTRACT.md section 7):
    meminfo-watch/          GB10 unified-memory bands from /proc
    disk-watch/             free-space bands on the model-weights filesystem
    dir-watch/              inventory diff of a drop folder (names, sizes, mtimes)
    git-watch/              refs, HEAD and worktree state via read-only ctx.git (bare-repo fixture)
  tests/                    170 self-tests and 7 fixture daemons, some deliberately bad
  evidence/                 r2 battery report and self-test output; evidence/r3/ holds the r3 runs;
                            evidence/r3.8/ the start-up verification benchmark (U-6); evidence/r3.9/ the restart-loop reproduction (HF-32); evidence/r4.0/ the same after the fix; evidence/r4.3/ the dir-watch capped-listing reproduction (HF-33), before and after.
                            (r2 also listed evidence/ap/, which was not in the uploaded zip: HF-23)
```

Requirements: Linux, Python 3.10 or later (tested on 3.10, 3.11, 3.12.3, the DGX venv version, and 3.13), Git 2.31 or later. Standard library only; `jsonschema` is an optional test dependency for the schema-agreement tests. Optional: `strace` for the two fault-injection checks, `systemd-analyze` for the unit score; without them those checks report UNKNOWN, never PASS.

## Try it

```bash
cd spark-daemon-pattern
python3 -I -B bin/spark-daemon validate --manifest examples/meminfo-watch/manifest.json
python3 -I -B bin/spark-daemon unit --manifest examples/meminfo-watch/manifest.json          # prints the unit
python3 -I -B bin/spark-daemon unit --plan --manifest examples/meminfo-watch/manifest.json   # prints the install plan
python3 -I -B bin/spark-daemon battery --manifest examples/meminfo-watch/manifest.json       # ~15 s, disposable workspace
python3 -B -m unittest discover -s tests -t tests                                             # ~65 s (or: make test)

# Authoring and handoff (r3)
python3 -I -B bin/spark-daemon describe --identity                                            # contract_version, contract_sha256
python3 -I -B bin/spark-daemon scaffold --name my-watch --dir /tmp/my-watch                   # blank page; passes the battery
python3 -I -B bin/spark-daemon validate --json --manifest /tmp/my-watch/manifest.json --envelope /tmp/my-watch/candidate.json
python3 -I -B bin/spark-daemon precheck --manifest /tmp/my-watch/manifest.json                # ~1 s fast lane, answers OK/FAIL
```

The battery works in a temporary folder with its own HOME, so it never touches your real output directory, `~/spark-core` or `~/spark-governance`.

## 1. The manifest

`manifest.json` sits beside `daemon.py`. Unknown keys are refused at every level, all values are bounded, and floats are refused.

| Field | Rule | Why |
| --- | --- | --- |
| `manifest_schema` | `spark-daemon-manifest/2` (r3.5; version 1 had no `blind_limit_seconds`) | Versioned like every other Spark schema |
| `name`, `version`, `purpose` | slug; `x.y.z`; one plain line, no `%` | `%` is a systemd specifier |
| `daemon_class` | `observe` only; `act` is reserved and refused | Acting daemons need their own pattern and authority gate |
| `trigger` | `{"kind": "poll", "interval_seconds": 5..86400}`; `inotify-wakeup` reserved | Observer proposal C2 is deferred |
| `reads` | 1–32 absolute or `~/` paths; none inside a denied path; no `.`/`..` segments, `//` or trailing `/` | What `ctx` may read |
| `commands` | up to 8 bare names; shells, command runners (`env`, `timeout`, `tar`, `less`, ...), interpreters including versioned names (`python3.12`), network, privilege and file-mutation tools always refused | What `ctx.run` may execute; `git` only through `ctx.git` |
| `output_dir` | the only writable place; never inside, and never containing, `~/spark-core`, `~/spark-governance` or a denied path (the manifest's own `deny` included); never overlapping `reads` | A daemon never observes its own output (v0.3 §6.7) |
| `deny` | extra denied paths, added to a fixed base list that no manifest can shrink | Base: `~/spark-core/data`, `~/spark-governance/history`, `~/.ssh`, `~/.gnupg`, `~/.claude`, `~/.codex` |
| `network` | `{"mode": "none"}`; `named` reserved | Outbound access needs relaxation R2 and a named destination |
| `run_as` | `{"unit": "system", "user": ...}` (not root) or `{"unit": "user"}` | v0.3 §4 prefers a system unit with a dedicated user on Ubuntu 24.04 |
| `resources` | `cpu_weight` 1–100, `cpu_budget_bp` (100 = 1%), `memory_max_mb` 64–2048, `tasks_max`, `io_class` | Budget checked by the battery; caps written into the unit |
| `watchdog_seconds`, `step_timeout_seconds` | 10–3600; step timeout at most half the watchdog | Every command times out before systemd would kill the daemon |
| `blind_limit_seconds` | 60–86400, at least 3 × the poll interval; required, no default | How long the daemon may go without an accepted cycle before it stops for a human (see *Blind period* below) |
| `ledger` | `record_max_bytes`; 1–32 declared `event_types`, none of them reserved | Only declared events can be written |
| `digest` | `enabled`, `max_bytes` | Size-bounded, with an explicit truncation marker |

The manifest's canonical SHA-256 is recorded in every `DAEMON_START`, so each run names the exact configuration it ran under. The same schema is published as JSON Schema 2020-12 in `contract/manifest.schema.json` (`spark-daemon schema`); the cross-field rules JSON Schema cannot express are listed in it under `x-spark-cross-field-rules`.

## 2. The skeleton

### What a daemon's author writes

`daemon.py` defines three functions:

- `sense(ctx)`: the only place with I/O, and only through `ctx`. Returns a snapshot of plain data, or `ctx.unsettled(reason)` when what it read was not stable (see *Blind period*).
- `decide(prev, snapshot)`: pure. Returns a list of `(event_type, payload)`.
- `digest(snapshot, recent)` (optional): pure. Returns `[(title, [(label, value), ...]), ...]`; the skeleton renders and contains it.

`ctx` offers `read_text`, `list_dir`, `stat` (does not follow symlinks; returns `mtime_us`), `disk_usage`, `run` (manifest commands only, never `git`), `git` (read-only subcommands only; see below), `now_utc` and `unsettled`. It has no method that writes, deletes, sends or executes anything else, so "observation never authorizes action" (v0.3 §6.2) holds by construction. `spark-daemon describe` lists the exact signatures and result types.

`ctx.git(repo, args)` takes a read-only subcommand first (`log`, `show`, `diff`, `status`, `rev-parse`, `for-each-ref`, ...), so the daemon's arguments can never be Git global options. File-writing, file-reading and program-running options are refused, including Git's abbreviations. Arguments may not name paths outside the repository. `--no-ext-diff --no-textconv` are forced on diff-producing subcommands, and `safe.directory` is set to exactly the declared repository (PD-25).

The code may import only `bisect collections dataclasses enum functools hashlib heapq itertools json math operator re statistics string textwrap typing`. It must not name `open`, `exec`, `eval`, `getattr` and similar, not even to alias them. It must not touch dunder names, private attributes (`ctx._policy`) or introspection attributes (`gi_frame`, `f_builtins`, ...), nor use dynamic-access helpers (`attrgetter`, `string.Formatter`, `typing.get_type_hints`). It must not use `global`, and it may make no calls at import time beyond simple constructors such as `re.compile`. `purity.py` checks all of this before import; the full list is in the contract (`contract.code`).

### What the skeleton guarantees, and where each rule comes from

| Guarantee | Module | Source |
| --- | --- | --- |
| Start-up order: validate, safe output dir, lock, purity, audit hook, import, recover, inventory, `DAEMON_START`, then `READY=1` | runtime | WBS 3.0 RC1; proposal C1 |
| Append-only, hash-chained records; one `write()` then `fsync` per record; any failure exits 70, never retries `fsync` | ledger | WBS 3.0 LG2–LG4, FL1 |
| Torn tail = every byte after the last newline, quarantined to a unique file, recorded; corrupt complete lines refuse start (65), nothing changed | ledger | WBS 3.0 RC2–RC4 |
| Streaming verification, memory bounded by record size | ledger | WBS 3.0 VR6, RC7 |
| Canonical JSON, pinned golden vector, no floats | canonical | WBS 3.0 CS1–CS9 |
| Untrusted text contained: code blocks, quoted or prefixed lines, visible escapes such as `\u{202E}` | render | WBS 3.0 ES5; script-board F2 |
| Output dir 0700, owned, not a symlink; foreign files reported, never touched | guard, runtime | WBS 3.0 FS1–FS2; proposal B3 |
| Single instance by `flock`; SIGTERM ends with `DAEMON_STOP` | runtime | Observer WBS 3.1 |
| Restart after an unclean stop is visible: `previous_run_ended_cleanly: false` | runtime | Observer WBS 3.2 (gap visibility) |
| Tool versions, manifest hash and code hash in every `DAEMON_START` | runtime | Proposal B2; script contract SC9 |
| Daemon code that raises `SystemExit` is recorded as a failure, never a silent exit 0 (r3, HF-09) | runtime | Observer WBS 3.2 (gap visibility) |
| CPU in `DAEMON_STOP` and DB-14 includes the commands the daemon ran (r3, HF-18) | runtime | Observer proposal A2 |
| Watchdog pings from the main loop only, so a hang starves them | runtime, notify | Proposal C1 |
| Errors recorded as category and exception class only, never messages; repeats collapsed | runtime | WBS 3.0 FL4 |
| Git with no user or system config, no pager, colour, fsmonitor, external diff or textconv; read-only subcommands only; private index copy on request (r3: HF-01, HF-02, HF-19) | proc | v0.3 §3.8; script-board F3, F4, F5 |
| Bounded command output, process-group kill on timeout, stderr discarded | proc | v0.3 §3.8; WBS 3.0 FL4 |
| Sandboxed system unit: exposure 0.4 ("SAFE") for the example under `systemd-analyze security --offline`; its seccomp filter permits the Landlock syscalls start-up needs (r3, HF-07) | unitgen | v0.3 §4; dev-agents C11 |

### Three layers, and what each cannot catch

| Layer | Catches | Cannot catch |
| --- | --- | --- |
| Purity check (static) | Forbidden imports and calls, dunder tricks, side effects at import | Anything computed at run time; it is syntactic |
| Audit hook (in process) | Writes outside the output dir, reads of denied paths, spawns, sockets, DNS, ctypes; counts violations even if the daemon swallows the exception, then fails closed (exit 78) | Code that bypasses Python (which is why ctypes is blocked); reads outside the manifest by the interpreter itself |
| systemd sandbox | Everything the unit forbids, enforced by the kernel | Only effective once installed as a unit; user units on Ubuntu 24.04 need verification |

### Lifecycle events the skeleton writes

`DAEMON_START`, `DAEMON_STOP` (with CPU and peak memory), `DAEMON_ERROR`, `DAEMON_ERROR_CLEARED`, `LEDGER_TAIL_QUARANTINED`, `DAEMON_HEARTBEAT` (r4.0, every `blind_limit_seconds` / 2). A manifest may not declare these names. Every run's first record is `DAEMON_START`; a `LEDGER_TAIL_QUARANTINED` from that start's recovery follows it (r3.2, HF-26).

### Blind period (r3.5)

A cycle is **accepted** when `sense()` returns a snapshot and its events are committed. Zero events still counts. Two kinds of cycle are not accepted:

- **Unsettled:** `sense()` returns `ctx.unsettled("REASON")` because what it read was changing, for example a worktree mid-build. The cycle is abandoned before `decide()`: no event, no error, and the digest keeps its older stamp.
- **Failed:** `sense()` or `decide()` raised, which is recorded as `DAEMON_ERROR` as before.

The runtime keeps the monotonic time of the last accepted cycle, and start-up counts as one. While the gap is under the manifest's `blind_limit_seconds`, the daemon keeps pinging the watchdog and waits: patience through a long build is correct behaviour. Once the gap reaches the limit, it writes `DAEMON_ERROR` with category `SENSE_BLIND`, carrying `blind_ms`, `limit_ms`, `unsettled_cycles`, `failed_cycles`, `last_cause` and `last_cause_kind`. It then exits 78, and the unit never restarts it.

Without the limit, a daemon that fails or waits every cycle keeps the watchdog happy over a ledger that records nothing. The limit is required and bounded (at most a day), so infinite patience cannot be written down. Set it above the longest legitimate unsettled period on the host, such as a long build or a large checkout, plus a margin.

**Across restarts (r4.0, PD-70):**
- **The clock is not reset by a restart.** It starts at the latest evidence of an accepted cycle in the verified ledger, so a restart loop cannot hide blindness (HF-32).
- **One reacquisition cycle.** A restart that inherits more than the limit gets one cycle: accepted, and the clock resets; not, and `SENSE_BLIND` follows at once.
- **Which clock (r4.5, HF-34).** Within one boot, inherited blindness is measured on `CLOCK_BOOTTIME`, using the `boot_id`, `boottime_ms` and `blind_since_boottime_ms` that `DAEMON_START`, `DAEMON_HEARTBEAT` and `DAEMON_STOP` carry, so a wall-clock step cannot reset it. Across a reboot the wall clock is used; if it went back, blind at the limit is assumed (one reacquisition cycle). `DAEMON_START` and `SENSE_BLIND` record the `clock_basis`.
- **Heartbeat.** `DAEMON_HEARTBEAT` every `T`/2 records the mode, the last accepted cycle and the cycle counts, so a quiet, healthy daemon is distinguishable from a dead one. `git-watch` shows the idiom: it reads refs and the dirty count twice and reports `REPO_CHANGING` when they differ. Its manifest allows 7200 s.

### Exit codes

0 stopped cleanly · 2 bad manifest or impure code · 65 corrupt ledger (nothing changed) · 70 uncertain ledger commit · 73 already running · 78 unsafe output directory, policy violation or `SENSE_BLIND`. The unit sets `RestartPreventExitStatus=2 65 73 78`: only 70 (and a crash) is restarted, because the next start's recovery decides from disk (r3.5, HF-28).

## 3. The batteries

### Conformance battery (every daemon, before activation)

| ID | Check | Passes when |
| --- | --- | --- |
| DB-01 | Manifest validates | Closed schema holds |
| DB-02 | Purity | No findings |
| DB-03 | Confinement | 5 cycles with the audit hook recording: zero events, no file outside the output dir changed |
| DB-04 | Ledger integrity and provenance | Chain verifies; starts with `DAEMON_START`, ends with `DAEMON_STOP`; hashes of manifest and code match; **no `DAEMON_ERROR` in this clean run** (r3, HF-16) |
| DB-05 | Crash and restart | 8 SIGKILLs at seeded random times, then a clean run: chain verifies; every restart records the unclean stop |
| DB-06 | Torn tails | Garbage, and a complete record without its newline, are both quarantined byte for byte and recorded |
| DB-07 | Corrupt ledger | One flipped byte mid-ledger: exit 65, bytes unchanged, nothing quarantined |
| DB-08 | fsync failure | Injected `fsync` EIO (strace): exit 70; restart recovers; chain verifies |
| DB-09 | Disk full | Injected `write` ENOSPC (strace): exit 70; restart recovers |
| DB-10 | Digest containment | The daemon's own digest structure with 220 hostile values: structure unchanged, no raw control or bidi characters |
| DB-11 | Notify protocol | `READY=1` only after `DAEMON_START` is committed; pings every cycle; `STOPPING=1` |
| DB-12 | Single instance, SIGTERM | Second instance exits 73; SIGTERM gives `DAEMON_STOP` and exit 0 |
| DB-13 | Audit hook | Ten forbidden operations blocked and counted for this manifest; output-dir write allowed |
| DB-14 | Resource budget | Self-measured CPU per cycle, projected to the real interval, and peak RSS within the manifest |
| DB-15 | Unit hardening | Every required directive present; `systemd-analyze` exposure at or under 2.0; the unit's resolved `SystemCallFilter` permits every start-up syscall (r3, HF-07) |
| DB-16 | Read-only verification | Two verifications agree; ledger bytes and mtime unchanged |
| DB-17 | Landlock enforces alone (r2) | With the audit hook in record-only mode, the kernel still blocks the forbidden write and the denied read; N/A below Landlock ABI 2 |
| DB-18 | `DAEMON_START.landlock` is honest (r2) | Its `abi` and `gaps` match a fresh in-process check on this host, for a clean single-cycle run |
| DB-20 | The daemon observes (r4.5, HF-35) | Over 12 cycles against a blind limit of five test intervals, the heartbeats show accepted cycles and no `SENSE_BLIND` follows. DB-19 is reserved for the direct cgroup memory reading (PD-76) |

The verdict is PASS only if nothing fails and nothing is UNKNOWN; otherwise FAIL or INCOMPLETE. N/A does not block a PASS (DB-17 when Landlock is unavailable). A JSON report (`spark-daemon-battery/1`) records every check, the seed, the environment and (r3) the contract identity. With `--envelope`, it also quotes the candidate envelope; if the envelope does not match the files that were run, the verdict cannot be PASS.

A daemon that reads under `~` ships a `fixture_home/` folder beside its manifest; the battery copies it into its disposable HOME, so the clean runs have something real to observe (see the reference daemons).

### Skeleton self-tests (the pattern itself)

170 tests (r3): the r2 suite below, plus `test_hardening.py` (35: one or more per HARDENING.md finding, each failing on r2) and `test_handoff.py` (30: the published contract and its version pin, cross-Python determinism, JSON Schema agreement with `manifest.py`, candidate envelopes, validate --json, precheck, scaffold, symlink-safe stat, and the battery failing an always-erroring daemon).

The r2 suite, 105 tests: canonical form, golden vector and RFC 8785 (JCS) key-order vectors (9), manifest refusals (15), ledger corruption categories, torn tails, uncertain commits and bounded memory (16), rendering including a 500-trial property test (10), output-dir checks and the audit hook in child processes (8), subprocess bounds and the F4/F5 Git regressions (5), end-to-end runtime behaviour including a watchdog-starvation test, polling jitter and Landlock's start-up refusal/PD-15 (18), Landlock enforcement itself under `os.fork()` isolation, real kernel rules for read, write, TCP and worker-style nested domains (12), and unit generation, purity, the battery against good and bad daemons, the GC-forcing structural guard, and a source-hygiene check for hidden characters (12).

## Evidence from this build (2026-09-27)

Environment: x86_64 Ubuntu 24.04.4 (workspace kernel 6.18.44), Python 3.12.3, Git 2.43.0, systemd 255, strace 6.8, run as an ordinary user, **Landlock ABI 7**. **Not yet run on the DGX Spark (aarch64)** — that is the first step of review. The Landlock ABI there depends on the DGX OS release (from the v0.1 line's battery review B-5, PD-80 here; inferred from the kernel ladder, not an NVIDIA statement; confirm on the box): 7.2.3 (kernel 6.11) ABI 5; 7.3.1 (6.14) ABI 6; 7.4.0 and 7.5.0 (6.17) ABI 7; 7.6.0 (7.0) ABI 8. Signal scoping, which AP-04's worker relies on, needs ABI 6.

- Self-tests: 105 of 105 pass on Python 3.10, 3.11 and 3.12.3, with real Landlock enforcement active for the whole run (not a stub) — including the SIGKILL-restart and strace fault-injection checks, which continued to work correctly under a live kernel-level confinement domain.
- Battery on `meminfo-watch`: PASS, 18 of 18, including DB-17 (Landlock alone blocks the forbidden write and the denied read with the audit hook in record-only mode) and DB-18 (`DAEMON_START.landlock` matches a fresh in-process ABI/gaps check). About 4.5 ms CPU per cycle at the 200 ms test interval (well under the 1% budget projected to the real 30 s interval); peak RSS about 19-20 MiB against the new 64 MiB floor; unit exposure 0.4.
- Battery on the `opener` fixture (calls `open()`): FAIL at DB-02 and every runtime check, as intended.
- Battery on the `sneaky` fixture (reads a denied path through `ctx` and swallows the error): FAIL; the daemon stops itself with exit 78 on its first cycle, as intended.
- `DAEMON_START` now also carries `jitter_max_ms` (the ± bound on the poll sleep, PD-21) and `landlock` (`{abi, status, gaps}`, PD-15/L4), both disabled/zeroed deterministically in test mode.

## 4. The handoff (r3)

The full design is in `DAEMON-CONTRACT.md`; in brief:

| Direction | Artifact | Command |
| --- | --- | --- |
| Pattern → author | The contract (`spark-daemon-contract/1`): everything a daemon must satisfy, with `contract_version` and `contract_sha256` | `describe`, `contract/daemon-contract.json` |
| Pattern → author | The manifest JSON Schema | `schema`, `contract/manifest.schema.json` |
| Pattern → author | A blank page that already passes the battery | `scaffold` |
| Author → pattern | A candidate folder with `candidate.json` (`spark-daemon-candidate/1`): contract targeted, file digests, producer, intent; no result fields | `envelope` |
| Pattern → author | Feedback: `spark-daemon-validate/1` (ms), `spark-daemon-precheck/1` (~1 s, OK/FAIL) | `validate --json`, `precheck` |
| Pattern → human | Evidence: `spark-daemon-battery/1` (~15 s); only its PASS is admissible | `battery --envelope` |

## Evidence from r3 (2026-09-27)

Same workspace (x86_64, kernel 6.18.44, Landlock ABI 7, systemd 255, strace 6.8, Git 2.43.0). Files in `evidence/r3/`.

- Self-tests: 170 of 170 pass on Python 3.12.3 with `jsonschema` installed (schema-agreement tests active), and on 3.10 and 3.13 with those 4 tests skipped. r2's 105 also pass unchanged.
- Battery: `meminfo-watch`, `disk-watch`, `dir-watch` and `git-watch` each PASS 18 of 18. DB-15 now also reports "start-up syscalls permitted by the unit's seccomp filter". DB-14 CPU includes child processes: `git-watch` 11.5 ms per cycle, the others about 4 to 4.5 ms; peak RSS about 20.5 to 20.8 MiB against the 64 MiB floor.
- `contract_sha256` `ef03a744…` is identical on Python 3.10, 3.11, 3.12 and 3.13.
- Every escape in HARDENING.md was first reproduced on the unmodified r2 package; each has a regression test.

## Decisions for adjudication

Each ends with a recommendation and a decision line, as in the WBS 3.0 spec. Record rulings in the decision log below. **PD-32 to PD-42 (r3.1: conformance to WBS 3.0 r3, digest semantics, convergence on the Observer's writer, one exit-code table, lifecycle rules, KECC vocabulary, digest-bound activation) are in `ADJUDICATION-SPARK-SOURCES.md` section 7.** **PD-23 to PD-31 (r3: the contract, versioning, Git `safe.directory`, the candidate envelope, evidence lanes, DB-04's zero-error rule, contract 1.0.0's rule set, the unit's Landlock syscalls, and the kernel-v2 authoring boundary) are set out in `DAEMON-CONTRACT.md` section 10.**

**PD-82 to PD-86 (r4.3: bind to the Local Transport Contract at declared edges rather than embed it; truncation as a typed failure in `ctx`; the LTC binding vocabulary for the activation register; declared pacing; retry bounds that hold over time) are in `LTC-INTEGRATION-REVIEW.md` section 7.** **A self-contained handoff for independent reviewers is `HANDOFF-REVIEW-PACKAGE.md`.** **The settled rulings revisited, and every open choice with its options, pros and cons, are in `DECISION-BRIEF.md` (r4.4).** **Both, in one document, with each decision's implementation status and the new seams it creates: `REVIEW-PACKAGE-COMBINED.md`.** **r4.5: those improvements are adjudicated in `ADJUDICATION-R4.5.md` (by Claude at the owner's request, subject to the owner), and the SQLite ledger recommendations in `SQLITE-LEDGER-REVIEW.md` (PD-87 to PD-92).**

**PD-01. Observe-only in v1.** `daemon_class: act` is reserved and refused.
REOPENED 2026-09-29, open for expansion: see `PD-01-DAEMON-CLASSES.md`. It covers a capability ladder (three families, two levels each), consequence levels, cross-class safety invariants including authentication under erratic conditions, and sub-decisions PD-01.1 to PD-01.8 (since extended: PD-01.9 adopted as the Blind Forester; PD-01.10 to PD-01.14, Break Glass and operator-gated recovery, in `BREAK-GLASS.md`). Code unchanged: only `observe` (class 1a) is accepted. Decision: PENDING

**PD-02. Ledger format is provisional.** It follows the WBS 3.0 r2 recommendations (seq from 1, 64-zero genesis, `ensure_ascii` false, microsecond `Z` timestamps, one write and fsync per record, no checkpoint file as in K1). When the Observer's own writer exists, `ledger.py` should be replaced by it so there is one implementation.
Recommendation: APPROVE as provisional. Decision: PENDING

**PD-03. One ledger per daemon, and no daemon writes inside `~/spark-governance`.** The Observer itself, if later rebuilt on this pattern, would need an explicit exception.
Recommendation: APPROVE. Decision: PENDING

**PD-04. The base deny list** (six paths, never removable by a manifest).
Recommendation: APPROVE; add paths as needs appear. Decision: PENDING

**PD-05. Daemon code rules**: the import allowlist, I/O only through `ctx`, no floats.
Recommendation: APPROVE. Relaxation R1 on the script board would widen the import list. Decision: PENDING

**PD-06. Fail closed on any policy violation** (exit 78), even when the daemon's own code caught the exception.
Recommendation: APPROVE. Decision: PENDING

**PD-07. Errors carry category and exception class only**, never the message; identical consecutive errors collapse into one `DAEMON_ERROR` plus a `DAEMON_ERROR_CLEARED` with a count.
Recommendation: APPROVE. Decision: PENDING

**PD-08. `~` expands through `SPARK_DAEMON_HOME`**, set by the unit to the generating user's home, so a dedicated system user resolves the same paths. Granting that user read access is a per-path install decision.
Recommendation: APPROVE. Decision: PENDING

**PD-09. System unit with a dedicated user is the default**; exposure threshold 2.0.
Recommendation: APPROVE. Decision: PENDING

**PD-10. Test knobs** (`SPARK_DAEMON_TEST`, a shorter interval, audit record mode) work only in test mode and are recorded in `DAEMON_START.test_overrides`. The alternative is a separate test entry point.
Recommendation: APPROVE. The runtime already refuses test mode when systemd started it (`INVOCATION_ID` is set). Decision: PENDING

**PD-11. Battery verdicts**: INCOMPLETE whenever any check is UNKNOWN; only PASS allows activation.
Recommendation: APPROVE. Decision: PENDING

**PD-12. Budget method**: the daemon's self-measured CPU per cycle, projected to its real interval, plus peak RSS, against the manifest (default 1% and 64 MiB, matching Observer proposal A2).
Recommendation: APPROVE; confirm on the DGX under inference load. Decision: PENDING

**PD-13. `meminfo-watch` band thresholds** (20% and 10% of MemTotal available) are placeholders.
Recommendation: DEFER to the Class C health-rules ruling. Decision: PENDING

**PD-14. Where the pattern lives**: `~/spark-tools` or `~/spark-governance/tools`, and where installed daemons live (for example `/opt`, root-owned and read-only).
Recommendation: decide together with the script board's Home question. Decision: PENDING

**PD-15 to PD-20** come from the adjudication of four external proposals and are set out with their evidence in `ADJUDICATION-AP.md`: Landlock availability policy (PD-15), denied paths inside granted reads (PD-16), the RFC 8785 alignment method (PD-17), the scope of the supervisor/worker split (PD-18), SCITT export (PD-19) and the memory floor (PD-20). The proposal-level verdicts (AP-01 to AP-04) are recorded in the decision log like the PDs.

**PD-21. Polling jitter** ("thundering herd" defence, submitted the same day as AP-01..AP-04). Each poll sleep gets `± min(5% of interval, 30 s)` of uniform random offset, so many daemons on one host wake at different moments; the watchdog pings on its own fixed cadence regardless, so jitter cannot starve it. Disabled deterministically in test mode; recorded as `jitter_max_ms` in `DAEMON_START`.
Recommendation: APPROVE. Decision: PENDING

**PD-22. Per-cycle `gc.collect()` forcing** (submitted the same day). Called once per cycle, after the daemon's own step and before the watchdog ping, never inside the sleep loop. CPython's refcounting already frees ordinary garbage immediately; what `gc.collect()` adds is collecting reference cycles and letting arenas release back to the OS between cycles, which matters more on a host running many daemons that share the same memory budget than it does for any one daemon's own RSS. Measured cost: about 0.2-0.25 ms per call, negligible against the budget.
Recommendation: APPROVE. Decision: PENDING

**A correction, for the record.** A follow-up submission argued for raising `memory_max_mb`'s floor to 32 MB, citing "a production constant `MAX_LEDGER_RECORD_BYTES` of 4 MiB" as part of its math. No such constant exists in this project at 4 MiB: `ledger.record_max_bytes`'s schema ceiling is 1,048,576 bytes (1 MiB), and `wbs-3.0-a2-secondary-review.md` records that figure as explicitly left open for the Observer track, never frozen, because "G0/G0.11 could not derive a finite bound." The floor recommendation here (64 MB, PD-20) stands on its own measurements instead: combined supervisor+worker RSS runs 25-35 MB depending on accounting method (`Pss`/`Private_Dirty` vs. a naive sum), which a 32 MB floor would not clear once AP-04 lands. 64 MB also needs no fixture changes, since every existing manifest already uses it.

## Known limits

- The audit hook is not a sandbox, and the purity check is syntactic. The systemd unit is the security boundary. r3 closed every purity route found in review (HARDENING.md HF-03 to HF-06), each now a named rule and a test, but it cannot prove none remain; Landlock stays the backstop (DB-17).
- A watched repository's own Git config is trusted once `safe.directory` names it (PD-25): a filter driver can still run during `status`/`diff` on its worktree, contained by Landlock and the unit. Prefer bare mirrors for repositories the owner does not control (HARDENING.md, residual risk 1).
- The hook does not enforce the read list for the interpreter itself; `ctx` enforces it for daemon code, and the unit's `ProtectSystem`, `ProtectHome` and `InaccessiblePaths` cover the process.
- Nothing has run under systemd for real yet: the watchdog is tested with a stand-in notify socket and the unit is scored offline. The r3 seccomp fix (HF-07) comes from `systemd-analyze syscall-filter` group membership and is checked by DB-15, but a real PID 1 start is the first DGX step (DAEMON-CONTRACT.md section 11).
- User units on Ubuntu 24.04 are generated with a warning; their sandboxing is unverified.
- DB-05's random kills rarely land mid-write; DB-06 covers torn tails deterministically.
- The CPU projection leaves out start-up cost and is measured with a 200 ms test interval.
- The hostile corpus is not exhaustive; the property tests are seeded and reproducible.
- IF-01 (found in r2, now fixed in code): `resources.memory_max_mb`'s floor is 64 (was 16, below the runtime's own measured peak of about 19 MB); see PD-20 and the correction above.
- Canonical JSON equals RFC 8785 (JCS): key ordering now follows UTF-16 code units (J1), matching JCS for every value this schema accepts (no floats). The golden hash is unchanged.
- AP-04 (the out-of-process supervisor/worker split) is design-only. `ADJUDICATION-AP.md` §S1-S7 specifies it; none of it is implemented, so the benefits it describes (least privilege per process, supervised worker restarts, a worker that cannot write the ledger at all) do not yet apply to this build.
- Landlock (AP-01) is applied but has the gaps `landlock.py` and `ADJUDICATION-AP.md` document: a denied path nested inside a granted read is not covered by Landlock alone (the audit hook and `InaccessiblePaths` still are); `stat()` of a denied path still succeeds; UDP is never covered.

## Revision history

| Revision | Date | By | Change |
| --- | --- | --- | --- |
| r1 | 2026-09-27 | Claude | First draft: manifest, skeleton, conformance battery, self-tests, reference daemon |
| r2 | 2026-09-27 | Claude | Draft adjudication of external proposals AP-01 to AP-04 with Landlock and JCS evidence; PD-15 to PD-20; IF-01 noted. No code change |
| r2 (implemented) | 2026-09-27 | Claude | Implemented and self-tested the parts of r2 with a clear draft verdict: `landlock.py` (AP-01, wired into start-up before the audit hook, DB-17/DB-18 added), JCS key ordering (AP-03/J1), the 64 MB memory floor (IF-01/PD-20). Adjudicated and implemented two further same-day proposals: polling jitter (PD-21) and per-cycle GC forcing (PD-22), and corrected a fabricated "4 MiB ledger constant" claim in one submission's math (see PD-20's note). AP-04 (S1-S7) remains design-only. 105 of 105 self-tests and 18 of 18 battery checks pass under real Landlock enforcement (ABI 7). Still nothing here is a Class C ruling — every PD stays PENDING until the human records one. |
| r3 | 2026-09-27 | Claude | Review, bug hunt and hardening of r2 as received (`HARDENING.md`): 22 defects reproduced on r2 and fixed, each with a regression test. Among them: Git global-option injection through `ctx.git`/`ctx.run` (arbitrary command execution), purity escapes (private attributes, which leaked a denied path; frame introspection; aliasing), a unit whose seccomp filter blocked Landlock (the daemon could never have started once installed), Git's ownership check failing every call under a dedicated user, silent `SystemExit`, `mtime_ns` overflowing the canonical range, and a battery that passed always-failing daemons. Added the handoff layer (`DAEMON-CONTRACT.md`): `describe`/`schema` with contract 1.0.0 and a test-pinned `contract_sha256`, `validate --json`, `precheck`, `scaffold`, candidate envelopes with no authority, `battery --envelope`. Three reference daemons (`disk-watch`, `dir-watch`, `git-watch`). Makefile and CI. Skeleton version 0.3.0. 170 of 170 self-tests; 4 × 18 of 18 battery checks. PD-23 to PD-31 added; every PD still PENDING. |
| r3.1 | 2026-09-28 | Claude | Cross-check against the Spark handoffs (`ADJUDICATION-SPARK-SOURCES.md`). Fixed HF-24: `ctx.git` now carries the Observer's WBS 2.5 hardened profile (signature verification and mailmap off, replace refs, grafts and lazy fetch neutralized); a repository's own `gpg.program` had been reachable through `log`/`show`, reproduced. Contract 1.0.1. Recorded conformance gaps against WBS 3.0 r3 (no startup fsync, crash-idempotent quarantine, origin-record check, artifact FS6), WBS 3.0E.1's review of this pattern (exit-code collision, lifecycle, sense budget, PD-22), and PD-32 to PD-42. Nothing else changed in code. |
| r3.2 | 2026-09-28 | Claude | Cross-check against the WBS 3.1 final adjudicated contract (`ADJUDICATION-SPARK-SOURCES.md` section 4a). Fixed HF-25: a rejected duplicate launch deleted the running instance's `tmp/` files, because crash cleanup ran before the lock (WBS 3.1 §4.2). Cross-check against the WBS 3.0 r4 closeout (section 4b). Fixed HF-26: a torn first-ever write made `LEDGER_TAIL_QUARANTINED` seq 1, a ledger DB-04 rejects; `DAEMON_START` is now always first, following r4's order. PD-34's trigger has been met. Revised PD-35 after 3.1 D-6: the Observer keeps its exit table, and the pattern aligns with it voluntarily instead of proposing a shared one. PD-37 updated; PD-43 (root refusal), PD-44 (lock anchor and continuity) and PD-45 (recorded divergences) added. Contract unchanged at 1.0.1. 175 of 175 self-tests (Python 3.12 with `jsonschema`); 4 × 18 of 18 battery checks. |
| r3.3 | 2026-09-29 | Claude | Reviewed a proposed plug-and-play contract stack (L0 component envelope; L1 to L4 layers; zero-touch, guided and learned-adapter connection) against this pattern (`ADJUDICATION-PLUG-AND-PLAY.md`). The pattern already implements the proposed L0 for resident components. Recommended changes to the proposal: no self-declared authority field, Spark-executed conformance, digest-pinned contract claims, a resident lifecycle shape, and explicitly named digests. Fixed HF-27: the battery report now carries `daemon_code_sha256`, so a verdict is bound to the code it ran. PD-46 to PD-52. Contract unchanged at 1.0.1. |
| r3.4 | 2026-09-29 | Claude | Framework/services boundary after the Asterinas framekernel split (`ADJUDICATION-PLUG-AND-PLAY.md` §7): the shape is adopted, the guarantee is not claimed. `tests/test_layers.py` locks in the pure service modules (`canonical`, `render`), `ctypes` only in `landlock`/`probes`, and one truncate site; each check fails on a planted violation. PD-53. Kernel v2 agenda (§8): PD-54 to PD-62 (roadmap slot, shared exit taxonomy, shared writer mechanism, one unit generator, off-host anchor, framework language, act/egress, model-facing evidence rule, producer trust). No runtime change. 179 of 179 self-tests. |
| r3.5 | 2026-09-29 | Claude | Blind period, at the owner's request: `ctx.unsettled(reason)`, a required `blind_limit_seconds` (60–86400, at least 3 poll intervals), and `DAEMON_ERROR SENSE_BLIND` with exit 78 when no cycle is accepted within it. `git-watch` reports `REPO_CHANGING` when refs or the dirty count move between two reads. Fixed HF-28: `Restart=on-failure` restarted every fail-closed 78 every 10 s forever; the unit now sets `RestartPreventExitStatus=2 65 73 78`. Fixed HF-29: `git-watch` recorded Git output over its cap as "unavailable" or `dirty: null`, accepted every cycle; overflow and timeout are now failed cycles that count towards the limit. Contract 2.0.0 (a required manifest field breaks every 1.x manifest); manifest schema `spark-daemon-manifest/2`, with a migration hint for version 1. PD-63 was closed by owner directive (decision log). 195 of 195 self-tests; 4 × 18 of 18 battery checks. |
| r3.6 | 2026-09-29 | Claude | PD-01 reopened at the owner's request and left open for expansion (`PD-01-DAEMON-CLASSES.md`). It covers: a capability ladder (Observe 1a/1b, Advise 2a/2b, Act 3a/3b), in which a daemon never gains power itself, only request channels to a gate; a separate consequence axis (standard, elevated, critical); cross-class safety invariants S-1 to S-7, including authentication and handshakes that never degrade (connector gate, fencing, idempotency, fail-closed); scripts under the same invariants; sub-decisions PD-01.1 to PD-01.8; open questions; an expansion log. No code change. |
| r3.7 | 2026-09-29 | Claude | Prior-art review at the owner's request (`PRIOR-ART-REVIEW.md`), covering IEC 61784-3, NAMUR NE 107, ISA-18.2, PackML, IEC 62443, NASA F´, ASTM F3269, AUTOSAR, ISO 21448, ROS 2, Autoware, UL 4600, systemd, Erlang/OTP, SPIFFE, Fuchsia, CaMeL and Levels of Autonomy. The design is confirmed in nine places, and twelve amendments to PD-01 are proposed (A-1 to A-12, `PD-01-DAEMON-CLASSES.md` §9a). Corrected HF-28: the unit's own start limit (5 in 300 s) already stopped fast 78 exits after five restarts; the endless loop applied to `SENSE_BLIND`, whose restarts are too far apart to trip it. Severity lowered to Medium. No behaviour change. |
| r3.8 | 2026-09-29 | Claude | Universal contract (`ADJUDICATION-PLUG-AND-PLAY.md` §9). It states what is in code and what is only recorded, places the recent pieces in the L0 to L4 stack (what a component claims about itself goes in its envelope; what Spark decides goes in Spark's records), and sets three rules: the invariants become a base conformance suite (U-1); nothing safety-related in L3 (U-2); escalation meaning is a contract and delivery a binding (U-3). Four further considerations: U-4 temporal honesty (unsettled gaps are folded silently into the next event's timestamp; S-8 proposed); U-5 stop and revocation, the inverse of activation; U-6 cumulative bounds (start-up verification measured at about 20,000 records/s, so git-watch's 120 s start timeout is exceeded at about 2.4 million records; `evidence/r3.8/`); U-7 evidence that expires and independence that must be shown (a watcher built on the same skeleton shares its defects). PD-64 to PD-68. No behaviour change. |
| r3.9 | 2026-09-30 | Claude | Integration review (`INTEGRATION-REVIEW.md`). Recurring cause of recent defects: limits checked alone, never against each other (U-8, PD-69). Fixed HF-30 (introduced by r3.5: git-watch's double read made a worst-case cycle, 6 × 20 s, equal its 120 s watchdog; watchdog now 240 s) and HF-31 (stop timeout now outlasts one watchdog period). Reproduced and left open HF-32: every start resets the blind clock, so slow restart loops hide blindness indefinitely (PD-70: observation heartbeat plus blindness counted across restarts, which also tells quiet from dead). Reviewed the DB-14 comment: the check is right; the evidence is one small fixture and per-process, not cgroup-wide; the D-6 citation is out of scope (PD-71). Blind modes for critical consequence (PD-01.9, BM-1 to BM-7): declared in advance, run by the side that still works, reconnection is a new session. Integration plan in five phases. 197 of 197 self-tests; git-watch 18 of 18 battery checks. |
| r4.0 | 2026-09-30 | Claude | Owner rulings recorded: PD-01.9 "Blind Forester" adopted as core pattern; PD-70 adopted. Built PD-70 (HF-32 fixed): blindness survives restarts (the clock starts at the latest ledger evidence of an accepted cycle), `DAEMON_HEARTBEAT` every `T`/2, one reacquisition cycle so a restart never locks a daemon out. The contract names the blind-forester slot (fail closed for observers; active survival loops need an Act class and the supervisor split). Hardening review adjudicated (`INTEGRATION-REVIEW.md` §7): environment sanitization and streaming with byte ceilings were already implemented (now pinned by a test); supervisor/worker split adopted as a precondition for active classes (PD-72); s6/tini not needed under systemd; WASI and WASM fuel deferred (PD-73, PD-74). Implementation conditions BF-1 to BF-6 proposed. Contract 3.0.0. 202 of 202 self-tests; 4 × 18 of 18 battery checks. |
| r4.1 | 2026-09-30 | Claude | Absorbed the parallel v0.1 line, moved in by the owner (`sources/v0.1-line/`, verbatim; `RECONCILIATION-V01-LINE.md`). Both lines grew from the same r2 and both numbered new decisions from PD-23, so that line's PD-23 to PD-29 are aliased here as PD-75 to PD-81 (pending owner confirmation). Recorded that line's owner ruling PD-24(a) as PD-76 (direct cgroup memory basis; not yet built; supersedes the memory part of PD-71). Adopted the AP adjudication's r3 edits (the only difference). Replaced the stale "ABI 7 expected on the DGX" with the inferred ABI table. Answered from the code three questions the documents-only battery review left open: DB-14 measures a per-process peak (B-1); DB-04 and DB-16 use the skeleton's own verifier, so they are not independent (B-6); `sneaky` is caught first by DB-03 (B-4). Conflict C-1 flagged: this battery exits 3 for INCOMPLETE, while the v0.1 line's proposal uses 3 for FAIL. No behaviour change. |
| r4.2 | 2026-09-30 | Claude | Break Glass Protocol adjudicated at the owner's request (`BREAK-GLASS.md`, PD-01 §9c, PD-01.10 to PD-01.14). Premise adopted: an emergency is a deterministic reflex over mechanical signals, never a model's conclusion. Scope rule adopted and made enforceable: external declared connections only, refused by the validator and again at connect time for anything internal. Mechanisms: dormant egress becomes a separate pre-granted relay (Landlock cannot be relaxed, and its network rules cannot name a host); the unauthenticated `/tmp` socket is rejected in favour of the owner's revised, epoch-bound, peer-identified socket; pre-signed envelopes adopted as single-use, epoch-bound tickets from a human-approved runbook. The owner's emergency epochs, independent witness and flight recorder, and the operator-gated recovery boundary (`SAFE_TO_ISOLATE`, `REJOIN_PROBATION`), merged into one state machine. Added: a beacon tier that can come before class 3b; fencing enforced at the external target (split brain); a new epoch at every boot; a rollback defence; a hard maximum emergency duration; recovery-panel rows that state evidence, not verdicts. No behaviour change. |
| r4.3 | 2026-09-30 | Claude | Reviewed the LTC hardening markup (LTC-H01 to H03) and the LTC v0.1/v0.2 requirements for integration (`LTC-INTEGRATION-REVIEW.md`). Recommendation: a standardized transport contract, bound at declared connections, not embedded in the pattern. Today's observers have no LTC channel, and the ledger is expressly not one (LTC-01). The seam arrives with the first request channel. Requirements LI-1 to LI-14 and decisions PD-82 to PD-86. Notable points: the supervisor must never pass survival outputs off as worker completions (LTC-14 with BF-1); a break-glass relay is a pre-bound rebind (LTC-V2-19). Markup on the three items: STREAM_BOUNDED needs a total-operation deadline, trust evidence must name the digest it covers, and retry bounds must hold over time (HF-28 had an independent 5-per-300 s budget and still looped forever). Applying LTC-H01 found **HF-33**: `dir-watch` recorded unchanged files as removed whenever its folder exceeded the 512-entry cap (17 of 20 additions on ext4, 20 of 20 on tmpfs); a folder over the cap is now a failed cycle (`dir-watch` 0.2.0; `evidence/r4.3/`). Added `HANDOFF-REVIEW-PACKAGE.md` for independent reviewers. 205 of 205 self-tests; 4 × 18 of 18 battery checks. Contract unchanged at 3.0.0. |
| r4.4 | 2026-09-30 | Claude | The owner asked whether any settled ruling is worth revisiting (`DECISION-BRIEF.md`). None should be reversed; each gets one proposed amendment or clarification. **PD-63:** `SENSE_BLIND` and a policy violation share exit 78, so when the output directory is unsafe (no ledger), a security stop looks operational; move `EXIT_POLICY` to its own code. **PD-70:** inherited blindness is measured on the wall clock, so a backward clock step brings back HF-32 (traced); measure on `CLOCK_BOOTTIME` within a boot and assume the worst across boots. **PD-01.9:** state that it does not unlock `critical`, and make BF-5 mandatory. **PD-76:** measured that this workspace *can* take a direct cgroup reading without systemd (cgroup v1); the cgroup peaked at 208 MiB where DB-14's basis read 128 MiB (`evidence/r4.4/`), which confirms the ruling's premise and corrects r4.1's statement that the reading was impossible here. Every open decision in Packages A to C is set out with options, pros, cons and a recommendation, plus a suggested order of rulings. No behaviour change. |
| r4.5 | 2026-09-30 | Claude | Adjudicated the r4.2 to r4.4 improvements at the owner's request (`ADJUDICATION-R4.5.md`; subject to the owner, two items need the owner's word). Nothing rejected; four recommendations changed on a second look: truncation always raises, with no opt-in partial type until one is needed (PD-83); the `pacing` field adopted but today's jitter stays the default (PD-85); a separate-hardware witness where a person could be harmed (PD-01.13); a change-detecting staleness checker (Phase 1). Fixed **HF-34** (High): after a backward wall-clock step every restart inherited 0 ms of blindness, bringing HF-32 back; inherited blindness is now measured on `CLOCK_BOOTTIME` within a boot, with `boot_id` in `DAEMON_START`, `DAEMON_HEARTBEAT` and `DAEMON_STOP`, and blind-at-the-limit assumed across a reboot when the wall clock went back. Contract 3.1.0. Fixed **HF-35** (High): the battery passed a daemon that never observes anything (RESULT: PASS for the new `neverseeing` fixture); new check DB-20 "observes within a blind limit". Conformance battery review for secondary review with the code map, per-check limits, gaps and planned checks DB-19, DB-21, DB-22, and a generated list of all 210 self-tests (`BATTERY-REVIEW-R4.5.md`). Reviewed the three-ledger SQLite store, `systemd.path` wake and UDS IPC (`SQLITE-LEDGER-REVIEW.md`). Measured on SQLite 3.45.1: a `JSON` column stores `123` as an integer and `1.50` as 1.5, so stored bytes differ from hashed bytes; plain concatenation lets two different rows hash alike; a read-only observer loses the ledger once the writer closes. PD-87 to PD-92. 210 of 210 self-tests; 4 × 19 of 19 battery checks. |
| r4.6 | 2026-09-30 | Claude | Reviewed and adjudicated hardening recommendations on screening before execution and runtime boundaries (`HARDENING-REVIEW-R4.6.md`, PD-93 to PD-98; subject to the owner). Fixed **HF-36** (Medium): the Landlock domain granted execute wherever it granted read, including the output directory, so a binary written there or dropped into a watched folder could be run if the in-process layers were bypassed (reproduced under ABI 7); execute now stays on system paths only. Rejected a disassembly gate for `svc`/`syscall` (every binary makes system calls; it would refuse everything or be evaded; it adds hostile-input parsing to the trusted side) in favour of execute-denial and digest-pinned executables. Reshaped ELF parsing into an activation-time battery check. Corrected two claims: Landlock restricts TCP `bind`/`connect`, not `socket()`, and the unit closes the network; WASM fuel bounds CPU only, and has no `fork`. Recorded F-1 (simulated): a directory swapped for a symlink between `ctx.read_text`'s check and its open can reach a denied subtree inside a declared read, outside systemd; fix planned as a check after opening. Credential stores and secret-shaped names to be denied in contract 4.0.0. 211 of 211 self-tests; 4 × 19 of 19 battery checks. |

## Decision log

| Date | ID | Decision | By | Notes |
| --- | --- | --- | --- | --- |
| 2026-09-29 | PD-76 (= v0.1 line PD-24) | ADOPTED (a): memory at the budget check needs a direct cgroup `memory.peak` reading from a fresh transient scope; process RSS is a proxy, and RSS-only runs are `INCOMPLETE` | Owner (recorded in the v0.1 line's battery review; imported 2026-09-30) | Number aliased because this repository's PD-24 is contract versioning (`RECONCILIATION-V01-LINE.md` §2). DB-14 to be retired for DB-19. **Not yet built in either line**; this workspace cannot take the reading (no systemd user manager). *Corrected in r4.4: it can, through a directly created cgroup (v1 here); measured 208 MiB for the cgroup against DB-14's 128 MiB (`evidence/r4.4/`)* |
| 2026-09-30 | PD-93 to PD-98 (r4.6 hardening review) | Verdicts as listed in `HARDENING-REVIEW-R4.6.md` §4 | Claude, at the owner's request; subject to the owner | HF-36 fixed; F-1 fix planned |
| 2026-09-30 | r4.5 adjudication (PD-63 split, PD-70 clock basis, PD-82 to PD-86, PD-01.10 to PD-01.14, BF-1 to BF-6, PD-72, PD-01.1 to PD-01.3, PD-69, PD-71, PD-75 to PD-81, C-1, Phase 1, PD-64 to PD-68, A-1 to A-12; PD-87 to PD-92) | Verdicts as listed in `ADJUDICATION-R4.5.md` §3 and `SQLITE-LEDGER-REVIEW.md` §6 | Claude, at the owner's request; subject to the owner | PD-01.9's scope and PD-76's wording need the owner's own word. PD-70's clock basis built (HF-34) |
| 2026-09-30 | PD-01.9 | ADOPTED AS CORE PATTERN (the "Blind Forester" protocol) | Owner | Active daemons shift into a pre-validated survival loop instead of failing closed when stopping would drop a critical downstream payload. Implementation conditions BF-1 to BF-6 proposed, pending confirmation (`PD-01-DAEMON-CLASSES.md` §9b) |
| 2026-09-30 | PD-70 | ADOPTED ("reversed and adopted") | Owner | Heartbeat every `T`/2 and blindness carried across restarts; built in r4.0 with one reacquisition cycle per restart (`INTEGRATION-REVIEW.md` §6) |
| 2026-09-29 | PD-63 | CLOSED BY OWNER DIRECTIVE: blind period in force as implemented in r3.5 | Owner (directive); recorded by Claude | The owner asked for it ("consider this an ability within the daemon pattern ... implement this solution") and asked that it not remain pending. `T` is set in the manifest; this was the implementer's reading of "deployment configuration" and is reversible (`ADJUDICATION-SPARK-SOURCES.md` §4c). |
