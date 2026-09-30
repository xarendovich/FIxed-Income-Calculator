# Integration review: limits that collide, a reviewed comment, and one integration plan (r3.9, r4.0)

- **Status:** REVIEW FOR ADJUDICATION. Two defects are fixed (HF-30, HF-31). One is reproduced and left open because its fix changes the ledger vocabulary (HF-32, PD-70). Everything else is a recommendation, and PD-69 to PD-71 are PENDING.
- **Revision:** r3.9, 2026-09-30, by Claude. r4.0 (same day) records the owner's rulings on PD-01.9 and PD-70, implements PD-70 (HF-32 fixed, contract 3.0.0), and adjudicates a three-point hardening review (sections 6 and 7).
- **Asked:**
  - ensure the recorded work is integrated;
  - determine whether similar edge cases will become connection problems later;
  - consider cases where a life depends on the running process ("a way for it to be flown and controlled if blind");
  - review a comment about DB-14 from an earlier Spark chat.

## 1. The finding that ties this together: limits are checked alone, never against each other

Every limit in the pattern is validated on its own: step timeout, watchdog, blind limit, start timeout, stop timeout, memory cap, start limit. The defects found in the last few revisions all sit *between* two limits that are each valid alone:

| ID | Limits in conflict | What happens | Status |
| --- | --- | --- | --- |
| **HF-28** | Blind limit vs systemd's start limit | A `SENSE_BLIND` exit restarts forever; its restarts are too far apart for the start limit to catch | Fixed r3.5 (no restart of 78) |
| **U-6** | Ledger growth vs `TimeoutStartSec` | Start-up verification of a large ledger exceeds the start timeout; the daemon can never start again | Recorded (PD-67) |
| **HF-30** | Git calls × step timeout vs `WatchdogSec` | A git-watch cycle may legally take 6 × 20 s = 120 s, exactly the 120 s watchdog. **My r3.5 change caused it**: the double read added two Git calls (it was 4 × 20 s = 80 s). The watchdog is pinged only between cycles, so a slow cycle during a long build (the case the blind period was built for) could be killed as a hang | **Fixed now:** git-watch's watchdog raised to 240 s, restoring the "half the watchdog" margin for the whole sense phase. The general fix is PD-38's per-cycle sense budget |
| **HF-31** | Worst-case cycle vs `TimeoutStopSec=30` | A stop is honoured only between cycles, so a stop during a slow cycle became a SIGKILL, recorded as an unclean end | **Fixed now:** the stop timeout outlasts one watchdog period, `max(30, watchdog_seconds + 10)` |
| **HF-32** | Blind clock (per process) vs restart policy | Every start resets the blind clock. A restart loop slower than the start limit (5 in 300 s) never trips it either: for example a watchdog kill every two minutes, or an out-of-memory kill every few minutes. The daemon stays blind indefinitely and silently, which is the outage the blind period exists to prevent. **Reproduced** (`evidence/r3.9/`): 4.0 s blind against a 1.5 s limit across four kill-restarts, with no `SENSE_BLIND` record; left alone, the same daemon exits 78 | **Open** (PD-70; needs a new lifecycle record) |
| **T-6** | "Record only changes" vs external monitoring (A-2) | A healthy daemon that sees no change writes nothing, so in the ledger it looks exactly like a dead one. A staleness check on the ledger (A-2) would false-alarm on quiet, healthy daemons. The same gap weakens temporal honesty (U-4): the ledger cannot say "I was watching and nothing changed" | Open (PD-70) |
| **T-5** | DB-14's memory measurement vs `MemoryMax=` | DB-14 reports the peak of the largest single process on a small fixture. `MemoryMax=` limits the whole cgroup: the daemon plus a concurrent `git` child plus kernel-charged memory. An out-of-memory kill under real load then becomes an HF-32 restart loop | Open (PD-71; see section 2) |

**U-8 (proposed rule): limits are checked against each other, not only alone.** Every daemon gets a timing and bounds budget, computed from its manifest and checked at validation and in the battery:

| Budget | Must hold |
| --- | --- |
| Worst-case sense phase (enforced at run time by PD-38) | ≤ `watchdog_seconds` / 2 |
| Worst-case cycle | ≤ stop timeout (HF-31) |
| Projected start-up work at the declared retention | ≤ start timeout (U-6) |
| Blindness | Counted across restarts (HF-32), so no restart pattern can hide it |
| Worst-case memory on a worst-case fixture, measured as the cgroup total | ≤ `MemoryMax` with headroom (T-5) |
| Any future connection's handshake timeout | ≤ the step budget that contains it |

The same rule answers "will similar cases become connection problems later": every new connection brings new timeouts (handshake, lease, heartbeat, reconnect), and each must be placed in this budget before it ships.

## 2. Review of the DB-14 comment

> "The daemon pattern's DB-14 battery check ("peak RSS within the manifest") is already Option A applied per daemon. If the Observer is later rebuilt on that pattern, N_max is the Observer manifest's evidence and the taxonomy question becomes "which fixture is worst-case for each daemon class." Blocked by D-6 (needs demonstrated multi-daemon need) and PD-03 (Observer would need an explicit exception)."

I do not have the earlier chat that defined "Option A" and `N_max`. I read them as "derive the capacity ceiling from measured evidence per daemon" and "the largest input size the daemon can handle within its memory cap".

| Claim | Verdict |
| --- | --- |
| DB-14 checks peak RSS against the manifest | **Correct, with two caveats.** (1) It measures the peak of the largest *single* process (`wait4`'s `ru_maxrss`), while `MemoryMax=` limits the *whole cgroup*: the daemon, a concurrent `git` child and kernel-charged memory together. (2) It measures once, on the battery's own small fixture (git-watch: a tiny bare repository, 19.3 MiB peak today), so it is evidence for that fixture only |
| "…is already Option A applied per daemon" | **Partly.** The place is right: DB-14 is where per-daemon evidence belongs. But a single small fixture cannot produce `N_max`. That needs a scaling run (the same daemon on fixtures of increasing size), fitted growth, and headroom below the cgroup cap |
| "N_max is the Observer manifest's evidence" | **Right in principle, and it understates the block.** Rebuilding the Observer on this pattern is not a small step. Its WBS 3.0 r4 and 3.1 contracts are frozen, WBS 3.0E.1 says this pattern "is not binding on the Observer", and the r4 closeout requires a deliberate Class C reopening for any semantic change |
| "which fixture is worst-case for each daemon class" | **Terminology clash.** Since r3.6, "class" means PD-01's capability rung (Recorder, Sentinel, …). The worst case depends on the *workload observed* (repository size, file count, entries), not on the rung. It should read "for each observation type", or simply "for each daemon" |
| "Blocked by D-6" | **Cited out of scope.** WBS 3.1 D-6 rules on the *exit-code* namespace; its "shared daemon taxonomy requires demonstrated multi-daemon need" is about exit reasons. The *principle* transfers (do not generalise before a second daemon shows the need; PD-51 applies the same test to event contracts), but D-6 does not govern a resource or fixture taxonomy |
| "Blocked by PD-03" | **Correct.** PD-03 says the Observer, if rebuilt on this pattern, would need an explicit exception, because it writes inside `~/spark-governance` |

**PD-71 (proposed):** DB-14 becomes worst-case evidence. Each daemon names its worst-case fixture (by observation type), DB-14 runs a scaling series to derive `N_max`, and the measurement covers what `MemoryMax=` enforces. That means the cgroup total, for example by running under a transient systemd scope where one is available, or conservatively the daemon plus its largest concurrent child. The limit keeps headroom, for example a peak of at most half the cap on the worst-case fixture.

## 3. When a life depends on it: blind is a mode, not an accident

The pattern's answer to blindness, stopping for a human (exit 78), is right for an observer, because stopping loses nothing but a record. It is wrong wherever the running process keeps something safe. Aviation and robotics answer "how is it flown when blind?" in a consistent way:
- **Pitch and power:** when airspeed data fails, pilots fly memorised attitude and thrust settings that are safe without the failed sensor.
- **Lost link:** a drone that loses its command link executes a pre-set procedure on board (loiter, return home, land).
- **Minimal-risk manoeuvre:** an automated vehicle steps down through fallback modes to one.

In every case the blind behaviour is **declared in advance, validated in advance, and executed by the side that still works**. PD-01.9 (in `PD-01-DAEMON-CLASSES.md` §9b) records this as requirements BM-1 to BM-7. The two that matter most for connections:
- **The procedure runs on the far side.** When a connection is lost, Spark cannot command anything, so the device itself must be able to act safely alone.
- **Reconnection is a new session.** After a lost link, control resumes only after full re-authentication and a reconciliation of what happened in the meantime; never by resuming the old session.

**The boundary this sets for Spark:** no process on the DGX may sit in the loop that keeps a person safe. Spark may observe and advise that loop. The loop itself runs on an independent controller with its own sensors and its own declared blind modes. Spark never flies anything blind; it makes sure whatever flies has a declared way to fly blind.

## 4. Integration plan

What "integrated" means for the recorded work, in dependency order. Each phase ships as one tested revision.

| Phase | Contents | Contract effect | Waiting on |
| --- | --- | --- | --- |
| **0 (done, r3.9)** | HF-30 git-watch margin; HF-31 stop timeout | None | — |
| **0b (done, r4.0)** | PD-70: heartbeat and blindness across restarts (HF-32 fixed); environment redirection pinned by a test | Contract 3.0.0 | — |
| **1 (small, no contract change)** | A-2 minimal: `OnFailure=` notifier. U-7: a separate, diverse staleness checker that does not import `spark_daemon`; until PD-70 lands it reads the digest's modification time, not the ledger's. The digest is rewritten after every accepted cycle, so this covers only daemons with a digest, and a failed digest render also shows as stale. U-8: DB-15 prints each daemon's timing and bounds budget | None | Owner's go-ahead |
| **2 (one batched breaking change, contract 3.0.0)** | PD-70: observation heartbeat plus blindness counted across restarts (fixes HF-32 and T-6). PD-38: per-cycle sense budget (WBS 3.1 §8). U-4: gap record. P3-2 and PD-01.3: `SENSE_DEGRADED`, recovered, `spark-escalation/1`. PD-37: lifecycle and signals. PD-32: WBS 3.0 r3/r4 recovery conformance. PD-35, PD-39, PD-48, PD-49: exit reasons, vocabulary, identity, digest names. PD-01.2: consequence field. PD-71: DB-14 worst-case evidence | One major version, so that authors migrate once | Rulings on the listed PDs |
| **3 (the activation register)** | PD-40 register; U-5 revocation and stop semantics; A-3 recorded risk assessment; U-7 evidence validity periods; cross-manifest checks (one daemon reading another's output) | Register format | Phase 2 |
| **4 (the connector gate)** | PD-01.7 gate with A-1 black-channel suite and A-11 security levels; then Sentinel paging, A-2 off-host heartbeat with PD-58 anchoring, P3-3 shelving and router | Action-plane contract | PD-42 egress preconditions |
| **5 (kernel v2)** | Advise and Act unlocks (PD-01.4 to PD-01.6); critical consequence with declared blind modes (PD-01.9) | Kernel v2 | Certification path |

## 5. Decisions

**PD-69. Adopt U-8: limits are checked against each other.** The budget table in section 1 is computed per daemon, checked at validation where the manifest allows it, and reported by the battery (DB-15) where it needs measurement. Every new timeout, including those of future connections, is placed in the table before it ships.
Recommendation: APPROVE. Decision: PENDING

**PD-70. Blindness survives restarts, and quiet is distinguishable from dead** (HF-32, T-6).
- **Heartbeat.** After every `T`/2 of continuous accepted cycles, the runtime writes one observation heartbeat record, carrying the number of cycles accepted since the last one. Growth stays bounded: 24 a day for git-watch, 96 for meminfo-watch.
- **Start-up.** The blind clock starts from the latest evidence of an accepted cycle: a daemon event or a heartbeat. It does not restart at zero. The time across restarts is measured on the wall clock, under U-4's clock rules.
- **What resets it.** An operator restart does not reset it. Only an accepted cycle does, or a recorded shelving window (A-6).
- **Accuracy.** Across restarts the limit fires between `T`/2 and `T` after the last true accepted cycle, because a heartbeat can be up to `T`/2 old. A shorter heartbeat interval narrows that window.
- **Benefits.** The heartbeat also lets an external monitor tell "quiet" from "dead" (A-2), and lets the ledger say "watching, nothing changed" (U-4).

Recommendation: APPROVE; build in phase 2. **Decision: ADOPTED by the owner, 2026-09-30** ("reversed and adopted"). Implemented in r4.0; see section 6.

**PD-71. DB-14 becomes worst-case evidence** (section 2): a named worst-case fixture per observation type, a scaling series deriving `N_max`, measurement of the cgroup total, and headroom.
Recommendation: APPROVE; build in phase 2. Decision: PENDING

## 6. Owner rulings of 2026-09-30, and what was built

**PD-70: ADOPTED** (the owner: "reversed and adopted").
- **The owner's mechanic:** a daemon restarted during a blind survival scenario must know it at once, and skip the fresh `T` countdown.
- **Built in r4.0 (contract 3.0.0).**
  - `DAEMON_HEARTBEAT` is written every `T`/2. It carries the mode (observing or blind), `last_accepted_utc`, `blind_ms` and cycle counts.
  - At start-up, the blind clock begins at the latest evidence of an accepted cycle in the verified ledger, read in chain order: a daemon event, or the `last_accepted_utc` carried by a heartbeat or a clean stop. It is measured on the wall clock across the restart. `DAEMON_START` records what was inherited.
  - `DAEMON_STOP` and `SENSE_BLIND` carry `last_accepted_utc` too.
- **One addition, to prevent a lockout.** A restart that inherits more than the limit gets exactly **one reacquisition cycle**.
  - Not accepted: `SENSE_BLIND` follows at once, with no fresh countdown. This is the owner's "bypass".
  - Accepted: the clock resets.
  - Without this, a daemon blinded by a finished build could never be restarted, because every start would fail closed before looking.
  - A restart that inherits *less* than the limit continues the countdown from where it stood.
- **Evidence.** The r3.9 reproduction now yields `SENSE_BLIND` (`evidence/r4.0/`). Four new tests fail on r3.9 and pass now: a restart loop, a clean stop that must not reset the clock, the reacquisition cycle both ways, and heartbeats from a quiet daemon.
- **Side effect worth using.** A live daemon's ledger now changes at least every `T`/2. The external staleness check (A-2, U-7) can therefore be as simple and diverse as a file-age check (`find … -mmin`) on the ledger, with no reading of its contents.

**PD-01.9 (the "Blind Forester" protocol): ADOPTED AS CORE PATTERN** by the owner.

The owner's rationale: an active daemon cannot simply fail closed when doing so drops a critical downstream payload. On breaching `T`, or on losing its primary inputs, it shifts into a pre-validated, degraded survival loop (last-known-safe outputs, holding station, a safe-descent sequence) and signals distress.

What is built, and what is not:
- **Observers (the only class today).** The contract now names the blind-forester slot. Its action for class `observe` is fail closed (`SENSE_BLIND`, 78, never restarted), since stopping an observer drops no downstream payload.
- **Active survival loops.** No active class exists yet, so no survival loop is built. The contract says so explicitly (`cycle.blind_forester.active_classes`).

Implementation conditions follow from the owner's own hardening review and from recorded invariants. They are recommendations that shape how the ruling is built, not changes to it, and are listed in `PD-01-DAEMON-CLASSES.md` §9b as BF-1 to BF-6.

## 7. Hardening review (three items)

Source: the owner's review "three structural improvements to harden the protocol against adversarial exploitation". Each claim was checked against the code before a verdict.

**Item 1: supervisor/worker split with fast supervision.**

| Point | Verdict |
| --- | --- |
| A blinded worker (memory corruption, a payload-induced infinite loop) cannot reliably run its own survival protocol in the same memory space | **Agreed, and decisive for PD-01.9.** It is BM-3's rule (an independent monitor makes the switch) applied inside the host. An active Blind Forester's survival loop must live in the supervisor, never in the untrusted worker |
| Supervisor keeps the ledger and the watchdog; sensing and decision logic run as an isolated child | **ADOPT for any active class.** It is already designed as AP-04 (S1 to S7, `ADJUDICATION-AP.md`), design-only so far. It also gives what item 3 asks for: a per-cycle hard deadline for the worker, enforced from outside by killing it |
| `s6` or `tini`/`dumb-init` for supervision and zombie reaping | **Not needed on a systemd host.** systemd is already the supervisor: it reaps, kills the whole control group on stop (`KillMode` defaults to `control-group`, so no detached process survives), and enforces the watchdog. The skeleton itself waits for every child it starts and kills its process group on timeout. `tini` or `dumb-init` matter only if the pattern ever runs as PID 1 in a container; record that as a deployment note. `s6` would add a second supervisor beside systemd, and a second source of restart decisions (HF-28, HF-32) |

**Item 2: kernel-level confinement and environment sanitization.**

| Point | Verdict |
| --- | --- |
| Strip inherited `GIT_WORK_TREE`, `GIT_DIR`, `GIT_OBJECT_DIRECTORY` before running subprocesses | **Already done, and more strongly.** Every child starts from a fixed allow-list (`SAFE_ENV`); nothing from the daemon's environment is inherited, so no variable, known or future, can redirect a call. r4.0 adds a test that proves it: plain Git follows a planted `GIT_DIR` to a decoy repository, and `proc.git` still reads the real one |
| Confine the survival loop with Landlock | **Already done for the whole daemon** (r2, DB-17 and DB-18: file access restricted at the kernel, TCP denied). One precision: Landlock restricts file access and TCP. Other network egress is closed by the unit (`PrivateNetwork=yes`, `RestrictAddressFamilies=AF_UNIX`, `IPAddressDeny=any`), so the two layers together close egress. A supervisor under AP-04 needs its own, separate Landlock domain |
| Move untrusted logic to WebAssembly (WASI) | **DEFER.** Deny-by-default capabilities are the right property. But today's worker runs `git` as a subprocess, which WASI cannot, and the Python-in-WASI toolchain is immature. Revisit on PD-59's trigger (a second kernel interface, or a measured need), and consider it first for pure `decide()` logic, which needs no I/O at all |

**Item 3: defensive streaming and resource fuel limits.**

| Point | Verdict |
| --- | --- |
| No `capture_output=True`; stream stdout with hard byte ceilings and terminate on breach | **Already done** (`proc.run`). Output is read in 64 KiB chunks against `max_bytes`; the process group is killed on breach or timeout; stderr is discarded; `capture_output` is never used. `test_output_is_bounded` and `test_timeout_kills_the_group` pin it |
| Hard caps on CPU (fuel) and I/O rates | **Partly present, and completed by the split.** Today: per-call timeouts, a CPU budget checked by DB-14, `CPUWeight`, `MemoryMax`, `TasksMax`, and the watchdog for a runaway main loop, which PD-70 now turns into `SENSE_BLIND` rather than an endless restart. Missing: a deterministic per-cycle cap on the daemon's own Python code. PD-38's sense budget bounds the ctx calls, and the AP-04 worker deadline bounds everything else. WASM fuel is deferred with item 2 |

**PD-72. Adopt the supervisor/worker split (AP-04) as a precondition for any active class.** The Blind Forester's survival loop runs in the supervisor, with its own Landlock domain, and the worker has a hard per-cycle deadline enforced by the supervisor. `s6` and `tini`/`dumb-init` are not adopted on systemd hosts; they are a deployment note for containers.
Recommendation: APPROVE. Decision: PENDING

**PD-73. Record item 2 as met, and defer WASI.** The allow-list environment is now pinned by a test, and Landlock plus the unit's network settings close egress. WASI waits for PD-59's trigger, starting with pure `decide()` logic.
Recommendation: APPROVE (record), DEFER (WASI). Decision: PENDING

**PD-74. Record item 3 as met for subprocesses; complete it with a per-cycle cap.** PD-38 (ctx budget) plus the AP-04 worker deadline, both in the phase 2 batch. WASM fuel is deferred with PD-73.
Recommendation: APPROVE. Decision: PENDING
