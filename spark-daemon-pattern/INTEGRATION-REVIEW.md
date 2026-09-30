# Integration review: limits that collide, a reviewed comment, and one integration plan (r3.9)

- **Status:** REVIEW FOR ADJUDICATION. Two defects are fixed (HF-30, HF-31). One is reproduced and left open because its fix changes the ledger vocabulary (HF-32, PD-70). Everything else is a recommendation, and PD-69 to PD-71 are PENDING.
- **Revision:** r3.9, 2026-09-30, by Claude.
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

Recommendation: APPROVE; build in phase 2. Decision: PENDING

**PD-71. DB-14 becomes worst-case evidence** (section 2): a named worst-case fixture per observation type, a scaling series deriving `N_max`, measurement of the cgroup total, and headroom.
Recommendation: APPROVE; build in phase 2. Decision: PENDING
