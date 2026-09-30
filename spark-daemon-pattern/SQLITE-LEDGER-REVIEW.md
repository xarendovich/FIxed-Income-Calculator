# Review: a three-ledger SQLite event store, event-driven wake and Unix-socket IPC

- **Status:** REVIEW AND ADJUDICATION. Decisions PD-87 to PD-92 are adjudicated by Claude at the owner's request, subject to the owner (`ADJUDICATION-R4.5.md` sets out that authority). Nothing here changes code. Four probes were run on this host (`evidence/r4.5/`); every other statement is marked as reasoned.
- **Revision:** r4.5, 2026-09-30, by Claude.
- **Asked:** the owner asked for a review of recommendations "for hardening and improving the daemon pattern's code map":
  - an append-only SQLite event store split into three ledgers (ingestion and events; memory reconciliation; external metadata), for an offline runtime kernel watched by a Git-based observer daemon;
  - `systemd.path` watchers on the SQLite write-ahead log, instead of polling;
  - Unix-domain sockets for all IPC, with POSIX permissions;
  - an application-layer SHA-256 hash chain, with a schema, an ingestion protocol and a verification loop.

## 1. Verdict in brief

The direction is sound, and much of it matches rules this repository already enforces for its own ledgers:
- one writer per ledger;
- append-only records;
- a hash chain from a genesis of 64 zeros;
- verification by a passive observer.

Five points need changing before the design is safe. Three of them are shown by probes on this host:

| # | Point | Evidence | Change |
| --- | --- | --- | --- |
| 1 | **The proposed `payload JSON` column changes what is stored.** A declared type of `JSON` gets NUMERIC affinity in SQLite, so the text `123` is stored as the integer 123, and `1.50` as the real 1.5. The bytes the verifier reads are not the bytes that were hashed | **Measured** (`sqlite_probes.txt`, SQLite 3.45.1) | Declare `payload TEXT NOT NULL CHECK (json_valid(payload))` in a `STRICT` table, which kept `1.50` as text in the probe. Verify the stored bytes; never re-serialize |
| 2 | **Plain concatenation is ambiguous.** `previous_hash + timestamp + event_type + canonical_json(payload)`: event type `A_B` with payload `{"x":1}`, and event type `A_` with payload `B{"x":1}`, give the same input and the same hash | **Measured** (`sqlite_probes.txt`) | Hash the canonical (JCS) encoding of the whole record as one object, including a sequence number and the previous hash. That is what this repository's ledger does (`canonical.py`, `ledger.py`) |
| 3 | **A read-only observer cannot read the ledger after the writer closes.** While the writer is open, a confined reader (uid 65534, directory not writable) reads fine. When the last writer closes, SQLite removes `-wal` and `-shm`, and the reader fails with "attempt to write a readonly database". The observer loses sight of the ledger exactly when it matters most: after the kernel stopped or crashed | **Measured** (`sqlite_readonly_wal_probe.txt`) | The writer keeps the WAL files persistent (`SQLITE_FCNTL_PERSIST_WAL`), or the observer verifies a snapshot the writer exports, or reads with `immutable=1` only on a copy. Solve this before any read-only observer is built (PD-91) |
| 4 | **A hash chain alone does not stop someone who can write the file.** "Any altered bit breaks the link" is true only for an attacker who does not recompute the rows after the edit. Whoever can write the database can rewrite the chain from the edit onward | Reasoned (and the r4 known limit in WBS 3.0) | Anchor the head hash somewhere the writer cannot rewrite: the observer's Git commits, pushed off-host (PD-58). Also add `BEFORE UPDATE` and `BEFORE DELETE` triggers that abort, against accidents, not attackers |
| 5 | **The observer must not govern canonical memory.** "The passive observer daemon reads this ledger to resolve conflicts and govern the canonical memory state", and it "halts memory reconciliation" on corruption. Both are actions. An observer that resolves conflicts is at least an Advisor (class 2a), and halting is class 3 | Against recorded rules: PD-01 §3, S-6, and the Observer's "observation never authorizes action" | The observer verifies, reports and proposes. The kernel, or a gate, writes supersession records and decides to halt. The observer's corruption finding is an escalation record the kernel acts on (PD-87) |

## 2. The three ledgers

**Adopted with modification (PD-92): split by writer authority, and name each writer.** Separating operational metadata from semantic memory is right. What makes a split safe is that each ledger has exactly one writer, and that no component can write a ledger it should only read.

| Ledger | Proposed | Adjudicated |
| --- | --- | --- |
| Ingestion and events | Written only by the active kernel | **Agree.** The kernel is the single writer. Commits use `BEGIN IMMEDIATE`; in WAL mode `EXCLUSIVE` behaves the same. Each commit uses `synchronous=FULL`, because `NORMAL` in WAL mode keeps the database consistent but can lose the last commits on power loss, and a ledger that loses acknowledged records is not a ledger |
| Memory reconciliation | Read by the observer, which "resolves conflicts and governs" | **Modified.** The single writer is the kernel or a governance gate. The observer reads it, verifies supersession paths and proposes resolutions (class 2a). Validity intervals follow S-8 and the HF-34 lesson: they state their clock basis, and order comes from sequence, never from wall-clock timestamps |
| External metadata | Written by WASM plugins "via its socket" | **Agree, made explicit.** A small ledger service owns the file and is its single writer. Plugins send requests over a socket and never hold the file, so the service, not the plugin, computes the chain |

**Row rules for all three**, which are this repository's ledger rules applied to SQLite (PD-88):
- a `STRICT` table;
- `seq INTEGER PRIMARY KEY`, contiguous and checked by the verifier;
- `record TEXT NOT NULL`, holding the exact canonical bytes that were hashed;
- `previous_hash` and `current_hash`, each `CHECK (length(x) = 64 AND x NOT GLOB '*[^0-9a-f]*')`;
- `current_hash UNIQUE`;
- triggers that abort any `UPDATE` or `DELETE`;
- genesis 64 zeros;
- timestamps are informational and never order anything.

## 3. Event-driven wake with `systemd.path`

**Adopted with modification (PD-89): an event is a hint that shortens the wait; it is never the liveness signal.**

- **A path unit starts a unit; it cannot wake a resident process.** If the observer is a long-running daemon (this pattern's shape), a path unit's activation does nothing while the service is already running. It suits a oneshot script, not a resident daemon.
- **Events are coalesced and can be dropped.** Writes that arrive while the triggered unit is still running are coalesced. Path units are rate-limited in recent systemd (`TriggerLimitIntervalSec=`, `TriggerLimitBurst=`): when the limit trips, the unit fails and stops triggering. inotify itself can overflow its queue. So completeness can never rest on events.
- **Silence and death look the same.** With push-only wake, "nothing was written" and "the writer is dead" produce the same silence. This pattern's blind period and heartbeat exist to tell those apart (PD-63, PD-70), and they need a periodic cycle.
- **"The exact millisecond" overstates it.** A write to `-wal` is not yet a commit, so a woken reader can see nothing new (reasoned). In this pattern that case is `ctx.unsettled`: nothing stable was read.

**Design:**
- The resident daemon keeps its poll interval as its **floor**: the most it can be late, and its liveness proof.
- A path unit starts a tiny oneshot that sends the daemon a wake signal, and the daemon's sleep ends early.
- A missed or coalesced event costs at most one interval.
- Nothing inside the skeleton needs inotify. That matters because `ctypes` is confined to `landlock.py` and `probes.py` (`tests/test_layers.py`).

Build it when a daemon needs latency below its interval. No reference daemon does today.

## 4. Unix-domain sockets for IPC

**Adopted as execution-profile guidance, not a contract rule (PD-90).**

- **It belongs to the execution profile, not the contract.** Under the transport contract, the IPC mechanism is a provider and execution-profile choice (LTC-02 forbids a contract that requires one primitive, and PD-82 binds the pattern to the LTC at its edges). "Use UDS" therefore goes in the execution profile, not in the LTC or the daemon contract.
- **Pathname sockets only.** Put them in a protected directory and check the peer with `SO_PEERCRED`, as Package B's socket does (BG-6). Avoid abstract-namespace sockets: they have no file permissions. Where they cannot be avoided, Landlock ABI 6 and later can scope them.
- **POSIX permissions cannot separate plugins inside one process.** "A WASM plugin has write access to the metadata ledger via its socket but is physically denied the reconciliation ledger" holds only if each plugin runs as its own process with its own uid. Plugins hosted inside one runtime process share its uid and its open sockets. There the separation has to come from the WASM host's capability handles (WASI), not from file permissions (reasoned).
- **The latency claim is overstated.** UDS avoids the TCP/IP stack and is faster for small messages, but for bulk ingestion the difference is modest. The strong argument for UDS is security: no network exposure, kernel-checked peer identity, and Landlock can deny TCP entirely (DB-17 shows it does for daemons today).

## 5. What changes in the daemon pattern's code map

| Recommendation | Where it would land | When |
| --- | --- | --- |
| Hash-chain rules shared by every Spark ledger (PD-88) | `canonical.py` stays the reference encoding; the planned independent verifier (DB-22) is written to verify both this JSONL format and a SQLite table built to PD-88 | With DB-22 |
| A read-only SQLite capability for daemons (PD-91) | `context.py` gains `ctx.sqlite_rows(db, sql, params, max_rows)`: `SELECT` only, `mode=ro` URI, `PRAGMA query_only`, a progress-handler time limit, a row cap that raises `TooLarge` (PD-83), and bounded read transactions (a long read blocks checkpoints and grows the WAL). `manifest.py` declares the database paths. `landlock.py` needs read access to the database and its `-wal` and `-shm` | Deferred until the kernel's ledger exists and point 3 is solved |
| A reference observer for a SQLite chain | A fifth reference daemon, `sqlite-ledger-watch`, class 1a: it verifies, records the head and anchors it, and escalates on corruption. It never halts anything itself | With PD-91 |
| Wake hints (PD-89) | `runtime.py`: a signal ends the sleep early; `unitgen.py`: an optional path unit and oneshot; `manifest.py`: `trigger.wake_on` paths, with the interval required as the floor | When a daemon needs it |
| This pattern's own ledgers | **No change.** They stay JSONL with one fsync per record (PD-02, PD-56). Two formats are acceptable if one independent verifier checks both | — |

**New seams**, which join the reviewer-focus list:
- **N-15:** the observer, the WAL files and Landlock (point 3).
- **N-16:** the ledger service and the plugin sockets (single writer).
- **N-17:** the wake hint and the poll floor, where an event must never replace the liveness cycle.
- **N-18:** a head anchor that the kernel cannot rewrite.

## 6. Decisions

**PD-87. Observers verify and propose; they never govern.** The reconciliation ledger's writer is the kernel or a gate. The observer's corruption finding is an escalation the kernel acts on.
Verdict: **ADOPTED** (design rule for the kernel's memory store).

**PD-88. One set of hash-chain rules for every Spark ledger, SQLite included** (§2): canonical encoding of the whole record, stored bytes verified as stored, a `STRICT` schema, abort triggers, `synchronous=FULL`, sequence-ordered, head anchored off the writer's reach.
Verdict: **ADOPTED.**

**PD-89. Event wake is a hint; the interval is the floor** (§3).
Verdict: **ADOPTED WITH MODIFICATION** (hint via signal, not path-unit activation of a resident daemon). Build when needed.

**PD-90. UDS in execution profiles, with peer credentials; per-plugin separation needs per-plugin processes or capability handles** (§4).
Verdict: **ADOPTED as profile guidance.**

**PD-91. A read-only SQLite capability and a reference observer.**
Verdict: **DEFERRED.** Triggers: the kernel's ledger exists, and point 3 is solved and probed under Landlock on the DGX's SQLite version.

**PD-92. The three-ledger split, by writer authority, with each writer named** (§2).
Verdict: **ADOPTED WITH MODIFICATION.**
