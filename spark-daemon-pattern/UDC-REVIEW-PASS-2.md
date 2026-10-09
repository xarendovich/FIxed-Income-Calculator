# UDC review pass 2 — the state machine and the manifest, by counterexample

- **Standing:** review-only, on this branch, at the owner's request. Nothing in 5.0.1 is changed. Every finding below is reproduced by a script under `evidence/v5/udc-pass2/` whose output is saved beside it; a claim without a reproduction is marked as reading.
- **Method, per the pass-1 charter:** attack the manifest and the runtime state machine with hostile values and boundary cases, prefer a counterexample or a deletion to an abstraction, and map each finding to one of the four laws. The six pass-2 questions were answered from the code in `UDC-PASS-1-ADJUDICATION.md` §3; this pass tries to break those answers.
- **Result in one paragraph.** The manifest's string surfaces are closed (two regex layers, both tested) and its numeric relations hold. The state machine has three over-statements, all of the same law (L2, evidence cannot overstate), none of them a confinement defect: `status` reports a silently killed daemon as `observing` for up to the blind limit; the home directory that decides what `~` paths mean is pinned by nothing and recorded nowhere, so one digest can watch two different directories and the ledger cannot say which; and every `DAEMON_START` stores a constant `qualified: true` that no reader uses and no reader should. Three items join the 6.0.0 cut; one deletion candidate goes to pass 3 with its cost named.

## 1. Findings

### P2-F1 — the home directory is a second owner of the path fact, and the ledger does not record it

**Reproduced:** `probe_two_homes.py`. The shipped `dir-watch` manifest declares `reads: ["~/spark-inbox"]`. Started twice with the same three expected digests under two values of `SPARK_DAEMON_HOME`, it recorded `INBOX_BASELINE` with 1 entry under home A and 7 under home B; both starts passed the digest gate; both ledgers carry the same `manifest_sha256`; no key of either `DAEMON_START` payload names the home, a read path or the output directory.

**What owns the fact today.** The manifest owns the *shape* (`~/spark-inbox`); the unit's `Environment=SPARK_DAEMON_HOME=` line owns the *meaning*; the report carries the judge's home as a unit input (the HF-41 fix); the runtime takes whatever the environment says; and `status` expands `~` with the *reader's* environment, which is why it has an `--output-dir` escape. Four readers, one fact, no pin. The `landlock` field of `DAEMON_START` records the ABI and `enforced`, not what was granted.

**Why this is not V-1 again, and not nothing.** V-1 is "the enforcer is not judged"; this is "what the enforcer was told is not evidenced". The three digests pin the manifest's text; the one value that turns that text into directories is outside every digest, and the ledger, which is supposed to be the record a reader can rely on without trusting the host's good day, is silent about it. L2.

**The simplest resolution is evidence, not a pin.** `DAEMON_START` records the resolved policy: the home it expanded with, the resolved reads, the output directory and the denied paths, as the runtime computed them from `PathPolicy.of(...)`. Then a reader sees what was granted, the differential between two homes is visible in the record, and no new flag exists. A pin (`--expect-home` beside the three digests) would only enforce consistency *inside* the unit file, which is the unpinned object M-3 already placed in G-6's hands, so it buys less than it costs.

**The deletion that would make the fact single-owned** is to forbid `~` in manifests: absolute paths only, `SPARK_DAEMON_HOME`, `paths.expand`, the report's `spark_daemon_home` input and `status --output-dir` all disappear, and the manifest digest pins the directories themselves. The FirstBorn pilot already uses absolute paths. The cost is the battery's design: today the battery runs the daemon in a throwaway home so that `~` lands inside the workspace, which is how the conformance checks run on any machine. With absolute paths the battery runs against the real directories, which is arguably what "battery on the DGX as the service user" should mean, but which checks need a populated read path is not known. **Pass 3 census:** which battery checks depend on the throwaway home. If none, delete `~` (U-9); if some, the evidence form above stands.

### P2-F2 — `status` reports a silently killed daemon as `observing` for up to the blind limit

**Reproduced:** `probe_status_liveness.py`. `meminfo-watch` started under the harness, allowed one accepted cycle, then `SIGKILL`ed. The ledger holds `DAEMON_START` and one observation, no `DAEMON_STOP`. `status --json` reports `state: observing`, `reason: null`, with `last_accepted_utc` set. The daemon's lock file exists and nothing holds it.

**Why.** `semantics.interpret` reads only the ledger: `not_watching` requires the last start or heartbeat to be older than the blind limit; until then, the last heartbeat's mode decides. So after any death without a `DAEMON_STOP` (SIGKILL, OOM, host crash, the systemd start limit giving up on an exit-70 loop), status overstates by up to `blind_limit_seconds`, which the manifest allows to be a day. The state's own definition, "alive and seeing", is the over-statement; the ledger can only say "alive as of the last heartbeat". L2.

**The simplest resolution is one fact, not a state.** The run lock already exists (`daemon.lock`, `flock LOCK_EX`, released by the kernel on death, exit 73 if held). `status` tries `LOCK_SH | LOCK_NB` on it: held means a process is alive in this output directory; free means none is; absent means this is not a live output directory (a copied ledger, the DGX bundle, a `cp`). Report it as `lock: held | free | absent`, and let `observing` require `held`: with `free`, the state is `not_watching` and the reason says "no process holds the run lock"; with `absent`, the ledger-only state is reported and the reader knows it is reading evidence, not a host. No new state, one syscall, one new status field (MINOR for `spark-daemon-status/1`; it rides 6.0.0 with the rest).

**Bound.** `blind` and `not_watching` as ledger states stay exactly as they are; the lock adds process liveness, which the ledger was never able to carry. The watchdog is the host's view of the same fact and remains outside the ledger (pass 1, question 2).

### P2-F3 — `DAEMON_START` stores a constant `qualified: true`

**Reading, verified by grep:** `runtime.py` writes `"qualified": True` into every start record. The runtime refuses to start without the expected digests, so the value is always `true` when it is written; no module, verifier or tool reads it. It is a stored claim of the exact kind the class-requirements review ruled out as an answer to consumer admission ("a stored `qualifying` field"), and the kind cut 2 removed from reports. L4 (a record must not certify itself) and the compression rule (a constant is not a fact).

**Resolution:** delete the key in 6.0.0 (a reserved-event payload change; readers that never used it lose nothing). The facts that *do* say a start was judged are the three digests in the same payload.

## 2. Attacks that found nothing, recorded so they are not repeated

| Surface | Attack | Held by | Test |
| --- | --- | --- | --- |
| `name`, `version`, `purpose`, `run_as.user`, event types, paths | Unit-file injection: newlines, `%` specifiers, spaces, quotes, `;` | Two layers: the manifest regexes (`NAME_RE`, `PURPOSE_RE`, `USER_RE`, `PATH_RE`, `EVENT_RE`) refuse them at load; `unitgen._unit_path` refuses them again at emission; `Description=` still escapes `%` defensively | `test_misc` and `test_hardening` raise `UnitError` on hostile paths; `test_manifest` on hostile fields |
| `reads` vs `output_dir` vs the base deny | Overlap, a read inside a denied tree, a symlinked parent | `PathPolicy` (I-5's no-overlap rule); three projections agree on every shipped manifest | `test_pathpolicy` (7), `test_manifest.test_reads_inside_denied_paths` |
| Numeric relations | `blind_limit < 3 × interval`, `cycle_budget > blind_limit / 2`, out-of-range bounds | `manifest.py` refuses both relations with the reason stated; every number is bounded, nothing defaults to infinity | `test_manifest`, `test_budget` |
| Heartbeat cadence | A cycle that starts just before the heartbeat is due and takes a whole budget | Worst gap is just under one blind limit; the reader's `not_watching` threshold is the limit, so the over-statement is in the safe direction (blind, not seeing), as `semantics.py` says | `test_blind` |
| Restart loop on a full disk | Exit 70 (uncertain commit) restarted forever | `StartLimitIntervalSec=300`, `StartLimitBurst=5`: systemd stops restarting; the daemon is then silently dead, which is P2-F2's case | reading |
| Failed cycles as blindness | A daemon whose `sense()` raises every cycle for days | `DAEMON_ERROR` repeats are collapsed; failed cycles are not accepted, so the blind clock runs; `SENSE_BLIND` (78, never restarted) at the limit. Correct: a daemon that cannot see is blind, however loudly it fails | `test_blind`, `test_runtime` |
| Restart continuity | Which record carries `last_accepted_utc` across a restart | `DAEMON_HEARTBEAT` and `DAEMON_STOP` carry it; `DAEMON_START` does not and anchors on `last_accepted_utc or first_start_utc`; so after a crash the last heartbeat decides, after a clean stop the stop does. Consistent with F2's model | `test_blind` (HF-32, HF-34) |

## 3. The state machine, as evidence

| State | What the ledger proves | What it does not prove | After this pass |
| --- | --- | --- | --- |
| `never_started` | No record | — | unchanged |
| `stopped` | A clean `DAEMON_STOP` or a fatal `DAEMON_ERROR` is last | — | unchanged |
| `not_watching` | No start or heartbeat within the limit | Whether a process exists | gains the lock as a second witness (P2-F2) |
| `blind` | Alive as of the last heartbeat, and not seeing | Alive now | unchanged in the ledger; `lock: free` overrides to `not_watching` |
| `observing` | Alive and seeing as of the last heartbeat | Alive now | requires `lock: held` when the lock is present (P2-F2) |

Five states remain. No event is derivable (pass 1, question 3). The one thing the vocabulary could never carry, process liveness, comes from the lock the runtime already holds.

## 4. The manifest, by owner, after this pass

| Owner | Fields | Finding |
| --- | --- | --- |
| Identity | `manifest_schema`, `name`, `version`, `purpose` | closed surfaces (§2) |
| Observation semantics | `reads`, `interval_seconds`, `cycle_budget_seconds`, `blind_limit_seconds`, `ledger.*` | `reads` has a second owner, the home (P2-F1) |
| Host binding | `output_dir`, `run_as.user`, `resources` | `output_dir` has the same second owner (P2-F1) |
| Profile-implied, deleted by U-1 | `daemon_class`, `network`, `trigger.kind` | — |
| Duplicate, deleted by U-7 | `digest` | — |

Pass 1's answer to question 6 stands and is sharper now: splitting host binding from observation semantics into two documents would not have found P2-F1 and would have added a second digest to the one fact that already has two owners.

## 5. What joins the 6.0.0 cut

Appended to the list in `UDC-PASS-1-ADJUDICATION.md` §4 as items 10–12; the order (deletions first, bundle digest last) is unchanged:

10. **P2-F3:** delete `qualified` from `DAEMON_START`.
11. **P2-F2:** `status` reports `lock: held | free | absent`; `observing` requires `held` when the lock is present; `free` reports `not_watching` with its reason.
12. **P2-F1:** `DAEMON_START` records the resolved policy (home, reads, output directory, denied paths). **Deletion candidate U-9** (absolute paths only; delete `~`, `SPARK_DAEMON_HOME` and `status --output-dir`) goes to pass 3 with its census.

Residual risks 8 and 9 in `HARDENING.md` record P2-F2 and P2-F1 against 5.0.1, so a DGX PASS at 5.0.1 is read with them, as residual 7 is.

## 6. For pass 3

- **The census for U-9:** which battery checks need the throwaway home. If none, `~` is deleted and three readers of one fact become one.
- **The reads a qualification actually exercises.** With `~`, the DGX battery exercises workspace copies, not the real directories; whether that is the right meaning of "battery on the DGX as the service user" is a question for the hardware-gate text, not for this pass.
- **The lock as a status witness on the DGX:** confirm `LOCK_SH | LOCK_NB` from the operator's user against a root-owned, mode 0600 lock file; if it cannot be opened, `lock: absent` is wrong and the field needs a fourth value, `unreadable`. That is a host fact to measure, not to design around in advance.
