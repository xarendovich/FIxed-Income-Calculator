# Open hardware gate: DGX Spark under the real host systemd

- **Status:** OPEN. Requires the physical NVIDIA DGX Spark. Nothing in this repository can close it.
- **Source:** the independent reviewer's specification (2026-10-07), adopted here with the adjudication notes in §0. Companion: `REVIEW-PACKAGE-V5.md` §7, `ADVERSARIAL-REVIEW-HANDOFF-V5.md` §6, `ADJUDICATION-V5.md` (pilot row).
- **Wording, corrected across the package:** the daemon is **not** PID 1. The condition is that it runs as a service under the **real host `systemd`, with `systemd` itself as PID 1**. Earlier wording ("a real PID 1 start", "runs under PID 1") meant this and now says it.

## 0. Adjudication of the reviewer's specification

| Point | Verdict | Note |
| --- | --- | --- |
| PID-1 wording | **ACCEPT** | Corrected in README, DAEMON-CONTRACT, REVIEW-PACKAGE-V5, VERIFICATION-V5, ADVERSARIAL-REVIEW-HANDOFF-V5, ADJUDICATION-V5 |
| G-6 (3.1.0 pilot) is distinct from contract 5.0.0 qualification | **ACCEPT** | Consistent with v5's design: a report binds the host *and* the contract digest, so a 3.1.0 run cannot be evidence for 5.0.0 by construction, not only by policy |
| The ten things the run must prove | **ACCEPT** | Nine are observable in the ledger, `systemctl show` and `/proc`; the tenth (no perturbation of FirstBorn/Spark workloads) needs the operator's judgement and the DB-14 resource lines |
| `INCOMPLETE` is insufficient | **ACCEPT, with a prerequisite made explicit** | DB-08/DB-09 report UNKNOWN without `strace`, and DB-15 without `systemd-analyze`; either makes the verdict INCOMPLETE. **The DGX host needs both installed before the run** (§2) |
| Operator procedure (FirstBorn commands and paths) | **ACCEPT as the pilot's runbook** | §3 is FirstBorn-operational (`fb unlock`, `/srv/firstborn/...`). It lives here because the gate is run there; nothing in the pattern's code or tests depends on it, which keeps the two projects unlinked. **Owner's call** whether it should move to the pilot's own repository instead |
| Step 7's ledger read | **ACCEPT, hardened** | Plain `json.loads` per line fails on a torn tail (a partial last line after a crash, which the next start quarantines). §3.7 skips an unterminated last line, so the evidence script cannot fail on exactly the condition the ledger is designed to survive |
| R-5c measurement collected, not acted on | **ACCEPT** | Matches `ADJUDICATION-V5.md`: the trigger is start-up verification projected past half the start timeout at the real record rate |
| Status wording before/after each run | **ACCEPT verbatim** | §6 |

**One addition for the v5 run (§5).** Contract 5's report binds the host it was made on *and* the home of whoever ran the battery (HF-41). So the v5 battery must be run **on the DGX itself, as the service's user** (the one whose `~` the manifest means), never under `sudo` from an operator's account and never on another machine: `unit --report` refuses a report from another host, and a report made as root would name `/root`.

## 0b. Where the click-path lives: adjudication of the two reviews (2026-10-08)

Two reviewers answered the §0 question ("owner's call whether §3 moves to the pilot's repository"). They agree, and so do I: **the standard stays here; the FirstBorn binding moves to the pilot; §3 stays until the pilot's copy exists.**

| Point | Verdict | Note |
| --- | --- | --- |
| Split by who owns the fact (LS-1: each fact once) | **ACCEPT** | §§1, 4, 5, 6, 7 are the standard and do not move. §3 is a visit procedure; its FirstBorn facts (`fb unlock`, `/srv/firstborn/…`, user `fb`, the unit name) belong to the pilot's schedule, not the freeze candidate's |
| "The code is unlinked, the document is not" | **ACCEPT** | My §0 note was true of tests and false of prose. Reviewers will read a FirstBorn map in the pattern as normative; R-8's point is exactly that one project's names do not live in the core |
| Phase 1 today: do not delete §3; mark it a holding copy | **ACCEPT, done** | The notice is at the top of §3. A move that is not committed in the pilot repo is a deletion, and this is the only reviewed procedure |
| Phase 2: pilot owns the click-path, pinned to a pattern commit and contract digest; then §3 here becomes the slot table | **ACCEPT** | §3a below is that slot table, written now so Phase 2 is a deletion, not a rewrite. The pilot's runbook must record `pattern_sha` and `contract_sha256`; if either moves it is stale by construction, which is how v5 already binds a report |
| Lock the ledger-reading semantics in the pattern as a tool, not prose | **ACCEPT, done** | `tools/daemon-start.py`: standard library, imports nothing from the pattern, torn tail reported and never parsed, a bad line is exit 2 and never a traceback (HF-43), no schema-string check so it reads a renamed vendored ledger too. Tested in `tests/test_tools.py`. §3.7 now calls it; the inline snippet is gone |
| "`cat`/`cp` violate the locking invariants" (reviewer 1) | **CORRECT THE CLAIM** | They do not. The single-instance lock is a separate `flock` file, and read-only verification is a tested guarantee (DB-16: bytes and mtime unchanged). The reason to use the tool is parse semantics, not locks: a torn tail, and a line that must not crash the reader |
| A `verify-bundle` checker in the pattern (reviewer 1) | **DEFER, right direction** | §4's bundle and §5's acceptance as an executable check is the natural next step after the first bundle exists; not a prerequisite for the DGX visit |
| `make dgx-runbook` concatenating the frozen gate and the pilot annex (reviewer 1) | **ACCEPT for the pilot repo, with one constraint** | Belongs there, not here. It must pin by commit SHA and must not put the word "spark" into any FirstBorn path or filename (the owner's standing rule); the output name is the pilot's |
| Do not copy §3 into both repos; do not build a click-path from `unit --plan` before this visit (reviewer 2) | **ACCEPT** | N-23 again, and not a prerequisite |

## 1. What the run must prove

The guarantees already proven in tests must survive the real deployment boundary:

1. The conformance battery passes on the DGX kernel.
2. Landlock is enforced by the kernel, not merely configured.
3. The generated unit starts under the real service manager.
4. The sandbox, watchdog, resource and restart settings are accepted by the host.
5. `DAEMON_START` truthfully reports the host's confinement state.
6. The daemon reaches `active (running)` and is observing.
7. It creates and continues a valid hash-chained ledger.
8. Independent ledger verification succeeds without modifying the ledger.
9. The daemon survives ordinary service lifecycle operations without leaving an unexplained gap.
10. Resource use is compatible with the host and does not interfere with FirstBorn/Spark workloads.

A battery PASS stays qualification evidence only; it grants no activation authority (invariant I-5).

## 2. Prerequisites on the host

- NVIDIA DGX Spark, `aarch64`; the production Ubuntu host, not a container or a CI VM.
- `systemd` as host PID 1.
- The DGX's own Landlock (expected ABI by DGX OS release: 7.2.3 → 5; 7.3.1 → 6; 7.4.0/7.5.0 → 7; 7.6.0 → 8 — inferred from the kernel ladder, confirm on the box).
- Python from the DGX environment (3.12.3 recorded).
- **`strace` and `systemd-analyze` installed**, or DB-08/09/15 report UNKNOWN and the verdict is INCOMPLETE, which does not pass this gate.
- FirstBorn's real filesystem ownership, drive mount and service layout (for G-6).

This gate is deliberately not replaced by emulation or by the x86_64 evidence in `evidence/`.

## 3a. The host interface: what a runbook must bind

The pattern's requirements of any host runbook. The pilot's runbook (§3, until it moves) is one binding of these slots; the next consumer writes its own.

| Slot | Requirement | FirstBorn's binding (3.1.0 visit) |
| --- | --- | --- |
| `pattern_sha` | the pattern commit the runbook was written against | recorded in the pilot's runbook |
| `contract_sha256` | the contract digest this visit qualifies against; a 3.1.0 run cannot be cited for 5.0.0 | 3.1.0's; 5.0.0 is `2d080b40…` |
| service user | the user whose `~` the manifest means; **never root**, never `sudo` from an operator account (HF-41) | `fb` |
| unit name | what the real service manager instantiated (§3.5, §3.6) | `firstborn-pressure-watch.service` |
| manifest path | the manifest the battery and the runtime bind | `/srv/firstborn/repo/host/daemons/pressure-watch/manifest.json` |
| ledger path | read only with `tools/daemon-start.py` (§3.7) and the host's own verifier (§3.8) | `/srv/firstborn/logs/daemons/pressure-watch/ledger.jsonl` |
| tools on the host | `strace`, `systemd-analyze` (§2) | the operator confirms in §3.1 |
| visit | G-6 at 3.1.0, or the later 5.0.0 qualification (§5) | G-6 at 3.1.0 |

## 3. Operator procedure (the pilot's runbook, G-6 at 3.1.0)

> **HOLDING COPY.** This section is one host's binding of §3a (FirstBorn, 3.1.0). It is kept in the pattern only until the pilot repository carries it, pinned to this commit and to the contract digest (§0b, Phase 2). Then it is deleted here and §3a remains. Any other consumer writes its own §3 against §3a; nothing in the pattern's code or tests depends on this text.

Record exact values at every step; never substitute expected ones.

**3.1 Host identity**
```
uname -m; uname -r; cat /etc/os-release; python3 --version
systemctl --version | head -1
ps -p 1 -o pid=,comm=,args=
stat -fc %T /sys/fs/cgroup
which strace systemd-analyze
```
Required: `aarch64`; PID 1 is `systemd`; both tools present.

**3.2 The FirstBorn drive**
```
fb unlock
test -e /srv/firstborn/.firstborn-volume && echo "PASS: drive unlocked" || echo "STOP: drive unavailable"
```
Stop if unavailable.

**3.3 Bring FirstBorn to the pilot state**
```
fb update          # expect: OK: The update is finished
fb update          # expect: already current, update completed
```
If the installed `fb` predates the consolidated workflow, follow what it prints; do not skip steps by hand.

**3.4 Preserve the battery result**
The installer runs the `pressure-watch` battery as the `fb` user and installs the unit only on PASS. For a direct run:
```
sudo -u fb python3 -I -B /srv/firstborn/repo/host/daemons/bin/fb-daemon battery \
  --manifest /srv/firstborn/repo/host/daemons/pressure-watch/manifest.json
```
Required: `RESULT: PASS`. Keep the whole output and the report, every DB line, the Landlock lines especially. INCOMPLETE does not pass.

**3.5 The real service**
```
sudo systemctl status firstborn-pressure-watch.service --no-pager
sudo systemctl show firstborn-pressure-watch.service -p ActiveState -p SubState -p MainPID \
  -p Restart -p RestartPreventExitStatus -p WatchdogUSec -p TasksMax -p MemoryMax -p CPUWeight
```
Required: `ActiveState=active`, `SubState=running`, `MainPID` nonzero. This is about what the service manager instantiated, not what the unit file says.

**3.6 The process is in systemd's cgroup**
```
PID="$(systemctl show -p MainPID --value firstborn-pressure-watch.service)"; echo "MainPID=$PID"
cat "/proc/$PID/cgroup"
```

**3.7 The actual `DAEMON_START`** (the ledger is authoritative, not the journal)
```
sudo -u fb python3 -I -B /path/to/pattern/tools/daemon-start.py \
  /srv/firstborn/logs/daemons/pressure-watch/ledger.jsonl
```
`daemon-start.py` is one standard-library file (copy it to the host; it imports nothing). It reads the ledger the way the ledger is designed to be read: a torn tail (an uncommitted partial last line after a crash, which the next start quarantines) is reported on stderr and never parsed; a committed line that does not parse is exit 2 with its position, never a traceback. It checks no schema string, so it also reads the 3.1.0 pilot's renamed ledger. It verifies nothing; §3.8 does.

Keep the complete record. Acceptance needs `landlock.status == "enforced"` and no unexplained entry in `landlock.gaps`; also record `landlock.abi`, the manifest and code digests, the contract/skeleton identity, the clock fields, and (for v5) that `harness` is `null`.

**3.8 Verify the ledger independently** (must not change bytes or mtime)
```
# 3.1.0 pilot:
sudo -u fb python3 -I -B /srv/firstborn/repo/host/daemons/bin/fb-daemon verify \
  --manifest /srv/firstborn/repo/host/daemons/pressure-watch/manifest.json
# contract 5, after migration:
#   spark-daemon status --verify-only --manifest <manifest>
```

**3.9 The daemon is observing** (after at least one interval)
```
sudo systemctl status firstborn-pressure-watch.service --no-pager
sudo -u fb tail -n 20 /srv/firstborn/logs/daemons/pressure-watch/ledger.jsonl
sudo -u fb sed -n '1,160p' /srv/firstborn/logs/daemons/pressure-watch/digest.md
```
The required fact is accepted cycles and heartbeats, with no `SENSE_BLIND` and no policy failure. A quiet host legitimately produces no threshold event.

**3.10 One ordinary lifecycle** (no fault injection for this gate)
```
sudo systemctl restart firstborn-pressure-watch.service; sleep 5
sudo systemctl status firstborn-pressure-watch.service --no-pager
```
Then verify the ledger again. The restart must be visible in the ledger (`DAEMON_STOP`, then `DAEMON_START` with `previous_run_ended_cleanly`) and must not reset or rewrite history.

## 4. The evidence bundle

One bundle, original output preserved, nothing summarised into PASS:

1. `uname -m`; 2. `uname -r`; 3. `/etc/os-release`; 4. `python3 --version`; 5. `systemctl --version` and the PID-1 proof; 6. cgroup mode; 7. the exact FirstBorn Git SHA; 8. the exact daemon contract/version used; 9. the complete battery output and report; 10. the generated/installed unit's identity; 11. `systemctl status`/`show` output; 12. MainPID and `/proc/<pid>/cgroup`; 13. the complete `DAEMON_START` record; 14. explicitly: `landlock.status`, `landlock.abi`, `landlock.gaps`; 15. the ledger verification result; 16. a short ledger tail after ordinary running; 17. the restart result and the post-restart verification; 18. every failure, UNKNOWN, N/A or warning exactly as emitted (DB-24 N/A with no envelope is expected and explained).

## 5. Acceptance

PASS only when all hold: `aarch64`; real host `systemd` is PID 1; battery PASS; the daemon runs as an actual systemd service; `DAEMON_START.landlock.status == "enforced"`; no unexplained Landlock gap; ledger verification succeeds; ordinary accepted cycles and heartbeats occur; no unexpected policy violation or `SENSE_BLIND`; a normal restart leaves understandable, verifiable lifecycle evidence; nothing suggests the observer perturbs FirstBorn/Spark. Otherwise the gate stays OPEN.

**Two runs, kept apart:**

```
G-6 / pilot hardware proof
        ↓
pressure-watch 3.1.0 on the real DGX / host systemd
        ↓
closes the basic hardware/systemd uncertainty
        ↓
FirstBorn migrates directly to contract 5
        ↓
contract 5.0.0 qualification repeated on the DGX
   (battery run on the DGX, as the service's user; unit from `unit --report`)
```

The 3.1.0 run must not be cited as DGX qualification evidence for contract 5.0.0.

## 6. R-5c measurement (collect; do not act)

While the hardware is available, record: ledger bytes, record count, full start-up verification wall time, and the daemon's record growth rate. The v5 rule stays full start-up verification. R-5c reopens only if the projection at the real growth rate exceeds half the start timeout. No checkpoint mechanism is built because this number was collected.

## 7. Status wording

- **Before hardware evidence:** contract 5.0.0 is independently reviewed and supported by clean-checkout software evidence, but DGX Spark hardware/systemd qualification remains open.
- **After the 3.1.0 G-6 run only:** the daemon pattern has passed its first real DGX Spark/aarch64/host-systemd run through the FirstBorn 3.1.0 pressure-watch pilot. This closes the general G-6 uncertainty but is not contract 5.0.0 DGX qualification.
- **After a later v5 run:** contract 5.0.0 has been exercised on the DGX Spark under the real aarch64 kernel and host service manager; the evidence bundle identifies the exact contract, code, unit, `DAEMON_START` and battery result used.
