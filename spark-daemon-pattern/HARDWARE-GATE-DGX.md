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

## 3. Operator procedure (the pilot's runbook, G-6 at 3.1.0)

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
sudo -u fb python3 - <<'PY'
import json
from pathlib import Path
p = Path("/srv/firstborn/logs/daemons/pressure-watch/ledger.jsonl")
starts = []
with p.open("rb") as f:
    for line in f:
        if not line.endswith(b"\n"):      # a torn tail: never committed, quarantined at next start
            break
        obj = json.loads(line)
        if obj.get("event_type") == "DAEMON_START":
            starts.append(obj)
if not starts:
    raise SystemExit("STOP: no DAEMON_START record found")
print(json.dumps(starts[-1], indent=2, sort_keys=True))
PY
```
Keep the complete record. Acceptance needs `landlock.status == "enforced"` and no unexplained entry in `landlock.gaps`; also record `landlock.abi`, the manifest and code digests, the contract/skeleton identity, the clock fields, and (for v5) `harness` is `null`.

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
