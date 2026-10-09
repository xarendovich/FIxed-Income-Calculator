"""P2-F2 probe: kill a running daemon silently (SIGKILL), then ask `status`.
The ledger has a DAEMON_START and heartbeats but no DAEMON_STOP; status reads only the ledger."""
import json, os, signal, subprocess, sys, time
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "..", "tests"))
from helpers import Sandbox
sb = Sandbox(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "examples", "meminfo-watch")),
             {"blind_limit_seconds": 600, "trigger": {"kind": "poll", "interval_seconds": 60}})
try:
    p = subprocess.Popen(sb.argv(cycles=None, interval_ms=200), env=sb.env(),
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    deadline = time.time() + 20
    while time.time() < deadline and not any(r["event_type"] == "DAEMON_START" for r in sb.records()):
        time.sleep(0.2)
    time.sleep(1.0)                                   # a few cycles
    os.kill(p.pid, signal.SIGKILL); p.wait()
    kinds = [r["event_type"] for r in sb.records()]
    s = sb.cli("status", "--manifest", sb.manifest, "--json")
    st = json.loads(s.stdout)
    print(json.dumps({"process": "SIGKILLed, exit", "returncode": p.returncode, "ledger_events": kinds,
                      "status_state": st.get("state"), "reason": st.get("reason"),
                      "last_accepted_utc": st.get("last_accepted_utc"),
                      "lock_file_exists": os.path.exists(os.path.join(sb.output, "daemon.lock"))}, indent=1))
finally:
    sb.cleanup()
