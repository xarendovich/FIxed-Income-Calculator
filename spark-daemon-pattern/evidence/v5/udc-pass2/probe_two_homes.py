"""P2-F1 probe: one manifest (one digest) watching `~/spark-inbox`, started under two values of
SPARK_DAEMON_HOME. Both starts pass the digest gate; the ledgers cannot say which directory was read."""
import json, os, shutil, subprocess, sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "..", "tests"))
from helpers import Sandbox
sb = Sandbox(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "examples", "dir-watch")))
try:
    homes = {}
    for tag, nfiles in (("A", 1), ("B", 7)):
        home = os.path.join(sb.tmp, f"home-{tag}")
        os.makedirs(os.path.join(home, "spark-inbox"))
        os.makedirs(os.path.join(home, "spark-core", "data"))
        for i in range(nfiles):
            open(os.path.join(home, "spark-inbox", f"f{i}.txt"), "w").write("x")
        env = sb.env(HOME=home, SPARK_DAEMON_HOME=home)
        p = subprocess.run(sb.argv(cycles=2, interval_ms=100), env=env, capture_output=True, text=True, timeout=60)
        out = os.path.join(home, "spark-daemons", "dir-watch", "ledger.jsonl")
        recs = [json.loads(l) for l in open(out, "rb") if l.endswith(b"\n")] if os.path.exists(out) else []
        start = next((r for r in recs if r["event_type"] == "DAEMON_START"), {})
        evs = [(r["event_type"], r.get("payload", {}).get("entries", r.get("payload", {}).get("count"))) for r in recs
               if r["event_type"] not in ("DAEMON_START", "DAEMON_STOP", "DAEMON_HEARTBEAT")]
        homes[tag] = {"exit": p.returncode, "manifest_sha256": start.get("payload", {}).get("manifest_sha256"),
                      "start_payload_mentions_home_or_paths": any(k for k in start.get("payload", {}) if "home" in k or "path" in k or "read" in k),
                      "observed_events": evs[:3], "stderr_tail": p.stderr.strip().splitlines()[-1:] }
    print(json.dumps(homes, indent=1))
finally:
    sb.cleanup()
