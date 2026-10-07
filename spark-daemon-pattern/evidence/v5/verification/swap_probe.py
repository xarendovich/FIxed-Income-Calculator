"""Run from tests/: python3 -B ../evidence/v5/verification/swap_probe.py "$PWD". Contract 5 verification, item 3."""
"""F-1 probe: a daemon that keeps reading ~/data/value.txt while ~/data is swapped for a symlink
into ~/.ssh after start-up. Records what it read."""
import json, os, shutil, subprocess, sys, tempfile, time
sys.path.insert(0, sys.argv[1])
from helpers import Sandbox, FIXTURES
src = tempfile.mkdtemp()
data = json.load(open(os.path.join(FIXTURES, "counter", "manifest.json")))
json.dump(data, open(os.path.join(src, "manifest.json"), "w"))
open(os.path.join(src, "daemon.py"), "w").write(
    "def sense(ctx):\n    try:\n        return {'value': ctx.read_text('~/data/value.txt', max_bytes=200)}\n"
    "    except ctx.Missing:\n        return {'value': 'missing'}\n\n\n"
    "def decide(prev, snapshot):\n    return [('VALUE_OBSERVED', {'value': str(snapshot['value'])[:200]})]\n")
sb = Sandbox(src)
sb.write("data/value.txt", "public")
os.makedirs(os.path.join(sb.home, ".ssh"))
open(os.path.join(sb.home, ".ssh", "value.txt"), "w").write("SECRET-KEY")
p = subprocess.Popen(sb.argv(40), env=sb.env(), stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)
time.sleep(1.0)
real = os.path.join(sb.home, "data")
os.rename(real, real + ".old")
os.symlink(os.path.join(sb.home, ".ssh"), real)            # the swap, after start-up
_, err = p.communicate(timeout=60)
values = [r["payload"].get("value") for r in sb.records() if r["event_type"] == "VALUE_OBSERVED"]
errors = [r["payload"] for r in sb.records() if r["event_type"] == "DAEMON_ERROR"]
print("exit", p.returncode, "| observed:", values, "| errors:", errors[:3])
print("SECRET read:", any("SECRET" in str(v) for v in values))
sb.cleanup(); shutil.rmtree(src)
