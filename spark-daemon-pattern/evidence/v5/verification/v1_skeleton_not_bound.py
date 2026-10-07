"""V-1 (contract 5 verification, item 2): what a qualified unit pins does not include the skeleton.
Copies the pattern into a scratch folder, takes the digests a qualified unit carries, then
stubs out the audit hook in the copy's guard.py and starts the daemon with those digests.

Run from the pattern root: python3 -B evidence/v5/verification/v1_skeleton_not_bound.py
Expected today: exit 0 and a normal run (the gap). With a skeleton digest bound: exit 78."""
import os
import shutil
import subprocess
import sys
import tempfile

root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
scratch = tempfile.mkdtemp(prefix="v1-")
copy = os.path.join(scratch, "p")
shutil.copytree(root, copy, ignore=shutil.ignore_patterns("__pycache__", "evidence", "battery-runs"))
sys.path[:0] = [os.path.join(copy, "tests"), copy]
from helpers import Sandbox  # noqa: E402

sb = Sandbox("counter")
sb.write("data/value.txt", "x")
args = sb.qualified_args()
guard = os.path.join(copy, "spark_daemon", "guard.py")
with open(guard) as fh:
    source = fh.read()
marker = 'def install_audit_hook(policy: Policy, mode: str = "enforce") -> None:\n'
with open(guard, "w") as fh:
    fh.write(source.replace(marker, marker + "    return\n", 1))
r = subprocess.run([sys.executable, "-I", "-B", os.path.join(copy, "bin", "spark-daemon"), "run", "--manifest",
                    sb.manifest, "--max-cycles", "1", *args], env=sb.env(), capture_output=True, text=True, timeout=60)
print("qualified start, audit hook stubbed out: exit", r.returncode, "| events:",
      [x["event_type"] for x in sb.records()])
sb.cleanup()
shutil.rmtree(scratch)
