"""dir-watch with more than MAX_ENTRIES files: does adding ONE file report files as REMOVED
that are still present? (LTC-H01: a truncated payload must not be consumed as complete.)"""
import importlib.util, os, sys, tempfile
sys.path.insert(0, sys.argv[1])
from spark_daemon.context import Context

class Policy:
    def readable(self, path, follow=True):
        return path

spec = importlib.util.spec_from_file_location("dw", os.path.join(sys.argv[1], "examples/dir-watch/daemon.py"))
dw = importlib.util.module_from_spec(spec); spec.loader.exec_module(dw)
d = tempfile.mkdtemp(dir=sys.argv[2] if len(sys.argv) > 2 else None)
dw.WATCHED = d
ctx = Context(Policy(), step_timeout=10, tmp_dir=d)
for i in range(dw.MAX_ENTRIES + 88):
    open(os.path.join(d, f"f{i:05d}"), "w").close()
def sense():
    try:
        return dw.sense(ctx)
    except ctx.TooLarge:
        return None                                   # r4.3: a failed cycle, never an inventory
s1 = sense()
spurious = failed = 0
for k in range(20):
    open(os.path.join(d, f"new{k:03d}"), "w").close()
    s2 = sense()
    if s2 is None:
        failed += 1
        continue
    for kind, payload in dw.decide(s1, s2):
        if kind == "INBOX_ENTRIES_REMOVED":
            still = [n for n in payload["names"] if os.path.exists(os.path.join(d, n))]
            spurious += len(still)
            if k < 3:
                print(f"add new{k:03d}: REMOVED count={payload['count']}, still present on disk: {len(still)} of {len(payload['names'])} listed")
    s1 = s2
print(f"files on disk: {len(os.listdir(d))}; MAX_ENTRIES={dw.MAX_ENTRIES}; nothing was deleted")
print(f"spurious REMOVED names over 20 single-file additions: {spurious}")
print(f"failed cycles (TooLarge): {failed} of 20")
import shutil; shutil.rmtree(d)
print(f"truncated flag carried on REMOVED events: {'truncated' in payload if spurious else 'n/a'}")
