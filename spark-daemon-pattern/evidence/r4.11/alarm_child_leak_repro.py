"""Two mechanisms enforce the cycle budget (r4.11, R-6): proc.run's own timeout, and the cycle's
alarm. If a command closes its stdout but keeps running, proc.run waits for it in its finally
block; an alarm that fires during that wait leaves the block before the process group is killed.
Runs `sh -c 'exec 1>&-; sleep 30'` under a 0.3 s alarm and reports whether the sleeper survives."""
import os
import signal
import subprocess
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
from spark_daemon import proc  # noqa: E402
from spark_daemon.context import CycleBudgetExceeded  # noqa: E402


def over(_s, _f):
    raise CycleBudgetExceeded("budget")


signal.signal(signal.SIGALRM, over)
marker = f"sleep {30 + os.getpid() % 7}"
signal.setitimer(signal.ITIMER_REAL, 0.3)
try:
    proc.run(["sh", "-c", f"exec 1>&-; {marker}"], executables={"sh": "/bin/sh"}, timeout=0.3, max_bytes=100)
    print("returned normally")
except CycleBudgetExceeded:
    print("alarm raised out of proc.run")
time.sleep(0.5)
alive = subprocess.run(["pgrep", "-f", marker], capture_output=True, text=True).stdout.split()
print(f"child still running after the alarm: {bool(alive)} {alive}")
for pid in alive:
    os.kill(int(pid), signal.SIGKILL)
