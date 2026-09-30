"""r3.9 (HF-32): does blindness survive restarts?

A daemon that is always unsettled, with a 1.5 s blind limit (test override), is SIGKILLed every
1.0 s and started again, as a watchdog or out-of-memory kill followed by Restart= would do.
Expected if blindness survived restarts: a SENSE_BLIND record. Observed on r3.8: none, because
every start resets the blind clock. The control run, left alone, exits 78 as designed.

    python3 -B evidence/r3.9/restart_loop_repro.py
"""

import os
import signal
import subprocess
import sys
import time

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(ROOT, "tests"))

import helpers        # noqa: E402
import test_blind     # noqa: E402

src = test_blind.fixture(test_blind.MODE_DRIVEN)
sb = helpers.Sandbox(src)
try:
    sb.write("data/mode.txt", "busy")
    env = sb.env(SPARK_DAEMON_TEST_BLIND_LIMIT_MS="1500")
    cmd = [sys.executable, "-I", "-B", helpers.ENTRY, "run", "--manifest", sb.manifest]
    t0 = time.monotonic()
    for _ in range(4):
        p = subprocess.Popen(cmd, env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        time.sleep(1.0)
        p.send_signal(signal.SIGKILL)
        p.wait()
    blind_s = time.monotonic() - t0
    recs = sb.records()
    starts = [r["payload"]["previous_run_ended_cleanly"] for r in recs if r["event_type"] == "DAEMON_START"]
    blind = [r for r in recs if r["event_type"] == "DAEMON_ERROR" and r["payload"].get("category") == "SENSE_BLIND"]
    print(f"restart loop: {blind_s:.1f} s blind in total against a 1.5 s limit; DAEMON_START x{len(starts)} "
          f"(previous_run_ended_cleanly={starts}); SENSE_BLIND records: {len(blind)}")
    control = subprocess.run(cmd, env=env, capture_output=True, text=True, timeout=30)
    print(f"control, one uninterrupted run: exit {control.returncode}")
finally:
    sb.cleanup()
    import shutil
    shutil.rmtree(src, ignore_errors=True)
