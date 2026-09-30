"""PD-70 and a backward wall-clock step: does a slow restart loop still reach SENSE_BLIND?

A daemon that is blind on every cycle (MODE_DRIVEN with mode "busy") is killed and restarted
every 1.0 s against a 1.5 s blind limit, the HF-32 scenario. Control: every run sees the real
clock. Case: the first run sees the real clock; every later run sees it one hour behind, as
after a backward NTP step or a manual correction. The shift is applied by wrapping
datetime.datetime in the launched process only; the system clock is not changed.
Run from spark-daemon-pattern/ with the self-test interpreter."""
import os, shutil, subprocess, sys, tempfile
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "tests"))
from helpers import ENTRY, Sandbox  # noqa: E402

MODE_DRIVEN = '''
def sense(ctx):
    if ctx.read_text("~/data/mode.txt").strip() == "busy":
        return ctx.unsettled("BUILD_RUNNING")
    return {"value": ctx.read_text("~/data/value.txt").strip()[:50]}

def decide(prev, snapshot):
    if prev == snapshot:
        return []
    return [("VALUE_OBSERVED", {"value": snapshot["value"]})]
'''
SHIFTED = ("import datetime as d, runpy, sys\n"
           "real = d.datetime\n"
           "class Behind(real):\n"
           "    @classmethod\n"
           "    def now(cls, tz=None):\n"
           "        return real.now(tz) - d.timedelta(seconds={shift})\n"
           "d.datetime = Behind\n"
           "sys.argv = [{entry!r}] + sys.argv[1:]\n"
           "runpy.run_path({entry!r}, run_name='__main__')\n")


def trial(shift_after_first):
    src = tempfile.mkdtemp()
    fixture = os.path.join(os.path.dirname(__file__), "..", "..", "tests", "fixtures", "daemons", "counter")
    shutil.copyfile(os.path.join(fixture, "manifest.json"), os.path.join(src, "manifest.json"))
    with open(os.path.join(src, "daemon.py"), "w") as fh:
        fh.write(MODE_DRIVEN)
    sb = Sandbox(src)
    sb.write("data/mode.txt", "busy")
    codes = []
    try:
        for i in range(6):
            shift = shift_after_first if i else 0
            cmd = [sys.executable, "-I", "-B", "-c", SHIFTED.format(shift=shift, entry=ENTRY),
                   "run", "--manifest", sb.manifest]
            p = subprocess.Popen(cmd, env=sb.env(SPARK_DAEMON_TEST_BLIND_LIMIT_MS="1500"),
                                 stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)
            try:
                codes.append(p.wait(timeout=1.0))
                p.communicate()
                break
            except subprocess.TimeoutExpired:
                p.kill()
                p.communicate()
                codes.append("killed")
        starts = [r["payload"]["inherited_blind_ms"] for r in sb.records() if r["event_type"] == "DAEMON_START"]
        blind = [r for r in sb.records() if r["event_type"] == "DAEMON_ERROR"
                 and r["payload"].get("category") == "SENSE_BLIND"]
        return codes, starts, len(blind)
    finally:
        sb.cleanup()
        shutil.rmtree(src)


for label, shift in (("control: real clock throughout", 0), ("clock stepped back 1 h after run 1", 3600)):
    codes, inherited, blind = trial(shift)
    print(f"{label}:\n  exits: {codes}\n  inherited_blind_ms per start: {inherited}\n  SENSE_BLIND records: {blind}")
