"""PD-76 feasibility: can this host take a direct cgroup memory reading without a systemd
user manager, and how does it compare with DB-14's per-process basis?

Run as root. Creates a child memory cgroup under this shell's own cgroup (v1 here, or v2
where mounted), moves a fresh process into it, has it hold 80 MiB while a child allocates
120 MiB, then reads the cgroup's peak and removes the cgroup."""
import os, resource, subprocess, sys

def own_memory_cgroup():
    for line in open("/proc/self/cgroup"):
        hid, ctrl, path = line.rstrip("\n").split(":", 2)
        if ctrl == "memory":
            return "/sys/fs/cgroup/memory" + path, "memory.max_usage_in_bytes", "v1"
        if hid == "0" and os.path.exists("/sys/fs/cgroup/cgroup.controllers"):
            return "/sys/fs/cgroup" + path, "memory.peak", "v2"
    sys.exit("no memory cgroup found")

if len(sys.argv) > 1 and sys.argv[1] == "--inside":
    held = bytearray(80 * 1024 * 1024)
    subprocess.run([sys.executable, "-c", "x = bytearray(120 * 1024 * 1024)"], check=True)
    self_kib = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    child_kib = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
    print(f"per-process peaks: daemon {self_kib // 1024} MiB, largest child {child_kib // 1024} MiB")
    print(f"DB-14 basis today, max(daemon, child): {max(self_kib, child_kib) // 1024} MiB")
    sys.exit(0)

base, peak_file, version = own_memory_cgroup()
probe = os.path.join(base, f"spark-pd76-probe-{os.getpid()}")
os.mkdir(probe)
try:
    proc = subprocess.Popen(["sh", "-c", f'echo $$ > {probe}/cgroup.procs && exec "$0" "$1" --inside',
                             sys.executable, os.path.abspath(__file__)])
    proc.wait()
    with open(os.path.join(probe, peak_file)) as fh:
        peak = int(fh.read())
    print(f"cgroup {version} {peak_file} (daemon + every child): {peak // (1024 * 1024)} MiB")
finally:
    os.rmdir(probe)
