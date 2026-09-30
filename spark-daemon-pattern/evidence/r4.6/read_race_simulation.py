"""F-1: the window between ctx.read_text's path check and its open(), simulated
deterministically. Policy.readable() resolves the path (realpath) and checks it against the
declared reads and denies; read_text() then calls open() on that resolved string. If a
directory component is replaced by a symlink in between, the kernel follows it. Here the
manifest reads ~/data and denies ~/data/secret (a "gap": Landlock cannot deny a subtree of a
granted read, so only the in-process check and, under systemd, InaccessiblePaths= cover it)."""
import os, shutil, sys, tempfile
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))
from spark_daemon import guard

home = tempfile.mkdtemp()
os.environ["SPARK_DAEMON_HOME"] = home
try:
    os.makedirs(os.path.join(home, "data", "dir"))
    os.makedirs(os.path.join(home, "data", "secret"))
    with open(os.path.join(home, "data", "dir", "f.txt"), "w") as fh:
        fh.write("ordinary")
    with open(os.path.join(home, "data", "secret", "f.txt"), "w") as fh:
        fh.write("SECRET")

    class M:                                   # the three fields guard.Policy reads
        output_dir, reads, all_deny, commands = "~/out", ("~/data",), ("~/data/secret",), ()
    policy = guard.Policy(M())
    try:
        policy.readable("~/data/secret/f.txt")
    except guard.GuardError as e:
        print("direct read of the denied path:", e)
    full = policy.readable("~/data/dir/f.txt")         # the check passes...
    os.rename(os.path.join(home, "data", "dir"), os.path.join(home, "data", "dir.old"))
    os.symlink(os.path.join(home, "data", "secret"), os.path.join(home, "data", "dir"))
    with open(full, "rb") as fh:                         # ...and the open that follows it
        print("read through the swapped directory:", fh.read().decode())
    fd = os.open(os.path.join(home, "data"), os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.open("dir", os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=fd)
    except OSError as e:
        print("the same step with O_NOFOLLOW on each component:", type(e).__name__, os.strerror(e.errno))
    finally:
        os.close(fd)
finally:
    shutil.rmtree(home)
