"""Probe 3 of SQLITE-LEDGER-REVIEW.md, run as an unprivileged reader (root ignores directory
permissions, so the first run of this probe as root proved nothing). A writer, as the invoking
user, creates a WAL database in a directory the reader may read but not write; the reader, run
as uid 65534 through setpriv, opens it read-only while the writer is open and after it closed."""
import os, shutil, sqlite3, stat, subprocess, sys, tempfile

READER = r'''
import sqlite3, sys
try:
    r = sqlite3.connect(f"file:{sys.argv[1]}?mode=ro", uri=True)
    print("rows:", r.execute("SELECT count(*) FROM t").fetchone()[0])
except sqlite3.Error as e:
    print("failed:", type(e).__name__, e)
'''
base = tempfile.mkdtemp(dir="/tmp")
os.chmod(base, 0o755)
d = os.path.join(base, "wal")
os.makedirs(d)
path = os.path.join(d, "ledger.db")
try:
    w = sqlite3.connect(path)
    w.execute("PRAGMA journal_mode=WAL")
    w.execute("CREATE TABLE t (v)")
    w.execute("INSERT INTO t VALUES (1)")
    w.commit()
    for f in os.listdir(d):
        os.chmod(os.path.join(d, f), 0o644)            # the reader may read every file
    os.chmod(d, 0o755)                                  # ...but not create files in the directory
    def read(label):
        p = subprocess.run(["setpriv", "--reuid=65534", "--regid=65534", "--clear-groups",
                            "/usr/bin/python3", "-c", READER, path], capture_output=True, text=True)
        print(f"{label}: files {sorted(os.listdir(d))}; reader {(p.stdout + p.stderr).strip()}")
    read("writer open")
    w.execute("INSERT INTO t VALUES (2)")
    w.commit()
    read("after a second commit, writer open")
    w.close()
    read("writer closed")
finally:
    shutil.rmtree(base)
