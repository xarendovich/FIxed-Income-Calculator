"""Probes behind SQLITE-LEDGER-REVIEW.md: three properties of the proposed SQLite ledger design,
checked on this host's SQLite rather than assumed."""
import hashlib, json, os, shutil, sqlite3, stat, tempfile

print("sqlite", sqlite3.sqlite_version)
d = tempfile.mkdtemp()
try:
    # 1. The proposed schema declares `payload JSON`. A declared type "JSON" gets NUMERIC affinity
    #    in SQLite, so a payload that looks like a number is stored as a number, not as the text
    #    that was hashed.
    db = sqlite3.connect(os.path.join(d, "a.db"))
    db.execute("CREATE TABLE event_ledger (event_id INTEGER PRIMARY KEY AUTOINCREMENT, payload JSON NOT NULL)")
    for text in ('{"a":1}', '123', '1.50', '1e2'):
        db.execute("INSERT INTO event_ledger (payload) VALUES (?)", (text,))
    for (stored, kind) in db.execute("SELECT payload, typeof(payload) FROM event_ledger"):
        print(f"1. inserted text -> stored {stored!r} as {kind}")
    try:
        db.execute("CREATE TABLE strict_ledger (payload TEXT NOT NULL CHECK (json_valid(payload))) STRICT")
        db.execute("INSERT INTO strict_ledger VALUES ('1.50')")
        print("1. STRICT TEXT column keeps:", db.execute("SELECT payload, typeof(payload) FROM strict_ledger").fetchone())
    except sqlite3.OperationalError as e:
        print("1. STRICT tables unavailable:", e)
    db.close()

    # 2. The proposed hash input is previous_hash + timestamp + event_type + canonical_json(payload),
    #    concatenated. Two different rows can produce the same input string.
    prev, ts = "0" * 64, "2026-09-30T00:00:00.000000Z"
    a = prev + ts + "A_B" + '{"x":1}'
    b = prev + ts + "A_" + 'B{"x":1}'
    print("2. two different (event_type, payload) pairs, same hash input:",
          a == b, hashlib.sha256(a.encode()).hexdigest()[:16], hashlib.sha256(b.encode()).hexdigest()[:16])

    # 3. A passive observer opening a live WAL database read-only, from a directory it cannot write.
    path = os.path.join(d, "wal", "ledger.db")
    os.makedirs(os.path.dirname(path))
    w = sqlite3.connect(path)
    w.execute("PRAGMA journal_mode=WAL")
    w.execute("CREATE TABLE t (v)")
    w.execute("INSERT INTO t VALUES (1)")
    w.commit()                                     # writer stays open: -wal and -shm exist
    print("3. files beside the database:", sorted(os.listdir(os.path.dirname(path))))
    os.chmod(os.path.dirname(path), stat.S_IRUSR | stat.S_IXUSR)
    try:
        r = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
        print("3. read-only reader, writer open, directory read-only:", r.execute("SELECT count(*) FROM t").fetchone())
        r.close()
    except sqlite3.Error as e:
        print("3. read-only reader failed:", type(e).__name__, e)
    w.close()                                      # the last connection may remove -wal and -shm
    print("3. after the writer closed:", sorted(os.listdir(os.path.dirname(path))))
    try:
        r = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
        print("3. read-only reader, writer closed:", r.execute("SELECT count(*) FROM t").fetchone())
        r.close()
    except sqlite3.Error as e:
        print("3. read-only reader, writer closed, failed:", type(e).__name__, e)
    os.chmod(os.path.dirname(path), stat.S_IRWXU)
finally:
    shutil.rmtree(d)
