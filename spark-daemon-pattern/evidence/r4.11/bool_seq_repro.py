"""HF-38: a ledger record whose seq is the JSON boolean true verified as seq 1, because in
Python True == 1, and canonical re-encoding keeps `true` as `true`. Prints what the skeleton's
verifier says about such a one-record ledger."""
import os
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
from spark_daemon import ledger  # noqa: E402
from spark_daemon.canonical import canonical_bytes  # noqa: E402

rec = {"schema": "spark-daemon-ledger/1", "seq": True, "prev_sha256": "0" * 64, "daemon": "probe",
       "run_id": "0" * 32, "event_id": "0" * 32, "timestamp_utc": "2026-10-06T00:00:00.000000Z",
       "event_type": "DAEMON_START", "payload": {}}
d = tempfile.mkdtemp()
path = os.path.join(d, "ledger.jsonl")
with open(path, "wb") as fh:
    fh.write(canonical_bytes(rec) + b"\n")
try:
    print("ACCEPTED:", ledger.verify_file(path, "probe", 8192).tail)
except ledger.LedgerCorrupt as e:
    print("REFUSED:", e)
