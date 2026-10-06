"""How long start-up verification of a ledger takes as it grows (U-6, PD-67).

Builds ledgers of heartbeat-sized records with the skeleton's own writer (prepare + one
buffered write, not one fsync per record, to build them quickly), then times ledger.recover(),
which is what every start runs before DAEMON_START. Prints records, bytes and seconds."""
import os, sys, tempfile, time
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
from spark_daemon import ledger
from spark_daemon.runtime import _AcceptEvidence

PAYLOAD = {"mode": "observing", "last_accepted_utc": "2026-10-06T00:00:00.000000Z", "blind_ms": 1234,
           "boot_id": "0" * 8 + "-0000-0000-0000-" + "0" * 12, "boottime_ms": 123456789,
           "blind_since_boottime_ms": 123455555, "accepted_cycles": 15, "unsettled_cycles": 0,
           "failed_cycles": 0}
clock = lambda: "2026-10-06T00:00:00.000000Z"
for n in (10_000, 50_000, 200_000):
    d = tempfile.mkdtemp(prefix="ledger-growth-")
    w = ledger.LedgerWriter(d, "probe", "0" * 32, 8192, ledger.Tail(), clock)
    with open(os.path.join(d, ledger.LEDGER_NAME), "wb") as fh:
        for _ in range(n // 1000):
            batch = w.prepare([("DAEMON_HEARTBEAT", PAYLOAD)] * 1000)
            fh.write(b"".join(data + b"\n" for _, data, _ in batch))
            record, _, head = batch[-1]
            w.tail = ledger.Tail(record["seq"], head)
    size = os.path.getsize(os.path.join(d, ledger.LEDGER_NAME))
    ev = _AcceptEvidence()
    t = time.perf_counter()
    scan, _ = ledger.recover(d, "probe", 8192, on_record=ev.observe)
    print(f"{scan.records:>8} records  {size/1e6:7.1f} MB  recover {time.perf_counter()-t:6.2f} s")
