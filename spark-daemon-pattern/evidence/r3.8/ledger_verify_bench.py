"""Start-up ledger verification cost (r3.8, universal-contract consideration U-6).

Builds a synthetic ledger of N records with the real LedgerWriter (fsync skipped, since only
the read path is timed), then times ledger.recover(): the same full-chain verification the
runtime performs before READY=1, under the unit's TimeoutStartSec = max(60, watchdog_seconds).

    python3 -B evidence/r3.8/ledger_verify_bench.py 20000 200000
"""

import os
import platform
import shutil
import sys
import tempfile
import time

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, ROOT)

from spark_daemon import ledger                   # noqa: E402
from spark_daemon.context import utc_now          # noqa: E402

PAYLOAD = {"created": 1, "deleted": 0, "moved": 2,
           "created_refs": ["refs/heads/feature/some-branch-name"], "created_refs_omitted": 0,
           "deleted_refs": [], "deleted_refs_omitted": 0,
           "moved_refs": ["refs/heads/main", "refs/tags/v1.2.3"], "moved_refs_omitted": 0,
           "moved_to": {"refs/heads/main": "a" * 40, "refs/tags/v1.2.3": "b" * 40}}


def measure(n: int) -> None:
    d = tempfile.mkdtemp(prefix="ledger-bench-")
    try:
        os.chmod(d, 0o700)
        real_fsync, ledger.os.fsync = ledger.os.fsync, (lambda fd: None)
        try:
            w = ledger.LedgerWriter(d, "bench-daemon", os.urandom(16).hex(), 32768, ledger.Tail(), utc_now)
            w.open()
            for _ in range(n):
                w.append("REFS_CHANGED", PAYLOAD)      # a git-watch-sized record (~700 bytes)
            w.close()
        finally:
            ledger.os.fsync = real_fsync
        size = os.path.getsize(os.path.join(d, ledger.LEDGER_NAME))
        t0 = time.perf_counter()
        scan, quarantine = ledger.recover(d, "bench-daemon", 32768)
        seconds = time.perf_counter() - t0
        assert scan.records == n and quarantine is None
        print(f"records={n} bytes={size} avg_record={size // n}B recover_s={seconds:.2f} "
              f"rate={n / seconds:,.0f} records/s")
    finally:
        shutil.rmtree(d)


if __name__ == "__main__":
    print(f"python={platform.python_version()} machine={platform.machine()} cpus={os.cpu_count()}")
    for arg in sys.argv[1:] or ["20000"]:
        measure(int(arg))
