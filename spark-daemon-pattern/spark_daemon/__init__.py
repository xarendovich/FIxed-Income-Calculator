"""Spark daemon pattern: the observe-only resident-daemon skeleton behind contract 5.x.

A daemon is declared by a manifest (manifest.py), runs inside a fixed skeleton (runtime.py
and the modules it uses), and must pass the conformance battery (battery.py) before it can
produce qualification evidence. Whoever writes a daemon builds against the published
contract (contract.py) and hands back a candidate envelope (handoff.py), which carries no
authority. Installation or activation remains a separate owner decision. Nothing in this
package installs, enables or starts a service.
"""

# The skeleton (this implementation), not the contract: 0.3.0 from r3 to r4.11, 0.5.0 with contract 5.
VERSION = "0.5.0"

MANIFEST_SCHEMA = "spark-daemon-manifest/4"
# The PATH given to the tools the battery runs (strace, systemd-analyze). Since v5 (R-2) the
# daemon itself runs no program at all.
SYSTEM_PATH = "/usr/bin:/bin"
LEDGER_SCHEMA = "spark-daemon-ledger/1"

# Event types the skeleton itself writes. A manifest may not declare them.
RESERVED_EVENT_TYPES = (
    "DAEMON_START",
    "DAEMON_STOP",
    "DAEMON_ERROR",
    "DAEMON_ERROR_CLEARED",
    "LEDGER_TAIL_QUARANTINED",
    "DAEMON_HEARTBEAT",          # r4.0 (PD-70): observation heartbeat, every blind_limit_seconds / 2
)

# Fixed file names inside a daemon's output directory.
LEDGER_NAME = "ledger.jsonl"
DIGEST_NAME = "digest.md"
LOCK_NAME = "daemon.lock"
QUARANTINE_DIR = "quarantine"
TMP_DIR = "tmp"
KNOWN_OUTPUT_ENTRIES = frozenset({LEDGER_NAME, DIGEST_NAME, LOCK_NAME, QUARANTINE_DIR, TMP_DIR})
TMP_PREFIX = ".tmp-"

# Exit codes. 0-3 follow the script contract (SC2); the rest follow sysexits.h.
EXIT_OK = 0
EXIT_FAILED = 1
EXIT_USAGE = 2
EXIT_FLAGGED = 3
EXIT_LEDGER_CORRUPT = 65      # EX_DATAERR: refuse to start, change nothing
EXIT_UNCERTAIN_COMMIT = 70    # EX_SOFTWARE: a ledger write or fsync failed; recovery decides on restart
EXIT_ALREADY_RUNNING = 73     # EX_CANTCREAT: another instance holds the lock
EXIT_POLICY = 78              # EX_CONFIG: output directory unsafe, or a policy violation (fail closed)
EXIT_SENSE_BLIND = 78         # EX_CONFIG: no accepted cycle within blind_limit_seconds (SENSE_BLIND);
                              # like every 78, it needs a human and the unit never restarts it
