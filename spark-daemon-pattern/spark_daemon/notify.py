"""systemd notification (sd_notify) over the standard library's socket module.

The runtime sends READY=1 only after startup recovery finished and DAEMON_START is committed,
and WATCHDOG=1 only from its main loop. There is no background thread: if an observation
hangs, the pings stop, and systemd restarts the service after WatchdogSec. A watchdog
restart then shows up in the ledger as a start after an unclean stop.
"""

import os
import socket


def notify(message: str, address: str | None = None) -> bool:
    addr = address if address is not None else os.environ.get("NOTIFY_SOCKET")
    if not addr:
        return False
    target = "\0" + addr[1:] if addr.startswith("@") else addr
    with socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM | socket.SOCK_CLOEXEC) as sock:
        try:
            sock.sendto(message.encode("ascii"), target)
        except OSError:
            return False
    return True
