"""The only I/O a daemon's code can perform: read-only helpers bound to its manifest.

There is no method that writes, deletes, sends or executes anything, so the rule "observation
never authorizes action" (Observer v0.3 section 6.2) holds by construction for code that follows
the purity rules. Since v5 (R-2) ctx runs no program: ctx.run and ctx.git are gone with the
manifest's commands, and the cycle's alarm is the only deadline (E-2).
"""

import datetime
import os
import re
import stat as stat_mod
from dataclasses import dataclass

from .guard import GuardError


class Missing(Exception):
    """The path does not exist (a normal condition a daemon may handle)."""


class TooLarge(Exception):
    """The content exceeds the requested bound."""


class CycleBudgetExceeded(BaseException):
    """The cycle reached cycle_budget_seconds (r4.11, R-6). The cycle fails and counts toward
    the blind limit. Raised by the cycle's alarm, the only deadline (v5, E-2). A BaseException, so
    a daemon's `except Exception` cannot swallow it; the runtime also checks the elapsed time
    itself, so swallowing it does not save the cycle."""


class NotAllowed(Exception):
    """The request is outside the manifest. Counted as a policy violation: the daemon stops
    with exit 78 after the cycle even if its own code catches this."""


@dataclass(frozen=True)
class StatInfo:
    kind: str          # "file", "dir", "link" or "other"
    size: int
    mtime_us: int      # microseconds since the epoch. r2 returned mtime_ns (about 1.8e18), which is
                       # outside the canonical integer range, so any snapshot holding it was refused.


@dataclass(frozen=True)
class DiskUsage:
    total_bytes: int
    free_bytes: int
    available_bytes: int


@dataclass(frozen=True)
class Listing:
    names: tuple
    truncated: bool


def _str_list(value, label) -> list:
    if isinstance(value, (str, bytes)) or not isinstance(value, (list, tuple)):
        raise NotAllowed(f"{label} must be a list of strings")
    out = list(value)
    if not all(isinstance(v, str) and "\0" not in v for v in out):
        raise NotAllowed(f"{label} must be a list of strings")
    return out


def utc_now() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")


UNSETTLED_REASON_RE = re.compile(r"[A-Z][A-Z0-9_]{2,39}")


class Unsettled:
    """What sense() returns instead of a snapshot when what it read is not stable yet (for
    example, a worktree changing between two reads during a build). The cycle is abandoned
    before decide(): no event, no error, no new digest. It is not an accepted cycle either, so
    a run of them counts towards blind_limit_seconds (r3.5)."""

    __slots__ = ("reason",)

    def __init__(self, reason: str):
        self.reason = reason


class Context:
    Missing = Missing
    TooLarge = TooLarge
    NotAllowed = NotAllowed

    def __init__(self, policy):
        self._policy = policy

    def _resolve(self, path, follow=True):
        if not isinstance(path, str) or "\0" in path:
            raise NotAllowed("paths must be strings")
        try:
            return self._policy.readable(path, follow=follow)
        except GuardError as e:
            raise NotAllowed(str(e)) from None

    def unsettled(self, reason: str) -> Unsettled:
        """Return this from sense() to abandon the cycle: nothing read was stable. The reason is
        an upper-case category (A-Z, 0-9, _; 3-40 characters), never text from what was read."""
        if not (isinstance(reason, str) and UNSETTLED_REASON_RE.fullmatch(reason)):
            raise ValueError("unsettled reason must be 3-40 characters of A-Z, 0-9 and _, starting with a letter")
        return Unsettled(reason)

    def read_text(self, path: str, max_bytes: int = 65536) -> str:
        """Read a text file (UTF-8, invalid bytes replaced). Raises TooLarge past max_bytes."""
        full = self._resolve(path)
        try:
            with open(full, "rb") as fh:
                data = fh.read(max_bytes + 1)
        except FileNotFoundError:
            raise Missing(path) from None
        if len(data) > max_bytes:
            raise TooLarge(path)
        return data.decode("utf-8", "replace")

    def list_dir(self, path: str, max_entries: int = 1024) -> Listing:
        """Sorted entry names of a directory; truncated=True past max_entries."""
        full = self._resolve(path)
        try:
            with os.scandir(full) as it:
                names = []
                for entry in it:
                    if len(names) >= max_entries:
                        return Listing(tuple(sorted(names)), True)
                    names.append(entry.name)
        except FileNotFoundError:
            raise Missing(path) from None
        return Listing(tuple(sorted(names)), False)

    def stat(self, path: str) -> StatInfo:
        """Kind ("file", "dir", "link" or "other"), size and mtime_us; symlinks are not followed."""
        full = self._resolve(path, follow=False)
        try:
            st = os.lstat(full)
        except FileNotFoundError:
            raise Missing(path) from None
        mode = st.st_mode
        kind = ("file" if stat_mod.S_ISREG(mode) else "dir" if stat_mod.S_ISDIR(mode)
                else "link" if stat_mod.S_ISLNK(mode) else "other")
        return StatInfo(kind, st.st_size, st.st_mtime_ns // 1000)

    def disk_usage(self, path: str) -> DiskUsage:
        """Total, free and available bytes of the filesystem holding a declared path."""
        full = self._resolve(path)
        try:
            v = os.statvfs(full)
        except FileNotFoundError:
            raise Missing(path) from None
        return DiskUsage(v.f_blocks * v.f_frsize, v.f_bfree * v.f_frsize, v.f_bavail * v.f_frsize)

    @staticmethod
    def now_utc() -> str:
        """Wall-clock time, informational only; never use it to order events."""
        return utc_now()
