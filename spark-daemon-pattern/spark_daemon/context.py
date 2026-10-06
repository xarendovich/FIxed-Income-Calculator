"""The only I/O a daemon's code can perform: read-only helpers bound to its manifest.

There is no method that writes, deletes, sends or executes anything outside the manifest's
commands, so the rule "observation never authorizes action" (Observer v0.3 section 6.2)
holds by construction for code that follows the purity rules.
"""

import datetime
import os
import re
import stat as stat_mod
import time
from dataclasses import dataclass

from . import proc
from .guard import GuardError, count_violation


class Missing(Exception):
    """The path does not exist (a normal condition a daemon may handle)."""


class TooLarge(Exception):
    """The content exceeds the requested bound."""


class CycleBudgetExceeded(BaseException):
    """The cycle reached cycle_budget_seconds (r4.11, R-6). The cycle fails and counts toward
    the blind limit. A BaseException, so a daemon's `except Exception` cannot swallow it; the
    runtime also checks the elapsed time itself, so swallowing it does not save the cycle."""


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

    def __init__(self, policy, *, cycle_budget: float, tmp_dir: str):
        self._policy = policy
        self._budget = cycle_budget
        self._deadline = None
        self._tmp = tmp_dir

    def _begin_cycle(self, deadline: float) -> None:
        """The runtime's: one monotonic deadline per cycle, set at cycle entry (R-6)."""
        self._deadline = deadline

    def _end_cycle(self) -> None:
        self._deadline = None

    def _remaining(self) -> float:
        """The time a blocking call may take: whatever is left of the cycle's budget."""
        if self._deadline is None:
            return float(self._budget)
        left = self._deadline - time.monotonic()
        if left <= 0:
            raise CycleBudgetExceeded("the cycle budget is spent")
        return left

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

    def run(self, argv, max_bytes: int = 65536) -> proc.RunResult:
        """Run one of the manifest's commands (argv list, never a shell). Git is refused
        here: it must go through ctx.git(), which enforces the read-only subcommand and
        option rules."""
        argv = _str_list(argv, "argv")
        if argv and argv[0] == "git":
            count_violation("run-git-outside-ctx-git")
            raise NotAllowed("use ctx.git() for Git")
        try:
            return proc.run(argv, executables=self._policy.commands, timeout=self._remaining(),
                            max_bytes=max_bytes)
        except proc.CommandNotAllowed as e:
            count_violation("command-not-allowed")
            raise NotAllowed(str(e)) from None

    def git(self, repo: str, args, max_bytes: int = 65536, index_copy: bool = False) -> proc.RunResult:
        """Run a read-only Git subcommand (proc.GIT_SUBCOMMANDS) in a declared repository."""
        full = self._resolve(repo)
        args = _str_list(args, "args")
        try:
            return proc.git(full, args, executables=self._policy.commands, timeout=self._remaining(),
                            max_bytes=max_bytes, tmp_dir=self._tmp, index_copy=bool(index_copy))
        except proc.CommandNotAllowed as e:
            count_violation("git-not-allowed")
            raise NotAllowed(str(e)) from None

    GIT_DIFF_FLAGS = proc.GIT_DIFF_FLAGS

    @staticmethod
    def now_utc() -> str:
        """Wall-clock time, informational only; never use it to order events."""
        return utc_now()
