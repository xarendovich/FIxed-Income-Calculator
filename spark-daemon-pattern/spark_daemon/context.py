"""The only I/O a daemon's code can perform: read-only helpers bound to its manifest.

There is no method that writes, deletes, sends or executes anything outside the manifest's
commands, so the rule "observation never authorizes action" (Observer v0.3 section 6.2)
holds by construction for code that follows the purity rules.
"""

import datetime
import os
import stat as stat_mod
from dataclasses import dataclass

from . import proc
from .guard import GuardError


class Missing(Exception):
    """The path does not exist (a normal condition a daemon may handle)."""


class TooLarge(Exception):
    """The content exceeds the requested bound."""


class NotAllowed(Exception):
    """The request is outside the manifest (counted as a policy violation)."""


@dataclass(frozen=True)
class StatInfo:
    kind: str          # "file", "dir", "link" or "other"
    size: int
    mtime_ns: int


@dataclass(frozen=True)
class DiskUsage:
    total_bytes: int
    free_bytes: int
    available_bytes: int


@dataclass(frozen=True)
class Listing:
    names: tuple
    truncated: bool


def utc_now() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")


class Context:
    Missing = Missing
    TooLarge = TooLarge
    NotAllowed = NotAllowed

    def __init__(self, policy, *, step_timeout: int, tmp_dir: str):
        self._policy = policy
        self._timeout = step_timeout
        self._tmp = tmp_dir

    def _resolve(self, path):
        if not isinstance(path, str):
            raise NotAllowed("paths must be strings")
        try:
            return self._policy.readable(path)
        except GuardError as e:
            raise NotAllowed(str(e)) from None

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
        full = self._resolve(path)
        try:
            st = os.lstat(full)
        except FileNotFoundError:
            raise Missing(path) from None
        mode = st.st_mode
        kind = ("file" if stat_mod.S_ISREG(mode) else "dir" if stat_mod.S_ISDIR(mode)
                else "link" if stat_mod.S_ISLNK(mode) else "other")
        return StatInfo(kind, st.st_size, st.st_mtime_ns)

    def disk_usage(self, path: str) -> DiskUsage:
        full = self._resolve(path)
        try:
            v = os.statvfs(full)
        except FileNotFoundError:
            raise Missing(path) from None
        return DiskUsage(v.f_blocks * v.f_frsize, v.f_bfree * v.f_frsize, v.f_bavail * v.f_frsize)

    def run(self, argv, max_bytes: int = 65536) -> proc.RunResult:
        try:
            return proc.run(list(argv), executables=self._policy.commands, timeout=self._timeout,
                            max_bytes=max_bytes)
        except proc.CommandNotAllowed as e:
            raise NotAllowed(str(e)) from None

    def git(self, repo: str, args, max_bytes: int = 65536, index_copy: bool = False) -> proc.RunResult:
        full = self._resolve(repo)
        try:
            return proc.git(full, list(args), executables=self._policy.commands, timeout=self._timeout,
                            max_bytes=max_bytes, tmp_dir=self._tmp, index_copy=index_copy)
        except proc.CommandNotAllowed as e:
            raise NotAllowed(str(e)) from None

    GIT_DIFF_FLAGS = proc.GIT_DIFF_FLAGS

    @staticmethod
    def now_utc() -> str:
        """Wall-clock time, informational only; never use it to order events."""
        return utc_now()
