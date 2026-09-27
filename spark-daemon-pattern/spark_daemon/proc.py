"""Hardened, bounded subprocess execution for allowlisted commands only.

- argv lists only, never a shell; the executable must be one the manifest names, resolved
  to an absolute path under /usr/bin or /bin at startup;
- a scrubbed environment: fixed PATH and locale, no pager, no prompts, and Git's system and
  global configuration switched off, so settings such as color.ui=always or diff.external
  cannot leak into evidence (script-board finding F5);
- stdout read in chunks up to a byte limit, then the process group is killed; stderr is
  discarded so raw, untrusted error text never reaches the ledger;
- a hard timeout that kills the whole process group.
"""

import os
import selectors
import shutil
import subprocess
import time
from dataclasses import dataclass

SAFE_ENV = {
    "PATH": "/usr/bin:/bin",
    "LC_ALL": "C",
    "LANG": "C",
    "HOME": "/nonexistent",
    "GIT_OPTIONAL_LOCKS": "0",
    "GIT_TERMINAL_PROMPT": "0",
    "GIT_CONFIG_NOSYSTEM": "1",
    "GIT_CONFIG_GLOBAL": "/dev/null",
    "GIT_PAGER": "cat",
    "PAGER": "cat",
}

# Every Git call: no pager, no colour, no fsmonitor hook, raw paths (v0.3 section 3.8).
GIT_BASE = ("--no-pager", "-c", "color.ui=never", "-c", "core.fsmonitor=false",
            "-c", "core.quotepath=off", "-c", "core.pager=cat")
# Diff-producing Git calls additionally (v0.3 section 3.8; script-board F3).
GIT_DIFF_FLAGS = ("--no-ext-diff", "--no-textconv", "--no-renames", "--no-color")


class CommandNotAllowed(Exception):
    pass


@dataclass(frozen=True)
class RunResult:
    returncode: int | None
    stdout: str
    truncated: bool
    timed_out: bool


def run(argv, *, executables: dict, timeout: float, max_bytes: int, cwd=None, extra_env=None) -> RunResult:
    if not argv or argv[0] not in executables:
        raise CommandNotAllowed("command is not in the manifest's commands")
    env = dict(SAFE_ENV)
    if extra_env:
        env.update(extra_env)
    exe = executables[argv[0]]
    proc = subprocess.Popen([exe, *argv[1:]], executable=exe, stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, cwd=cwd, env=env,
                            start_new_session=True, close_fds=True)
    chunks, size, truncated, timed_out = [], 0, False, False
    deadline = time.monotonic() + timeout
    sel = selectors.DefaultSelector()
    sel.register(proc.stdout, selectors.EVENT_READ)
    try:
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                timed_out = True
                break
            if not sel.select(timeout=min(remaining, 0.5)):
                continue
            data = os.read(proc.stdout.fileno(), 65536)
            if not data:
                break
            if size + len(data) > max_bytes:
                chunks.append(data[:max_bytes - size])
                size = max_bytes
                truncated = True
                break
            chunks.append(data)
            size += len(data)
    finally:
        sel.close()
        if timed_out or truncated:
            _kill_group(proc)
        proc.stdout.close()
        try:
            proc.wait(timeout=max(0.1, deadline - time.monotonic()))
        except subprocess.TimeoutExpired:
            _kill_group(proc)
            proc.wait()
            timed_out = True
    text = b"".join(chunks).decode("utf-8", "replace")
    return RunResult(None if (timed_out or truncated) else proc.returncode, text, truncated, timed_out)


def _kill_group(proc):
    try:
        os.killpg(proc.pid, 9)
    except ProcessLookupError:
        pass


def git(repo, args, *, executables, timeout, max_bytes, tmp_dir=None, index_copy=False) -> RunResult:
    """Run a read-only Git command. index_copy=True points Git at a private copy of the index,
    because porcelain `git diff` and `git status` rewrite .git/index even with optional locks
    off (script-board finding F4)."""
    argv = ["git", "-C", repo, *GIT_BASE, *args]
    if not index_copy:
        return run(argv, executables=executables, timeout=timeout, max_bytes=max_bytes)
    if tmp_dir is None:
        raise ValueError("index_copy needs tmp_dir")
    where = run(["git", "-C", repo, *GIT_BASE, "rev-parse", "--path-format=absolute", "--git-path", "index"],
                executables=executables, timeout=timeout, max_bytes=4096)
    index_path = where.stdout.strip()
    if where.returncode != 0 or not index_path:
        return run(argv, executables=executables, timeout=timeout, max_bytes=max_bytes)
    copy_path = os.path.join(tmp_dir, f"index-copy-{os.getpid()}-{time.monotonic_ns()}")
    try:
        if os.path.exists(index_path):
            shutil.copyfile(index_path, copy_path)
        return run(argv, executables=executables, timeout=timeout, max_bytes=max_bytes,
                   extra_env={"GIT_INDEX_FILE": copy_path})
    finally:
        if os.path.exists(copy_path):
            os.remove(copy_path)
