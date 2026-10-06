"""Hardened, bounded subprocess execution for allowlisted commands only.

- argv lists only, never a shell; the executable must be one the manifest names, resolved
  to an absolute path in SYSTEM_PATH at startup;
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

# The only directories an allowlisted command is resolved in, and the PATH every child gets.
SYSTEM_PATH = "/usr/bin:/bin"

SAFE_ENV = {
    "PATH": SYSTEM_PATH,
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

# Every Git call: no pager, no colour, no fsmonitor hook, raw paths (v0.3 section 3.8), and
# the Observer's WBS 2.5 hardened-profile additions (A1, evidence E1): a repository's own
# log.showSignature + gpg.program otherwise runs a program during `git log` / `git show`
# (reproduced against r3's ctx.git), and log.mailmap rewrites identities. Command-line -c
# outranks repository config.
GIT_BASE = ("--no-pager", "-c", "color.ui=never", "-c", "core.fsmonitor=false",
            "-c", "core.quotepath=off", "-c", "core.pager=cat",
            "-c", "log.showSignature=false", "-c", "log.mailmap=false")
# Environment for every Git call (WBS 2.5 A1, E2/E3): replace refs show forged metadata and
# ancestry under genuine SHAs, grafts rewrite ancestry and survive --no-replace-objects, and a
# partial clone would fetch missing objects over the network during a read.
GIT_ENV = {"GIT_NO_REPLACE_OBJECTS": "1", "GIT_GRAFT_FILE": "/dev/null", "GIT_NO_LAZY_FETCH": "1"}
# Diff-producing Git calls additionally (v0.3 section 3.8; script-board F3).
GIT_DIFF_FLAGS = ("--no-ext-diff", "--no-textconv", "--no-renames", "--no-color")

# Read-only subcommands ctx.git() accepts as its first argument. Anything that can write a
# ref, the index or the worktree, fetch, or run hooks is absent. The daemon's arguments can
# therefore never be Git *global* options (-c, --git-dir, --exec-path, ...): the subcommand
# always comes first, so `-c alias.x=!cmd` style injection is impossible.
GIT_SUBCOMMANDS = frozenset({
    "log", "show", "diff", "status", "rev-parse", "rev-list", "ls-files", "ls-tree",
    "cat-file", "for-each-ref", "show-ref", "describe", "merge-base", "name-rev", "diff-tree",
    "diff-index", "diff-files", "count-objects", "shortlog", "blame",
})
# Subcommands that produce diffs get "--no-ext-diff --no-textconv" inserted by the skeleton,
# so a repository's own config (diff.external, textconv drivers) never runs a program.
GIT_DIFF_SUBCOMMANDS = frozenset({"log", "show", "diff", "diff-tree", "diff-index", "diff-files"})
# Long options refused anywhere before a "--" separator: they write files, read files
# outside the repository, run programs (external diff, textconv, gpg for signatures) or
# fetch from elsewhere. (Global options such as -c, --git-dir and --exec-path cannot appear:
# the subcommand is always first, and after it they are either query flags, as in
# `rev-parse --git-dir`, or unknown options.) Git accepts any unambiguous abbreviation of a long
# option (--outp=x means --output=x), so an argument is refused when it is a prefix of one
# of these, as well as when it starts with one.
GIT_REFUSED_OPTIONS = (
    "--output", "--no-index", "--ext-diff", "--textconv", "--exec", "--upload-pack",
    "--receive-pack",
    "--contents", "--ignore-revs-file", "--exclude-from", "--exclude-per-directory",
    "--pathspec-from-file", "--open-files-in-pager", "--orderfile", "--mailmap-file",
    "--show-signature", "--verify-signatures", "--stdin", "--batch", "--filters",
)
# Short options refused per subcommand (they read a file named by the daemon).
GIT_REFUSED_SHORT = {"diff": ("-O",), "log": ("-O",), "show": ("-O",), "diff-tree": ("-O",),
                     "diff-index": ("-O",), "diff-files": ("-O",), "blame": ("-S",),
                     "ls-files": ("-X",)}


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


def git_base(repo: str) -> tuple:
    """GIT_BASE plus safe.directory for exactly this repository (r3, PD-25). A daemon runs as
    its own system user (PD-09), so the repositories it observes belong to someone else, and
    Git (2.35.2+) refuses them as "dubious ownership" - every ctx.git() call would fail with
    exit 128. Only the declared, already-resolved path is trusted, never "*"."""
    return (*GIT_BASE, "-c", f"safe.directory={repo}")


def git_refusal(args):
    """Why ctx.git() refuses this argument list, or None if it is allowed.

    Arguments are also refused when they name a path outside the repository (absolute, or
    with a ".." segment): outside a repository `git diff a b` silently becomes --no-index
    and would read arbitrary files."""
    if not args:
        return "a Git subcommand is required"
    sub = args[0]
    if sub not in GIT_SUBCOMMANDS:
        return f"Git subcommand {sub!r} is not in the read-only allowlist"
    options_done = False
    for arg in args[1:]:
        if arg.startswith("/") or ".." in arg.split("/") or arg.startswith("~"):
            return "Git arguments must not name paths outside the repository"
        if options_done:
            continue
        if arg == "--":
            options_done = True
            continue
        if arg.startswith("--"):
            name = arg.split("=", 1)[0]
            for refused in GIT_REFUSED_OPTIONS:
                if name.startswith(refused) or (len(name) > 3 and refused.startswith(name)):
                    return f"Git option {refused!r} is not allowed"
            if name in ("--format", "--pretty") and "%G" in arg:
                return "signature placeholders (%G...) run gpg and are not allowed"
        elif arg.startswith("-"):
            for refused in GIT_REFUSED_SHORT.get(sub, ()):
                if arg.startswith(refused):
                    return f"Git option {refused!r} is not allowed for {sub}"
    return None


def git(repo, args, *, executables, timeout, max_bytes, tmp_dir=None, index_copy=False) -> RunResult:
    """Run a read-only Git command. index_copy=True points Git at a private copy of the index,
    because porcelain `git diff` and `git status` rewrite .git/index even with optional locks
    off (script-board finding F4)."""
    args = list(args)
    problem = git_refusal(args)
    if problem:
        raise CommandNotAllowed(problem)
    if args[0] in GIT_DIFF_SUBCOMMANDS:
        args = [args[0], "--no-ext-diff", "--no-textconv", *args[1:]]
    base = git_base(repo)
    argv = ["git", "-C", repo, *base, *args]
    if not index_copy:
        return run(argv, executables=executables, timeout=timeout, max_bytes=max_bytes, extra_env=GIT_ENV)
    if tmp_dir is None:
        raise ValueError("index_copy needs tmp_dir")
    where = run(["git", "-C", repo, *base, "rev-parse", "--path-format=absolute", "--git-path", "index"],
                executables=executables, timeout=timeout, max_bytes=4096, extra_env=GIT_ENV)
    index_path = where.stdout.strip()
    if where.returncode != 0 or not index_path:
        return run(argv, executables=executables, timeout=timeout, max_bytes=max_bytes, extra_env=GIT_ENV)
    copy_path = os.path.join(tmp_dir, f"index-copy-{os.getpid()}-{time.monotonic_ns()}")
    try:
        if os.path.exists(index_path):
            shutil.copyfile(index_path, copy_path)
        return run(argv, executables=executables, timeout=timeout, max_bytes=max_bytes,
                   extra_env={**GIT_ENV, "GIT_INDEX_FILE": copy_path})
    finally:
        if os.path.exists(copy_path):
            os.remove(copy_path)
