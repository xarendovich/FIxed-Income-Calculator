"""Path policy, output-directory checks and the in-process audit hook.

Three layers keep a daemon inside its declared bounds; each catches what the one before
it cannot:

1. purity.py, a static check of the daemon's code before it is imported;
2. the audit hook here, which sees every file open, process spawn and socket call made in
   the daemon's process and blocks anything outside the manifest (it stops mistakes and
   leaves evidence, but it is not a sandbox: code that uses ctypes could bypass it, which
   is why ctypes is blocked too);
3. the systemd sandbox written by unitgen.py, which is the actual security boundary.
"""

import json
import os
import socket
import stat
import sys

from . import KNOWN_OUTPUT_ENTRIES, QUARANTINE_DIR, TMP_DIR, TMP_PREFIX
from .pathpolicy import PathPolicy


class GuardError(Exception):
    """The output directory is unsafe, or a path is outside the manifest's bounds."""


class PolicyViolation(PermissionError):
    """Raised by the audit hook (enforce mode) when the process steps outside the manifest."""


# Counts every violation, including ones the daemon's own code may have caught and ignored.
# The runtime checks it after each cycle and fails closed if it moved.
VIOLATIONS = {"count": 0, "last": None}

_WRITE_FLAGS = os.O_WRONLY | os.O_RDWR | os.O_CREAT | os.O_TRUNC | os.O_APPEND
_BLOCKED_EVENTS = frozenset({
    "os.system", "os.exec", "os.posix_spawn", "os.spawn", "os.fork", "os.forkpty",
    "pty.spawn", "os.startfile",
    "ctypes.dlopen", "ctypes.dlsym", "ctypes.cdata", "ctypes.call_function",
    "ctypes.addressof", "ctypes.string_at", "ctypes.wstring_at",
    "socket.getaddrinfo", "socket.gethostbyname", "socket.gethostbyname_ex",
    "socket.gethostbyaddr", "urllib.Request", "http.client.connect", "ftplib.connect",
    "smtplib.connect", "poplib.connect", "imaplib.open", "nntplib.connect", "webbrowser.open",
    "os.symlink", "os.link", "sys.addaudithook",
})
_PATH_MUTATIONS = frozenset({
    "os.remove", "os.rmdir", "os.mkdir", "os.chmod", "os.chown", "os.truncate", "os.utime",
    "shutil.rmtree", "os.chflags", "os.lchflags", "os.lchown", "os.lchmod",
})


class Policy:
    """What one daemon may read and write (pathpolicy.PathPolicy, the one canonical path
    policy, contract 5), plus where it may send notifications.

    Immutable once built: the audit hook and ctx both consult it, so a daemon that reached
    it (purity.py forbids private attributes, which is the only route) still could not widen
    its own bounds. The hook additionally captures its own copies at install time."""

    __slots__ = ("paths", "notify_target")

    def __init__(self, manifest, notify_socket=None, output_dir=None, home=None):
        # output_dir: the harness's recorded override for a manifest whose output_dir is
        # absolute (contract 5); validated by the caller against the manifest's placement rules.
        # home: what "~" expands to when not this process's home() (the battery, for its
        # workspace, without changing its own environment).
        object.__setattr__(self, "paths", PathPolicy.of(manifest, home=home, output_dir=output_dir))
        object.__setattr__(self, "notify_target", _notify_target(
            notify_socket if notify_socket is not None else os.environ.get("NOTIFY_SOCKET")))

    def __setattr__(self, name, value):
        raise AttributeError("Policy is immutable")

    def __delattr__(self, name):
        raise AttributeError("Policy is immutable")

    output_dir = property(lambda self: self.paths.output_dir)
    reads = property(lambda self: self.paths.reads)
    deny = property(lambda self: self.paths.deny)

    def denied(self, full: str) -> bool:
        return self.paths.denied(full)

    def readable(self, path: str, follow: bool = True) -> str:
        """Resolve a path the daemon asked to read, or raise GuardError. follow=False resolves
        only the parent directory and keeps the last component as named, for lstat()-style
        calls: a symlink inside a declared read is then judged by where it is, not by where
        it points, so a link planted in a watched folder cannot trip fail-closed (78)."""
        name = os.path.basename(path)
        if not follow and name not in ("", ".", "..", "~"):
            full = os.path.join(self.paths.resolve(os.path.dirname(path) or "."), name)
        else:
            full = self.paths.resolve(path)
        if self.paths.denied(full):
            _count("read-denied-path")
            raise GuardError("path is denied")
        if not self.paths.may_read(full):
            _count("read-outside-manifest")
            raise GuardError("path is outside the manifest's reads")
        return full

    def writable(self, full: str) -> bool:
        return self.paths.may_write(full)


def _notify_target(address):
    if not address:
        return None
    return "\0" + address[1:] if address.startswith("@") else address


def _count(kind):
    VIOLATIONS["count"] += 1
    VIOLATIONS["last"] = kind


def prepare_output_dir(path: str) -> None:
    """Create the output directory (0700) or refuse an unsafe one. Never chmods silently.

    Creates and checks only; it never deletes anything, because it runs before the
    single-instance lock is taken. A rejected duplicate launch must leave the running
    instance's artifacts untouched (WBS 3.1 §4.2; r3.2: the cleanup below used to run here
    and deleted the running instance's in-flight tmp/ files)."""
    os.makedirs(path, mode=0o700, exist_ok=True)
    _check_dir(path, "output_dir")
    for sub in (QUARANTINE_DIR, TMP_DIR):
        full = os.path.join(path, sub)
        if not os.path.lexists(full):
            os.mkdir(full, 0o700)
        _check_dir(full, sub)


def _check_dir(path, label):
    st = os.lstat(path)
    if stat.S_ISLNK(st.st_mode):
        raise GuardError(f"{label} is a symlink")
    if not stat.S_ISDIR(st.st_mode):
        raise GuardError(f"{label} is not a directory")
    if st.st_uid != os.getuid():
        raise GuardError(f"{label} is not owned by the running user")
    if st.st_mode & 0o077:
        raise GuardError(f"{label} has mode {stat.S_IMODE(st.st_mode):o}; it must be 700")


def remove_stray_temp_files(path: str) -> None:
    """Remove temp files a crashed run left behind: digest temps in the output directory and
    everything in tmp/. Removed, never read. Only ever called while holding the lock."""
    for entry in os.listdir(path):
        if entry.startswith(TMP_PREFIX):
            full = os.path.join(path, entry)
            if os.path.isfile(full) and not os.path.islink(full):
                os.remove(full)
    tmp = os.path.join(path, TMP_DIR)
    for entry in os.listdir(tmp):
        full = os.path.join(tmp, entry)
        if os.path.isfile(full) and not os.path.islink(full):
            os.remove(full)


def inventory(path: str) -> list:
    """Names in the output directory that the skeleton did not create (reported, never touched)."""
    return sorted(e for e in os.listdir(path)
                  if e not in KNOWN_OUTPUT_ENTRIES and not e.startswith(TMP_PREFIX))


def install_audit_hook(policy: Policy, mode: str = "enforce") -> None:
    """Install the process-wide audit hook. It cannot be removed once installed."""
    if mode not in ("enforce", "record"):
        raise ValueError("mode must be 'enforce' or 'record'")
    notify = policy.notify_target
    paths = policy.paths            # frozen; the hook keeps its own reference
    denied, writable = paths.denied, paths.may_write

    def violation(kind, detail=""):
        _count(kind)
        if mode == "record":
            line = json.dumps({"spark_daemon_audit": kind, "detail": str(detail)[:200]})
            os.write(2, (line + "\n").encode("utf-8", "replace"))
            return
        raise PolicyViolation(f"spark-daemon policy blocked: {kind}")

    def as_path(value):
        if isinstance(value, bytes):
            value = os.fsdecode(value)
        if not isinstance(value, str):
            return None
        return os.path.normpath(os.path.abspath(value))

    def hook(event, args):
        if event == "open":
            path, mode_arg, flags = args
            full = as_path(path)
            if full is None:
                return
            writing = bool((flags or 0) & _WRITE_FLAGS) or bool(
                mode_arg and any(ch in str(mode_arg) for ch in "wax+"))
            if denied(full):
                violation("open-denied-path", full)
            elif writing and not writable(full):
                violation("write-outside-output-dir", full)
        elif event in ("os.listdir", "os.scandir"):
            full = as_path(args[0] if args else ".")
            if full is not None and denied(full):
                violation("list-denied-path", full)
        elif event == "subprocess.Popen":
            # v5 (R-2): a daemon runs no program, so every spawn is a violation.
            executable, argv = args[0], args[1]
            violation("spawn", as_path(executable if executable is not None else (argv[0] if argv else None)))
        elif event in _BLOCKED_EVENTS:
            violation(event)
        elif event == "socket.__new__":
            family = args[1]
            if family != socket.AF_UNIX:
                violation("socket-family", family)
        elif event in ("socket.connect", "socket.bind", "socket.sendto", "socket.sendmsg"):
            address = args[1] if len(args) > 1 else None
            if isinstance(address, bytes):
                address = address.decode("latin-1")
            if not (event in ("socket.sendto", "socket.sendmsg") and notify and address == notify):
                violation(event, address)
        elif event in _PATH_MUTATIONS:
            target = args[0] if args else None
            if isinstance(target, int):               # fd-based: the fd was already checked at open
                return
            full = as_path(target)
            if full is None or not writable(full):
                violation(f"{event}-outside-output-dir", full)
        elif event in ("os.rename", "shutil.move", "shutil.copyfile", "shutil.copytree"):
            dst = as_path(args[1]) if len(args) > 1 else None
            src = as_path(args[0]) if args else None
            if dst is None or not writable(dst):
                violation(f"{event}-outside-output-dir", dst)
            elif event in ("os.rename", "shutil.move") and (src is None or not writable(src)):
                violation(f"{event}-source-outside-output-dir", src)

    sys.addaudithook(hook)
