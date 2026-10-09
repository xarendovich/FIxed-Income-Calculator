"""Kernel-enforced confinement via the Landlock LSM (AP-01, r2, standard library only).

Applied once, early in start-up (before the audit hook and before daemon code is imported:
see runtime.py's start-up order). It is irreversible for the life of the process, which is
what makes it different from the audit hook: once restricted, the kernel denies whatever
was not explicitly granted, even if the interpreter is later fully compromised, including by
code that reaches for ctypes (which is why ctypes is one of the audit hook's own blocked
events - Landlock is what actually backs that up). It sits alongside two other layers, each
catching what the others cannot; guard.py's module docstring lays out the full division.

Coverage and gaps, measured on real kernels with evidence/ap/landlock_probe.py (see
ADJUDICATION-AP.md, AP-01, for the full table across ABI 1/3/4/7):

- Blocks: reading or listing anything not explicitly granted (~/.ssh, etc.); writing
  anywhere outside output_dir; from ABI 4, outbound TCP entirely (this schema's
  network.mode is always "none", so this is pure defence in depth); from ABI 6, signalling
  a process outside this domain.
- Does NOT block, and this module never claims it does:
  - a denied path *nested inside* a granted read. Landlock has no "allow this tree except
    that subtree" rule, only allow-list rules on whole subtrees. A daemon that reads
    "~/spark-core" gets no Landlock protection against also reading
    "~/spark-core/data" (BASE_DENY). That gap is covered by the audit hook (guard.py) and
    by InaccessiblePaths in the generated systemd unit (unitgen.py), and is reported here
    (gaps()) so it is never silently assumed to be closed by this layer alone.
  - stat() of a denied path (existence and metadata are still visible).
  - UDP traffic, or anything before ABI 4 (Landlock's network rules did not exist yet).

Policy PD-15 (an owner ruling, unresolved): Landlock unavailable, or older than
MIN_USABLE_ABI, is a start-up refusal (apply_supervisor_domain raises). Since contract 5
there is no test mode that tolerates it: the self-tests and the battery need a kernel with
Landlock too, and DB-17's probe reports N/A on one without it.
"""

import ctypes
import os

from . import pathpolicy

# landlock_create_ruleset / landlock_add_rule / landlock_restrict_self. Generic syscall table
# numbers: the same on x86_64 and aarch64 (the DGX Spark), unlike some older architectures'
# syscall tables.
_NR_CREATE_RULESET = 444
_NR_ADD_RULE = 445
_NR_RESTRICT_SELF = 446
_LANDLOCK_CREATE_RULESET_VERSION = 1 << 0
_PR_SET_NO_NEW_PRIVS = 38
_RULE_TYPE_PATH_BENEATH = 1

ACCESS_FS_EXECUTE = 1 << 0
ACCESS_FS_WRITE_FILE = 1 << 1
ACCESS_FS_READ_FILE = 1 << 2
ACCESS_FS_READ_DIR = 1 << 3
_ACCESS_FS_REFER = 1 << 13      # ABI 2+: cross-directory rename/link
_ACCESS_FS_TRUNCATE = 1 << 14   # ABI 3+
_ACCESS_FS_IOCTL_DEV = 1 << 15  # ABI 5+
_ACCESS_NET_BIND_TCP = 1 << 0
_ACCESS_NET_CONNECT_TCP = 1 << 1
_SCOPE_SIGNAL = 1 << 1          # ABI 6+

# The minimum ABI this module will rely on (PD-15). Below this, apply_supervisor_domain
# treats Landlock as unavailable. ABI 2 (Linux 5.19, 2022) added FS_REFER, which a future
# out-of-process worker's tmp/ -> output/ rename would need (AP-04); the current single-
# process runtime only ever renames within one directory, which needs no REFER at all, but
# the floor is set for what the adopted design needs, not only what today's code happens to.
MIN_USABLE_ABI = 2

READ = ACCESS_FS_EXECUTE | ACCESS_FS_READ_FILE | ACCESS_FS_READ_DIR
# r4.6 (HF-36): read without execute on the daemon's own folder, its reads and its output
# directory. r4.11 (R-2b): the system and interpreter paths lost execute too. v5 (R-2): a daemon
# runs no program, so the domain grants execute nowhere at all, and with it the loader residual
# of R-2b is gone. Python still loads native modules, which needs read and mmap, not execute
# (evidence/r4.11/exec_grant_probe.txt).
READ_NO_EXEC = ACCESS_FS_READ_FILE | ACCESS_FS_READ_DIR
# Every fs access right this module knows how to name, regardless of which ABI actually
# grants each one; restrict_self() ANDs this down to what the running ABI handles.
ALL_FS_ACCESS = (1 << 16) - 1


class LandlockError(OSError):
    """Landlock is unavailable, older than MIN_USABLE_ABI, or a rule could not be applied."""


class _RulesetAttr(ctypes.Structure):
    _fields_ = [("fs", ctypes.c_uint64), ("net", ctypes.c_uint64), ("scoped", ctypes.c_uint64)]


class _PathBeneath(ctypes.Structure):
    _pack_ = 1
    _fields_ = [("allowed_access", ctypes.c_uint64), ("parent_fd", ctypes.c_int32)]


def _libc():
    lib = ctypes.CDLL(None, use_errno=True)
    lib.syscall.restype = ctypes.c_long
    lib.prctl.restype = ctypes.c_int
    return lib


def abi_version() -> int:
    """The kernel's Landlock ABI (>=1). A non-positive result is -errno: -ENOSYS means no
    Landlock support at all; -EOPNOTSUPP means it is disabled (a boot parameter)."""
    lib = _libc()
    ctypes.set_errno(0)
    result = lib.syscall(_NR_CREATE_RULESET, None, ctypes.c_size_t(0),
                         ctypes.c_uint32(_LANDLOCK_CREATE_RULESET_VERSION))
    return result if result > 0 else -ctypes.get_errno()


def _fs_access_mask(abi: int) -> int:
    mask = (1 << 13) - 1  # every ABI-1 file access right (bits 0-12)
    if abi >= 2:
        mask |= _ACCESS_FS_REFER
    if abi >= 3:
        mask |= _ACCESS_FS_TRUNCATE
    if abi >= 5:
        mask |= _ACCESS_FS_IOCTL_DEV
    return mask


def _fs_access_for(path: str, requested: int) -> int:
    """A file rule may only carry file-shaped rights; the kernel rejects a directory-only
    right (READ_DIR) on a regular file. Directory rules may carry every right."""
    if os.path.isdir(path):
        return requested
    return requested & (ACCESS_FS_EXECUTE | ACCESS_FS_WRITE_FILE | ACCESS_FS_READ_FILE
                        | _ACCESS_FS_TRUNCATE | _ACCESS_FS_IOCTL_DEV)


def restrict_self(rules, *, scope_signals: bool = True) -> int:
    """Apply a Landlock domain to the calling process. Irreversible; inherited by children,
    who may only narrow it further, never widen it (this is what lets a worker process get a
    strictly smaller domain than its supervisor - AP-04).

    rules: an iterable of (path, access_bits) pairs. Every fs access right the running ABI
    knows becomes "handled": anything not explicitly granted under a handled right is denied
    for the rest of this process's life. A path that does not exist is skipped, not an error
    (a manifest is allowed to declare a read path that does not exist yet). Outbound TCP
    (ABI 4+) is always fully handled and never granted: no rule type here ever opens it, and
    this schema's network.mode is always "none", so there is nothing to parameterize.

    Returns the ABI version actually used. Raises LandlockError below MIN_USABLE_ABI, or if
    any syscall in the sequence fails (create, a rule, prctl, or restrict_self itself) - a
    partially-applied Landlock domain is not left in place: only landlock_restrict_self
    commits anything, and every failure before that point simply closes the unused ruleset
    fd and raises without having restricted the process at all."""
    lib = _libc()
    abi = abi_version()
    if abi < MIN_USABLE_ABI:
        raise LandlockError(f"Landlock ABI {abi} is unavailable or below the minimum usable ABI {MIN_USABLE_ABI}")

    fs_handled = _fs_access_mask(abi)
    net_handled = (_ACCESS_NET_BIND_TCP | _ACCESS_NET_CONNECT_TCP) if abi >= 4 else 0
    scoped_handled = _SCOPE_SIGNAL if (abi >= 6 and scope_signals) else 0
    attr = _RulesetAttr(fs_handled, net_handled, scoped_handled)
    attr_size = 8 if abi < 4 else (16 if abi < 6 else 24)  # the struct grew across ABI 4 and ABI 6

    ctypes.set_errno(0)
    ruleset_fd = lib.syscall(_NR_CREATE_RULESET, ctypes.byref(attr), ctypes.c_size_t(attr_size),
                             ctypes.c_uint32(0))
    if ruleset_fd < 0:
        raise LandlockError(f"landlock_create_ruleset failed: errno {ctypes.get_errno()}")
    try:
        for path, access in rules:
            if not os.path.exists(path):
                continue
            fd = os.open(path, os.O_PATH | os.O_CLOEXEC)
            try:
                granted = _fs_access_for(path, access) & fs_handled
                if not granted:
                    continue
                rule = _PathBeneath(granted, fd)
                ctypes.set_errno(0)
                rc = lib.syscall(_NR_ADD_RULE, ctypes.c_int(ruleset_fd),
                                 ctypes.c_int(_RULE_TYPE_PATH_BENEATH), ctypes.byref(rule),
                                 ctypes.c_uint32(0))
                if rc != 0:
                    raise LandlockError(f"landlock_add_rule failed for {path!r}: errno {ctypes.get_errno()}")
            finally:
                os.close(fd)
        ctypes.set_errno(0)
        if lib.prctl(_PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0:
            raise LandlockError(f"prctl(PR_SET_NO_NEW_PRIVS) failed: errno {ctypes.get_errno()}")
        ctypes.set_errno(0)
        if lib.syscall(_NR_RESTRICT_SELF, ctypes.c_int(ruleset_fd), ctypes.c_uint32(0)) != 0:
            raise LandlockError(f"landlock_restrict_self failed: errno {ctypes.get_errno()}")
    finally:
        os.close(ruleset_fd)
    return abi


def gaps(policy) -> list:
    """Denied paths that sit inside a granted read (pathpolicy.gaps). Landlock cannot close
    these on its own. Recorded in DAEMON_START so a gap is always visible, never assumed away."""
    return pathpolicy.gaps(policy)


def apply_supervisor_domain(policy, *, extra_read_paths=()) -> dict:
    """The domain runtime.py applies once, before the audit hook and before daemon code is
    imported: read without execute on the system and interpreter paths (system_read_paths()),
    the daemon's own folder and every declared read path that exists; execute nowhere (v5, R-2);
    every access right except execute on the output directory (it already exists by this
    point - guard.prepare_output_dir ran earlier). No TCP bind or connect from ABI 4; signal
    scoping from ABI 6 (PD-15/L2). r4.6 (HF-36): execute was granted everywhere read was.

    Returns a dict for DAEMON_START.landlock: {abi, status, gaps}, status "enforced". Never
    "partially enforced": an unavailable or too-old Landlock, a failed rule or a failed
    restrict_self call raises (fail closed, PD-15). Since contract 5 there is no test mode
    that makes an unavailable Landlock non-fatal."""
    try:
        abi = abi_version()
    except OSError:
        abi = -1
    if abi < MIN_USABLE_ABI:
        raise LandlockError(
            f"Landlock ABI {abi} unavailable or below the minimum usable ABI {MIN_USABLE_ABI}; PD-15 requires it")

    # The kernel projection of the one path policy (pathpolicy.grants, contract 5): only the
    # access masks and the existence checks are Landlock's own.
    access = {pathpolicy.READ: READ_NO_EXEC, pathpolicy.WRITE: ALL_FS_ACCESS & ~ACCESS_FS_EXECUTE}
    rules = [(path, access[kind]) for path, kind in pathpolicy.grants(policy, extra_read_paths)
             if (os.path.isdir(path) if kind == pathpolicy.WRITE else os.path.exists(path))]
    used_abi = restrict_self(rules, scope_signals=True)
    return {"abi": used_abi, "status": "enforced", "gaps": gaps(policy)}


def system_read_paths():
    """Read paths every daemon needs regardless of its manifest: the Python runtime it is
    running under, and the system libraries the interpreter loads. Read only, never execute."""
    import sys
    paths = ["/usr", "/etc/ld.so.cache"]
    for candidate in ("/lib", "/lib64"):
        if os.path.exists(candidate):
            paths.append(candidate)
    for candidate in sorted({sys.prefix, sys.base_prefix, sys.exec_prefix, sys.base_exec_prefix}):
        if candidate and candidate not in paths and not candidate.startswith("/usr"):
            paths.append(candidate)
    return tuple(paths)
