"""Qualified execution identity for UDC observe-profile admission.

This module is deliberately small and non-authoritative. It never installs, activates, resolves,
fetches or imports an object by hash. It only measures already-local bytes, canonicalizes a
closed identity object, and computes the public content address used for equality checks.

Security boundary:
- execution_sha256 proves identity, never permission;
- the expected root is trusted only through the owner-qualified, root-owned deployment path;
- hostile root is outside UDC's threat model;
- loader, shared libraries and stdlib remain host TCB;
- policy, host facts, ledger head and authority are intentionally outside this static root.
"""

from __future__ import annotations

import hashlib
import os
import platform
import re
import stat
import sys

from .canonical import canonical_bytes

EXECUTION_DOMAIN = "UDC.EXECUTION.OBSERVE.V1"
EXECUTION_DOMAIN_BYTES = EXECUTION_DOMAIN.encode("ascii") + b"\0"

_HEX64 = re.compile(r"^[0-9a-f]{64}$")
_IMPLEMENTATION = re.compile(r"^[a-z][a-z0-9._-]{0,31}$")
_VERSION = re.compile(r"^[0-9A-Za-z.+_-]{1,64}$")

IDENTITY_KEYS = frozenset({
    "contract_sha256",
    "manifest_sha256",
    "daemon_code_sha256",
    "runtime_bundle_sha256",
    "interpreter",
})
INTERPRETER_KEYS = frozenset({
    "implementation",
    "version",
    "executable_realpath",
    "executable_sha256",
})

# ADM-9 / V-1 frozen runtime bundle: one entrypoint, all package source files, and the
# independent verifier. Tests, evidence, vendoring, authoring entrypoints and tools stay out.
# The list is explicit on purpose: adding executable/importable package code is a review event.
RUNTIME_BUNDLE_FILES = (
    "bin/spark-daemon",
    "spark_daemon/__init__.py",
    "spark_daemon/author.py",
    "spark_daemon/battery.py",
    "spark_daemon/canonical.py",
    "spark_daemon/cli.py",
    "spark_daemon/context.py",
    "spark_daemon/contract.py",
    "spark_daemon/guard.py",
    "spark_daemon/handoff.py",
    "spark_daemon/identity.py",
    "spark_daemon/judge.py",
    "spark_daemon/landlock.py",
    "spark_daemon/ledger.py",
    "spark_daemon/manifest.py",
    "spark_daemon/notify.py",
    "spark_daemon/pathpolicy.py",
    "spark_daemon/paths.py",
    "spark_daemon/probes.py",
    "spark_daemon/purity.py",
    "spark_daemon/qualify.py",
    "spark_daemon/render.py",
    "spark_daemon/runtime.py",
    "spark_daemon/scaffold.py",
    "spark_daemon/semantics.py",
    "spark_daemon/status.py",
    "spark_daemon/unitgen.py",
    "verifier/ledger_verify.py",
)

FORBIDDEN_EXEC_ENV = (
    "LD_AUDIT",
    "LD_LIBRARY_PATH",
    "LD_PRELOAD",
    "PYTHONHOME",
    "PYTHONPATH",
    "PYTHONSTARTUP",
)


class IdentityError(ValueError):
    """The execution identity cannot be constructed or verified."""


def _require_hex(label: str, value) -> str:
    if not isinstance(value, str) or not _HEX64.fullmatch(value):
        raise IdentityError(f"{label} must be a full lowercase SHA-256 hex digest")
    return value


def _validate_interpreter(value) -> dict:
    if not isinstance(value, dict) or set(value) != INTERPRETER_KEYS:
        raise IdentityError("interpreter must carry exactly implementation, version, executable_realpath, executable_sha256")
    implementation = value.get("implementation")
    version = value.get("version")
    path = value.get("executable_realpath")
    if not isinstance(implementation, str) or not _IMPLEMENTATION.fullmatch(implementation):
        raise IdentityError("interpreter.implementation has an invalid form")
    if not isinstance(version, str) or not _VERSION.fullmatch(version):
        raise IdentityError("interpreter.version has an invalid form")
    if not isinstance(path, str) or "\0" in path or not os.path.isabs(path):
        raise IdentityError("interpreter.executable_realpath must be an absolute path")
    return {
        "implementation": implementation,
        "version": version,
        "executable_realpath": os.path.realpath(path),
        "executable_sha256": _require_hex("interpreter.executable_sha256", value.get("executable_sha256")),
    }


def validate_qualified_execution_identity(value) -> dict:
    """Return a normalized closed QualifiedExecutionIdentity or raise IdentityError."""
    if not isinstance(value, dict) or set(value) != IDENTITY_KEYS:
        raise IdentityError("QualifiedExecutionIdentity has unknown or missing fields")
    return {
        "contract_sha256": _require_hex("contract_sha256", value.get("contract_sha256")),
        "manifest_sha256": _require_hex("manifest_sha256", value.get("manifest_sha256")),
        "daemon_code_sha256": _require_hex("daemon_code_sha256", value.get("daemon_code_sha256")),
        "runtime_bundle_sha256": _require_hex("runtime_bundle_sha256", value.get("runtime_bundle_sha256")),
        "interpreter": _validate_interpreter(value.get("interpreter")),
    }


def make_qualified_execution_identity(*, contract_sha256: str, manifest_sha256: str,
                                      daemon_code_sha256: str, runtime_bundle_sha256: str,
                                      interpreter: dict) -> dict:
    return validate_qualified_execution_identity({
        "contract_sha256": contract_sha256,
        "manifest_sha256": manifest_sha256,
        "daemon_code_sha256": daemon_code_sha256,
        "runtime_bundle_sha256": runtime_bundle_sha256,
        "interpreter": interpreter,
    })


def execution_sha256(identity: dict) -> str:
    """Public content address for the observe-profile QualifiedExecutionIdentity."""
    normalized = validate_qualified_execution_identity(identity)
    return hashlib.sha256(EXECUTION_DOMAIN_BYTES + canonical_bytes(normalized)).hexdigest()


def _sha256_fileobj(fh) -> str:
    h = hashlib.sha256()
    while True:
        chunk = fh.read(1024 * 1024)
        if not chunk:
            return h.hexdigest()
        h.update(chunk)


def measure_interpreter() -> dict:
    """Measure the executable backing this Linux process, not a path reopened through sys.executable."""
    proc_exe = "/proc/self/exe"
    if not os.path.exists(proc_exe):
        raise IdentityError("/proc/self/exe is required by the qualified Linux host profile")
    try:
        with open(proc_exe, "rb", buffering=0) as fh:
            st = os.fstat(fh.fileno())
            if not stat.S_ISREG(st.st_mode):
                raise IdentityError("/proc/self/exe does not refer to a regular executable")
            digest = _sha256_fileobj(fh)
    except OSError as e:
        raise IdentityError(f"cannot measure /proc/self/exe ({e.__class__.__name__})") from None
    return _validate_interpreter({
        "implementation": platform.python_implementation().lower(),
        "version": platform.python_version(),
        "executable_realpath": os.path.realpath(proc_exe),
        "executable_sha256": digest,
    })


def execution_environment_problems(env=None, *, isolated=None) -> tuple:
    """Names of execution-environment conditions that disagree with the qualified profile.

    Loader variables must be neutralized before Python exec by the qualified unit. This runtime
    check is a consistency/refusal check, not the pre-exec security boundary. Values are never
    returned or logged.
    """
    source = os.environ if env is None else env
    problems = [name for name in FORBIDDEN_EXEC_ENV if source.get(name)]
    is_isolated = bool(sys.flags.isolated) if isolated is None else bool(isolated)
    if not is_isolated:
        problems.append("PYTHON_NOT_ISOLATED")
    return tuple(problems)


def _safe_relative(path: str) -> bool:
    if not isinstance(path, str) or not path or path.startswith("/") or "\0" in path:
        return False
    parts = path.split("/")
    return all(part not in ("", ".", "..") for part in parts)


def _reject_symlink_components(root: str, rel: str) -> None:
    current = root
    for part in rel.split("/")[:-1]:
        current = os.path.join(current, part)
        try:
            st = os.lstat(current)
        except OSError as e:
            raise IdentityError(f"runtime bundle path {rel!r} cannot be inspected ({e.__class__.__name__})") from None
        if stat.S_ISLNK(st.st_mode):
            raise IdentityError(f"runtime bundle path {rel!r} traverses a symlink")


def _read_bundle_file(root: str, rel: str) -> bytes:
    if not _safe_relative(rel):
        raise IdentityError(f"invalid frozen runtime-bundle path {rel!r}")
    _reject_symlink_components(root, rel)
    full = os.path.join(root, *rel.split("/"))
    try:
        st = os.lstat(full)
    except OSError as e:
        raise IdentityError(f"runtime bundle file {rel!r} is missing ({e.__class__.__name__})") from None
    if stat.S_ISLNK(st.st_mode) or not stat.S_ISREG(st.st_mode):
        raise IdentityError(f"runtime bundle file {rel!r} must be a regular non-symlink file")
    with open(full, "rb") as fh:
        return fh.read()


def _discovered_closed_files(root: str) -> set:
    """Files under the closed package/verifier roots; bytecode and symlinks always refuse."""
    found = set()
    for top in ("spark_daemon", "verifier"):
        base = os.path.join(root, top)
        try:
            base_st = os.lstat(base)
        except OSError as e:
            raise IdentityError(f"runtime bundle root {top!r} is missing ({e.__class__.__name__})") from None
        if stat.S_ISLNK(base_st.st_mode) or not stat.S_ISDIR(base_st.st_mode):
            raise IdentityError(f"runtime bundle root {top!r} must be a regular directory")
        for current, dirs, names in os.walk(base, topdown=True, followlinks=False):
            for d in list(dirs):
                full = os.path.join(current, d)
                st = os.lstat(full)
                if stat.S_ISLNK(st.st_mode):
                    raise IdentityError("runtime bundle contains a symlink directory")
                if d == "__pycache__":
                    raise IdentityError("runtime bundle contains __pycache__")
            for name in names:
                full = os.path.join(current, name)
                st = os.lstat(full)
                if stat.S_ISLNK(st.st_mode) or not stat.S_ISREG(st.st_mode):
                    raise IdentityError("runtime bundle contains a non-regular file")
                if name.endswith(".pyc"):
                    raise IdentityError("runtime bundle contains .pyc bytecode")
                found.add(os.path.relpath(full, root).replace(os.sep, "/"))
    return found


def runtime_bundle_sha256(root: str) -> str:
    """Digest the frozen ADM-9 runtime file list using the existing deterministic tree-hash shape."""
    root = os.path.realpath(root)
    expected_closed = {p for p in RUNTIME_BUNDLE_FILES if p.startswith(("spark_daemon/", "verifier/"))}
    discovered = _discovered_closed_files(root)
    if discovered != expected_closed:
        missing = sorted(expected_closed - discovered)
        extra = sorted(discovered - expected_closed)
        detail = []
        if missing:
            detail.append("missing " + ", ".join(missing))
        if extra:
            detail.append("unexpected " + ", ".join(extra))
        raise IdentityError("runtime bundle file set differs from the frozen list: " + "; ".join(detail))

    h = hashlib.sha256()
    for rel in sorted(RUNTIME_BUNDLE_FILES, key=lambda p: p.encode("utf-8")):
        data = _read_bundle_file(root, rel)
        digest = hashlib.sha256(data).hexdigest().encode("ascii")
        h.update(rel.encode("utf-8") + b"\0" + digest + b"\n")
    return h.hexdigest()
