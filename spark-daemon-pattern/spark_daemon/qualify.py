"""Qualification facts shared by the two gates (contract 5, E-11).

An installable unit is a projection of a qualifying battery report (judge.unit_from_report):
the report binds every input of the unit generator, so the unit is reproducible from the
report alone. There is no separate qualification record any more. The two gates each check
what only they can, and neither repeats the other's predicate:

- installing (`spark-daemon unit --report R`): the report qualifies (judge.conclusions) and
  was made on this host (host_facts); then the unit is projected from it;
- the runtime, at every start: the manifest, daemon.py and contract digests are the ones the
  unit's ExecStart carries (EXPECT_KEYS). Code or a manifest changed since the battery never
  starts.

Nothing here installs, enables or starts anything.
"""

import os
import shutil
import subprocess

from .canonical import sha256_hex

EXPECT_KEYS = ("manifest_sha256", "daemon_code_sha256", "contract_sha256")


def file_sha256(path) -> str:
    with open(path, "rb") as fh:
        return sha256_hex(fh.read())


def expected_identity(manifest_path) -> dict:
    """The three digests a qualified unit carries, computed from the files as they are now."""
    from . import manifest as manifest_mod
    from .contract import contract_identity
    m = manifest_mod.load(manifest_path)
    code = os.path.join(os.path.dirname(os.path.abspath(manifest_path)), "daemon.py")
    return {"manifest_sha256": m.sha256, "daemon_code_sha256": file_sha256(code),
            "contract_sha256": contract_identity()["contract_sha256"]}


def expect_args(expect: dict) -> list:
    return [arg for key in EXPECT_KEYS for arg in (f"--expect-{key.replace('_', '-')}", expect[key])]


def host_facts() -> dict:
    """What a PASS is bound to (G-5): a PASS on one host must not qualify another."""
    from . import landlock
    facts = {"kernel": os.uname().release, "machine": os.uname().machine,
             "landlock_abi": landlock.abi_version(),
             "cgroup": "v2" if os.path.exists("/sys/fs/cgroup/cgroup.controllers") else "v1",
             "systemd": None, "machine_id_sha256": None}
    systemctl = shutil.which("systemctl", path="/usr/bin:/bin")
    if systemctl:
        try:
            out = subprocess.run([systemctl, "--version"], capture_output=True, text=True, timeout=10,
                                 env={"PATH": "/usr/bin:/bin", "LANG": "C"}).stdout
            facts["systemd"] = (out.splitlines() or [""])[0].strip() or None
        except (OSError, subprocess.SubprocessError):
            pass
    try:
        with open("/etc/machine-id", "rb") as fh:
            value = fh.read(64).strip()
        facts["machine_id_sha256"] = sha256_hex(value) if value else None
    except OSError:
        pass
    return facts


def host_differences(recorded) -> list:
    """How this host differs from the one a report was made on (empty: the same host)."""
    if not isinstance(recorded, dict) or not recorded:
        return ["the report names no host"]
    now = host_facts()
    if set(recorded) != set(now):
        return ["the report's host facts are not the set this skeleton records"]
    return [f"host {key} differs from the report's ({value!r} then, {now.get(key)!r} now)"
            for key, value in sorted(recorded.items()) if now.get(key) != value]
