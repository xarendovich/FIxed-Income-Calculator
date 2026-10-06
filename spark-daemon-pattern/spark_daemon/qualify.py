"""Qualification: the only path from a battery PASS to an installable unit (r4.11, R-3).

Only the qualifying profile of the one judge (the full battery, not --quick) may emit an
installable unit, and only on PASS. What it writes is evidence for a person's activation
decision, never permission: nothing here installs, enables or starts anything.

`battery --emit-unit DIR` writes two files:

    <unit>.service                the unit; its ExecStart carries the expected digests
    <unit>.qualification.json     what was qualified: the unit's exact bytes, the manifest as
                                  written, daemon.py, the contract, the unit options, the
                                  battery report and the host

Two gates read them:

- the installer's gate, `spark-daemon qualified --unit U --record R`: the unit's bytes, the
  current files, the contract and this host must all match the record. A preview unit
  (`spark-daemon unit`), a hand-edited unit, or a unit whose options changed fails it;
- the runtime's gate, at every start: the manifest, daemon.py and contract digests must equal
  the ones the unit carries, and outside test mode a start that carries none is refused. So a
  preview unit never runs as a service, and code changed after qualification never starts.

Unit options (--require-path, --part-of, --python) are inputs to the battery, not edits after
it: DB-15 and DB-25 judge the exact unit that is emitted, and a different option needs a
fresh qualification.
"""

import json
import os
import shutil
import subprocess

from .canonical import sha256_hex

QUALIFICATION_SCHEMA = "spark-daemon-qualification/1"
RECORD_SUFFIX = ".qualification.json"
AUTHORITY_NOTE = ("Qualification evidence only. It installs, enables and activates nothing; "
                  "activation is a separate decision by a person with that authority.")
EXPECT_KEYS = ("manifest_sha256", "daemon_code_sha256", "contract_sha256")


class Refused(Exception):
    """No installable unit may be emitted, with the reason."""


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


def emit(j, outdir, report_path) -> tuple:
    """Writes the unit and its record for a qualifying PASS. Raises Refused otherwise, having
    written nothing. Returns (unit_path, record_path)."""
    from . import unitgen
    from .battery import PATTERN_ROOT
    from .contract import contract_identity
    if not j.qualifying:
        raise Refused(f"the {j.profile} profile{' with --quick' if j.quick else ''} does not qualify")
    if j.result != "PASS":
        raise Refused(f"the battery result is {j.result}, not PASS")
    if j.m is None or j.code_sha is None:
        raise Refused("the manifest or daemon.py could not be identified")
    identity = contract_identity()
    expect = {"manifest_sha256": j.m.sha256, "daemon_code_sha256": j.code_sha,
              "contract_sha256": identity["contract_sha256"]}
    try:
        text = unitgen.generate(j.m, root=PATTERN_ROOT, expect=expect, **j.unit_options)
    except unitgen.UnitError as e:
        raise Refused(f"the unit cannot be generated: {e}") from None
    if unitgen.lint(text):
        raise Refused(f"the unit is missing directives: {', '.join(unitgen.lint(text))}")
    name = unitgen.unit_name(j.m)
    record = {
        "schema": QUALIFICATION_SCHEMA,
        "unit_name": name,
        "unit_sha256": sha256_hex(text.encode()),
        "manifest_path": j.manifest_path,
        **expect,
        "contract_version": identity["contract_version"],
        "unit_options": j.unit_options,
        "battery": {"profile": j.profile, "qualifying": True, "result": j.result, "seed": j.seed,
                    "report_sha256": file_sha256(report_path)},
        "host": host_facts(),
        "authority": AUTHORITY_NOTE,
    }
    os.makedirs(outdir, exist_ok=True)
    unit_path = os.path.join(outdir, name)
    record_path = unit_path + RECORD_SUFFIX
    with open(unit_path, "w") as fh:
        fh.write(text)
    with open(record_path, "w") as fh:
        json.dump(record, fh, indent=2)
        fh.write("\n")
    return unit_path, record_path


def check(unit_path, record_path) -> list:
    """The installer's gate. Returns the reasons the unit is not qualified (empty: qualified)."""
    from .contract import contract_identity
    try:
        with open(record_path) as fh:
            record = json.load(fh)
    except (OSError, ValueError) as e:
        return [f"no readable qualification record at {record_path} ({e.__class__.__name__})"]
    if not isinstance(record, dict) or record.get("schema") != QUALIFICATION_SCHEMA:
        return [f"the record is not a {QUALIFICATION_SCHEMA} record"]
    problems = []
    battery = record.get("battery") or {}
    if not (battery.get("qualifying") is True and battery.get("result") == "PASS"):
        problems.append("the record is not from a qualifying battery PASS")
    try:
        if file_sha256(unit_path) != record.get("unit_sha256"):
            problems.append("the unit's bytes differ from the qualified unit (edited, a preview, "
                            "or generated with other options): run the battery again")
    except OSError:
        problems.append(f"no unit at {unit_path}")
    if os.path.basename(unit_path) != record.get("unit_name"):
        problems.append(f"the unit file must be named {record.get('unit_name')}")
    try:
        now = expected_identity(record.get("manifest_path") or "")
    except Exception as e:  # noqa: BLE001 - any failure to identify the files means "not qualified"
        problems.append(f"the qualified files cannot be identified now ({e.__class__.__name__})")
    else:
        for key in ("manifest_sha256", "daemon_code_sha256"):
            if now[key] != record.get(key):
                problems.append(f"{key} changed since qualification")
    if contract_identity()["contract_sha256"] != record.get("contract_sha256"):
        problems.append("this skeleton implements a different contract than the one qualified")
    host = host_facts()
    for key, value in (record.get("host") or {}).items():
        if host.get(key) != value:
            problems.append(f"host {key} differs from the qualified host ({value!r} then, {host.get(key)!r} now)")
    return problems
