"""The daemon manifest: a closed schema that declares everything about a daemon's safety.

A manifest lives in manifest.json beside the daemon's code (daemon.py). Unknown keys are
refused at every level, every value is bounded, and v1 only admits observe-class daemons
with no network access. See README.md for the field reference.
"""

import json
import os
import re
from dataclasses import dataclass

from . import MANIFEST_SCHEMA, RESERVED_EVENT_TYPES
from .canonical import CanonicalError, canonical_bytes, sha256_hex, strict_loads
from .paths import expand, within

NAME_RE = re.compile(r"^[a-z][a-z0-9-]{2,39}$")
VERSION_RE = re.compile(r"^(0|[1-9]\d{0,3})\.(0|[1-9]\d{0,3})\.(0|[1-9]\d{0,3})$")
EVENT_RE = re.compile(r"^[A-Z][A-Z0-9_]{2,47}$")
# Paths: absolute or "~/", and only characters that are safe in a systemd unit file.
PATH_RE = re.compile(r"^(~|~/[A-Za-z0-9._/-]*|/[A-Za-z0-9._/-]*)$")
COMMAND_RE = re.compile(r"^[a-z][a-z0-9._-]{0,31}$")
USER_RE = re.compile(r"^[a-z_][a-z0-9_-]{0,31}$")
PURPOSE_RE = re.compile(r"^[A-Za-z0-9 .,;:()/'+-]{10,200}$")

# Always denied, for reading and writing, whatever the manifest says (never removable).
BASE_DENY = (
    "~/spark-core/data",
    "~/spark-governance/history",
    "~/.ssh",
    "~/.gnupg",
    "~/.claude",
    "~/.codex",
)
# An output directory may not sit inside any of these.
FORBIDDEN_OUTPUT_ROOTS = ("~/spark-core", "~/spark-governance") + BASE_DENY[2:]
# Shells, interpreters, network clients and privilege tools are never runnable.
FORBIDDEN_COMMANDS = frozenset({
    "sh", "bash", "dash", "zsh", "ksh", "fish", "env", "xargs", "find", "awk", "sed", "perl",
    "python", "python3", "node", "ruby", "sudo", "su", "doas", "pkexec", "ssh", "scp", "sftp",
    "rsync", "curl", "wget", "nc", "ncat", "socat", "telnet", "ftp", "docker", "podman",
    "systemctl", "systemd-run", "loginctl", "mount", "umount", "chmod", "chown", "rm", "mv",
    "cp", "dd", "tee", "sqlite3", "crontab", "at",
})

TOP_KEYS = {
    "manifest_schema", "name", "version", "purpose", "daemon_class", "trigger", "reads",
    "commands", "output_dir", "deny", "network", "run_as", "resources", "watchdog_seconds",
    "step_timeout_seconds", "ledger", "digest",
}


class ManifestError(ValueError):
    def __init__(self, problems):
        self.problems = list(problems)
        super().__init__("; ".join(self.problems))


@dataclass(frozen=True)
class Trigger:
    kind: str
    interval_seconds: int


@dataclass(frozen=True)
class RunAs:
    unit: str
    user: str


@dataclass(frozen=True)
class Resources:
    cpu_weight: int
    cpu_budget_bp: int
    memory_max_mb: int
    tasks_max: int
    io_class: str


@dataclass(frozen=True)
class LedgerSpec:
    record_max_bytes: int
    event_types: tuple


@dataclass(frozen=True)
class DigestSpec:
    enabled: bool
    max_bytes: int


@dataclass(frozen=True)
class Manifest:
    name: str
    version: str
    purpose: str
    daemon_class: str
    trigger: Trigger
    reads: tuple
    commands: tuple
    output_dir: str
    deny: tuple
    network_mode: str
    run_as: RunAs
    resources: Resources
    watchdog_seconds: int
    step_timeout_seconds: int
    ledger: LedgerSpec
    digest: DigestSpec
    sha256: str          # sha256 of the canonical manifest, recorded in DAEMON_START
    path: str            # absolute path of manifest.json

    @property
    def code_path(self) -> str:
        return os.path.join(os.path.dirname(self.path), "daemon.py")

    @property
    def all_deny(self) -> tuple:
        return BASE_DENY + self.deny


class _Checker:
    def __init__(self):
        self.problems = []

    def fail(self, where, message):
        self.problems.append(f"{where}: {message}")

    def keys(self, where, obj, allowed, required=None):
        if not isinstance(obj, dict):
            self.fail(where, "must be an object")
            return False
        for key in sorted(set(obj) - set(allowed)):
            self.fail(where, f"unknown key {key!r}")
        for key in sorted(set(required if required is not None else allowed) - set(obj)):
            self.fail(where, f"missing key {key!r}")
        return True

    def integer(self, where, value, low, high):
        if isinstance(value, bool) or not isinstance(value, int):
            self.fail(where, "must be an integer")
            return None
        if not low <= value <= high:
            self.fail(where, f"must be between {low} and {high}")
            return None
        return value

    def text(self, where, value, pattern, hint):
        if not isinstance(value, str) or not pattern.match(value):
            self.fail(where, hint)
            return None
        return value

    def choice(self, where, value, allowed, reserved=None):
        if reserved and value in reserved:
            self.fail(where, f"{value!r} is reserved: {reserved[value]}")
            return None
        if value not in allowed:
            self.fail(where, f"must be one of {', '.join(repr(a) for a in allowed)}")
            return None
        return value

    def path_list(self, where, value, low, high):
        if not isinstance(value, list) or not low <= len(value) <= high:
            self.fail(where, f"must be a list of {low} to {high} paths")
            return ()
        out = []
        for i, item in enumerate(value):
            p = self.text(f"{where}[{i}]", item, PATH_RE,
                          "must be an absolute or ~/ path of letters, digits and ._/-")
            if p is not None:
                if p in out:
                    self.fail(f"{where}[{i}]", "duplicate path")
                out.append(p)
        return tuple(out)


def parse(data: dict, path: str = "<memory>") -> Manifest:
    c = _Checker()
    if not c.keys("manifest", data, TOP_KEYS):
        raise ManifestError(c.problems)

    if data.get("manifest_schema") != MANIFEST_SCHEMA:
        c.fail("manifest_schema", f"must be {MANIFEST_SCHEMA!r}")
    name = c.text("name", data.get("name"), NAME_RE,
                  "must be 3-40 characters: lowercase letters, digits and '-', starting with a letter")
    version = c.text("version", data.get("version"), VERSION_RE, "must look like 1.2.3")
    purpose = c.text("purpose", data.get("purpose"), PURPOSE_RE,
                     "must be one line of 10-200 plain characters")
    daemon_class = c.choice("daemon_class", data.get("daemon_class"), ("observe",),
                            {"act": "acting daemons need their own pattern and authority gate"})

    trigger = None
    t = data.get("trigger")
    if c.keys("trigger", t, {"kind", "interval_seconds"}):
        kind = c.choice("trigger.kind", t.get("kind"), ("poll",),
                        {"inotify-wakeup": "deferred (Observer proposal C2)"})
        interval = c.integer("trigger.interval_seconds", t.get("interval_seconds"), 5, 86400)
        if kind and interval:
            trigger = Trigger(kind, interval)

    reads = c.path_list("reads", data.get("reads"), 1, 32)
    deny = c.path_list("deny", data.get("deny"), 0, 16)

    commands = ()
    cmds = data.get("commands")
    if not isinstance(cmds, list) or len(cmds) > 8:
        c.fail("commands", "must be a list of at most 8 command names")
    else:
        seen = []
        for i, cmd in enumerate(cmds):
            name_ok = c.text(f"commands[{i}]", cmd, COMMAND_RE, "must be a bare command name")
            if name_ok is None:
                continue
            if cmd in FORBIDDEN_COMMANDS:
                c.fail(f"commands[{i}]", f"{cmd!r} is never allowed (shell, interpreter, network or privilege tool)")
            elif cmd in seen:
                c.fail(f"commands[{i}]", "duplicate command")
            else:
                seen.append(cmd)
        commands = tuple(seen)

    output_dir = c.text("output_dir", data.get("output_dir"), PATH_RE,
                        "must be an absolute or ~/ path of letters, digits and ._/-")

    network_mode = None
    n = data.get("network")
    if c.keys("network", n, {"mode"}):
        network_mode = c.choice("network.mode", n.get("mode"), ("none",),
                                {"named": "outbound access needs relaxation R2 and a named destination"})

    run_as = None
    r = data.get("run_as")
    if c.keys("run_as", r, {"unit", "user"}, required={"unit"}):
        unit = c.choice("run_as.unit", r.get("unit"), ("system", "user"))
        user = r.get("user", "")
        if unit == "system":
            user = c.text("run_as.user", user, USER_RE, "a system unit needs a dedicated user name")
            if user in ("root",):
                c.fail("run_as.user", "must not be root")
                user = None
        elif "user" in r:
            c.fail("run_as.user", "only allowed for system units")
            user = None
        if unit and user is not None:
            run_as = RunAs(unit, user)

    resources = None
    res = data.get("resources")
    if c.keys("resources", res, {"cpu_weight", "cpu_budget_bp", "memory_max_mb", "tasks_max", "io_class"}):
        weight = c.integer("resources.cpu_weight", res.get("cpu_weight"), 1, 100)
        budget = c.integer("resources.cpu_budget_bp", res.get("cpu_budget_bp"), 1, 10000)
        memory = c.integer("resources.memory_max_mb", res.get("memory_max_mb"), 64, 2048)
        tasks = c.integer("resources.tasks_max", res.get("tasks_max"), 4, 64)
        io_class = c.choice("resources.io_class", res.get("io_class"), ("idle", "best-effort"))
        if None not in (weight, budget, memory, tasks, io_class):
            resources = Resources(weight, budget, memory, tasks, io_class)

    watchdog = c.integer("watchdog_seconds", data.get("watchdog_seconds"), 10, 3600)
    step_timeout = c.integer("step_timeout_seconds", data.get("step_timeout_seconds"), 1, 600)
    if watchdog and step_timeout and step_timeout * 2 > watchdog:
        c.fail("step_timeout_seconds", "must be at most half of watchdog_seconds")

    ledger = None
    lg = data.get("ledger")
    if c.keys("ledger", lg, {"record_max_bytes", "event_types"}):
        rmax = c.integer("ledger.record_max_bytes", lg.get("record_max_bytes"), 1024, 1048576)
        types = lg.get("event_types")
        ok_types = []
        if not isinstance(types, list) or not 1 <= len(types) <= 32:
            c.fail("ledger.event_types", "must be a list of 1 to 32 event type names")
        else:
            for i, et in enumerate(types):
                if c.text(f"ledger.event_types[{i}]", et, EVENT_RE,
                          "must be UPPER_SNAKE_CASE, 3-48 characters") is None:
                    continue
                if et in RESERVED_EVENT_TYPES:
                    c.fail(f"ledger.event_types[{i}]", f"{et} is reserved for the skeleton")
                elif et in ok_types:
                    c.fail(f"ledger.event_types[{i}]", "duplicate event type")
                else:
                    ok_types.append(et)
        if rmax:
            ledger = LedgerSpec(rmax, tuple(ok_types))

    digest = None
    dg = data.get("digest")
    if c.keys("digest", dg, {"enabled", "max_bytes"}):
        enabled = dg.get("enabled")
        if not isinstance(enabled, bool):
            c.fail("digest.enabled", "must be true or false")
        dmax = c.integer("digest.max_bytes", dg.get("max_bytes"), 1024, 262144)
        if isinstance(enabled, bool) and dmax:
            digest = DigestSpec(enabled, dmax)

    # Relations between paths, checked on expanded paths.
    if output_dir and reads:
        out = expand(output_dir)
        for root in FORBIDDEN_OUTPUT_ROOTS:
            if within(out, expand(root)):
                c.fail("output_dir", f"must not be inside {root}")
        for p in reads:
            rp = expand(p)
            if within(out, rp) or within(rp, out):
                c.fail("output_dir", f"must not overlap the read path {p} (a daemon never observes its own output)")
    for i, p in enumerate(reads):
        rp = expand(p)
        for d in BASE_DENY + deny:
            if within(rp, expand(d)):
                c.fail(f"reads[{i}]", f"{p} is inside the denied path {d}")

    if c.problems:
        raise ManifestError(c.problems)

    try:
        digest_sha = sha256_hex(canonical_bytes(data))
    except CanonicalError as e:
        raise ManifestError([f"manifest: {e}"]) from None
    return Manifest(
        name=name, version=version, purpose=purpose, daemon_class=daemon_class,
        trigger=trigger, reads=reads, commands=commands, output_dir=output_dir, deny=deny,
        network_mode=network_mode, run_as=run_as, resources=resources,
        watchdog_seconds=watchdog, step_timeout_seconds=step_timeout, ledger=ledger,
        digest=digest, sha256=digest_sha, path=os.path.abspath(path),
    )


def load(path: str) -> Manifest:
    try:
        with open(path, "rb") as fh:
            raw = fh.read(65537)
    except OSError as e:
        raise ManifestError([f"manifest: cannot read {path}: {e.strerror}"]) from None
    if len(raw) > 65536:
        raise ManifestError(["manifest: larger than 64 KiB"])
    try:
        data = strict_loads(raw)
    except (UnicodeDecodeError, ValueError) as e:
        raise ManifestError([f"manifest: not valid strict JSON ({type(e).__name__})"]) from None
    return parse(data, path)


def to_dict(path: str) -> dict:
    with open(path, "rb") as fh:
        return strict_loads(fh.read())


def dump(data: dict, path: str) -> None:
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(data, fh, indent=2, ensure_ascii=False)
        fh.write("\n")
