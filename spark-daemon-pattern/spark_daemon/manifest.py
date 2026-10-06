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
MANIFEST_MAX_BYTES = 65536
# blind_limit_seconds (r3.5): a minute at least; a day at most, so "infinite patience" cannot
# be written down; and at least three poll intervals, so one slow cycle is never fatal.
BLIND_LIMIT_MIN, BLIND_LIMIT_MAX, BLIND_LIMIT_INTERVALS = 60, 86400, 3
# cycle_budget_seconds (r4.11, R-6): the one declared cycle time. Every other timing is derived
# from it here, by the framework, so no two declared numbers can contradict each other (HF-30,
# HF-31). A cycle (sense, decide, digest) that reaches its budget fails and counts as blind time.
CYCLE_BUDGET_MIN, CYCLE_BUDGET_MAX = 5, 1800
WATCHDOG_MARGIN_SECONDS = 10


def watchdog_seconds_for(cycle_budget: int) -> int:
    """systemd WatchdogSec=. The runtime pings between cycles and at least every second while it
    sleeps, so the longest silence is one cycle plus a second; twice the budget plus a margin
    covers it with room for a slow interpreter."""
    return 2 * cycle_budget + WATCHDOG_MARGIN_SECONDS


def stop_timeout_seconds_for(cycle_budget: int) -> int:
    """systemd TimeoutStopSec=. A stop is honoured between cycles (HF-31), so it must outlast one
    watchdog period, which outlasts any cycle."""
    return watchdog_seconds_for(cycle_budget) + 10


def start_timeout_seconds_for(cycle_budget: int) -> int:
    """systemd TimeoutStartSec=: start-up verifies the whole ledger (U-6) before READY=1."""
    return max(60, watchdog_seconds_for(cycle_budget))

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
# Shells, interpreters, network clients, privilege tools, command runners and file
# mutators are never runnable. Names that run other programs (env, xargs, nice, timeout,
# tar --to-command, less !cmd, ...) count as shells: any one of them turns a manifest's
# command list into "run anything".
FORBIDDEN_COMMANDS = frozenset({
    # shells and command runners
    "sh", "bash", "dash", "zsh", "ksh", "mksh", "csh", "tcsh", "fish", "ash", "busybox",
    "toybox", "env", "xargs", "find", "nice", "ionice", "nohup", "timeout", "stdbuf", "setsid",
    "watch", "script", "expect", "chroot", "unshare", "nsenter", "flock", "parallel", "make",
    # text tools that can execute or write files
    "awk", "gawk", "mawk", "nawk", "sed", "ed", "ex", "vi", "vim", "nvim", "nano", "emacs",
    "less", "more", "man", "tar", "zip", "unzip", "patch", "split", "csplit",
    # interpreters and compilers (see also FORBIDDEN_COMMAND_PREFIXES)
    "gcc", "cc", "clang", "ld", "gdb", "strace", "ltrace", "java", "tclsh", "wish", "irb",
    "pip", "pip3", "npm", "npx", "deno", "bun", "pwsh",
    # privilege and identity
    "sudo", "su", "doas", "pkexec", "runuser", "setpriv", "capsh",
    # network
    "ssh", "scp", "sftp", "rsync", "curl", "wget", "nc", "ncat", "netcat", "socat", "telnet",
    "ftp", "openssl", "gpg", "gpg2", "ip", "iptables", "nft", "nmcli", "busctl", "dbus-send",
    # services, containers, scheduling, processes
    "docker", "podman", "systemctl", "systemd-run", "loginctl", "crontab", "at", "batch",
    "kill", "pkill", "killall", "shutdown", "reboot", "halt", "poweroff",
    # file mutation
    "mount", "umount", "chmod", "chown", "chgrp", "rm", "rmdir", "mv", "cp", "ln", "dd", "tee",
    "touch", "truncate", "shred", "install", "mkdir", "mkfifo", "mknod", "sqlite3",
})
# Any command whose name starts with one of these is an interpreter family (python3.12,
# perl5.38, node18, ruby3.2, php8.3, lua5.4, pypy3, ...). Refused like the names above.
FORBIDDEN_COMMAND_PREFIXES = ("python", "pypy", "perl", "node", "ruby", "php", "lua", "tcl",
                              "java", "busybox")


def command_refusal(cmd: str):
    """Why a bare command name is never allowed, or None if it may be declared."""
    if cmd in FORBIDDEN_COMMANDS or cmd.startswith(FORBIDDEN_COMMAND_PREFIXES):
        return f"{cmd!r} is never allowed (shell, interpreter, network, privilege or file-mutation tool)"
    return None


def path_refusal(path: str):
    """Why a manifest path is refused beyond PATH_RE, or None. "." and ".." segments, empty
    segments and trailing slashes are refused so that the path a reviewer reads is the path
    the skeleton enforces (the validator still resolves symlinks before comparing)."""
    body = path[2:] if path.startswith("~/") else path.lstrip("/")
    if path in ("~", "/"):
        return None
    if path.endswith("/") or "//" in path:
        return "must not end with '/' or contain '//'"
    if any(seg in (".", "..") for seg in body.split("/")):
        return "must not contain '.' or '..' segments"
    return None


TOP_KEYS = {
    "manifest_schema", "name", "version", "purpose", "daemon_class", "trigger", "reads",
    "commands", "output_dir", "network", "run_as", "resources", "cycle_budget_seconds",
    "blind_limit_seconds", "ledger", "digest",
}
# Schema 2 (contract 3.x) to schema 3 (contract 4.0.0): what changed, for the migration message.
SCHEMA_2 = "spark-daemon-manifest/2"
MIGRATION_2_TO_3 = {
    "deny": "removed in contract 4.0.0 (R-1): what a daemon reads is exactly its reads list, and no "
            "read may contain an always-denied path, so a deny list can no longer add anything. "
            "Delete the key; narrow reads if a path you meant to exclude lies inside one",
    "watchdog_seconds": "removed in contract 4.0.0 (R-6): set cycle_budget_seconds, the longest a "
                        "whole cycle may take; the watchdog is derived from it",
    "step_timeout_seconds": "removed in contract 4.0.0 (R-6): every ctx call now gets the time left "
                            "in the cycle's budget (cycle_budget_seconds)",
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
    network_mode: str
    run_as: RunAs
    resources: Resources
    cycle_budget_seconds: int
    blind_limit_seconds: int
    ledger: LedgerSpec
    digest: DigestSpec
    sha256: str          # sha256 of the canonical manifest, recorded in DAEMON_START
    path: str            # absolute path of manifest.json

    @property
    def code_path(self) -> str:
        return os.path.join(os.path.dirname(self.path), "daemon.py")

    @property
    def all_deny(self) -> tuple:
        """The always-denied paths. Since contract 4.0.0 a manifest adds none (R-1)."""
        return BASE_DENY

    @property
    def watchdog_seconds(self) -> int:
        return watchdog_seconds_for(self.cycle_budget_seconds)

    @property
    def stop_timeout_seconds(self) -> int:
        return stop_timeout_seconds_for(self.cycle_budget_seconds)

    @property
    def start_timeout_seconds(self) -> int:
        return start_timeout_seconds_for(self.cycle_budget_seconds)


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
        if not isinstance(value, str) or not pattern.fullmatch(value):
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
            if p is not None and path_refusal(p):
                self.fail(f"{where}[{i}]", path_refusal(p))
                p = None
            if p is not None:
                if p in out:
                    self.fail(f"{where}[{i}]", "duplicate path")
                out.append(p)
        return tuple(out)


def parse(data: dict, path: str = "<memory>") -> Manifest:
    c = _Checker()
    if isinstance(data, dict) and data.get("manifest_schema") == SCHEMA_2:
        # Contract 4.0.0 migration: say exactly what changed, field by field, instead of listing
        # unknown and missing keys.
        c.fail("manifest_schema", f"is {SCHEMA_2} (contract 3.x); contract 4.0.0 needs {MANIFEST_SCHEMA!r}")
        for key, why in MIGRATION_2_TO_3.items():
            if key in data:
                c.fail(key, why)
        if "cycle_budget_seconds" not in data:
            c.fail("cycle_budget_seconds", "required since contract 4.0.0: the longest a whole cycle may take")
        raise ManifestError(c.problems)
    if isinstance(data, dict):
        for key in sorted(set(MIGRATION_2_TO_3) & set(data)):
            c.fail(key, MIGRATION_2_TO_3[key])
        data = {k: v for k, v in data.items() if k not in MIGRATION_2_TO_3}
    if not c.keys("manifest", data, TOP_KEYS):
        raise ManifestError(c.problems)

    if data.get("manifest_schema") == "spark-daemon-manifest/1":
        c.fail("manifest_schema", f"is version 1; {MANIFEST_SCHEMA!r} adds the required blind_limit_seconds "
                                  "(contract 2.0.0): set it and change manifest_schema")
    elif data.get("manifest_schema") != MANIFEST_SCHEMA:
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
            if command_refusal(cmd):
                c.fail(f"commands[{i}]", command_refusal(cmd))
            elif cmd in seen:
                c.fail(f"commands[{i}]", "duplicate command")
            else:
                seen.append(cmd)
        commands = tuple(seen)

    output_dir = c.text("output_dir", data.get("output_dir"), PATH_RE,
                        "must be an absolute or ~/ path of letters, digits and ._/-")
    if output_dir is not None and path_refusal(output_dir):
        c.fail("output_dir", path_refusal(output_dir))
        output_dir = None

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

    cycle_budget = c.integer("cycle_budget_seconds", data.get("cycle_budget_seconds"),
                             CYCLE_BUDGET_MIN, CYCLE_BUDGET_MAX)
    # r3.5: how long the daemon may go without an accepted cycle (unsettled samples or failed
    # cycles) before it exits SENSE_BLIND. Required and bounded: no default, no infinity.
    blind_limit = c.integer("blind_limit_seconds", data.get("blind_limit_seconds"),
                            BLIND_LIMIT_MIN, BLIND_LIMIT_MAX)
    if blind_limit and trigger and blind_limit < BLIND_LIMIT_INTERVALS * trigger.interval_seconds:
        c.fail("blind_limit_seconds", f"must be at least {BLIND_LIMIT_INTERVALS} x trigger.interval_seconds")
    if blind_limit and cycle_budget and cycle_budget * 2 > blind_limit:
        c.fail("cycle_budget_seconds", "must be at most half of blind_limit_seconds (one slow cycle is never fatal)")

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

    # Relations between paths, checked on expanded paths. The output directory is the one
    # place a daemon may write, and every access right on it is granted to the process
    # (Landlock, ReadWritePaths=), so it must neither sit inside nor contain a protected tree.
    if output_dir:
        out = expand(output_dir)
        for root in FORBIDDEN_OUTPUT_ROOTS:
            full_root = expand(root)
            if within(out, full_root):
                c.fail("output_dir", f"must not be inside {root}")
            elif within(full_root, out):
                c.fail("output_dir", f"must not contain the protected path {root}")
        for p in reads:
            rp = expand(p)
            if within(out, rp) or within(rp, out):
                c.fail("output_dir", f"must not overlap the read path {p} (a daemon never observes its own output)")
    for i, p in enumerate(reads):
        rp = expand(p)
        for d in BASE_DENY:
            if within(rp, expand(d)):
                c.fail(f"reads[{i}]", f"{p} is inside the denied path {d}")
            elif within(expand(d), rp):
                # r4.11 (R-1, PD-100): no gaps. Landlock grants whole directories and cannot carve a
                # denied folder out of one, so a read containing a denied path was enforced only by
                # Python checks and the unit (F-1's race). Now the kernel's grant is the policy.
                c.fail(f"reads[{i}]", f"{p} contains the always-denied path {d}; declare narrower reads")

    if c.problems:
        raise ManifestError(c.problems)

    try:
        digest_sha = sha256_hex(canonical_bytes(data))
    except CanonicalError as e:
        raise ManifestError([f"manifest: {e}"]) from None
    return Manifest(
        name=name, version=version, purpose=purpose, daemon_class=daemon_class,
        trigger=trigger, reads=reads, commands=commands, output_dir=output_dir,
        network_mode=network_mode, run_as=run_as, resources=resources,
        cycle_budget_seconds=cycle_budget, blind_limit_seconds=blind_limit, ledger=ledger,
        digest=digest, sha256=digest_sha, path=os.path.abspath(path),
    )


def load(path: str) -> Manifest:
    try:
        data = strict_loads(_read_bounded(path))
    except (UnicodeDecodeError, ValueError) as e:
        raise ManifestError([f"manifest: not valid strict JSON ({type(e).__name__})"]) from None
    return parse(data, path)


def _read_bounded(path: str) -> bytes:
    try:
        with open(path, "rb") as fh:
            raw = fh.read(MANIFEST_MAX_BYTES + 1)
    except OSError as e:
        raise ManifestError([f"manifest: cannot read {path}: {e.strerror}"]) from None
    if len(raw) > MANIFEST_MAX_BYTES:
        raise ManifestError(["manifest: larger than 64 KiB"])
    return raw


def to_dict(path: str) -> dict:
    """The raw manifest object (strict JSON, bounded), without schema validation."""
    try:
        data = strict_loads(_read_bounded(path))
    except (UnicodeDecodeError, ValueError) as e:
        raise ManifestError([f"manifest: not valid strict JSON ({type(e).__name__})"]) from None
    if not isinstance(data, dict):
        raise ManifestError(["manifest: must be an object"])
    return data


def dump(data: dict, path: str) -> None:
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(data, fh, indent=2, ensure_ascii=False)
        fh.write("\n")
