"""Generate a sandboxed systemd unit and a human install plan from a manifest.

The generator never installs, enables or starts anything. Its output is text for review.
Default is a system unit with a dedicated user: Observer v0.3 section 4 notes that Ubuntu
24.04 restricts unprivileged user namespaces, so many sandbox directives only take effect
reliably in system units. User units are generated with the same directives and a warning;
verify them with `systemd-analyze --user security` before relying on them.
"""

import os
import re

from . import EXIT_ALREADY_RUNNING, EXIT_LEDGER_CORRUPT, EXIT_POLICY, EXIT_USAGE, VERSION
from .paths import expand, home

# The three Landlock syscalls sit in systemd's @sandbox group, which @system-service does
# NOT include (checked with `systemd-analyze syscall-filter` on systemd 255). Without this
# line every one of them fails with EPERM under the unit, landlock.abi_version() reports it
# as unavailable, and runtime.py refuses to start (exit 78, PD-15): the daemon could never
# run as installed. Found in r3; the r2 battery could not catch it because it never runs
# the daemon under the unit's seccomp filter.
LANDLOCK_SYSCALLS = "landlock_create_ruleset landlock_add_rule landlock_restrict_self"
# Characters a path may carry into a unit file unquoted: no spaces (argument splitting),
# no "%" (specifiers), no quotes, backslashes, control characters or ";" (ExecStart syntax).
_UNIT_SAFE_PATH = re.compile(r"^/[A-Za-z0-9._/+@-]*$")
_UNIT_NAME = re.compile(r"^[A-Za-z0-9._@-]+\.(service|target)$")


class UnitError(ValueError):
    """A path cannot be written into a unit file safely."""


def _unit_path(label, path) -> str:
    if not _UNIT_SAFE_PATH.fullmatch(path):
        raise UnitError(f"{label} {path!r} contains characters that are unsafe in a systemd unit")
    return path

# r3.5: exits that mean "a human must look" are never restarted: bad manifest or code (2),
# corrupt ledger (65), already running (73), and every fail-closed 78 (policy, unsafe output
# directory, Landlock refused, SENSE_BLIND). Before this, Restart=on-failure restarted every
# 78. The start limit below stopped fast exits after five restarts, but SENSE_BLIND comes only
# after blind_limit_seconds, so for any realistic limit its restarts are too far apart for five
# to fit in the 300 s window: a blind daemon would have restarted forever (HF-28). 70 (uncertain commit)
# stays restartable; the start limit bounds a persistent fault, and the next start's
# recovery decides from disk.
NO_RESTART_EXIT_CODES = (EXIT_USAGE, EXIT_LEDGER_CORRUPT, EXIT_ALREADY_RUNNING, EXIT_POLICY)
RESTART_PREVENT = "RestartPreventExitStatus=" + " ".join(str(c) for c in NO_RESTART_EXIT_CODES)

def stop_timeout_seconds(m) -> int:
    """r3.9 (HF-31): SIGTERM is honoured between cycles, and a cycle may legally run until the
    watchdog would fire. A fixed 30 s turned a stop during a slow cycle (git-watch: up to six Git
    calls of 20 s) into a SIGKILL, recorded as an unclean end. Outlast one watchdog period. Since
    r4.11 (R-6) derived from cycle_budget_seconds by manifest.stop_timeout_seconds_for."""
    return m.stop_timeout_seconds


def timing_problems(unit_text: str, m) -> list:
    """The unit's timings against the ones derived from the manifest's cycle budget (R-6): a
    unit whose watchdog, start or stop timeout says anything else contradicts the budget."""
    expected = {"WatchdogSec": m.watchdog_seconds, "TimeoutStartSec": m.start_timeout_seconds,
                "TimeoutStopSec": m.stop_timeout_seconds}
    found = {}
    for line in unit_text.splitlines():
        key, _, value = line.strip().partition("=")
        if key in expected:
            found.setdefault(key, []).append(value)
    return [f"{key}={','.join(found.get(key, ['missing']))}, but the cycle budget gives {value}"
            for key, value in expected.items() if found.get(key) != [str(value)]]


REQUIRED_DIRECTIVES = (
    "Type=notify", "NotifyAccess=main", "WatchdogSec=", "Restart=on-failure", RESTART_PREVENT,
    "UMask=0077",
    "NoNewPrivileges=yes", "ProtectSystem=strict", "ProtectHome=read-only", "ReadWritePaths=",
    "InaccessiblePaths=", "PrivateTmp=yes", "PrivateDevices=yes", "PrivateNetwork=yes",
    "IPAddressDeny=any", "RestrictAddressFamilies=AF_UNIX", "CapabilityBoundingSet=",
    "SystemCallFilter=@system-service", f"SystemCallFilter={LANDLOCK_SYSCALLS}",
    "MemoryDenyWriteExecute=yes", "RestrictNamespaces=yes",
    "LockPersonality=yes", "RestrictSUIDSGID=yes", "ProtectKernelTunables=yes",
    "ProtectKernelModules=yes", "ProtectControlGroups=yes", "CPUWeight=", "MemoryMax=",
    "TasksMax=",
)


def unit_name(m) -> str:
    return f"spark-daemon-{m.name}.service"


def _unit_ref(name) -> str:
    if not _UNIT_NAME.fullmatch(name):
        raise UnitError(f"unit name {name!r} is not a plain .service or .target name")
    return name


_HEX64 = re.compile(r"^[0-9a-f]{64}$")


def generate(m, *, root: str, python: str = "/usr/bin/python3", require_paths=(), part_of=None,
             expect=None, daemon_home=None) -> str:
    """The unit text, a pure function of its arguments and the file system's symlinks.
    daemon_home is the SPARK_DAEMON_HOME the unit sets and every "~" path expands to; it
    defaults to this process's home() (a preview), and a qualified unit passes the one its
    report bound (HF-41). With `expect` (the manifest, daemon.py and contract digests a
    qualifying battery PASS bound) it is installable: the runtime refuses to start unless the
    files still match. Without it, it is a preview, which the runtime refuses to start outside
    test mode. Installable units come only from `unit --report` (contract 5, E-11)."""
    base = home() if daemon_home is None else os.path.realpath(daemon_home)
    out = _unit_path("output_dir", expand(m.output_dir, base))
    reads = [_unit_path("read path", expand(p, base)) for p in m.reads]
    ro = [p for p in reads if not (p.startswith("/proc/") or p.startswith("/sys/"))]
    deny = [_unit_path("denied path", expand(p, base)) for p in m.all_deny]
    manifest_path = _unit_path("manifest path", os.path.abspath(m.path))
    entry = _unit_path("pattern root", os.path.join(os.path.abspath(root), "bin", "spark-daemon"))
    python = _unit_path("python", python)
    daemon_home = _unit_path("SPARK_DAEMON_HOME", base)
    # Paths that must exist for the unit to start at all (systemd skips it, without a failure,
    # when one is missing): for example a removable or encrypted drive's marker, so a locked
    # drive does not turn into a fail-closed stop that nothing restarts.
    conditions = [f"ConditionPathExists={_unit_path('required path', p)}" for p in require_paths]
    # A daemon whose output lives on storage another unit mounts or owns starts and stops with
    # that unit: PartOf= stops it first (its open ledger would otherwise keep the drive busy),
    # and WantedBy= that unit means it starts with it rather than at boot.
    system = m.run_as.unit == "system"
    bound = []
    wanted_by = "multi-user.target" if system else "default.target"
    if part_of:
        part_of = _unit_ref(part_of)
        bound = [f"PartOf={part_of}", f"After={part_of}"]
        wanted_by = part_of
    exec_start = f"ExecStart={python} -I -B {entry} run --manifest {manifest_path}"
    if expect is not None:
        from .qualify import EXPECT_KEYS, expect_args
        if set(expect) != set(EXPECT_KEYS) or not all(_HEX64.fullmatch(str(v)) for v in expect.values()):
            raise UnitError("expect must carry exactly the manifest, code and contract sha256")
        exec_start += " " + " ".join(expect_args(expect))
        banner = ["# QUALIFIED by a battery PASS: projected from its report by `spark-daemon unit --report`.",
                  "# Any edit to it, or to the files it names, stops it from starting."]
    else:
        banner = ["# PREVIEW, NOT QUALIFIED: the runtime refuses to start it outside test mode.",
                  "# An installable unit comes only from `spark-daemon unit --report <battery report>`."]
    lines = [
        f"# Generated by spark-daemon-pattern {VERSION} from {manifest_path}",
        f"# manifest sha256 {m.sha256}",
        *banner,
        "# Do not edit by hand: change the manifest and regenerate. Review before installing.",
    ]
    if not system:
        lines.append("# WARNING: user unit. On Ubuntu 24.04 many sandbox directives need user namespaces;"
                     " verify with systemd-analyze --user security before relying on them.")
    lines += [
        "",
        "[Unit]",
        f"Description=Spark daemon {m.name}: {m.purpose.replace('%', '%%')}",
        *bound,
        *conditions,
        "StartLimitIntervalSec=300",
        "StartLimitBurst=5",
        "",
        "[Service]",
        "Type=notify",
        "NotifyAccess=main",
        exec_start,
    ]
    if system:
        lines.append(f"User={m.run_as.user}")
        lines.append(f"Group={m.run_as.user}")
    lines += [
        f"Environment=SPARK_DAEMON_HOME={daemon_home}",
        f"WatchdogSec={m.watchdog_seconds}",
        f"TimeoutStartSec={m.start_timeout_seconds}",
        f"TimeoutStopSec={stop_timeout_seconds(m)}",
        "Restart=on-failure",
        RESTART_PREVENT,
        "RestartSec=10",
        "UMask=0077",
        "",
        "# Resources",
        f"CPUWeight={m.resources.cpu_weight}",
        f"MemoryMax={m.resources.memory_max_mb}M",
        f"TasksMax={m.resources.tasks_max}",
        f"IOSchedulingClass={m.resources.io_class}",
        "Nice=10",
        "",
        "# Filesystem: everything read-only except the output directory; denied paths invisible",
        "ProtectSystem=strict",
        "ProtectHome=read-only",
        f"ReadWritePaths={out}",
    ]
    if ro:
        lines.append("ReadOnlyPaths=" + " ".join(f"-{p}" for p in ro))
    lines += [
        "InaccessiblePaths=" + " ".join(f"-{p}" for p in deny),
        "PrivateTmp=yes",
        "PrivateDevices=yes",
        "ProtectProc=invisible",
        "",
        "# Network: none (manifest network.mode = none)",
        "PrivateNetwork=yes",
        "IPAddressDeny=any",
        "RestrictAddressFamilies=AF_UNIX",
        "",
        "# Privileges and kernel",
        "NoNewPrivileges=yes",
        "CapabilityBoundingSet=",
        "AmbientCapabilities=",
        "ProtectKernelTunables=yes",
        "ProtectKernelModules=yes",
        "ProtectKernelLogs=yes",
        "ProtectControlGroups=yes",
        "ProtectClock=yes",
        "ProtectHostname=yes",
        "RestrictNamespaces=yes",
        "RestrictRealtime=yes",
        "RestrictSUIDSGID=yes",
        "LockPersonality=yes",
        "MemoryDenyWriteExecute=yes",
        "RemoveIPC=yes",
        "KeyringMode=private",
        "SystemCallArchitectures=native",
        "SystemCallFilter=@system-service",
        "# Landlock (runtime.py applies it at start-up); @system-service does not include @sandbox",
        f"SystemCallFilter={LANDLOCK_SYSCALLS}",
        "SystemCallFilter=~@privileged @resources",
        "SystemCallErrorNumber=EPERM",
        "",
        "[Install]",
        f"WantedBy={wanted_by}",
        "",
    ]
    return "\n".join(lines)


# Syscalls the runtime makes during start-up that a seccomp filter could silently break.
STARTUP_SYSCALLS = ("landlock_create_ruleset", "landlock_add_rule", "landlock_restrict_self",
                    "prctl", "flock", "fsync")


def resolve_syscall_filter(unit_text: str, expand_group) -> tuple:
    """(allowed, denied) syscall sets from the unit's SystemCallFilter= lines, with groups
    expanded by expand_group("@name") -> iterable of syscall names (for example from
    `systemd-analyze syscall-filter`). Positive lines are merged (a union); lines starting
    with "~" are subtracted."""
    allowed, denied = set(), set()
    for line in unit_text.splitlines():
        line = line.strip()
        if not line.startswith("SystemCallFilter="):
            continue
        value = line.split("=", 1)[1]
        target = allowed
        if value.startswith("~"):
            target, value = denied, value[1:]
        for item in value.split():
            if item.startswith("@"):
                target.update(expand_group(item))
            else:
                target.add(item)
    return allowed, denied


def lint(unit_text: str) -> list:
    """Directives every generated unit must carry. Returns the missing ones."""
    present = [line.strip() for line in unit_text.splitlines() if not line.lstrip().startswith("#")]
    return [d for d in REQUIRED_DIRECTIVES if not any(line.startswith(d) for line in present)]


def install_plan(m, *, root: str, python: str = "/usr/bin/python3", require_paths=(), part_of=None) -> str:
    """Text for a person. Nothing here is run. The unit is a projection of a qualifying battery
    report (`unit --report`, contract 5): it is reproduced from the report, on the host it was
    made on, and the runtime checks the files it names at every start."""
    name = unit_name(m)
    out = expand(m.output_dir)
    reads = [expand(p) for p in m.reads]
    entry = os.path.join(root, "bin", "spark-daemon")
    options = ("".join(f" --require-path {p}" for p in require_paths)
               + (f" --part-of {_unit_ref(part_of)}" if part_of else "")
               + (f" --python {python}" if python != "/usr/bin/python3" else ""))
    lines = [
        f"# Install plan for {name}. Nothing below has been run.",
        "# A battery PASS qualifies the unit; it does not activate it. Installing and enabling it is a",
        "# separate decision by a person with that authority (Class C).",
        f"# Generated by spark-daemon-pattern {VERSION}; manifest sha256 {m.sha256}.",
        "",
        "# 0. Qualify: the full battery (not --quick) must print RESULT: PASS and \"qualifies: yes\".",
        "#    It writes its report into the workspace folder given:",
        f"python3 -I -B {entry} battery --manifest {os.path.abspath(m.path)}{options} --workdir ./battery",
        "# 1. Project the installable unit from the report (refused unless it qualifies and was made on",
        "#    this host; the runtime refuses to start it if the files changed since):",
        f"python3 -I -B {entry} unit --report ./battery/battery-report.json --out ./qualified",
        "",
    ]
    if m.run_as.unit == "system":
        user = m.run_as.user
        lines += [
            "# The service user must be able to read the pattern and this daemon's folder, and reach the",
            "# output directory. Installing the pattern and daemon folders root-owned and read-only (for",
            "# example under /opt) is simplest; an output directory under a home directory also needs",
            "# traverse (x) permission on each parent. Decide these per path.",
            f"sudo useradd --system --no-create-home --shell /usr/sbin/nologin {user}",
            f"sudo install -d -m 0700 -o {user} -g {user} {out}",
            "# Read access: the service user must be able to read each path below. Decide per path;",
            "# never grant anything under ~/spark-core/data or ~/spark-governance/history.",
        ]
        lines += [f"#   {p}" for p in reads]
        lines += [
            f"sudo install -m 0644 ./qualified/{name} /etc/systemd/system/{name}",
            f"sudo systemd-analyze security --threshold=20 {name}",
            "sudo systemctl daemon-reload",
            f"sudo systemctl enable --now {name}",
            *([f"# from now on it starts and stops with {part_of}"] if part_of else []),
            f"systemctl status {name}    # READY and the last watchdog ping",
        ]
    else:
        lines += [
            "loginctl enable-linger \"$USER\"    # so the unit survives logout (Observer v0.3 section 3.4)",
            f"install -m 0644 ./qualified/{name} ~/.config/systemd/user/{name}",
            f"systemd-analyze --user security --threshold=20 {name}",
            "systemctl --user daemon-reload",
            f"systemctl --user enable --now {name}",
        ]
    return "\n".join(lines)
