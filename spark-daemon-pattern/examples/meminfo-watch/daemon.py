"""meminfo-watch: the reference daemon for the Spark daemon pattern.

It records when unified-memory headroom moves between bands. On the DGX Spark's GB10 the
CPU and GPU share one memory pool, so MemAvailable and SwapFree from /proc/meminfo are the
figures NVIDIA recommends (observability handoff section 13.2), never "total minus CUDA free".

The band thresholds below are placeholders: health-rule thresholds are a Class C decision.

This file follows the pattern's rules for daemon code: no I/O modules, all I/O through the
ctx object in sense(), decide() and digest() pure, no floats (integers with a stated unit).
"""

import re

# Lower bound of each band, in permille of MemTotal available. Placeholders.
BANDS = (("ok", 200), ("tight", 100), ("critical", 0))

_FIELD = re.compile(r"^(\w+):\s+(\d+) kB$")
_PSI_SOME = re.compile(r"^some avg10=(\d+)\.(\d\d) ")


def sense(ctx):
    """The only function that performs I/O, and only through ctx."""
    snapshot = {"meminfo": ctx.read_text("/proc/meminfo", max_bytes=16384), "psi": None}
    try:
        snapshot["psi"] = ctx.read_text("/proc/pressure/memory", max_bytes=4096)
    except ctx.Missing:
        pass
    return snapshot


def _fields(text):
    values = {}
    for line in text.splitlines():
        match = _FIELD.match(line)
        if match:
            values[match.group(1)] = int(match.group(2))
    return values


def _psi_some_avg10_x100(text):
    if text is None:
        return None
    for line in text.splitlines():
        match = _PSI_SOME.match(line)
        if match:
            return int(match.group(1)) * 100 + int(match.group(2))
    return None


def _band(permille):
    for name, floor in BANDS:
        if permille >= floor:
            return name
    return "critical"


def _summary(snapshot):
    v = _fields(snapshot["meminfo"])
    total = v.get("MemTotal", 0)
    available = v.get("MemAvailable", 0)
    permille = available * 1000 // total if total else 0
    return {
        "band": _band(permille),
        "mem_total_kb": total,
        "mem_available_kb": available,
        "available_permille": permille,
        "swap_total_kb": v.get("SwapTotal", 0),
        "swap_free_kb": v.get("SwapFree", 0),
        "psi_some_avg10_x100": _psi_some_avg10_x100(snapshot["psi"]),
    }


def decide(prev, snapshot):
    """Pure: the previous snapshot (None after a start) and the new one in, events out."""
    now = _summary(snapshot)
    if prev is None:
        return [("MEMORY_OBSERVATION_BASELINE", now)]
    before = _summary(prev)["band"]
    if before != now["band"]:
        return [("MEMORY_BAND_CHANGED", dict(now, previous_band=before))]
    return []


def digest(snapshot, recent):
    """Pure: returns sections of (label, value) rows; the skeleton renders and contains them."""
    s = _summary(snapshot)
    rows = [
        ("Band", s["band"]),
        ("Available", f"{s['mem_available_kb'] // 1024} MiB of {s['mem_total_kb'] // 1024} MiB"),
        ("Available (permille)", s["available_permille"]),
        ("Swap free", f"{s['swap_free_kb'] // 1024} MiB of {s['swap_total_kb'] // 1024} MiB"),
        ("Memory pressure, some avg10 x100", s["psi_some_avg10_x100"]),
    ]
    changes = [r for r in recent if r["event_type"] == "MEMORY_BAND_CHANGED"][-5:]
    change_rows = [
        (f"seq {r['seq']}", f"{r['payload']['previous_band']} to {r['payload']['band']} at {r['timestamp_utc']}")
        for r in changes
    ] or [("Band changes", "none recorded since this run started")]
    return [("Unified memory now", rows), ("Recent band changes", change_rows)]
