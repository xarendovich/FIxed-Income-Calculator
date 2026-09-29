"""disk-watch: reference daemon for the "threshold bands over a numeric reading" idiom.

It records when available space on the filesystem holding ~/models (model weights on the
DGX Spark) moves between headroom bands, and when the path appears or disappears (an
unmounted volume). The same shape fits any gauge: read a number through ctx, map it to a
small set of named bands, and emit an event only when the band changes, so a steady
system writes nothing.

Band floors are placeholders in permille of total size; thresholds are a Class C decision.
"""

WATCHED = "~/models"
# Lower bound of each band, in permille of the filesystem's size available. Placeholders.
BANDS = (("ok", 150), ("low", 50), ("critical", 0))
MIB = 1024 * 1024


def sense(ctx):
    """The only function with I/O: one statvfs through ctx."""
    try:
        usage = ctx.disk_usage(WATCHED)
    except ctx.Missing:
        return {"present": False}
    return {"present": True, "total_bytes": usage.total_bytes, "available_bytes": usage.available_bytes}


def _band(snapshot):
    if not snapshot["present"] or not snapshot["total_bytes"]:
        return None
    permille = snapshot["available_bytes"] * 1000 // snapshot["total_bytes"]
    for name, floor in BANDS:
        if permille >= floor:
            return name
    return "critical"


def _summary(snapshot):
    if not snapshot["present"]:
        return {"present": False, "band": None}
    total = snapshot["total_bytes"]
    return {
        "present": True,
        "band": _band(snapshot),
        "total_mib": total // MIB,
        "available_mib": snapshot["available_bytes"] // MIB,
        "available_permille": snapshot["available_bytes"] * 1000 // total if total else 0,
    }


def decide(prev, snapshot):
    """Pure: an event only when presence or band changes."""
    now = _summary(snapshot)
    if prev is None:
        return [("DISK_OBSERVATION_BASELINE", now)]
    before = _summary(prev)
    if before["present"] != now["present"]:
        return [("DISK_PATH_AVAILABILITY_CHANGED", dict(now, previously_present=before["present"]))]
    if before["band"] != now["band"]:
        return [("DISK_BAND_CHANGED", dict(now, previous_band=before["band"]))]
    return []


def digest(snapshot, recent):
    s = _summary(snapshot)
    if not s["present"]:
        rows = [("Path", WATCHED), ("State", "not present (volume not mounted?)")]
    else:
        rows = [
            ("Path", WATCHED),
            ("Band", s["band"]),
            ("Available", f"{s['available_mib']} MiB of {s['total_mib']} MiB"),
            ("Available (permille)", s["available_permille"]),
        ]
    changes = [r for r in recent if r["event_type"] != "DISK_OBSERVATION_BASELINE"
               and r["event_type"].startswith("DISK_")][-5:]
    change_rows = [(f"seq {r['seq']}", f"{r['event_type']} at {r['timestamp_utc']}") for r in changes]
    return [("Model-weights filesystem", rows),
            ("Recent changes", change_rows or [("Changes", "none recorded since this run started")])]
