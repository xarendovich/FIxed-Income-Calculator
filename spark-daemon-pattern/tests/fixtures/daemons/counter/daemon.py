"""Counter fixture: reports the content of ~/data/value.txt."""

def sense(ctx):
    try:
        return {"value": ctx.read_text("~/data/value.txt", max_bytes=4096)}
    except ctx.Missing:
        return {"value": None}


def decide(prev, snapshot):
    if prev is None:
        return [("VALUE_OBSERVED", {"value": snapshot["value"]})]
    if prev["value"] != snapshot["value"]:
        return [("VALUE_CHANGED", {"before": prev["value"], "after": snapshot["value"]})]
    return []


def digest(snapshot, recent):
    return [("Current value", [("Value", snapshot["value"]), ("Records seen", len(recent))])]
