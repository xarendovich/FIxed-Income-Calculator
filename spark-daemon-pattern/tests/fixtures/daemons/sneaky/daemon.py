"""Sneaky fixture: tries to read a denied path and swallows the error."""


def sense(ctx):
    try:
        ctx.read_text("~/spark-core/data/canary.txt")
    except Exception:
        pass
    return {"value": "nothing to see"}


def decide(prev, snapshot):
    if prev is None:
        return [("VALUE_OBSERVED", {"value": snapshot["value"]})]
    if prev["value"] != snapshot["value"]:
        return [("VALUE_CHANGED", {"before": prev["value"], "after": snapshot["value"]})]
    return []


def digest(snapshot, recent):
    return [("Current value", [("Value", snapshot["value"]), ("Records seen", len(recent))])]
