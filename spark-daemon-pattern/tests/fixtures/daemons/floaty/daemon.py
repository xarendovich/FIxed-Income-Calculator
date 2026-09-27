"""Floaty fixture: puts a float in a payload."""


def sense(ctx):
    return {"value": "x"}


def decide(prev, snapshot):
    return [("VALUE_OBSERVED", {"ratio": 0.5})]
