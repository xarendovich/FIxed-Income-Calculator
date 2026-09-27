"""Undeclared fixture: emits an event type its manifest does not declare."""


def sense(ctx):
    return {"value": "x"}


def decide(prev, snapshot):
    return [("NOT_DECLARED", {"value": 1})]
