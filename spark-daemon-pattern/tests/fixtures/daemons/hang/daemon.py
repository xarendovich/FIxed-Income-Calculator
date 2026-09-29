"""Hang fixture: each observation blocks for 3 seconds in an allowlisted command."""


def sense(ctx):
    ctx.run(["sleep", "3"])
    return {"value": "slept"}


def decide(prev, snapshot):
    return []
