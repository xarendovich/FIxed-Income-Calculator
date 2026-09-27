"""Opener fixture: breaks the purity rules by calling open() directly."""


def sense(ctx):
    with open("/etc/hostname") as fh:
        return {"value": fh.read()}


def decide(prev, snapshot):
    return []
