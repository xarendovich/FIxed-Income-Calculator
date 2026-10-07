"""Hang fixture: each observation blocks opening a FIFO that has no writer, until the cycle's
alarm ends it (contract 5: a daemon runs no program, so the block is a read)."""


def sense(ctx):
    ctx.read_text("~/data/fifo")
    return {"value": "unblocked"}


def decide(prev, snapshot):
    return []
