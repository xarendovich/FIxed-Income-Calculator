"""Never-seeing fixture (r4.5, HF-35): every cycle is unsettled, so it records no error and no
event. Before DB-20 the battery passed it."""


def sense(ctx):
    return ctx.unsettled("NEVER_SETTLES")


def decide(prev, snapshot):
    return []
