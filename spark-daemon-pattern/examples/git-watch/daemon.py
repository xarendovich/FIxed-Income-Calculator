"""git-watch: reference daemon for the "observe a tool's output" idiom, through ctx.git.

It records branch and tag movements, HEAD switches, and (for a repository with a worktree)
changes in how many paths are dirty. Each observation is a read-only Git subcommand run by
the skeleton: no pager, no colour, no external diff or textconv, no fsmonitor, no global or
system config, a private copy of the index for `status` (script-board F4), and
safe.directory scoped to exactly this repository, since the daemon runs as its own user.

Things any command-driven daemon must handle, shown here:
- a command can fail, time out or overflow its cap: a failing Git is an observation (the
  repository is unavailable), but a timeout or an overflow is not, so it fails the cycle;
- output is untrusted text: parse it strictly, cap it, and never copy raw lines into events;
- one noisy repository must not flood the ledger: ref changes are batched into a single
  bounded event per cycle;
- a sample taken while the repository is changing is not an observation: refs and the dirty
  count are read twice, and if they differ the cycle returns ctx.unsettled("REPO_CHANGING")
  instead of recording a half-way state. During a long build or rebase the daemon stays
  patient up to the manifest's blind_limit_seconds, then stops for a human (r3.5).
"""

REPO = "~/repos/watched"
MAX_REFS = 256
MAX_REFS_PER_EVENT = 20
REF_CHARS = 200


def _git(ctx, args, index_copy=False):
    result = ctx.git(REPO, args, max_bytes=262144, index_copy=index_copy)
    # Output over the cap, or a timeout, is not an observation of the repository: it is a
    # failed cycle (a typed DAEMON_ERROR) that counts towards blind_limit_seconds. r3.5: it
    # used to read as "unavailable" or "dirty: null", accepted every cycle, which hid a
    # capacity problem from both the ledger and the limit.
    if result.truncated:
        raise ctx.TooLarge("git output over the cap")
    if result.timed_out:
        raise TimeoutError("git timed out")
    if result.returncode != 0:
        return None
    return result.stdout


def _parse_refs(refs_text):
    refs = {}
    for line in refs_text.splitlines()[:MAX_REFS]:
        sha, _, name = line.partition(" ")
        if len(sha) in (40, 64) and name:
            refs[name[:REF_CHARS]] = sha
    return refs


def _dirty_count(status):
    return len([p for p in status.split("\0") if p])


def _moving(ctx, snapshot):
    """Re-read what changes fastest; True if it moved since the first read."""
    refs_text = _git(ctx, ["for-each-ref", "--format=%(objectname) %(refname)", "refs/heads", "refs/tags"])
    if refs_text is None or _parse_refs(refs_text) != snapshot["refs"]:
        return True
    if snapshot["dirty"] is not None:
        status = _git(ctx, ["status", "--porcelain=v1", "-z"], index_copy=True)
        if status is None or _dirty_count(status) != snapshot["dirty"]:
            return True
    return False


def sense(ctx):
    """The only function with I/O: read-only Git calls, the fast-changing ones twice."""
    try:
        bare = _git(ctx, ["rev-parse", "--is-bare-repository"])
    except ctx.Missing:
        return {"available": False}
    if bare is None:
        return {"available": False}
    refs_text = _git(ctx, ["for-each-ref", "--format=%(objectname) %(refname)", "refs/heads", "refs/tags"])
    head = _git(ctx, ["rev-parse", "--abbrev-ref", "HEAD"])
    if refs_text is None or head is None:
        return {"available": False}
    refs = _parse_refs(refs_text)
    snapshot = {"available": True, "bare": bare.strip() == "true", "head": head.strip()[:REF_CHARS],
                "refs": refs, "refs_truncated": len(refs_text.splitlines()) > MAX_REFS, "dirty": None}
    if not snapshot["bare"]:
        status = _git(ctx, ["status", "--porcelain=v1", "-z"], index_copy=True)
        if status is not None:
            snapshot["dirty"] = _dirty_count(status)
    if _moving(ctx, snapshot):
        return ctx.unsettled("REPO_CHANGING")
    return snapshot


def _bounded(names):
    ordered = sorted(names)
    return ordered[:MAX_REFS_PER_EVENT], max(0, len(ordered) - MAX_REFS_PER_EVENT)


def decide(prev, snapshot):
    """Pure: compare two snapshots; at most one event of each kind per cycle."""
    if prev is None:
        if not snapshot["available"]:
            return [("REPO_BASELINE", {"available": False})]
        return [("REPO_BASELINE", {"available": True, "bare": snapshot["bare"], "head": snapshot["head"],
                                   "ref_count": len(snapshot["refs"]), "dirty": snapshot["dirty"]})]
    if prev["available"] != snapshot["available"]:
        return [("REPO_AVAILABILITY_CHANGED", {"available": snapshot["available"]})]
    if not snapshot["available"]:
        return []
    events = []
    before, now = prev["refs"], snapshot["refs"]
    created = [r for r in now if r not in before]
    deleted = [r for r in before if r not in now]
    moved = [r for r in now if r in before and now[r] != before[r]]
    if created or deleted or moved:
        payload = {"created": len(created), "deleted": len(deleted), "moved": len(moved)}
        for label, names in (("created_refs", created), ("deleted_refs", deleted), ("moved_refs", moved)):
            shown, omitted = _bounded(names)
            payload[label] = shown
            payload[label + "_omitted"] = omitted
        payload["moved_to"] = {r: now[r] for r in payload["moved_refs"]}
        events.append(("REFS_CHANGED", payload))
    if prev["head"] != snapshot["head"]:
        events.append(("HEAD_SWITCHED", {"before": prev["head"], "after": snapshot["head"]}))
    if prev["dirty"] != snapshot["dirty"]:
        events.append(("WORKTREE_STATE_CHANGED", {"dirty_before": prev["dirty"], "dirty_after": snapshot["dirty"]}))
    return events


def digest(snapshot, recent):
    if not snapshot["available"]:
        return [("Repository", [("Path", REPO), ("State", "unavailable (missing, not a repository, or Git failed)")])]
    rows = [("Path", REPO), ("Bare", snapshot["bare"]), ("HEAD", snapshot["head"]),
            ("Refs", len(snapshot["refs"])), ("Dirty paths", snapshot["dirty"])]
    branches = sorted(r for r in snapshot["refs"] if r.startswith("refs/heads/"))[:10]
    branch_rows = [(f"{i + 1}", f"{name} {snapshot['refs'][name][:12]}") for i, name in enumerate(branches)]
    changes = [r for r in recent if r["event_type"] in ("REFS_CHANGED", "HEAD_SWITCHED")][-5:]
    change_rows = [(f"seq {r['seq']}", f"{r['event_type']} at {r['timestamp_utc']}") for r in changes]
    return [("Repository now", rows),
            ("Branches", branch_rows or [("Branches", "none")]),
            ("Recent changes", change_rows or [("Changes", "none recorded since this run started")])]
