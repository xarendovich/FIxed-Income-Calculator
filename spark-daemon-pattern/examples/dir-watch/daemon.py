"""dir-watch: reference daemon for the "inventory diff" idiom.

It records entries appearing, disappearing or changing (size or mtime) in ~/spark-inbox.
It never reads file contents: list_dir and stat only. The same shape fits a log directory,
a drop folder or a model cache: snapshot an inventory, diff it against the previous one,
and emit bounded, batched events.

Two things every inventory daemon must handle, shown here:
- names are untrusted text chosen by whoever can write to the folder: they are truncated
  and only ever reach the digest as values (the skeleton contains and escapes them);
- inventories are unbounded: the snapshot is capped (MAX_ENTRIES) and every event lists
  at most MAX_NAMES_PER_EVENT names plus an exact count, so a folder of 100,000 files
  cannot exceed the ledger's record size.
ctx.stat does not follow symlinks and judges a link by where it sits, so a link planted in
the folder is recorded as kind "link", never followed.
"""

WATCHED = "~/spark-inbox"
MAX_ENTRIES = 512
MAX_NAMES_PER_EVENT = 20
NAME_CHARS = 128


def sense(ctx):
    """The only function with I/O: one listing, then one lstat per entry."""
    try:
        listing = ctx.list_dir(WATCHED, max_entries=MAX_ENTRIES)
    except ctx.Missing:
        return {"present": False, "entries": {}, "truncated": False}
    entries = {}
    for name in listing.names:
        try:
            info = ctx.stat(WATCHED + "/" + name)
        except ctx.Missing:
            continue                                   # removed between listing and stat
        entries[name[:NAME_CHARS]] = [info.kind, info.size, info.mtime_us]
    return {"present": True, "entries": entries, "truncated": listing.truncated}


def _batch(names):
    ordered = sorted(names)
    return {"count": len(ordered), "names": ordered[:MAX_NAMES_PER_EVENT],
            "names_omitted": max(0, len(ordered) - MAX_NAMES_PER_EVENT)}


def decide(prev, snapshot):
    """Pure: diff two inventories into at most four bounded events."""
    now = snapshot["entries"]
    if prev is None:
        return [("INBOX_BASELINE", {"present": snapshot["present"], "count": len(now),
                                    "truncated": snapshot["truncated"]})]
    if prev["present"] != snapshot["present"]:
        return [("INBOX_AVAILABILITY_CHANGED", {"present": snapshot["present"], "count": len(now)})]
    before = prev["entries"]
    added = [n for n in now if n not in before]
    removed = [n for n in before if n not in now]
    changed = [n for n in now if n in before and now[n] != before[n]]
    events = []
    if added:
        events.append(("INBOX_ENTRIES_ADDED", _batch(added)))
    if removed:
        events.append(("INBOX_ENTRIES_REMOVED", _batch(removed)))
    if changed:
        events.append(("INBOX_ENTRIES_CHANGED", _batch(changed)))
    return events


def digest(snapshot, recent):
    entries = snapshot["entries"]
    kinds = {}
    for kind, _size, _mtime in entries.values():
        kinds[kind] = kinds.get(kind, 0) + 1
    total_bytes = sum(size for kind, size, _mtime in entries.values() if kind == "file")
    rows = [("Folder", WATCHED),
            ("Present", snapshot["present"]),
            ("Entries", len(entries)),
            ("Listing truncated", snapshot["truncated"]),
            ("Regular files, total bytes", total_bytes)]
    rows += [(f"Kind {kind}", count) for kind, count in sorted(kinds.items())]
    newest = sorted(entries.items(), key=lambda item: item[1][2], reverse=True)[:10]
    newest_rows = [(f"{i + 1}", name) for i, (name, _info) in enumerate(newest)]
    return [("Inbox now", rows),
            ("Most recently modified", newest_rows or [("Entries", "none")])]
