#!/usr/bin/env python3
"""A vendored copy of the daemon pattern under other names, bound by three hashes (r4.11, R-8b).

A project that copies the pattern may have to rename it (one forbids the word "spark" in its
paths). The core keeps its own names; a copy is made by a deterministic rename map, and the copy
is bound by:

    source_tree_sha256        the files as they are in this repository
    rename_map_sha256         the map, canonically encoded
    transformed_tree_sha256   the files after the map is applied

so "this copy is that source under that map" is one comparison, and a rename that follows the
map can never look like a contract change. The core is renamed itself only when a second real
consumer exists (R-8, deferred).

A map (spark-daemon-rename-map/1) names the roots to copy and an ordered list of [from, to]
replacements, applied in that order to every relative path and to every file's bytes.

    python3 -I -B vendor.py --source DIR --map MAP              print the binding
    python3 -I -B vendor.py --source DIR --map MAP --write OUT  also write the copy into OUT
    python3 -I -B vendor.py --source DIR --map MAP --check OUT  exit 1 unless OUT is exactly that copy

Standard library only; it imports nothing from the pattern.
"""

import hashlib
import json
import os
import sys

MAP_SCHEMA = "spark-daemon-rename-map/1"
SKIP_DIRS = {"__pycache__"}


def _canonical(value) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode("utf-8")


def load_map(path) -> dict:
    with open(path, encoding="utf-8") as fh:
        data = json.load(fh)
    if not (isinstance(data, dict) and data.get("schema") == MAP_SCHEMA
            and isinstance(data.get("roots"), list) and isinstance(data.get("replace"), list)
            and set(data) == {"schema", "roots", "replace"}):
        raise ValueError(f"not a {MAP_SCHEMA} map")
    for pair in data["replace"]:
        if not (isinstance(pair, list) and len(pair) == 2 and all(isinstance(s, str) and s for s in pair)):
            raise ValueError("each replacement must be a [from, to] pair of non-empty strings")
    return data


def files(root, roots) -> dict:
    """{relative path: bytes} for every file under the map's roots, in a fixed order."""
    out = {}
    for top in roots:
        base = os.path.join(root, top)
        if os.path.isfile(base):
            with open(base, "rb") as fh:
                out[top] = fh.read()
            continue
        for current, dirs, names in os.walk(base):
            dirs[:] = sorted(d for d in dirs if d not in SKIP_DIRS)
            for name in sorted(names):
                full = os.path.join(current, name)
                if os.path.islink(full) or not os.path.isfile(full):
                    continue
                with open(full, "rb") as fh:
                    out[os.path.relpath(full, root)] = fh.read()
    return dict(sorted(out.items()))


def transform(tree: dict, rename: dict) -> dict:
    out = {}
    for path, data in tree.items():
        for old, new in rename["replace"]:
            path = path.replace(old, new)
            data = data.replace(old.encode("utf-8"), new.encode("utf-8"))
        if path in out:
            raise ValueError(f"the map sends two files to {path}")
        out[path] = data
    return dict(sorted(out.items()))


def tree_sha256(tree: dict) -> str:
    h = hashlib.sha256()
    for path, data in sorted(tree.items()):
        h.update(path.encode("utf-8") + b"\0" + hashlib.sha256(data).hexdigest().encode() + b"\n")
    return h.hexdigest()


def binding(source, map_path) -> tuple:
    rename = load_map(map_path)
    tree = files(source, rename["roots"])
    copy = transform(tree, rename)
    return {"schema": "spark-daemon-vendor-binding/1",
            "source_tree_sha256": tree_sha256(tree),
            "rename_map_sha256": hashlib.sha256(_canonical(rename)).hexdigest(),
            "transformed_tree_sha256": tree_sha256(copy),
            "files": len(tree)}, copy, rename


def main(argv=None) -> int:
    import argparse
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("--source", required=True)
    p.add_argument("--map", required=True)
    group = p.add_mutually_exclusive_group()
    group.add_argument("--write", metavar="OUT")
    group.add_argument("--check", metavar="OUT")
    args = p.parse_args(argv)
    bound, copy, rename = binding(args.source, args.map)
    if args.write:
        for path, data in copy.items():
            full = os.path.join(args.write, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "wb") as fh:
                fh.write(data)
        with open(os.path.join(args.write, "VENDORED.json"), "w") as fh:
            json.dump(bound, fh, indent=2)
            fh.write("\n")
    if args.check:
        roots = sorted({path.split("/")[0] for path in copy})
        found = tree_sha256(files(args.check, roots))
        bound["checked"] = {"path": args.check, "tree_sha256": found,
                            "matches": found == bound["transformed_tree_sha256"]}
    print(json.dumps(bound, indent=2))
    return 1 if args.check and not bound["checked"]["matches"] else 0


if __name__ == "__main__":
    sys.exit(main())
