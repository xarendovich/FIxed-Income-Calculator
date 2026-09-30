"""Emit the self-test inventory as Markdown: per file, per class, each test with its first
docstring line or leading comment, and any HF/PD/DB/U tags it cites."""
import ast, os, re, sys
root = sys.argv[1]
tag_re = re.compile(r"\b(HF-\d+|PD-\d+(?:\.\d+)?|DB-\d+|U-\d+|AP-\d+|F\d|PX-\d+|B-\d+|BF-\d|S-\d)\b")
# HARDENING.md names, for each defect, the tests that fail without its fix.
hf_by_test = {}
for line in open(os.path.join(root, "HARDENING.md")):
    m = re.match(r"\| (HF-\d+) \|", line)
    if m:
        for name in re.findall(r"`(test_[A-Za-z0-9_]+)", line):
            hf_by_test.setdefault(name, set()).add(m.group(1))
out, total = [], 0
for fn in sorted(os.listdir(os.path.join(root, "tests"))):
    if not (fn.startswith("test_") and fn.endswith(".py")):
        continue
    path = os.path.join(root, "tests", fn)
    src = open(path).read()
    lines = src.splitlines()
    tree = ast.parse(src)
    mod_doc = (ast.get_docstring(tree) or "").split("\n")[0]
    rows = []
    for cls in [n for n in tree.body if isinstance(n, ast.ClassDef)]:
        for f in [n for n in cls.body if isinstance(n, ast.FunctionDef) and n.name.startswith("test_")]:
            doc = ast.get_docstring(f) or ""
            note = doc.strip().replace("\n", " ")
            if not note:
                body_start = f.body[0].lineno - 1
                comments = []
                for ln in lines[f.lineno:body_start + 3]:
                    s = ln.strip()
                    if s.startswith("#"):
                        comments.append(s.lstrip("# "))
                note = " ".join(comments)
            text = " ".join(lines[f.lineno - 1:f.end_lineno])
            found = set(tag_re.findall(doc + " " + text)) | hf_by_test.get(f.name, set())
            tags = sorted(found, key=lambda t: (t.split("-")[0], int(re.sub(r"\D", "", t) or 0)))
            note = re.sub(r"\s+", " ", note)
            if len(note) > 220:
                note = note[:217].rsplit(" ", 1)[0] + " …"
            rows.append((cls.name, f.name, note, ", ".join(tags)))
    total += len(rows)
    out.append(f"\n#### `tests/{fn}` ({len(rows)} tests)\n")
    if mod_doc:
        out.append(f"{mod_doc}\n")
    out.append("| Class | Test | What it pins | Cites |\n| --- | --- | --- | --- |")
    for c, n, note, tags in rows:
        out.append(f"| `{c}` | `{n}` | {note.replace('|', '/') or n[5:].replace('_', ' ').capitalize() + '.'} | {tags} |")
print(f"<!-- generated: {total} tests -->")
print("\n".join(out))
