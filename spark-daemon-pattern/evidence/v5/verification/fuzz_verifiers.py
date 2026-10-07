"""Run from tests/: python3 -B ../evidence/v5/verification/fuzz_verifiers.py "$PWD/helpers.py" SEED COUNT"""
"""Differential fuzz of the two ledger verifiers: random mutations of a real ledger; any
difference in outcome (including a crash) is printed."""
import os, random, sys, collections
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(sys.argv[1])), ".."))
sys.path.insert(0, os.path.dirname(os.path.abspath(sys.argv[1])))
from helpers import Sandbox
from spark_daemon import status

def outcome(fn, *a):
    try:
        return fn(*a)
    except BaseException as e:
        return {"crash": type(e).__name__}

sb = Sandbox("counter")
sb.write("data/value.txt", "x")
for i in range(3):
    sb.write("data/value.txt", f"value {i} é中\U0001F600")
    sb.run(cycles=2)
good = sb.ledger_bytes()
name, maxb = "fixture-counter", 8192
rng = random.Random(int(sys.argv[2]))
path = os.path.join(sb.tmp, "fuzz.jsonl")
TOKENS = [b"true", b"1", b"-0", b"1.0", b"1e2", b"\\ud800", b"\\u0000", b"\r", b" ", b"\xef\xbb\xbf", b"\x80",
          b'"', b"\\", b"{", b"}", b"[", b"]", b",", b":", b"null", b"9007199254740993", b"\\u2028", b"\n", b"[" * 1500, b"{\"\\\\udc00\":0}"]
diffs, kinds = [], collections.Counter()
N = int(sys.argv[3])
for i in range(N):
    data = bytearray(good)
    for _ in range(rng.choice((1, 1, 1, 2, 3))):
        op = rng.randrange(6)
        at = rng.randrange(len(data) + 1)
        if op == 0 and data:
            at = min(at, len(data) - 1); data[at] = rng.randrange(256)
        elif op == 1:
            data[at:at] = rng.choice(TOKENS)
        elif op == 2 and data:
            del data[at:at + rng.randrange(1, 4)]
        elif op == 3:
            lines = bytes(data).split(b"\n"); j = rng.randrange(len(lines)); k = rng.randrange(len(lines))
            lines[j], lines[k] = lines[k], lines[j]; data = bytearray(b"\n".join(lines))
        elif op == 4:
            data = data[:rng.randrange(len(data) + 1)]
        else:
            lines = bytes(data).split(b"\n"); j = rng.randrange(len(lines)); lines.insert(j, lines[j]); data = bytearray(b"\n".join(lines))
    with open(path, "wb") as fh:
        fh.write(data)
    a = outcome(status.primary_outcome, path, name, maxb)
    b = outcome(status.independent_outcome, path, name, maxb)
    kinds["crash" if ("crash" in a or "crash" in b) else ("intact" if a.get("intact") else "broken")] += 1
    if a != b:
        diffs.append((i, a, b))
print(f"{N} mutations: {dict(kinds)}; disagreements: {len(diffs)}")
for i, a, b in diffs[:10]:
    print(" ", i, "primary", a, "| independent", b)
sb.cleanup()
