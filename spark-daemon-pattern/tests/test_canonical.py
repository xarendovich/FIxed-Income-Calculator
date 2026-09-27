"""Canonical JSON: golden vector, refusals, determinism, strict parsing."""

import dataclasses
import enum
import os
import subprocess
import sys
import unittest

from helpers import ROOT  # noqa: F401  (puts the package on sys.path)
from spark_daemon.canonical import (GENESIS, CanonicalError, canonical_bytes, sha256_hex,
                                    strict_loads, to_json_value)

GOLDEN_INPUT = {"b": [1, "é", None, True], "a": {"z": "\u202e", "y": -5}}
GOLDEN_BYTES = '{"a":{"y":-5,"z":"\u202e"},"b":[1,"é",null,true]}'.encode("utf-8")
GOLDEN_SHA = "2964fce716e88f1ca7d354c01b0c5da474f3b451d2279b4823da4b620adaa34b"  # pinned: changing it is a schema change


# AP-03/J1: a mapping whose keys mix U+E000-U+FFFF with a character above U+FFFF (a UTF-16
# surrogate pair) is the one case where code-point key order and RFC 8785 (JCS, UTF-16 code
# unit order) disagree. chr(0x1F600) encodes as the surrogate pair U+D83D U+DE00, which sorts
# below chr(0xE000) in UTF-16 but above it in code points, so JCS order is the grinning-face
# key first. Built with chr() (never a literal \\uXXXX escape - see source-hygiene note below).
JCS_DIVERGENT_INPUT = {chr(0xE000): 1, chr(0x1F600): 2}
JCS_DIVERGENT_BYTES = ('{"' + chr(0x1F600) + '":2,"' + chr(0xE000) + '":1}').encode("utf-8")
CODE_POINT_ORDER_BYTES = ('{"' + chr(0xE000) + '":1,"' + chr(0x1F600) + '":2}').encode("utf-8")


class Colour(enum.Enum):
    RED = "red"


class Number(enum.IntEnum):
    ONE = 1


@dataclasses.dataclass(frozen=True)
class Point:
    x: int
    label: str


class CanonicalTests(unittest.TestCase):
    def test_golden_vector(self):
        data = canonical_bytes(GOLDEN_INPUT)
        self.assertEqual(data, GOLDEN_BYTES)
        self.assertEqual(sha256_hex(data), GOLDEN_SHA)

    def test_genesis_is_64_zeros(self):
        self.assertEqual(GENESIS, "0" * 64)

    def test_key_order_matches_rfc8785_not_code_point_order(self):
        data = canonical_bytes(JCS_DIVERGENT_INPUT)
        self.assertEqual(data, JCS_DIVERGENT_BYTES)
        self.assertNotEqual(data, CODE_POINT_ORDER_BYTES)

    def test_key_order_nested(self):
        # The reordering must apply at every depth, not only the top level.
        nested = {"outer": {chr(0xE000): "a", chr(0x1F600): "b"}}
        data = canonical_bytes(nested)
        self.assertIn(
            ('"' + chr(0x1F600) + '":"b","' + chr(0xE000) + '":"a"').encode("utf-8"), data)

    def test_allowed_conversions(self):
        self.assertEqual(to_json_value(Colour.RED), "red")
        self.assertEqual(to_json_value(Point(3, "p")), {"x": 3, "label": "p"})
        self.assertEqual(to_json_value((1, 2)), [1, 2])

    def test_refusals(self):
        for bad in (0.5, float("nan"), {1, 2}, frozenset(), b"x", 2**53, -(2**53), {1: "a"},
                    Number.ONE, object()):
            with self.subTest(bad=repr(bad)[:30]):
                with self.assertRaises(CanonicalError):
                    canonical_bytes({"v": bad})

    def test_lone_surrogate_refused(self):
        with self.assertRaises(CanonicalError):
            canonical_bytes({"v": "\ud800"})

    def test_independent_of_hash_seed(self):
        code = ("import sys; sys.path.insert(0, %r); from spark_daemon.canonical import canonical_bytes;"
                "s = {'delta', 'alpha', 'charlie', 'bravo', 'echo'};"
                "sys.stdout.write(canonical_bytes({k: len(k) for k in s}).hex())") % ROOT
        outputs = set()
        for seed in ("0", "1", "12345"):
            env = dict(os.environ, PYTHONHASHSEED=seed)
            outputs.add(subprocess.run([sys.executable, "-c", code], env=env, capture_output=True,
                                       text=True, check=True).stdout)
        self.assertEqual(len(outputs), 1)

    def test_strict_loads(self):
        self.assertEqual(strict_loads(b'{"a":1}'), {"a": 1})
        for bad in (b'{"a":1,"a":2}', b'{"a":1.5}', b'{"a":NaN}', b'\xff'):
            with self.subTest(bad=bad):
                with self.assertRaises((ValueError, UnicodeDecodeError)):
                    strict_loads(bad)



if __name__ == "__main__":
    unittest.main()
