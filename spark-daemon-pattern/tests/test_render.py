"""Rendering: hostile values stay inside their blocks; structure never changes; bounds hold."""

import random
import unicodedata
import unittest

from helpers import ROOT  # noqa: F401
from spark_daemon import render
from spark_daemon.probes import HOSTILE, _random_text

STAMP = {"seq": 3, "head": "a" * 64, "last_event_utc": "2026-09-27T00:00:00.000000Z"}
SECTIONS = [("Status now", [("Value", "placeholder"), ("Count", 3), ("Flag", True), ("Missing", None)]),
            ("History", [("Last change", "placeholder")])]


def with_values(pick):
    return [(t, [(label, pick() if isinstance(v, str) else v) for label, v in rows]) for t, rows in SECTIONS]


def raw_forbidden(text):
    return [ch for ch in text if ch != "\n" and unicodedata.category(ch) in ("Cc", "Cf", "Zl", "Zp", "Co", "Cs", "Cn")]


class RenderTests(unittest.TestCase):
    def reference(self):
        return render.outside_block_lines(render.render_digest("demo", STAMP, with_values(lambda: ""), 1 << 20).decode())

    def test_hostile_corpus_is_inert(self):
        ref = self.reference()
        for value in HOSTILE:
            with self.subTest(value=value[:30]):
                text = render.render_digest("demo", STAMP, with_values(lambda: value), 1 << 20).decode()
                self.assertEqual(render.outside_block_lines(text), ref)
                self.assertEqual(raw_forbidden(text), [])

    def test_structure_invariance_property(self):
        rng = random.Random(20260927)
        ref = self.reference()
        for trial in range(500):
            text = render.render_digest("demo", STAMP, with_values(lambda: _random_text(rng)), 1 << 20).decode()
            self.assertEqual(render.outside_block_lines(text), ref, f"trial {trial}, seed 20260927")
            self.assertEqual(raw_forbidden(text), [], f"trial {trial}")

    def test_escapes_are_visible_and_unambiguous(self):
        rlo, bom = chr(0x202E), chr(0xFEFF)
        self.assertEqual(render.escape_visible("a" + rlo + "b"), "a\\u{202E}b")
        self.assertEqual(render.escape_visible(bom), "\\u{FEFF}")
        self.assertEqual(render.escape_visible("\\u{202E}"), "\\\\u{202E}")   # a typed look-alike stays distinct
        self.assertEqual(render.escape_visible("\t"), "\\u{0009}")

    def test_multiline_values_are_prefixed(self):
        block = render.render_block("```\n# heading")
        self.assertEqual(block[1:-1], ["| ```", "| # heading"])
        self.assertTrue(block[0].startswith("```"))

    def test_fence_longer_than_any_backtick_run(self):
        block = render.render_block("`````")
        self.assertEqual(block[0], "``````text")
        self.assertEqual(block[-1], "``````")

    def test_labels_are_validated(self):
        for bad in ("# heading", "[link](x)", "**bold**", "line\nbreak", "", "a" * 65, "<b>"):
            with self.subTest(bad=bad):
                with self.assertRaises(render.RenderError):
                    render.render_digest("demo", STAMP, [("Title", [(bad, "v")])], 1 << 20)

    def test_values_are_type_checked(self):
        for bad in (1.5, b"x", ["list"], {"a": 1}):
            with self.subTest(bad=bad):
                with self.assertRaises(render.RenderError):
                    render.render_digest("demo", STAMP, [("Title", [("Label", bad)])], 1 << 20)

    def test_size_bound_truncates_with_marker(self):
        sections = [("Rows", [(f"Row {i}", "x" * 200) for i in range(50)])]
        data = render.render_digest("demo", STAMP, sections, 2048)
        self.assertLessEqual(len(data), 2048)
        self.assertIn(b"rows omitted to stay within the digest size limit", data)

    def test_stamp_is_validated(self):
        with self.assertRaises(render.RenderError):
            render.render_digest("demo", dict(STAMP, head="not-hex"), SECTIONS, 1 << 20)

    def test_deterministic(self):
        a = render.render_digest("demo", STAMP, SECTIONS, 1 << 20)
        b = render.render_digest("demo", STAMP, SECTIONS, 1 << 20)
        self.assertEqual(a, b)


if __name__ == "__main__":
    unittest.main()
