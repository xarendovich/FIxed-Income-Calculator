"""Lists every self-test ID (module.Class.test), sorted, so test-count changes between revisions
can be explained one by one. Run from the pattern root: python3 -B evidence/v5/list_tests.py"""
import os
import sys
import unittest

root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path[:0] = [root, os.path.join(root, "tests")]


def walk(suite):
    for item in suite:
        if isinstance(item, unittest.TestSuite):
            yield from walk(item)
        else:
            yield item.id()


ids = sorted(walk(unittest.defaultTestLoader.discover(os.path.join(root, "tests"), top_level_dir=os.path.join(root, "tests"))))
print("\n".join(ids))
