"""Puts the pattern's package and these tests' helpers on the import path before pytest
collects the test modules, so the order of their imports does not matter."""

import os
import sys

TESTS = os.path.dirname(os.path.abspath(__file__))
for path in (os.path.dirname(TESTS), TESTS):
    if path not in sys.path:
        sys.path.insert(0, path)
