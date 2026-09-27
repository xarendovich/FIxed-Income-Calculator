"""Canonical JSON and hashing.

One total mapping from Python values to JSON values, so equal inputs always give equal
bytes (WBS 3.0 CS1-CS9 recommendations, applied here to daemon ledgers):

- UTF-8, keys sorted by UTF-16 code unit, separators (",", ":"), ensure_ascii=False,
  allow_nan=False
- allowed: None, bool, int within +/-(2**53 - 1), str, list, tuple, dict with str keys,
  str-valued Enum, dataclass instances
- refused: float, set, frozenset, bytes, datetime, non-string keys, anything else
- a string that cannot be encoded as UTF-8 (a lone surrogate) is refused

Key order (r2, AP-03/J1): mapping keys sort by their UTF-16 code units, not by Python's
default code-point order. For every value this module accepts (no floats, no surrogates),
that makes canonical_bytes() identical, byte for byte, to RFC 8785 JSON Canonicalization
Scheme (JCS): confirmed against an independent ECMAScript reference on 10,002 sampled
values, including the adversarial case (mapping keys mixing U+E000-U+FFFF with characters
above U+FFFF) where code-point order disagrees with JCS. See evidence/ap/jcs_crosscheck.py.
A ledger can then be verified by any RFC 8785 implementation plus SHA-256, not only by this
package. This changed the golden hash below; there is no deployed ledger to migrate (r1 was
never installed).
"""

import dataclasses
import enum
import hashlib
import json

GENESIS = "0" * 64
MAX_SAFE_INT = 2**53 - 1
MAX_DEPTH = 64


class CanonicalError(ValueError):
    """A value cannot be represented canonically. Nothing is written."""


def to_json_value(value, _depth=0):
    if _depth > MAX_DEPTH:
        raise CanonicalError("nesting too deep")
    if value is None or isinstance(value, bool):
        return value
    if isinstance(value, enum.Enum):
        if not isinstance(value.value, str):
            raise CanonicalError("enum values must be strings")
        return value.value
    if isinstance(value, int):
        if not -MAX_SAFE_INT <= value <= MAX_SAFE_INT:
            raise CanonicalError("integer out of the safe range")
        return value
    if isinstance(value, str):
        return value
    if isinstance(value, float):
        raise CanonicalError("floats are not allowed; use integers with a stated unit")
    if isinstance(value, (list, tuple)):
        return [to_json_value(v, _depth + 1) for v in value]
    if isinstance(value, dict):
        out = {}
        for key, item in value.items():
            if not isinstance(key, str):
                raise CanonicalError("mapping keys must be strings")
            out[key] = to_json_value(item, _depth + 1)
        return out
    if dataclasses.is_dataclass(value) and not isinstance(value, type):
        return {f.name: to_json_value(getattr(value, f.name), _depth + 1)
                for f in dataclasses.fields(value)}
    raise CanonicalError(f"type {type(value).__name__} is not allowed")


def _jcs_key_order(value, _depth=0):
    """Rebuild every mapping with its keys already in RFC 8785 order (UTF-16 code units),
    so json.dumps(..., sort_keys=False) below emits them in that order. Python's own
    sort_keys=True instead sorts by code point, which disagrees with JCS exactly when a
    mapping's keys mix U+E000-U+FFFF with characters above U+FFFF (surrogate pairs sort
    below U+E000 in UTF-16 but above it in code points)."""
    if _depth > MAX_DEPTH:
        raise CanonicalError("nesting too deep")
    if isinstance(value, dict):
        ordered = sorted(value.items(), key=lambda kv: kv[0].encode("utf-16-be"))
        return {k: _jcs_key_order(v, _depth + 1) for k, v in ordered}
    if isinstance(value, list):
        return [_jcs_key_order(v, _depth + 1) for v in value]
    return value


def canonical_bytes(value) -> bytes:
    text = json.dumps(_jcs_key_order(to_json_value(value)), sort_keys=False,
                      separators=(",", ":"), ensure_ascii=False, allow_nan=False)
    try:
        return text.encode("utf-8")
    except UnicodeEncodeError:
        raise CanonicalError("text is not valid Unicode (lone surrogate)") from None


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def strict_loads(line: bytes):
    """Parse one JSON document strictly: valid UTF-8, no duplicate keys, no floats, no NaN."""

    def pairs(items):
        out = {}
        for key, item in items:
            if key in out:
                raise CanonicalError("duplicate key")
            out[key] = item
        return out

    def no_float(_text):
        raise CanonicalError("floats are not allowed")

    def no_constant(_text):
        raise CanonicalError("NaN and Infinity are not allowed")

    text = line.decode("utf-8")
    return json.loads(text, object_pairs_hook=pairs, parse_float=no_float,
                      parse_constant=no_constant)
