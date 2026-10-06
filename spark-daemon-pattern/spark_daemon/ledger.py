"""The daemon's append-only, hash-chained ledger, and startup recovery.

Provisional: this follows the WBS 3.0 spec's recommendations (r2, awaiting adjudication) in
miniature. When the Repository Observer's own writer exists, this module should be replaced
by it rather than maintained as a second implementation.

Record: one canonical JSON object per line, terminated by "\n". Its SHA-256 is taken over
the canonical bytes without the newline and carried by the next record's prev_sha256.
seq starts at 1; the first record links to 64 zeros.

Commit point: a record is committed once its whole line, newline included, is written in
one write() call and fsync'd. Any write error, short write or fsync error raises
UncertainCommit; the runtime then exits and startup recovery decides from what is on disk.
fsync is never retried.
"""

import os
import secrets
from dataclasses import dataclass, replace

from . import LEDGER_NAME, LEDGER_SCHEMA, QUARANTINE_DIR
from .canonical import GENESIS, CanonicalError, canonical_bytes, sha256_hex, strict_loads

ENVELOPE_KEYS = frozenset({"schema", "seq", "prev_sha256", "daemon", "run_id", "event_id",
                           "timestamp_utc", "event_type", "payload"})


class LedgerCorrupt(Exception):
    """A complete line fails verification. The daemon refuses to start and changes nothing."""

    def __init__(self, category, seq=None, offset=None):
        self.category, self.seq, self.offset = category, seq, offset
        super().__init__(f"ledger corrupt: {category} at seq {seq} (byte offset {offset})")


class UncertainCommit(Exception):
    """A ledger write or fsync failed; the process must exit without retrying."""


class RecordTooLarge(ValueError):
    """A record exceeds the manifest's record_max_bytes. Nothing is written."""


@dataclass(frozen=True)
class Tail:
    seq: int = 0
    head: str = GENESIS


@dataclass(frozen=True)
class TornTail:
    offset: int
    length: int


@dataclass(frozen=True)
class ScanResult:
    tail: Tail
    records: int
    torn: TornTail | None
    last_event_type: str | None
    last_timestamp: str | None


@dataclass(frozen=True)
class Quarantine:
    file: str
    length: int
    sha256: str


def verify_line(body: bytes, expected_seq: int, expected_prev: str, daemon: str,
                max_bytes: int, offset: int):
    """Verify one complete line (without its newline). Returns (record, sha256)."""
    if len(body) > max_bytes:
        raise LedgerCorrupt("RECORD_TOO_LARGE", expected_seq, offset)
    try:
        record = strict_loads(body)
    except (UnicodeDecodeError, ValueError):
        raise LedgerCorrupt("UNPARSEABLE", expected_seq, offset) from None
    if not isinstance(record, dict):
        raise LedgerCorrupt("ENVELOPE", expected_seq, offset)
    schema = record.get("schema")
    if schema != LEDGER_SCHEMA:
        category = "UNSUPPORTED_SCHEMA" if isinstance(schema, str) and schema.startswith(
            "spark-daemon-ledger/") else "ENVELOPE"
        raise LedgerCorrupt(category, expected_seq, offset)
    if set(record) != ENVELOPE_KEYS:
        raise LedgerCorrupt("ENVELOPE", expected_seq, offset)
    try:
        if canonical_bytes(record) != body:
            raise LedgerCorrupt("NOT_CANONICAL", expected_seq, offset)
    except CanonicalError:
        raise LedgerCorrupt("NOT_CANONICAL", expected_seq, offset) from None
    # r4.11 (HF-38): `type(...) is int`, because True == 1 in Python: a record whose seq was the
    # JSON boolean true verified as seq 1. Found by writing the independent verifier.
    if type(record["seq"]) is not int or record["seq"] != expected_seq:
        raise LedgerCorrupt("SEQUENCE", expected_seq, offset)
    if record["prev_sha256"] != expected_prev:
        raise LedgerCorrupt("LINK", expected_seq, offset)
    if record["daemon"] != daemon:
        raise LedgerCorrupt("DAEMON_MISMATCH", expected_seq, offset)
    return record, sha256_hex(body)


def scan(fh, daemon: str, max_bytes: int, on_record=None) -> ScanResult:
    """One forward streaming pass: memory bounded by max_bytes, not by ledger length.

    The torn tail is every byte after the last newline, whatever it contains, including a
    complete, parseable record whose newline was never written (it was never committed).
    """
    offset, seq, head = 0, 0, GENESIS
    last_type = last_ts = None
    while True:
        line = fh.readline(max_bytes + 2)
        if not line:
            break
        if not line.endswith(b"\n"):
            if fh.read(1):                          # longer than any record can be
                raise LedgerCorrupt("RECORD_TOO_LARGE", seq + 1, offset)
            return ScanResult(Tail(seq, head), seq, TornTail(offset, len(line)), last_type, last_ts)
        record, sha = verify_line(line[:-1], seq + 1, head, daemon, max_bytes, offset)
        seq, head = record["seq"], sha
        last_type, last_ts = record["event_type"], record["timestamp_utc"]
        if on_record is not None:
            on_record(record)
        offset += len(line)
    return ScanResult(Tail(seq, head), seq, None, last_type, last_ts)


def verify_file(path: str, daemon: str, max_bytes: int, on_record=None) -> ScanResult:
    """Read-only verification. Never modifies the file."""
    with open(path, "rb") as fh:
        return scan(fh, daemon, max_bytes, on_record)


def _fsync_dir(path: str) -> None:
    fd = os.open(path, os.O_RDONLY | os.O_DIRECTORY | os.O_CLOEXEC)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def recover(directory: str, daemon: str, max_bytes: int, on_record=None):
    """Startup recovery: verify the chain, quarantine a torn tail. Returns (ScanResult, Quarantine|None).

    Raises LedgerCorrupt (nothing changed) if any complete line fails verification.
    Running it twice on the same files gives the same result and the same bytes.
    """
    path = os.path.join(directory, LEDGER_NAME)
    if not os.path.exists(path):
        return ScanResult(Tail(), 0, None, None, None), None
    fd = os.open(path, os.O_RDONLY | os.O_CLOEXEC)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)
    _fsync_dir(directory)
    result = verify_file(path, daemon, max_bytes, on_record)
    if result.torn is None:
        return result, None

    with open(path, "rb") as fh:
        fh.seek(result.torn.offset)
        fragment = fh.read()
    frag_sha = sha256_hex(fragment)
    qdir = os.path.join(directory, QUARANTINE_DIR)
    counter = 0
    while True:
        name = f"torn-seq{result.tail.seq + 1:08d}-{frag_sha[:16]}-{counter}.bin"
        try:
            qfd = os.open(os.path.join(qdir, name), os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_CLOEXEC, 0o600)
            break
        except FileExistsError:
            counter += 1
    try:
        view = memoryview(fragment)
        while view:
            written = os.write(qfd, view)
            view = view[written:]
        os.fsync(qfd)
    finally:
        os.close(qfd)
    _fsync_dir(qdir)
    lfd = os.open(path, os.O_WRONLY | os.O_CLOEXEC)
    try:
        os.ftruncate(lfd, result.torn.offset)
        os.fsync(lfd)
    finally:
        os.close(lfd)
    return replace(result, torn=None), Quarantine(name, len(fragment), frag_sha)


class LedgerWriter:
    """The only writer of a daemon's ledger. The single-instance lock ensures one appender."""

    def __init__(self, directory: str, daemon: str, run_id: str, max_bytes: int, tail: Tail, clock):
        self.path = os.path.join(directory, LEDGER_NAME)
        self.directory = directory
        self.daemon = daemon
        self.run_id = run_id
        self.max_bytes = max_bytes
        self.tail = tail
        self.clock = clock
        self._fd = None

    def open(self) -> None:
        existed = os.path.exists(self.path)
        self._fd = os.open(self.path, os.O_WRONLY | os.O_APPEND | os.O_CREAT | os.O_CLOEXEC, 0o600)
        if not existed:
            try:
                _fsync_dir(self.directory)
            except OSError:
                raise UncertainCommit("directory fsync after create") from None

    def close(self) -> None:
        if self._fd is not None:
            os.close(self._fd)
            self._fd = None

    def prepare(self, events) -> list:
        """Build and size-check a batch before writing any of it. Raises before any byte is written."""
        seq, head, out = self.tail.seq, self.tail.head, []
        for event_type, payload in events:
            seq += 1
            record = {
                "schema": LEDGER_SCHEMA, "seq": seq, "prev_sha256": head, "daemon": self.daemon,
                "run_id": self.run_id, "event_id": secrets.token_hex(16),
                "timestamp_utc": self.clock(), "event_type": event_type, "payload": payload,
            }
            data = canonical_bytes(record)            # CanonicalError: nothing written
            if len(data) > self.max_bytes:
                raise RecordTooLarge(f"{event_type} record is {len(data)} bytes")
            head = sha256_hex(data)
            out.append((record, data, head))
        return out

    def commit(self, prepared) -> list:
        committed = []
        for record, data, head in prepared:
            line = data + b"\n"
            try:
                written = os.write(self._fd, line)
            except OSError:
                raise UncertainCommit("write failed") from None
            if written != len(line):
                raise UncertainCommit("short write")
            try:
                os.fsync(self._fd)
            except OSError:
                raise UncertainCommit("fsync failed") from None
            self.tail = Tail(record["seq"], head)
            committed.append(record)
        return committed

    def append(self, event_type: str, payload) -> dict:
        return self.commit(self.prepare([(event_type, payload)]))[0]
