"""Shared helpers for the skeleton self-tests (standard library only)."""

import json
import os
import shutil
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ENTRY = os.path.join(ROOT, "bin", "spark-daemon")
FIXTURES = os.path.join(ROOT, "tests", "fixtures", "daemons")
EXAMPLE = os.path.join(ROOT, "examples", "meminfo-watch")

if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from spark_daemon import SYSTEM_PATH  # noqa: E402  (needs ROOT on sys.path)


class Sandbox:
    """A disposable HOME with a copy of one daemon, run in child processes."""

    def __init__(self, fixture: str, manifest_overrides: dict | None = None):
        self.tmp = tempfile.mkdtemp(prefix="spark-selftest-")
        self.home = os.path.join(self.tmp, "home")
        os.makedirs(os.path.join(self.home, "data"), mode=0o700)
        os.makedirs(os.path.join(self.home, "spark-core", "data"), mode=0o700)
        with open(os.path.join(self.home, "spark-core", "data", "canary.txt"), "w") as fh:
            fh.write("CANARY-7f3a: must never be read\n")
        src = fixture if os.path.isabs(fixture) else os.path.join(FIXTURES, fixture)
        self.daemon_dir = os.path.join(self.tmp, "daemon")
        os.makedirs(self.daemon_dir)
        shutil.copyfile(os.path.join(src, "daemon.py"), os.path.join(self.daemon_dir, "daemon.py"))
        with open(os.path.join(src, "manifest.json")) as fh:
            data = json.load(fh)
        data.update(manifest_overrides or {})
        self.manifest = os.path.join(self.daemon_dir, "manifest.json")
        with open(self.manifest, "w") as fh:
            json.dump(data, fh, indent=2)
        self.data = data
        out = data["output_dir"]
        self.output = os.path.join(self.home, out[2:]) if out.startswith("~/") else out

    def env(self, **extra):
        env = {"HOME": self.home, "SPARK_DAEMON_HOME": self.home, "PATH": SYSTEM_PATH,
               "LANG": "C.UTF-8", "PYTHONDONTWRITEBYTECODE": "1"}
        env.update(extra)
        return env

    def write(self, rel: str, text: str):
        path = os.path.join(self.home, rel)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as fh:
            fh.write(text)

    def qualified_args(self):
        """What a qualified unit's ExecStart adds: the digests of the files as they are, so the
        runtime's gate accepts the start."""
        from spark_daemon import qualify
        return qualify.expect_args(qualify.expected_identity(self.manifest))

    def argv(self, cycles=3, *, production=False, qualified=True, **params):
        """The command line of one start. By default the harness entry (contract 5: explicit,
        recorded parameters; interval_ms defaults to 100) with the files' digests. production
        uses `run`, which takes no harness parameter at all."""
        args = [sys.executable, "-I", "-B", ENTRY, "run" if production else "harness", "--manifest", self.manifest]
        if qualified:
            args += self.qualified_args()
        if not production:
            params.setdefault("interval_ms", 100)
            for key, value in params.items():
                args += [f"--{key.replace('_', '-')}", str(value)]
        elif params:
            raise TypeError(f"run takes no harness parameter: {sorted(params)}")
        if cycles is not None:
            args += ["--max-cycles", str(cycles)]
        return args

    def run(self, cycles=3, timeout=30, production=False, qualified=True, env=None, **params):
        return subprocess.run(self.argv(cycles, production=production, qualified=qualified, **params),
                              env=self.env(**(env or {})), capture_output=True, text=True, timeout=timeout)

    def cli(self, *args, timeout=60, **env):
        return subprocess.run([sys.executable, "-I", "-B", ENTRY, *args], env=self.env(**env),
                              capture_output=True, text=True, timeout=timeout)

    @property
    def ledger_path(self):
        return os.path.join(self.output, "ledger.jsonl")

    def records(self):
        if not os.path.exists(self.ledger_path):
            return []
        with open(self.ledger_path, "rb") as fh:
            return [json.loads(line) for line in fh if line.endswith(b"\n")]

    def ledger_bytes(self):
        with open(self.ledger_path, "rb") as fh:
            return fh.read()

    def cleanup(self):
        shutil.rmtree(self.tmp, ignore_errors=True)
