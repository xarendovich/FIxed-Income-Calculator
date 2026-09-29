"""The handoff layer: the published contract, the manifest JSON Schema, candidate envelopes,
validate --json, precheck, scaffold and the reference daemons."""

import copy
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

from helpers import ENTRY, FIXTURES, ROOT, Sandbox
from spark_daemon import contract, handoff, manifest, scaffold

CONTRACT_DIR = os.path.join(ROOT, "contract")
EXAMPLES = os.path.join(ROOT, "examples")
EXAMPLE_NAMES = sorted(d for d in os.listdir(EXAMPLES) if os.path.isfile(os.path.join(EXAMPLES, d, "manifest.json")))

try:
    import jsonschema
except ImportError:          # optional: CI installs it; the stdlib-only runtime never needs it
    jsonschema = None


def cli(*args, timeout=120):
    return subprocess.run([sys.executable, "-I", "-B", ENTRY, *args], capture_output=True, text=True,
                          timeout=timeout, env={"PATH": "/usr/bin:/bin", "HOME": "/nonexistent", "LANG": "C.UTF-8"})


def load_json(path):
    with open(path, encoding="utf-8") as fh:
        return json.load(fh)


class PublishedContractTests(unittest.TestCase):
    def test_committed_contract_matches_the_running_rules(self):
        self.assertEqual(load_json(os.path.join(CONTRACT_DIR, "daemon-contract.json")), contract.describe(),
                         "contract/daemon-contract.json is stale: run `make contract`")

    def test_committed_schema_matches_the_running_validator(self):
        self.assertEqual(load_json(os.path.join(CONTRACT_DIR, "manifest.schema.json")),
                         contract.manifest_json_schema(), "contract/manifest.schema.json is stale: run `make contract`")

    def test_contract_version_is_pinned_to_its_sha(self):
        """Changing any enforced rule changes contract_sha256. That must come with a new
        CONTRACT_VERSION and a new line in contract/versions.json, never a silent edit."""
        versions = {k: v for k, v in load_json(os.path.join(CONTRACT_DIR, "versions.json")).items()
                    if not k.startswith("_")}
        identity = contract.contract_identity()
        self.assertIn(identity["contract_version"], versions,
                      "CONTRACT_VERSION is not recorded in contract/versions.json")
        self.assertEqual(versions[identity["contract_version"]], identity["contract_sha256"],
                         "the contract changed without a CONTRACT_VERSION bump (see contract/versions.json)")
        self.assertEqual(len(set(versions.values())), len(versions), "two versions share one contract")

    def test_contract_is_identical_on_every_installed_python(self):
        seen = {}
        for version in ("3.10", "3.11", "3.12", "3.13"):
            exe = shutil.which(f"python{version}", path="/usr/bin:/bin:/usr/local/bin")
            if not exe:
                continue
            p = subprocess.run([exe, "-I", "-B", ENTRY, "describe", "--identity"], capture_output=True, text=True,
                               timeout=60, env={"PATH": "/usr/bin:/bin", "HOME": "/nonexistent"})
            if p.returncode == 0:
                seen[version] = json.loads(p.stdout)["contract_sha256"]
        if len(seen) < 2:
            self.skipTest("fewer than two Python versions installed")
        self.assertEqual(len(set(seen.values())), 1, seen)

    def test_contract_names_every_ctx_method_and_battery_check(self):
        body = contract.describe()["contract"]
        names = {m["name"] for m in body["ctx"]["methods"]}
        self.assertEqual(names, {"read_text", "list_dir", "stat", "disk_usage", "run", "git", "now_utc",
                                 "unsettled"})
        self.assertEqual(len(body["evidence"]["battery"]["checks"]), 18)
        self.assertIn("no authority", body["authority"])

    def test_describe_cli(self):
        p = cli("describe")
        self.assertEqual(p.returncode, 0)
        self.assertEqual(json.loads(p.stdout)["schema"], "spark-daemon-contract/1")


@unittest.skipUnless(jsonschema, "jsonschema not installed (pip install jsonschema); CI runs this")
class SchemaAgreementTests(unittest.TestCase):
    """The JSON Schema and manifest.py must agree: everything the schema can express, both
    accept or both refuse. Cross-field rules are manifest.py's alone, and listed as such."""

    @classmethod
    def setUpClass(cls):
        cls.validator = jsonschema.Draft202012Validator(contract.manifest_json_schema())

    def schema_ok(self, data):
        return not list(self.validator.iter_errors(data))

    def python_ok(self, data):
        try:
            manifest.parse(data)
            return True
        except manifest.ManifestError:
            return False

    def test_schema_is_valid_draft_2020_12(self):
        jsonschema.Draft202012Validator.check_schema(contract.manifest_json_schema())

    def test_every_shipped_manifest_passes_both(self):
        paths = [os.path.join(EXAMPLES, d, "manifest.json") for d in EXAMPLE_NAMES]
        paths += [os.path.join(FIXTURES, d, "manifest.json") for d in sorted(os.listdir(FIXTURES))]
        for path in paths:
            with self.subTest(path=os.path.relpath(path, ROOT)):
                data = load_json(path)
                self.assertTrue(self.schema_ok(data))
                self.assertTrue(self.python_ok(data))

    def test_mutations_are_refused_by_both(self):
        base = load_json(os.path.join(EXAMPLES, "meminfo-watch", "manifest.json"))
        mutations = {
            "unknown top-level key": lambda d: d.update(extra=1),
            "unknown nested key": lambda d: d["trigger"].update(extra=1),
            "missing key": lambda d: d.pop("digest"),
            "bad name": lambda d: d.update(name="Bad_Name"),
            "name with trailing newline": lambda d: d.update(name="meminfo-watch\n"),
            "bad version": lambda d: d.update(version="1.2"),
            "percent in purpose": lambda d: d.update(purpose="Uses 100% of something, which is refused"),
            "act class": lambda d: d.update(daemon_class="act"),
            "inotify trigger": lambda d: d["trigger"].update(kind="inotify-wakeup"),
            "interval too short": lambda d: d["trigger"].update(interval_seconds=4),
            "float interval": lambda d: d["trigger"].update(interval_seconds=30.5),
            "no reads": lambda d: d.update(reads=[]),
            "dot-dot read": lambda d: d.update(reads=["/proc/../etc/shadow"]),
            "trailing slash": lambda d: d.update(reads=["/proc/meminfo/"]),
            "relative read": lambda d: d.update(reads=["proc/meminfo"]),
            "shell command": lambda d: d.update(commands=["bash"]),
            "versioned interpreter": lambda d: d.update(commands=["python3.12"]),
            "duplicate command": lambda d: d.update(commands=["git", "git"]),
            "named network": lambda d: d["network"].update(mode="named"),
            "root user": lambda d: d["run_as"].update(user="root"),
            "user unit with user": lambda d: d.update(run_as={"unit": "user", "user": "x"}),
            "memory below floor": lambda d: d["resources"].update(memory_max_mb=63),
            "reserved event": lambda d: d["ledger"].update(event_types=["DAEMON_START"]),
            "lowercase event": lambda d: d["ledger"].update(event_types=["memory_changed"]),
            "digest enabled not bool": lambda d: d["digest"].update(enabled="yes"),
        }
        for label, mutate in mutations.items():
            data = copy.deepcopy(base)
            mutate(data)
            with self.subTest(mutation=label):
                self.assertFalse(self.schema_ok(data), "schema accepted it")
                self.assertFalse(self.python_ok(data), "manifest.py accepted it")

    def test_cross_field_rules_are_manifest_py_only_and_declared(self):
        base = load_json(os.path.join(EXAMPLES, "meminfo-watch", "manifest.json"))
        data = copy.deepcopy(base)
        data["step_timeout_seconds"] = 60                    # more than half of watchdog 90
        self.assertTrue(self.schema_ok(data))
        self.assertFalse(self.python_ok(data))
        self.assertTrue(any("half of watchdog" in r for r in
                            contract.manifest_json_schema()["x-spark-cross-field-rules"]))


class EnvelopeTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.dir = os.path.join(self.tmp, "cand")
        scaffold.scaffold(self.dir, name="hostname-watch")
        self.manifest = os.path.join(self.dir, "manifest.json")
        self.envelope = os.path.join(self.dir, "candidate.json")

    def tearDown(self):
        shutil.rmtree(self.tmp)

    def rewrite(self, change):
        env = load_json(self.envelope)
        change(env)
        with open(self.envelope, "w") as fh:
            json.dump(env, fh)

    def report(self):
        return handoff.validate_report(self.manifest, self.envelope)[0]

    def test_scaffold_envelope_is_exact(self):
        r = self.report()
        self.assertTrue(r["valid"], r["diagnostics"])
        self.assertEqual(r["candidate"]["contract_match"], "exact")
        self.assertEqual(r["candidate"]["producer"], {"kind": "script", "id": "spark-daemon scaffold"})

    def test_a_candidate_cannot_certify_itself(self):
        for key, value in (("battery_result", "PASS"), ("approved", True), ("verdict", "ok"),
                           ("class_c_ruling", "APPROVE")):
            with self.subTest(key=key):
                self.setUp()
                self.rewrite(lambda e: e.update({key: value}))
                r = self.report()
                self.assertFalse(r["valid"])
                self.assertTrue(any("carries no claims about itself" in d["message"] for d in r["diagnostics"]))

    def test_files_changed_after_sealing(self):
        with open(os.path.join(self.dir, "daemon.py"), "a") as fh:
            fh.write("\n# edited after the envelope was made\n")
        r = self.report()
        self.assertFalse(r["valid"])
        self.assertTrue(any("changed after the envelope was made" in d["message"] for d in r["diagnostics"]))

    def test_other_major_version_is_incompatible(self):
        other = f"{int(contract.CONTRACT_VERSION.split('.')[0]) + 1}.0.0"
        self.rewrite(lambda e: e["required_contract"].update(contract_version=other))
        r = self.report()
        self.assertFalse(r["valid"])
        self.assertEqual(r["candidate"]["contract_match"], "incompatible")

    def test_same_version_other_rules_is_a_mismatch(self):
        self.rewrite(lambda e: e["required_contract"].update(contract_sha256="0" * 64))
        r = self.report()
        self.assertFalse(r["valid"])
        self.assertEqual(r["candidate"]["contract_match"], "mismatch")

    def test_same_major_other_minor_is_a_warning(self):
        major = contract.CONTRACT_VERSION.split(".")[0]
        self.rewrite(lambda e: e["required_contract"].update(contract_version=f"{major}.99.0",
                                                            contract_sha256="1" * 64))
        r = self.report()
        self.assertTrue(r["valid"], r["diagnostics"])
        self.assertEqual(r["candidate"]["contract_match"], "compatible")
        self.assertEqual(r["warning_count"], 1)

    def test_bad_producer_kind(self):
        self.rewrite(lambda e: e["producer"].update(kind="oracle"))
        self.assertFalse(self.report()["valid"])

    def test_envelope_cli_round_trip(self):
        p = cli("envelope", "--dir", self.dir, "--producer-kind", "model", "--producer-id", "generator-v0 run 7",
                "--intent", "Watches the host name as a handoff round-trip test.")
        self.assertEqual(p.returncode, 0, p.stderr)
        r = self.report()
        self.assertTrue(r["valid"], r["diagnostics"])
        self.assertEqual(r["candidate"]["producer"]["kind"], "model")


class ValidateJsonTests(unittest.TestCase):
    def test_structured_diagnostics(self):
        p = cli("validate", "--json", "--manifest", os.path.join(FIXTURES, "opener", "manifest.json"))
        self.assertEqual(p.returncode, 1)
        r = json.loads(p.stdout)
        self.assertEqual(r["schema"], "spark-daemon-validate/1")
        self.assertEqual(r["diagnostics"], [{"layer": "purity", "severity": "error", "where": "daemon.py",
                                             "line": 5, "message": "call to open() is not allowed"}])

    def test_manifest_diagnostics_carry_the_field(self):
        tmp = tempfile.mkdtemp()
        try:
            data = load_json(os.path.join(EXAMPLES, "meminfo-watch", "manifest.json"))
            data["trigger"]["interval_seconds"] = 1
            data["commands"] = ["bash"]
            with open(os.path.join(tmp, "manifest.json"), "w") as fh:
                json.dump(data, fh)
            shutil.copy(os.path.join(EXAMPLES, "meminfo-watch", "daemon.py"), tmp)
            r, _ = handoff.validate_report(os.path.join(tmp, "manifest.json"))
            wheres = {d["where"] for d in r["diagnostics"]}
            self.assertEqual(wheres, {"trigger.interval_seconds", "commands[0]"})
            self.assertTrue(all(d["layer"] == "manifest" for d in r["diagnostics"]))
        finally:
            shutil.rmtree(tmp)

    def test_text_mode_is_unchanged_for_people(self):
        p = cli("validate", "--manifest", os.path.join(EXAMPLES, "meminfo-watch", "manifest.json"))
        self.assertEqual(p.returncode, 0)
        self.assertTrue(p.stdout.strip().endswith("RESULT: PASS"))


class PrecheckTests(unittest.TestCase):
    def precheck(self, manifest_path, *extra):
        p = cli("precheck", "--manifest", manifest_path, *extra)
        return p.returncode, json.loads(p.stdout)

    def test_every_reference_daemon_prechecks_ok(self):
        for name in EXAMPLE_NAMES:
            with self.subTest(example=name):
                code, r = self.precheck(os.path.join(EXAMPLES, name, "manifest.json"))
                self.assertEqual(code, 0, json.dumps(r["checks"]))
                self.assertEqual(r["result"], "OK")
                self.assertFalse(r["activation_evidence"])

    def test_impure_daemon_fails_fast_and_skips_the_run(self):
        code, r = self.precheck(os.path.join(FIXTURES, "opener", "manifest.json"))
        self.assertEqual(code, 1)
        self.assertEqual([c["state"] for c in r["checks"]], ["FAIL", "SKIPPED", "SKIPPED", "SKIPPED"])

    def test_policy_violation_is_caught_by_the_short_run(self):
        code, r = self.precheck(os.path.join(FIXTURES, "sneaky", "manifest.json"))
        self.assertEqual(code, 1)
        self.assertEqual(r["checks"][1]["state"], "FAIL")

    def test_precheck_never_says_pass(self):
        code, r = self.precheck(os.path.join(EXAMPLES, "meminfo-watch", "manifest.json"))
        self.assertNotEqual(r["result"], "PASS")


class ScaffoldTests(unittest.TestCase):
    def test_refuses_a_non_empty_folder_and_bad_names(self):
        tmp = tempfile.mkdtemp()
        try:
            with open(os.path.join(tmp, "keep"), "w") as fh:
                fh.write("x")
            with self.assertRaises(FileExistsError):
                scaffold.scaffold(tmp, name="hostname-watch")
            with self.assertRaises(ValueError):
                scaffold.scaffold(os.path.join(tmp, "new"), name="Bad Name")
        finally:
            shutil.rmtree(tmp)

    def test_scaffold_passes_validate_and_user_unit_variant(self):
        tmp = tempfile.mkdtemp()
        try:
            for unit in ("system", "user"):
                d = os.path.join(tmp, unit)
                scaffold.scaffold(d, name="a-very-long-scaffolded-daemon-name-x", unit=unit)
                r, m = handoff.validate_report(os.path.join(d, "manifest.json"), os.path.join(d, "candidate.json"))
                self.assertTrue(r["valid"], r["diagnostics"])
                if unit == "system":
                    self.assertLessEqual(len(m.run_as.user), 32)
        finally:
            shutil.rmtree(tmp)


class SymlinkStatTests(unittest.TestCase):
    """r3: ctx.stat judges a symlink by where it sits, not where it points, so a link planted
    in a watched folder is reported as a link instead of tripping fail-closed (78)."""

    def test_planted_symlink_is_a_link_not_a_violation(self):
        source = '''
def sense(ctx):
    info = ctx.stat("~/data/planted")
    return {"value": info.kind + " " + str(info.mtime_us > 0)}


def decide(prev, snapshot):
    return [("VALUE_OBSERVED", {"value": snapshot["value"]})]
'''
        tmp = tempfile.mkdtemp()
        try:
            shutil.copy(os.path.join(FIXTURES, "counter", "manifest.json"), tmp)
            with open(os.path.join(tmp, "daemon.py"), "w") as fh:
                fh.write(source)
            sb = Sandbox(tmp)
            try:
                os.symlink(os.path.join(sb.home, "spark-core", "data", "canary.txt"),
                           os.path.join(sb.home, "data", "planted"))
                p = sb.run(cycles=1)
                self.assertEqual(p.returncode, 0, p.stderr)
                observed = [r for r in sb.records() if r["event_type"] == "VALUE_OBSERVED"]
                self.assertEqual(observed[0]["payload"]["value"], "link True")
            finally:
                sb.cleanup()
        finally:
            shutil.rmtree(tmp)

    def test_following_reads_still_judge_the_target(self):
        tmp = tempfile.mkdtemp()
        try:
            shutil.copy(os.path.join(FIXTURES, "counter", "manifest.json"), tmp)
            with open(os.path.join(tmp, "daemon.py"), "w") as fh:
                fh.write('def sense(ctx):\n    try:\n        return {"value": ctx.read_text("~/data/planted")}\n'
                         '    except Exception:\n        return {"value": "blocked"}\n\n'
                         'def decide(prev, snapshot):\n    return [("VALUE_OBSERVED", snapshot)]\n')
            sb = Sandbox(tmp)
            try:
                os.symlink(os.path.join(sb.home, "spark-core", "data", "canary.txt"),
                           os.path.join(sb.home, "data", "planted"))
                p = sb.run(cycles=1)
                self.assertEqual(p.returncode, 78, p.stderr)
                self.assertNotIn("CANARY", sb.ledger_bytes().decode())
            finally:
                sb.cleanup()
        finally:
            shutil.rmtree(tmp)


class BatteryCatchesAlwaysFailingDaemonsTests(unittest.TestCase):
    def test_a_daemon_that_errors_every_cycle_fails_db04(self):
        # r2 passed the raiser fixture: its sense() fails every cycle in the battery's
        # workspace (no ~/data/mode.txt), yet the process exits 0 with a valid chain.
        work = tempfile.mkdtemp(prefix="battery-report-")
        try:
            p = cli("battery", "--quick", "--manifest", os.path.join(FIXTURES, "raiser", "manifest.json"),
                    "--workdir", work, timeout=300)
            self.assertRegex(p.stdout, r"DB-04\s+FAIL\s.*DAEMON_ERROR")
            self.assertTrue(p.stdout.strip().endswith("RESULT: FAIL"))
            # r3.3: the report names the code it ran, not only the manifest, even with no envelope.
            report = load_json(os.path.join(work, "battery-report.json"))
            with open(os.path.join(FIXTURES, "raiser", "daemon.py"), "rb") as fh:
                self.assertEqual(report["daemon_code_sha256"], hashlib.sha256(fh.read()).hexdigest())
        finally:
            shutil.rmtree(work)


if __name__ == "__main__":
    unittest.main()
