# Contract 5.0.0: verification pass (continuing the interim record)

- **Continues:** the interim independent verification (`Spark_Daemon_v5_Independent_Verification_Interim.md`, 2026-10-07). That pass verified the archive against GitHub, the commit chain, the contract identity, the 261-test inventory, the facts-only report and focused static suites. It could not run the Landlock-dependent half, because its sandbox had no Landlock (ABI −38) and no `strace`. It listed the next work as items 1 to 6, answered below.
- **Who:** Claude, at the owner's request, while the first reviewer was unavailable. **This pass is not independent:** Claude wrote the code under review. It is an adversarial self-review plus the dynamic reruns the first sandbox could not do. Its findings are reproduced, and its evidence is committed so the first reviewer can check it.
- **Environment:** this container has Linux 6.18 x86_64, **Landlock ABI 7**, `strace`, `systemd-analyze`, and Python 3.10, 3.11.15, **3.12.3 (the DGX version)** and 3.13.12. It is not the DGX, and nothing here ran as a service under a real host systemd (systemd as PID 1).
- **Revision:** 2026-10-07.

## Result in one paragraph

The dynamic half the interim pass could not reproduce does reproduce here, at the reviewed code (`5a6fdb5`, which equals `16b8db9` plus evidence). All 261 self-tests pass on Python 3.12.3 and 3.13.12. The four schema-agreement tests, which skip without `jsonschema`, pass in a 3.12.3 environment that has it. All three reference batteries PASS on both Pythons (21 PASS, DB-24 N/A). The adversarial review found **two reproduced defects**, both of the same kind: a corrupt ledger crashed a verifier instead of being reported as corrupt. They are fixed with regression tests (HF-43, HF-44). It also found **two design limits that need the owner** (residual risks 5 and 6 in `HARDENING.md`) and **two smaller observations**. Neither fix changes the contract: 5.0.0 and `2d080b40…` stand. Nothing found weakens confinement.

## The six items

### 1. Verifier diversity: real in encoding, not in parsing (HF-43, HF-44)

The two verifiers share no code. `verifier/ledger_verify.py` imports only `hashlib`, `json` and `sys`, and encodes RFC 8785 by hand where the primary uses `json.dumps`. But both parse with **CPython's `json.loads`**, and both run the same checks in the same order, as they must, since they implement one specification. Their diversity is in re-encoding and in the type checks (HF-38), not in parsing. A parser quirk is therefore a common-mode fault that DB-22 cannot see.

- **HF-43 (Medium), common mode.** A line nested about 3,000 levels deep (6 KB, inside the 8 KB record limit) raised `RecursionError` in both verifiers. A ledger with one such line made start-up exit 1 with a traceback, where the contract says 65. The unit restarts exit 1, so the daemon would restart in a loop instead of stopping for a person. `status` and `status --verify-only` crashed instead of reporting `CORRUPT`. Nothing on disk changed. Present since r2.
- **HF-44 (Low), found by a differential fuzz.** Twenty thousand random mutations of a real ledger, comparing the two verifiers' outcomes (crashes included), gave 39 disagreements. A payload key carrying a lone surrogate (a JSON `\ud800` escape) crashed the primary verifier's key sort (`UnicodeEncodeError`), while the independent one correctly reported `NOT_CANONICAL`. That is diversity doing its job. The effects were HF-43's, on the primary side. The write side was already safe: a daemon emitting such a key gets a failed cycle and nothing is written.
- **Fixes.** `canonical.strict_loads` turns `RecursionError` into a parse failure, and the key sort refuses a key with no UTF-16 form as `CanonicalError`. The independent verifier catches `RecursionError` in its own parser. Each fix lives in its verifier's own code.
- **After the fix:** 40,000 mutations over two seeds give zero disagreements and zero crashes (`evidence/v5/verification/fuzz-after-fix.txt`). A 1,500-mutation seeded version is now a self-test, and the before-fix output is `evidence/v5/hf43_44_before.txt`.
- **What the fuzz also showed (residual risk 5).** About 18% of mutations leave a chain that verifies as intact, and some with a head no original prefix had. These are edits to the *last* record, which nothing after it links to. A hash chain proves every record before its head and nothing more, so the last record can be changed, or the ledger cut back, undetectably until the head is anchored outside the output directory (PD-58, not built). The self-test now asserts exactly what the chain guarantees. I-4's words "verified two independent ways" should say *relative to the head*. Changing them changes the contract text, so it is proposed for **5.0.1**, not done here.

### 2. Qualification versus runtime admission: separated as designed; one gap (V-1)

| Predicate | Installer (`unit --report`) | Runtime (every start) |
| --- | --- | --- |
| The report is a full, complete battery PASS | yes (`judge.conclusions`) | no |
| The report was made on this host | yes (`qualify.host_differences`) | no |
| The unit is the one the battery judged | yes (reproduced from the report and checked against `unit_sha256`) | no |
| Manifest, `daemon.py` and contract equal the unit's digests | no | yes |
| Landlock available | no | yes |

No predicate appears in both columns, and none of the r4.11 gates is lost. Three observations:

- **V-1, design gap (residual risk 6).** The skeleton's own code is pinned by neither gate. The contract digest covers the contract's text, not the code that enforces it. With the audit hook stubbed out in `spark_daemon/guard.py` after qualification, a start carrying the qualified digests runs normally, and `describe --identity` is unchanged (`evidence/v5/verification/v1_skeleton_not_bound.py`, `v1-output.txt`). r4.11 had the same gap. **For the owner:** bind a skeleton tree digest (the vendoring tool's `tree_sha256` over `spark_daemon/` and `verifier/`) as a fourth expected digest. It would change the qualification contract, so it is not done here.
- **V-2, observation.** A report's facts are self-asserted JSON: whoever can edit the report can turn a FAIL into a PASS before `unit --report`. The r4.11 qualification record was exactly as editable. The real guard is the person who decides activation, plus the runtime's digests. Signing reports is a possible later step, not proposed now.
- **V-3, observation.** r4.11's `qualified --unit U --record R` could check a unit file someone handed you; v5 has no equivalent mode. The check is still possible: `sha256sum` the file and compare it with the report's `unit.unit_sha256`, or run `unit --report` again and diff. A `--check FILE` option would make it one command. Small; for the owner's list.

### 3. `PathPolicy`: symlink, ancestor and race edges hold

- **A declared read that is a symlink into a denied tree** (`~/data → ~/.ssh`) is refused by the manifest validator, which resolves symlinks: "reads[0]: ~/data is inside the denied path ~/.ssh". The runtime loads the manifest the same way at every start.
- **The same swap made after start-up**, while the daemon runs: the next read resolves to `~/.ssh/value.txt`, the Python check refuses it, and the daemon fails closed (exit 78, `POLICY_VIOLATION`). The secret is never read (`evidence/v5/verification/swap_probe.py`, `swap-probe.txt`).
- **The race between that check and the open (F-1)** is covered by the kernel. Landlock's grant is on the inode of the directory as it was at start-up, and no rule grants `~/.ssh`. Under systemd, `InaccessiblePaths=` covers it too.
- **Ancestors:** the no-gaps rule refuses a read that contains a denied path, a read inside one, and an output directory that contains a protected tree. `pathpolicy.agreement` checks every shipped manifest for all three, and checks that `denied()` refuses every denied path.

### 4. HF-41 and HF-42: closed independently

- **HF-41.** The root cause, a change to the battery process's environment, is gone with cut 3. `battery.Workspace` no longer touches `os.environ`. The judge fixes every unit input, the home included, before the workspace exists, and `PathPolicy`, `guard.Policy` and the unit generator all take the home explicitly. One operational note follows from the design, not a defect: the report binds the home of whoever ran the battery. So run the battery as the person whose `~` the manifest means, not under `sudo`, or the unit will name `/root`.
- **HF-42.** DB-11's socket now lives in a short private directory under `/tmp`, so its path no longer depends on the workspace. The clean-checkout rerun passed with deliberately long workspace paths, and the regression test runs with a 100-character directory name.

### 5. The cycle alarm after the per-command timeouts were removed

- It is armed for `sense`, `decide` and `validate` (which only prepares records), and disarmed in a `finally`.
- A signal that arrives just as it is being disarmed is raised inside the `try` that records `CYCLE_BUDGET_EXCEEDED`. It can never escape into the commit.
- The ledger commit (one `write`, then `fsync`) runs with the alarm off, so a commit is never cut short.
- The digest refresh re-arms it only for the time left. It never writes to the ledger: it reads the writer's head and replaces the digest file atomically, and a temp file left by an interrupted write is removed at the next start.
- Tests cover a busy loop, a blocking read and a swallowed alarm, all failing within the deadline, and an overrun counting as blind time.

### 6. The full suite and the batteries, Landlock enabled

| Run | Self-tests | Batteries |
| --- | --- | --- |
| `5a6fdb5`, Python 3.12.3 | 261 run, OK, 4 skipped (`jsonschema` absent); those 4 pass separately with `jsonschema` | dir-watch, disk-watch, meminfo-watch: PASS, 21 PASS + DB-24 N/A |
| `5a6fdb5`, Python 3.13.12 | 261 run, OK, 4 skipped (same 4) | the same three PASS |
| The fixes, Python 3.11.15 and 3.12.3 with `jsonschema` | **264 of 264, none skipped** (261 + the 3 new tests in `tests-verification.txt`); `fixed-selftests-*.txt` | the same three PASS on 3.12.3 (`fixed-battery-*.txt`); `make check-contract` up to date |
| The fixes, from a clean checkout (3f67f51), Python 3.12.3, long workspace paths | **264 of 264, none skipped** (`clean-selftests-python3.12.txt`) | three batteries PASS (`clean-battery-*`); contract up to date; clone clean (`clean-summary.txt`) |

The DGX run is still the one piece no container can stand in for: aarch64, its kernel's Landlock ABI, and the daemon running as a service under the real host systemd, with systemd as PID 1 (G-6; `HARDWARE-GATE-DGX.md`).

## For the owner

1. **Freeze 5.0.0 with HF-43 and HF-44 fixed?** The fixes change no contract rule, so the identity is unchanged.
2. **5.0.1 wording for I-4** ("relative to the head"), and a decision on **PD-58**, anchoring the head off the output directory.
3. **V-1:** bind a skeleton tree digest as a fourth expected digest.
4. **V-3** (`unit --report --check FILE`) and **V-2** (signed reports): keep, defer, or drop.
5. **The DGX run:** the full suite, the three batteries, and the first start under systemd.
