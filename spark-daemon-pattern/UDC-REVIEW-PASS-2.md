# UDC review pass 2 — manifest ownership, host binding, identity and state

- **Status:** REVIEW FINDINGS ONLY. No implementation, no contract edit, no Spark Core attachment.
- **Controlling pass-1 adjudication:** ADOPT WITH MODIFICATIONS. Pass 2 does not reopen heartbeat vs watchdog, lifecycle-event necessity, I-1..I-6, or the four-law reviewer model.
- **Reviewed line:** UDC branch after pass-1 review; contract remains 5.0.1 with the claimed digest unchanged by this review.
- **Method:** trace each active manifest/report/runtime identity field to the code that consumes it, then ask whether the field owns a current guarantee, host binding, operator presentation, or only historical/future taxonomy.

## 1. Result in one paragraph

Pass 2 finds that the current design should keep **one manifest with two deterministic readers**: the resident runtime/policy reader and the host-unit/qualification reader. Splitting host binding into a second manifest would recreate identity drift. The useful simplification is instead to delete fields that neither reader needs for a current guarantee.

Three deletion candidates are strong for contract 6: the manifest's authored `version`, the free-text `purpose`, and the redundant `DAEMON_START.qualified=true` conclusion. A fourth finding is more important than those deletions: the qualifying battery executes dynamic checks under `sys.executable`, while the projected unit may independently name another interpreter through the battery's `--python` input. That means the unit can run under an interpreter the dynamic battery did not exercise. This is an I-5 identity gap, not a reason for a new invariant.

The state machine otherwise resists further reduction: heartbeat and watchdog prove different facts; lifecycle events carry sparse-ledger facts that cannot be reconstructed; the existing status vocabulary remains useful once pass 1's startup-as-blind correction is applied in 6.0.

## 2. One manifest, two readers

Do not create a host manifest.

The current fields divide naturally by who consumes them:

| Field | Runtime / observation reader | Host / unit / qualification reader | Pass-2 disposition |
|---|---|---|---|
| `manifest_schema` | parser/migration | schema agreement | **Keep** |
| `name` | ledger identity | unit name/description | **Keep; shared identity** |
| `version` | no active semantic reader | no unit reader | **6.0 delete/move to author provenance** |
| `purpose` | none | systemd `Description=` only | **6.0 delete/move to author provenance** |
| `daemon_class` | fixed to observe | none | **6.0 delete; profile owns it** |
| `trigger.kind` | fixed to poll | none | **6.0 delete; profile owns it** |
| `trigger.interval_seconds` | scheduling/blindness | derived host timing evidence | **Keep** |
| `reads` | ctx/PathPolicy | unit read-only projection | **Keep; capability boundary** |
| `output_dir` | ledger/write boundary | unit write projection | **Keep; shared capability/host binding** |
| `network.mode` | fixed to none | unit network projection | **6.0 delete; profile owns it** |
| `run_as` | no observation semantics | service identity/unit kind | **Keep pending user-unit census** |
| `resources` | resource evidence only indirectly | unit limits + DB-14 | **Keep; host binding** |
| `cycle_budget_seconds` | runtime deadline | watchdog/start/stop timing derivation | **Keep; shared timing source** |
| `blind_limit_seconds` | blindness semantics | heartbeat/restart evidence | **Keep** |
| `ledger.record_max_bytes` | writer/verifier bound | battery/verifier | **Keep** |
| `ledger.event_types` | event admission | battery provenance | **Keep until real L2 trigger** |
| `digest.enabled` | duplicates code presence | digest probe | **6.0 delete per pass 1** |
| `digest.max_bytes` | bounded operator projection | digest test | **Keep when `digest()` exists** |

The simplification is therefore not “semantic config + host config.” It is:

> **one declared object, with field ownership made explicit and no duplicated representation.**

The runtime and unit generator read different subsets of the same digest-bound declaration.

## 3. UDC-F3 — the projected unit can use an interpreter the battery did not exercise

### Current shape

`battery.Workspace.cmd()` launches every dynamic run and probe with:

```
sys.executable
```

The judge separately binds unit input:

```
python = --python value, or /usr/bin/python3
```

and `unit_from_report` projects that path into `ExecStart=`.

Therefore this is possible:

```
battery process / dynamic checks: Python A
battery --python:              Python B
qualified unit ExecStart:      Python B
```

The report records the battery process's Python version under `environment.python`, but that value is not part of `host_differences()` and does not make the dynamic checks run under Python B.

### Why it matters

E-11/I-5 says the installable unit is the thing the battery judged. That is true for manifest/code/unit text, but not necessarily for the interpreter that executes the runtime.

The earlier extractor CI failure is a concrete reminder that Python releases can differ in parser behavior. That failure was only a test assumption, but it proves “same Python language” is not “same runtime behavior.”

### Simplest 6.0 direction

Prefer deletion over another synchronization rule:

> **The qualifying battery interpreter is the unit interpreter.**

Remove the independent unit-only `--python` choice from the qualifying path. The report records the exact interpreter path/identity used for the qualifying dynamic runs, and the unit projection reuses it.

If an operator wants `/usr/bin/python3`, they run the qualifying battery with that interpreter.

The 6.0 `runtime_bundle_sha256` design must explicitly decide whether interpreter identity is inside the bound runtime identity or a separately checked host/runtime fact. Do not leave it as an informational `environment.python` value.

### 5.0.1 hardware consequence

Do not patch the contract in this review. For the DGX evidence, record the interpreter used to run the qualifying battery and the interpreter in the generated unit's `ExecStart`; they should be the same path/runtime for the hardware claim “the unit tested is the unit run.”

**Disposition: OPEN I-5 residual for 6.0; no new invariant.**

## 4. UDC-U9 — manifest `version` is a third identity with no active owner

The manifest requires an `x.y.z` `version`.

In the active runtime/unit/judge/status paths reviewed here, no code reads `m.version`. Its practical effect is only that it participates in the canonical manifest digest.

Consequences:

- changing `version` forces a new manifest digest and requalification;
- no runtime behavior, capability, unit property, ledger semantic or consumer contract changes because of the value;
- exact implementation identity already exists as `daemon_code_sha256`;
- authoring/provenance versioning can live with the author-side metadata after U-2 removes the envelope from core.

This is identity duplication.

**Recommendation: DELETE from the 6.0 UDC core manifest.** If a project wants a human semantic version, the authoring/project metadata owns it. Do not replace it with another runtime field.

## 5. UDC-U10 — free-text `purpose` is presentation, not runtime identity

The manifest's `purpose` is not read by runtime, status, ledger verification or judge semantics. The active consumer is `unitgen.py`, where it becomes systemd `Description=`.

That means editing human prose changes:

- the manifest digest;
- qualification identity;
- the generated unit hash;

without changing observation logic, capability, resource limits, timing or event semantics.

The current validation restrictions on `purpose` exist because it is interpolated into a systemd unit, not because the daemon needs the text.

### Simpler 6.0 shape

- UDC core unit description derives from the stable `name`, e.g. “UDC observer <name>”.
- richer purpose/intent text belongs to the author/project metadata side with PD-62;
- no free text is needed in the security/qualification identity merely to decorate `systemctl status`.

This pairs naturally with U-2: producer intent/purpose leaves core together with the candidate envelope.

**Recommendation: DELETE `purpose` from the 6.0 core manifest unless a current consumer demonstrates a machine-semantic use.**

## 6. UDC-U11 — `DAEMON_START.qualified=true` stores a conclusion the start gate already proves

The runtime refuses a production start unless the expected manifest, daemon-code and contract digests are present and match. `DAEMON_START` is written only after that gate, Landlock setup, audit-hook installation and ledger recovery have succeeded.

The start record nevertheless stores:

```json
"qualified": true
```

No active reader in the reviewed tree uses that field to establish qualification.

So the field states a conclusion that is already entailed by the existence of a contract-5+ production `DAEMON_START`.

**Recommendation for 6.0:** delete the Boolean rather than keep another always-true proof-looking field. Exact identity belongs in digests, not a stored conclusion.

When `runtime_bundle_sha256` lands, also review whether `skeleton_version` still earns a durable-ledger slot or whether the exact runtime identity supersedes it. Do not remove the human-readable version in the same review unless its consumer value is checked.

## 7. Report duplication — one fact appears in both `host` and `environment`

The unified report currently records:

- `host.kernel`, `host.machine` from `host_facts()`;
- `environment.kernel`, `environment.machine` again;
- `environment.python`, `strace`, `systemd_analyze`.

Only `host` is used by the installer gate. `environment` is evidence/debug context.

The same kernel/machine fact has two owners in one report.

### Simplification candidate

Keep:

- one **host** object for qualification-relevant facts;
- one **tooling/evidence** object only for facts not already in host, such as optional test-tool availability.

Do not duplicate kernel/machine.

The interpreter question from UDC-F3 must be decided before deciding whether Python belongs in `host`, runtime identity, or tooling evidence.

**Disposition: 6.0 report cleanup candidate; no 5.x change.**

## 8. Host-binding fields should stay in the one manifest

Pass 2 specifically tested whether `run_as`, `resources` and related deployment properties should leave the manifest.

The answer is **not yet**.

They matter to the exact unit that qualification scores:

- `run_as` selects system/user service behavior and service identity;
- `memory_max_mb`, `tasks_max`, `cpu_weight`, `io_class` become host controls;
- `cpu_budget_bp` is checked by DB-14;
- `output_dir` and `reads` are simultaneously observation capability and unit confinement inputs.

Moving them into a second host file would force the report/unit/runtime path to bind two configuration identities and reopen the manifest-copy class of failure.

**Recommendation: KEEP one manifest.** If 6.0 reorganizes fields, it may group host-binding fields for readability, but grouping must not create a separately mutable object.

## 9. State-machine review — no additional state deletion is justified

Pass 2 does not reopen the pass-1 rulings:

### Heartbeat vs watchdog

They prove different facts.

- systemd watchdog: process/main-loop liveness, not durable evidence;
- `DAEMON_HEARTBEAT`: durable ledger evidence of recent process presence plus observation/blindness counters and accepted-observation lineage.

Neither can replace the other.

### Lifecycle events

On a sparse observation ledger, START/STOP/HEARTBEAT/ERROR/CLEARED/TAIL_QUARANTINED contain facts not derivable from domain events alone. Keep them.

### Status states

After the 6.0 F2 correction, the state vocabulary remains coherent:

- `never_started`;
- `stopped`;
- `not_watching`;
- `blind`;
- `observing`.

A successful start with no durable accepted observation is `blind`, not a new `starting` state and not `SENSE_BLIND`. `not_watching` remains distinct because it means no recent start/heartbeat vouches for the process.

No further state merge found.

## 10. Universality blocker to carry into the neutral-packaging review

The active path policy still contains Spark/tool-specific protected roots and names, including Spark data/governance paths and project/tool home directories. The CLI/unit/environment names are also Spark-branded.

This is not a reason to abstract the OS enforcement layer before DGX qualification. But it does mean “UDC” is not yet project-neutral packaging.

The neutralization review after hardware must distinguish:

1. **UDC profile safety rules** that every project receives;
2. **project policy** that exists because Spark owns particular sensitive paths;
3. branding/package names that should become neutral/configurable.

Do not solve this by adding a second policy manifest. The preferred solution should preserve one declaration and one PathPolicy source; any project-specific protection mechanism must prove why ordinary narrow `reads` plus owner review is insufficient before UDC gains another policy input.

**Disposition: POST-HARDWARE R-8/neutral-packaging review, not a 5.0.1 blocker.**

## 11. CI status after the pass-1 test repair

The PR #3 workflow at head `0709b5cb` is now green across Python 3.10, 3.11 and 3.12. For each matrix job:

- contract check: PASS;
- self-tests: PASS;
- reference-daemon batteries: PASS.

Per the pass-1 adjudication, this does **not** close the extractor's “parses but is not a record” outcome and is not HF-45 evidence. The extractor residual remains separate.

## 12. Pass-2 disposition table

| Item | Pass-2 recommendation |
|---|---|
| One manifest / two readers | **KEEP.** Do not create a host manifest |
| Interpreter chosen separately from battery interpreter | **OPEN I-5 gap; fix in 6.0 by single-owning interpreter identity** |
| Manifest `version` | **DELETE from 6.0 core; author/project provenance only** |
| Manifest `purpose` | **DELETE from 6.0 core; unit description derives from name** |
| `DAEMON_START.qualified=true` | **DELETE in 6.0; derivable conclusion** |
| Report kernel/machine duplication | **CONSOLIDATE in 6.0 report shape** |
| `run_as` / resources | **KEEP in the one manifest; user-unit census still open** |
| Heartbeat vs watchdog | **KEEP; distinct facts** |
| Lifecycle events | **KEEP; not derivable on sparse ledger** |
| Five status states | **KEEP; apply pass-1 startup-as-blind semantics in 6.0** |
| Spark-specific names/protected roots | **Carry to post-DGX neutral packaging review; no second policy manifest** |

## 13. Effect on the four-law model

No fifth law is needed.

- F3 is **L3 — execution is identity-bound and bounded**.
- U9/U10 remove identity/presentation data that do not own an L3 guarantee.
- U11 reinforces **L2/L3** by deleting a stored conclusion and keeping exact facts.
- One-manifest/two-readers preserves **L1** without opening a second mutable configuration.
- Nothing changes **L4 — authority cannot self-grant**.

I-1..I-6 remain the implementation invariants.

## 14. Pass 3 target

The next review should attack **identity/evidence payloads**, not host state:

- define the minimum exact identity that `DAEMON_START`, the qualifying report and the unit must share after `runtime_bundle_sha256`;
- determine whether `skeleton_version`, tool versions, contract digest and runtime-bundle identity can be represented once without losing human auditability;
- close or deliberately bound the extractor's parsed-non-record outcome without turning the extractor into a third verifier;
- census user-unit consumers;
- test whether any remaining free-text/provenance field changes qualification without changing a guarantee.

No implementation should begin from this document alone.
