# Consolidation pass: contract 5.0.1

- **Status:** REVIEW CANDIDATE. This pass removes documentation drift and closes non-critical design questions without widening the runtime.
- **Branch:** `review/v5-consolidation-501`
- **Parent line:** contract 5.0.0 / r4.12
- **Candidate contract:** **5.0.1**
- **Contract digest:** `26e543fd8b36dd1d945be953c12d705c306adaa7cddf1cc2d24b893eff09dd16`
- **Manifest-schema digest:** `955844b51162ebe250a61f83b5a06db7ae4eaacaea3909a957ca43c734e593db`
- **Runtime scope:** unchanged. `daemon_class: observe` only; no program execution, no network, no Memory write, no model in the resident daemon.

## 1. Why 5.0.1

5.0.0's enforcement model remains intact. The patch corrects claims that had drifted behind the implementation and removes ambiguous current terminology. No daemon that satisfied 5.0.0 fails because of 5.0.1.

The contract changes are:

1. **I-4 is now exact.** The ledger is the canonical local record, but the hash chain is verified **relative to the observed head**. Completeness across observations requires a head anchor outside the daemon output directory.
2. **Activation language is unambiguous.** Current normative text says **owner activation decision**, not “human Class C”. Historical documents may retain the old label as history.
3. **The sd_notify boundary is written down.** READY, STATUS, WATCHDOG and STOPPING carry host lifecycle/operator status only; they carry no observation payload, grant, outcome or authority. A ledger sequence displayed in STATUS is an ordering position, never a clock or authority input.
4. **Cycle-budget wording matches E-2.** The one runtime alarm is the in-process whole-cycle deadline. The removed per-call timeout design is no longer described as current behavior.

This is a PATCH change: published words and identities change; enforcement semantics do not.

## 2. Chain and anchor disposition

PD-58 does **not** create another daemon, service, protocol or invariant.

A future consumer attachment records a verified head in a storage/authority domain the daemon cannot modify. That receipt establishes the first externally held checkpoint. It can detect rollback or truncation **after** that checkpoint. It cannot retroactively prove the ledger was complete before the first attachment.

Therefore:

- “intact” means intact relative to the head being verified;
- “complete since checkpoint X” requires comparison to an externally held prior head;
- no consumer may silently upgrade the first observed head into proof of pre-attachment completeness.

## 3. Non-critical items closed or deferred

| Item | Consolidated disposition |
| --- | --- |
| E-4 denied-path outcome | **CLOSED as intentional layered asymmetry.** One PathPolicy owns policy; layers need not invent identical error labels when they have different knowledge |
| ADM/STB/LAT IDs | **Review matrices only.** They do not become another normative requirements hierarchy beside I-1..I-6 |
| V-2 signed reports | **DEFER.** Trigger: report crosses a trust boundary or feeds automated activation |
| V-3 `unit --report --check FILE` | **DEFER / non-critical.** Deterministic regeneration + digest/diff already answers the question |
| Hand-written independent JSON parser | **DO NOT BUILD now.** Continue bounds, exception normalization, independent canonical re-encoding and differential fuzzing; reopen only on a concrete uncontained common-mode fault |
| R-5c checkpoint acceleration | **DEFER by measurement.** Reopen only if DGX projection exceeds half the start timeout |
| Advise / Act classes | **Future taxonomy only.** Contract 5.x and planned 6.0 remain observe-only |
| Event-type/L2 registry | **DEFER by interoperability trigger.** An event string is a citation label, not a cross-implementation contract |
| R-8 neutral packaging | **TRIGGER MET, POST-HARDWARE.** FirstBorn is a real second project consumer; do not mix packaging into 5.0.1 qualification |

## 4. Event-type attachment rule

Today a consumer may cite one qualified implementation by:

`(manifest digest, daemon-code digest, event type)`

That is an **implementation citation**, not L2.

After contract 6 binds the runtime, the citation also includes the runtime-bundle digest.

A shared event contract is justified only when:

1. two independently qualified implementations intentionally claim the same semantic observation; **and**
2. a real consumer needs to switch between them without binding to either implementation.

Similar names, multiple projects, or shared lifecycle events do not satisfy that trigger by themselves.

## 5. Kernel alignment

KU-33 is narrowed to an owner-map clarification only:

> Resident observe-only components are owned by the daemon-pattern contract outside the Spark kernel. They are host-managed. The Spark kernel holds no daemon lifecycle state and takes no runtime dependency on them. Integration, if present, consumes typed verified facts through an external adapter rather than storage-format internals.

The kernel row does not name daemon CLI commands, define per-head attachments, authorize PD-40/58/61, create a provider interface, or sequence future integrations.

## 6. Hardware order

The exact **5.0.1** baseline is the gating hardware target.

If the existing FirstBorn 3.1.0 pilot naturally comes up during `fb update`, preserve its evidence as preliminary G-6 proof. Do not create a separate old-version qualification campaign just to remove it immediately afterwards.

Then:

1. migrate FirstBorn directly to 5.0.1;
2. run the full current suite and three reference batteries on the DGX as the service user;
3. create the unit from a qualifying report;
4. run under the real host systemd;
5. preserve the bundle required by `HARDWARE-GATE-DGX.md`;
6. collect R-5c timing as measurement only.

Status vocabulary:

- **SOFTWARE BASELINE FROZEN** — exact code/contract bytes fixed for qualification.
- **DGX QUALIFIED** — that exact baseline passed the hardware gate.

## 7. Next major change

V-1 remains the one substantive admission gap and is the planned **6.0.0** change.

Name the object `runtime_bundle_sha256`, not a generic repository/skeleton tree digest. It should cover the exact trusted code that can affect admission, confinement setup, ledger writing, interpretation and verification, expected to include:

- the runtime entrypoint;
- `spark_daemon/`;
- the independent verifier.

Tests, evidence, prose/PDFs, vendoring and authoring tooling stay outside that trusted bundle unless the 6.0.0 design demonstrates a runtime reason to include them.

Reuse the existing deterministic tree-hash mechanics. This strengthens **I-5** (“it runs only what was judged”); it does not create I-7.

## 8. Verification performed for this consolidation

The contract generator was exercised on the matching 5.0.0 source baseline with the 5.0.1 wording changes:

- `make check-contract`: PASS;
- `PublishedContractTests`: PASS for the available interpreter; the cross-Python identity test skipped because the local environment exposed fewer than two Python versions;
- `InvariantTests`: PASS;
- generated contract digest: `26e543fd8b36dd1d945be953c12d705c306adaa7cddf1cc2d24b893eff09dd16`;
- generated manifest-schema digest: `955844b51162ebe250a61f83b5a06db7ae4eaacaea3909a957ca43c734e593db`.

The full Landlock/systemd/DGX qualification is intentionally not claimed here.

## 9. Stop line

No additional registry, class, signing scheme, checkpoint service, parser subsystem or kernel-facing mechanism should be added before the 5.0.1 DGX qualification unless a new reproduced defect demonstrates that the current boundary cannot contain it.
