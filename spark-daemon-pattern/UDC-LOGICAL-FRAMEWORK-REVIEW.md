# Universal Daemon Contract (UDC): logical-framework review charter

- **Status:** REVIEW ONLY. This document does not change contract 5.0.1, unlock another daemon class, create a new invariant, or authorize a kernel/runtime integration.
- **Review baseline:** PR #2 as merged, plus UDC branch head `6a860d8027d20028ae46018ed6f5a73639b6f9c5`.
- **Current contract:** 5.0.1, `26e543fd8b36dd1d945be953c12d705c306adaa7cddf1cc2d24b893eff09dd16`.
- **Purpose:** help reviewers discover the smallest logical framework underneath the implementation, identify where history already taught us to delete rather than guard, and ask whether another layer or invariant can be removed without weakening a guarantee.
- **Constraint:** “universal” means portable across projects, authors and consumers. It does **not** mean “supports every capability.” The active profile remains observe-only.

## 1. End goal

The target is a small, neutral contract for a **resident fact producer**:

> A human, script or model can author a bounded resident observer; deterministic qualification proves the declared identity and limits; the host operating system enforces its capabilities; the component writes a durable local record of what it actually observed; consumers receive verified facts without inheriting daemon lifecycle, storage-format or activation authority.

The component is deliberately boring:

```
author
  |
  v
candidate -----> one judge / one facts report -----> owner activation decision
                                                        |
                                                        v
                                               host service manager
                                                        |
                                                        v
declared inputs --> observe-only resident component --> local ledger
                                                        |
                                                        v
                                              independent verification
                                                        |
                                                        v
                                                   verified facts
                                                        |
                                                        v
                                                     consumer
```

The Spark kernel is not in this runtime loop. It may consume facts through an external adapter later; it does not own, schedule or parse the daemon.

## 2. Scope boundary

### UDC owns

- the author-visible manifest/contract;
- the small observation API exposed to `daemon.py`;
- deterministic authoring/qualification checks;
- the fixed runtime skeleton;
- host confinement projections;
- cycle/blindness/lifecycle behavior;
- one append-only local ledger writer;
- read-only verification and interpretation;
- evidence sufficient to reproduce the unit that was judged.

### UDC does not own

- activation authority;
- Spark Core orchestration or state;
- model inference or agent decisions;
- Tool invocation;
- Memory writes;
- outbound networking;
- command execution;
- event routing or subscription;
- a global event-schema registry;
- external head-anchor storage;
- signing/key custody;
- generic emergency or Act/Advise behavior;
- project-specific install/update workflows.

A proposal that moves one of those into UDC must first prove why its existing owner cannot hold it.

## 3. The compression rule discovered by the history

The strongest recurring design rule is:

> **One fact, one owner. Multiple implementations are justified only when independence itself buys safety.**

Examples:

- one manifest identity, not a rewritten battery copy;
- one cycle deadline, not command timeout plus alarm;
- one judge registry and one report shape, not four report schemas;
- one `PathPolicy`, projected into several independent enforcement layers;
- one interpreter, but two independent ledger verifiers because diversity found real defects;
- one owner activation decision, not battery PASS plus an implicit second authority;
- one local ledger writer; external completeness belongs to a consumer-held receipt, not another writer.

A reviewer should treat every duplicated representation or duplicated decision as suspicious unless its independence has already found a class of defects.

## 4. How earlier problems led to the current design

| Historical pressure | Earlier response | What we eventually learned | Current simplification |
| --- | --- | --- | --- |
| Git/child execution produced repeated escape and hardening defects | more command checks, timeouts, Git-specific rules | the generic observer did not need process execution | **R-2:** remove commands, `proc.py`, loaders and execute grants from the core |
| Manifest `deny` duplicated the allow policy | reconcile allow + deny | a redundant negative list added appearance, not capability | remove `deny`; one positive read policy |
| Whole-cycle deadline and per-command deadline collided (HF-40) | cleanup hardening | two enforcers of one deadline create a race | one runtime cycle alarm; watchdog is an outer backstop |
| validate/precheck/battery/qualification had separate result shapes | synchronize schemas | the result is the same kind of fact at different costs | one check registry, nested profiles, one facts-only report |
| battery rewrote the manifest | bind both identities | two copies of one identity drift (HF-37/HF-39) | battery runs the original manifest; harness parameters stay external |
| broad test mode relaxed production rules | guard the switch | a production-visible bypass is a permanent seam | explicit harness with recorded parameters; no ambient test authority |
| Python, Landlock and systemd each expressed path policy | compare gaps | diverse enforcement is useful; diverse policy invention is not | one `PathPolicy`, several enforcement projections |
| checkpointed startup looked attractive | design an anchor | no performance problem and no trustworthy anchor existed | full verification; R-5c only after a measured trigger |
| hash-chain language implied completeness | consider a new anchoring subsystem | the actual guarantee is relative to the observed head | correct I-4; first consumer receipt establishes the first external checkpoint |
| class terminology collided across projects | import/split class vocabularies | the current decision is simpler than the taxonomy | say **owner activation decision**; keep future classes historical/non-normative |

The pattern improved most when it **deleted a capability, copy, schema, switch or duplicated decision**, not when it added another guard.

## 5. Four candidate laws underneath the six current invariants

Contract 5.0.1 still publishes six testable invariants. Do not change them during this review. Instead, ask whether they are implementation facets of four smaller laws.

### L1 — Capability cannot expand

The running component cannot acquire an effect that the contract did not declare.

Covers most of current I-1 and the capability side of I-5.

Examples: no program, no network, declared reads only, output directory only, one canonical path policy, host kernel enforcement.

### L2 — Evidence cannot overstate

The system never converts uncertainty, failure, truncation, corruption or absence into a stronger fact than it actually possesses.

Covers I-2, I-3 and I-4.

Examples: no event from an unsettled/failed cycle; blindness becomes visible; corrupt ledgers fail closed; “verified” is relative to the observed head; first attachment does not invent prior completeness.

### L3 — Execution is identity-bound and bounded

The thing that runs is the thing that was judged, and it cannot run forever outside its declared resource/time envelope.

Covers the identity half of I-5 plus I-6.

Contract 6's planned `runtime_bundle_sha256` belongs here/I-5; it should not create a seventh invariant.

### L4 — Authority cannot self-grant

A producer, candidate, report, daemon event or consumer observation cannot promote itself into permission to activate or act.

Covers the authority half of I-5 and the standing preamble.

The battery produces evidence. The owner makes the activation decision. A future agent can produce a proposal only under its own separately authorized contract.

### Review challenge

Try to reduce I-1..I-6 to these four laws **only if every existing check/test still has one obvious normative parent**. If a merge makes the evidence map less clear, keep the six implementation invariants and use the four laws only as the conceptual model.

The goal is not a smaller number on paper. The goal is fewer independently mutable rules.

## 6. The simplest possible UDC profile

A useful thought experiment is to ask what fields exist only because we imagined future capabilities.

Contract 5.0.1 currently carries several single-valued choices:

- `daemon_class` can only be `observe`;
- `network.mode` can only be `none`;
- `trigger.kind` can only be `poll`.

These are legitimate migration hooks, but they also preserve future taxonomy inside the current manifest.

**Simplification candidate U-1 (major-version only):** let the contract/profile identity imply “observe + no network + poll.” The manifest would carry only the poll interval, not three fields whose other values are refused. A future active/networked/event-driven resident component would use a different profile/contract rather than unlock a reserved enum in place.

Do not implement U-1 in 5.x. Ask whether the future compatibility value of those fields is worth their current conceptual surface.

## 7. Other high-value deletion questions

### U-2 — Is the candidate envelope core or authoring metadata?

The qualifying report already binds the manifest, code, contract and unit inputs. The optional candidate envelope carries producer provenance and intent but no authority.

Question: should the envelope remain part of the UDC core contract, or move wholly to `spark-daemon-author` / authoring provenance in the next major?

Keep it only if a runtime/qualification invariant actually needs it.

### U-3 — Is the persistent digest file core or a derived view?

`digest.md` is explicitly non-authoritative. It adds a callback, rendering rules, size bounds, atomic replacement and another file in the output directory.

Question: can every useful operator view be derived from verified ledger/status facts? If yes, move presentation outside the resident core. If no, identify the information that exists only in the digest and why recording it as facts would be worse.

Do not remove it merely for line count; require a complete replacement path for its useful information.

### U-4 — Are three author-facing judge commands necessary?

The implementation already has one registry, nested profiles and one report shape.

Question: are `precheck`, `validate`, and `battery` three real contracts, or one `judge(profile)` with author-friendly aliases? This may be a documentation/CLI simplification rather than an architectural change.

Only `battery` has qualification significance.

### U-5 — Do purity and the audit hook both earn their cost?

They are tripwires/diagnostics; Landlock + systemd are the actual capability boundary.

Question: what guarantee would be lost if either tripwire were removed or moved entirely to qualification? Do not remove them unless kernel-level containment plus remaining diagnostics preserve the same failure visibility. This is a review target, not a recommendation to weaken defense-in-depth.

## 8. Things that are already near-minimal and should be hard to remove

A reviewer should assume these survive unless a concrete replacement is simpler **and** preserves the evidence:

- one manifest as the declaration/policy source;
- one local writer;
- two independent ledger verifiers (HF-38/HF-44 justify diversity);
- one interpreter;
- one canonical path policy with independent OS enforcement layers;
- one facts-only qualification report;
- deterministic unit projection;
- owner activation separate from qualification;
- full startup ledger verification until measurement proves it is a problem;
- no command execution/network/model/Memory write in the observe profile.

## 9. What “universal” should mean

UDC should be universal in **placement and protocol**, not universal in power.

A useful definition:

> Any project can host the same contract and qualification logic for a resident fact producer without adopting Spark-specific authority, paths, names or lifecycle ownership.

That points toward neutral packaging after DGX qualification, but it does **not** require generalizing the current observer into a tool runner, advisor, actuator or network service.

Future capability should be added by a new profile only after a real consumer proves that the observe profile cannot satisfy the use case.

## 10. Reviewer questions

1. Can one of the current six invariants disappear because it is merely a facet of L1-L4, while every test retains an obvious owner?
2. Which current seam exists only to preserve a future capability we do not have?
3. Which current artifact stores a conclusion that could instead be derived from facts?
4. Where do we represent the same identity, policy, deadline or verdict twice?
5. Which security layer is intentionally independent, and has that independence found a real defect? If not, is it redundant?
6. Which field in the manifest has only one legal value? Should contract/profile identity imply it instead?
7. Does any UDC core mechanism exist solely for an authoring convenience or presentation view?
8. Can a future consumer attach without knowing a CLI command, file name or project-specific path?
9. Can a new project vendor/package UDC without importing Spark authority or names?
10. If the proposed simplification fails, what exact guarantee fails? If no guarantee can be named, why does the mechanism exist?

## 11. Acceptance standard for a simplification

A proposed simplification is better only when it:

1. deletes a representation, authority, capability, or independently mutable rule;
2. leaves the remaining owner obvious;
3. preserves or improves fail-closed behavior;
4. does not replace one seam with a hidden cross-project dependency;
5. keeps qualification reproducible;
6. requires no new invariant merely to explain the simplification.

Prefer deletion over indirection; prefer derivation over synchronized copies; prefer a real trigger over a future-proof registry.

## 12. Stop line

This review may recommend a 6.0 simplification plan, but it must not modify the 5.0.1 hardware candidate.

The immediate execution line remains:

```
5.0.1 exact bytes
    -> software baseline freeze
    -> DGX / real-host-systemd qualification
    -> neutral packaging review
    -> 6.0 admission/runtime-bundle design
```

Any finding that demonstrates 5.0.1 cannot safely reach hardware qualification is an exception and must be reproduced as the smallest failing fixture.
