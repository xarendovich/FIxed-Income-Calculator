# UDC review pass 1 — subtraction, invariant ownership, and counterexamples

- **Status:** REVIEW FINDINGS. No runtime or contract change is authorized by this document.
- **Reviewed branch:** `Universal-Daemon-Contract-(UDC)`.
- **Baseline contract:** 5.0.1, `26e543fd8b36dd1d945be953c12d705c306adaa7cddf1cc2d24b893eff09dd16`.
- **Method:** read the active manifest, context, judge/report, runtime, semantics, handoff, digest/rendering, confinement and unit-generation paths; then ask the UDC charter's question: *if this mechanism disappears, what exact current guarantee fails?*
- **Bias:** prefer deletion/derivation over a new guard. Intentional independent enforcement survives only where its independence has a demonstrated safety benefit.

## 1. Result in one paragraph

The current observe-only core is materially simpler than the history that produced it, and most of the remaining complexity earns its place. The review found **one real structural mismatch** between a published invariant and the generic API, **one evidence-semantics overstatement**, and **three strong contract-6 simplification candidates**. It also found several things that look duplicative but should remain because deleting them would either lose current operational information or weaken an independent boundary.

The strongest theme is that UDC can probably become smaller by making the **profile identity** own fixed capabilities and by moving **authoring provenance** out of the runtime contract. It should not become smaller by deleting independent verification or OS enforcement.

## 2. Finding UDC-F1 — I-2 is not structurally true for `list_dir`

### The claim

I-2 says, in substance:

> Never record a guess: only an accepted cycle produces events; an unsettled, **truncated** or failed read abandons the cycle.

### The implementation

`Context.read_text` raises `TooLarge` when its byte limit is exceeded.

`Context.list_dir`, however, does something different: when it reaches `max_entries`, it returns a successful `Listing(names, truncated=True)`. The framework does **not** force the cycle to fail.

The shipped `dir-watch` correctly adds its own rule:

```python
listing = ctx.list_dir(...)
if listing.truncated:
    raise ctx.TooLarge(...)
```

The regression tests therefore prove that **dir-watch** handles the condition. They do not prove that the UDC API makes I-2 true for every conforming daemon.

A different otherwise-valid daemon can ignore `listing.truncated`, treat the partial names as a complete snapshot, emit an event, and still use only the published ctx API.

### Why this matters

This is the same lesson as HF-33 at the framework level: a successful partial payload creates an author-side obligation that the invariant claims the framework already owns.

It violates the compression rule: the fact “this result is too large to be complete” has two possible owners — ctx and every daemon author.

### Simplest resolution

For the next major API:

> **A bounded collection read is complete or it fails.**

Make `list_dir` raise `ctx.TooLarge` when another entry exists past the cap. A successful `Listing` is therefore always complete.

For migration, the existing `Listing.truncated` field can remain temporarily but is always false, then disappear in the next schema/API cleanup. Do **not** add an `allow_truncated` switch to the generic observe profile; that recreates the seam.

### Disposition

**ADOPT FOR CONTRACT 6 DESIGN.** Before declaring 5.0.1 a universal author contract, decide whether its I-2 wording is deliberately narrower than the API or whether this framework gap must be fixed sooner. The three shipped reference daemons are not evidence that arbitrary authors cannot ignore the flag.

This finding does not require a new invariant. It makes I-2 true by construction.

## 3. Finding UDC-F2 — startup is used as a blindness origin but is called an accepted observation

The published cycle text says that startup “counts as accepted.” The implementation is more nuanced:

- `DAEMON_START` supplies `first_start_utc` as the fallback origin of the blindness clock;
- `last_accepted_utc` remains `None` until an actual accepted cycle is later reflected by a daemon event, heartbeat or stop;
- immediately after a fresh `DAEMON_START`, `semantics.interpret` can report `observing` while `last_accepted_utc` is still `None`.

That conflates two facts:

1. **the grace/blindness timer has an origin**, and
2. **an observation was accepted**.

UDC's L2 law (“evidence cannot overstate”) says those should not share a name.

### Simplest logical model

- a fresh successful start **anchors the blind timer**;
- it is **not** an accepted observation;
- `last_accepted_utc=None` means exactly that no accepted observation is durably evidenced yet.

For status semantics, the conservative option is to report `blind` while there is no accepted-observation evidence, even if the process is alive. After an accepted zero-event cycle, external status may remain conservative until the next heartbeat persists `last_accepted_utc`; that is safer than claiming “seeing” without durable evidence.

### Disposition

**MODIFY WORDING NOW; EVALUATE STATE BEHAVIOR FOR 6.0.** Do not add a new state merely to explain the distinction unless a reviewer can show that `blind` is inadequate.

## 4. Simplification U-2 — move the candidate envelope out of UDC core

This review finds stronger evidence for the charter's U-2 question.

The envelope is optional:

- DB-24 returns `N/A` when no envelope is supplied;
- `N/A` does not prevent a battery PASS;
- `judge.conclusions` can qualify the report without a candidate envelope;
- the report already binds the actual manifest, daemon-code digest, contract identity, host and unit inputs.

Therefore the envelope is **not part of the runtime admission proof**. Its unique information is authoring provenance: producer kind/id, intent, and what contract/files the producer says it targeted.

That is useful, but its natural owner is `spark-daemon-author` / the surrounding development workflow.

### Contract-6 candidate

- remove candidate-envelope schema and DB-24 from the UDC runtime/judge contract;
- keep producer provenance as an authoring-side artifact if a workflow needs it;
- qualification records what was actually judged rather than trusting a producer's description of what it intended;
- do not add a new runtime provenance interface just to preserve the old envelope.

**Recommendation: ADOPT for 6.0 unless a current consumer can name a runtime/qualification guarantee that DB-24 uniquely enforces.**

## 5. Simplification U-1 — fixed capabilities belong to the profile, not the manifest

Three current manifest fields have one legal value:

- `daemon_class = observe`;
- `network.mode = none`;
- `trigger.kind = poll`.

These are not choices. They are facts about the active profile.

Keeping refused future enum values in today's manifest makes future power look like something that can be unlocked by configuration.

### Contract-6 candidate

The profile/contract identity implies:

```
observe-only
no network
polling
```

The manifest carries the **poll interval**, not a trigger-kind selector. A future networked/active/event-driven component is a different profile/contract and gets its own qualification evidence rather than a new allowed value in place.

**Recommendation: ADOPT IN PRINCIPLE for the 6.0 schema review.** `manifest_schema` itself should remain because it is the migration/version discriminator.

## 6. Simplification U-7 — `digest.enabled` and `digest()` duplicate one fact

The manifest declares `digest.enabled`, while the code independently may or may not define the optional `digest(snapshot, recent)` function.

Today:

- `digest.enabled=false` makes DB-10 N/A;
- `digest.enabled=true` **with no `digest()` function** also makes the digest probe N/A;
- runtime refreshes only when both the flag is true **and** the function exists.

So “does this daemon have a digest view?” has two owners.

### Simpler contract-6 shape

Let the **presence of `digest()`** own the feature.

If the function is absent, there is no digest view. If it is present, the manifest may carry only the bound that policy must supply, such as `digest.max_bytes` (or an optional digest block that exists only when the function exists).

Do not fix the mismatch by adding another validator relation if a representation can be deleted.

**Recommendation: ADOPT for 6.0 design; non-blocking for 5.0.1 hardware.**

## 7. U-3 result — keep the persistent digest view for now

The charter asked whether `digest.md` can be derived from ledger facts and removed from the resident core.

The reference implementations show why the answer is currently **no**.

Examples:

- `meminfo-watch`'s digest shows the current available memory, swap and PSI even while the band does not change and therefore no new daemon event is written;
- `disk-watch` shows the current free-space reading inside the same band;
- `dir-watch` shows current entry counts, total bytes and newest names that are not all ledger events.

Removing the digest would require one of two less-elegant substitutes:

1. write current snapshots into the authoritative ledger much more often, increasing durable data and changing event semantics; or
2. add a second live sensing/read path for presentation, creating another I/O/authority seam.

The current digest is a pure, bounded, non-authoritative projection of the current accepted snapshot plus recent records. That is a coherent role.

**Recommendation: KEEP.** Describe it explicitly as an **ephemeral operator projection/cache**, never a source of durable truth. Revisit only if another component already owns the exact same projection.

## 8. U-4 result — keep the three CLI names; they are aliases, not three contracts

`precheck`, `validate` and `battery` already share:

- one check registry;
- nested immutable check sets;
- one verdict rule;
- one report schema.

The architectural duplication has already been removed. Replacing the three useful author verbs with `judge --profile X` would mostly exchange familiar CLI names for a generic switch while deleting little code or authority.

Only `battery` has qualification significance, which is already derived by `judge.conclusions`.

**Recommendation: KEEP the aliases.** State in the UDC model that there is **one judge with three cost profiles**. Do not create a fourth generic CLI merely to expose that internal fact.

## 9. U-5 result — purity and the audit hook are not currently redundant

They are not the primary OS boundary, but they do different work.

### Purity

- rejects accidental/dynamic I/O surfaces before import;
- preserves the small reviewable author language;
- prevents import-time side effects and hangs;
- makes `sense/decide/digest`'s intended roles mechanically reviewable.

### Audit hook

- catches runtime policy attempts that escape the syntax check;
- records/fail-closes swallowed violations;
- blocks Python process-creation/spawn surfaces that Landlock's filesystem policy does not itself prohibit;
- DB-17 can turn it record-only specifically to prove Landlock independently.

The fact that Landlock remains the ultimate filesystem/network boundary does not make either layer useless.

**Recommendation: KEEP BOTH for the current UDC.** A future removal requires a replacement for the lost diagnostic/process-control property, not merely proof that Landlock still blocks file reads.

## 10. Candidate U-8 — user-unit support may be future-proofing without a consumer

The active reference manifests all use `run_as.unit = system`. The FirstBorn pilot also uses a system unit/user. The current unit generator nevertheless carries a second user-unit branch and warns that Ubuntu sandbox directives may depend on user namespaces.

The reviewed sources do not show a current qualified consumer that needs the user-unit mode.

This is exactly the kind of branch UDC should challenge.

### Review question

If no real consumer uses a user unit, should the first universal observe profile be **system-service only**, with a dedicated non-root service user? A later user-service profile would be added when a consumer proves the need and qualifies its different host-security assumptions.

**Disposition: CANDIDATE, NEEDS CONSUMER CENSUS.** Do not remove it from 5.x based only on the reference set.

## 11. Platform scope — do not abstract Linux/systemd/Landlock yet

“Universal” must not silently turn into “cross-platform”.

The current implementation is intentionally tied to Linux, Landlock and systemd semantics. There is only one qualified enforcement binding.

Creating an abstract OS-confinement interface now would be another registry/interface of one.

For this line:

> UDC is project-neutral, author-neutral and consumer-neutral **within its qualified Linux/systemd/Landlock host profile**.

A second enforcement implementation is the trigger for extracting a platform-neutral binding interface.

## 12. Additional logical inconsistency caught during the pass — CI's extractor test was version-fragile

The UDC branch's GitHub Actions run passed Python 3.10 and 3.11 but failed Python 3.12 in `test_tools.DaemonStartToolTests.test_a_bad_committed_line_is_a_clear_exit_not_a_traceback`.

The test used 3,000 nested JSON arrays as if that necessarily meant “unparseable”. CPython releases differ in how deeply `json.loads` can parse before raising `RecursionError`; on the 3.12 runner the JSON parsed, so the evidence extractor correctly did not take its parse-error path.

This was a **test assumption**, not a runtime/UDC defect. HF-43's deep nesting remains tested against the actual verifiers.

The branch test was changed to use syntax-invalid JSON for the extractor's deterministic promise: “a committed line that cannot parse exits 2 without a traceback.” The same edit closes two unclosed-file ResourceWarnings.

Commit: `7ff59ec8f59d34fa7c557efdb281b233a58091e3`.

The corrected branch still needs the workflow rerun before this pass calls the CI matrix green.

## 13. What the first pass says about the four-law model

No counterexample yet requires a fifth conceptual law.

- UDC-F1 and UDC-F2 both strengthen **L2 — Evidence cannot overstate**.
- removing fixed capability selectors strengthens **L1 — Capability cannot expand** by moving capabilities into profile identity;
- moving the envelope out of core simplifies **L3/L4** by separating author provenance from execution identity and authority;
- `runtime_bundle_sha256` still belongs under **L3 — Execution is identity-bound and bounded**;
- no finding requires a new authority invariant beyond **L4**.

The six contract invariants should remain during this pass. The four laws are currently a successful explanatory compression, not yet a replacement test map.

## 14. First-pass recommendation order

### Before DGX / software-baseline declaration

1. Decide UDC-F1: either make bounded directory listings all-or-fail before the universal baseline, or explicitly narrow I-2 and accept author-owned truncation handling. The former is the preferred design.
2. Correct the startup/accepted-observation wording from UDC-F2; separately decide whether status should stay conservative until accepted evidence exists.
3. Get the corrected CI matrix green.

### Contract 6 / post-hardware simplification work

4. Move candidate-envelope provenance out of UDC core (U-2).
5. Make fixed observe/no-network/poll properties profile-owned (U-1).
6. Make digest presence single-owned by `digest()` (U-7).
7. Bind `runtime_bundle_sha256` as the planned I-5 admission fix.
8. Decide user-unit support only after the consumer census.
9. Keep digest projection, judge aliases, purity/audit-hook diversity, two ledger verifiers and OS enforcement.

## 15. Reviewer challenge for pass 2

Pass 2 should attack the remaining **manifest and runtime state machine**, not add features:

- Which manifest fields influence enforcement versus only presentation/provenance?
- Can the blindness/status model use fewer semantic categories without overstating evidence?
- Is any lifecycle event derivable and therefore redundant?
- Are heartbeat and systemd watchdog duplicated, or do they prove different facts? (Current hypothesis: different facts; heartbeat is durable observation/blindness evidence, watchdog is process liveness.)
- Does any output artifact besides `digest.md` store data that can be derived from the ledger/report without loss?
- Can the host/deployment inputs be separated from observation semantics without creating a second configuration source?

The pass should again prefer a counterexample or deletion over a new abstraction.
