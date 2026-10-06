# Design review package (r4.11): fewer seams, fewer invariants

- **For:** reviewers (people or models). This document stands alone; every file it names is in `spark-daemon-pattern/` on branch `claude/clever-bardeen-qqkxez`.
- **The ask:** find design simplifications that **remove a seam or an invariant without weakening any guarantee in §3**. We would rather have one elegant removal than ten new checks.
- **Status:** FOR REVIEW. Nothing here is adopted. The candidates in §5 (E-1 to E-9) are ours, offered to be beaten.
- **Revision:** r4.11, 2026-10-06, by Claude. Code candidate `926720f`; contract 4.0.0 (`dfdb9f1b…`).
- **Evidence:** 267 of 267 self-tests, none skipped; the four reference daemons pass all 22 battery checks (`evidence/r4.11/`). The rulings as built are recorded in `ADJUDICATION-R4.9-OWNER-IMPLEMENTATION.md`.

**How to mark:** for each seam (§4) and candidate (§5), say KEEP, SIMPLIFY (how) or REMOVE, and name the evidence type: reproduced, measured, traced to code, or opinion. Then add your own candidates. A good candidate names:
- the seam or invariant it removes;
- the guarantee in §3 that still holds without it, and why;
- what would make it wrong.

## 1. What r4.11 built, and what it taught

The owner's rulings on R-1 to R-8 are built, except R-5c, which is gated because no checkpoint anchor exists. In one line each:

| Ruling | Built as |
| --- | --- |
| R-7 | One judge (`judge.py`): validate, precheck and battery are profiles of one check registry with one verdict rule |
| R-3 | An installable unit comes only from a qualifying battery PASS, with a qualification record. Two gates: `qualified` for the installer, and the runtime's digest check |
| R-1 | The manifest's `deny` list is gone; no read may contain an always-denied path |
| R-2b | Execute only on declared executables and their loaders. A command-free daemon can run nothing. One residual: the loader can be run on another readable binary |
| R-6 | `cycle_budget_seconds`: one deadline per cycle, enforced by `ctx` timeouts and an alarm; unit timings are derived from it |
| R-4 | One interpreter (`semantics.py`) for start-up and `status`; two independent verifiers, compared by DB-22 |
| R-8b | A rename map and a three-hash binding for vendored copies |

**Three defects were found this round. Each sat on a seam, and each says something about simplifying:**

| Defect | The seam it sat on | What it suggests |
| --- | --- | --- |
| **HF-38** (Low): a record with `"seq": true` verified as seq 1 (`True == 1` in Python) | Between the writer and its readers. Both verifiers had the blind spot until the second one was written | Diversity found it. Keep verification diverse, and keep interpretation single (E-8) |
| **HF-39** (Low): the battery report named its rewritten workspace copy of the manifest, not the manifest that would run | Between the manifest as written and the battery's copy (the same seam as HF-37 in r4.7) | Two copies of one identity drift. Remove the copy rather than guard it (E-3) |
| **HF-40** (Low): a command could outlive its cycle, because the alarm fired inside `proc.run`'s cleanup | Between two enforcers of one budget: `proc`'s timeout and the cycle alarm | Two enforcers of one rule create a seam between them. Choose one (E-2) |

## 2. The design today, by count

| What | Count | Notes |
| --- | --- | --- |
| Package code | 6,115 lines in 24 modules (plus 199 in the independent verifier and 131 in the vendoring tool) | The largest are `battery.py` 799, `runtime.py` 584, `manifest.py` 494, `contract.py` 400 |
| Schemas a reader may meet | 12 | ledger/1, manifest/3, contract/1, candidate/1, validate/2, precheck/2, battery/2, qualification/1, status/1, independent-verifier/1, rename-map/1, vendor-binding/1 |
| CLI commands | 15 | 12 public (describe, schema, scaffold, envelope, validate, precheck, unit, run, verify, status, battery, qualified) and 3 internal probes |
| Battery checks | 22 | DB-01 to DB-18, DB-20, DB-22, DB-24, DB-25 (DB-19, 21, 23 reserved) |
| Seams named for review | 11 | §4 |
| Invariants a reviewer must hold the design to | 8 owner invariants, plus 9 core invariants proposed in r4.9 | §3; they overlap (E-8) |
| Behaviours test mode relaxes | 7 | Poll interval, blind limit, cycle budget, audit mode, jitter, tolerance of missing Landlock, and unqualified starts; refused under systemd (E-6) |
| Self-tests | 267 | |

## 3. Guarantees that must not weaken

These are the fixed points. A simplification that weakens one is not a simplification.

1. A battery PASS is qualification evidence, not permission to activate.
2. No daemon class above `observe` is unlocked.
3. A command declaration is an allowlist, not a Boolean switch.
4. Start-up and `status` share interpretation; corruption and tamper detection stays diverse.
5. Integrity uncertainty is always visible, never rendered as healthy.
6. `cycle_budget_seconds` constrains the whole cycle.
7. Activation is a separate authority decision after qualification.
8. Nothing relaxes confinement, blindness, authentication, or proposal-versus-authority.

The owner's rulings that bound the design also stand:
- **PD-63:** the blind period; exit 78, never restarted.
- **PD-70:** blindness survives restarts.
- **PD-01.9:** the Blind Forester, outside observe-only daemons.
- **Break Glass:** never for internal systems, and "an AI model cannot unilaterally decide that an emergency exists".

## 4. The seams in r4.11

<figure>
<svg viewBox="0 0 980 300" role="img" aria-label="The r4.11 pipeline from manifest to judge, unit, runtime, ledger and status, with the eleven seams numbered and the three where this round's defects were found marked." style="max-width:100%;height:auto" font-family="inherit" font-size="12">
<defs>
<marker id="sm-a" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="currentColor"/></marker>
</defs>
<g fill="none" stroke="currentColor" stroke-width="1.4">
<rect x="20" y="40" width="130" height="52" rx="5"/>
<rect x="180" y="40" width="130" height="52" rx="5"/>
<rect x="340" y="40" width="130" height="52" rx="5"/>
<rect x="500" y="40" width="130" height="52" rx="5"/>
<rect x="660" y="40" width="130" height="52" rx="5"/>
<rect x="820" y="40" width="140" height="52" rx="5"/>
<rect x="180" y="205" width="130" height="52" rx="5"/>
<rect x="380" y="205" width="130" height="52" rx="5"/>
<rect x="525" y="205" width="130" height="52" rx="5"/>
<rect x="700" y="205" width="130" height="52" rx="5"/>
</g>
<g fill="currentColor" text-anchor="middle">
<text x="85" y="62">manifest.json</text><text x="85" y="80" font-size="11">as written</text>
<text x="245" y="62">judge</text><text x="245" y="80" font-size="11">battery profile</text>
<text x="405" y="62">unit + record</text><text x="405" y="80" font-size="11">qualified</text>
<text x="565" y="62">runtime</text><text x="565" y="80" font-size="11">start gate · cycle</text>
<text x="725" y="62">ledger.jsonl</text><text x="725" y="80" font-size="11">primary verifier</text>
<text x="890" y="62">status</text><text x="890" y="80" font-size="11">shared interpreter</text>
<text x="245" y="227">workspace copy</text><text x="245" y="245" font-size="11">rewritten manifest</text>
<text x="445" y="227">ctx · proc</text><text x="445" y="245" font-size="11">time left, timeouts</text>
<text x="590" y="227">kernel</text><text x="590" y="245" font-size="11">Landlock · alarm</text>
<text x="765" y="227">independent</text><text x="765" y="245" font-size="11">verifier</text>
</g>
<g stroke="currentColor" stroke-width="1.4" fill="none">
<line x1="150" y1="66" x2="178" y2="66" marker-end="url(#sm-a)"/>
<line x1="310" y1="66" x2="338" y2="66" marker-end="url(#sm-a)"/>
<line x1="470" y1="66" x2="498" y2="66" marker-end="url(#sm-a)"/>
<line x1="630" y1="66" x2="658" y2="66" marker-end="url(#sm-a)"/>
<line x1="245" y1="92" x2="245" y2="203" stroke="#e8590c" marker-end="url(#sm-a)"/>
<line x1="545" y1="92" x2="450" y2="203" stroke="#e8590c" marker-end="url(#sm-a)"/>
<line x1="595" y1="92" x2="590" y2="203" marker-end="url(#sm-a)"/>
<line x1="735" y1="92" x2="760" y2="203" stroke="#e8590c" marker-end="url(#sm-a)"/>
<line x1="830" y1="231" x2="880" y2="94" marker-end="url(#sm-a)"/>
<path d="M565,40 C565,6 890,6 890,38" marker-end="url(#sm-a)"/>
<path d="M685,92 C685,140 610,140 610,94" stroke-dasharray="4 3" marker-end="url(#sm-a)"/>
</g>
<g font-size="11" text-anchor="middle">
<g fill="none" stroke-width="1.4">
<circle cx="325" cy="66" r="10" stroke="currentColor" style="fill:var(--panel,#fff)"/>
<circle cx="485" cy="66" r="10" stroke="currentColor" style="fill:var(--panel,#fff)"/>
<circle cx="245" cy="148" r="10" stroke="#e8590c" style="fill:var(--panel,#fff)"/>
<circle cx="497" cy="148" r="10" stroke="#e8590c" style="fill:var(--panel,#fff)"/>
<circle cx="592" cy="130" r="11" stroke="currentColor" style="fill:var(--panel,#fff)"/>
<circle cx="592" cy="170" r="11" stroke="currentColor" style="fill:var(--panel,#fff)"/>
<circle cx="747" cy="148" r="10" stroke="#e8590c" style="fill:var(--panel,#fff)"/>
<circle cx="855" cy="162" r="10" stroke="currentColor" style="fill:var(--panel,#fff)"/>
<circle cx="727" cy="15" r="10" stroke="currentColor" style="fill:var(--panel,#fff)"/>
<circle cx="647" cy="128" r="10" stroke="currentColor" style="fill:var(--panel,#fff)"/>
</g>
<text x="325" y="70" fill="currentColor">6</text>
<text x="485" y="70" fill="currentColor">10</text>
<text x="245" y="152" fill="#e8590c" font-weight="600">8</text>
<text x="497" y="152" fill="#e8590c" font-weight="600">2</text>
<text x="592" y="134" fill="currentColor">1</text>
<text x="592" y="174" fill="currentColor">11</text>
<text x="747" y="152" fill="#e8590c" font-weight="600">4</text>
<text x="855" y="166" fill="currentColor">4</text>
<text x="727" y="19" fill="currentColor">3</text>
<text x="647" y="132" fill="currentColor">5</text>
<text x="262" y="185" fill="#e8590c" text-anchor="start">HF-37, HF-39</text>
<text x="420" y="175" fill="#e8590c" text-anchor="end">HF-40</text>
<text x="770" y="185" fill="#e8590c" text-anchor="start">HF-38</text>
</g>
</svg>
<figcaption>Where the seams sit in r4.11. Numbers match the table below; orange marks the three where this round's defects were found. Seam 3 is the shared interpreter (start-up and status), 4 the two verifiers feeding it, and 5 the checkpoint R-5c would add (dashed: not built). Seams 7 (vendoring) and 9 (test mode) are not drawn.</figcaption>
</figure>

| # | Seam | What each side assumes | What enforces it today | Found this round |
| --- | --- | --- | --- | --- |
| 1 | Manifest `commands` → kernel execute grant | Every declared command resolves to one ELF file with one loader | `landlock.execute_paths`; `test_landlock.py` | The loader residual (R-2b) |
| 2 | Cycle budget → `ctx`/`proc` timeouts → alarm → watchdog → stop timeout | Exactly one thing ends an overrunning cycle, cleanly | `context._remaining`, the runtime alarm, `proc`'s timeout; derived unit timings; `test_budget.py` | **HF-40** (the two enforcers met) |
| 3 | Shared interpreter → start-up and `status` | Both feed it the same records and clock readings | `semantics.py`; `test_independent.py` | — |
| 4 | Two verifiers → interpreter | Only an intact chain is interpreted; the verifiers agree | `Verified.from_facts`; DB-22 | **HF-38** (a shared blind spot) |
| 5 | Full verifier → checkpoint → start-up | (not built: no anchor) | Full start-up verification | — |
| 6 | Battery result → unit generator | The unit judged is the unit emitted | Unit options are battery inputs; DB-15 and DB-25 | — |
| 7 | Source tree → rename map → copy | The copy is the source under the map | `vendor.py --check` | — |
| 8 | Manifest as written ↔ battery workspace copy | Both name one identity | Reports bind the original; run checks use the copy | **HF-39** (and HF-37 in r4.7) |
| 9 | Test mode ↔ production | The seven relaxations never reach a service | `INVOCATION_ID` refusal; overrides recorded in `DAEMON_START` | — |
| 10 | Qualification record ↔ unit ↔ runtime gate | Two gates check overlapping things: unit bytes and host (installer), code and contract (runtime) | `qualify.check`; the runtime's digest check | — |
| 11 | Python deny checks ↔ Landlock ↔ the unit's `InaccessiblePaths=` | They express one policy | Validation (no gaps) makes them agree on paths as written; a symlink created later is caught by Python (exit 78) and by Landlock (`EACCES`), with different outcomes | — |

## 5. Candidate simplifications

Ordered by what they would remove. Each one is ours and unadopted, and each says what would make it wrong.

<details markdown="1" open>
<summary>E-1. One report shape, and no separate qualification record</summary>

**Today:**
- R-7 unified the judge, but its three profiles still write three report schemas (validate/2, precheck/2, battery/2).
- R-3 added a fourth artifact, the qualification record (qualification/1).

**Proposal:**
- One report schema for every profile, with `profile` and `qualifying` fields. They already exist.
- The battery report itself becomes the qualification evidence. It already binds the manifest and code digests, the contract, the unit options and the host facts.
- The installer gate then regenerates the unit from the report's inputs and compares bytes: `qualified --unit U --report R`.

**Removes:** three schemas and one artifact, and narrows seam 10.

**What would make it wrong:** a report that must stay readable by consumers of the old shapes, or a reason to keep qualification evidence smaller than a report.
</details>

<details markdown="1" open>
<summary>E-2. One enforcer for the cycle budget</summary>

**Today:** HF-40 came from two enforcers of one deadline:
- `proc`'s own timeout, fed the time left by `ctx`;
- the cycle's alarm.

**Option A (proposed):** the alarm is the only deadline.
- `proc.run` takes no timeout. It guarantees, as it now does, that the process group dies on any exit path.
- `ctx._remaining` disappears.

**Option B:** keep both, as now.

**Removes (A):** half of seam 2, a parameter on every `ctx` call, and the race HF-40 sat in.

**What would make A wrong:** a blocking call the alarm cannot interrupt where a timeout could, for example a read in uninterruptible sleep. But neither mechanism helps there; the watchdog does.
</details>

<details markdown="1" open>
<summary>E-3. The battery never rewrites the manifest</summary>

**Today:** the battery copies the manifest and rewrites an absolute `output_dir` into its workspace, so two digests exist for one daemon. That seam has produced two defects (HF-37 and HF-39).

**Proposal:** run the battery's children with a test-mode output root instead. One override, recorded in `DAEMON_START` like the others, maps `output_dir` under the workspace. The manifest is never copied, and one digest is used everywhere.

**Removes:** seam 8 and its class of defect.

**What would make it wrong:** it adds a test-mode relaxation (see E-6), and a path mapping that a confused deployment could misuse. It is refused under systemd like every override.
</details>

<details markdown="1">
<summary>E-4. One outcome for a denied path</summary>

**Today:** validation forbids gaps, so for paths as written the Python deny checks, Landlock and `InaccessiblePaths=` agree. A symlink planted later inside a watched folder, pointing at a denied path, is refused by both Python and Landlock, but differently:
- **Python:** a policy violation, so the daemon stops with exit 78.
- **Landlock alone:** a failed read, which counts toward the blind limit.

**Proposal:** pick one outcome, then delete the code that only exists to produce the other.
- **If a failed read:** `guard.denied`, the audit hook's deny branch, `landlock.gaps()`, `DAEMON_START.landlock.gaps` and DB-18's gap comparison all go.
- **If a violation:** keep Python as the single detector, and drop `InaccessiblePaths=` for the always-denied paths.

**Removes:** seam 11, and one invariant (`gaps == []`).

**What would make it wrong:** whether a planted link is an attack signal that must stop the daemon (HF-17 argued that stopping is a denial of service by anyone who can write the folder).
</details>

<details markdown="1">
<summary>E-5. One gate, at start</summary>

**Today:** there are two gates.
- The installer's checks the unit bytes and the host.
- The runtime's checks the code, manifest and contract at every start.

**Proposal:** the runtime also checks the host facts. The qualified unit carries them, or a digest of them, in `ExecStart`. The installer's gate is then a convenience, not a guarantee.

**Removes:** the overlap in seam 10.

**What would make it wrong:** a kernel update would stop every daemon until it is qualified again. That may be exactly what G-5 asks for, or may be too strict for an owner.
</details>

<details markdown="1">
<summary>E-6. A smaller test mode</summary>

**Today:** one environment variable relaxes seven behaviours (§2).

**Proposal:** the battery runs the daemon qualified, with the digests it is judging, so "an unqualified start is allowed" is no longer a test-mode relaxation. The limit overrides become one recorded object instead of four variables.

**Removes:** part of seam 9.

**What would make it wrong:** the self-tests run many hand-made fixtures, and qualifying each one costs a digest computation per run (milliseconds).
</details>

<details markdown="1">
<summary>E-7. Decide R-2 by inventory</summary>

**Today:** one reference daemon (`git-watch`) runs a program, and the pilot's daemon runs none. The command surface carries:
- 11 of the 40 entries in `HARDENING.md`, HF-40 included;
- PD-93 and PD-94;
- half of seam 2 (E-2);
- seam 1 and the loader residual.

**Proposal:** count the daemons actually planned. If few need a program, adopt R-2: the core runs none, and commands become an extension that runs them through a broker outside the confined process (PD-72).

**Removes:** seam 1, the residual, `proc.py` from the core, and every command-related invariant.

**What would make it wrong:** if most planned daemons read Git through `git`, the commands belong in the core, and the broker is the work.
</details>

<details markdown="1">
<summary>E-8. Fewer invariants, each with one check</summary>

**Today:** the owner's 8 invariants (§3) and the 9 core invariants proposed in r4.9 overlap. For example:
- owner 3 and INV-1 and INV-2 are all about confinement;
- owner 4 and 5 and INV-5 and INV-6 are all about the ledger.

**Proposal:** six invariants, each enforced in one place and tested once:

| # | Invariant | Absorbs |
| --- | --- | --- |
| 1 | Observe only, and the kernel enforces it | Owner 2, 3 and 8 (confinement); INV-1, INV-2 |
| 2 | Never record a guess | INV-3 |
| 3 | Blindness surfaces within `T` | Owner 6; INV-4 |
| 4 | The ledger is the one source of truth: written once, verified two ways, interpreted once, uncertainty visible | Owner 4 and 5; INV-5, INV-6 |
| 5 | It runs only what was judged, and judging is not activation | Owner 1 and 7; INV-7 |
| 6 | Bounded: one budget per cycle, limits derived, restart by class | INV-8, INV-9 |

**What would make it wrong:** an invariant that loses its own check when merged.
</details>

<details markdown="1">
<summary>E-9. Fewer commands</summary>

**Today:** `verify` (the primary verifier's head) and `status` (independent verification plus interpretation) overlap.

**Proposal:**
- `verify` becomes `status --verify-only`.
- `scaffold` and `envelope` are authoring tools that a frozen core need not carry.

**Removes:** one command and one reader path. A reader then always meets the same verification and interpretation.

**What would make it wrong:** tooling that already calls `verify`.
</details>

## 6. Questions for the reviewer

1. Which seam in §4 could be **removed** rather than guarded? Name the guarantee in §3 that survives without it.
2. Is there a second pair of enforcers of one rule, like HF-40's, that we have not found?
3. Is there another place where one identity has two copies, like HF-39's?
4. Of E-1 to E-9, which would you adopt first, and which would you reject? Why?
5. Is six invariants (E-8) the right number, or can two more merge?
6. What would you delete from the 6,115 lines without losing a guarantee?

## 7. Markup table

| # | Item | KEEP / SIMPLIFY / REMOVE | Evidence type | Notes |
| --- | --- | --- | --- | --- |
| 1 | Seam 1, command grant | | | |
| 2 | Seam 2, cycle budget (E-2) | | | |
| 3 | Seam 3, shared interpreter | | | |
| 4 | Seam 4, two verifiers | | | |
| 5 | Seam 5, checkpoint | | | |
| 6 | Seam 6, battery to unit | | | |
| 7 | Seam 7, vendoring | | | |
| 8 | Seam 8, workspace copy (E-3) | | | |
| 9 | Seam 9, test mode (E-6) | | | |
| 10 | Seam 10, two gates (E-1, E-5) | | | |
| 11 | Seam 11, deny outcome (E-4) | | | |
| 12 | E-7, R-2 by inventory | | | |
| 13 | E-8, six invariants | | | |
| 14 | E-9, fewer commands | | | |
| 15 | Your own candidate | | | |
