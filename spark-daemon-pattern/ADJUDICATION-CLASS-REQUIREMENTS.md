# Adjudication: the package-accuracy and class-requirements review of `165aa17`

- **Asked:** the owner forwarded an independent reviewer's note on the `165aa17` archive ("ADOPT WITH MODIFICATIONS as a review package only"), with three corrections, a ten-row accuracy table, and three requirement sets (CL-ADM, CL-STB, CL-LAT), and asked whether it had been reviewed. It had not; this is that review, made at `e92fa96` (one commit after the archive; documents only).
- **Standing:** Claude's verdicts, subject to the owner. The reviewer's "not authorized" list is honoured in full: no class above 1a, no `PROPOSAL` event, no Kernel-Update edit, no X1 payload registry, no claim that 270 tests passed or that the archive adopts 5.0.0.
- **What the reviewer read correctly, confirmed here:** archive digest `d85e5df5…`; contract identity 5.0.0 / `2d080b40…` (the JSON file hash is not the identity); 305 entries, 255 regular files; 270 is a `def test_` inventory count, and `VERIFICATION-V5.md` narrates 261 then 264 from runs not repeated by the reviewer. All four readings hold.

## 1. The three corrections: all applied

| # | Reviewer's correction | Finding here | Applied |
| --- | --- | --- | --- |
| 1 | KU-33 S10 pins `6ab59a0`; the archive is `165aa17` | True; the pin had not moved with the documents | S10 now separates the **contract pin** (`2d080b40…`, unchanged since `7d128e1`) from the **source pin** (the commit the owner carries; reviewed at `165aa17`; deltas since `6ab59a0` are documents only). The digest is the identity; the commit pins the documents |
| 2 | The owner-map line says "kernel-enforced"; a Kernel-Update reader takes that as the six-part foundation | True, and the pattern's own `README.md` uses the same word for Landlock; in the kernel repository it means something else | The line now says "Landlock and the systemd unit, enforced by the operating-system kernel; the Spark kernel takes no dependency and holds no daemon state" |
| 3 | PD-51 says a head "attaches by subscribing"; the pattern is pull-only; `event_types` is a citation unit, not a channel; option C stays pattern-owned, X1 cites and never owns the schema | True on all three points. The daemon pushes nothing (I-1); every consumer path (`status`, the verifier) is a read. "Subscribe" named a seam that must not form | §2 rewritten (pull of verified facts; citation unit; no push seam); option A says "read by", option C is pattern-owned with X1 citing an id; §4 "where C lives" corrected, noting that it narrows the earlier PD-46/47 reading; question 4 and markup rows 3 and 6 reworded |

Two consequential rewordings the reviewer's table asked for were also made: §5 (ii) now reads "a named class decision plus a proposal envelope with no authority and validation before a person sees it; not a free unlock", and the taxonomy carries two bounds (a declared read of `.git` is a path grant with E-4 still open; exit 70 is ledger recovery, a process exit class, not a class privilege and not `UNKNOWN_AFTER_RESTART`).

## 2. The accuracy table: remaining rows

| Reviewer's row | Verdict here |
| --- | --- |
| §5 (i) rejected; §5 (iii) agreed, PD-61 first, no PD-40 recording until that register exists as evidence | **Agree.** Already the handoff's position; the PD-40 bound is now explicit by reference to this document |
| `CPUWeight` already corrected | **Agree** |
| "Human Class C" in `contract.authority` still present; the send does not close M-04 | **True that the words are present** (`contract/daemon-contract.json` `authority` and `admissible_for_activation`; `DAEMON-CONTRACT.md` §4, §7; `spark_daemon/contract.py`). In this pattern "Class C" has meant one thing since r1: a person's decision to install a unit. Splitting the words is a contract **text** change, so it belongs to the 5.0.1 wording bundle with I-4, not to a documents-only commit. See §4, finding 2, on the ordered cut |
| KU-33 "before any unbuilt edge" is too strong | **Agree.** The gate now reads "kernel-facing edges only; the pattern's own 5.0.1 wording items do not wait on it" |
| Commit count is not authority; `CODEOWNERS` must not raise trust | **Agree.** The row names a role and a holder as a label; PD-62 stands; the `CODEOWNERS` remark was already the owner's choice, not a trust mechanism |
| FirstBorn "built and in use" holds for the vendored 3.1.0 pilot only | **Agree.** Migration to 5.0.0 is listed in `REVIEW-PACKAGE-V5.md` §6 and not done; the pilot is not adoption of this archive |

## 3. The requirement sets, checked against the code

The reviewer wrote "holds now" against each row. Each was checked here against the enforcing code and its test or battery check; nothing was taken from the documents.

**CL-ADM (admission).** ADM-1 holds (`judge.conclusions` derives qualification on read; `unit --report` is the only unit path; nothing installs or enables: I-5, `test_invariants`). ADM-2 holds (`runtime.run` refuses without `expect`, exit 78; `unitgen.NO_RESTART_EXIT_CODES` lists 78). ADM-3 holds (execute granted nowhere in `pathpolicy.grants`; `guard` blocks fork and every spawn; `network.mode` admits `none` only, `PrivateNetwork=yes`). ADM-4 holds (`manifest.py` refuses a reserved name in `event_types`; the writer refuses an undeclared event). ADM-5 holds as a rule (N-19, now in the KU-33 row). ADM-6 holds (closed schema 4; the rename map is a supply citation). ADM-7 holds as a stop; E-4 still hides the layer, as the reviewer says, and a violation never counts toward the blind limit (cut 4 removed `count_violation`). ADM-8 recorded (PD-62), authoring protocol built. **ADM-9 open:** it is V-1, residual 6, and the 6.0.0 change in `ROADMAP.md` §2; the reviewer's sizing ("a skeleton-tree digest, not a kernel pillar") matches the proposal there. ADM-10 holds (`qualifies` false → `unit --report` refuses; a preview unit cannot pass the runtime gate).

**CL-STB (stability).** STB-1 holds (`RestartPreventExitStatus=2 65 73 78`). STB-2 holds (exit 70 is the uncertain commit; restart recovers through tail quarantine, U-6); the bound is now written in the taxonomy. STB-3 holds (one writer, JCS, quarantine, two verifiers, DB-22; exit 65 changes nothing); residual 5 stays open and is not closed by a checkpoint file (R-5c deferred). STB-4 holds (PD-70, HF-32, HF-34; one reacquisition cycle then `SENSE_BLIND`). STB-5 holds (`WATCHDOG=1` sent only from the main loop; DB-11 proves a hung `sense()` starves it; nothing clears blindness). STB-6 holds (`test_handoff.test_contract_version_is_pinned_to_its_sha`; the profile-name swap and the report collapse are recorded as changes in `contract/versions.json`'s 5.0.0 line and `cut2-accounting.md`; whether they are additionally recorded as "declared losses" in the reviewer's sense belongs to the ordered cut, §4). STB-7 open (RSS; DB-19 reserved). STB-8 holds (single process; the hook blocks `os.fork`).

**CL-LAT (latency).** LAT-1 holds (one `cycle_budget_ms` deadline; the alarm is off during the commit; a late digest keeps the previous one). LAT-2 holds, corrected in the taxonomy. **LAT-3 holds as the socket and is not written as an invariant, as the reviewer says.** One fact the reviewer's bound anticipates: at start the runtime sends `STATUS=observing; ledger seq N` (`runtime.py`), so the ledger sequence does appear on the notify socket once. It is informational and no consumer reads it, but "must not become a clock" is exactly the sentence that should be written down: added to the 5.0.1 wording bundle. LAT-4 holds (no network, no model, pull only). LAT-5 holds with one precision. `ctx.read_text` raises `TooLarge` past `max_bytes`: a failure, never a short success. `ctx.list_dir` past `max_entries` does not raise; it returns a `Listing` with `truncated=True`, so the partial listing is marked and the daemon code must look at the flag (HF-33's neighbourhood). That is a visible partial, not a silent one, and it is the one place the reviewer's "truncation is failure" should read "truncation is visible".

## 4. Two findings of this review

1. **The notify `STATUS=` line copies the ledger seq.** A fact, not a defect: it is lifecycle text for `systemctl status`. But the pattern has never written down that the four notify messages carry no observation, grant, outcome or clock, and the reviewer is right that the seam should be named before anyone reads it. It goes into the 5.0.1 wording bundle (`ROADMAP.md` §2, item 2).
2. **The "wording cut already ordered" is recorded in neither repository this session can read.** The reviewer refers to an order to split the two "Class C" words, point an LTC note at `utc/` `c026165b`, mark PD-82 unbuilt, and record two declared losses, and to M-04. None of `c026165b`, `utc/`, M-04 or that order appears in the pattern repository or in `Kernel-Update` at `ddec911`. The pattern's `LTC-INTEGRATION-REVIEW.md` and PD-82 exist, but no instruction to retarget them does. So the cut is not disputed here and not executed here: the owner supplies its source, and the Class C split is then done as a 5.0.1 contract-text change together with I-4 and the notify invariant, one digest bump for all three.

## 5. What changed in this commit

- `KERNEL-REGISTER-CANDIDATE-KU-33.md`: S10 pin split (contract pin / source pin); owner-map line says Landlock and the unit; the gate scoped to kernel-facing edges.
- `HANDOFF-PD-51-L2-EVENT-TYPES.md`: pull not subscribe; `event_types` as citation unit; option C pattern-owned, X1 cites; §5 (ii) is a class decision plus envelope and validation; question 4 and two markup rows reworded.
- `DAEMON-TAXONOMY-RECONCILIATION.md`: the `.git` path-grant bound; the exit-70 bound.
- `ROADMAP.md` §2: the 5.0.1 wording bundle (I-4, the Class C split pending the owner's source, the notify invariant).
- `HARDENING.md` residual 6: named ADM-9.
- `README.md`: file map and revision row.

No code changed. Contract unchanged (5.0.0). The reviewer's requirement IDs (ADM/STB/LAT) are cited as the reviewer's, not adopted as pattern IDs; if the owner wants them in the contract they are a 5.0.1 text addition.

## 6. For the owner

1. Supply the source of the ordered wording cut (the `utc/` `c026165b` note, M-04) so the Class C split can be done as written rather than guessed.
2. Confirm the 5.0.1 wording bundle as one digest bump: I-4 "relative to the head", the Class C split, the notify invariant.
3. Confirm ADM-9 = V-1 as the 6.0.0 code cut that follows the wording.

## 7. 2026-10-09 consolidation disposition

The owner authorized the consolidation pass after this review. The requirement sets remain **review matrices**, not a second normative hierarchy beside I-1..I-6.

- **5.0.1 wording bundle:** implemented. I-4 now states verification relative to the observed head; current normative “Class C” wording is replaced by **owner activation decision**; the sd_notify boundary is documented; stale per-call cycle-timeout wording is corrected.
- **ADM-9 = V-1:** remains the one substantive admission gap and is planned for contract 6.0.0 as `runtime_bundle_sha256`, strengthening I-5 rather than creating I-7.
- **STB/LAT rows:** continue as review evidence mappings only. DB-19 remains reserved for the direct cgroup/resource measurement work; real-host-systemd qualification is G-6 / `HARDWARE-GATE-DGX.md`, not a repurposed DB-19.
- **Noncritical mechanisms:** signed reports, unit-file check convenience, a hand-written JSON parser, checkpoint acceleration, Advise-class runtime work and an event registry remain deferred to their explicit triggers in `ROADMAP.md`.
