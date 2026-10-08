# Adversarial review handoff: Spark daemon pattern, contract 5.0.0

Purpose: hand the next reviewer a map for attacking this pattern, not a reassurance that it is
sound. It names the trust boundaries, the things already probed and how they held, the known
holes, and a prioritized list of what to attack next, with the fixtures and commands to do it.

- **Source:** `xarendovich/FIxed-Income-Calculator`, branch `claude/clever-bardeen-qqkxez`,
  `spark-daemon-pattern/`. Contract 5.0.0, `2d080b407b6c82b230563670746f4a615b2c96660ec68f4509574562c282001d`.
  HF-43/HF-44 fixed at `3f67f51`; this handoff is the commit that follows.
- **Companion documents:** `REVIEW-PACKAGE-V5.md` (what changed and why), `VERIFICATION-V5.md`
  (the verification pass that found HF-43, HF-44 and residuals 5–6), `HARDENING.md` (every
  defect HF-01…HF-44 with a reproduction, and residual risks 1–6), `ADJUDICATION-V5.md`.
- **Conflict of interest, stated plainly:** Claude wrote this code and wrote this handoff. None
  of the checking in it is independent. Treat every "held" below as a claim to re-break, not a
  result. The point of this document is to make re-breaking cheap.

## 1. How to stand the thing up for attack

```
python3 -B -m unittest discover -s tests -t tests        # 264 tests, needs Landlock; ~2.5 min
make battery-examples                                     # the three reference daemons
python3 -I -B bin/spark-daemon describe --identity        # confirm 5.0.0 / 2d080b40…
```

It needs a Linux kernel with **Landlock** (ABI ≥ 1). Contract 5 refuses to start a daemon
without it, so on a kernel that lacks it (a stock container, ABI −38) the dynamic half fails
closed with exit 78 and cannot be exercised — the first reviewer hit exactly this. `jsonschema`
is an optional test dependency; four schema-agreement tests skip without it. The DGX (aarch64,
the daemon as a service under the real host systemd) is the one environment no container
substitutes for: `HARDWARE-GATE-DGX.md`.

## 2. The trust boundaries, as an attacker sees them

| # | Boundary | What is trusted | What is hostile | Enforced by |
| --- | --- | --- | --- | --- |
| A | Daemon code → the host | the skeleton, the kernel | `daemon.py` (a person/script/model wrote it) | purity (tripwire) → audit hook (tripwire) → **Landlock + the systemd unit (the real boundary)** |
| B | Observed data → the daemon | the skeleton | file contents under `reads`, file names in a watched dir, timestamps | `ctx` bounds (`max_bytes`, `max_entries`), canonical refusal of floats/surrogates, `ctx.unsettled` for unstable reads |
| C | The ledger → any reader | the chain back from the head | the ledger bytes on disk (a prior crash, tampering, or bit-rot) | two independent verifiers, DB-22 cross-check, `semantics.interpret` only on an intact chain |
| D | A report → the installer | nothing | the report JSON (self-asserted facts) | `judge.conclusions` + host match at install; **the runtime's digest gate at every start** |
| E | Author tooling → the core | the core | a candidate envelope's claims | the envelope is a checked input (DB-24), carries no authority |

The design principle under all of them: **a tripwire (purity, the audit hook) is allowed to
have gaps; the kernel layer behind it is not.** So the highest-value attacks are those that
reach past the kernel layer, not past a tripwire.

## 3. Probed in this pass, with the result and how to re-run it

Everything here is reproducible; scripts live in `evidence/v5/verification/`.

1. **Verifier diversity (boundary C).** The two verifiers share no code but both use CPython's
   `json` parser and run the checks in one order — diversity is in re-encoding, not parsing.
   A differential fuzz (`fuzz_verifiers.py`, 40,000 mutations, 2 seeds) found **HF-44** (now
   fixed); a hand case found **HF-43** (now fixed). After the fix: zero disagreements, zero
   crashes. **Re-attack:** extend the token set in the fuzzer, raise the mutation count, and
   target the parser specifically (depth, duplicate keys, number forms, BOM, NUL, overlong
   UTF-8). A common-mode parser fault is the open seam here.
2. **Skeleton not bound by qualification (boundary D, V-1 / residual 6).** A qualified unit
   pins the manifest, `daemon.py` and the contract *text*, not the enforcing code. With the
   audit hook stubbed out in a copied skeleton, a qualified start runs normally and the contract
   identity is unchanged (`v1_skeleton_not_bound.py`). **This is a real gap, open for the
   owner.** Re-attack: find anything else a post-qualification skeleton edit can change that no
   gate notices.
3. **Chain head not anchored (boundary C, residual 5).** A hash chain proves every record
   *before* the head. The last record can be rewritten, or the ledger truncated, and both
   verifiers still report intact with a new head. The fuzz confirms it. Only an off-host head
   (PD-58, not built) closes it. Re-attack: confirm no reader treats "intact" as "complete."
4. **Purity escape to `io`/`os` (boundary A, V-5).** `dataclasses.sys.modules['io']` is
   syntactically clean — purity does not follow module-attribute chains — but at runtime the
   audit hook catches the read of a denied path (exit 78, nothing leaked). Now a regression test
   (`test_hardening.RuntimeHardeningTests.test_a_purity_escape_to_io_is_caught_by_the_audit_hook`).
   This is the tripwire-vs-boundary principle in action. **Re-attack: find an escape the audit
   hook ALSO misses, then confirm Landlock still blocks the effect** (DB-17 shows the kernel
   blocking with the hook in record-only mode). That is the attack that matters.
5. **PathPolicy symlink/ancestor/TOCTOU (boundary A).** A read symlinked into a denied tree is
   refused at load; the same swap after start-up fails closed without leaking
   (`swap_probe.py`); the check-to-open race is covered by Landlock's inode grant. `agreement()`
   checks all three projections on every shipped manifest. Re-attack: hardlinks, bind mounts, a
   denied path that becomes a parent of a read after start, `output_dir` reached via `..`
   through a symlinked parent.
6. **Cycle alarm after the per-command timeouts were removed (E-2).** Armed for sense/decide/
   validate, off during the commit, re-armed only for the digest's leftover time; a late signal
   lands inside the recording `try`. Re-attack: daemon code that installs its own SIGALRM
   handler, or that blocks in C without releasing the GIL, and see whether the watchdog
   (2×budget+10) still backstops it.
7. **HF-41, HF-42 regressions.** Closed and tested; see `VERIFICATION-V5.md` §4.

## 4. The surfaces most worth attacking next (ranked)

1. **The `json` parser as a common-mode fault (C).** Both verifiers depend on it. HF-43 proved
   one such fault existed and was invisible to DB-22. Hunt for others; consider whether the
   independent verifier should hand-parse rather than call `json.loads`.
2. **Post-qualification skeleton tampering (D, V-1).** The qualification gate's blind spot. Work
   out the full set of edits it misses, to size the fix (a skeleton tree digest).
3. **A purity escape the audit hook also misses (A).** The hook's blocked-event list
   (`guard._BLOCKED_EVENTS`, `_PATH_MUTATIONS`) is an allowlist-by-omission; find an I/O or
   execution path it does not name, then prove Landlock and the unit still contain the effect.
   If the kernel contains it, it is a tripwire gap (fix the list); if not, it is a real hole.
4. **`ctx` data bounds (B).** `read_text`'s decode, `list_dir`'s ordering past the cap (HF-33's
   neighbourhood), `stat` on exotic inodes, a watched file that grows during the read.
5. **The systemd unit as generated (A).** `systemd-analyze` scores it SAFE offline, but no unit
   has yet been started by a real host systemd. On the DGX, confirm `DAEMON_START.landlock.status == "enforced"`, the
   seccomp filter permits the Landlock syscalls (DB-15), and a forbidden write/connect is
   actually blocked by the running unit, not only in the battery's stand-in.
6. **Canonical/JCS equivalence (C).** `canonical_bytes` is claimed byte-identical to RFC 8785
   for every value the schema allows. The cross-check is `evidence/ap/jcs_crosscheck.py`.
   Re-run it against a different JCS implementation and widen the sampled values.

## 5. Ground rules a reviewer should hold the code to (do not let a fix break these)

These are the contract's six invariants (`spark-daemon describe`, `contract.invariants`;
`DAEMON-CONTRACT.md` §5a) plus the owner's standing constraints. A "hardening" change that
weakens any of them is not an improvement:

- Observe only; the kernel enforces it. No program is run; execute is granted nowhere.
- Never record a guess; only an accepted cycle writes events.
- Blindness surfaces within the limit, across restarts and clock steps, and is never restarted
  away.
- The ledger is append-only, verified two independent ways, interpreted once; integrity
  uncertainty is always visible, never rendered healthy.
- It runs only what was judged, and judging is not activation — nothing here installs or enables
  a unit.
- Bounded: one budget per whole cycle, every unit timing derived from it, restart by exit class.

And: qualification is evidence, not permission; a corrupt ledger means refuse and change
nothing (exit 65); Break Glass is never for internal systems, and an AI model cannot decide an
emergency exists.

## 6. Open items that need the owner (not a code fix)

1. Freeze 5.0.0 with HF-43/HF-44 fixed (the fixes change no contract rule).
2. Residual 5 / PD-58: anchor the chain head off the output directory; reword I-4 as "relative
   to the head" in 5.0.1.
3. Residual 6 / V-1: bind a skeleton tree digest as a fourth expected digest.
4. V-2 (sign reports?) and V-3 (`unit --report --check FILE`?): keep, defer or drop.
5. E-4: the outcome when more than one layer refuses a path (today: exit 78).
6. The DGX run (G-6): the full suite, the three batteries, and the first start as a service under
   the real host systemd on aarch64 with that kernel's Landlock ABI (`HARDWARE-GATE-DGX.md`).
7. Where the FirstBorn click-path lives: decided (`HARDWARE-GATE-DGX.md` §0b). The standard stays
   in the pattern; the binding moves to the pilot, pinned to a pattern commit and contract digest;
   §3 is a holding copy until the pilot's copy exists. The ledger-reading semantics are now a tested
   tool (`tools/daemon-start.py`), not prose, so a host runbook cannot simplify them away.
