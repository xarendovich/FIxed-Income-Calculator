# Daemon pattern v0.1 — adjudication of external proposals AP-01 to AP-04

Draft, 2026-09-27. Prepared by Claude as architecture reviewer. **These are recommendations, not rulings**: the human records each decision (Class C) in the README decision log. Nothing here changes v0.1 code; a v0.2 would implement whatever is approved.

Subject: a submission from another LLM proposing four changes to the v0.1 package (kernel confinement, WASI instead of `purity.py`, an IETF SCITT / Sigstore ledger, out-of-process execution), framed by comparisons with Dapr, MCP and Erlang/OTP.

Evidence for every measured claim below is in `evidence/ap/` and can be re-run with `python3 evidence/ap/<script>`.

## Summary

| ID | Proposal | Draft verdict | Effect on v0.2 |
| --- | --- | --- | --- |
| AP-01 | Kernel confinement with Landlock or eBPF instead of relying on Python audit hooks | **Adopt Landlock, modified. Decline eBPF** for the skeleton | New kernel layer applied before daemon code is imported; ABI and gaps recorded in `DAEMON_START`; two battery checks |
| AP-02 | Compile `sense()`/`decide()` to WASI components, making `purity.py` obsolete | **Defer** | `purity.py` stays, relabelled as a review lint; revisit when X1 chooses a WASM runtime |
| AP-03 | Replace the bespoke hash-chained JSONL with IETF SCITT or Sigstore | **Modify.** Adopt RFC 8785 (JCS) canonical form now; SCITT as a WBS 7.0 export adapter later; decline public Sigstore | One change to key ordering in `canonical.py`; new test vectors |
| AP-04 | Run daemon logic out of process under a compiled supervisor, over a socket or lock-free ring buffers | **Adopt the process split, modified. Decline the compiled binary and the ring buffers** | Python supervisor plus one worker process per daemon, pipes, OTP-style restart limits |

Incidental defect found while measuring AP-04: **IF-01**, the manifest's memory floor is below the runtime's own footprint (below, and PD-20).

**Implementation status (r2, same day).** AP-01 (Landlock, modified), AP-03/J1 (JCS key ordering) and IF-01/PD-20 (64 MB floor) are implemented in code and self-tested — see README.md's r2 (implemented) revision-history row. Two further proposals submitted the same day, polling jitter and per-cycle GC forcing, are adjudicated and implemented alongside them as PD-21 and PD-22 (README.md). AP-02 (WASI) stays deferred, no code change. AP-04 (the supervisor/worker split, S1-S7 below) stays a design only — none of it is built. None of this is a Class C ruling; the decision log records PENDING for every PD until the human rules.

## Framing corrections

These apply across all four proposals.

- **"Sidecar" is the wrong precedent.** Dapr and Envoy sidecars are trusted infrastructure that serve *trusted* application code over HTTP or gRPC; they are not confinement. This pattern does the reverse: a trusted runtime confines *untrusted* logic. The closer precedents are Erlang/OTP supervision (a supervisor that owns restarts and a `gen_server` whose state is handed to callback code) and OpenSSH privilege separation (a privileged monitor with an unprivileged child). AP-04 uses OTP vocabulary for that reason.
- **MCP maps to a different component.** An MCP server answers requests from an LLM client. A Spark daemon has no client and takes no requests. The MCP analogy fits the future read-only query surface over daemon and Observer outputs, not the daemon itself.
- **"Exactly like your systemd watchdog integration" is not accurate for v0.1.** In v0.1 a hung `sense()` starves the watchdog and systemd kills the whole process; the ledger learns of it only on the next start (`previous_run_ended_cleanly: false`). An OTP supervisor records the cause at the time. AP-04 closes that gap.
- **"Your roadmap already targets WASI Preview 2 for the X1 Registry" is not what the project documents say.** The Steps 9/10/X1 v0.2 Integration Handoff lists "Exact WASM runtime", "One package transparency system" and "One signing stack" as implementation details to defer, and says not to make Step 9 depend on one runtime or sandbox technology. Observer handoff v0.2/v0.3 §13.3(b) declined a WASI sandbox for this track.
- **"The lock-free ring buffers you proposed for the universal transport plane"** does not match any document in the project. Treated as unverified.
- **"Over its 100-year lifespan"**: this pattern makes no lifespan claim. Its durability argument is a small, documented, standard-conformant format (AP-03) that any tool can verify.

## AP-01 — Kernel confinement (Landlock / eBPF)

### What the proposal gets right

Python audit hooks are not a security boundary (PEP 578 says so, and the v0.1 README's Known limits says so too). A kernel-enforced layer that holds even if the interpreter is subverted is worth having.

### What it leaves out

v0.1 already has a kernel layer: the generated systemd unit (`ProtectSystem=strict`, `ProtectHome=read-only`, `InaccessiblePaths`, `PrivateNetwork`, empty capability set, syscall filter; exposure 0.4). Landlock adds value in three specific places:

1. **Runs outside a system unit**: the battery, test mode, manual runs, and user units. On Ubuntu 24.04, mount-namespace directives in user units may not take effect (Observer v0.3 §4), and Landlock needs no namespaces or privileges.
2. **Defence in depth inside the unit.**
3. **Per-process domains.** systemd sandboxes a whole service; Landlock can give one child a *smaller* view than its parent. AP-04 depends on this.

### Evidence

Evidence file: `evidence/ap/landlock_probe.py`. It is standard library only (ctypes) and uses syscalls 444–446, which are the same numbers on x86_64 and aarch64.

Test conditions:

- The probe builds a fake home, applies a supervisor-style domain, then forks a child with a stricter worker-style domain.
- It ran on this workspace's kernel (6.18.44, **Landlock ABI 7**), then again with the ruleset limited to ABI 4, 3 and 1 to show what an older kernel would give.
- The probe creates every file it touches, so file permissions allow every attempt. Each denial below comes from Landlock alone.

| Attempt | ABI 7 | ABI 4 | ABI 3 | ABI 1 |
| --- | --- | --- | --- | --- |
| Read a declared read path | allowed | allowed | allowed | allowed |
| Read or list `~/.ssh` (never granted) | **blocked** | blocked | blocked | blocked |
| `stat` a file in `~/.ssh` | *allowed* | allowed | allowed | allowed |
| Read `~/spark-core/data/canary.txt` when `~/spark-core` is a granted read | *allowed* | allowed | allowed | allowed |
| Write inside a read path, or anywhere outside the output dir | **blocked** | blocked | blocked | blocked |
| Append, `ftruncate`, and `tmp/` → output rename inside the output dir | allowed | allowed | allowed | rename **fails (EXDEV)** |
| TCP connect to 127.0.0.1 | **blocked** | blocked | *allowed* | allowed |
| UDP `sendto` 127.0.0.1 | *allowed* | allowed | allowed | allowed |
| Exec `/usr/bin/git` (inherits the domain) | allowed | allowed | allowed | allowed |
| Worker (nested domain): write or create in the output dir | **blocked** | blocked | blocked | blocked |
| Worker: signal the supervisor | **blocked** | *allowed* | allowed | allowed |

Italics mark the gaps. Output files: `landlock-probe-abi7.json` and `landlock-probe-forced-abi{4,3,1}.json`.

### Corrections to the proposal's claims

- **"The OS will deny reads to `~/.ssh`" holds only when no granted read contains the path.** Landlock is allow-list only; it has no "allow this tree except that subtree" rule. The canary row shows the consequence: if a daemon reads `~/spark-core`, Landlock cannot keep it out of `~/spark-core/data`.
  - The workaround is to grant each sibling except `data`. The probe shows it blocks the canary, but it also blocks listing `~/spark-core` itself, which breaks `git status` on the repository root.
  - So protection of a denied path *inside* a granted read stays with systemd `InaccessiblePaths` and the audit hook. The skeleton must record such gaps, not hide them.
- **Landlock does not hide existence or metadata**: `stat` succeeds. `InaccessiblePaths` does hide them.
- **Network coverage is TCP only, and only from ABI 4.** UDP is not covered, so `PrivateNetwork`, `RestrictAddressFamilies=AF_UNIX` and the hook remain necessary.
- **ABI 1 breaks the skeleton's own `tmp/` → output rename.** The minimum usable ABI is 2.

### Expected ABI on the DGX Spark

NVIDIA's forum reports that DGX OS 7.4.0 (February 2026) moved DGX Spark to Linux 6.17 (`6.17.0-1008-nvidia`). Landlock ABI 7 arrived in Linux 6.15, so ABI 7 is expected. A machine still on the older 6.14 kernel would give ABI 6, and Ubuntu 24.04's generic 6.8 kernel gives ABI 4.

This is inference. The real value must be measured on the machine: `python3 evidence/ap/landlock_probe.py` prints it in its first lines.

### Draft ruling: adopt Landlock, modified

- **L1 — When it is applied.** The skeleton applies Landlock after it loads the manifest and records tool versions, and before it installs the audit hook, because the hook blocks ctypes. That is also before daemon code is imported.
- **L2 — The ruleset.** The skeleton handles every access right the running ABI knows. It grants:
  - read and execute on the Python runtime and system libraries, the pattern, the daemon's folder and each declared read path;
  - all rights beneath the output directory;
  - read and write on `/dev/null`;
  - no TCP bind or connect (ABI 4 and later);
  - signal scoping from ABI 6.

  The exact runtime grant list is fixed by running the whole self-test suite under Landlock.
- **L3 — Minimum ABI and missing Landlock.** The minimum is ABI 2. What happens when Landlock is missing or older than ABI 2 is decision PD-15.
- **L4 — Recorded in the ledger.** `DAEMON_START` gains `landlock: {abi, status, gaps}`. `gaps` lists each denied path that lies inside a granted read.
- **L5 — Two new battery checks (implemented).**
  - **DB-17:** with the audit hook in record-only mode (counts, never blocks), the forbidden write and the denied read are still refused by the kernel alone; from ABI 4, TCP as well. PASS on this workspace (ABI 7); N/A below MIN_USABLE_ABI, not a failure.
  - **DB-18:** a fresh single-cycle run's `DAEMON_START.landlock` (`abi`, `status`, `gaps`) matches a fresh in-process `landlock.abi_version()`/`landlock.gaps()` check on the same host.
- **L6 — First review step on the DGX:** run the probe and record the ABI.

### eBPF: decline for the skeleton

Loading BPF programs needs `CAP_BPF` or `CAP_SYS_ADMIN`, and BPF LSM must be enabled at boot. A daemon with no capabilities cannot do it. systemd already applies cgroup BPF on the daemon's behalf: `IPAddressDeny=` in the generated unit is implemented that way.

Two optional unit additions can be checked with `systemd-analyze` on the DGX:

- `SocketBindDeny=any` (cgroup BPF);
- `RestrictFileSystems=` (needs BPF LSM).

Recommendation: APPROVE L1–L6; DECLINE eBPF in the skeleton. Decision: PENDING

## AP-02 — WASI components instead of `purity.py`

### Assessment

The premise treats `purity.py` as the sandbox. It is not, and v0.1 does not claim it is: the README's Known limits says "the purity check is syntactic. The systemd unit is the security boundary."

`purity.py` does two other jobs:

- it keeps daemon code small, readable and deterministic for Class C review;
- it fails fast before start.

"Brittle as a boundary" is true and already accounted for: confinement comes from the hook and the unit, and with AP-01 and AP-04, from Landlock and a separate worker process.

### Cost of WASI today

- A WebAssembly runtime (for example wasmtime) and a Python-to-component toolchain. Neither is standard library, and both enlarge the supply chain on the DGX.
- `sense()` reads `/proc` and runs Git. In WASI both become host functions, so `ctx` would be rebuilt as WIT interfaces.
- The project has deliberately not chosen a WASM runtime yet (Integration Handoff v0.2), and declined WASI for this track (Observer v0.2/v0.3 §13.3(b)).

### What is worth keeping from it

`sense(ctx)` already *is* capability injection, the same model WASI uses: daemon code gets no ambient authority, only what `ctx` hands it. v0.2 should keep that contract strict (no I/O except through `ctx`, data-only returns), so a WASM host could run the same logic later without redesign.

### Draft ruling: defer

- Keep `purity.py`, relabelled in the README as "review and determinism lint, not confinement".
- Revisit trigger: when X1 selects a WASM runtime (a Class C decision), evaluate daemon logic as one of its consumers.

Recommendation: DEFER. Decision: PENDING

## AP-03 — IETF SCITT / Sigstore ledger

### Assessment

The goal is right: other tools should be able to verify a daemon's history without Spark's own code. The proposed means is at the wrong layer.

- **SCITT** is now RFC 9943 (Proposed Standard, June 2026). It makes *signed statements* transparent: issuers produce COSE_Sign1 statements, and a Transparency Service registers them and returns COSE receipts. It is not a format for a local append-only event log.
  - Adopting it "as the ledger" would bring in CBOR, COSE, signing keys and a Transparency Service.
  - None of those is standard library, and key custody is a new trust root.
- **Sigstore's public Rekor** is a public transparency log. Using it means network egress and publishing entry metadata to the internet, which conflicts with the no-egress constraint. A private Rekor would be one more service to run.
- The X1 handoff also defers "one package transparency system" and "one signing stack".

### What achieves the interoperability now, at no dependency cost

Verifying a v0.1 ledger needs two things: a canonical JSON form and SHA-256. If the canonical form is **RFC 8785 JSON Canonicalization Scheme (JCS)**, any JCS library plus any SHA-256 tool can verify the chain.

### Evidence

`evidence/ap/jcs_crosscheck.py` compares `canonical_bytes()` with an independent JCS reference, `jcs_ref.js`. The reference is Node's built-in ECMAScript `JSON.stringify` plus the default UTF-16 key sort, which is how RFC 8785 §3.2.2–3.2.3 defines the form for values without floats. Package registries were blocked in this workspace, so the `rfc8785` package could not be used as a second reference.

| Corpus (seeded) | v0.1 equals JCS | Candidate equals JCS | Candidate equals v0.1 |
| --- | --- | --- | --- |
| 5,000 values, ASCII keys, hostile string values (controls, bidi, U+2028, astral) | 5,000 | 5,000 | 5,000 |
| 5,000 values with non-ASCII keys mixed in | 3,089 | 5,000 | 3,089 |
| Minimal case: keys U+E000 and U+1F600 | 0 | 1 | 0 |
| The package's pinned golden vector | 1 | 1 | 1 |

Findings:

- v0.1 already produces JCS bytes for every value except one case: mapping keys that mix U+E000–U+FFFF with characters above U+FFFF.
- The difference is sort order. Python sorts keys by code point; JCS sorts by UTF-16 code units, where surrogates (U+D800–U+DFFF) sort below U+E000.
- The candidate orders keys by UTF-16 code units. It matches JCS on all 10,002 values and leaves every ASCII-keyed output byte-identical, including the golden hash.

### Draft ruling: modify

- **J1.** Change key ordering in `canonical_bytes()` to UTF-16 code units. The canonical form then *is* RFC 8785 JCS, restricted to I-JSON values without floats. State this in the ledger spec. Do it before any ledger is deployed, while no migration is needed.
- **J2.** Add pinned JCS vectors for the divergent key cases to `test_canonical.py`. Before freezing, re-run the cross-check once against a third-party JCS library outside this workspace.
- **J3.** Defer SCITT to WBS 7.0 as an *export adapter*. It would sign a checkpoint statement (daemon, seq, head hash) as COSE_Sign1 and register it with a *local* Transparency Service if one is adopted. That needs Class C decisions on key custody and on an exception to standard-library-only.
- **J4.** Decline public Sigstore/Rekor.

A linear hash chain is sufficient for a single local verifier. Merkle inclusion proofs become useful only at export, and a SCITT service provides them there.

Recommendation: APPROVE J1–J2; DEFER J3; DECLINE J4. Decision: PENDING

## AP-04 — Out-of-process execution

### Assessment

The stated benefit ("a memory leak or crash in the user's observation logic cannot corrupt the ledger") is already true in v0.1:

- each record is one `write` plus `fsync`;
- verification at start is strict;
- torn tails are quarantined;
- an uncertain commit exits instead of retrying.

The split is still worth doing, for different reasons:

1. **Least privilege per process.** The untrusted logic gets *no write access at all*, while the supervisor keeps output rights. The probe shows the nested worker domain cannot write or create files in the output directory, and from ABI 6 cannot signal the supervisor. systemd cannot give one child a smaller view than its service; Landlock can.
2. **Supervision recorded in the ledger.** The supervisor kills a hung worker at `step_timeout_seconds` and writes the cause (`WORKER_TIMEOUT`) at the time. The watchdog then guards the supervisor alone. Because `step_timeout_seconds ≤ watchdog_seconds / 2`, a worker hang is handled before systemd would act.
3. **Crash isolation.** A crash in native code, a `MemoryError` or a runaway recursion ends the worker. The supervisor records `WORKER_EXITED` with the signal or exit status and restarts it.
4. **Memory containment.** The worker sets its own `oom_score_adj` to 1000 (no privilege needed), so an OOM inside the cgroup kills the worker rather than the supervisor. The supervisor recycles the worker after N cycles, or when its resident memory (read from `/proc/<pid>/status`) exceeds a limit.
5. **State survives restarts.** The supervisor holds `prev` (already a canonical JSON value) and passes it to the worker each cycle, the OTP `gen_server` shape. A worker restart does not reset change detection.

### Costs (measured)

- A worker interpreter with the pattern's modules imported peaks at about **17 MB** RSS (17,044 / 17,044 / 17,012 KB over three runs).
- The v0.1 single-process example peaks at about **19 MB** (19,172 KB over three cycles), so a split daemon needs roughly 36 MB plus Git.
- About 300 lines of supervisor and worker code, a small protocol, and new tests.
- The watchdog-starvation self-test changes meaning and must be rewritten.

### IF-01, found while measuring

`resources.memory_max_mb` accepts 16 MB, below the v0.1 runtime's own measured peak of 19 MB. A manifest at the floor validates but cannot run. The floor should rise to 64 MB whether or not the split is adopted (PD-20). **Now fixed**: `manifest.py`'s floor is 64; every existing fixture and example already used 64, so nothing else needed to change.

A same-day follow-up submission argued instead for a 32 MB floor, reasoning from the DGX Spark's 128 GB unified memory and the cost of over-allocation across many daemons (both fair points, reflected in the 64 MB choice too) plus "12-15 MB Python baseline + a production constant `MAX_LEDGER_RECORD_BYTES` of 4 MiB = 16 MB, zero headroom." That 4 MiB figure does not exist in this project: `ledger.record_max_bytes`'s schema ceiling is 1 MiB (1,048,576 bytes), and `wbs-3.0-a2-secondary-review.md` records it as deliberately left open for the Observer track ("G0/G0.11 could not derive a finite bound"), never frozen at any value. Measured instead: bare interpreter ~7.5 MB RSS, full pattern imports ~18.3 MB, single-process daemon peak ~19.2 MB (matches the evidence above), and a combined supervisor+worker estimate of ~25.6 MB (`Pss`/`Private_Dirty` accounting) to ~34.5 MB (naive sum) once AP-04 lands. A 32 MB floor would leave that split with little to no headroom; 64 MB does, at negligible cost on a 128 GB host, and needs no fixture changes.

### Draft ruling: adopt the process split, modified

- **S1 — Supervisor.** Trusted pattern code: manifest, lock, recovery, ledger writer, digest rendering and writing, notify, watchdog. Its Landlock domain grants the union of what it and the worker need, because a nested domain can only narrow its parent's rights.
- **S2 — Worker.**
  - It starts as a fresh interpreter (`python3 -I -B -m spark_daemon.worker`), not a fork, so it inherits no state or descriptors.
  - It talks to the supervisor over stdin and stdout pipes; stderr is discarded.
  - Before importing `daemon.py` it:
    - applies its nested domain: read on the runtime and declared reads, exec only for allow-listed commands, write only under `output/tmp/worker/` for the Git index copy;
    - installs the audit hook;
    - runs the purity check.
  - It runs `sense`, `decide` and `digest`, and returns data only. The supervisor alone renders and writes.
- **S3 — Protocol.**
  - One canonical JSON line per message in each direction.
  - Each message has a size cap derived from `ledger.record_max_bytes` times the per-cycle event cap.
  - The supervisor parses with `strict_loads`. Any oversize, malformed or out-of-turn message kills the worker and records `WORKER_PROTOCOL`.
- **S4 — Timeouts.**
  - The supervisor waits at most `step_timeout_seconds`, then kills the worker's process group, which includes any Git child.
  - It records `WORKER_TIMEOUT` and restarts the worker.
- **S5 — Restart intensity (OTP style).**
  - At most 5 worker restarts in 300 s, then the supervisor exits with a new code (proposed 75) and systemd's start limits take over.
  - A policy violation in the worker is not restarted: the supervisor records it and exits 78 (PD-06 unchanged).
- **S6 — Recycling.** The worker is recycled every N cycles (manifest field, default 10,000) and above an RSS limit.
- **S7 — Battery checks.** New checks for worker hang, crash, garbage and oversize messages, the restart limit, and "worker cannot write the output directory". They use a test-mode-only fault knob, recorded in `test_overrides` like the other knobs (PD-10).

### Decline: a compiled supervisor binary

Isolation comes from the process boundary and the kernel (Landlock, the systemd unit), not from the language the supervisor is written in. The supervisor's untrusted input is one bounded JSON line, parsed by the standard library.

A second language would add a toolchain, aarch64 builds and a supply chain. It would also break the standard-library-only rule and the project's review-by-reading practice for Class C.

Revisit if many daemons run and per-supervisor memory starts to matter.

### Decline: lock-free ring buffers

- Shared memory with an untrusted writer means the supervisor reads bytes the worker can still change while they are being read (the double-fetch class of bugs), so it must copy, then validate.
- Pipes give copy semantics from the kernel for free.
- The load is one message per cycle (every 30 s for the example), where throughput is irrelevant.
- The transport plane the proposal cites is not in the project documents.

Recommendation: APPROVE S1–S7; DECLINE the compiled binary and ring buffers. Decision: PENDING

## Decisions for the human (continuing the README numbering)

**PD-15. When Landlock is missing or older than ABI 2.**
Options:
- *required*: refuse to start, exit 78;
- *best-effort*: run and record `status: unavailable`.

Recommendation: required for installed units; best-effort only in test mode, where it is recorded in `test_overrides`. Decision: PENDING

**PD-16. Denied paths inside granted reads** (for example `~/spark-core/data` under a `~/spark-core` read).
Options:
- accept them as recorded Landlock gaps, covered by `InaccessiblePaths` and the audit hook;
- refuse such manifests.

Refusing would block the Observer's own read of `~/spark-core`.
Recommendation: accept and record. Decision: PENDING

**PD-17. JCS alignment method.**
Options:
- UTF-16 key ordering (exact JCS for every accepted value);
- restricting mapping keys to ASCII (also JCS-safe, but narrower).

Recommendation: UTF-16 ordering. Decision: PENDING

**PD-18. Scope of the process split.**
Options:
- every daemon;
- opt-in per manifest.

Recommendation: every daemon, so there is one execution model to review and test. Decision: PENDING

**PD-19. SCITT export at WBS 7.0.** It needs an exception to standard-library-only and a key-custody decision.
Recommendation: DEFER; no action now. Decision: PENDING

**PD-20. IF-01: raise the `memory_max_mb` floor from 16 to 64.**
Recommendation: APPROVE, independent of AP-04. Decision: PENDING

## What a v0.2 would contain if the drafts are approved

- A Landlock layer (`landlock.py`, ctypes, standard library only) with ABI detection and gap reporting.
- `DAEMON_START.landlock`.
- JCS key ordering and vectors.
- The supervisor/worker split with its protocol and restart policy.
- The 64 MB memory floor.
- Battery checks DB-17 and DB-18, plus the worker checks.
- The whole self-test suite re-run with Landlock enforced.
- First evidence from the DGX Spark: the Landlock ABI, a battery run, and `systemd-analyze security`.

## Sources

- RFC 9943, An Architecture for Trustworthy and Transparent Digital Supply Chains: https://www.rfc-editor.org/info/rfc9943 and https://datatracker.ietf.org/doc/draft-ietf-scitt-architecture/
- NVIDIA Developer Forums, "DGX Spark is now on Linux 6.17": https://forums.developer.nvidia.com/t/dgx-spark-is-now-on-linux-6-17/360461
- Project: Spark Steps 9/10/X1 v0.2 Integration Handoff (§5, §6); Spark Repository Observer Handoff & Roadmap v0.2 and v0.3 (§4, §13.3); this package's README (Known limits, PD-01 to PD-14).
- Evidence: `evidence/ap/landlock_probe.py`, `landlock-probe-abi7.json`, `landlock-probe-forced-abi{4,3,1}.json`, `jcs_crosscheck.py`, `jcs_ref.js`, `jcs-crosscheck.json`.
