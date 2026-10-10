# UDC 6.0 Qualified Execution Identity — controlling hardening decision

- **Status:** OWNER-AUTHORIZED IMPLEMENTATION TRACK. Contract 5.0.1 remains untouched on its qualification line; this branch is the contract-6 design/implementation track.
- **Scope:** observe-only UDC. No Class C terminology, no Spark Core attachment, no capability grant.
- **Goal:** compress execution admission to one typed content-addressed identity without turning a hash into authority, a resolver, a payload route, or a host-attestation claim.

## 1. Root object

The object is **QualifiedExecutionIdentity**. Its exact execution domain is:

`UDC.EXECUTION.OBSERVE.V1`

The root is:

```
execution_sha256 =
    SHA256(
        UTF8("UDC.EXECUTION.OBSERVE.V1")
        || 0x00
        || JCS(QualifiedExecutionIdentity)
    )
```

Fixed SHA-256. Canonical JCS only. No algorithm-agility field. Full 64-hex digests only. Unknown fields refuse.

The closed identity leaves are:

```
{
  "contract_sha256": "...",
  "manifest_sha256": "...",
  "daemon_code_sha256": "...",
  "runtime_bundle_sha256": "...",
  "interpreter": {
    "implementation": "cpython",
    "version": "...",
    "executable_realpath": "...",
    "executable_sha256": "..."
  }
}
```

The domain owns the observe profile. No `profile` selector appears in the root.

## 2. Explicit exclusions

The root does **not** contain:

- `skeleton_version` or `contract_version` (human labels);
- unit hash or unit text (derived deployment projection);
- resolved PathPolicy or a policy digest (runtime binding evidence);
- host facts, kernel, systemd or Landlock ABI;
- ledger head;
- activation decision or authority.

A root match proves identity only.

## 3. No resolver

An execution hash is never converted into a path, package, network lookup, import, plugin or payload.

The only permitted operation is:

```
already-local qualified objects -> recompute root -> compare to expected root
```

No CAS lookup. No fetch-by-hash. No load-by-hash.

## 4. Runtime bundle

Consume the existing ADM-9 definition: runtime entrypoint + `spark_daemon/**` + independent verifier.

The bundle is a frozen explicit file list. Files are regular, non-symlink, source artifacts only; `.pyc` and `__pycache__` refuse. Relative paths sort bytewise. The tree digest uses the existing path + per-file-SHA-256 deterministic tree-hash mechanics. No timestamps, UID/GID or mutable filesystem metadata enter the digest.

Do **not** create a custom memory importer. The deployment gate instead requires the qualified bundle to be root-owned and not writable by the service UID/GID or other users. Hostile root remains outside UDC's threat model. A future hostile-root requirement belongs to measured boot / IMA / fs-verity / signed host packages.

Author `daemon.py` is different: open once; hash, purity-check and compile/execute those exact bytes. No hash-then-reopen.

## 5. Interpreter and pre-exec environment

The interpreter leaf measures the executable actually running. On the Linux host profile, executable bytes are measured via `/proc/self/exe`; human path/implementation/version remain explanatory leaves.

The interpreter leaf is **not** a host-environment attestation. Loader, stdlib, shared libraries and kernel remain host TCB.

Pre-exec loader injection is prevented at the systemd/activation boundary, before Python starts. The qualified unit neutralizes at least:

- `LD_PRELOAD`
- `LD_LIBRARY_PATH`
- `LD_AUDIT`
- `PYTHONPATH`
- `PYTHONHOME`
- `PYTHONSTARTUP`

Python starts with `-I -B`. Runtime confirms isolated mode and absence as a consistency check; runtime refusal uses the existing fail-closed policy exit family, not a new exit-code class.

The qualifying battery interpreter is the unit interpreter. The qualifying path has no independent `--python` selector.

## 6. Two gates and effective unit

The unit stays outside the execution root to avoid circularity.

The activation/qualification gate:

1. regenerates the unit from the qualifying report using the generator already contained in the qualified runtime bundle;
2. requires the installed unit bytes to equal the qualified `unit_sha256`;
3. requires **no systemd drop-ins** for a qualified UDC service;
4. after daemon-reload, checks the security-critical effective properties (ExecStart, User/Group, relevant environment, confinement paths/directives, watchdog and restart policy);
5. protects the expected execution root through the root-owned deployment path.

The daemon does not perform these activation checks. It only recomputes local identity and compares to the expected root.

## 7. Resolved policy evidence

`resolved_policy` is a closed evidence object produced directly from the exact enforced `PathPolicy` instance:

```
{
  "home": "<absolute resolved path>",
  "reads": ["<absolute resolved path>", ...],
  "output_dir": "<absolute resolved path>",
  "deny": ["<absolute resolved path>", ...]
}
```

Arrays are sorted; empty arrays are present; no unknown fields; no tilde forms.

Landlock ABI/status/gaps remain in their existing Landlock evidence object. They are not PathPolicy fields.

No `policy_sha256` exists until a second independent artifact needs to cite the same canonical policy object.

## 8. DAEMON_START and independent verification

`DAEMON_START` retains:

- `execution_sha256`;
- full QualifiedExecutionIdentity leaves;
- exact resolved policy evidence;
- Landlock evidence and lifecycle facts.

The independent verifier independently recomputes the execution root from recorded leaves using the same public domain/JCS specification but without importing the runtime identity helper. It refuses leaf/root mismatch.

That proves self-consistency. Historical authenticity still requires the existing chain plus an external prior receipt when PD-58 is in use; an internally self-consistent forged ledger is not magically trusted.

## 9. Removable/derived leaves in 6.0

The 6.0 cut removes from core/admission:

- manifest `version`;
- manifest `purpose`;
- `DAEMON_START.qualified`;
- duplicate `tools.python`;
- duplicated report kernel/machine facts;
- candidate envelope / DB-24 per the prior ruling;
- fixed capability selectors per the prior profile-identity ruling;
- `digest.enabled` per the prior single-owner ruling.

`skeleton_version` and `contract_version` remain human labels pending final consumer review; neither authorizes admission.

## 10. Pass-3 acceptance properties

Implementation must demonstrate:

1. full hashes only;
2. exact execution-domain separation;
3. closed schemas;
4. fixed SHA-256 and JCS;
5. no resolver;
6. no authority from identity;
7. observe identity cannot be interpreted as active;
8. every identity-leaf mutation changes the root and refuses admission;
9. report root = unit expected root = runtime recomputed root = DAEMON_START root;
10. resolved policy remains outside the static root and equals the enforced PathPolicy serialization;
11. ledger head remains outside executable identity;
12. author code has no hash-then-reopen window;
13. expected root is protected by host deployment authority;
14. loader injection is neutralized before Python exec and runtime confirms isolated mode;
15. runtime bundle is a frozen exact regular-file set, root-owned/non-service-writable at activation;
16. independent verifier recomputes leaf/root consistency;
17. qualified UDC unit has no drop-ins and its installed bytes/effective critical properties match qualification;
18. hostile-root protection is explicitly out of scope.

## 11. Stop line

Do not add a resolver, second manifest, custom runtime importer, new authority object, new exit-code family, or host-attestation framework to satisfy this identity design.
