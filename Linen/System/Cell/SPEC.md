# Precompiled Lean cells

- **S1. Identity.** The manifest binds typed references to immutable
  intent, platform policy, contract and pinned Git source. A distinct build
  key covers admitted source, wrapper and codecs, transitive dependencies,
  link inputs and the runtime profile. The manifest records the executable
  digest and length. Unknown manifest versions and invalid coordinates are
  rejected.
- **S2. Admission.** A caller supplies the manifest pin, runtime and codec
  profile, and compiler keyring independently of the package. Admission
  verifies the detached OpenPGP signature over the exact UTF-8 manifest,
  then verifies the payload length and SHA-256. The manifest and signature
  are each limited to 64 KiB and the executable to 128 MiB. Local reads
  use fixed filenames in a caller-controlled directory. HTTPS reads use
  fixed paths, TLS verification, bounded I/O and no redirect following.
- **S3. Typed calls.** A Lean signature has ordered input, input-indexed
  output and an effect row. The compiler-side adapter decodes the input
  before interpreting the program. The caller pins the contract and codec
  identities and decodes with its original input, reconstructing dependent
  witnesses where needed. Invalid wire values return a decoding error.
- **S4. Execution.** The local interpreter writes the retained, verified
  bytes to its private directory and starts a bounded JSON-RPC worker.
  One scope supports repeated calls and reaps the worker on exit. A
  separate scope starts a fresh worker from the same admitted bytes.
  A trusted HTTPS worker must match the manifest pin and wire version
  on each request and response. Both transports execute existing code
  without compiling or evaluating Lean source at call time.
- **S5. Trust and effects.** The compiler signer attests the checked
  source, wrapper, audit and artifact correspondence. Lun supplies the
  process isolation, loaded identity for remote dispatch, effect
  interpreter and invocation authority. A manifest is metadata and
  carries no credentials, warrants, state, or authority. A local
  executable uses the host's provisioned runtime libraries.
- **S6. Semantic releases.** A compiler may add a signed `release` object
  with a project-scoped name and a canonical SemVer 2.0.0 version. Numeric
  components and prerelease identifiers follow SemVer precedence. Build
  metadata is part of exact equality and is ignored in precedence. Naming
  a release changes the manifest pin while preserving the native build key.
  Existing v2 manifests keep their encoding and can still be opened by pin.
- **S7. Resolution.** An in-memory catalog accepts manifests authenticated
  with independently supplied pins and project-authorized compiler keys.
  Each name and SemVer precedence has one exact version, revision, and
  dependency declaration, with one pin per full runtime profile.
  Re-registering an identical pin is
  idempotent. Resolution selects an exact version or the highest compatible
  stable version with the caller's exact runtime, Lean contract, and policy.
  Catalog insertion and lookup scan the entries once. The caller persists
  the selected pin and verifies and opens the worker by that pin. Persistence,
  project authorization, and deciding whether a change merits a major,
  minor, or patch bump belong to the host and publisher.
- **S8. Dependency declarations.** A named release with dependencies uses
  manifest format 3. Each edge pins the dependency name, SemVer requirement,
  Lean contract, approved policy, codec, runtime ABI, and audit version.
  Dependencies inherit the parent's exact target and toolchain. The compiler
  signs the canonical name-ordered declarations. Duplicate and self
  dependencies fail. Format 2 carries no dependencies. The single-package
  execution API refuses dependency manifests.
- **S9. Dependency resolution.** Given one or more authorized root queries,
  the resolver selects one compatible variant per reachable name. It tries
  candidates in descending SemVer precedence and backtracks on conflicts.
  Missing variants, incompatible requirements, and cycles fail resolution.
  The version 1 JSON lock lists exact signed manifests in dependency-first
  order and can itself be pinned. The search is exhaustive over the finite
  catalog and may take exponential time when many alternatives conflict.
- **S10. Locked admission.** Before returning a runnable closure, replay
  checks the expected lock pin, root constraints, all signed manifests and
  dependency edges, and the size and digest of every distinct worker. Replay
  uses exact pins and does not resolve ranges. Identical worker bytes are
  retained once and counted once against the configurable closure budget
  (512 MiB by default). The host makes typed calls from its checked plan and
  supplies fresh invocation authority. Compiled dependencies and native
  system libraries are accounted for in the build key and runtime profile.
