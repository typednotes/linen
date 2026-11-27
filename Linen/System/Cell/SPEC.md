# Precompiled Lean cells

- **S1. Identity.** A version 2 manifest binds typed references to immutable
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
