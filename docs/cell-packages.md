# Precompiled Lean cells

A cell package distributes a platform-specific native worker alongside its Lean
contract and compiler attestation. A Lean program compiles against the
signature and codecs. Its calls use the worker that was compiled when the
package was built, including after a restart or in a second graph.

Linen provides the package format and the Lean call interface. Integration
with Lun's compiler, checked DAG placements, and Typednotes storage is
tracked in [TODO.md](../TODO.md). The tests use a compiler fixture and a
standalone Lean consumer.

## From source to a typed call

1. Lode writes the Lean signature and implementation. The signature carries
   the ordered arguments, a result type indexed by the input, and an
   `Eff` row. Use `Signature.Run` to check the implementation against it and
   `Codecs.handler` to build the wire adapter. Keep the contract and codec
   definitions in the pinned Lean source.
2. The compiler checks the project against the separately approved platform
   ceiling, builds the worker, and uses `Manifest.make` and `Manifest.sign`.
   Supply digests of the actual admitted sources, generated wrapper,
   dependency closure and link inputs. Save the resulting worker and its
   detached signature beside the rendered manifest as `worker`,
   `manifest.asc` and `manifest.json`. Publish the manifest after its
   payload is durable.
3. Lun or another Lean caller selects an exact manifest pin and compatible
   runtime profile from its own configuration or checked graph.
   `openPackage` verifies the signed record and payload bytes from a local
   directory or HTTPS base URL. The resulting `Verified` value retains
   those bytes.
4. The caller uses `Verified.runLocal` with a typed `Local.call` program, or
   `Verified.callHttp` with a trusted HTTPS worker. Lun can scope a local
   worker to a graph and reuse it for successive calls. `dispatch` checks
   the claimed pin against the loaded identity and invokes an executor
   supplied by the host. Lun resolves authority independently for each
   placement and keeps execution state with its checked plan.

For a signature whose result is `Vector Nat input.size`, both the worker's
encoder and the consumer's decoder receive the input. The decoder checks
the returned JSON array's length before constructing the vector. The
signature can be published as a narrow Lean package so callers can
type-check their own code against it without compiling the cell body.

```lean
import Linen.System.Cell

open System.Cell

def invoke (cell : Verified) (codec : Codecs sig) (input : sig.Input) :
    IO (Except String (sig.Output input)) :=
  cell.runLocal codec (Local.call (sig := sig) input)
```

Put multiple calls in one `runLocal` program to reuse the same process.
The codec bundle pins both the Lean contract and its wire representation.

The wire protocol packages one native executable per variant. It uses
JSON-RPC `call` with positional arguments for local workers. The HTTPS
`/call` endpoint adds a leading version and pin object and echoes it
alongside the value. Version 2 manifests require compiler evidence.
A producer can decode Lode's version 1 `identity` authoring fields as a
`Revision`. The compiler supplies the new build and runtime evidence and
signs a version 2 record for the worker it checked.

## Semantic release versions

A publisher can name a release `math.addition@1.2.0`. The public interface
includes the ordered Lean inputs, outputs, effects and wire codecs. Use a
major bump for incompatible interface changes, a minor bump for compatible
additions and a patch bump for compatible fixes. The publisher decides
whether a change is compatible. [Semantic Versioning 2.0.0](https://semver.org/spec/v2.0.0.html)

Attach the release before signing the manifest. The signed v2 record then
contains `"release":{"name":"math.addition","version":"1.2.0"}`. Every target
for this release shares one exact version and authoring revision. The build
key still depends on compiler inputs. The release label changes the manifest
pin. Existing v2 records can still be opened by their exact pins.

```lean
import Linen.System.Cell

open System.Cell

def labelAddition (manifest : Manifest) : Except String Manifest := do
  let name ← ReleaseName.parse "math.addition"
  let version ← Version.parse "1.2.0"
  manifest.withRelease { name, version }
```

Load a project catalog by calling `inspectPackage` with each exact manifest
pin and the project's trusted keyring, then insert each authenticated record
with `Catalog.insert`. Inspection reads the manifest and detached signature.
The catalog rejects reassigning a published version to another revision or
manifest at the same runtime. Build metadata is retained for exact selection.
Versions that differ only in build metadata cannot claim separate releases
under the same name.

`VersionRequirement.parse "1.2.0-rc.1"` selects that exact prerelease.
`VersionRequirement.parse "^1.2.0"` selects the highest stable version from
`1.2.0` up to `2.0.0`. For major zero, `^0.2.0` stops before `0.3.0`, and
`^0.0.2` selects only patch `2`. Selection also requires the exact runtime,
contract and policy supplied by the caller. When a placement resolves the
range, save the returned manifest pin in the plan and open that worker by
pin. Later catalog additions affect future resolutions.

Linen supplies an in-memory catalog. Persistent project storage and
placement integration are tracked in [TODO.md](../TODO.md).

## Dependency locks

Declare direct cell dependencies with `Manifest.withRelease release deps`
before signing. Each dependency names a cell and an exact version or stable
range, and pins the expected Lean contract, policy, codecs, ABI and audit
version. Each edge inherits the parent's target and Lean toolchain. For
example, an `arm64` Linux root selects Linux `arm64` dependencies with their
own declared codec and ABI. A publisher can build the same immutable release
for different targets and resolve a separate lock for each target.

```lean
import Linen.System.Cell

open System.Cell

def lockForPlan (catalog : Catalog) (roots : List ReleaseQuery) :
    Except String Lockfile :=
  catalog.lock roots

def openLockedWorker (lock : Lockfile) (pin : Ref .lock)
    (roots : List ReleaseQuery) (keyring : System.FilePath)
    (locate : Ref .manifest → IO Location) (name : ReleaseName) : IO Verified := do
  let closure ← lock.admit pin roots keyring locate
  closure.openPackage name
```

1. Resolve a dependency graph from the project's attested catalog. Linen
   tries newer compatible releases first and backtracks when shared
   dependencies conflict. All roots in one lock agree on one version and
   native variant per name.
2. Store the lock's versioned JSON and its `Lockfile.pin` with the checked
   plan. The lock records every selected release and signed manifest hash.
   Each manifest in turn binds the source revision, runtime, and exact worker.
3. Replay by resolving package locations by manifest hash. Linen checks every
   signed manifest, required dependency and worker before exposing a runnable
   handle. The executor passes results through its checked Lean wiring and
   supplies fresh authority to each call.

The resolver uses Cargo's range-and-lock model and pins exact inputs in the
style of Nix flake locks. [Cargo resolver](https://doc.rust-lang.org/cargo/reference/resolver.html)
· [Nix flake locks](https://nix.dev/manual/nix/2.24/command-ref/new-cli/nix3-flake.html)

Cell dependency edges describe separately executed workers. When a build uses
another package at compile time, record those exact inputs in
`BuildInputs.dependenciesSha256`. System libraries are provisioned by the host
for the chosen runtime. Manifests with dependency declarations use format 3
and are opened through the locked closure.

## Offline integration test

```sh
lake build --wfail cell-integration cell-worker-fixture
python3 Integration/cell_package.py .lake/build/bin/cell-integration
```

The test builds a Lean consumer that imports the cell contract. A separate
executable contains the addition implementation. The test signs that worker
with a disposable key and reuses it across two local scopes and a loopback TLS
server. It exercises signed release selection, dependency backtracking and
lock replay, and checks refusal of altered versions, payloads, signatures,
manifest pins and runtime ABIs. It needs
`gpg`, `gpgv`, `curl` and `openssl`. See the
[format and execution contract](../Linen/System/Cell/SPEC.md).
