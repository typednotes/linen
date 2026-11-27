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

The first wire format packages one native executable per variant. It uses
JSON-RPC `call` with positional arguments for local workers. The HTTPS
`/call` endpoint adds a leading version and pin object and echoes it
alongside the value. Version 2 manifests require compiler evidence.
A producer can decode Lode's version 1 `identity` authoring fields as a
`Revision`. The compiler supplies the new build and runtime evidence and
signs a version 2 record for the worker it checked.

## Offline integration test

```sh
lake build --wfail cell-integration cell-worker-fixture
python3 Integration/cell_package.py .lake/build/bin/cell-integration
```

The test builds a Lean consumer that imports the cell contract. A separate
executable contains the addition implementation. The test signs that worker
with a disposable key and reuses it across two local scopes and a loopback TLS
server. It checks refusal of an altered payload, signature, manifest pin and
runtime ABI. It needs `gpg`, `gpgv`, `curl` and `openssl`. See the
[format and execution contract](../Linen/System/Cell/SPEC.md).
