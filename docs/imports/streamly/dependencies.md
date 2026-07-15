# `streamly` (`streamly-core`) module dependencies

Topological order of the in-scope modules of the
[`streamly-core`](https://hackage.haskell.org/package/streamly-core) package
(v0.3.1, source:
https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/streamly-core.cabal
— the real `exposed-modules`/`build-depends` fields were fetched and read
verbatim, not recalled from memory) planned for import into `linen`, per
[AGENTS.md](../../AGENTS.md)'s Hackage-import convention.

**Status: done.** All in-scope modules have been ported to `Linen/` (with
mirrored `Tests/` counterparts) and merged into `Linen.lean`/`Tests.lean`;
`lake build Linen Tests` passes. The final import contributed 36 new
`Linen.Data.*`/`Linen.System.IO` modules (the plan below projected 39
in-scope modules — the difference is a handful of the plan's separately-listed
facades/`.Type` splits that were consolidated during porting). The tier
breakdown and scope notes below remain accurate as the historical dependency
plan.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**. A representative sample of module import lists was fetched from
the `streamly-core-0.3.1` source tree to confirm the general dependency
*shape* (a strict layering: strict-data/`Step`/`SVar.Type` at the bottom, the
`*.Type` core types in the middle, the operation modules and public facades on
top). Sampled directly: `Unfold/Type.hs` (→ `Stream.Step` only),
`StreamK/Type.hs` (→ `BaseCompat`, `Maybe.Strict`, `SVar.Type`). Combined with
the edges confirmed in the prior research pass (`Fold.Type`, `Parser`,
`Stream.Type`, `MutArray.Type` — quoted per-module below), this fixes the
layering; the remaining intra-tier edges are **inferred from streamly's
`.Type`-module naming convention, not individually verified** (flagged inline
where relevant), consistent with the time-box on this planning step.

## Headline finding: this is a genuinely new streaming *paradigm*, not covered by the existing `Conduit` port

`linen` already ports one streaming library — `Conduit` (#30,
`Linen.Data.Conduit.*`) — but `streamly` is a **different streaming model**
and does not reuse it. Conduit is a demand-driven coroutine pipeline (a single
`ConduitT i o m r` free-monad-ish type); streamly-core is a **stream-fusion**
library built on the `Step`/`skip`/`stop` state-machine encoding (`Unfold`,
`Stream`/`StreamD`, `Fold`, `Scanl`, `Parser`, `Producer` are all
`s -> m (Step s a)`-shaped stepper functions designed for GHC's
`fusion-plugin` to inline into tight loops). The two coexist as distinct
`Linen.Data.*` families the way the Lean stdlib would keep distinct
abstractions distinct. **No separate not-yet-ported Hackage prerequisite is
needed before this one** (unlike `lens` → `profunctors`): every external
`build-depends` entry resolves against the Lean stdlib, an already-ported
`linen` module, or a narrow inline/drop — see "External dependencies" below.

## Namespace decision

The stream-fusion core is re-rooted under **`Linen.Data.Stream.*`** and its
sibling `Linen.Data.*` families, following Lean-stdlib `Data.*` placement
rather than mirroring streamly's `Streamly.Internal.Data.*` internal
hierarchy (AGENTS.md's "place modules the way the Lean stdlib would" rule; the
`Streamly.` package prefix and the `.Internal.` layer are Haskell-package
branding, dropped per the Lean-ify rule):

- `Streamly.Internal.Data.Stream.*` → `Linen.Data.Stream.*`
  (`.Type`, `.Generate`, `.Eliminate`, `.Transform`, `.Lift`, `.Nesting`, `.Step`)
- `Streamly.Internal.Data.StreamK*` → `Linen.Data.StreamK*`
- `Streamly.Internal.Data.Fold*` → `Linen.Data.Fold*`
- `Streamly.Internal.Data.Scanl*` → `Linen.Data.Scanl*`
- `Streamly.Internal.Data.Unfold*` → `Linen.Data.Unfold*`
- `Streamly.Internal.Data.Parser*` → `Linen.Data.Parser*` (streamly's
  streaming parser; distinct from `Std.Internal.Parsec`)
- `Streamly.Internal.Data.{Producer,Refold}*` → `Linen.Data.{Producer,Refold}*`
- `Streamly.Internal.Data.{MutByteArray,Unbox}*` → `Linen.Data.{MutByteArray,Unbox}*`
- `Streamly.Internal.Data.{Array,MutArray}*` → `Linen.Data.Array.Unboxed*` /
  `Linen.Data.MutArray*` (streamly's *unboxed, `Unbox`-serialized* arrays;
  namespaced to sit beside the existing `Linen.Data.Array.Shaped.*` from
  `repa` without collision)
- `Streamly.Internal.Data.{Tuple,Maybe,Either}.Strict` →
  `Linen.Data.{Tuple,Maybe,Either}.Strict`
- `Streamly.Internal.Data.SVar.Type` → `Linen.Data.Stream.SVarType`
  (only the pure `State`/`adaptState`/`defState` stream-scheduling-state
  record is in scope, not the concurrency `SVar` itself — see scope note)
- `Streamly.Internal.System.IO` → `Linen.System.IO` (default chunk/buffer sizes)
- `Streamly.Internal.BaseCompat` → `Linen.Data.Stream.BaseCompat` (a handful
  of coercion/composition helpers, e.g. `(#.)`)
- The public `Streamly.Data.*` facades → the matching `Linen.Data.*` facade
  (e.g. `Streamly.Data.Stream` → `Linen.Data.Stream`).

## External (non-`streamly-core`) dependencies

Resolved against `streamly-core-0.3.1.cabal`'s library `build-depends`, in
Hackage-import precedence order (Lean stdlib > existing `linen` Haskell port >
new source):

Already ported / covered by the Lean stdlib, reused as-is:

- `base` → `Base` / Lean stdlib.
- `containers` → `Containers` (stdlib `Std.HashMap`/`Std.TreeMap` where
  applicable) — only needed by the deferred `*.Container` modules
  (`Fold.Container`, `Stream.Container`) that integrate a `Map`/`Set` sink;
  not in this batch's in-scope set.
- `transformers` → Lean's own `ExceptT`/`ReaderT`/`StateT` (already relied on
  by the `Mtl` port), for `Stream.Lift`'s `liftInner`/`runReaderT`/`evalStateT`
  stream lifters.

Substituted with directly-inlined code rather than a separate package import
(same treatment `hedis`'s `dependencies.md` gives `errors`/`scanner`, and
`lens`'s gives `call-stack`):

- **`exceptions`** (`Control.Monad.Catch`) — used by the stream/`Fold`
  exception combinators for `bracket`/`onException`/`finally`-style resource
  handling. Ported directly against `Linen.Control.Exception`'s existing
  `IO`-based exception machinery instead of a generic `MonadThrow`/`MonadCatch`
  port — the identical precedence-rule call `hedis`, `hoauth2`, and `lens`
  each made for this same package. (Chiefly relevant to the `Stream.Exception`/
  `Fold.Exception` modules, which are deferred here anyway.)

Dropped outright (GHC-toolchain / laziness / metaprogramming shims with no
Lean analogue — same category as `deepseq`/`base-orphans`/`template-haskell`
in the `hip` and `lens` entries):

- **`fusion-plugin-types`** — provides the `Fuse` annotation the GHC
  `fusion-plugin` reads to force stream-fusion inlining. Lean has no such
  rewrite-rule plugin (and, being eager, no lazy fusion to trigger); the
  annotations are no-ops for the port. This is streamly's defining
  GHC-specific machinery — **the port reproduces streamly's *fused data
  encoding* (the `Step` state machine) faithfully, but not the GHC plugin that
  optimizes it**, which is out of scope, not a simplification of behavior.
- **`ghc-bignum`, `integer-gmp`** — GHC's arbitrary-precision integer
  backends; Lean's native `Nat`/`Int` are already arbitrary-precision.
- **`ghc-prim`** — GHC primops (`ByteArray#`, `unsafeCoerce#`, …); Lean
  stdlib's `ByteArray`/`Array` and native ops cover what the in-scope modules
  need (`MutByteArray.Type`, `Unbox`).
- **`template-haskell`** — drives the `Unbox.TH`/`Serialize.TH` derivation
  macros (auto-generate `Unbox`/`Serialize` instances for user records). No
  Lean TH; those TH modules are deferred (see scope note), and the hand-written
  per-type `Unbox` instances substitute for the generated ones — the same
  treatment `lens`'s `Control.Lens.TH` note and
  `Linen.Database.DuckDB.Simple.Generic` already document.
- **`heaps`** — a leftist-heap priority queue used only by the time/rate
  scheduling paths (deferred with the concurrency layer, see scope note).
- **`monad-control`** — `MonadBaseControl`'s `liftBaseWith`/`restoreM`, used
  only by `Stream.Transformer`/`Stream.Lift`'s most general control-operation
  lifters (`Stream.Transformer` is deferred; `Stream.Lift`'s in-scope
  `ReaderT`/`StateT` lifters need only plain `transformers`, above).
- **`filepath`, `Win32`** — filesystem-path and Windows FFI, needed only by
  the deferred `FileSystem.*` module tree (see scope note).

## In-scope topologically sorted modules (the stream-fusion core)

Scoped to the core stream-fusion abstractions. Tiers are in build order;
within a tier, order is not load-bearing.

### Tier 0 — strict data, stepper encoding, low-level support (no internal deps)

1. `Streamly.Internal.BaseCompat` → `Linen.Data.Stream.BaseCompat` — a few
   coercion/composition helpers (`(#.)`, `(.#)`). No internal deps.
2. `Streamly.Internal.Data.Tuple.Strict` → `Linen.Data.Tuple.Strict` — strict
   `Tuple'`/`Tuple3'`/… accumulator types. No internal deps.
3. `Streamly.Internal.Data.Maybe.Strict` → `Linen.Data.Maybe.Strict` — strict
   `Maybe'` (`toMaybe`). No internal deps.
4. `Streamly.Internal.Data.Either.Strict` → `Linen.Data.Either.Strict` —
   strict `Either'`. No internal deps.
5. `Streamly.Internal.System.IO` → `Linen.System.IO` — default array/chunk
   buffer sizes (`arrayPayloadSize`, `defaultChunkSize`). No internal deps.
6. `Streamly.Internal.Data.SVar.Type` → `Linen.Data.Stream.SVarType` — **only
   the pure stream-scheduling `State` record** (`adaptState`, `defState`)
   threaded through `StreamK`/`Stream`; the concurrent `SVar` itself is
   deferred (scope note). No internal deps.
7. `Streamly.Internal.Data.Stream.Step` → `Linen.Data.Stream.Step` — the
   `Step s a = Yield a s | Skip s | Stop` fusion state machine. No internal
   deps (imported by `Unfold.Type` — confirmed via direct fetch).
8. `Streamly.Internal.Data.Fold.Step` → `Linen.Data.Fold.Step` — the fold
   `Step s b = Partial s | Done b` state machine. No internal deps.
9. `Streamly.Internal.Data.Unbox` → `Linen.Data.Unbox` — the `Unbox` class
   (fixed-size (de)serialization to/from a `MutByteArray`), over Lean's
   `ByteArray` primops (the hand-written-instance substitute for `Unbox.TH`).
   No internal deps.
10. `Streamly.Internal.Data.MutByteArray.Type` → `Linen.Data.MutByteArray` —
    the pinned/unpinned mutable byte array over Lean's `ByteArray`. No internal
    deps.

### Tier 1 — the `*.Type` core types

11. `Streamly.Internal.Data.Refold.Type` → `Linen.Data.Refold.Type` — a fold
    reader (`Refold m c a b`), the seed-parameterized fold. Depends on #2
    (inferred from naming convention — a small standalone `.Type`).
12. `Streamly.Internal.Data.StreamK.Type` → `Linen.Data.StreamK.Type` — the
    CPS-encoded stream (`StreamK`), streamly's non-fused stream. Depends on #1,
    #3, #6 (confirmed via direct fetch: imports `BaseCompat`, `Maybe.Strict`,
    `SVar.Type`).
13. `Streamly.Internal.Data.Scanl.Type` → `Linen.Data.Scanl.Type` — the
    stateful left-scan type. Depends on #2, #8 (inferred).
14. `Streamly.Internal.Data.Fold.Type` → `Linen.Data.Fold.Type` — the
    `Fold m a b` terminating left-fold. Depends on #2, #8, #11, #12, #13
    (confirmed prior pass: `Fold.Type → Refold.Type, Scanl.Type, Tuple.Strict,
    StreamK.Type, Fold.Step`).
15. `Streamly.Internal.Data.Unfold.Type` → `Linen.Data.Unfold.Type` — the
    `Unfold m a b` seed→stream generator. Depends on #7 (confirmed via direct
    fetch: imports `Stream.Step` only).
16. `Streamly.Internal.Data.Unfold.Enumeration` → `Linen.Data.Unfold.Enumeration`
    — `Enum`-range unfolds (`enumerateFromTo`, …). Depends on #15 (inferred;
    the prior pass's edge to a `Stream.Enumeration` reflects an earlier
    version's module split — in 0.3.1 enumeration lives here under `Unfold.*`).
17. `Streamly.Internal.Data.Producer.Type` → `Linen.Data.Producer.Type` — an
    `Unfold` variant that can extract its residual seed. Depends on #7, #15
    (inferred).
18. `Streamly.Internal.Data.Producer` → `Linen.Data.Producer` — producer
    combinators. Depends on #17 (inferred).

### Tier 2 — the fused `Stream` (`StreamD`) type

19. `Streamly.Internal.Data.Stream.Type` → `Linen.Data.Stream.Type` — the
    fused direct-style stream (`Stream m a`), the library's centerpiece.
    Depends on #1, #7, #6, #3, #2, #11, #14, #15, #12, #18 (confirmed prior
    pass: `Stream.Type → BaseCompat, Fold.Type, Maybe.Strict, Refold.Type,
    Stream.Step, SVar.Type, Tuple.Strict, Unfold.Type, StreamK.Type,
    Producer`).

### Tier 3 — stream operation modules

20. `Streamly.Internal.Data.Stream.Generate` → `Linen.Data.Stream.Generate` —
    generators (`fromList`, `unfoldr`, `replicate`, `iterate`). Depends on #15,
    #19.
21. `Streamly.Internal.Data.Stream.Eliminate` → `Linen.Data.Stream.Eliminate`
    — consumers (`fold`, `toList`, `drain`, `uncons`). Depends on #14, #19.
22. `Streamly.Internal.Data.Stream.Transform` → `Linen.Data.Stream.Transform` —
    mapping/filtering/scanning transforms. Depends on #13, #14, #19.
23. `Streamly.Internal.Data.Stream.Lift` → `Linen.Data.Stream.Lift` — monad
    lifting/hoisting (`liftInner`, `runReaderT`, `evalStateT`) over
    `transformers`. Depends on #19.
24. `Streamly.Internal.Data.Stream.Nesting` → `Linen.Data.Stream.Nesting` —
    append/interleave/zip/merge and nested-loop combinators. Depends on #14,
    #15, #19 (inferred).

### Tier 4 — the streaming `Parser`

25. `Streamly.Internal.Data.Parser.Type` → `Linen.Data.Parser.Type` — the
    backtracking streaming-parser type (`Parser a m b`) built on the fold
    `Step`. Depends on #4, #3, #2, #8 (inferred).
26. `Streamly.Internal.Data.Parser` → `Linen.Data.Parser` — parser
    combinators. Depends on #14, #6, #4, #3, #2, #19, #16, #25 (confirmed prior
    pass: `Parser → Fold.Type, SVar.Type, Either.Strict, Maybe.Strict,
    Tuple.Strict, Stream.Type, <enumeration>, Parser.Type`).

### Tier 5 — unboxed arrays over the fused stream

27. `Streamly.Internal.Data.MutArray.Type` → `Linen.Data.MutArray.Type` — the
    growable unboxed mutable array. Depends on #10, #9, #14, #13, #19, #25,
    #12, #6, #2, #15, #5, #23, #20 (confirmed prior pass: `MutArray.Type →
    MutByteArray.Type, Unbox, Fold.Type, Scanl.Type, Stream.Type, Parser.Type,
    StreamK.Type, SVar.Type, Tuple.Strict, Unfold.Type, System.IO, Stream.Lift,
    Stream.Generate`).
28. `Streamly.Internal.Data.MutArray` → `Linen.Data.MutArray` — mutable-array
    combinators. Depends on #27 (inferred).
29. `Streamly.Internal.Data.Array.Type` → `Linen.Data.Array.Unboxed.Type` — the
    immutable unboxed array (frozen `MutArray`). Depends on #27 (inferred).
30. `Streamly.Internal.Data.Array` → `Linen.Data.Array.Unboxed` — immutable
    array combinators. Depends on #29 (inferred).

### Tier 6 — public `Streamly.Data.*` facades (re-export only)

31. `Streamly.Data.StreamK` → `Linen.Data.StreamK` — re-exports #12.
32. `Streamly.Data.Stream` → `Linen.Data.Stream` — re-exports #19–#24.
33. `Streamly.Data.Fold` → `Linen.Data.Fold` — re-exports #14.
34. `Streamly.Data.Scanl` → `Linen.Data.Scanl` — re-exports #13.
35. `Streamly.Data.Unfold` → `Linen.Data.Unfold` — re-exports #15, #16.
36. `Streamly.Data.Parser` → `Linen.Data.Parser` — re-exports #25, #26.
37. `Streamly.Data.MutByteArray` → `Linen.Data.MutByteArray` — re-exports #9,
    #10.
38. `Streamly.Data.MutArray` → `Linen.Data.MutArray` (facade) — re-exports #28.
39. `Streamly.Data.Array` → `Linen.Data.Array.Unboxed` (facade) — re-exports
    #30.

**Total: 39 in-scope modules** (10 foundational, 8 core `.Type` types, 1 fused
`Stream`, 5 stream-operation, 2 parser, 4 array, 9 public facades). None of the
39 is folded away or dropped; the drops and deferrals below sit *outside* this
39-module core.

## Scope note: what is deferred, and why

Following the same "scope note" pattern the `hip` (#72), `duckdb-ffi` (#74),
and `hedis` (#80) entries use, the following ~55 of `streamly-core`'s ~95
library modules are **deferred out of this batch** — they are peripheral to
the stream-fusion abstractions above, and each is a self-contained subtree
that can be a later batch without changing the core:

- **`Streamly.Internal.FileSystem.*`** (Path/PosixPath/WindowsPath/Handle/
  FileIO/DirIO/ReadDir and the `Streamly.FileSystem.*` facades) — OS
  path/handle streaming I/O, pulling in `filepath`/`Win32`/POSIX-`errno` FFI.
  Peripheral to the stream algebra itself; the same "OS-specific subtree, out
  of scope" call the `duckdb-ffi` entry makes for its `Deprecated.*` tree.
- **`Streamly.Internal.Unicode.*`** (`Stream`/`String`/`Parser`/`Array`) —
  UTF-8/UTF-16 encode/decode *streams* built on top of the core; a distinct
  concern from the fusion machinery and deferrable to its own batch.
- **`Streamly.Internal.Data.Serialize.*` and `Unbox.TH`** — Template-Haskell
  instance derivation (`template-haskell`, no Lean TH); the hand-written
  `Unbox` instances (#9) cover what the in-scope arrays need.
- **`Streamly.Internal.Data.{Pipe,Scanr,RingArray,Binary,CString,IsMap,Path,
  Builder,IORef,IOFinalizer,Time.*}`, the `*.Container`/`*.Top`/`*.Transformer`/
  `*.Exception`/`*.Window`/`*.Combinators`/`*.Tee` sub-modules, the
  `*.Generic` (boxed) array variants, and every `deprecated`-flagged module
  (`Stream.StreamD`, `Fold.Chunked`, `Array.Stream`, `MutArray.Stream`)** —
  secondary combinator layers and container integrations built on the 39-module
  core; excluded to keep this batch focused, exactly as `hip` excluded
  `Graphics.Image.IO.Histogram` and `duckdb-ffi` excluded its 26 unused C-API
  modules.
- **The full `streamly` package's concurrency layer** — `streamly-core` is
  deliberately the *non-concurrent* foundation; the larger `streamly` package
  adds the `SVar`-based concurrent scheduler (parallel/async/ahead stream
  evaluation, rate control, the `heaps`/`monad-control`-backed worker pools).
  That scheduler is **explicitly out of scope for this import**, the same way
  `hedis`'s note bounds its plan to RESP2 (upstream's own scope) — here the
  boundary is `streamly-core`'s own package boundary. Deferred as a possible
  future `streamly` batch.

The in-scope 39 give a complete, self-contained stream-fusion core (`Stream`/
`StreamK`/`Unfold`/`Fold`/`Scanl`/`Parser`/`Producer` plus the unboxed
`Array`/`MutArray` they read and write) that stands on its own without any of
the deferred subtrees.
