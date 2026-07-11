# `repa` module dependencies

Topological order of the modules of the
[`repa`](https://hackage.haskell.org/package/repa) Hackage package (version
`3.4.2.0`, the latest published — `4.2.3.1` does not exist) to import into
`linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention. A
prerequisite of `hip`.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**.

Ported namespace: `Data.Array.Repa.*` becomes `Linen.Data.Array.Shaped.*`.
"Repa" is the upstream project's own name, not a Haskell/GHC-specific term,
but per AGENTS.md's module-hierarchy rule the target namespace should
describe the functionality the way the Lean stdlib would, not mirror the
source project's branding — `Shaped` names the actual concept ported
(rank-polymorphic, shape-indexed arrays), matching how `Data.Colour` (not
some project-name-derived path) was used for `colour`.

## Scope and simplifications

`repa`'s implementation is built almost entirely around **GHC-specific
performance machinery** for parallel, in-place, unboxed array filling: a
worker-thread pool (`Gang`), associated data families selecting an unboxed
backing store per representation tag, `MagicHash`/`GHC.Exts` unboxed
arithmetic, and `unsafePerformIO`-driven mutable-buffer construction. None of
this is *observable behavior* — the actual specified behavior of every
`Load`/`Target`/`Eval` function is "materialize this delayed array into a
concrete backing store" or "fold/filter over an array" — so, following the
precedent already set in the `colour` port (dropping `RGB`'s `Applicative`
instance, collapsing `quantize`'s `RealFrac`/`Integral`/`Bounded` constraints
to a concrete `Float → UInt8`), this port keeps every module that carries
genuine, rank-polymorphic **interface** (the `Shape` class, the `Source`
class, and every representation with distinct *behavior*), while collapsing
GHC's dual sequential/parallel implementation strategy to a single,
straightforward, pure-functional Lean implementation with identical
observable results. This is a faithful port of repa's *specified semantics*,
not a scoped-down subset of its *API surface* — every public
function/representation reachable from `Data.Array.Repa` still has a Lean
counterpart; the change is in how it's implemented, not what it computes.
Concretely:

- **No `Target`/`Load`/`Elt` type classes.** Their sole purpose upstream is
  parallel-safe, GC-safe, in-place buffer construction via `IO` and
  `touch#`. Lean's persistent `Array` already provides efficient,
  single-pass construction (`Array.ofFn`/`Array.map`) with no separate
  "freeze the mutable buffer" phase and no boxing/GC-timing hazard to guard
  against with `touch`. `Data.Array.Repa.Eval.{Elt,Target,Load,Chunked,
  Cursored,Reduction,Selection,Interleaved}` and `Data.Array.Repa.Eval`
  itself (9 files) collapse away with no standalone `Eval.lean`: `computeS`
  is fundamentally "materialize any `Source` array into a `Manifest`" — since
  `Target` (which upstream needed to stay polymorphic over the destination
  representation) is dropped, `Manifest` is the only sensible destination,
  so `computeS`/`copyS` are defined directly in `Repr/Manifest.lean` against
  the generic `Source` class, needing nothing from a separate `Eval` module.
  `now` (upstream: force a suspended parallel computation before
  proceeding) is dropped outright, for the same reason as `deepSeq` — it
  exists solely to pin down evaluation order under GHC's laziness/parallelism,
  which has no counterpart under Lean's eager, sequential semantics.
  Sequential/parallel duplication (`loadS`/`loadP`, `foldS`/`foldP`, …)
  collapses to one function each, since there is no worker-gang to target.
- **No `Data.Array.Repa.Eval.Gang`.** GHC-specific worker-thread pool
  (`forkOn`, `MVar`-based barriers); dropped outright, per the standing
  decision. Every module that only existed to feed it a parallel strategy
  (see above) is simplified along with it.
- **Unboxed/ForeignPtr/Vector reprs (`U`/`F`/`V`) collapse into one
  `Manifest` representation.** These three upstream modules differ *only* in
  backing store (`Data.Vector.Unboxed`, raw `ForeignPtr`, boxed
  `Data.Vector`) — their `Source`/`Target` instance logic is structurally
  identical (confirmed by direct comparison of `unsafeFreezeMVec` etc. across
  all three). Lean's `Array e` is the one efficient backing store needed;
  distinguishing "unboxed" from "boxed" from "foreign-pointer-backed" buys
  nothing in Lean the way it does under GHC's boxing model. `U`'s bonus
  `zip`/`unzip`, which upstream needs `Vector`'s `Unbox` type-family trick to
  make efficient, is implemented directly on `Manifest` with no analogous
  constraint needed; ported at arity 2–3 rather than upstream's 2–6, since
  arities 4–6 are mechanical repeats of the same pattern with no further
  consumer in this port's topological list.
- **ByteString repr (`B`) folds into `Manifest` too.** Upstream `B` is a
  strictly *weaker* read-only special case of the same flat-storage idea
  (immutable `ByteString`, `Word8` only, no `Target` instance at all) — it
  loses no capability by being represented as a `Manifest UInt8`.
- **`HintSmall` (`S`) and `HintInterleave` (`I`) reprs are dropped.** Both
  are transparent wrappers whose *entire purpose* is to select a different
  Gang-scheduling strategy (skip parallel dispatch for small arrays;
  interleave chunks across workers for load balancing) — with no Gang, both
  wrappers are no-ops around their inner representation, exactly the
  "unused once its reason for existing is gone" pattern already applied to
  `AffineSpace Chromaticity` in `colour`.
- **`Data.Array.Repa.Unsafe` is dropped.** Its functions are bounds-check-
  elided duplicates of the "safe" versions in `Operators.{IndexSpace,
  Traversal}` — a pure GHC-performance distinction (skip a runtime
  bounds-check assertion). The ported "safe" versions already don't carry
  a separate checked/unchecked code path.
- **`Data.Array.Repa.Stencil.Template` (Template Haskell quasiquoter) is
  dropped.** Lean has no TH. `Stencil.Base`'s `makeStencil`/`makeStencil2`
  (plain functions) are the only stencil-construction API upstream itself
  guarantees exists without TH (`Stencil.Dim2`'s import of `Template` is
  itself conditional on a `REPA_NO_TH` flag being unset), so this is a
  genuinely optional, GHC-macro-only convenience with a full non-TH
  fallback already present in the source.
- **`Data.Array.Repa.Arbitrary` (QuickCheck instances) is dropped.**
  Testing-only infrastructure; `linen`'s convention is `#guard`, not
  QuickCheck property tests.

Representations that **do** carry genuinely distinct behavior are kept as
distinct types: `Delayed` (an unevaluated function), `Manifest` (materialized
flat storage, as above), `Cursored` (shares index arithmetic between
neighbouring stencil taps via an explicit cursor type), `Partitioned`
(dispatches between two sub-arrays by region), and `Undefined` (a
placeholder array whose elements are never read). The `Shape`/`Source`
type-class hierarchy itself is ported in full generality — every rank
(`DIM0`–`DIM5` and beyond) and every kept representation — per the
user's explicit "full generic port" decision.

## Topologically sorted modules

1. `Data.Array.Repa.Shape` → `Shape.lean`
2. `Data.Array.Repa.Index` → `Index.lean` — 1
3. `Data.Array.Repa.Slice` → `Slice.lean` — 2
4. `Data.Array.Repa.Base` → `Base.lean` (the `Source` class) — 1
5. `Data.Array.Repa.Repr.Delayed` → `Repr/Delayed.lean` — 1, 2, 4
6. `Data.Array.Repa.{Repr.Unboxed,Repr.ForeignPtr,Repr.Vector,
   Repr.ByteString}` plus `Data.Array.Repa.{Eval.Elt,Eval.Target,Eval.Load,
   Eval.Chunked,Eval.Cursored,Eval.Reduction,Eval.Selection,
   Eval.Interleaved,Eval}` → `Repr/Manifest.lean` (collapsed representation,
   see above, plus `computeS`/`copyS` defined directly against `Source` —
   no separate `Eval.lean`) — 1, 4, 5
7. `Data.Array.Repa.Repr.Undefined` → `Repr/Undefined.lean` — 1, 4
8. `Data.Array.Repa.Repr.Cursored` → `Repr/Cursored.lean` — 1, 2, 4, 5, 6, 7
9. `Data.Array.Repa.Repr.Partitioned` → `Repr/Partitioned.lean` — 1, 4, 5, 6
10. `Data.Array.Repa.Operators.Traversal` → `Operators/Traversal.lean` — 1, 4, 5
11. `Data.Array.Repa.Operators.IndexSpace` → `Operators/IndexSpace.lean` —
    1, 2, 3, 4, 5, 10
12. `Data.Array.Repa.Operators.Interleave` → `Operators/Interleave.lean` —
    1, 2, 4, 5, 10
13. `Data.Array.Repa.Operators.Mapping` → `Operators/Mapping.lean` (the
    `Structured` class, with instances now for `Delayed`/`Manifest`/
    `Cursored`/`Partitioned`/`Undefined` only, `HintSmall`/`HintInterleave`
    dropped) — 1, 4, 5, 6, 7, 8, 9
14. `Data.Array.Repa.Operators.Reduction` → `Operators/Reduction.lean`
    (fold/sum/`Eq` instance, implemented directly with `Array.foldl`,
    `Eval.Reduction`'s Gang-based splitting dropped) — 1, 2, 4, 6, 13
15. `Data.Array.Repa.Operators.Selection` → `Operators/Selection.lean`
    (implemented directly with `Array.filter`, `Eval.Selection`'s
    Gang-based splitting dropped) — 1, 2, 4, 6
16. `Data.Array.Repa.Specialised.Dim2` → `Specialised/Dim2.lean` — 1, 2, 4, 7, 9
17. `Data.Array.Repa.Stencil.Base` → `Stencil/Base.lean` — 2
18. `Data.Array.Repa.Stencil.Partition` → `Stencil/Partition.lean` (no
    upstream dependencies — pure geometry)
19. `Data.Array.Repa.Stencil.Dim2` → `Stencil/Dim2.lean` (TH quasiquoter
    import dropped, see above) — 1, 2, 4, 5, 7, 8, 9, 17, 18
20. `Data.Array.Repa.Stencil` → `Stencil.lean` (thin re-export) — 4, 16, 17, 19
21. `Data.Array.Repa` → `Shaped.lean` (root re-export; `Arbitrary` import
    dropped) — 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15

## Excluded upstream modules

- `Data.Array.Repa.Eval.Gang` — GHC-specific parallel worker-thread pool;
  folded away along with every module that only used it to offer a
  now-unnecessary parallel strategy (see Scope and simplifications).
- `Data.Array.Repa.Stencil.Template` — Template Haskell quasiquoter for
  stencil literal syntax; no Lean analogue, and optional even upstream.
- `Data.Array.Repa.Arbitrary` — QuickCheck `Arbitrary` instances; testing
  infrastructure, not needed under `linen`'s `#guard` convention.
- `Data.Array.Repa.Unsafe` — bounds-check-elided duplicates of the safe
  index-space operators; no distinct behavior once ported.
