/-
  Linen.Data.Stream.Nesting — interleave / merge combinators for fused streams

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Stream.Nesting`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Stream/Nesting.hs),
  module #24 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Combinators that weave two `Stream m a` (#19) together: alternating
  (`interleave`) and ordered merging (`mergeBy`).

  ## Overlap with `Data.Stream.Type` (#19)

  The nested-loop and product combinators — `append`, `zipWith`/`zipWithM`,
  `crossWith`/`cross`/`crossApply`, `unfoldEach`/`unfoldCross`, and
  `concatMap`/`concatFor` — are already ported on `Stream.Type` (Tier 2) and are
  **not re-ported here**; use them from that module.

  ## Substitutions / deviations

  - Both combinators here are total `def`s: they assemble a stepper without
    driving the `Skip` loop.
  - The upstream `Fuse`-annotated `InterleaveState`/tuple fusion states become a
    plain inductive / nested `Option` tuple.
  - **`interleaveMin`'s unreachable `…Only` states → `Stop`** instead of
    upstream's `undefined` (it stops as soon as either stream ends, so those
    states are never entered).
  - **Scope.** `interleave`/`interleaveMin` and `mergeByM`/`mergeBy` are ported
    as the representative interleave/merge core. The remaining large families —
    `roundRobin`, the `interleaveEndBy*`/`interleaveSepBy*` variants,
    `unfoldEach{SepBy,EndBy}*`, the breadth-first/`fair*`/`sched*` schedulers,
    `intercalate`/`interpose`, the `parse*`/`groups*`/`splitOnSeq` families, and
    `mergeMinBy`/`mergeFstBy` — build on `Unfold`/`Parser`/array machinery from
    later tiers and belong to later batches, matching the plan's scoping.
-/

import Linen.Data.Stream.Type

namespace Data.Stream

open Data.Stream (Step State)

namespace Stream

-- ── Interleaving ─────────────────────────────────────────────────────────────

/-- Fusion state for `interleave`: which stream to pull from next, or which one
    is exhausted and being drained. -/
inductive InterleaveState (σ₁ σ₂ : Type) where
  /-- Pull the first stream next. -/
  | first : σ₁ → σ₂ → InterleaveState σ₁ σ₂
  /-- Pull the second stream next. -/
  | second : σ₁ → σ₂ → InterleaveState σ₁ σ₂
  /-- The second stream is exhausted; drain the first. -/
  | firstOnly : σ₁ → InterleaveState σ₁ σ₂
  /-- The first stream is exhausted; drain the second. -/
  | secondOnly : σ₂ → InterleaveState σ₁ σ₂

/-- Interleave two streams, alternating elements starting from the first; when
    one is exhausted, all remaining elements of the other are emitted. Both
    streams are fully drained. `O(n²)` in the number of appends. -/
@[inline] def interleave [Functor m] (t1 t2 : Stream m a) : Stream m a where
  s := InterleaveState t1.s t2.s
  step gst := fun
    | .first st1 st2 => (fun r => match r with
        | .Yield x s => Step.Yield x (.second s st2)
        | .Skip s => Step.Skip (.first s st2)
        | .Stop => Step.Skip (.secondOnly st2)) <$> t1.step gst st1
    | .second st1 st2 => (fun r => match r with
        | .Yield x s => Step.Yield x (.first st1 s)
        | .Skip s => Step.Skip (.second st1 s)
        | .Stop => Step.Skip (.firstOnly st1)) <$> t2.step gst st2
    | .firstOnly st1 => (fun r => match r with
        | .Yield x s => Step.Yield x (.firstOnly s)
        | .Skip s => Step.Skip (.firstOnly s)
        | .Stop => Step.Stop) <$> t1.step gst st1
    | .secondOnly st2 => (fun r => match r with
        | .Yield x s => Step.Yield x (.secondOnly s)
        | .Skip s => Step.Skip (.secondOnly s)
        | .Stop => Step.Stop) <$> t2.step gst st2
  state := .first t1.state t2.state

/-- Like `interleave` but stops as soon as either stream is exhausted. -/
@[inline] def interleaveMin [Applicative m] (t1 t2 : Stream m a) : Stream m a where
  s := InterleaveState t1.s t2.s
  step gst := fun
    | .first st1 st2 => (fun r => match r with
        | .Yield x s => Step.Yield x (.second s st2)
        | .Skip s => Step.Skip (.first s st2)
        | .Stop => Step.Stop) <$> t1.step gst st1
    | .second st1 st2 => (fun r => match r with
        | .Yield x s => Step.Yield x (.first st1 s)
        | .Skip s => Step.Skip (.second st1 s)
        | .Stop => Step.Stop) <$> t2.step gst st2
    -- Unreachable: `interleaveMin` stops before entering a drain-only state.
    | .firstOnly _ => pure Step.Stop
    | .secondOnly _ => pure Step.Stop
  state := .first t1.state t2.state

-- ── Merging ──────────────────────────────────────────────────────────────────

/-- Merge two streams by a monadic comparison: at each step the buffered heads
    are compared and the smaller emitted (ties favour the first stream). If both
    inputs are sorted ascending, the output is sorted ascending. -/
@[inline] def mergeByM [Monad m] (cmp : a → a → m Ordering) (ta tb : Stream m a) :
    Stream m a where
  s := Option ta.s × Option tb.s × Option a × Option a
  step gst := fun (msa, msb, mha, mhb) =>
    match mha, msa with
    | none, some sa => (fun r => match r with
        | .Yield x sa' => Step.Skip (some sa', msb, some x, mhb)
        | .Skip sa' => Step.Skip (some sa', msb, none, mhb)
        | .Stop => Step.Skip (none, msb, none, mhb)) <$> ta.step gst sa
    | _, _ =>
      match mhb, msb with
      | none, some sb => (fun r => match r with
          | .Yield x sb' => Step.Skip (msa, some sb', mha, some x)
          | .Skip sb' => Step.Skip (msa, some sb', mha, none)
          | .Stop => Step.Skip (msa, none, mha, none)) <$> tb.step gst sb
      | _, _ =>
        match mha, mhb with
        | some x, some y => do
            match ← cmp x y with
            | .gt => pure (Step.Yield y (msa, msb, some x, none))
            | _ => pure (Step.Yield x (msa, msb, none, some y))
        | some x, none => pure (Step.Yield x (msa, msb, none, none))
        | none, some y => pure (Step.Yield y (msa, msb, none, none))
        | none, none => pure Step.Stop
  state := (some ta.state, some tb.state, none, none)

/-- Merge two streams by a pure comparison (see `mergeByM`). -/
@[inline] def mergeBy [Monad m] (cmp : a → a → Ordering) (ta tb : Stream m a) : Stream m a :=
  mergeByM (fun x y => pure (cmp x y)) ta tb

end Stream
end Data.Stream
