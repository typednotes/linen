/-
  Linen.Data.Fold.Type — the `Fold` terminating left fold

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Fold.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Fold/Type.hs),
  module #14 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A `Fold m a b` consumes a stream of `a`s and produces a `b`. It has the same
  shape as `Scanl` (#13) — `step`/`initial`/`extract`/`final` over the fold
  `Step` (`Partial`/`Done`) — but is meant for *terminating* consumption rather
  than exposing every intermediate result. `initial` embeds a default starting
  accumulator (so a `Fold` is monoid-like, whereas a `Refold` (#11) is
  semigroup-like and takes a seed).

  ## Substitutions / deviations

  - **Existential state via a structure field**, exactly as `Refold`/`Scanl`.
  - **`Step` is `Data.Fold.Step`** (#8); `Fuse`/`SeqFoldState`/`TeeState`/
    `Tuple'Fused` annotations dropped — the corresponding states use `Sum`,
    `Prod`, and `Data.Tuple.Tuple'`.
  - **`fromPure`/`fromEffect` use a total dummy `step`.** Upstream writes
    `Fold undefined (pure (Done b)) …`; the step is never reached (initial is
    `Done`), so here it is a total `fun s _ => pure (.Partial s)`.
  - **Scanning `extract`s made total.** Upstream `error`s in the `extract` of
    `splitWith`/`split_`/`concatMap`/`duplicate` ("cannot be used for
    scanning"). Per AGENTS.md (no partial landmines) these are ported total as
    `extract := final`, which is well defined and sensible.
  - **`toStreamK`/`toStreamKRev` are `unsafe`** (they build `Data.StreamK`, #12,
    which is `unsafe`), via `Scanl.toStreamK*`.
  - **Core scope:** the type, `Functor`/`Applicative` instances (streamly's
    `Fold` has no `Monad`), the fold constructors (`fromScanl`, `fromRefold`,
    `foldl'`, `foldlM'`, `fromPure`, `fromEffect`), the sequential
    (`splitWith`/`split_`) and distributing (`teeWith`) combinators,
    `rmapM`/`lmap`/`lmapM`, `filter`, `take`, `duplicate`, `drain`/`toList`/
    `toListRev`/`toStreamK*`. The larger secondary layer of the 2400-line
    upstream module (`concatMap`, the `teeWith{Fst,Min}`/`many`/`chunksOf`/
    `refold`/`scan` families, key-value/demux folds) belongs to later tiers,
    matching the plan's own module-level scoping.
-/

import Linen.Data.Fold.Step
import Linen.Data.Tuple.Strict
import Linen.Data.Refold.Type
import Linen.Data.Scanl.Type
import Linen.Data.StreamK.Type

namespace Data.Fold

open Data.Fold (Step)
open Data.Tuple (Tuple')
open Data.Scanl (Scanl)
open Data.Refold (Refold)

-- ── The Fold type ───────────────────────────────────────────────────────────

/-- A terminating left fold: consume `a`s, produce a `b`. The state type `s`
    is existentially hidden (an implicit field), as for `Scanl`/`Refold`. -/
structure Fold (m : Type u → Type v) (a b : Type u) where
  /-- The hidden accumulator-state type. -/
  {s : Type u}
  /-- Consume one input, advancing the state or terminating. -/
  step : s → a → m (Step s b)
  /-- The initial state (or an immediate `Done`). -/
  initial : m (Step s b)
  /-- Read the (intermediate) result out of the state. -/
  extract : s → m b
  /-- Read the final result and clean up the state. -/
  final : s → m b

-- ── Mapping on the output ───────────────────────────────────────────────────

/-- Map a monadic function on the output of a fold. -/
@[inline] def rmapM [Monad m] (f : b → m c) (fld : Fold m a b) : Fold m a c where
  s := fld.s
  step s a := fld.step s a >>= Step.mapMStep f
  initial := fld.initial >>= Step.mapMStep f
  extract := fld.extract >=> f
  final := fld.final >=> f

/-- `Functor`: map over the output `b`. -/
instance [Functor m] : Functor (Fold m a) where
  map f fld :=
    { s := fld.s
      step := fun s b => Functor.map (Functor.map f) (fld.step s b)
      initial := Functor.map (Functor.map f) fld.initial
      extract := fun s => Functor.map f (fld.extract s)
      final := fun s => Functor.map f (fld.final s) }

-- ── Fold constructors ───────────────────────────────────────────────────────

/-- Convert a left scan to a fold (identical field shape). -/
@[inline] def fromScanl (sc : Scanl m a b) : Fold m a b :=
  { s := sc.s, step := sc.step, initial := sc.initial, extract := sc.extract,
    final := sc.final }

/-- Make a fold from a pure left-fold step and initial accumulator. -/
@[inline] def foldl' [Monad m] (step : b → a → b) (initial : b) : Fold m a b :=
  fromScanl (Scanl.mkScanl step initial)

/-- Make a fold from a monadic step and initial accumulator. -/
@[inline] def foldlM' [Monad m] (step : b → a → m b) (initial : m b) : Fold m a b :=
  fromScanl (Scanl.mkScanlM step initial)

/-- Make a fold from a `Refold` and a seed value. -/
@[inline] def fromRefold (rf : Refold m c a b) (c : c) : Fold m a b :=
  { s := rf.s, step := rf.step, initial := rf.inject c, extract := rf.extract,
    final := rf.extract }

/-- A fold that yields `x` without consuming any input. -/
@[inline] def fromPure [Monad m] (x : b) : Fold m a b where
  s := b
  step s _ := pure (.Partial s)
  initial := pure (.Done x)
  extract := pure
  final := pure

/-- A fold that yields the result of an effect without consuming input. -/
@[inline] def fromEffect [Monad m] (act : m b) : Fold m a b where
  s := b
  step s _ := pure (.Partial s)
  initial := (.Done ·) <$> act
  extract := pure
  final := pure

-- ── Specific folds ──────────────────────────────────────────────────────────

/-- A fold that consumes and discards all input. -/
@[inline] def drain [Monad m] : Fold m a PUnit := fromScanl Scanl.drain

/-- A fold collecting all input into a list, in order. -/
@[inline] def toList [Monad m] : Fold m a (List a) :=
  rmapM (fun s => pure s.reverse) (foldl' (fun xs x => x :: xs) [])

/-- A fold collecting all input into a list, reversed. -/
@[inline] def toListRev [Monad m] : Fold m a (List a) :=
  foldl' (fun xs x => x :: xs) []

-- ── Sequential application ────────────────────────────────────────────────────

/-- Sequential fold application: feed input to the first fold; once it is done,
    feed the rest to the second, then combine both outputs with `func`. -/
@[inline] def splitWith {m : Type u → Type v} [Monad m] {x a b c : Type u}
    (func : a → b → c) (l : Fold m x a) (r : Fold m x b) : Fold m x c where
  s := l.s ⊕ ((b → c) × r.s)
  step
    | .inl sl, a => runL (l.step sl a)
    | .inr (f, sr), a => runR (r.step sr a) f
  initial := runL l.initial
  extract := final'
  final := final'
where
  runR (action : m (Step r.s b)) (f : b → c) :
      m (Step (l.s ⊕ ((b → c) × r.s)) c) :=
    (Step.bimap (fun sr => .inr (f, sr)) f) <$> action
  runL (action : m (Step l.s a)) : m (Step (l.s ⊕ ((b → c) × r.s)) c) := do
    Step.chainStepM (fun sl => pure (Sum.inl sl))
      (fun a => runR r.initial (func a)) (← action)
  final' : (l.s ⊕ ((b → c) × r.s)) → m c
    | .inr (f, sR) => f <$> r.final sR
    | .inl sL => do
        let rL ← l.final sL
        match ← r.initial with
        | .Partial sR => func rL <$> r.final sR
        | .Done rR => pure (func rL rR)

/-- Run two folds serially, discarding the first's result (applicative `*>`). -/
@[inline] def split_ {m : Type u → Type v} [Monad m] {x a b : Type u}
    (l : Fold m x a) (r : Fold m x b) : Fold m x b where
  s := l.s ⊕ r.s
  initial := do
    match ← l.initial with
    | .Partial sl => pure (.Partial (.inl sl))
    | .Done _ => Step.mapFst Sum.inr <$> r.initial
  step
    | .inl st, a => do
        match ← l.step st a with
        | .Partial s => pure (.Partial (.inl s))
        | .Done _ => Step.mapFst Sum.inr <$> r.initial
    | .inr st, a => Step.mapFst Sum.inr <$> r.step st a
  extract := final'
  final := final'
where
  final' : (l.s ⊕ r.s) → m b
    | .inr sR => r.final sR
    | .inl sL => do
        let _ ← l.final sL
        match ← r.initial with
        | .Partial sR => r.final sR
        | .Done rR => pure rR

/-- `Applicative`: `pure = fromPure`, `<*> = splitWith id`, `*> = split_`. -/
instance [Monad m] : Applicative (Fold m a) where
  pure := fromPure
  seq f x := splitWith (fun g y => g y) f (x ())
  seqRight l r := split_ l (r ())

-- ── Distributing (tee) ────────────────────────────────────────────────────────

/-- Distribute the input to both folds until both terminate, combining their
    outputs with `f`. -/
@[inline] def teeWith {m : Type u → Type v} [Monad m] {x b c d : Type u}
    (f : b → c → d) (l : Fold m x b) (r : Fold m x c) : Fold m x d where
  s := (l.s × r.s) ⊕ ((c × l.s) ⊕ (b × r.s))
  initial := runBoth l.initial r.initial
  step
    | .inl (sL, sR), a => runBoth (l.step sL a) (r.step sR a)
    | .inr (.inl (bR, sL)), a => (Step.bimap (fun s => .inr (.inl (bR, s))) (f · bR)) <$> l.step sL a
    | .inr (.inr (bL, sR)), a => (Step.bimap (fun s => .inr (.inr (bL, s))) (f bL)) <$> r.step sR a
  extract := ex l.extract r.extract
  final := ex l.final r.final
where
  runBoth (actionL : m (Step l.s b)) (actionR : m (Step r.s c)) :
      m (Step ((l.s × r.s) ⊕ ((c × l.s) ⊕ (b × r.s))) d) := do
    let resL ← actionL
    let resR ← actionR
    pure <|
      match resL with
      | .Partial sl =>
          match resR with
          | .Partial sr => .Partial (.inl (sl, sr))
          | .Done br => .Partial (.inr (.inl (br, sl)))
      | .Done bl => Step.bimap (fun sr => .inr (.inr (bl, sr))) (f bl) resR
  ex (exL : l.s → m b) (exR : r.s → m c) :
      (l.s × r.s) ⊕ ((c × l.s) ⊕ (b × r.s)) → m d
    | .inl (sL, sR) => f <$> exL sL <*> exR sR
    | .inr (.inl (bR, sL)) => (f · bR) <$> exL sL
    | .inr (.inr (bL, sR)) => f bL <$> exR sR

-- ── Mapping on the input ────────────────────────────────────────────────────

/-- `lmap f fold` maps `f` on the input of the fold. -/
@[inline] def lmap (f : a → b) (fld : Fold m b r) : Fold m a r :=
  { fld with step := fun x a => fld.step x (f a) }

/-- `lmapM f fold` maps the monadic `f` on the input of the fold. -/
@[inline] def lmapM [Monad m] (f : a → m b) (fld : Fold m b r) : Fold m a r :=
  { fld with step := fun x a => f a >>= fld.step x }

-- ── Filtering / trimming ──────────────────────────────────────────────────────

/-- Include only elements that pass a predicate. -/
@[inline] def filter [Monad m] (f : a → Bool) (fld : Fold m a r) : Fold m a r :=
  { fld with step := fun x a => if f a then fld.step x a else pure (.Partial x) }

/-- Take at most `n` input elements and fold them. -/
@[inline] def take [Monad m] (n : Int) (fld : Fold m a b) : Fold m a b where
  s := Tuple' Int fld.s
  initial := fld.initial >>= next (-1)
  step := fun ⟨i, r⟩ a => fld.step r a >>= next i
  extract := fun ⟨_, r⟩ => fld.extract r
  final := fun ⟨_, r⟩ => fld.final r
where
  next (i : Int) : Step fld.s b → m (Step (Tuple' Int fld.s) b)
    | .Partial s =>
        let i1 := i + 1
        if i1 < n then pure (.Partial ⟨i1, s⟩) else .Done <$> fld.final s
    | .Done b => pure (.Done b)

-- ── Duplicate (not portable at this universe) ────────────────────────────────

-- Upstream's `duplicate :: Fold m a b → Fold m a (Fold m a b)` is **not
-- portable** here: its output type parameter is `Fold m a b`, which — because
-- our existential-state `Fold` lives in `Type (max (u+1) v)` — sits in a
-- strictly higher universe than the `b : Type u` a `Fold`'s output parameter
-- admits. Lean rejects the resulting universe constraint. This is a genuine
-- universe-polymorphism limit (a `Fold` of `Fold`s), not a dodged proof, so it
-- is omitted rather than weakened.

-- ── To StreamK ──────────────────────────────────────────────────────────────

/-- Fold the input into a `StreamK` in order. -/
@[inline] unsafe def toStreamK [Monad m] : Fold m a (Data.StreamK.StreamK n a) :=
  fromScanl Scanl.toStreamK

/-- Fold the input into a reversed `StreamK`. -/
@[inline] unsafe def toStreamKRev [Monad m] : Fold m a (Data.StreamK.StreamK n a) :=
  fromScanl Scanl.toStreamKRev

end Data.Fold
