/-
  Linen.Data.Refold.Type — the `Refold` seed-parameterized fold

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Refold.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Refold/Type.hs),
  module #11 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A `Refold m c a b` is like a `Fold` except that the initial accumulator state
  is generated from a dynamically supplied input `c` (the *seed*), via `inject`,
  rather than being embedded as a fixed default. So a `Refold` is to a `Fold` as
  a `Semigroup` is to a `Monoid`. Refolds can be appended to build a fold
  incrementally (`append`, `iterate`).

  ## Substitutions / deviations

  - **Existential state via a structure field.** Upstream is
    `data Refold m c a b = forall s. Refold (s → a → m (Step s b)) …`; Lean has
    no `forall`-existential in a data declaration, so the hidden state type `s`
    becomes an (implicit) field of the `structure`, the standard Lean encoding
    of an existential. This lifts the type to `Type (max (u+1) v)`.
  - **`Step` is `Data.Fold.Step`** (`Partial`/`Done`), already ported (#8).
  - **`Fuse` annotation / `Tuple'Fused` dropped** — GHC-plugin markers with no
    Lean analogue; `take`'s counter/state pair uses `Data.Tuple.Tuple'` (#2).
  - **`sconcat` uses Lean's `Append`** in place of Haskell's `Semigroup (<>)`
    (linen has no `Semigroup` class); the count in `take` is a `Nat` (upstream
    `Int`, "a negative count is treated as 0" — `Nat` encodes that directly).
  - **`drainBy` yields `PUnit`** (upstream `()`), to stay in `Type u`.
-/

import Linen.Data.Fold.Step
import Linen.Data.Tuple.Strict

namespace Data.Refold

open Data.Fold (Step)
open Data.Tuple (Tuple')

-- ── The Refold type ─────────────────────────────────────────────────────────

/-- Like a `Fold` except the initial accumulator state is generated from a
    dynamically supplied seed `c` through `inject`. The state type `s` is
    existentially hidden (an implicit field). -/
structure Refold (m : Type u → Type v) (c a b : Type u) where
  /-- The hidden accumulator-state type. -/
  {s : Type u}
  /-- Consume one input, advancing the state or terminating. -/
  step : s → a → m (Step s b)
  /-- Build the initial state (or terminate immediately) from the seed. -/
  inject : c → m (Step s b)
  /-- Extract the final result from the state. -/
  extract : s → m b

-- ── Left fold constructors ──────────────────────────────────────────────────

/-- Make a consumer from a pure left-fold step function. Never returns `Done`. -/
@[inline] def foldl' [Monad m] (step : b → a → b) : Refold m b a b where
  s := b
  step s a := pure (.Partial (step s a))
  inject := (pure <| .Partial ·)
  extract := pure

-- ── Mapping on input ────────────────────────────────────────────────────────

/-- `lmapM f fold` maps the monadic function `f` on the input of the fold. -/
@[inline] def lmapM [Monad m] (f : a → m b) (fld : Refold m c b r) : Refold m c a r :=
  { fld with step := fun x a => f a >>= fld.step x }

-- ── Mapping on the output ─────────────────────────────────────────────────────

/-- Map a monadic function on the output of a fold. -/
@[inline] def rmapM [Monad m] (f : b → m c) (fld : Refold m x a b) : Refold m x a c where
  s := fld.s
  step s a := fld.step s a >>= Step.mapMStep f
  inject x := fld.inject x >>= Step.mapMStep f
  extract := fld.extract >=> f

-- ── Refolds ───────────────────────────────────────────────────────────────────

/-- Run `f` for each input purely for its effect, accumulating nothing. -/
@[inline] def drainBy [Monad m] (f : c → a → m b) : Refold m c a PUnit where
  s := c
  step c a := f c a *> pure (.Partial c)
  inject := (pure <| .Partial ·)
  extract _ := pure ⟨⟩

/-- Append the elements of the input stream to a provided starting value
    (Haskell's `Semigroup (<>)` rendered with Lean's `Append`). -/
@[inline] def sconcat [Monad m] [Append a] : Refold m a a a := foldl' (· ++ ·)

-- ── append ────────────────────────────────────────────────────────────────────

/-- Supply the output of the first consumer as the seed of the second. -/
@[inline] def append [Monad m] (r1 : Refold m x a b) (r2 : Refold m b a b) :
    Refold m x a b where
  s := r1.s ⊕ r2.s
  step
    | .inl s, a => r1.step s a >>= goLeft
    | .inr s, a => r2.step s a >>= fun r => pure <|
        match r with
        | .Partial s1 => .Partial (.inr s1)
        | .Done b => .Done b
  inject x := r1.inject x >>= goLeft
  extract
    | .inl s => r1.extract s
    | .inr s => r2.extract s
where
  goLeft : Step r1.s b → m (Step (r1.s ⊕ r2.s) b)
    | .Partial s => pure (.Partial (.inl s))
    | .Done b => r2.inject b >>= fun r => pure <|
        match r with
        | .Partial s => .Partial (.inr s)
        | .Done b1 => .Done b1

/-- Keep running the same consumer over and over, feeding each run's output as
    the seed of the next. -/
@[inline] def iterate [Monad m] (r1 : Refold m b a b) : Refold m b a b where
  s := r1.s
  step s a := r1.step s a >>= go
  inject x := r1.inject x >>= go
  extract := r1.extract
where
  go : Step r1.s b → m (Step r1.s b)
    | .Partial s => pure (.Partial s)
    | .Done b => r1.inject b

-- ── Transformation ────────────────────────────────────────────────────────────

/-- Take at most `n` input elements and fold them with the supplied refold. -/
@[inline] def take [Monad m] (n : Nat) (fld : Refold m x a b) : Refold m x a b where
  s := Tuple' Nat fld.s
  inject x := do
    match ← fld.inject x with
    | .Partial s => if n > 0 then pure (.Partial ⟨0, s⟩) else .Done <$> fld.extract s
    | .Done b => pure (.Done b)
  step := fun ⟨i, r⟩ a => do
    match ← fld.step r a with
    | .Partial sres =>
        let i1 := i + 1
        if i1 < n then pure (.Partial ⟨i1, sres⟩) else .Done <$> fld.extract sres
    | .Done bres => pure (.Done bres)
  extract := fun ⟨_, r⟩ => fld.extract r

end Data.Refold
