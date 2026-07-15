/-
  Linen.Data.Producer.Type — the `Producer` (an `Unfold` with an extractable seed)

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Producer.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Producer/Type.hs),
  module #17 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A `Producer m a b` generates a stream of `b`s from a seed `a`, like an
  `Unfold`, but additionally carries an `extract : s → m a` that recovers the
  (residual) seed from the internal state — so a producer is an *open* loop that
  can be stopped, examined, and resumed, whereas an `Unfold` is closed.

  ## Substitutions / deviations

  - **Existential state via a structure field**, as for `Unfold`/`Fold`/…
  - **`Step` is `Data.Stream.Step`** (`Yield`/`Skip`/`Stop`, #7). `Fuse`
    annotations dropped.
  - **`NestedLoop` is a public inductive** (`outerLoop`/`innerLoop`), used both
    as the seed type of `concat` and, at the internal state types, as `concat`'s
    fusion state — mirroring upstream (which reuses the same type for both).
-/

import Linen.Data.Stream.Step

namespace Data.Producer

-- `m`'s domain and codomain universes are independent by design, but always
-- co-occur syntactically in `unfoldrM`, so the linter can't tell they need
-- to stay free.
set_option linter.checkUnivs false

open Data.Stream (Step)

-- ── The Producer type ───────────────────────────────────────────────────────

/-- A generator of a `b`-stream from a seed `a`, whose internal state can be
    extracted back to a seed. The state type `s` is existentially hidden. -/
structure Producer (m : Type u → Type v) (a b : Type u) where
  /-- The hidden generator-state type. -/
  {s : Type u}
  /-- Advance the state, yielding a value, skipping, or stopping. -/
  step : s → m (Step s b)
  /-- Seed the initial state from the input. -/
  inject : a → m s
  /-- Recover the (residual) seed from the state. -/
  extract : s → m a

-- ── Producers ───────────────────────────────────────────────────────────────

/-- An empty producer that runs `f` on the seed for its effect, then stops. -/
@[inline] def nilM [Monad m] (f : a → m c) : Producer m a b where
  step x := f x *> pure .Stop
  inject := pure
  extract := pure

/-- An empty producer (no output, no effect). -/
@[inline] def nil [Monad m] : Producer m a b := nilM (fun _ => (pure ⟨⟩ : m PUnit))

/-- Unfold a monadic step from a seed; ends on `none`. -/
@[inline] def unfoldrM [Monad m] (next : a → m (Option (b × a))) : Producer m a b where
  step st := do
    match ← next st with
    | some (x, s) => pure (.Yield x s)
    | none => pure .Stop
  inject := pure
  extract := pure

/-- Convert a list of pure values to a producer. -/
@[inline] def fromList [Monad m] : Producer m (List a) a where
  step
    | x :: xs => pure (.Yield x xs)
    | [] => pure .Stop
  inject := pure
  extract := pure

-- ── Mapping ───────────────────────────────────────────────────────────────────

/-- Interconvert the producer between two interconvertible seed types. -/
@[inline] def translate [Functor m] (f : a → c) (g : c → a) (p : Producer m c b) :
    Producer m a b where
  s := p.s
  step := p.step
  inject := p.inject ∘ f
  extract s := g <$> p.extract s

/-- Map the producer seed to another value of the same type. -/
@[inline] def lmap (f : a → a) (p : Producer m a b) : Producer m a b :=
  { p with inject := p.inject ∘ f }

/-- Map a function on the output of the producer. -/
@[inline] def map [Functor m] (f : b → c) (p : Producer m a b) : Producer m a c :=
  { p with step := fun st => Functor.map (Functor.map f) (p.step st) }

/-- `Functor`: map over the output `b`. -/
instance [Functor m] : Functor (Producer m a) where
  map := map

-- ── Nesting ─────────────────────────────────────────────────────────────────

/-- A nested-loop state/seed: an outer loop, or an outer paired with an inner. -/
inductive NestedLoop (s1 s2 : Type u) where
  /-- Only the outer loop is running. -/
  | outerLoop : s1 → NestedLoop s1 s2
  /-- The outer and inner loops are both running. -/
  | innerLoop : s1 → s2 → NestedLoop s1 s2

/-- Apply the second producer to each output of the first, flattening the
    result (a nested loop). -/
@[inline] def concat {m : Type u → Type v} [Monad m] {a b c : Type u}
    (p1 : Producer m a b) (p2 : Producer m b c) :
    Producer m (NestedLoop a b) c where
  s := NestedLoop p1.s p2.s
  inject
    | .outerLoop x => (.outerLoop ·) <$> p1.inject x
    | .innerLoop x y => do
        let s1 ← p1.inject x
        let s2 ← p2.inject y
        pure (.innerLoop s1 s2)
  step
    | .outerLoop st => do
        match ← p1.step st with
        | .Yield x s => (fun innerSt => .Skip (.innerLoop s innerSt)) <$> p2.inject x
        | .Skip s => pure (.Skip (.outerLoop s))
        | .Stop => pure .Stop
    | .innerLoop ost ist => do
        match ← p2.step ist with
        | .Yield x s => pure (.Yield x (.innerLoop ost s))
        | .Skip s => pure (.Skip (.innerLoop ost s))
        | .Stop => pure (.Skip (.outerLoop ost))
  extract
    | .outerLoop s1 => (.outerLoop ·) <$> p1.extract s1
    | .innerLoop s1 s2 => do
        let r1 ← p1.extract s1
        let r2 ← p2.extract s2
        pure (.innerLoop r1 r2)

end Data.Producer
