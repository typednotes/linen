/-
  Linen.Data.Unfold.Type — the `Unfold` seed→stream generator

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Unfold.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Unfold/Type.hs),
  module #15 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  An `Unfold m a b` is a stream generator: `inject` turns a seed `a` into an
  internal state, and `step` advances the state producing `Yield`/`Skip`/`Stop`
  (`Data.Stream.Step`, #7). It is the dual of a `Fold` and composes/fuses better
  than a bare `a → Stream m b` in nested loops.

  ## Substitutions / deviations

  - **Existential state via a structure field**, as for `Fold`/`Scanl`/`Refold`.
  - **`Step` is `Data.Stream.Step`** (`Yield`/`Skip`/`Stop`, #7).
  - **No `Applicative`/`Monad`.** Upstream provides none (its `crossApplySnd`/
    `crossApplyFst` are `undefined`); only `Functor` is lawful and ported.
  - **Local fusion-state types inlined** as `Option`/`Bool`/`Sum`/`Prod` (their
    `Fuse` annotations are GHC-plugin no-ops).
  - **Core scope:** the type, constructors (`mkUnfoldM`/`mkUnfoldrM`/`unfoldrM`/
    `unfoldr`/`function{,M}`/`identity`/`fromEffect`/`fromPure`/`fromList`/
    `fromTuple`), input maps (`lmap`/`lmapM`/`supply`), output maps
    (`map`/`Functor`/`mapM`), `takeWhile{,M}`, and the cross product
    (`crossWithM`/`crossWith`/`cross`). The larger nesting/`unfoldEach`/`many`/
    fair-cross layer belongs to later tiers.
-/

import Linen.Data.Stream.Step

namespace Data.Unfold

-- `m`'s domain and codomain universes are independent by design, but always
-- co-occur syntactically in `unfoldr`/`unfoldrM`, so the linter can't tell
-- they need to stay free.
set_option linter.checkUnivs false

open Data.Stream (Step)

-- ── The Unfold type ─────────────────────────────────────────────────────────

/-- A seed→stream generator: `inject` seeds the state, `step` advances it,
    yielding values. The state type `s` is existentially hidden. -/
structure Unfold (m : Type u → Type v) (a b : Type u) where
  /-- The hidden generator-state type. -/
  {s : Type u}
  /-- Advance the state, yielding a value, skipping, or stopping. -/
  step : s → m (Step s b)
  /-- Seed the initial state from the input. -/
  inject : a → m s

-- ── Basic constructors ──────────────────────────────────────────────────────

/-- Make an unfold from `step` and `inject` functions. -/
@[inline] def mkUnfoldM (step : s → m (Step s b)) (inject : a → m s) : Unfold m a b :=
  { step := step, inject := inject }

/-- Make an unfold from a step function (seed is the state itself). -/
@[inline] def mkUnfoldrM [Applicative m] (step : a → m (Step a b)) : Unfold m a b :=
  { step := step, inject := pure }

/-- Unfold a monadic step from a seed; ends on `none`. -/
@[inline] def unfoldrM [Applicative m] (next : a → m (Option (b × a))) : Unfold m a b where
  step st := (fun r => match r with
    | some (x, s) => .Yield x s
    | none => .Stop) <$> next st
  inject := pure

/-- Unfold a pure step from a seed; ends on `none`. -/
@[inline] def unfoldr [Applicative m] (step : a → Option (b × a)) : Unfold m a b :=
  unfoldrM (fun x => pure (step x))

-- ── Mapping the input ───────────────────────────────────────────────────────

/-- Map a function on the input (seed) of the unfold. -/
@[inline] def lmap (f : a → c) (u : Unfold m c b) : Unfold m a b :=
  { u with inject := u.inject ∘ f }

/-- Map a monadic action on the input of the unfold. -/
@[inline] def lmapM [Monad m] (f : a → m c) (u : Unfold m c b) : Unfold m a b :=
  { u with inject := f >=> u.inject }

/-- Supply the seed, closing the input end of the unfold. -/
@[inline] def supply (x : a) (u : Unfold m a b) : Unfold m PUnit b :=
  lmap (fun _ => x) u

-- ── Mapping the output ──────────────────────────────────────────────────────

/-- Map a function on the output of the unfold. -/
@[inline] def map [Functor m] (f : b → c) (u : Unfold m a b) : Unfold m a c :=
  { u with step := fun st => Functor.map (Functor.map f) (u.step st) }

/-- `Functor`: map over the output `b`. -/
instance [Functor m] : Functor (Unfold m a) where
  map := map

/-- Map a monadic action on the output of the unfold. -/
@[inline] def mapM [Monad m] (f : b → m c) (u : Unfold m a b) : Unfold m a c where
  s := u.s
  inject := u.inject
  step st := do
    match ← u.step st with
    | .Yield x s => (fun a => .Yield a s) <$> f x
    | .Skip s => pure (.Skip s)
    | .Stop => pure .Stop

-- ── From values ─────────────────────────────────────────────────────────────

/-- The unfold discards its input and generates a singleton from an effect. -/
@[inline] def fromEffect [Applicative m] (act : m b) : Unfold m a b where
  s := Bool
  inject _ := pure false
  step
    | false => (fun x => .Yield x true) <$> act
    | true => pure .Stop

/-- Discard the input and always generate the singleton `x`. -/
@[inline] def fromPure [Applicative m] (x : b) : Unfold m a b := fromEffect (pure x)

/-- Lift a monadic function into a singleton-stream unfold. -/
@[inline] def functionM [Applicative m] (f : a → m b) : Unfold m a b where
  s := Option a
  inject x := pure (some x)
  step
    | some x => (fun b => .Yield b none) <$> f x
    | none => pure .Stop

/-- Lift a pure function into a singleton-stream unfold. -/
@[inline] def function [Applicative m] (f : a → b) : Unfold m a b :=
  functionM (fun x => pure (f x))

/-- The identity unfold: the seed becomes the single output element. -/
@[inline] def identity [Applicative m] : Unfold m a a := function id

/-- Convert a list of pure values to a stream. -/
@[inline] def fromList [Applicative m] : Unfold m (List a) a where
  s := List a
  inject := pure
  step
    | x :: xs => pure (.Yield x xs)
    | [] => pure .Stop

/-- Local three-state type for `fromTuple`. -/
inductive TupleState (a : Type u) where
  | both : a → a → TupleState a
  | one : a → TupleState a
  | none : TupleState a

/-- Convert a pair to a two-element stream. -/
@[inline] def fromTuple [Applicative m] : Unfold m (a × a) a where
  s := TupleState a
  inject := fun (x, y) => pure (.both x y)
  step
    | .both x y => pure (.Yield x (.one y))
    | .one y => pure (.Yield y .none)
    | .none => pure .Stop

-- ── Trimming ────────────────────────────────────────────────────────────────

/-- End the stream as soon as the monadic predicate fails on an element. -/
@[inline] def takeWhileM [Monad m] (f : b → m Bool) (u : Unfold m a b) : Unfold m a b :=
  { u with step := fun st => do
      match ← u.step st with
      | .Yield x s => do if ← f x then pure (.Yield x s) else pure .Stop
      | .Skip s => pure (.Skip s)
      | .Stop => pure .Stop }

/-- End the stream as soon as the predicate fails on an element. -/
@[inline] def takeWhile [Monad m] (f : b → Bool) (u : Unfold m a b) : Unfold m a b :=
  takeWhileM (fun x => pure (f x)) u

-- ── Cross product ─────────────────────────────────────────────────────────────

/-- Cross product of two unfolds combining outputs with the monadic `f`. The
    fusion state is `(a × s₁) ⊕ (a × s₁ × b × s₂)` (upstream `CrossOuter`/
    `CrossInner`). -/
@[inline] def crossWithM {m : Type u → Type v} [Monad m] {a b c d : Type u}
    (f : b → c → m d) (u1 : Unfold m a b) (u2 : Unfold m a c) : Unfold m a d where
  s := (a × u1.s) ⊕ (a × u1.s × b × u2.s)
  inject a := do
    let s1 ← u1.inject a
    pure (.inl (a, s1))
  step
    | .inl (a, s1) => do
        match ← u1.step s1 with
        | .Yield b s => do
            let s2 ← u2.inject a
            pure (.Skip (.inr (a, s, b, s2)))
        | .Skip s => pure (.Skip (.inl (a, s)))
        | .Stop => pure .Stop
    | .inr (a, s1, b, s2) => do
        match ← u2.step s2 with
        | .Yield c s => (fun d => .Yield d (.inr (a, s1, b, s))) <$> f b c
        | .Skip s => pure (.Skip (.inr (a, s1, b, s)))
        | .Stop => pure (.Skip (.inl (a, s1)))

/-- Cross product combining outputs with a pure `f`. -/
@[inline] def crossWith {m : Type u → Type v} [Monad m] {a b c d : Type u}
    (f : b → c → d) (u1 : Unfold m a b) (u2 : Unfold m a c) : Unfold m a d :=
  crossWithM (fun b c => pure (f b c)) u1 u2

/-- Cross product of two unfolds as pairs. -/
@[inline] def cross {m : Type u → Type v} [Monad m] {a b c : Type u}
    (u1 : Unfold m a b) (u2 : Unfold m a c) : Unfold m a (b × c) :=
  crossWith Prod.mk u1 u2

end Data.Unfold
