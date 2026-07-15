/-
  Linen.Data.Stream.Generate — fused-stream generators

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Stream.Generate`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Stream/Generate.hs),
  module #20 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Generators that build a fused `Stream m a` (#19) directly from a seed, a
  count, an index function, or an iteration step.

  ## Overlap with `Data.Stream.Type` (#19)

  `nil`, `fromList`, `fromPure`/`fromEffect`, `consM` and `unfold` (from an
  `Unfold`) are already defined on `Stream.Type` by the Tier-2 port and are
  **not re-ported here** — use them from that module. This module adds the
  remaining generators. Note `Stream.Generate.unfoldr`/`unfoldrM` build a
  stream *directly* from a `seed → Option (a × seed)` step (distinct from the
  `Unfold`-valued `Data.Unfold.unfoldr`).

  ## Substitutions / deviations

  - Every generator here is a plain total `def` — a generator merely *assembles*
    a stepper and never drives the `Skip` loop, so none needs `unsafe` (unlike
    the consumers in `Eliminate`). Infinite generators (`repeatValue`/`iterate`)
    are total to *build*; only *driving* them would diverge.
  - **`repeat` → `repeatValue`** and the enumeration `from`/`to` arguments →
    `start`/`stop`: `repeat`/`from` are Lean keywords, renamed per the Lean-ify
    convention.
  - **Counts use `Nat`** (upstream `Int` with an `i <= 0` guard); indices use
    `Nat` (upstream `Int`).
  - **Enumeration:** only the `Integral`-style enumerators are ported, generic
    over `[Add a]` (`+`) / `[One a]` (`1`); the `Num`/`Fractional`/`Bounded`
    `Enum` machinery, the `Enumerable` class, and the `times`/`durations`
    clock generators (needing `MonadIO`/time) belong to later batches.
  - **`fromPtr`/`fromCString#`/`fromByteStr#` dropped** — GHC-primop raw-memory
    generators with no Lean analogue.
-/

import Linen.Data.Stream.Type

namespace Data.Stream

open Data.Stream (Step State)

namespace Stream

-- ── Consing ──────────────────────────────────────────────────────────────────

/-- Prepend a pure element to a stream (`cons`). The `consM` sibling that
    prepends an *effect* already lives on `Stream.Type`. -/
@[inline] def cons [Applicative m] (x : a) (t : Stream m a) : Stream m a where
  s := Option t.s
  step gst := fun
    | none => pure (Step.Yield x (some t.state))
    | some st => (fun r => match r with
        | .Yield a s => Step.Yield a (some s)
        | .Skip s => Step.Skip (some s)
        | .Stop => Step.Stop) <$> t.step gst st
  state := none

@[inherit_doc] scoped infixr:60 " .:. " => cons

-- ── Unfolding ────────────────────────────────────────────────────────────────

/-- Build a stream by unfolding a monadic step from a seed; ends on `none`. -/
@[inline] def unfoldrM [Functor m] (next : σ → m (Option (a × σ))) (seed : σ) :
    Stream m a where
  s := σ
  step _ st := (fun r => match r with
    | some (x, s) => Step.Yield x s
    | none => Step.Stop) <$> next st
  state := seed

/-- Build a stream by unfolding a pure step from a seed; ends on `none`. -/
@[inline] def unfoldr [Applicative m] (f : σ → Option (a × σ)) (seed : σ) : Stream m a :=
  unfoldrM (fun x => pure (f x)) seed

-- ── From values ──────────────────────────────────────────────────────────────

/-- Generate a stream by repeatedly running a monadic action forever. -/
@[inline] def repeatM [Functor m] (act : m a) : Stream m a where
  s := PUnit
  step _ _ := (fun r => Step.Yield r ⟨⟩) <$> act
  state := ⟨⟩

/-- Generate an infinite stream repeating a pure value (upstream `repeat`). -/
@[inline] def repeatValue [Applicative m] (x : a) : Stream m a :=
  repeatM (pure x)

/-- Generate a stream by performing a monadic action `n` times. -/
@[inline] def replicateM [Monad m] (n : Nat) (act : m a) : Stream m a where
  s := Nat
  step _ i := if i == 0 then pure Step.Stop else (fun x => Step.Yield x (i - 1)) <$> act
  state := n

/-- Generate a stream of length `n` by repeating a value. -/
@[inline] def replicate [Monad m] (n : Nat) (x : a) : Stream m a :=
  replicateM n (pure x)

-- ── Iteration ────────────────────────────────────────────────────────────────

/-- Infinite stream: first element from `act`, each next by applying the monadic
    `f` to the previous element. -/
@[inline] def iterateM [Functor m] (f : a → m a) (act : m a) : Stream m a where
  s := m a
  step _ st := (fun x => Step.Yield x (f x)) <$> st
  state := act

/-- Infinite stream: `x`, `f x`, `f (f x)`, … (upstream `iterate`). -/
@[inline] def iterateValue [Applicative m] (f : a → a) (x : a) : Stream m a :=
  iterateM (fun y => pure (f y)) (pure x)

-- ── From index functions ─────────────────────────────────────────────────────

/-- Generate an infinite stream by applying the monadic `gen` to `0, 1, 2, …`. -/
@[inline] def fromIndicesM [Functor m] (gen : Nat → m a) : Stream m a where
  s := Nat
  step _ i := (fun x => Step.Yield x (i + 1)) <$> gen i
  state := 0

/-- Generate an infinite stream by applying `gen` to `0, 1, 2, …`. -/
@[inline] def fromIndices [Applicative m] (gen : Nat → a) : Stream m a :=
  fromIndicesM (fun i => pure (gen i))

/-- Generate a stream of length `n` by applying the monadic `gen` to `0 … n-1`. -/
@[inline] def generateM [Applicative m] (n : Nat) (gen : Nat → m a) : Stream m a where
  s := Nat
  step _ i := if i < n then (fun x => Step.Yield x (i + 1)) <$> gen i else pure Step.Stop
  state := 0

/-- Generate a stream of length `n` by applying `gen` to `0 … n-1`. -/
@[inline] def generate [Applicative m] (n : Nat) (gen : Nat → a) : Stream m a :=
  generateM n (fun i => pure (gen i))

-- ── From containers ──────────────────────────────────────────────────────────

/-- Convert a list of monadic actions to a stream. -/
@[inline] def fromListM [Applicative m] (l : List (m a)) : Stream m a where
  s := List (m a)
  step _ := fun
    | act :: acts => (fun x => Step.Yield x acts) <$> act
    | [] => pure Step.Stop
  state := l

/-- Construct a stream from a list of pure values via `cons`/`nil`
    (`foldr cons nil`). Equivalent to `Stream.Type.fromList`. -/
@[inline] def fromFoldable [Applicative m] (l : List a) : Stream m a :=
  l.foldr cons nil

-- ── Enumeration (Integral) ───────────────────────────────────────────────────

/-- Enumerate from `start` in increments of `stride`, forever. -/
@[inline] def enumerateFromStepIntegral [Applicative m] [Add a] (start stride : a) :
    Stream m a where
  s := a
  step _ x := pure (Step.Yield x (x + stride))
  state := start

/-- Enumerate an integral type from `start` up to `stop` in increments of `1`. -/
@[inline] def enumerateFromToIntegral [Monad m] [Add a] [One a] [LE a] [DecidableLE a]
    (start stop : a) : Stream m a :=
  takeWhile (fun x => decide (x ≤ stop)) (enumerateFromStepIntegral start (1 : a))

end Stream
end Data.Stream
