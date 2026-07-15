/-
  Linen.Data.Stream.Lift — transform the inner monad of a fused stream

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Stream.Lift`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Stream/Lift.hs)
  together with the monad-transformer specialisations from
  `Streamly.Internal.Data.Stream.Transformer` (`liftInner`, `runReaderT`,
  `evalStateT`, `runStateT`, `withReaderT`, `usingReaderT`, `usingStateT`,
  `foldlT`), module #23 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  These change or run the *inner monad* of a `Stream m a` (#19): hoisting via a
  natural transformation, lifting into a transformer, and evaluating a
  `ReaderT`/`StateT` layer.

  ## Substitutions / deviations

  - **Rank-2 natural transformations become `{β : Type} → m β → n β` function
    arguments** (`morphInner`/`liftInnerWith`/`runInnerWith`).
  - **`Identity` → Lean `Id`** for `generalizeInner`.
  - **Transformer specialisations over Lean's `ReaderT`/`StateT`** (core), using
    `ReaderT.run`/`StateT.run`/`ReaderT.adapt` and `MonadLift` — the same stack
    the `Mtl` port relies on. `liftInner` is generic over any `[MonadLift m n]`.
  - No universe wall here: every signature keeps the residual `Stream` in the
    *result* position (`Stream n a`, `Stream m (σ × a)`), never inside `m`.
  - **`foldlT` is `unsafe`** — it drives the `Skip` loop (as the `Stream.Type`
    drivers do); `foldrT` (a lazy right fold to a transformer) is omitted as its
    laziness has no eager-Lean analogue that adds over `foldlT`.
-/

import Linen.Data.Stream.Type

namespace Data.Stream

open Data.Stream (Step State defState adaptState)

namespace Stream

variable {m n : Type → Type} {a : Type}

-- ── Generalize the inner monad ───────────────────────────────────────────────

/-- Transform the inner monad by a natural transformation (a.k.a. `hoist`). -/
@[inline] def morphInner (f : {β : Type} → m β → n β) (t : Stream m a) : Stream n a where
  s := t.s
  step gst st := f (t.step (adaptState gst) st)
  state := t.state

/-- Generalize the inner monad from `Id` to any monad. -/
@[inline] def generalizeInner [Monad m] (t : Stream Id a) : Stream m a :=
  morphInner (fun x => pure (Id.run x)) t

-- ── Transform the inner monad ────────────────────────────────────────────────

/-- Lift the inner monad `m` to `n` using the supplied lift function. -/
@[inline] def liftInnerWith (lift : {β : Type} → m β → n β) (t : Stream m a) : Stream n a where
  s := t.s
  step gst st := lift (t.step (adaptState gst) st)
  state := t.state

/-- Evaluate the inner monad `n` down to `m` using the supplied runner. -/
@[inline] def runInnerWith (run : {β : Type} → n β → m β) (t : Stream n a) : Stream m a where
  s := t.s
  step gst st := run (t.step (adaptState gst) st)
  state := t.state

/-- Evaluate the inner monad using a stateful runner: the state threaded out of
    one step is fed into the next; each yielded value is paired with the state. -/
@[inline] def runInnerWithState [Monad m] {σ : Type}
    (run : {β : Type} → σ → n β → m (β × σ)) (initial : m σ) (t : Stream n a) :
    Stream m (σ × a) where
  s := t.s × m σ
  step gst := fun (st, action) => do
    let sv ← action
    let (r, sv1) ← run sv (t.step (adaptState gst) st)
    pure (match r with
      | .Yield x s => Step.Yield (sv1, x) (s, pure sv1)
      | .Skip s => Step.Skip (s, pure sv1)
      | .Stop => Step.Stop)
  state := (t.state, initial)

-- ── Lift into a transformer ──────────────────────────────────────────────────

/-- Lift the inner monad `m` into `n` via its `MonadLift` instance. -/
@[inline] def liftInner [MonadLift m n] (t : Stream m a) : Stream n a :=
  liftInnerWith (fun x => MonadLift.monadLift x) t

-- ── ReaderT ──────────────────────────────────────────────────────────────────

/-- Evaluate the inner monad of a stream as `ReaderT ρ m`, supplying the
    environment. -/
@[inline] def runReaderT [Monad m] {ρ : Type} (env : m ρ) (t : Stream (ReaderT ρ m) a) :
    Stream m a where
  s := t.s × m ρ
  step gst := fun (st, action) => do
    let sv ← action
    let r ← (t.step (adaptState gst) st).run sv
    pure (match r with
      | .Yield x s => Step.Yield x (s, pure sv)
      | .Skip s => Step.Skip (s, pure sv)
      | .Stop => Step.Stop)
  state := (t.state, env)

/-- Modify the environment of the underlying `ReaderT`. -/
@[inline] def withReaderT [Monad m] {ρ₁ ρ₂ : Type} (f : ρ₂ → ρ₁)
    (t : Stream (ReaderT ρ₁ m) a) : Stream (ReaderT ρ₂ m) a where
  s := t.s
  step gst st := (t.step (adaptState gst) st).adapt f
  state := t.state

/-- Run a stream transformation in a given `ReaderT` environment. -/
@[inline] def usingReaderT [Monad m] {ρ : Type} (env : m ρ)
    (f : Stream (ReaderT ρ m) a → Stream (ReaderT ρ m) a) (xs : Stream m a) : Stream m a :=
  runReaderT env (f (liftInner xs))

-- ── StateT ───────────────────────────────────────────────────────────────────

/-- Evaluate the inner monad as `StateT σ m`, discarding the final state. -/
@[inline] def evalStateT [Monad m] {σ : Type} (init : m σ) (t : Stream (StateT σ m) a) :
    Stream m a where
  s := t.s × m σ
  step gst := fun (st, action) => do
    let sv ← action
    let (r, sv') ← (t.step (adaptState gst) st).run sv
    pure (match r with
      | .Yield x s => Step.Yield x (s, pure sv')
      | .Skip s => Step.Skip (s, pure sv')
      | .Stop => Step.Stop)
  state := (t.state, init)

/-- Evaluate the inner monad as `StateT σ m`, emitting the running state with
    each value. -/
@[inline] def runStateT [Monad m] {σ : Type} (init : m σ) (t : Stream (StateT σ m) a) :
    Stream m (σ × a) where
  s := t.s × m σ
  step gst := fun (st, action) => do
    let sv ← action
    let (r, sv') ← (t.step (adaptState gst) st).run sv
    pure (match r with
      | .Yield x s => Step.Yield (sv', x) (s, pure sv')
      | .Skip s => Step.Skip (s, pure sv')
      | .Stop => Step.Stop)
  state := (t.state, init)

/-- Run a stateful (`StateT`) stream transformation with a given initial state. -/
@[inline] def usingStateT [Monad m] {σ : Type} (init : m σ)
    (f : Stream (StateT σ m) a → Stream (StateT σ m) a) (xs : Stream m a) : Stream m a :=
  evalStateT init (f (liftInner xs))

-- ── Fold to a transformer monad ──────────────────────────────────────────────

/-- Strict left fold whose accumulator lives in a transformer monad `n` over
    `m`. `unsafe` — it drives the `Skip` loop. -/
@[specialize] unsafe def foldlT [Monad m] [Monad n] [MonadLift m n] {b : Type}
    (fstep : n b → a → n b) (begin : n b) (t : Stream m a) : n b :=
  go begin t.state
where
  go (acc : n b) (st : t.s) : n b := do
    let r ← (MonadLift.monadLift (t.step defState st) : n (Step t.s a))
    match r with
    | .Yield x s => go (fstep acc x) s
    | .Skip s => go acc s
    | .Stop => acc

end Stream
end Data.Stream
