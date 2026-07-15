/-
  Linen.Data.Scanl.Type — the `Scanl` stateful left-scan type

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Scanl.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Scanl/Type.hs),
  module #13 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A `Scanl m a b` is a stateful left scan: `initial` produces the starting
  state, `step` folds each input advancing the state (or terminating with
  `Done`), `extract` reads the running result out of the state, and `final`
  reads the last result and cleans up. Unlike a `Fold`, a scan exposes an
  intermediate result at every step via `extract`.

  ## Substitutions / deviations

  - **Existential state via a structure field** — the same encoding used for
    `Refold` (#11): `data Scanl … = forall s. Scanl …` becomes a `structure`
    whose state type `s` is an implicit field, lifting it to `Type (max (u+1) v)`.
  - **`Step` is `Data.Fold.Step`** (`Partial`/`Done`, #8); `Fuse`/`Tuple'Fused`
    dropped (GHC-plugin markers) — `take`'s counter pair uses `Data.Tuple.Tuple'`.
  - **`toStreamK`/`toStreamKRev` are `unsafe`** because `Data.StreamK` (#12) is
    `unsafe` (its CPS/effectful type is non-strictly-positive; see that module).
  - **Only the core scan constructors and transformers are ported** (mkScanl*,
    mkScanr*, mkScant*, rmapM/Functor, lmap*, filter*, catMaybes, postscanl,
    take, drain, toStreamK*). The larger secondary combinator layer of the
    2000-line upstream module (takeEndBy family, teeing/distributing scans,
    demux/classify key-value scans, etc.) belongs to the later stream-operation
    tiers and is out of this `.Type`-core batch, matching the plan's own
    module-level scoping.
-/

import Linen.Data.Fold.Step
import Linen.Data.Tuple.Strict
import Linen.Data.Maybe.Strict
import Linen.Data.StreamK.Type

namespace Data.Scanl

open Data.Fold (Step)
open Data.Maybe (Maybe')
open Data.Tuple (Tuple')

-- ── The Scanl type ──────────────────────────────────────────────────────────

/-- A stateful left scan. The state type `s` is existentially hidden. -/
structure Scanl (m : Type u → Type v) (a b : Type u) where
  /-- The hidden accumulator-state type. -/
  {s : Type u}
  /-- Consume one input, advancing the state or terminating. -/
  step : s → a → m (Step s b)
  /-- The initial state (or an immediate `Done`). -/
  initial : m (Step s b)
  /-- Read the running result out of the state. -/
  extract : s → m b
  /-- Read the final result and clean up the state. -/
  final : s → m b

-- ── Mapping on the output ───────────────────────────────────────────────────

/-- Map a monadic function on the output of a scan. -/
@[inline] def rmapM [Monad m] (f : b → m c) (fld : Scanl m a b) : Scanl m a c where
  s := fld.s
  step s a := fld.step s a >>= Step.mapMStep f
  initial := fld.initial >>= Step.mapMStep f
  extract := fld.extract >=> f
  final := fld.final >=> f

/-- `Functor`: map over the output `b`. -/
instance [Functor m] : Functor (Scanl m a) where
  map f fld :=
    { s := fld.s
      step := fun s b => Functor.map (Functor.map f) (fld.step s b)
      initial := Functor.map (Functor.map f) fld.initial
      extract := fun s => Functor.map f (fld.extract s)
      final := fun s => Functor.map f (fld.final s) }

-- ── Left fold constructors ──────────────────────────────────────────────────

/-- Make a scan from a pure left-fold step function and an initial accumulator. -/
@[inline] def mkScanl [Monad m] (step : b → a → b) (initial : b) : Scanl m a b where
  s := b
  step s a := pure (.Partial (step s a))
  initial := pure (.Partial initial)
  extract := pure
  final := pure

/-- Make a scan from a monadic step function and initial accumulator. -/
@[inline] def mkScanlM [Monad m] (step : b → a → m b) (initial : m b) : Scanl m a b where
  s := b
  step s a := (.Partial ·) <$> step s a
  initial := (.Partial ·) <$> initial
  extract := pure
  final := pure

/-- Strict left scan for non-empty streams, using the first element as the
    starting value; `none` on an empty stream. -/
@[inline] def mkScanl1 [Monad m] (step : a → a → a) : Scanl m a (Option a) :=
  Functor.map Maybe'.toMaybe (mkScanl step1 Maybe'.Nothing')
where
  step1 : Maybe' a → a → Maybe' a
    | .Nothing', a => .Just' a
    | .Just' x, a => .Just' (step x a)

/-- Like `mkScanl1` but with a monadic step. -/
@[inline] def mkScanl1M [Monad m] (step : a → a → m a) : Scanl m a (Option a) :=
  Functor.map Maybe'.toMaybe (mkScanlM step1 (pure Maybe'.Nothing'))
where
  step1 : Maybe' a → a → m (Maybe' a)
    | .Nothing', a => pure (.Just' a)
    | .Just' x, a => .Just' <$> step x a

/-- Right-scan style constructor via a pure step and seed. -/
@[inline] def mkScanr [Monad m] (f : a → b → b) (z : b) : Scanl m a b :=
  Functor.map (· z) (mkScanl (fun g x => g ∘ f x) id)

/-- Make a terminating scan from pure step / initial / extract. -/
@[inline] def mkScant [Monad m] (step : s → a → Step s b) (initial : Step s b)
    (extract : s → b) : Scanl m a b where
  step s a := pure (step s a)
  initial := pure initial
  extract s := pure (extract s)
  final s := pure (extract s)

/-- Make a terminating scan from effectful step / initial / extract (the raw
    constructor, `final := extract`). -/
@[inline] def mkScantM (step : s → a → m (Step s b)) (initial : m (Step s b))
    (extract : s → m b) : Scanl m a b :=
  { step := step, initial := initial, extract := extract, final := extract }

-- ── Specific scans ──────────────────────────────────────────────────────────

/-- A scan that discards its input and produces `()`. -/
@[inline] def drain [Monad m] : Scanl m a PUnit := mkScanl (fun _ _ => ⟨⟩) ⟨⟩

-- ── Mapping on the input ────────────────────────────────────────────────────

/-- `lmap f scan` maps `f` on the input of the scan. -/
@[inline] def lmap (f : a → b) (fld : Scanl m b r) : Scanl m a r :=
  { fld with step := fun x a => fld.step x (f a) }

/-- `lmapM f scan` maps the monadic `f` on the input of the scan. -/
@[inline] def lmapM [Monad m] (f : a → m b) (fld : Scanl m b r) : Scanl m a r :=
  { fld with step := fun x a => f a >>= fld.step x }

-- ── Filtering ───────────────────────────────────────────────────────────────

/-- Feed only the `some` values of a `Option`-input to the underlying scan. -/
@[inline] def catMaybes [Monad m] (fld : Scanl m a b) : Scanl m (Option a) b :=
  { fld with step := fun s a =>
      match a with
      | none => pure (.Partial s)
      | some x => fld.step s x }

/-- A scan that keeps only the elements passing a predicate (as `Option`). -/
@[inline] def filtering [Monad m] (f : a → Bool) : Scanl m a (Option a) :=
  mkScanl (fun _ a => if f a then some a else none) none

/-- Include only elements that pass a predicate. -/
@[inline] def filter [Monad m] (f : a → Bool) (fld : Scanl m a r) : Scanl m a r :=
  { fld with step := fun x a => if f a then fld.step x a else pure (.Partial x) }

/-- Like `filter` but with a monadic predicate. -/
@[inline] def filterM [Monad m] (f : a → m Bool) (fld : Scanl m a r) : Scanl m a r :=
  { fld with step := fun x a => do
      if ← f a then fld.step x a else pure (.Partial x) }

-- ── Composition ─────────────────────────────────────────────────────────────

/-- Postscan the input of a scan through another scan (an append operation). -/
@[inline] def postscanl [Monad m] (l : Scanl m a b) (r : Scanl m b c) : Scanl m a c where
  s := l.s × r.s
  initial := do
    match ← r.initial with
    | .Partial sR =>
        match ← l.initial with
        | .Done _ => .Done <$> r.final sR
        | .Partial sL => pure (.Partial (sL, sR))
    | .Done b => pure (.Done b)
  step := fun (sL, sR) x => runStep (l.step sL x) sR
  extract := fun (_, sR) => r.extract sR
  final := fun (sL, sR) => l.final sL *> r.final sR
where
  runStep (actionL : m (Step l.s b)) (sR : r.s) : m (Step (l.s × r.s) c) := do
    match ← actionL with
    | .Done bL =>
        match ← r.step sR bL with
        | .Partial sR1 => .Done <$> r.final sR1
        | .Done bR => pure (.Done bR)
    | .Partial sL =>
        let b ← l.extract sL
        match ← r.step sR b with
        | .Partial sR1 => pure (.Partial (sL, sR1))
        | .Done bR => l.final sL *> pure (.Done bR)

/-- Postscan through a `Option`-producing scan, dropping `none`. -/
@[inline] def postscanlMaybe [Monad m] (f1 : Scanl m a (Option b)) (f2 : Scanl m b c) :
    Scanl m a c := postscanl f1 (catMaybes f2)

-- ── Trimming ────────────────────────────────────────────────────────────────

/-- Take at most `n` input elements and scan them with the supplied scan. -/
@[inline] def take [Monad m] (n : Int) (fld : Scanl m a b) : Scanl m a b where
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

-- ── To StreamK ──────────────────────────────────────────────────────────────

/-- Scan the input into a reversed `StreamK` (each element prepended). -/
@[inline] unsafe def toStreamKRev [Monad m] : Scanl m a (Data.StreamK.StreamK n a) :=
  mkScanl (fun s a => Data.StreamK.StreamK.cons a s) Data.StreamK.StreamK.nil

/-- Scan the input into a `StreamK` in order. -/
@[inline] unsafe def toStreamK [Monad m] : Scanl m a (Data.StreamK.StreamK n a) :=
  mkScanr Data.StreamK.StreamK.cons Data.StreamK.StreamK.nil

end Data.Scanl
