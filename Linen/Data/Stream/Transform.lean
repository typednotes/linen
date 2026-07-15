/-
  Linen.Data.Stream.Transform — fused-stream transforms

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Stream.Transform`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Stream/Transform.hs),
  module #22 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Element-wise transforms of a `Stream m a` (#19): mapping side effects,
  scanning through a `Scanl`/`Fold`, filtering, dropping, interspersing and
  indexing. Each is stream→stream and only *reassembles* a stepper, so it is a
  total `def` — except `reverse`, which must drive the stream to materialize it.

  ## Overlap with `Data.Stream.Type` (#19)

  `map`/`mapM` and `take`/`takeWhile`/`takeWhileM` are already on `Stream.Type`
  and are **not re-ported here**; this module adds the complementary transforms.

  ## Substitutions / deviations

  - **`scan`/`postscan` (Fold) and `scanl`/`postscanl` (Scanl)** share one
    fused `ScanState` machine. `scan`/`scanl` include the initial accumulator;
    `postscan`/`postscanl` omit it. The `Fold`-taking versions are the `Scanl`
    versions applied to the field-identical `Fold` (both carry a
    `Data.Fold.Step` accumulator whose state type is `Type 0`, so no universe
    wall — the scan state legitimately stores the fold state).
  - **`Either` → Lean `Sum`** for `catLefts`/`catRights`/`catEithers`.
  - **`reverse` is `unsafe`** (it drives the stream via a strict left fold to
    materialize the reversed list before re-emitting) — the sanctioned `unsafe`
    driver, as on `Stream.Type`.
  - **Scope.** The headline transforms are ported (`sequence`, the scan family,
    `filter{,M}`, `mapMaybe{,M}`/`catMaybes`, `drop`/`dropWhile{,M}`,
    `intersperse{,M}`, `indexed`/`indexedR`, `findIndices`/`elemIndices`,
    `catLefts`/`catRights`/`catEithers`, `uniq`/`uniqBy`,
    `rollingMap`/`rollingMapM`, `reverse`). The large secondary layer —
    `tap`/`trace`, time-`delay`/`timestamp` (needing `MonadIO`/clocks), the
    prescan/`intersperseEndBy*` family, `splitOn`/`reassembleBy`,
    `pipe`/`foldrS`/`foldlS`, `reverseUnbox` (array-backed) — belongs to later
    batches, matching the plan's scoping.
-/

import Linen.Data.Stream.Type
import Linen.Data.Scanl.Type

namespace Data.Stream

open Data.Stream (Step State)
open Data.Scanl (Scanl)
open Data.Fold (Fold)

namespace Stream

-- ── Sequencing effects ───────────────────────────────────────────────────────

/-- Run each yielded action for its result. -/
@[inline] def sequence [Monad m] (t : Stream m (m a)) : Stream m a where
  s := t.s
  step gst st := do
    match ← t.step gst st with
    | .Yield x s => (fun a => Step.Yield a s) <$> x
    | .Skip s => pure (Step.Skip s)
    | .Stop => pure Step.Stop
  state := t.state

-- ── Scanning ─────────────────────────────────────────────────────────────────

/-- Fusion state of the scanning drivers: seeding, running (carrying the
    upstream state `st` and the scan/fold accumulator `fs`), or finished. -/
inductive ScanState (σ φ : Type) where
  /-- Not yet initialised. -/
  | init : σ → ScanState σ φ
  /-- Running: upstream state and accumulator. -/
  | run : σ → φ → ScanState σ φ
  /-- Finished. -/
  | done : ScanState σ φ

/-- Postscan a stream through a `Scanl` (omitting the initial accumulator). -/
@[inline] def postscanl [Monad m] (sc : Scanl m a b) (t : Stream m a) : Stream m b where
  s := ScanState t.s sc.s
  step gst := fun
    | .init st => do
        match ← sc.initial with
        | .Partial fs => pure (Step.Skip (.run st fs))
        | .Done b => pure (Step.Yield b .done)
    | .run st fs => do
        match ← t.step gst st with
        | .Yield x s => do
            match ← sc.step fs x with
            | .Partial fs1 => do let b ← sc.extract fs1; pure (Step.Yield b (.run s fs1))
            | .Done b => pure (Step.Yield b .done)
        | .Skip s => pure (Step.Skip (.run s fs))
        | .Stop => sc.final fs *> pure Step.Stop
    | .done => pure Step.Stop
  state := .init t.state

/-- Scan a stream through a `Scanl`, including the initial accumulator. -/
@[inline] def scanl [Monad m] (sc : Scanl m a b) (t : Stream m a) : Stream m b where
  s := ScanState t.s sc.s
  step gst := fun
    | .init st => do
        match ← sc.initial with
        | .Partial fs => do let b ← sc.extract fs; pure (Step.Yield b (.run st fs))
        | .Done b => pure (Step.Yield b .done)
    | .run st fs => do
        match ← t.step gst st with
        | .Yield x s => do
            match ← sc.step fs x with
            | .Partial fs1 => do let b ← sc.extract fs1; pure (Step.Yield b (.run s fs1))
            | .Done b => pure (Step.Yield b .done)
        | .Skip s => pure (Step.Skip (.run s fs))
        | .Stop => sc.final fs *> pure Step.Stop
    | .done => pure Step.Stop
  state := .init t.state

/-- View a `Fold` as a `Scanl` (identical field layout). -/
@[inline] def foldToScanl (fld : Fold m a b) : Scanl m a b where
  s := fld.s
  step := fld.step
  initial := fld.initial
  extract := fld.extract
  final := fld.final

/-- Scan a stream through a `Fold`, including the initial accumulator. -/
@[inline] def scan [Monad m] (fld : Fold m a b) (t : Stream m a) : Stream m b :=
  scanl (foldToScanl fld) t

/-- Postscan a stream through a `Fold` (omitting the initial accumulator). -/
@[inline] def postscan [Monad m] (fld : Fold m a b) (t : Stream m a) : Stream m b :=
  postscanl (foldToScanl fld) t

/-- Strict left scan from a monadic step and seed: yields the seed, then the
    accumulator after each element. -/
@[inline] def scanlM [Monad m] (fstep : b → a → m b) (begin : m b) (t : Stream m a) :
    Stream m b where
  s := Option (t.s × b)
  step gst := fun
    | none => do let x ← begin; pure (Step.Yield x (some (t.state, x)))
    | some (st, acc) => do
        match ← t.step gst st with
        | .Yield x s => do let y ← fstep acc x; pure (Step.Yield y (some (s, y)))
        | .Skip s => pure (Step.Skip (some (s, acc)))
        | .Stop => pure Step.Stop
  state := none

/-- Strict left scan from a pure step and seed. -/
@[inline] def scanl' [Monad m] (fstep : b → a → b) (seed : b) (t : Stream m a) : Stream m b :=
  scanlM (fun acc x => pure (fstep acc x)) (pure seed) t

-- ── Filtering ────────────────────────────────────────────────────────────────

/-- Keep only elements passing a monadic predicate. -/
@[inline] def filterM [Monad m] (f : a → m Bool) (t : Stream m a) : Stream m a where
  s := t.s
  step gst st := do
    match ← t.step gst st with
    | .Yield x s => do if ← f x then pure (Step.Yield x s) else pure (Step.Skip s)
    | .Skip s => pure (Step.Skip s)
    | .Stop => pure Step.Stop
  state := t.state

/-- Keep only elements passing a predicate. -/
@[inline] def filter [Monad m] (f : a → Bool) (t : Stream m a) : Stream m a :=
  filterM (fun x => pure (f x)) t

/-- Map a monadic `Option`-producer over the stream, dropping the `none`s. -/
@[inline] def mapMaybeM [Monad m] (f : a → m (Option b)) (t : Stream m a) : Stream m b where
  s := t.s
  step gst st := do
    match ← t.step gst st with
    | .Yield x s => do
        match ← f x with
        | some y => pure (Step.Yield y s)
        | none => pure (Step.Skip s)
    | .Skip s => pure (Step.Skip s)
    | .Stop => pure Step.Stop
  state := t.state

/-- Map an `Option`-producer over the stream, dropping the `none`s. -/
@[inline] def mapMaybe [Monad m] (f : a → Option b) (t : Stream m a) : Stream m b :=
  mapMaybeM (fun x => pure (f x)) t

/-- Keep only the `some` values of a stream of `Option`s. -/
@[inline] def catMaybes [Monad m] (t : Stream m (Option a)) : Stream m a :=
  mapMaybe (fun o => o) t

-- ── Dropping ─────────────────────────────────────────────────────────────────

/-- Drop the first `n` elements. -/
@[inline] def drop [Monad m] (n : Nat) (t : Stream m a) : Stream m a where
  s := t.s × Nat
  step gst := fun (st, i) => do
    match ← t.step gst st with
    | .Yield x s => if i == 0 then pure (Step.Yield x (s, 0)) else pure (Step.Skip (s, i - 1))
    | .Skip s => pure (Step.Skip (s, i))
    | .Stop => pure Step.Stop
  state := (t.state, n)

/-- Drop the longest prefix whose elements satisfy the monadic predicate. -/
@[inline] def dropWhileM [Monad m] (f : a → m Bool) (t : Stream m a) : Stream m a where
  s := t.s × Bool
  step gst := fun (st, dropping) => do
    match ← t.step gst st with
    | .Yield x s =>
        if dropping then
          (do if ← f x then pure (Step.Skip (s, true)) else pure (Step.Yield x (s, false)))
        else pure (Step.Yield x (s, false))
    | .Skip s => pure (Step.Skip (s, dropping))
    | .Stop => pure Step.Stop
  state := (t.state, true)

/-- Drop the longest prefix whose elements satisfy the predicate. -/
@[inline] def dropWhile [Monad m] (f : a → Bool) (t : Stream m a) : Stream m a :=
  dropWhileM (fun x => pure (f x)) t

-- ── Interspersing ────────────────────────────────────────────────────────────

/-- Fusion state for `intersperseM`. -/
inductive InterState (σ a : Type) where
  /-- Waiting to emit the very first element. -/
  | first : σ → InterState σ a
  /-- Emitted a value; pull the next (a separator will precede it). -/
  | pull : σ → InterState σ a
  /-- A separator was just emitted; emit the buffered element next. -/
  | buf : a → σ → InterState σ a

/-- Insert a monadic separator between successive elements. -/
@[inline] def intersperseM [Monad m] (sep : m a) (t : Stream m a) : Stream m a where
  s := InterState t.s a
  step gst := fun
    | .first st => do
        match ← t.step gst st with
        | .Yield x s => pure (Step.Yield x (.pull s))
        | .Skip s => pure (Step.Skip (.first s))
        | .Stop => pure Step.Stop
    | .pull st => do
        match ← t.step gst st with
        | .Yield x s => do let sv ← sep; pure (Step.Yield sv (.buf x s))
        | .Skip s => pure (Step.Skip (.pull s))
        | .Stop => pure Step.Stop
    | .buf x s => pure (Step.Yield x (.pull s))
  state := .first t.state

/-- Insert a separator element between successive elements. -/
@[inline] def intersperse [Monad m] (sep : a) (t : Stream m a) : Stream m a :=
  intersperseM (pure sep) t

-- ── Indexing ─────────────────────────────────────────────────────────────────

/-- Pair each element with its index, starting from `0`. -/
@[inline] def indexed [Functor m] (t : Stream m a) : Stream m (Nat × a) where
  s := t.s × Nat
  step gst := fun (st, i) => (fun r => match r with
    | .Yield x s => Step.Yield (i, x) (s, i + 1)
    | .Skip s => Step.Skip (s, i)
    | .Stop => Step.Stop) <$> t.step gst st
  state := (t.state, 0)

/-- Pair each element with a decreasing index, starting from `n`. -/
@[inline] def indexedR [Functor m] (n : Nat) (t : Stream m a) : Stream m (Nat × a) where
  s := t.s × Nat
  step gst := fun (st, i) => (fun r => match r with
    | .Yield x s => Step.Yield (i, x) (s, i - 1)
    | .Skip s => Step.Skip (s, i)
    | .Stop => Step.Stop) <$> t.step gst st
  state := (t.state, n)

/-- The indices of the elements satisfying a predicate. -/
@[inline] def findIndices [Functor m] (p : a → Bool) (t : Stream m a) : Stream m Nat where
  s := t.s × Nat
  step gst := fun (st, i) => (fun r => match r with
    | .Yield x s => if p x then Step.Yield i (s, i + 1) else Step.Skip (s, i + 1)
    | .Skip s => Step.Skip (s, i)
    | .Stop => Step.Stop) <$> t.step gst st
  state := (t.state, 0)

/-- The indices of the elements equal to `e`. -/
@[inline] def elemIndices [Functor m] [BEq a] (e : a) (t : Stream m a) : Stream m Nat :=
  findIndices (fun x => x == e) t

-- ── Splitting `Sum` ──────────────────────────────────────────────────────────

/-- Keep only the `inl` values. -/
@[inline] def catLefts [Monad m] (t : Stream m (a ⊕ b)) : Stream m a :=
  mapMaybe (fun | .inl x => some x | .inr _ => none) t

/-- Keep only the `inr` values. -/
@[inline] def catRights [Monad m] (t : Stream m (a ⊕ b)) : Stream m b :=
  mapMaybe (fun | .inr y => some y | .inl _ => none) t

/-- Collapse a stream of same-type `Sum`s to the underlying values. -/
@[inline] def catEithers [Functor m] (t : Stream m (a ⊕ a)) : Stream m a :=
  map (fun | .inl x => x | .inr x => x) t

-- ── Deduplication ────────────────────────────────────────────────────────────

/-- Drop consecutive duplicates according to `eq`. -/
@[inline] def uniqBy [Monad m] (eq : a → a → Bool) (t : Stream m a) : Stream m a where
  s := Option a × t.s
  step gst := fun (prev, st) => do
    match ← t.step gst st with
    | .Yield x s =>
        match prev with
        | some p => if eq p x then pure (Step.Skip (some x, s)) else pure (Step.Yield x (some x, s))
        | none => pure (Step.Yield x (some x, s))
    | .Skip s => pure (Step.Skip (prev, s))
    | .Stop => pure Step.Stop
  state := (none, t.state)

/-- Drop consecutive duplicates. -/
@[inline] def uniq [Monad m] [BEq a] (t : Stream m a) : Stream m a :=
  uniqBy (fun x y => x == y) t

-- ── Rolling map ──────────────────────────────────────────────────────────────

/-- Map over each element together with its predecessor (`none` for the first),
    using a monadic function. -/
@[inline] def rollingMapM [Monad m] (f : Option a → a → m b) (t : Stream m a) : Stream m b where
  s := Option a × t.s
  step gst := fun (prev, st) => do
    match ← t.step gst st with
    | .Yield x s => (fun y => Step.Yield y (some x, s)) <$> f prev x
    | .Skip s => pure (Step.Skip (prev, s))
    | .Stop => pure Step.Stop
  state := (none, t.state)

/-- Map over each element together with its predecessor (`none` for the first). -/
@[inline] def rollingMap [Monad m] (f : Option a → a → b) (t : Stream m a) : Stream m b :=
  rollingMapM (fun p x => pure (f p x)) t

-- ── Reversing ────────────────────────────────────────────────────────────────

/-- Reverse a stream. `unsafe` — it drives the stream (strict left fold) to
    materialize the reversed list before re-emitting it. -/
@[specialize] unsafe def reverse [Monad m] (t : Stream m a) : Stream m a where
  s := Option (List a)
  step _ := fun
    | none => do let l ← foldl' (fun acc x => x :: acc) [] t; pure (Step.Skip (some l))
    | some (x :: xs) => pure (Step.Yield x (some xs))
    | some [] => pure Step.Stop
  state := none

end Stream
end Data.Stream
