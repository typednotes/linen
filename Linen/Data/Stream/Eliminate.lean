/-
  Linen.Data.Stream.Eliminate — fused-stream consumers

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Stream.Eliminate`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Stream/Eliminate.hs),
  module #21 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Consumers that *drive* a `Stream m a` (#19) to a summary value in `m`.

  ## Overlap with `Data.Stream.Type` (#19)

  `fold` (a `Fold` driver), `toList`, `drain`, `head`, `foldl'`/`foldlM'`,
  `foldr`/`foldrM`, and `eqBy` are already the Tier-2 drivers on `Stream.Type`
  and are **not re-ported here**. This module adds the remaining consumers,
  defined on top of those drivers.

  ## Substitutions / deviations

  - **Every consumer is `unsafe`** for the same reason as the Tier-2 drivers: a
    fused stepper may `Skip` unboundedly, so the loop has no structural or
    well-founded measure. These are built on the `unsafe` `foldl'`/`foldrM`/
    `drain`/`toList`/`mapM` of `Stream.Type` (AGENTS.md's sanctioned `unsafe`
    alternative — neither `partial` nor `sorry`).
  - **`uncons`, `init`, `tail`, `foldr1` originals, `stripPrefix`/`stripSuffix`
    dropped — the universe wall.** Each returns a *residual* `Stream m a` inside
    `m` (`m (Option (a × Stream m a))`, `m (Option (Stream m a))`), which is not
    expressible because `Stream m a : Type 1` cannot sit inside `m : Type → Type`
    (the exact wall documented on `Stream.Type`; `fold` is the standalone-driver
    workaround). `foldr1` is nonetheless provided via a pure list-level fold
    over `toList`, which needs no residual.
  - **`parse`/`parseBreak`/`parseD` dropped** — they consume a streaming
    `Parser` (`Data.Parser`, Tier 4), not yet ported; out of this batch's scope.
  - **`isPrefixOf`/`isInfixOf`/`isSuffixOf`/`isSubsequenceOf` dropped** —
    secondary comparison combinators (some `Unbox`/array-backed) belonging to
    the array/parser tiers, matching the plan's scoping.
-/

import Linen.Data.Stream.Type

namespace Data.Stream

namespace Stream

-- ── Boolean queries ──────────────────────────────────────────────────────────

/-- `True` iff the stream is empty. -/
@[specialize] unsafe def null [Monad m] (t : Stream m a) : m Bool :=
  foldrM (fun _ _ => pure false) (pure true) t

/-- `True` iff `e` occurs in the stream. -/
@[specialize] unsafe def elem [Monad m] [BEq a] (e : a) (t : Stream m a) : m Bool :=
  foldrM (fun x xs => if x == e then pure true else xs) (pure false) t

/-- `True` iff `e` does not occur in the stream. -/
@[inline] unsafe def notElem [Monad m] [BEq a] (e : a) (t : Stream m a) : m Bool :=
  not <$> elem e t

/-- `True` iff every element passes the predicate. -/
@[specialize] unsafe def all [Monad m] (p : a → Bool) (t : Stream m a) : m Bool :=
  foldrM (fun x xs => if p x then xs else pure false) (pure true) t

/-- `True` iff some element passes the predicate. -/
@[specialize] unsafe def any [Monad m] (p : a → Bool) (t : Stream m a) : m Bool :=
  foldrM (fun x xs => if p x then pure true else xs) (pure false) t

-- ── Extraction ───────────────────────────────────────────────────────────────

/-- The last element, if any. -/
@[inline] unsafe def last [Monad m] (t : Stream m a) : m (Option a) :=
  foldl' (fun _ y => some y) none t

/-- Collect the stream into a list, in reverse order. -/
@[inline] unsafe def toListRev [Monad m] (t : Stream m a) : m (List a) :=
  foldl' (fun acc x => x :: acc) [] t

/-- The maximum element by a comparison function, if any. -/
@[specialize] unsafe def maximumBy [Monad m] (cmp : a → a → Ordering) (t : Stream m a) :
    m (Option a) :=
  foldl' (fun acc y => match acc with
    | none => some y
    | some x => match cmp x y with | .lt => some y | _ => some x) none t

/-- The maximum element, if any. -/
@[inline] unsafe def maximum [Monad m] [Ord a] (t : Stream m a) : m (Option a) :=
  maximumBy compare t

/-- The minimum element by a comparison function, if any. -/
@[specialize] unsafe def minimumBy [Monad m] (cmp : a → a → Ordering) (t : Stream m a) :
    m (Option a) :=
  foldl' (fun acc y => match acc with
    | none => some y
    | some x => match cmp x y with | .gt => some y | _ => some x) none t

/-- The minimum element, if any. -/
@[inline] unsafe def minimum [Monad m] [Ord a] (t : Stream m a) : m (Option a) :=
  minimumBy compare t

/-- Look up the first value paired with a key equal to `e`. -/
@[specialize] unsafe def lookup [Monad m] [BEq a] (e : a) (t : Stream m (a × b)) :
    m (Option b) :=
  foldrM (fun p xs => if p.1 == e then pure (some p.2) else xs) (pure none) t

/-- The first element satisfying a monadic predicate, if any. -/
@[specialize] unsafe def findM [Monad m] (p : a → m Bool) (t : Stream m a) : m (Option a) :=
  foldrM (fun x xs => do if ← p x then pure (some x) else xs) (pure none) t

/-- The first element satisfying a predicate, if any. -/
@[inline] unsafe def find [Monad m] (p : a → Bool) (t : Stream m a) : m (Option a) :=
  findM (fun x => pure (p x)) t

/-- The element at index `n` (0-based), if the stream is long enough. -/
@[inline] unsafe def index [Monad m] (n : Nat) (t : Stream m a) : m (Option a) :=
  (fun l => l[n]?) <$> toList t

/-- Right fold over a non-empty stream (`none` on an empty stream). Provided via
    a pure list-level fold over `toList` — the upstream `uncons`-based
    definition hits the universe wall (see the module header). -/
@[inline] unsafe def foldr1 [Monad m] (f : a → a → a) (t : Stream m a) : m (Option a) :=
  (fun l => listFoldr1 l) <$> toList t
where
  listFoldr1 : List a → Option a
    | [] => none
    | [x] => some x
    | x :: xs => match listFoldr1 xs with
        | some r => some (f x r)
        | none => some x

/-- `some x` iff every element of a non-empty stream equals `x`; `none`
    otherwise (including the empty stream). -/
@[inline] unsafe def the [Monad m] [BEq a] (t : Stream m a) : m (Option a) :=
  (fun l => match l with
    | [] => none
    | x :: xs => if xs.all (· == x) then some x else none) <$> toList t

-- ── Map and drain ────────────────────────────────────────────────────────────

/-- Apply a monadic action to each element for its effect, discarding results. -/
@[inline] unsafe def mapM_ [Monad m] (f : a → m b) (t : Stream m a) : m PUnit :=
  drain (mapM f t)

end Stream
end Data.Stream
