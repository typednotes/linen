/-
  Linen.Control.Lens.Traversal — `traverseOf`, `forOf`, `sequenceAOf`,
  `mapAccumLOf`, `scanl1Of`, `failover`, `both`, `traversed`, `ignored`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Traversal` (fetched and read
  via Hackage's rendered Haddock and source). A `Traversal s t a b` is like a
  `Lens`, but may focus on zero, one, or many `a`s inside an `s` at once
  (`Traversal S T A B := ∀ {F} [Applicative F], LensLike F S T A B` — already
  defined in `Linen.Control.Lens.Type`, reused here rather than redefined).

  **Scope note (`(%%~)`).** Upstream defines `(%%~)` again in this module (it
  re-exports the identical combinator from `Control.Lens.Lens`); `linen`
  already has it as `Linen.Control.Lens.Lens.overF`/`%%~`, so it is not
  redefined here.

  **`both`, via the new `Data.Bitraverse` class.** Upstream's `both ::
  Bitraversable r => Traversal (r a a) (r b b) a b` needs a `Bitraversable`
  constraint that `Linen.Data.Bifunctor` did not previously have; this batch
  adds the minimal `Data.Bitraverse` class (with `bitraverse`) to that module
  first, with instances for `Prod`/`Sum`/`Except`, and `both` here is exactly
  `bitraverse` applied to the same function on both sides.

  **Scope note (`mapAccumROf`/`scanr1Of`, `Backwards`).** Both need
  `Backwards`, the functor-reversing wrapper that runs an `Applicative`
  traversal right-to-left by swapping `(<*>)`'s argument order. `linen` has
  ported no `Backwards` (`Linen.Control.Lens.Internal.Setter`'s own scope
  note already drops it as the sole consumer at the time); `mapAccumLOf`/
  `scanl1Of` (the left-to-right halves, needing only the already-available
  `State`) are ported, their right-to-left mirrors are not.

  **Scope note (`failover`, `Any`).** Upstream's real `failover ::
  Alternative m => LensLike ((,) Any) s t a b -> (a -> b) -> s -> m t` tracks
  "was anything visited at all" via the `Any` monoid running alongside the
  traversal, then dispatches into an arbitrary `Alternative` on success/
  failure. `linen` has ported no `Any` newtype (see `Linen.Control.Lens.Fold`
  's own deviation note on the `Endo`/`Any`/`All`/… family); this port tracks
  the same boolean directly via a `Prod Bool` orphan `Applicative` instance
  (`Bool`'s `||` playing `Any`'s role, exactly as `Fold`'s `Const`-orphan
  precedent tracks `List` in place of `Endo`), and dispatches into Lean's
  core `Alternative` class (`pure`/`failure`) rather than a ported
  `Alternative` class of our own, since core already has one.

  **Scope note (`partsOf`/`ipartsOf`, `cloneTraversal`,
  `cloneIndexPreservingTraversal`).** All three need the reified-traversal
  `Bazaar`/`ATraversal`/`Pretext` machinery that `Linen.Control.Lens.Internal.
  {Bazaar,Context}`'s own scope notes already flag as not fully ported (no
  `IndexedComonad`/`Sellable`, no way to safely "replay" an already-applied
  optic at a different functor). Skipped here for the same reason
  `Linen.Control.Lens.Lens`'s `cloneLens`/`ALens` are skipped.

  **Scope note (`beside`).** Needs `Representable q`/`Applicative (Rep q)`,
  generalizing over an arbitrary profunctor pair the same way `Control.Lens.
  Setter`'s real `sets` does; `linen`'s concrete optic aliases are
  deliberately *not* profunctor-generalized (see `Getter.to`'s doc comment),
  so there is no faithful home for `beside`'s profunctor-polymorphic
  signature here. Skipped.

  **Scope note (`elementOf`/`element`, `Indexing`).** Both need the counting
  `Indexing` applicative that `Linen.Control.Lens.Internal.Indexed`'s own
  scope note explicitly defers to whichever module ports `Control.Lens.
  Indexed` — i.e. `Linen.Control.Lens.Indexed`, not this module. Ported
  there instead, alongside `Indexing` itself.

  **Scope note (`traversed`/`traversed64`).** Upstream's real `traversed ::
  Traversable f => IndexedTraversal Int (f a) (f b) a b` also carries the
  traversal-order integer index. As with `Control.Lens.Fold`'s `folded`, the
  indexed variant needs `Control.Lens.Indexed`'s `Indexing` (not yet
  ported); this module gives the plain, non-indexed `Traversal` core it is
  built from. `traversed64` is the same combinator at a different index
  type (`Int64` instead of `Int`) and collapses to the exact same
  non-indexed degenerate case, so it is not ported as a separate
  definition. -/

import Linen.Control.Lens.Type
import Linen.Control.Lens.Getter
import Linen.Data.Bifunctor
import Linen.Data.Traversable
import Linen.Control.Monad.State

open Data.Functor Control.Monad.State

namespace Control.Lens

-- ── traverseOf / forOf / sequenceAOf ─────────────

/-- `traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t`: running a
    `Traversal` at a given `Applicative f` is exactly applying it —
    `traverseOf = id`, kept as a named synonym for readability at call
    sites, matching upstream. -/
@[inline] def traverseOf {F : Type u → Type u} {S T A B : Type u}
    (l : LensLike F S T A B) (afb : A → F B) (s : S) : F T :=
  l afb s

/-- `forOf :: LensLike f s t a b -> s -> (a -> f b) -> f t`: `traverseOf`
    with its last two arguments flipped. -/
@[inline] def forOf {F : Type u → Type u} {S T A B : Type u}
    (l : LensLike F S T A B) (s : S) (afb : A → F B) : F T :=
  traverseOf l afb s

/-- `sequenceAOf :: LensLike f s t (f b) b -> s -> f t`: flip every focused
    `f b` inside-out, collecting the effects. -/
@[inline] def sequenceAOf {F : Type u → Type u} {S T B : Type u}
    (l : LensLike F S T (F B) B) (s : S) : F T :=
  traverseOf l id s

-- ── mapAccumLOf / scanl1Of ───────────────────────

/-- `mapAccumLOf :: LensLike (State acc) s t a b -> (acc -> a -> (acc, b)) ->
    acc -> s -> (acc, t)`: thread an accumulator left-to-right through every
    focused element, replacing each `a` with the `b` `f` produces alongside
    the updated accumulator. See the module's scope note on why the
    right-to-left mirror `mapAccumROf` (needing `Backwards`) is not ported. -/
@[inline] def mapAccumLOf {ACC S T A B : Type}
    (l : LensLike (State ACC) S T A B) (f : ACC → A → ACC × B) (acc0 : ACC) (s : S) :
    ACC × T :=
  let (t, accFinal) :=
    runState
      (traverseOf l
        (fun a => do
          let acc ← gets id
          let (acc', b) := f acc a
          put acc'
          pure b)
        s)
      acc0
  (accFinal, t)

/-- `scanl1Of :: LensLike (State (Maybe a)) s t a a -> (a -> a -> a) -> s ->
    t`: replace each focused element with the running left fold of `f` over
    every element focused so far (the first element is left unchanged). See
    the module's scope note on why the right-to-left mirror `scanr1Of`
    (needing `Backwards`) is not ported. -/
@[inline] def scanl1Of {S A : Type} (l : LensLike (State (Option A)) S S A A)
    (f : A → A → A) (s : S) : S :=
  evalState
    (traverseOf l
      (fun a => do
        let prev ← gets id
        let a' := match prev with
          | some p => f p a
          | none => a
        put (some a')
        pure a')
      s)
    none

-- ── orphan instance: `Applicative (Prod Bool)` ───

/-- `Prod Bool` is `Pure`: start with `false` (Haskell's `Any`'s `mempty`).
    See the module's `failover`/`Any` scope note. -/
instance : Pure (Prod Bool) where
  pure a := (false, a)

/-- `Prod Bool` is `Seq`: combine both sides' flags with `||` (Haskell's
    `Any`'s `(<>)`). See the module's `failover`/`Any` scope note. -/
instance : Seq (Prod Bool) where
  seq pf px := (pf.1 || (px ()).1, pf.2 (px ()).2)

/-- `Prod Bool` is `Applicative`, given `Pure`/`Seq`/`Functor` above. See the
    module's `failover`/`Any` scope note. -/
instance : Applicative (Prod Bool) where

-- ── failover ──────────────────────────────────────

/-- `failover :: Alternative m => LensLike ((,) Any) s t a b -> (a -> b) -> s
    -> m t`: run a `Traversal`, rewriting every focused `a` via `f`;
    dispatches into an arbitrary `Alternative`, succeeding with the rewritten
    `t` if at least one element was actually focused, and failing (`failure`)
    otherwise. See the module's scope note on the `Prod Bool`-for-`Any`
    substitution. -/
@[inline] def failover {S T A B : Type u} {M : Type u → Type u} [Alternative M]
    (l : Traversal S T A B) (f : A → B) (s : S) : M T :=
  let (touched, t) := l (F := Prod Bool) (fun a => (true, f a)) s
  if touched then pure t else failure

-- ── both ──────────────────────────────────────────

/-- `both :: Bitraversable r => Traversal (r a a) (r b b) a b`: focus on both
    components of a bitraversable pair-shaped container at once — `both f =
    bitraverse f f`. See the module's doc comment on the new `Data.Bitraverse`
    class this needs. -/
@[inline] def both {R : Type u → Type u → Type u} [Data.Bitraverse R] {A B : Type u} :
    Traversal (R A A) (R B B) A B :=
  fun {F} [Applicative F] afb raa => Data.Bitraverse.bitraverse afb afb raa

-- ── traversed ─────────────────────────────────────

/-- `traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b`:
    every `Traversable` container gives rise to a `Traversal` over its
    elements, via `traverse`. See the module's scope note on why the index
    is deferred to `Linen.Control.Lens.Indexed`. -/
@[inline] def traversed {T : Type u → Type u} [Data.Traversable T] {A B : Type u} :
    Traversal (T A) (T B) A B :=
  fun {F} [Applicative F] afb ta => Data.Traversable.traverse afb ta

-- ── ignored ───────────────────────────────────────

/-- `ignored :: Applicative f => pafb -> s -> f s`: a `Traversal` that
    focuses on nothing at all — leaves every `s` completely untouched,
    regardless of what optic-shaped argument it is given. -/
@[inline] def ignored {S A B : Type u} : Traversal S S A B :=
  fun {F} [Applicative F] _afb s => pure s

end Control.Lens
