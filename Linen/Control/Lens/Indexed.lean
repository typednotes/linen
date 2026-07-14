/-
  Linen.Control.Lens.Indexed — `itraverse`, `ifoldMap`, `ifoldr`, `ifoldl`,
  `imap`, `icompose`, `(<.>)`, `(.>)`, `indexing`, `withIndex`, `asIndex`,
  `itraversed`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Indexed` (fetched and read
  via Hackage's rendered Haddock and source). This module is the bridge
  between the plain `Functor`/`Foldable`/`Traversable`-with-an-index classes
  from `indexed-traversable` (`Linen.Data.{Functor,Foldable,Traversable}.
  WithIndex`, already ported) and the *indexed optics* (`IndexedLens`,
  `IndexedTraversal`, `IndexedFold`, `IndexedSetter`) defined in
  `Linen.Control.Lens.Type`.

  **Scope note (`imap`/`ifoldr`/`ifoldl`/`ifoldMap`, plain wrappers).**
  These are direct one-line wrappers around the already-ported
  `Data.Functor.WithIndex.mapWithIndex`/`Data.Foldable.WithIndex.
  {foldrWithIndex,foldMapWithIndex}`, renamed to upstream's `Control.Lens`
  names (matching upstream's own definitions, e.g. `imap = imap` — i.e. the
  class method itself, re-exported under this module's name). `ifoldl` has
  no primitive of its own in `Linen.Data.Foldable.WithIndex` (only the
  right fold `foldrWithIndex`), so it is built with the standard
  "foldl via foldr" continuation-passing trick, indexed.

  **Scope note (`ifoldMap'`/`ifoldr'`/`ifoldl'`, strictness variants).**
  Upstream's primed variants are strict-accumulator versions that exist
  because GHC is lazy by default; Lean is eager, so (as with `Data.Strict.*`
  in the top-level dependency list) the lazy/strict distinction these encode
  does not exist here — they would be definitionally identical to the
  unprimed versions, so only the unprimed names are ported.

  **Scope note (`itraverse_`/`ifor_`/`imapM_`/`iforM_`/`iconcatMap`/`ifind`/
  `ifoldrM`/`ifoldlM`/`itoList`).** Secondary combinators built trivially
  from `itraverse`/`ifoldr`/`ifoldMap` with no further optics content (the
  same category `Linen.Data.Foldable`'s own module already keeps small);
  out of scope for this batch, which focuses on the optics-facing core.

  **Scope note (`indices`/`index`, `selfIndex`, `reindexed`, `(<.)`).**
  `indices`/`index` restrict an already-indexed optic to indices matching a
  predicate/a specific index — direct analogues of `Control.Lens.Fold`'s
  `filtered`, but for the indexed family; `selfIndex`/`reindexed`/`(<.)` are
  further composition helpers in the same family as `(<.>)`/`icompose`/
  `(.>)`. Only the latter three are ported (see below); the others have no
  call site anywhere in this batch's scope and are deferred rather than
  manufactured speculatively.

  **Deviation (`icompose`/`(<.>)`/`(.>)`, concrete `IndexedTraversal`
  shape).** Upstream states all three in a fully `Indexable`-polymorphic,
  continuation-passing style (`(Indexed i s t -> r) -> ... -> c a b -> r`)
  so they can compose *any* two optics in the indexed family (`IndexedLens`
  with `IndexedFold`, etc.) into any result shape `r`. As with `Getter.to`/
  `Fold.filtered`, `linen`'s concrete optic aliases are deliberately not
  profunctor-`r`-polymorphic, so these three land at the most general
  concrete shape their direct implementation supports: composing two
  `IndexedTraversal`s (the shape every other indexed optic in this port can
  degrade to). `icompose` combines both indices with a supplied function;
  `(<.>)` is `icompose Prod.mk` (upstream's own default when no combiner is
  given); `(.>)` keeps only the outer index, threading the inner optic as a
  plain (non-indexed) `Traversal` underneath it — upstream's own doc note
  ("this is the same as `(.)`") already flags it as ordinary composition
  once the index-plumbing is unwound.

  **Deviation (`withIndex`/`asIndex`, concrete `Fold` shape).** Upstream's
  real signatures are similarly `Indexable i p`-polymorphic and land in an
  `Indexed i s (f t)`, generalizing over an arbitrary functor `f` wrapping
  the target. This port gives the directly useful concrete degenerate case
  every real call site actually wants: turning an `IndexedFold i s a` into
  an ordinary `Fold s (i, a)` (`withIndex`) or `Fold s i` (`asIndex`),
  pairing/replacing each focused value with its index — built from
  `Control.Lens.Fold`'s `folding`, the same way `filtered` is built there. -/

import Linen.Control.Lens.Fold
import Linen.Control.Lens.Traversal
import Linen.Data.Functor.WithIndex
import Linen.Data.Foldable.WithIndex
import Linen.Data.Traversable.WithIndex

open Control.Lens.Internal Data.Functor

namespace Control.Lens

-- ── imap / imapped ────────────────────────────────

/-- `imap :: FunctorWithIndex i f => (i -> a -> b) -> f a -> f b`: map with
    access to each element's index — `imap = Data.Functor.WithIndex.
    mapWithIndex`. -/
@[inline] def imap {I : Type u} {T : Type u → Type u} [Data.Functor.WithIndex I T] {A B : Type u}
    (f : I → A → B) (t : T A) : T B :=
  Data.Functor.WithIndex.mapWithIndex f t

/-- `imapped :: FunctorWithIndex i f => IndexedSetter i (f a) (f b) a b`:
    every `FunctorWithIndex` container gives rise to an `IndexedSetter` on
    its elements. -/
@[inline] def imapped {I : Type u} {T : Type u → Type u} [Data.Functor.WithIndex I T] {A B : Type u} :
    IndexedSetter I (T A) (T B) A B :=
  fun {F} [Settable F] {P} [Indexable I P] pab t =>
    (pure (imap (fun i a => Settable.untainted (Indexable.indexed pab i a)) t) : F (T B))

-- ── ifoldr / ifoldl / ifoldMap ────────────────────

/-- `ifoldr :: FoldableWithIndex i f => (i -> a -> b -> b) -> b -> f a -> b`:
    right-fold with access to each element's index — `ifoldr =
    Data.Foldable.WithIndex.foldrWithIndex`. -/
@[inline] def ifoldr {I : Type u} {T : Type u → Type u} [Data.Foldable.WithIndex I T] {A B : Type u}
    (f : I → A → B → B) (z : B) (t : T A) : B :=
  Data.Foldable.WithIndex.foldrWithIndex f z t

/-- `ifoldl :: FoldableWithIndex i f => (i -> b -> a -> b) -> b -> f a -> b`:
    left-fold with access to each element's index, built from `ifoldr` via
    the standard "left fold as a right fold over continuations" trick. -/
@[inline] def ifoldl {I : Type u} {T : Type u → Type u} [Data.Foldable.WithIndex I T] {A B : Type u}
    (f : I → B → A → B) (z : B) (t : T A) : B :=
  ifoldr (fun i a (k : B → B) (acc : B) => k (f i acc a)) id t z

/-- `ifoldMap :: (FoldableWithIndex i f, Monoid m) => (i -> a -> m) -> f a ->
    m`: fold every element (with its index) into a semigroup and combine —
    `ifoldMap = Data.Foldable.WithIndex.foldMapWithIndex`. -/
@[inline] def ifoldMap {I M : Type u} {T : Type u → Type u} [Data.Foldable.WithIndex I T] [Append M] [Inhabited M]
    {A : Type u} (f : I → A → M) (t : T A) : M :=
  Data.Foldable.WithIndex.foldMapWithIndex f t

/-- `ifolded :: FoldableWithIndex i f => IndexedFold i (f a) a`: every
    `FoldableWithIndex` container gives rise to an `IndexedFold` over its
    elements. -/
@[inline] def ifolded {I : Type u} {T : Type u → Type u} [Data.Foldable.WithIndex I T] {A : Type u} :
    IndexedFold I (T A) A :=
  fun {F} [Contravariant F] [Applicative F] {P} [Indexable I P] pab t =>
    Contravariant.contramap (fun (_ : T A) => PUnit.unit)
      (ifoldr (fun i a acc => SeqRight.seqRight (Indexable.indexed pab i a) (fun _ => acc))
        (Pure.pure PUnit.unit : F PUnit) t)

-- ── itraverse / itraversed ────────────────────────

/-- `itraverse :: (TraversableWithIndex i t, Applicative f) => (i -> a -> f
    b) -> t a -> f (t b)`: traverse with access to each element's index —
    `itraverse = Data.Traversable.WithIndex.traverseWithIndex`. -/
@[inline] def itraverse {I : Type u} {T : Type u → Type u} [Data.Functor.WithIndex I T]
    [Data.Foldable.WithIndex I T] [Data.Traversable.WithIndex I T] {G : Type u → Type u}
    [Applicative G] {A B : Type u} (f : I → A → G B) (t : T A) : G (T B) :=
  Data.Traversable.WithIndex.traverseWithIndex f t

/-- `itraversed :: TraversableWithIndex i t => IndexedTraversal i (t a) (t
    b) a b`: every `TraversableWithIndex` container gives rise to an
    `IndexedTraversal` over its elements, via `itraverse`. -/
@[inline] def itraversed {I : Type u} {T : Type u → Type u} [Data.Functor.WithIndex I T]
    [Data.Foldable.WithIndex I T] [Data.Traversable.WithIndex I T] {A B : Type u} :
    IndexedTraversal I (T A) (T B) A B :=
  fun {F} [Applicative F] {P} [Indexable I P] pab t =>
    itraverse (fun i a => Indexable.indexed pab i a) t

-- ── Indexing / indexing ───────────────────────────

/-- `Indexing f a := Int -> (Int, f a)`: the small counting applicative
    upstream's `indexing` combinator uses to auto-number the elements a
    plain (non-indexed) `Traversal`/`Fold` visits, deferred here from
    `Linen.Control.Lens.Internal.Indexed`'s own scope note (its only
    consumer). Threads a running `Nat` counter alongside the wrapped
    effect, incrementing it once per visited element. -/
structure Indexing (F : Type u → Type u) (A : Type u) where
  /-- Run the counting computation from a given starting index, producing
      the next index to use and the wrapped effect. -/
  runIndexing : Nat → Nat × F A

/-- `Indexing f` is a `Functor`: map through the wrapped effect, leaving the
    counter untouched. -/
instance [Functor F] : Functor (Indexing F) where
  map f x := ⟨fun i => let (j, fa) := x.runIndexing i; (j, f <$> fa)⟩

/-- `Indexing f` is `Pure`: don't advance the counter. -/
instance [Applicative F] : Pure (Indexing F) where
  pure a := ⟨fun i => (i, pure a)⟩

/-- `Indexing f` is `Seq`: thread the counter through both sides in order. -/
instance [Applicative F] : Seq (Indexing F) where
  seq mf mx := ⟨fun i =>
    let (j, ff) := mf.runIndexing i
    let (k, fa) := (mx ()).runIndexing j
    (k, ff <*> fa)⟩

/-- `Indexing f` is `Applicative`, given `Pure`/`Seq`/`Functor` above. -/
instance [Applicative F] : Applicative (Indexing F) where

/-- `indexing :: Indexable Int p => LensLike (Indexing f) s t a b ->
    IndexedLensLike Int f s t a b`: turn a plain (non-indexed) `Traversal`/
    `Fold` into its indexed counterpart, numbering every visited element in
    traversal order starting from `0`. -/
@[inline] def indexing {S T A B : Type} {F : Type → Type} [Applicative F]
    (l : LensLike (Indexing F) S T A B) : IndexedLensLike Nat F S T A B :=
  fun {P} [Indexable Nat P] pab s =>
    (l (fun a => ⟨fun i => (i + 1, Indexable.indexed pab i a)⟩) s).runIndexing 0 |>.2

-- ── icompose / (<.>) / (.>) ───────────────────────

/-- `icompose :: Indexable p c => (i -> j -> p) -> (Indexed i s t -> r) ->
    (Indexed j a b -> s -> t) -> c a b -> r`: compose two `IndexedTraversal`s
    into one, combining their indices with `combine`. See the module's
    deviation note for why this lands at the concrete `IndexedTraversal`
    shape rather than upstream's fully polymorphic continuation-passing
    form. -/
@[inline] def icompose {I J P : Type u} (combine : I → J → P) {S T A B A' B' : Type u}
    (outer : IndexedTraversal I S T A B) (inner : IndexedTraversal J A B A' B') :
    IndexedTraversal P S T A' B' :=
  fun {F} [Applicative F] {P'} [Indexable P P'] pab' s =>
    outer (F := F) (P := Indexed I) (Indexed.mk (fun i a =>
      inner (F := F) (P := Indexed J) (Indexed.mk (fun j a' =>
        Indexable.indexed pab' (combine i j) a')) a)) s

/-- `(<.>) :: Indexable (i, j) p => (Indexed i s t -> r) -> (Indexed j a b ->
    s -> t) -> p a b -> r`: `icompose`, pairing both indices — upstream's
    own default combiner when none is supplied. -/
@[inline] def composeIndices {I J S T A B A' B' : Type u}
    (outer : IndexedTraversal I S T A B) (inner : IndexedTraversal J A B A' B') :
    IndexedTraversal (I × J) S T A' B' :=
  fun {F} [Applicative F] {P'} [Indexable (I × J) P'] pab' s =>
    outer (F := F) (P := Indexed I) (Indexed.mk (fun i a =>
      inner (F := F) (P := Indexed J) (Indexed.mk (fun j a' =>
        Indexable.indexed pab' (i, j) a')) a)) s
@[inherit_doc composeIndices] infixr:80 " <.> " => composeIndices

/-- `(.>) :: (st -> r) -> (kab -> st) -> kab -> r`: compose an
    `IndexedTraversal` with a further plain `Traversal`, keeping only the
    outer optic's index — upstream's own note flags this as "the same as
    `(.)`" once the index-plumbing is unwound. See the module's deviation
    note. -/
@[inline] def composeKeepOuterIndex {I S T A B A' B' : Type u}
    (outer : IndexedTraversal I S T A B) (inner : Traversal A B A' B') :
    IndexedTraversal I S T A' B' :=
  fun {F} [Applicative F] {P} [Indexable I P] pab s =>
    outer (F := F) (P := Indexed I) (Indexed.mk (fun i a =>
      inner (F := F) (fun a' => Indexable.indexed pab i a') a)) s
@[inherit_doc composeKeepOuterIndex] infixr:80 " .> " => composeKeepOuterIndex

-- ── withIndex / asIndex ───────────────────────────

/-- Collect every `(index, value)` pair an `IndexedFold` focuses on, in
    order — the shared primitive `withIndex`/`asIndex` are built from,
    mirroring `Control.Lens.Fold`'s `toListOf`. -/
@[inline] def itoListOf {I S A : Type u} (l : IndexedFold I S A) (s : S) : List (I × A) :=
  (l (F := Const (List (I × A))) (P := Indexed I)
      (Indexed.mk (fun i a => Const.mk [(i, a)])) s).getConst

/-- `withIndex :: (Indexable i p, Functor f) => p (i, s) (f (j, t)) ->
    Indexed i s (f t)`: turn an `IndexedFold` into an ordinary `Fold` that
    pairs each value with the index it was focused at. See the module's
    deviation note for why this lands at the concrete `Fold`-of-pairs shape. -/
@[inline] def withIndex {I S A : Type u} (l : IndexedFold I S A) : Fold S (I × A) :=
  folding (itoListOf l)

/-- `asIndex :: (Indexable i p, Contravariant f, Functor f) => p i (f i) ->
    Indexed i s (f s)`: turn an `IndexedFold` into an ordinary `Fold` over
    just the indices it visits, discarding the focused values. See the
    module's deviation note. -/
@[inline] def asIndex {I S A : Type u} (l : IndexedFold I S A) : Fold S I :=
  folding (fun s => (itoListOf l s).map Prod.fst)

end Control.Lens
