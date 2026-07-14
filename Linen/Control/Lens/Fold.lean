/-
  Linen.Control.Lens.Fold — `Fold`, `folding`, `folded`, `toListOf`, `(^..)`,
  `preview`, `(^?)`, `has`, `hasn't`, `foldOf`, `foldrOf`, `foldlOf`,
  `anyOf`, `allOf`, `andOf`, `orOf`, `elemOf`, `lengthOf`, `nullOf`,
  `notNullOf`, `firstOf`, `lastOf`, `sumOf`, `productOf`, `minimumOf`,
  `maximumOf`, `findOf`, `foldByOf`, `foldMapByOf`, `filtered`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Fold` (fetched and read via
  Hackage's rendered Haddock and source; this module is by far upstream's
  largest, so only the core, load-bearing subset is ported — see the scope
  notes below for what is skipped and why). `Fold s a := ∀ f, (Contravariant
  f, Applicative f) => (a -> f a) -> s -> f s`: like a `Getter`, a read-only
  optic, but (being restricted to `Applicative`, not merely `Functor`, on
  top of `Contravariant`) may focus on any number of `a`s inside an `s`, not
  just exactly one.

  **On `both`.** The plan accompanying this batch suggested `both` might
  belong here; upstream's real source places it in `Control.Lens.Traversal`
  instead (it needs `Bitraversable`, i.e. `Applicative`, not just
  `Contravariant`+`Applicative` — a genuine `Traversal`, not merely a
  `Fold`). It is ported there, in `Linen.Control.Lens.Traversal`, matching
  upstream's real module.

  **Orphan instances (`Applicative (Data.Functor.Const α)`).** Every
  `toListOf`-style combinator below runs its optic argument concretely at
  `F := Const (List A)`, which needs a full `Applicative (Const (List A))`
  instance — `Linen.Data.Functor`'s own `Const` namespace stops short of
  that (it gives `Const α` `Functor`/`Contravariant` unconditionally, and
  `Pure` under `[Append α] [Inhabited α]`, since nothing prior to this
  module needed the rest). This module adds the missing `Seq`/`Applicative`
  instances here (guarded by the same `[Append α] [Inhabited α]`
  constraints as the existing `Pure` instance) rather than editing
  `Linen.Data.Functor` itself, since `Const`'s `Applicative` instance is
  exactly the ingredient a `Fold` (as opposed to a mere `Getter`) needs to
  run at all, and this is that combinator family's first appearance.

  **Deviation (`List` in place of `Endo`/`Dual`/`Any`/`All`/`First`/
  `Last`/`Min`/`Max`).** Upstream's combinators are individually specialized
  to a menagerie of small `Monoid` newtypes (`Endo r` for `foldrOf`, `Dual
  (Endo r)` for `foldlOf`, `Any`/`All` for `anyOf`/`allOf`/`andOf`/`orOf`,
  `First`/`Last`/`Leftmost`/`Rightmost` for `preview`/`firstOf`/`lastOf`,
  `Min`/`Max` for the total `minimum1Of`/`maximum1Of`, …), chosen so that
  each combinator can short-circuit on a *possibly infinite* Haskell
  `Foldable`. `linen` has ported none of those newtypes (see
  `Linen.Control.Lens.Internal.Fold`'s own scope note for why
  `Leftmost`/`Rightmost` specifically were dropped), and every container
  `linen` folds over is a finite `List`/`Option`/… underneath. This module
  therefore funnels every combinator through one shared primitive,
  `toListOf` (running the optic at `Const (List A)`, the shape upstream's
  own `toListOf` itself already uses via `Endo [a]`), and builds everything
  else — `foldrOf`, `foldlOf`, `anyOf`, `allOf`, …, `foldByOf`,
  `foldMapByOf` — as a plain `List` operation on the result. This changes
  nothing observable for any finite structure (which is all `linen` has),
  and follows this batch's own explicit guidance for `firstOf`/`lastOf`
  (`List.head?`/`List.getLast?` over `toListOf`) generalized to every other
  combinator that upstream builds the same way, out of the same underlying
  motivation.

  **Scope note (`preview`/`(^?)`, `MonadReader`).** As with `Getter`'s
  `view`/`views`, upstream's real `preview :: MonadReader s m => Getting
  (First a) s a -> m (Maybe a)` is generalized over an arbitrary reader
  monad; this port keeps the essential degenerate case at `m := Id`,
  matching `Linen.Control.Lens.Getter`'s identical scope note. `(^?!)` is
  skipped outright: upstream's real signature (`HasCallStack => s ->
  Getting (Endo a) s a -> a`) panics on an empty focus, which has no total
  Lean counterpart without an `Inhabited`/`Option.get!`-style escape hatch
  this codebase's no-`sorry`/no-`partial` discipline rules out.

  **Scope note (`foldByOf`/`foldMapByOf`, `Folding`).** These take their
  combining operation and seed value as plain runtime arguments rather than
  through a type-class-resolved `Monoid`, which is exactly why upstream's
  own real implementation (`foldByOf l f z = foldrOf l f z`, `foldMapByOf l
  f z g = foldrOf l (f . g) z` — i.e. these are literally `foldrOf` in a
  trench coat) never actually reaches for `Control.Lens.Internal.Fold`'s
  `Folding` newtype at all; that type instead backs `Control.Lens.Fold`'s
  `foldOf`-adjacent `Endo`-avoiding internals for combinators out of this
  batch's scope (e.g. `preview`'s real `Traversed`/`Sequenced`-flavoured
  cousins). This port follows the same direct `foldrOf`-based definition.

  **Scope note (`takingWhile`/`droppingWhile`).** Upstream's real
  implementations reify the traversed elements into a `Bazaar`-shaped tree
  via a dedicated `TakingWhile` profunctor wrapper (not ported anywhere in
  `linen`) precisely so early termination can be threaded through an
  arbitrary `Conjoined p`-generalized optic, not just a concrete `Fold`.
  Building that wrapper with no other call site anywhere in this batch's
  scope, on top of the already-reduced `Bazaar`/`Conjoined` this codebase
  ported (see those modules' own scope notes on what they already dropped),
  would not be a faithful subset port so much as new machinery; skipped.

  **Scope note (`worded`/`lined`).** Upstream's own documentation already
  flags these as "not a legal `Traversal`" (they cannot round-trip runs of
  non-single-space whitespace or trailing newlines faithfully). Porting them
  faithfully needs no new *type*, but every one of `linen`'s indexed optic
  types requires a `TraversableWithIndex`/`Indexable`-polymorphic threading
  this module (ported before `Control.Lens.Indexed`, per this batch's
  ordering) does not yet have a natural non-indexed degenerate case for
  here; deferred rather than manufacturing a bespoke one-off index scheme
  with no other consumer. -/

import Linen.Control.Lens.Getter
import Linen.Data.Foldable

open Control.Lens.Internal Data.Functor

namespace Control.Lens

-- ── Fold ────────────────────────────────────────

/-- `Fold s a := ∀ f, (Contravariant f, Applicative f) => (a -> f a) -> s ->
    f s`: a read-only optic that may focus on any number of `a`s inside an
    `s`. -/
abbrev Fold (S A : Type u) :=
  ∀ {F : Type u → Type u} [Contravariant F] [Applicative F], LensLike' F S A

-- ── orphan instances: `Applicative (Const α)` ───

/-- `Const α` is `Seq`: combine both sides' stored values with `(++)`
    (upstream's `Applicative (Const m)` instance, `Const f <*> Const x =
    Const (f <> x)`, specialized here to the `Append`/`Inhabited`
    substitution for `Monoid` already used by `Const`'s own `Pure`
    instance). See the module's orphan-instance note. -/
instance [Append α] [Inhabited α] : Seq (Const α) where
  seq cf cx := ⟨cf.getConst ++ (cx ()).getConst⟩

/-- `Const α` is `Applicative`, given `Seq`/`Pure`/`Functor` above. See the
    module's orphan-instance note. -/
instance [Append α] [Inhabited α] : Applicative (Const α) where

-- ── folding / folded ─────────────────────────────

/-- `folding :: Foldable f => (s -> f a) -> Fold s a`: build a `Fold` out of
    a function into any `Foldable` container — `folding sfa agb = phantom .
    traverse_ agb . sfa`, i.e. visit every element `sfa s` produces, in
    order, discarding the covariant result (`phantom`, the same
    contravariant-erasure trick as `Linen.Control.Lens.Internal.Getter`'s
    `noEffect`). -/
@[inline] def folding {S A : Type u} {T : Type u → Type u} [Data.Foldable T]
    (sfa : S → T A) : Fold S A :=
  fun {F} [Contravariant F] [Applicative F] afa s =>
    Contravariant.contramap (fun (_ : S) => PUnit.unit)
      (Data.Foldable.foldr
        (fun a acc => SeqRight.seqRight (afa a) (fun _ => acc))
        (Pure.pure PUnit.unit : F PUnit) (sfa s))

/-- `folded :: Foldable f => IndexedFold Int (f a) a`: every `Foldable`
    container gives rise to a `Fold` over its elements — `folded = folding
    id`.

    **Deviation from upstream's `IndexedFold Int`.** Upstream's real
    `folded` also carries the traversal-order integer index, built via the
    counting `Indexing` applicative. `Control.Lens.Indexed` (this batch's
    third module) is the one that actually ports `Indexing`; `folded` here
    gives the plain, non-indexed `Fold` core it is built from — the same
    order this batch's own module list already commits to (`Fold` before
    `Indexed`). -/
@[inline] def folded {T : Type u → Type u} [Data.Foldable T] {A : Type u} :
    Fold (T A) A :=
  folding id

-- ── toListOf / (^..) ────────────────────────────

/-- `toListOf :: Getting (Endo [a]) s a -> s -> [a]`: collect every value a
    `Fold`/`Getter`/`Lens`/`Traversal` focuses on, in order — the shared
    primitive every other combinator in this module is built from. See the
    module's deviation note for why this runs at `Const (List A)` rather
    than upstream's `Endo [a]`. -/
@[inline] def toListOf {S A : Type u} (l : Getting (List A) S A) (s : S) : List A :=
  (l (fun a => Const.mk [a]) s).getConst

/-- `(^..) :: s -> Getting (Endo [a]) s a -> [a]`: infix flip of `toListOf`. -/
@[inline] def getToListOf {S A : Type u} (s : S) (l : Getting (List A) S A) : List A :=
  toListOf l s

@[inherit_doc getToListOf] infixl:75 " ^.. " => getToListOf

-- ── preview / (^?) ──────────────────────────────

/-- `preview :: Getting (First a) s a -> s -> Maybe a`: like `view`, but for
    a `Fold` that might focus on zero elements — returns the first focused
    value, if any. See the module's scope note for why this is the direct,
    non-`MonadReader` form. -/
@[inline] def preview {S A : Type u} (l : Getting (List A) S A) (s : S) : Option A :=
  (toListOf l s).head?

/-- `(^?) :: s -> Getting (First a) s a -> Maybe a`: infix flip of
    `preview`. -/
@[inline] def getPreview {S A : Type u} (s : S) (l : Getting (List A) S A) : Option A :=
  preview l s

@[inherit_doc getPreview] infixl:75 " ^? " => getPreview

-- ── has / hasn't / nullOf / notNullOf ───────────

/-- `has :: Getting Any s a -> s -> Bool`: does the optic focus on at least
    one element? -/
@[inline] def has {S A : Type u} (l : Getting (List A) S A) (s : S) : Bool :=
  !(toListOf l s).isEmpty

/-- `hasn't :: Getting All s a -> s -> Bool`: does the optic focus on no
    elements at all? -/
@[inline] def hasn't {S A : Type u} (l : Getting (List A) S A) (s : S) : Bool :=
  (toListOf l s).isEmpty

/-- `nullOf :: Getting All s a -> s -> Bool`: synonym for `hasn't`, named to
    match `Data.Foldable.null`. -/
@[inline] def nullOf {S A : Type u} (l : Getting (List A) S A) (s : S) : Bool :=
  hasn't l s

/-- `notNullOf :: Getting Any s a -> s -> Bool`: synonym for `has`. -/
@[inline] def notNullOf {S A : Type u} (l : Getting (List A) S A) (s : S) : Bool :=
  has l s

-- ── foldOf / foldrOf / foldlOf ──────────────────

/-- `foldOf :: Getting a s a -> s -> a`: extract the (unique) focused value —
    `foldOf l = getConst . l Const`, exactly `view`. -/
@[inline] def foldOf {S A : Type u} (l : Getting A S A) (s : S) : A := view l s

/-- `foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r`:
    right-fold the focused elements. -/
@[inline] def foldrOf {S A R : Type u} (l : Getting (List A) S A) (f : A → R → R) (z : R)
    (s : S) : R :=
  (toListOf l s).foldr f z

/-- `foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r`:
    left-fold the focused elements. -/
@[inline] def foldlOf {S A R : Type u} (l : Getting (List A) S A) (f : R → A → R) (z : R)
    (s : S) : R :=
  (toListOf l s).foldl f z

-- ── anyOf / allOf / andOf / orOf ────────────────

/-- `anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool`: does any focused
    element satisfy the predicate? -/
@[inline] def anyOf {S A : Type u} (l : Getting (List A) S A) (p : A → Bool) (s : S) : Bool :=
  (toListOf l s).any p

/-- `allOf :: Getting All s a -> (a -> Bool) -> s -> Bool`: do all focused
    elements satisfy the predicate? -/
@[inline] def allOf {S A : Type u} (l : Getting (List A) S A) (p : A → Bool) (s : S) : Bool :=
  (toListOf l s).all p

/-- `andOf :: Getting All s Bool -> s -> Bool`: conjunction of every focused
    `Bool`. -/
@[inline] def andOf {S : Type} (l : Getting (List Bool) S Bool) (s : S) : Bool :=
  (toListOf l s).all id

/-- `orOf :: Getting Any s Bool -> s -> Bool`: disjunction of every focused
    `Bool`. -/
@[inline] def orOf {S : Type} (l : Getting (List Bool) S Bool) (s : S) : Bool :=
  (toListOf l s).any id

-- ── elemOf / lengthOf ────────────────────────────

/-- `elemOf :: Eq a => Getting Any s a -> a -> s -> Bool`: is `a` among the
    focused elements? -/
@[inline] def elemOf {S A : Type u} [BEq A] (l : Getting (List A) S A) (a : A) (s : S) : Bool :=
  (toListOf l s).contains a

/-- `lengthOf :: Getting (Endo (Endo Int)) s a -> s -> Int`: how many
    elements does the optic focus on? -/
@[inline] def lengthOf {S A : Type u} (l : Getting (List A) S A) (s : S) : Nat :=
  (toListOf l s).length

-- ── firstOf / lastOf ────────────────────────────

/-- `firstOf :: Getting (Leftmost a) s a -> s -> Maybe a`: the first focused
    element, if any — implemented directly as `List.head?` over `toListOf`,
    per this batch's own guidance (see the module's deviation note). -/
@[inline] def firstOf {S A : Type u} (l : Getting (List A) S A) (s : S) : Option A :=
  (toListOf l s).head?

/-- `lastOf :: Getting (Rightmost a) s a -> s -> Maybe a`: the last focused
    element, if any — `List.getLast?` over `toListOf`. -/
@[inline] def lastOf {S A : Type u} (l : Getting (List A) S A) (s : S) : Option A :=
  (toListOf l s).getLast?

-- ── sumOf / productOf ───────────────────────────

/-- `sumOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a`: sum of every
    focused element. -/
@[inline] def sumOf {S A : Type u} [Add A] [OfNat A 0] (l : Getting (List A) S A) (s : S) : A :=
  Data.Foldable.sum (toListOf l s)

/-- `productOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a`: product of
    every focused element. -/
@[inline] def productOf {S A : Type u} [Mul A] [OfNat A 1] (l : Getting (List A) S A) (s : S) :
    A :=
  Data.Foldable.product (toListOf l s)

-- ── minimumOf / maximumOf ───────────────────────

/-- `minimumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe
    a`: the smallest focused element, if any. -/
@[inline] def minimumOf {S A : Type u} [Min A] (l : Getting (List A) S A) (s : S) : Option A :=
  Data.Foldable.minimum? (toListOf l s)

/-- `maximumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe
    a`: the largest focused element, if any. -/
@[inline] def maximumOf {S A : Type u} [Max A] (l : Getting (List A) S A) (s : S) : Option A :=
  Data.Foldable.maximum? (toListOf l s)

-- ── findOf ──────────────────────────────────────

/-- `findOf :: Getting (Endo (Maybe a)) s a -> (a -> Bool) -> s -> Maybe a`:
    the first focused element satisfying the predicate, if any. -/
@[inline] def findOf {S A : Type u} (l : Getting (List A) S A) (p : A → Bool) (s : S) :
    Option A :=
  (toListOf l s).find? p

-- ── foldByOf / foldMapByOf ──────────────────────

/-- `foldByOf :: Fold s a -> (a -> a -> a) -> a -> s -> a`: fold the focused
    elements with an arbitrary (not type-class-resolved) combining function
    and seed value — `foldByOf l f z = foldrOf l f z`, matching upstream's
    own definition (see the module's scope note on `Folding`). -/
@[inline] def foldByOf {S A : Type u} (l : Getting (List A) S A) (f : A → A → A) (z : A)
    (s : S) : A :=
  foldrOf l f z s

/-- `foldMapByOf :: Fold s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r`:
    like `foldByOf`, but first maps each focused element through `g` —
    `foldMapByOf l f z g = foldrOf l (f . g) z`. -/
@[inline] def foldMapByOf {S A R : Type u} (l : Getting (List A) S A) (f : R → R → R) (z : R)
    (g : A → R) (s : S) : R :=
  foldrOf l (fun a acc => f (g a) acc) z s

-- ── filtered ────────────────────────────────────

/-- `filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a`:
    an "affine" optic that visits its argument only if it satisfies a
    predicate, leaving it untouched otherwise.

    **Deviation from upstream's `Optic'`.** As with `Getter`'s `to`/`like`,
    `linen`'s concrete optics are plain `LensLike`-shaped functions of `→`
    rather than `Profunctor`-parameterized ones, so `filtered` lands
    directly at the concrete `Traversal'` shape here — the most general
    concrete alias its actual implementation (needing only `Applicative`,
    not `Contravariant`) supports, letting it double as a `Fold` wherever
    one is expected (any `Traversal'` is usable as a `Fold`) or compose with
    an actual `Traversal` to build one that also writes. -/
@[inline] def filtered {A : Type u} (p : A → Bool) : Traversal' A A :=
  fun {F} [Applicative F] afa a => if p a then afa a else (pure a : F A)

end Control.Lens
