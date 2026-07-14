/-
  Linen.Control.Lens.Getter — `Getter`, `Getting`, `to`, `like`, `view`,
  `views`, `(^.)`, `iview`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Getter` (fetched and read
  via Hackage's rendered Haddock and source). A `Getter s a` is a read-only
  optic: it can extract an `a` from an `s`, but (being restricted to
  `Contravariant`, rather than `Applicative`, functors) can never be used to
  write one back. `Getting r s a := (a -> Const r a) -> s -> Const r s` is
  the concretely-`Const`-instantiated shape every `view`/`views`-style
  combinator actually runs a `Getter`/`Lens`/`Traversal`/`Fold` through: any
  of those wider optics can be passed wherever a `Getting r s a` is expected,
  since `Const r` is `Contravariant` (see `Linen.Data.Functor`) and
  `Applicative` whenever `r` is a monoid, satisfying every one of their
  weaker constraints.

  **Scope note (`use`/`uses`/`listening`/`listenings` and their indexed
  counterparts).** Upstream's `use`/`uses` need `MonadState`, and
  `listening`/`listenings` need `MonadWriter` — `linen` has ported neither
  an mtl-style `MonadState`/`MonadWriter` *class* (only the concrete
  `StateT`/`Reader` monads themselves, in `Linen.Control.Monad.{State,
  Reader}`), so there is no class to dispatch these combinators against.
  They are skipped here, following this codebase's existing precedent for
  MonadState-dependent combinators (see `Linen.Control.Lens.Setter`'s own
  scope note for `.=`/`%=`).

  **Scope note (`view`/`views`/`iview`, `MonadReader`).** For the same
  reason, upstream's real `view :: MonadReader s m => Getting a s a -> m a`
  is generalized over an arbitrary reader monad `m`; `linen` has likewise
  ported no `MonadReader` class. This port keeps the essential, directly
  useful degenerate case at `m := Id` (i.e. running the `Getting` directly
  against a concrete `s`), matching upstream's own definition before its
  `MonadReader` wrapping: `view l s = getConst (l Const s)`.

  **Scope note (`ito`/`ilike`/`iviews`/`iuse`/`iuses`/`ilistening`/
  `ilistenings`/`getting`).** These are index-preserving or
  `Optical`-composing variants with no call site anywhere in this batch's
  scope; only the core, non-indexed combinators plus `iview` (needed to
  exercise `IndexedGetting`/`Indexed` at all) are ported here. -/

import Linen.Control.Lens.Type
import Linen.Control.Lens.Internal.Getter

open Control.Profunctor Control.Lens.Internal Data.Functor

namespace Control.Lens

-- ── Getter ──────────────────────────────────────

/-- `Getter s a := ∀ f, (Contravariant f, Functor f) => (a -> f a) -> s -> f
    s`: a read-only optic focused on exactly one `a`. -/
abbrev Getter (S A : Type u) :=
  ∀ {F : Type u → Type u} [Contravariant F] [Functor F], (A → F A) → S → F S

-- ── Getting / IndexedGetting / Accessing ───────

/-- `Getting r s a := (a -> Const r a) -> s -> Const r s`: a `Getter`/
    `Lens`/`Traversal`/`Fold` concretely instantiated at `Data.Functor.Const
    r`, the shape `view`/`views` actually run against. -/
abbrev Getting (R S A : Type u) := (A → Const R A) → S → Const R S

/-- `IndexedGetting i m s a := Indexed i a (Const m a) -> s -> Const m s`:
    the indexed counterpart of `Getting`, used by `iview`. -/
abbrev IndexedGetting (I M S A : Type u) := Indexed I A (Const M A) → S → Const M S

/-- `Accessing p m s a := p a (Const m a) -> s -> Const m s`: `Getting`
    generalized over an arbitrary profunctor `p` in place of the bare
    function arrow. -/
abbrev Accessing (P : Type u → Type u → Type v) (M S A : Type u) := P A (Const M A) → S → Const M S

-- ── to / like ───────────────────────────────────

/-- `to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a`:
    build a `Getter`-shaped optic out of a plain function, by pushing it
    through `dimap` on the input side and `contramap` on the output side —
    `to k = dimap k (contramap k)`.

    **Deviation from upstream's `Optic' p f s a` generality.** Upstream
    states `to`/`like` over an arbitrary `Profunctor p`, since Haskell's
    `(->)` is *itself* directly usable as that profunctor with no wrapping.
    In `linen`, the analogous instance lives on the nominal wrapper
    `Control.Fun` (`Linen.Control.Profunctor.Unsafe`), not on bare `→`, so a
    `p`-polymorphic `to`/`like` could never be instantiated at the bare-arrow
    shape that `Getter`/`Lens`/`Traversal`/`Setter` are actually defined with
    (`Linen.Control.Lens.Type` deliberately keeps those as direct
    `LensLike`-style functions of `→`, not routed through a `Profunctor`
    parameter, exactly so that composing them needs nothing but ordinary
    function composition). `to`/`like` are therefore specialized here to
    land directly in `Getter S A`, matching that same "no profunctor
    parameter" choice already made for every other concrete optic alias. -/
@[inline] def to {S A : Type u} (sa : S → A) : Getter S A :=
  fun {F} [Contravariant F] [Functor F] afa s => Contravariant.contramap sa (afa (sa s))

/-- `like :: (Profunctor p, Contravariant f, Functor f) => a -> Optic' p f s
    a`: a `Getter` that ignores its input and always focuses on the same
    fixed value — `like a = to (const a)`. See `to`'s doc comment for why
    this lands directly in `Getter S A` rather than a `Profunctor`-polymorphic
    `Optic'`. -/
@[inline] def like {S A : Type u} (a : A) : Getter S A :=
  to (fun (_ : S) => a)

-- ── view / views ────────────────────────────────

/-- `view :: Getting a s a -> s -> a`: run a `Getter`/`Lens`/`Traversal`/
    `Fold` to extract the focused value — `view l s = getConst (l Const s)`.
    See the module's scope note for why this is the direct, non-`MonadReader`
    form. -/
@[inline] def view {S A : Type u} (l : Getting A S A) (s : S) : A :=
  (l (Const.mk) s).getConst

/-- `(^.) :: s -> Getting a s a -> a`: infix flip of `view`. -/
@[inline] def getView {S A : Type u} (s : S) (l : Getting A S A) : A := view l s

@[inherit_doc getView] infixl:75 " ^. " => getView

/-- `views :: LensLike' (Const r) s a -> (a -> r) -> s -> r`: run a `Getter`/
    `Lens`/`Traversal`/`Fold`, post-processing the focused value with `f`
    before extracting it — `views l f s = getConst (l (Const . f) s)`. See
    the module's scope note for why this is the direct, non-`MonadReader`
    form. -/
@[inline] def views {S A R : Type u} (l : LensLike' (Const R) S A) (f : A → R) (s : S) : R :=
  (l (fun a => Const.mk (f a)) s).getConst

-- ── iview ───────────────────────────────────────

/-- `iview :: IndexedGetting i (i, a) s a -> s -> (i, a)`: like `view`, but
    for an indexed optic — also recovers the index at which the value was
    focused. See the module's scope note for why this is the direct,
    non-`MonadReader` form. -/
@[inline] def iview {I S A : Type u} (l : IndexedGetting I (I × A) S A) (s : S) : I × A :=
  (l (Indexed.mk (fun i a => Const.mk (i, a))) s).getConst

/-- `(^@.) :: s -> IndexedGetting i (i, a) s a -> (i, a)`: infix flip of
    `iview`. -/
@[inline] def getIView {I S A : Type u} (s : S) (l : IndexedGetting I (I × A) S A) : I × A :=
  iview l s

@[inherit_doc getIView] infixl:75 " ^@. " => getIView

end Control.Lens
