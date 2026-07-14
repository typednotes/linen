/-
  Linen.Control.Lens.Internal.Getter — `noEffect`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Getter` (fetched and
  read via Hackage's rendered source). Upstream exports one small utility,
  `noEffect`, plus two newtypes, `AlongsideLeft`/`AlongsideRight` (each
  wrapping `f (a, b)` to derive `Functor`/`Contravariant`/`Foldable`/
  `Traversable`/`Bifunctor`/`Bifoldable`/`Bitraversable` instances that
  operate on one component of the pair).

  **Scope note (`AlongsideLeft`/`AlongsideRight`).** These back upstream's
  public `alongside` combinator (`Control.Lens.Getter`), which is out of
  scope until a later batch — no module in this batch's scope calls into
  either newtype. They would also need `Bifunctor`/`Bifoldable`/
  `Bitraversable` machinery `linen` has not ported. Rather than manufacture
  that infrastructure with no call site, this port keeps only `noEffect`,
  the one piece `Control.Lens.Internal.Fold` (this batch's other module)
  actually needs.

  `noEffect` is "the `mempty` equivalent for a `Contravariant` `Applicative`
  functor": since a contravariant applicative like `Data.Functor.Const r`
  never actually uses its (phantom) covariant slot, a value of `F α` can be
  built for *any* `α` from a single `F Unit`, by contramapping every possible
  `α` down to `()`. -/

import Linen.Data.Functor

open Data.Functor

namespace Control.Lens.Internal

/-- The `mempty` equivalent for a `Contravariant` `Applicative` functor:
    given any way to produce an `F Unit`, contramap it up to an arbitrary
    `F α`, discarding whatever `α` would have been. -/
@[inline] def noEffect {F : Type u → Type u} [Contravariant F] [Pure F] : F α :=
  Contravariant.contramap (fun (_ : α) => PUnit.unit) (Pure.pure PUnit.unit : F PUnit)

end Control.Lens.Internal
