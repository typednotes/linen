/-
  Linen.Control.Lens.Internal.Profunctor — `WrappedPafb`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Profunctor`. The
  real upstream module (fetched and read via Hackage's rendered source,
  not recalled from memory) is much smaller than this batch's own plan
  assumed: it exports exactly one type, `WrappedPafb`, with `Profunctor` and
  `Choice` instances. `Bicontravariant`/`Conjoined`/`Indexable` — which the
  plan expected here — actually live in upstream's own
  `Control.Lens.Internal.Indexed` (`Conjoined`/`Indexable`) and are ported
  there instead, in `Linen.Control.Lens.Internal.Indexed`; no real upstream
  module defines a `Bicontravariant` class at all (a plan artifact, dropped).

  `WrappedPafb F P A B := P A (F B)`: a profunctor `P` whose second argument
  has been pushed through an outer functor `F` — used upstream to let
  `Choice`-polymorphic code build values of shape `p a (f b)` (e.g. inside
  `Control.Lens.Prism`'s `outside`) while still presenting a `Choice`
  interface at the `WrappedPafb` level itself.
-/

import Linen.Control.Profunctor.Choice

open Control Control.Profunctor

namespace Control.Lens.Internal

/-- `WrappedPafb F P A B := P A (F B)`: `P`'s second argument pushed through
    an outer functor `F`. -/
structure WrappedPafb (F : Type u → Type u) (P : Type u → Type u → Type u) (A B : Type u) where
  /-- Unwrap to the underlying `P A (F B)`. -/
  unwrapPafb : P A (F B)

/-- `WrappedPafb F P` is a `Profunctor` whenever `F` is a `Functor` and `P` is
    a `Profunctor`: map the covariant side through both layers at once. -/
instance {F : Type u → Type u} {P : Type u → Type u → Type u} [Functor F] [Profunctor P] :
    Profunctor (WrappedPafb F P) where
  dimap f g w := ⟨Profunctor.dimap f (fun fb => g <$> fb) w.unwrapPafb⟩
  lmap f w := ⟨Profunctor.lmap f w.unwrapPafb⟩
  rmap g w := ⟨Profunctor.rmap (fun fb => g <$> fb) w.unwrapPafb⟩

/-- Sequence an `Either` with the functor-wrapping on its left branch:
    $\text{sequenceL} : F\,\alpha \oplus \gamma \to F\,(\alpha \oplus \gamma)$. -/
def sequenceL {F : Type u → Type u} [Applicative F] : F α ⊕ γ → F (α ⊕ γ)
  | .inl fa => Sum.inl <$> fa
  | .inr c => pure (.inr c)

/-- Sequence an `Either` with the functor-wrapping on its right branch:
    $\text{sequenceR} : \gamma \oplus F\,\alpha \to F\,(\gamma \oplus \alpha)$. -/
def sequenceR {F : Type u → Type u} [Applicative F] : γ ⊕ F α → F (γ ⊕ α)
  | .inl c => pure (.inl c)
  | .inr fa => Sum.inr <$> fa

/-- `WrappedPafb F P` is `Choice` whenever `F` is `Applicative` and `P` is
    `Choice`: thread the extra alternative through `P`, then resolve the
    resulting `Either (F _) _` back into `F (Either _ _)` via `sequenceL`/
    `sequenceR`. -/
instance {F : Type u → Type u} {P : Type u → Type u → Type u} [Applicative F] [Choice P] :
    Choice (WrappedPafb F P) where
  left' w := ⟨Profunctor.rmap sequenceL (Choice.left' w.unwrapPafb)⟩
  right' w := ⟨Profunctor.rmap sequenceR (Choice.right' w.unwrapPafb)⟩

end Control.Lens.Internal
