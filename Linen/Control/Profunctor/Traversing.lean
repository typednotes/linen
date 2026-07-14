/-
  Linen.Control.Profunctor.Traversing — the `Traversing` typeclass

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Traversing`
  (module #8 of `docs/imports/profunctors/dependencies.md`). `Traversing` is
  the direct profunctor counterpart of `Control.Lens.Traversal`'s van
  Laarhoven encoding: `wander` packages exactly the shape of a van Laarhoven
  traversal, `∀ F, Applicative F ⇒ (a → F b) → s → F t`.

  **Scope note.** Upstream's `MINIMAL wander | traverse'` lets an instance
  supply either method, deriving the other; the `traverse' → wander`
  direction upstream uses is a free-applicative encoding (`Baz`, a
  `Bazaar`-shaped rank-2 existential — the same construction
  `Control.Lens.Internal.Bazaar` needs downstream) purely to make that one
  default implementation type-check, with no further use anywhere else in
  this package. Duplicating that machinery here to support a default that
  every concrete instance in this module bypasses anyway (each one below
  defines `wander` directly, exactly as upstream's own instances do) isn't
  worth the added surface, so this port makes `wander` the primitive method
  and defaults `traverse'` from it — the direction that needs no auxiliary
  type at all.

  **Scope note (`Forget`).** Upstream also gives `Monoid m => Traversing
  (Forget m)`; `linen` has no general `Monoid` class yet (only the
  ad-hoc `Append`/`Inhabited` pairing `Data.Functor.Const`'s `Pure` instance
  uses), so that instance is left for whenever such a class is ported.
-/

import Linen.Control.Profunctor.Choice
import Linen.Control.Profunctor.Strong
import Linen.Data.Traversable

open Control

namespace Control.Profunctor

/-- A **traversing profunctor** lets a `Traversable` structure pass through
    it, in the van Laarhoven shape `wander` packages directly.

    Laws:
    $$\text{traverse}' = \text{wander}\;\text{traverse}$$
    $$\text{dimap}\;\text{Id}\;\text{runId} \circ \text{traverse}' = \text{id}$$ -/
class Traversing (P : Type u → Type u → Type v) extends Choice P, Strong P where
  /-- The van Laarhoven traversal packaged directly:
      $\text{wander} : (\forall F,\,\text{Applicative}\,F \Rightarrow (α \to F\,β) \to σ \to F\,τ) \to P\,α\,β \to P\,σ\,τ$. -/
  wander : (∀ {F : Type u → Type u} [Applicative F], (α → F β) → σ → F τ) → P α β → P σ τ
  /-- $\text{traverse}' : P\,a\,b \to P\,(T\,a)\,(T\,b)$ for any `Traversable T`. -/
  traverse' {T : Type u → Type u} [Data.Traversable T] : P α β → P (T α) (T β) :=
    fun p => wander (fun f => Data.Traversable.traverse f) p

/-- Default `first'` for a `Traversing` profunctor, via `wander` over the
    (single-element, from the traversal's point of view) left component of
    the pair. -/
def firstTraversing [Traversing P] (p : P α β) : P (α × γ) (β × γ) :=
  Traversing.wander (fun f (s : α × γ) => (fun b => (b, s.2)) <$> f s.1) p

/-- Default `left'` for a `Traversing` profunctor, via `wander` over the
    (single-element, from the traversal's point of view) left alternative of
    the sum. -/
def leftTraversing [Traversing P] (p : P α β) : P (α ⊕ γ) (β ⊕ γ) :=
  Traversing.wander
    (fun f (s : α ⊕ γ) => match s with
      | .inl a => Sum.inl <$> f a
      | .inr c => pure (.inr c))
    p

/-- Ordinary functions are `Traversing`: `traverse'` is `Functor.map`. -/
instance : Traversing Control.Fun where
  wander f ab := ⟨fun s => (f (F := Id) (fun a => (pure (ab.apply a) : Id _)) s : Id _)⟩
  traverse' p := ⟨fun t => p.apply <$> t⟩

/-- `Star F` is `Traversing` for any `Applicative F`. -/
instance [Applicative F] : Traversing (Star F) where
  wander f amb := ⟨f amb.runStar⟩
  traverse' amb := ⟨fun t => Data.Traversable.traverse amb.runStar t⟩

end Control.Profunctor
