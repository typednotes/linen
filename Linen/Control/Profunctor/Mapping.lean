/-
  Linen.Control.Profunctor.Mapping — the `Mapping` typeclass

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Mapping` (module
  #9 of `docs/imports/profunctors/dependencies.md`). `Mapping` is the
  `Setter`-side dual of `Traversing`: where `traverse'`/`wander` need a
  `Traversable`/an applicative-effectful update, `map'`/`roam` only need a
  `Functor`/a plain update — this is exactly the extra power
  `Control.Lens.Setter` needs over `Control.Lens.Traversal`.

  **Scope note.** Upstream's `Star`/`Kleisli` `Mapping` instances need
  `Distributive` *together with* `Applicative`
  (`map' (Star f) = Star (collect f)`); porting that combination pulls in
  more of `Distributive` than `Linen.Control.Profunctor.Rep`'s minimal
  fold-in covers for comparatively little payoff (`lens` itself only ever
  needs `Mapping` for the concrete `(->)`-shaped setter case), so only the
  `(->)` instance is given here. Upstream's `CofreeMapping`/`FreeMapping`
  (free/cofree-adjoin-mapping scaffolding) are left unported for the same
  reason as `Strong`'s `Tambara`/`Pastro`.
-/

import Linen.Control.Profunctor.Closed
import Linen.Control.Profunctor.Traversing

open Control

namespace Control.Profunctor

/-- A **mapping profunctor** lets *any* `Functor` (not just a `Traversable`
    one) pass through it — the direct profunctor counterpart of
    `Control.Lens.Setter`'s van Laarhoven encoding.

    Laws:
    $$\text{map}' \circ \text{rmap}\;f = \text{rmap}\;(\text{fmap}\;f) \circ \text{map}'$$ -/
class Mapping (P : Type u → Type u → Type v) extends Traversing P, Closed P where
  /-- $\text{roam} : ((α \to β) \to σ \to τ) \to P\,α\,β \to P\,σ\,τ$. -/
  roam : ((α → β) → σ → τ) → P α β → P σ τ
  /-- $\text{map}' : P\,a\,b \to P\,(F\,a)\,(F\,b)$ for any `Functor F`. -/
  map' {F : Type u → Type u} [Functor F] : P α β → P (F α) (F β) :=
    fun p => roam (fun ab fa => ab <$> fa) p

/-- Ordinary functions are `Mapping`: `roam` is the identity coercion, `map'`
    is `Functor.map`. -/
instance : Mapping Control.Fun where
  roam f p := ⟨f p.apply⟩
  map' p := ⟨fun fa => p.apply <$> fa⟩
  wander f ab := ⟨fun s => (f (F := Id) (fun a => (pure (ab.apply a) : Id _)) s : Id _)⟩
  closed f := ⟨fun g => f.apply ∘ g⟩

end Control.Profunctor
