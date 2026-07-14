/-
  Linen.Control.Profunctor.Adjunction — the `ProfunctorAdjunction` typeclass

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Adjunction`
  (module #11 of `docs/imports/profunctors/dependencies.md`). Ported after
  `Linen.Control.Profunctor.Monad` and
  `Linen.Control.Profunctor.Composition` rather than strictly at plan
  position #11 — see the deviation note at the top of
  `Linen.Control.Profunctor.Monad` for why the real upstream dependency
  order differs from the plan's.

  `ProfunctorAdjunction f u` witnesses that the profunctor functors `f` and
  `u` are adjoint: `f` is left adjoint to `u` in the 2-category of
  profunctors. The only concrete instance upstream actually ships (`Cayley
  f`/`Cayley g` from a `Hask`-level `Adjunction`) is itself commented out in
  the source; the one *uncommented* concrete instance,
  `ProfunctorAdjunction (Procompose p) (Rift p)`, is ported below.
-/

import Linen.Control.Profunctor.Composition
import Linen.Control.Profunctor.Monad

open Control

namespace Control.Profunctor

/-- `f` is left adjoint to `u` in the 2-category of profunctors.

    Laws:
    $$\text{unit} \circ \text{counit} = \text{id}$$
    $$\text{counit} \circ \text{unit} = \text{id}$$ -/
class ProfunctorAdjunction
    (F U : (Type u → Type u → Type v) → Type u → Type u → Type v)
    [ProfunctorFunctor F] [ProfunctorFunctor U] where
  unit {P : Type u → Type u → Type v} [Profunctor P] : NatTrans P (U (F P))
  counit {P : Type u → Type u → Type v} [Profunctor P] : NatTrans (F (U P)) P

/-- `Procompose p` is left adjoint to `Rift p`: composing then right-lifting
    (or vice versa) round-trips. -/
instance [Profunctor P] : ProfunctorAdjunction (Procompose P) (Rift P) where
  counit pr := pr.inner.runRift pr.outer
  unit {Q} [Profunctor Q] := fun q => ⟨fun p => ⟨p, q⟩⟩

end Control.Profunctor
