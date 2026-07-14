/-
  Linen.Control.Profunctor.Monad — profunctor functors/monads/comonads

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Monad`. This
  module lives logically at plan position #13 of
  `docs/imports/profunctors/dependencies.md`, but is ported **ahead of**
  `Linen.Control.Profunctor.Composition` (#10) and
  `Linen.Control.Profunctor.Adjunction` (#11): reading the real upstream
  source shows `Data.Profunctor.Adjunction` actually imports
  `Data.Profunctor.Monad` (for `ProfunctorFunctor`), and
  `Data.Profunctor.Composition`'s `Procompose`/`Rift` need
  `ProfunctorFunctor`/`ProfunctorMonad`/`ProfunctorComonad` too — the plan's
  stated dependency list for #11 ("Depends on #6, #10") omits this. This
  port follows the *real* dependency order instead of the plan's numbering;
  see the final report for this noted as a deviation.

  A value of `t p` (for a "profunctor transformer" `t`) is acted on
  functorially/monadically/comonadically by natural transformations between
  the underlying profunctors — the exact profunctor-level analogue of
  `Functor`/`Monad`/`Comonad` acting on natural transformations between
  ordinary functors.

  **Scope note.** Upstream also gives concrete instances of these three
  classes for `Data.Bifunctor.Tannen`/`Product`/`Sum` (from the `bifunctors`
  package). `linen`'s own `Linen.Data.Bifunctor` does not define those
  specific bifunctor-transformer types, and no module in this plan calls
  into them, so only the class definitions are ported here; each concrete
  profunctor transformer defined later in this port (`Cayley`, `Ran`,
  `Rift`, `Yoneda`, `Coyoneda`) supplies its own instances directly instead.
-/

import Linen.Control.Profunctor.Unsafe

open Control

namespace Control.Profunctor

/-- A **natural transformation** between profunctors, written `P :→ Q`
    upstream: a polymorphic map from every `P α β` to `Q α β`. -/
def NatTrans (P Q : Type u → Type u → Type v) := ∀ {α β : Type u}, P α β → Q α β

/-- A **profunctor functor** transports natural transformations between
    profunctors to natural transformations between the transformed
    profunctors — `t` acts functorially on `Profunctor`s.

    Laws:
    $$\text{promap}\;f \circ \text{promap}\;g = \text{promap}\;(f \circ g)$$
    $$\text{promap}\;\text{id} = \text{id}$$ -/
-- `T`'s result universe `w` is deliberately independent from the profunctors'
-- own universe `v`: a transformer such as `Procompose`/`Rift` hides a bound
-- `Type u` variable (an existential/universal intermediate object) inside its
-- result, which pushes that result up to `Type (max (u + 1) v)` — strictly
-- above `Type v` in general. Haskell's single flat `Type` has no such
-- stratification, so tying `w` to `v` here would reject exactly the
-- transformers this module exists to describe.
class ProfunctorFunctor (T : (Type u → Type u → Type v) → Type u → Type u → Type w) where
  promap {P Q : Type u → Type u → Type v} [Profunctor P] [Profunctor Q] :
    NatTrans P Q → NatTrans (T P) (T Q)

/-- A **profunctor monad**: `T` is a monad on the category of profunctors
    equipped with natural transformations.

    Laws:
    $$\text{promap}\;f \circ \text{proreturn} = \text{proreturn} \circ f$$
    $$\text{projoin} \circ \text{proreturn} = \text{id}$$
    $$\text{projoin} \circ \text{promap}\;\text{proreturn} = \text{id}$$
    $$\text{projoin} \circ \text{projoin} = \text{projoin} \circ \text{promap}\;\text{projoin}$$ -/
class ProfunctorMonad (T : (Type u → Type u → Type v) → Type u → Type u → Type v)
    extends ProfunctorFunctor T where
  proreturn {P : Type u → Type u → Type v} [Profunctor P] : NatTrans P (T P)
  projoin {P : Type u → Type u → Type v} [Profunctor P] : NatTrans (T (T P)) (T P)

/-- A **profunctor comonad**: `T` is a comonad on the category of
    profunctors equipped with natural transformations.

    Laws:
    $$\text{proextract} \circ \text{promap}\;f = f \circ \text{proextract}$$
    $$\text{proextract} \circ \text{produplicate} = \text{id}$$
    $$\text{promap}\;\text{proextract} \circ \text{produplicate} = \text{id}$$
    $$\text{produplicate} \circ \text{produplicate} = \text{promap}\;\text{produplicate} \circ \text{produplicate}$$ -/
class ProfunctorComonad (T : (Type u → Type u → Type v) → Type u → Type u → Type v)
    extends ProfunctorFunctor T where
  proextract {P : Type u → Type u → Type v} [Profunctor P] : NatTrans (T P) P
  produplicate {P : Type u → Type u → Type v} [Profunctor P] : NatTrans (T P) (T (T P))

end Control.Profunctor
