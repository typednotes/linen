/-
  Tests for `Linen.Control.Lens.Internal.Iso`.

  `Exchange Nat Nat Nat`: `Functor`/`Profunctor` instances.
-/
import Linen.Control.Lens.Internal.Iso

open Control Control.Profunctor Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Iso

/-- The `Iso Nat Nat Nat Nat` doubling `n ↦ 2 * n` / `n ↦ n / 2`, reified as
    an `Exchange`. -/
def doubled : Exchange Nat Nat Nat Nat := ⟨(· * 2), (· / 2)⟩

/-! ### Functor -/

#guard (Functor.map (· + 1) doubled).sa 5 == 10
#guard (Functor.map (· + 1) doubled).bt 10 == 6

/-! ### Profunctor -/

#guard (Profunctor.rmap (· + 1) doubled).sa 5 == 10
#guard (Profunctor.rmap (· + 1) doubled).bt 10 == 6

#guard (Profunctor.lmap (· + 10) doubled).sa 5 == 30
#guard (Profunctor.lmap (· + 10) doubled).bt 10 == 5

#guard (Profunctor.dimap (· + 10) (· + 1) doubled).sa 5 == 30
#guard (Profunctor.dimap (· + 10) (· + 1) doubled).bt 10 == 6

end Tests.Control.Lens.Internal.Iso
