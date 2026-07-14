/-
  Tests for `Linen.Control.Profunctor.Adjunction`.

  The `ProfunctorAdjunction (Procompose P) (Rift P)` instance's `unit`/
  `counit`.

  **Universe note.** `Procompose P (Rift P Q)`/`Rift P (Procompose P Q)`
  combine `Rift`'s hidden `∀{X}` field (which bumps its own result universe
  above that of its flat arguments) with `Procompose`'s requirement that
  both composed slots share one literal universe. A flat, concrete
  profunctor like `Control.Fun` can never satisfy that shared-universe
  constraint here, so `counit`/`unit`'s defining equations are illustrated
  abstractly (matching how the instance itself is only ever stated
  generically) rather than via a concrete instantiation.
-/
import Linen.Control.Profunctor.Adjunction

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Adjunction

/-! ### counit: discharge a `Procompose P (Rift P Q)` down to `Q` -/

example [Profunctor P] [Profunctor Q] (pr : Procompose P (Rift P Q) α β) :
    (ProfunctorAdjunction.counit (F := Procompose P) (U := Rift P) pr) =
      pr.inner.runRift pr.outer := rfl

/-! ### unit: lift a `Q` up into `Rift P (Procompose P Q)` -/

example [Profunctor P] [Profunctor Q] (q : Q α β) :
    (ProfunctorAdjunction.unit (F := Procompose P) (U := Rift P) (P := Q) q) =
      (⟨fun p => ⟨p, q⟩⟩ : Rift P (Procompose P Q) α β) := rfl

end Tests.Control.Profunctor.Adjunction
