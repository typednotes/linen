/-
  Tests for `Linen.Control.Lens.Equality`.
-/
import Linen.Control.Lens.Equality

open Control.Lens

namespace Tests.Linen.Control.Lens.Equality

/-- The trivial `Equality' α α`: identity, for any concrete `α`. -/
def reflEq (α : Type) : Equality' α α := fun {_P} {_F} p => p

-- `runEq` on the trivial `Equality'` recovers `rfl`. Stated as
-- `example ... := rfl` rather than `#guard`, per `AGENTS.md`'s guidance for
-- `Prop`-valued checks `#guard` cannot decide (an equality-of-types proof
-- has no `Decidable` instance at all).
example : (runEq (reflEq Nat) : Nat = Nat) = rfl := rfl

-- `runEq'` on the trivial `Equality'` also recovers `rfl`.
example : (runEq' (reflEq Nat) : Nat = Nat) = rfl := rfl

-- `mapEq` along the trivial equality is the identity, up to `cast`.
example : mapEq (reflEq Nat) (F := Id) (3 : Id Nat) = (3 : Id Nat) := rfl

/-- `simple` is definitionally the identity. -/
example (p : Nat → Id Nat) : simple (P := fun a b => a → b) p = p := rfl

end Tests.Linen.Control.Lens.Equality
