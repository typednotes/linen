/-
  Tests for `Linen.Control.Lens.Internal.Profunctor`.

  `WrappedPafb Option Control.Fun`: `Profunctor`/`Choice` instances, plus
  `sequenceL`/`sequenceR`.
-/
import Linen.Control.Lens.Internal.Profunctor

open Control Control.Profunctor Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Profunctor

def incSome : Fun Nat (Option Nat) := ⟨fun n => some (n + 1)⟩

def wp : WrappedPafb Option Fun Nat Nat := ⟨incSome⟩

/-! ### Profunctor -/

#guard (Profunctor.rmap (· + 100) wp).unwrapPafb.apply 5 == some 106
#guard (Profunctor.lmap (· + 10) wp).unwrapPafb.apply 5 == some 16
#guard (Profunctor.dimap (· + 10) (· + 100) wp).unwrapPafb.apply 5 == some 116

/-! ### Choice -/

def wpc : WrappedPafb Option Fun Nat Nat := ⟨incSome⟩

#guard (Choice.left' (γ := String) wpc).unwrapPafb.apply (.inl 5) == some (.inl 6)
#guard (Choice.left' (γ := String) wpc).unwrapPafb.apply (.inr "x") == some (.inr "x")
#guard (Choice.right' (γ := String) wpc).unwrapPafb.apply (.inr 5) == some (.inr 6)
#guard (Choice.right' (γ := String) wpc).unwrapPafb.apply (.inl "x") == some (.inl "x")

/-! ### sequenceL / sequenceR -/

#guard sequenceL (Sum.inl (some 5) : Option Nat ⊕ String) == some (.inl 5)
#guard sequenceL (γ := String) (Sum.inr "x" : Option Nat ⊕ String) == some (.inr "x")
#guard sequenceR (γ := String) (Sum.inl "x" : String ⊕ Option Nat) == some (.inl "x")
#guard sequenceR (Sum.inr (some 5) : String ⊕ Option Nat) == some (.inr 5)

end Tests.Control.Lens.Internal.Profunctor
