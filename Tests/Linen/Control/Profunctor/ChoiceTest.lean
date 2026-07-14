/-
  Tests for `Linen.Control.Profunctor.Choice`.

  `Choice` over `Control.Fun`, `Star`, `Tagged`, and `WrappedArrow`; `Cochoice`
  over `Forget`. (`Sum` has no core `BEq`, so those results are inspected
  with `match`.)
-/
import Linen.Control.Profunctor.Choice

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Choice

/-! ### Choice: Control.Fun -/

def inc : Fun Nat Nat := ⟨(· + 1)⟩

#guard (match (Choice.left' inc).apply (Sum.inl 5 : Nat ⊕ String) with
        | .inl n => n == 6  | .inr _ => false)
#guard (match (Choice.left' inc).apply (Sum.inr "x" : Nat ⊕ String) with
        | .inr s => s == "x" | .inl _ => false)
#guard (match (Choice.right' inc).apply (Sum.inr 5 : String ⊕ Nat) with
        | .inr n => n == 6  | .inl _ => false)

/-! ### Choice: Star -/

def starInc : Star Option Nat Nat := ⟨fun n => some (n + 1)⟩

#guard (match (Choice.left' starInc).runStar (Sum.inl 5 : Nat ⊕ String) with
        | some (.inl n) => n == 6 | _ => false)
#guard (match (Choice.left' starInc).runStar (Sum.inr "x" : Nat ⊕ String) with
        | some (.inr s) => s == "x" | _ => false)

/-! ### Choice: Tagged -/

def tag : Tagged String Nat := ⟨42⟩

def leftTag : Tagged (String ⊕ Bool) (Nat ⊕ Bool) := Choice.left' tag
def rightTag : Tagged (Bool ⊕ String) (Bool ⊕ Nat) := Choice.right' tag

#guard (match leftTag with
        | ⟨.inl n⟩ => n == 42 | ⟨.inr _⟩ => false)
#guard (match rightTag with
        | ⟨.inr n⟩ => n == 42 | ⟨.inl _⟩ => false)

/-! ### Cochoice: Forget -/

def forgetSumLeft : Forget Nat (Nat ⊕ Bool) (String ⊕ Bool) :=
  ⟨fun | .inl n => n | .inr b => if b then 1 else 0⟩

def forgetSumRight : Forget Nat (Bool ⊕ Nat) (Bool ⊕ String) :=
  ⟨fun | .inl _ => 0 | .inr n => n⟩

def unrightResult : Forget Nat Nat String := Cochoice.unright forgetSumRight

#guard (Cochoice.unleft forgetSumLeft).runForget 5 == 5
#guard unrightResult.runForget 5 == 5

end Tests.Control.Profunctor.Choice
