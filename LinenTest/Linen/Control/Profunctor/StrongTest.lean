/-
  Tests for `Linen.Control.Profunctor.Strong`.

  `Strong` over `Control.Fun`, `Star`, and `Forget`; `Costrong` over
  `Tagged`; plus the `uncurry'`/`strong` helpers.
-/
import Linen.Control.Profunctor.Strong

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Strong

/-! ### Strong: Control.Fun -/

def inc : Fun Nat Nat := ⟨(· + 1)⟩

#guard (Strong.first' inc).apply (5, "x") == (6, "x")
#guard (Strong.second' inc).apply ("x", 5) == ("x", 6)

/-! ### Strong: Star -/

def starInc : Star Option Nat Nat := ⟨fun n => some (n + 1)⟩

#guard (Strong.first' starInc).runStar (5, "x") == some (6, "x")
#guard (Strong.second' starInc).runStar ("x", 5) == some ("x", 6)

/-! ### Strong: Forget -/

def forgetLen : Forget Nat String Bool := ⟨String.length⟩

#guard (Strong.first' forgetLen).runForget ("ab", true) == 2
#guard (Strong.second' forgetLen).runForget (true, "ab") == 2

/-! ### strong / uncurry' helpers -/

#guard (strong (α := Nat) (· + ·) inc).apply 5 == 11

def addFun : Fun Nat (Nat → Nat) := ⟨fun a b => a + b⟩

#guard (uncurry' addFun).apply (5, 6) == 11

/-! ### Costrong: Tagged -/

def tagFirst : Tagged (String × Bool) (Nat × Bool) := ⟨(1, true)⟩
def tagSecond : Tagged (Bool × String) (Bool × Nat) := ⟨(true, 1)⟩

#guard (Costrong.unfirst tagFirst : Tagged String Nat).unTagged == 1
#guard (Costrong.unsecond tagSecond : Tagged String Nat).unTagged == 1

end Tests.Control.Profunctor.Strong
