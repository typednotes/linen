/-
  Tests for `Linen.Control.Profunctor.Closed`.

  `Closed` over `Tagged`, `Control.Fun`, and `Costar`; the `curry'` helper.
-/
import Linen.Control.Profunctor.Closed

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Closed

/-! ### Closed: Tagged -/

def tag : Tagged String Nat := ⟨42⟩

#guard (Closed.closed (X := Bool) tag).unTagged true == 42

/-! ### Closed: Control.Fun -/

def inc : Fun Nat Nat := ⟨(· + 1)⟩

#guard (Closed.closed (X := Bool) inc).apply (fun _ => 5) true == 6

/-! ### Closed: Costar -/

def costarSum : Costar List Nat Nat := ⟨List.foldl (· + ·) 0⟩

def g1 : Bool → Nat | true => 1 | false => 0
def g2 : Bool → Nat := fun _ => 2
def g3 : Bool → Nat | true => 3 | false => 0

#guard (Closed.closed (X := Bool) costarSum).runCostar [g1, g2, g3] true == 6

/-! ### curry' -/

def addFun : Fun (Nat × Nat) Nat := ⟨fun (a, b) => a + b⟩

#guard (curry' addFun).apply 5 7 == 12

end Tests.Control.Profunctor.Closed
