/-
  Tests for `Linen.Control.Arrow`.

  `Arrow` / `ArrowChoice` over the `Fun` (plain-function) instance. Sum results
  (`.inl`/`.inr`) are inspected with `match`, since `Sum` has no core `BEq`.
-/
import Linen.Control.Arrow

open Control

namespace Tests.Control.Arrow

def inc : Fun Nat Nat := Arrow.arr (· + 1)
def dbl : Fun Nat Nat := Arrow.arr (· * 2)

/-! ### Arrow: arr / first / second / split -/

#guard inc.apply 5 == 6
#guard (Arrow.first inc).apply (5, "x") == (6, "x")
#guard (Arrow.second inc).apply ("x", 5) == ("x", 6)
#guard (Arrow.split inc dbl).apply (5, 5) == (6, 10)

/-! ### ArrowChoice: left / right -/

#guard (match (ArrowChoice.left inc).apply (Sum.inl 5 : Nat ⊕ String) with
        | .inl n => n == 6   | .inr _ => false)
#guard (match (ArrowChoice.left inc).apply (Sum.inr "x" : Nat ⊕ String) with
        | .inr s => s == "x" | .inl _ => false)
#guard (match (ArrowChoice.right inc).apply (Sum.inr 5 : String ⊕ Nat) with
        | .inr n => n == 6   | .inl _ => false)
#guard (match (ArrowChoice.right inc).apply (Sum.inl "x" : String ⊕ Nat) with
        | .inl s => s == "x" | .inr _ => false)

/-! ### ArrowChoice: fanin (merge branches) -/

#guard (ArrowChoice.fanin inc dbl).apply (Sum.inl 5 : Nat ⊕ Nat) == 6
#guard (ArrowChoice.fanin inc dbl).apply (Sum.inr 5 : Nat ⊕ Nat) == 10

end Tests.Control.Arrow
