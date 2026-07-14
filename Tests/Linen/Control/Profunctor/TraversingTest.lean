/-
  Tests for `Linen.Control.Profunctor.Traversing`.

  `Traversing`/`wander`/`traverse'` over `Control.Fun` and `Star`; the
  `firstTraversing`/`leftTraversing` default-implementation helpers.
-/
import Linen.Control.Profunctor.Traversing

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Traversing

/-! ### Traversing: Control.Fun -/

def inc : Fun Nat Nat := ⟨(· + 1)⟩

#guard (Traversing.traverse' (T := List) inc).apply [1, 2, 3] == [2, 3, 4]
#guard (Traversing.traverse' (T := Option) inc).apply (some 5) == some 6

/-! ### Traversing: Star -/

def starInc : Star Option Nat Nat := ⟨fun n => some (n + 1)⟩

#guard (Traversing.traverse' (T := List) starInc).runStar [1, 2, 3] == some [2, 3, 4]

/-! ### firstTraversing / leftTraversing -/

#guard (firstTraversing inc).apply (5, "x") == (6, "x")
#guard (match (leftTraversing inc).apply (Sum.inl 5 : Nat ⊕ String) with
        | .inl n => n == 6  | .inr _ => false)
#guard (match (leftTraversing inc).apply (Sum.inr "x" : Nat ⊕ String) with
        | .inr s => s == "x" | .inl _ => false)

end Tests.Control.Profunctor.Traversing
