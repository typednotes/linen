/-
  Tests for `Linen.Control.Lens.Review`.
-/
import Linen.Control.Lens.Review
import Linen.Control.Lens.Prism

open Control.Lens

namespace Tests.Linen.Control.Lens.Review

-- `unto`: build a write-only optic straight from a plain function.
#guard review (unto (fun n => n + 1)) (3 : Nat) = 4

-- Any `Prism` is directly runnable as an `AReview` (it's already polymorphic
-- enough to instantiate at `P := Tagged`, `F := Id`).
#guard review (_Just (A := Nat)) 3 = some (3 : Nat)
#guard review (_Left (C := Nat)) 3 = (Sum.inl 3 : Nat ⊕ Nat)

-- `(#)`: infix alias for `review`.
#guard ((_Just (A := Nat)) # 3) = some (3 : Nat)

-- `reviews`: `review`, then post-process.
#guard reviews (_Just (A := Nat)) (fun o => o.getD 0 + 1) 3 = 4

-- `re`: view an `AReview` "backwards", as a `Getter`.
#guard view (re (_Just (A := Nat))) 3 = some (3 : Nat)

-- `un`: turn a `Getter` around into a write-only optic that rebuilds via the
-- getter's own view function.
def negGetter : Getter Int Int := to (fun n => -n)
#guard review (un negGetter) (3 : Int) = -3

end Tests.Linen.Control.Lens.Review
