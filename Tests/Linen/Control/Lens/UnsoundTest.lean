/-
  Tests for `Linen.Control.Lens.Unsound`.
-/
import Linen.Control.Lens.Unsound

open Control.Lens

namespace Tests.Linen.Control.Lens.Unsound

structure Point where
  x : Nat
  y : Nat
deriving Repr, BEq, DecidableEq

def xL : Lens' Point Nat := lens Point.x (fun p x => { p with x := x })
def yL : Lens' Point Nat := lens Point.y (fun p y => { p with y := y })

/-- `lensProduct` of two lenses aimed at *disjoint* fields (`xL`/`yL`) is
    well-behaved: reading and writing a pair through it behaves exactly
    like reading/writing `x`/`y` independently. -/
def xyL : Lens' Point (Nat × Nat) := lensProduct xL yL

#guard view xyL ⟨3, 4⟩ = (3, 4)
#guard set xyL (10, 20) ⟨3, 4⟩ = ⟨10, 20⟩
#guard over xyL (fun (x, y) => (x + 1, y + 1)) ⟨3, 4⟩ = ⟨4, 5⟩

-- "You get what you put in" holds under the disjoint-fields side condition.
#guard view xyL (set xyL (10, 20) ⟨3, 4⟩) = (10, 20)
example (p : Point) : view xyL (set xyL (view xyL p) p) = view xyL p := rfl

-- Composing the same lens with itself (`lensProduct xL xL`) instead violates
-- the very law demonstrated above — this is the module's own documented
-- warning example (`badLens`), included to illustrate the *unsound* case
-- rather than the intended usage.
def badLens : Lens' Point (Nat × Nat) := lensProduct xL xL

#guard view badLens (set badLens (1, 2) ⟨3, 4⟩) = (2, 2) -- not `(1, 2)` !

end Tests.Linen.Control.Lens.Unsound
