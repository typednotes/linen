/-
  Tests for `Linen.Control.Lens.Lens`.
-/
import Linen.Control.Lens.Lens

open Control.Lens

namespace Tests.Linen.Control.Lens.Lens

structure Point where
  x : Nat
  y : Nat
deriving Repr, BEq, DecidableEq

/-- `lens` builds a `Lens'` out of a getter and a setter. -/
def xL : Lens' Point Nat :=
  lens Point.x (fun p x => { p with x := x })

#guard view xL ⟨3, 4⟩ = 3
#guard over xL (· + 1) ⟨3, 4⟩ = ⟨4, 4⟩
#guard set xL 10 ⟨3, 4⟩ = ⟨10, 4⟩

-- The lens laws hold definitionally for `xL`.
example (p : Point) : set xL (view xL p) p = p := by cases p; rfl
example (p : Point) (n : Nat) : view xL (set xL n p) = n := rfl
example (p : Point) (n m : Nat) : set xL m (set xL n p) = set xL m p := by cases p; rfl

-- `(%%~)` runs a `Lens` (or any `LensLike`) exactly as plain application —
-- here specialized at `Id`, matching `over`. Stated as `example ... := rfl`
-- rather than `#guard`, since `#guard`'s automatic `Decidable`-to-`Bool`
-- coercion doesn't unfold through `Id`/`overF` far enough to find the
-- derived `DecidableEq Point` instance, even though the equality holds by
-- `rfl` outright (per `AGENTS.md`'s guidance for such cases).
example : ((xL %%~ (fun n => (n + 1 : Id Nat))) (⟨3, 4⟩ : Point) : Point) = ⟨4, 4⟩ := rfl

-- `(<%~)` pairs the result with the value written.
#guard (xL <%~ (· + 1)) (⟨3, 4⟩ : Point) = (4, ⟨4, 4⟩)

-- `(<<%~)` pairs the result with the value read out (the *old* value).
#guard (xL <<%~ (· + 1)) (⟨3, 4⟩ : Point) = (3, ⟨4, 4⟩)

-- `united`: every value has a trivial `()` lens.
#guard view (united (A := Point)) ⟨3, 4⟩ = ()
#guard set (united (A := Point)) () ⟨3, 4⟩ = ⟨3, 4⟩

end Tests.Linen.Control.Lens.Lens
