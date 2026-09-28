/-
  Tests for `Linen.Data.Void`.

  `Empty` (Haskell's `Void`) is uninhabited, so there are no values to compare
  or print directly. We instead check that `Empty` works as a type parameter
  (the only `List Empty` is `[]`, so its traversals reduce), that the vacuous
  instances resolve, and that the `Empty → α` singleton law holds.
-/
import Linen.Data.Void

namespace Tests.Data.Void

/-! ### `Empty` as a type parameter -/

#guard (([] : List Empty).map (fun e => (e.elim : Nat))) == []
#guard ([] : List Empty).isEmpty
#guard (([] : List Empty).foldr (fun e _ => e.elim) 0) == 0

/-! ### vacuous instances resolve -/

example : BEq Empty := inferInstance
example : Ord Empty := inferInstance
example : Hashable Empty := inferInstance
example : ToString Empty := inferInstance
example : Inhabited (Empty → Nat) := inferInstance
-- core already provides these:
example : DecidableEq Empty := inferInstance
example : Repr Empty := inferInstance

/-! ### the function space `Empty → α` is a singleton -/

example (f : Empty → Nat) : f = Empty.elim := Empty.eq_absurd f
example (f g : Empty → Nat) : f = g := by
  rw [Empty.eq_absurd f, Empty.eq_absurd g]

end Tests.Data.Void
