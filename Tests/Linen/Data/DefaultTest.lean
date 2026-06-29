/-
  Tests for `Linen.Data.Default` — the `Default` typeclass.
-/
import Linen.Data.Default

open Data

namespace Tests.Data.Default

/-! ### default values per instance -/

#guard (Default.default : Bool) == false
#guard (Default.default : Nat) == 0
#guard (Default.default : Int) == 0
#guard (Default.default : String) == ""
#guard (Default.default : List Nat) == []
#guard (Default.default : Array Nat) == #[]
#guard (Default.default : Option Nat) == none
#guard (Default.default : Unit) == ()
#guard (Default.default : Nat × String) == (0, "")
#guard (Default.default : Bool × (List Nat)) == (false, [])

/-! ### default is `false`, unlike a mere inhabitant -/

example : (Default.default : Bool) = false := rfl
example : (Default.default : Nat × Int) = (0, 0) := rfl

end Tests.Data.Default
