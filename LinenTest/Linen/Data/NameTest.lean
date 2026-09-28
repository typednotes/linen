/-
  Tests for `Linen.Data.Name`: reading names back from their dotted syntax.
-/
import Linen.Data.Name

namespace Tests.Data.Name

def ok? (e : Except String Lean.Name) (n : Lean.Name) : Bool :=
  match e with | .ok m => m == n | .error _ => false

def err? (e : Except String Lean.Name) (msg : String) : Bool :=
  match e with | .error m => m == msg | .ok _ => false

#guard ok? (Data.Name.parse "Foo.bar") `Foo.bar
#guard ok? (Data.Name.parse "double.1") (.num `double 1)
#guard ok? (Data.Name.parse "«a.b».c") (.str (.str .anonymous "a.b") "c")
#guard ok? (Data.Name.parse "«»") (.str .anonymous "")
#guard ok? (Data.Name.parse "α.x'") (.str (.str .anonymous "α") "x'")
-- Malformed input is an error, never a panic.
#guard err? (Data.Name.parse "a..b") "`a..b` is not a name in Lean's dotted syntax"
#guard (Data.Name.parse "a.1x") matches .error _     -- `String.toName` reaches `unreachable!`
#guard (Data.Name.parse "a.«b") matches .error _     -- unterminated escape
#guard (Data.Name.parse "a b") matches .error _      -- needs `«a b»`
#guard (Data.Name.parse "") matches .error _

-- `toString` round-trips for ordinary names, not for `anonymous`.
#guard Data.Name.roundTrips `Foo.bar && Data.Name.roundTrips (.num `x 2)
#guard Data.Name.roundTrips (.str .anonymous "a b")
#guard !Data.Name.roundTrips .anonymous

end Tests.Data.Name
