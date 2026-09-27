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
#guard err? (Data.Name.parse "a..b") "empty component"
#guard err? (Data.Name.parse "a.1x") "label `a.1x`: component `1x` starts with a digit"
#guard err? (Data.Name.parse "a.«b") "label `a.«b`: unterminated `«`"

-- `toString` round-trips for ordinary names, not for `anonymous`.
#guard Data.Name.roundTrips `Foo.bar && Data.Name.roundTrips (.num `x 2)
#guard Data.Name.roundTrips (.str .anonymous "a b")
#guard !Data.Name.roundTrips .anonymous

end Tests.Data.Name
