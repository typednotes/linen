/-
  Tests for `Linen.Data.Float`.

  Pure, so everything is checked with `#guard`. This module consolidated three
  previously separate copies, so the assertions cover what each of them relied
  on: whole-string parsing for `Data.Yaml` and `Database.SQL.Decoders`, and the
  `ofScientificParts` assembly the JSON decoder shares.
-/
import Linen.Data.Float

open Data.Float

namespace Tests.Data.Float

/-! ### Assembly

  `ofScientificParts s m e` is $(-1)^s \times m \times 10^{e}$. -/

#guard ofScientificParts false 15 (-1) == 1.5
#guard ofScientificParts true 15 (-1) == -1.5
#guard ofScientificParts false 1 3 == 1000.0
#guard ofScientificParts false 0 0 == 0.0
#guard ofScientificParts false 123 0 == 123.0
#guard ofScientificParts true 0 0 == 0.0        -- negative zero equals zero

/-! ### Whole-string parsing -/

#guard parseFloat? "1.5" == some 1.5
#guard parseFloat? "-1.5" == some (-1.5)
#guard parseFloat? "+1.5" == some 1.5
#guard parseFloat? "0" == some 0.0
#guard parseFloat? "42" == some 42.0             -- no fractional part needed
#guard parseFloat? ".5" == some 0.5              -- no integer part needed
#guard parseFloat? "5." == some 5.0
#guard parseFloat? "  1.5  " == some 1.5         -- surrounding space ignored

/-! ### Exponents -/

#guard parseFloat? "1e3" == some 1000.0
#guard parseFloat? "1E3" == some 1000.0
#guard parseFloat? "1e+3" == some 1000.0
#guard parseFloat? "1e-3" == some 0.001
#guard parseFloat? "1.5e2" == some 150.0
#guard parseFloat? "-2.5e-1" == some (-0.25)

/-! ### Rejected

  The *whole* string must parse, so trailing junk is an error rather than a
  silent truncation — which is what lets `Data.Yaml` tell a float from a
  version string. -/

#guard parseFloat? "" == none
#guard parseFloat? "abc" == none
#guard parseFloat? "1.5x" == none
#guard parseFloat? "1.2.3" == none               -- a version, not a float
#guard parseFloat? "1e" == none                  -- exponent with no digits
#guard parseFloat? "1e+" == none
#guard parseFloat? "-" == none
#guard parseFloat? "." == none

/-! ### Agreement with float literals

  Parsing goes through `Float.ofScientific`, the same primitive the compiler
  uses for literals, so the two must agree. -/

#guard parseFloat? "3.14159" == some 3.14159
#guard parseFloat? "2.718281828" == some 2.718281828
#guard parseFloat? "1234567890.123" == some 1234567890.123
#guard parseFloat? "6.02e23" == some 6.02e23
#guard parseFloat? "-273.15" == some (-273.15)

/-! ### Signatures -/

example : String → Option Float := parseFloat?
example : Bool → Nat → Int → Float := ofScientificParts

end Tests.Data.Float
