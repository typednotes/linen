/-
  Tests for `Linen.Data.Bool`.

  `guard'` (the only addition; the rest of Haskell's `Data.Bool` is Lean core's
  `bool`, exercised here too to document the equivalence).
-/
import Linen.Data.Bool

open Data.Bool

namespace Tests.Data.Bool

-- `guard'` yields a singleton on `true`, nothing on `false`.
#guard guard' true 7 == [7]
#guard guard' false 7 == ([] : List Nat)
#guard guard' true "a" == ["a"]

-- Haskell's `Data.Bool.bool` is Lean core's `bool` (false-case first); no re-port.
#guard bool "f" "t" false == "f"
#guard bool "f" "t" true  == "t"

-- Reduction laws (compile-time).
example (x : Nat) : guard' true x = [x] := guard'_true x
example (x : Nat) : guard' false x = [] := guard'_false x

end Tests.Data.Bool
