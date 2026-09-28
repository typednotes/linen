/-
  Tests for `Linen.Control.Lens.Internal.List`.

  `ordinalNub`, `stripSuffix`.
-/
import Linen.Control.Lens.Internal.List

open Control.Lens.Internal

namespace Tests.Control.Lens.Internal.List

/-! ### `ordinalNub` -/

-- Upstream's own doctest example.
#guard ordinalNub 3 [-1, 2, 1, 4, 2, 3] == [2, 1]

#guard ordinalNub 5 [0, 1, 2, 3, 4] == [0, 1, 2, 3, 4]
#guard ordinalNub 0 [0, 1, -1] == []
#guard ordinalNub 3 [] == []
#guard ordinalNub 3 [1, 1, 1] == [1]

/-! ### `stripSuffix` -/

#guard stripSuffix "bar".toList "foobar".toList == some "foo".toList
#guard stripSuffix "baz".toList "foobar".toList == none
#guard stripSuffix ([] : List Nat) [1, 2, 3] == some [1, 2, 3]
#guard stripSuffix [1, 2, 3] ([] : List Nat) == none
#guard stripSuffix [1, 2, 3] [1, 2, 3] == some []

end Tests.Control.Lens.Internal.List
