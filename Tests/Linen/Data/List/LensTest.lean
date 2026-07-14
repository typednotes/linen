/-
  Tests for `Linen.Data.List.Lens`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Setter
import Linen.Data.List.Lens

open Control.Lens

namespace Tests.Linen.Data.List.Lens

#guard preview Data.List.Lens._head ([1, 2, 3] : List Nat) = some 1
#guard preview Data.List.Lens._tail ([1, 2, 3] : List Nat) = some [2, 3]
#guard preview Data.List.Lens._init ([1, 2, 3] : List Nat) = some [1, 2]
#guard preview Data.List.Lens._last ([1, 2, 3] : List Nat) = some 3

#guard preview Data.List.Lens._head ([] : List Nat) = none
#guard preview Data.List.Lens._last ([] : List Nat) = none

end Tests.Linen.Data.List.Lens
