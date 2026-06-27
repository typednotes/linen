/-
  Tests for `Linen.Data.Rat`.

  `Rat.round` (the one helper core's `Rat` lacks). The rest of Haskell's
  `Data.Ratio` is core `Rat`, exercised below to document the mapping.
-/
import Linen.Data.Rat

namespace Tests.Data.Rat

/-! ### round — halves away from zero -/

#guard Rat.round (1 / 2 : Rat) == 1
#guard Rat.round (-1 / 2 : Rat) == -1
#guard Rat.round (1 / 3 : Rat) == 0
#guard Rat.round (2 / 3 : Rat) == 1
#guard Rat.round (3 / 2 : Rat) == 2
#guard Rat.round (-3 / 2 : Rat) == -2
#guard Rat.round (5 : Rat) == 5

/-! ### Data.Ratio is core `Rat` (arithmetic, floor/ceil/abs, canonical form) -/

#guard ((1 / 2 : Rat) + (1 / 3 : Rat)).num == 5    -- 1/2 + 1/3 = 5/6
#guard ((1 / 2 : Rat) + (1 / 3 : Rat)).den == 6
#guard ((2 / 4 : Rat)).num == 1                     -- auto-reduced to 1/2
#guard ((2 / 4 : Rat)).den == 2
#guard Rat.floor (7 / 2 : Rat) == 3
#guard Rat.ceil (7 / 2 : Rat) == 4
#guard (Rat.abs (-3 / 4 : Rat)).num == 3

end Tests.Data.Rat
