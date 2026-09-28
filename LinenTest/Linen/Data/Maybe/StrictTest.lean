import Linen.Data.Maybe.Strict

open Data.Maybe Data.Maybe.Maybe'

-- `toMaybe` converts to the lazy `Option`.
#guard toMaybe (Just' 5) == some 5
#guard toMaybe (a := Nat) Nothing' == none

-- `isJust'` distinguishes the constructors.
#guard isJust' (Just' 5)
#guard !isJust' (a := Nat) Nothing'

-- `fromJust'` extracts a `Just'` and yields `default` on `Nothing'`.
#guard fromJust' (Just' 5) == 5
#guard fromJust' (a := Nat) Nothing' == 0
