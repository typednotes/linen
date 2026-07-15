import Linen.Data.Stream.BaseCompat

open Data.Stream.BaseCompat

-- `coerceComp` composes two functions.
#guard coerceComp (· + 1) (· * 2) 3 == 7

-- The `#.` notation matches `coerceComp`.
#guard ((· + 1) #. (· * 2)) 10 == 21

-- Composing with `id` on the right is the identity of the second function.
#guard (Nat.succ #. id) 4 == 5
