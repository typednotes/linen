import Linen.Data.Tuple.Strict

open Data.Tuple

-- `Tuple'` stores and projects two strict fields.
#guard (Tuple'.mk 1 "a").fst == 1
#guard (Tuple'.mk 1 "a").snd == "a"

-- `Tuple3'` projects its three fields.
#guard (Tuple3'.mk 1 2 3).thd == 3

-- `Tuple4'` projects its four fields.
#guard (Tuple4'.mk 1 2 3 4).fth == 4

-- Strict tuples compare structurally via the derived `BEq`.
#guard (Tuple'.mk 1 2 == Tuple'.mk 1 2)
#guard !(Tuple'.mk 1 2 == Tuple'.mk 1 3)
