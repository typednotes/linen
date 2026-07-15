import Linen.Data.Either.Strict

open Data.Either Data.Either.Either'

-- `isLeft'`/`isRight'` distinguish the constructors.
#guard isLeft' (Left' 1 : Either' Nat String)
#guard isRight' (Right' "x" : Either' Nat String)
#guard !isRight' (Left' 1 : Either' Nat String)

-- `fromLeft'`/`fromRight'` extract, with `default` on the wrong branch.
#guard fromLeft' (Left' 7 : Either' Nat String) == 7
#guard fromRight' (Right' "ok" : Either' Nat String) == "ok"
#guard fromLeft' (Right' "x" : Either' Nat String) == 0
