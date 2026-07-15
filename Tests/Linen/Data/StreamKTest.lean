/-
  Tests for the `Data.StreamK` facade (module #31).

  Exercises names *through the facade*: importing only `Linen.Data.StreamK` and
  opening only `Data.StreamK` (not the nested `Data.StreamK.StreamK`), so the
  combinators are reached via the facade's `export` lifting. `StreamK` is
  `unsafe`, so checks run inside `#eval show IO Unit` (the `Data.Conduit`
  convention).
-/
import Linen.Data.StreamK

open Data.StreamK

namespace Tests.Data.StreamK.Facade

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"StreamK facade test failed: {name}")

private unsafe def runList (s : StreamK Id a) : List a := Id.run (toList s)

#eval show IO Unit from do
  let s : StreamK Id Nat := cons 1 (cons 2 (fromPure 3))
  check "cons/fromPure/toList" (runList s == [1, 2, 3])
  check "nil" (runList (nil : StreamK Id Nat) == [])
  check "fromList" (runList (fromList [4, 5, 6] : StreamK Id Nat) == [4, 5, 6])
  check "map" (runList (map (· * 10) s) == [10, 20, 30])
  check "foldl'" (Id.run (foldl' (· + ·) 0 s) == 6)

end Tests.Data.StreamK.Facade
