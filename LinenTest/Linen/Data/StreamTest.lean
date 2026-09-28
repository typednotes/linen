/-
  Tests for the `Data.Stream` facade (module #32).

  Exercises names *through the facade*: importing only `Linen.Data.Stream` and
  opening only `Data.Stream` (not the nested `Data.Stream.Stream`), so the
  combinators — drawn from the Type/Generate/Eliminate/Transform/Nesting
  operation modules — are reached via the facade's `export` lifting. The stream
  drivers are `unsafe`, so checks run inside `#eval show IO Unit`.
-/
import Linen.Data.Stream

open Data.Stream

namespace Tests.Data.Stream.Facade

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Stream facade test failed: {name}")

private unsafe def runList (t : Stream Id a) : List a := Id.run (toList t)

#eval show IO Unit from do
  let s : Stream Id Nat := fromList [1, 2, 3]
  -- `fromList` (#19 Type) + `toList` (#21 Eliminate)
  check "fromList/toList" (runList s == [1, 2, 3])
  -- `replicate` (#20 Generate)
  check "replicate" (runList (replicate 3 7 : Stream Id Nat) == [7, 7, 7])
  -- `filter` (#22 Transform)
  check "filter" (runList (filter (· % 2 == 1) (fromList [1, 2, 3, 4] : Stream Id Nat)) == [1, 3])
  -- `foldl'` (#19) and `all` (#21 Eliminate)
  check "foldl'" (Id.run (foldl' (· + ·) 0 s) == 6)
  check "all" (Id.run (all (· > 0) s) == true)
  -- `interleave` (#24 Nesting)
  check "interleave"
    (runList (interleave (fromList [1, 3]) (fromList [2, 4]) : Stream Id Nat) == [1, 2, 3, 4])

end Tests.Data.Stream.Facade
