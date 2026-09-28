/-
  Tests for `Data.Stream.Generate`.

  Generators are total, but the `toList` driver used to observe them is
  `unsafe` (see `Data.Stream.Type`), so every check runs inside
  `#eval show IO Unit from do …`, as in the `Stream.Type` tests.
-/
import Linen.Data.Stream.Generate

open Data.Stream Data.Stream.Stream

namespace Tests.Data.Stream.Generate

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Generate test failed: {name}")

private unsafe def runList (t : Stream Id a) : List a := Id.run (toList t)

#eval show IO Unit from do
  check "cons" (runList (cons 0 (fromList [1, 2, 3]) : Stream Id Nat) == [0, 1, 2, 3])
  check "unfoldr" (runList (unfoldr (fun b => if b > 2 then none else some (b, b + 1)) 0
                    : Stream Id Nat) == [0, 1, 2])
  check "replicate" (runList (replicate 3 (7 : Nat)) == [7, 7, 7])
  check "replicateM 0" (runList (replicate 0 (7 : Nat)) == [])
  check "repeatValue+take" (runList (take 4 (repeatValue (5 : Nat))) == [5, 5, 5, 5])
  check "iterateValue+take" (runList (take 5 (iterateValue (· + 1) (1 : Nat))) == [1, 2, 3, 4, 5])
  check "generate" (runList (generate 4 (fun i => i * i) : Stream Id Nat) == [0, 1, 4, 9])
  check "fromIndices+take" (runList (take 3 (fromIndices (· + 10) : Stream Id Nat))
                            == [10, 11, 12])
  check "fromListM" (runList (fromListM [pure 1, pure 2, pure 3] : Stream Id Nat) == [1, 2, 3])
  check "fromFoldable" (runList (fromFoldable [1, 2, 3] : Stream Id Nat) == [1, 2, 3])
  check "enumerateFromToIntegral" (runList (enumerateFromToIntegral 0 4 : Stream Id Nat)
                                    == [0, 1, 2, 3, 4])
  check "enumerateFromStepIntegral+take"
    (runList (take 4 (enumerateFromStepIntegral 0 2 : Stream Id Nat)) == [0, 2, 4, 6])

end Tests.Data.Stream.Generate
