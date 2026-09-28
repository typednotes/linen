/-
  Tests for `Data.Stream.Eliminate`.

  Every consumer is `unsafe` (a fused-stream driver, see `Data.Stream.Type`),
  so checks run inside `#eval show IO Unit from do …`.
-/
import Linen.Data.Stream.Eliminate

open Data.Stream Data.Stream.Stream

namespace Tests.Data.Stream.Eliminate

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Eliminate test failed: {name}")

private unsafe def run (act : Id a) : a := Id.run act

#eval show IO Unit from do
  let s : Stream Id Nat := fromList [1, 2, 3, 4]
  check "null false" (run (null s) == false)
  check "null true" (run (null (nil : Stream Id Nat)) == true)
  check "elem" (run (elem 3 s) == true)
  check "elem absent" (run (elem 9 s) == false)
  check "notElem" (run (notElem 9 s) == true)
  check "all" (run (all (· < 10) s) == true)
  check "all false" (run (all (· < 3) s) == false)
  check "any" (run (any (· == 3) s) == true)
  check "last" (run (last s) == some 4)
  check "last nil" (run (last (nil : Stream Id Nat)) == none)
  check "toListRev" (run (toListRev s) == [4, 3, 2, 1])
  check "maximum" (run (maximum s) == some 4)
  check "minimum" (run (minimum s) == some 1)
  check "find" (run (find (· > 2) s) == some 3)
  check "find none" (run (find (· > 9) s) == none)
  check "index" (run (index 2 s) == some 3)
  check "index oob" (run (index 9 s) == none)
  check "foldr1" (run (foldr1 (· + ·) s) == some 10)
  check "foldr1 nil" (run (foldr1 (· + ·) (nil : Stream Id Nat)) == none)
  check "the equal" (run (the (fromList [5, 5, 5] : Stream Id Nat)) == some 5)
  check "the unequal" (run (the s) == none)
  check "lookup" (run (lookup 2 (fromList [(1, 10), (2, 20)] : Stream Id (Nat × Nat))) == some 20)
  -- `mapM_` drives the stream for effect (Id: just forces evaluation)
  let _ := run (mapM_ (fun x => (pure x : Id Nat)) s)
  check "mapM_ ran" true

end Tests.Data.Stream.Eliminate
