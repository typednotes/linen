/-
  Tests for `Data.StreamK.Type`.

  `StreamK` is `unsafe` (see the module header), so its values can't appear in a
  kernel-checked `#guard`. Following the `Data.Conduit` convention, every check
  runs inside `#eval show IO Unit from do … unless … throw …`.
-/
import Linen.Data.StreamK.Type

open Data.StreamK Data.StreamK.StreamK

namespace Tests.Data.StreamK

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"StreamK test failed: {name}")

/-- Run a pure `Id`-stream to a list. -/
private unsafe def runList (s : StreamK Id a) : List a := Id.run (toList s)

#eval show IO Unit from do
  let s : StreamK Id Nat := cons 1 (cons 2 (fromPure 3))
  -- construction and `toList`
  check "toList/cons/fromPure" (runList s == [1, 2, 3])
  -- `nil` is empty; `null`
  check "nil" (runList (nil : StreamK Id Nat) == [])
  check "null nil" (Id.run (null (nil : StreamK Id Nat)) == true)
  check "null cons" (Id.run (null s) == false)
  -- `fromList` round-trips
  check "fromList" (runList (fromList [4, 5, 6] : StreamK Id Nat) == [4, 5, 6])
  -- `uncons`
  match Id.run (uncons s) with
  | some (h, t) => check "uncons head" (h == 1) ; check "uncons tail" (runList t == [2, 3])
  | none => check "uncons nonempty" false
  -- folds
  check "foldl'" (Id.run (foldl' (· + ·) 0 s) == 6)
  check "foldr" (Id.run (foldr (· :: ·) [] s) == [1, 2, 3])
  -- `map`
  check "map" (runList (map (· * 10) s) == [10, 20, 30])
  -- `append`
  check "append" (runList (append (fromList [1, 2]) (fromList [3, 4]) : StreamK Id Nat) == [1, 2, 3, 4])
  -- `interleave`
  check "interleave" (runList (interleave (fromList [1, 3, 5]) (fromList [2, 4]) : StreamK Id Nat) == [1, 2, 3, 4, 5])
  -- `reverse`
  check "reverse" (runList (reverse (fromList [1, 2, 3]) : StreamK Id Nat) == [3, 2, 1])
  -- `concatMap` / bind
  check "concatMap" (runList (concatMap (fun x => fromList [x, x]) (fromList [1, 2]) : StreamK Id Nat) == [1, 1, 2, 2])
  -- cross product
  check "cross" (runList (cross (fromList [1, 2]) (fromList [10, 20]) : StreamK Id (Nat × Nat))
                  == [(1, 10), (1, 20), (2, 10), (2, 20)])
  -- Monad do-notation (>>= = concatMap)
  let bound : StreamK Id Nat := do let x ← fromList [1, 2]; fromList [x, x + 10]
  check "bind" (runList bound == [1, 11, 2, 12])
  -- `unfoldr` (finite)
  check "unfoldr" (runList (unfoldr (fun n => if n > 3 then none else some (n, n + 1)) 1 : StreamK Id Nat) == [1, 2, 3])
  -- Alternative `<|>` = append
  check "orElse" (runList ((fromList [1] <|> fromList [2]) : StreamK Id Nat) == [1, 2])

end Tests.Data.StreamK
