/-
  Tests for `Data.Stream.Type`.

  The `Stream` *type* is a safe structure, but its driver/consumption functions
  (`toList`, `fold`, `uncons`, …) and the `StreamK` bridge are `unsafe` (see the
  module header). Following the `Data.StreamK`/`Data.Conduit` convention, every
  check runs inside `#eval show IO Unit from do … unless … throw …`.
-/
import Linen.Data.Stream.Type

open Data.Stream Data.Stream.Stream

namespace Tests.Data.Stream

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Stream test failed: {name}")

/-- Run a pure `Id`-stream to a list. -/
private unsafe def runList (t : Stream Id a) : List a := Id.run (toList t)

#eval show IO Unit from do
  let s : Stream Id Nat := fromList [1, 2, 3]
  -- construction and `toList`
  check "toList/fromList" (runList s == [1, 2, 3])
  check "nil" (runList (nil : Stream Id Nat) == [])
  check "fromPure" (runList (fromPure 7 : Stream Id Nat) == [7])
  check "consM" (runList (consM (pure 0) s) == [0, 1, 2, 3])
  -- folds
  check "foldl'" (Id.run (foldl' (· + ·) 0 s) == 6)
  check "foldr" (Id.run (foldr (· :: ·) [] s) == [1, 2, 3])
  check "head" (Id.run (head s) == some 1)
  check "head nil" (Id.run (head (nil : Stream Id Nat)) == none)
  check "drain" (Id.run (drain s) == ⟨⟩)
  -- `map`
  check "map" (runList (map (· * 10) s) == [10, 20, 30])
  check "Functor.map" (runList ((· + 1) <$> s) == [2, 3, 4])
  -- `take` / `takeWhile`
  check "take" (runList (take 2 s) == [1, 2])
  check "take over" (runList (take 10 s) == [1, 2, 3])
  check "takeWhile" (runList (takeWhile (· < 3) s) == [1, 2])
  -- `append`
  check "append" (runList (append (fromList [1, 2]) (fromList [3, 4]) : Stream Id Nat)
                    == [1, 2, 3, 4])
  -- `zipWith`
  check "zipWith" (runList (zipWith (· + ·) (fromList [1, 2, 3]) (fromList [10, 20]))
                    == [11, 22])
  -- cross product
  check "cross" (runList (cross (fromList [1, 2]) (fromList [10, 20]) : Stream Id (Nat × Nat))
                  == [(1, 10), (1, 20), (2, 10), (2, 20)])
  check "crossWith" (runList (crossWith (· + ·) (fromList [1, 2]) (fromList [10, 20]))
                      == [11, 21, 12, 22])
  -- `unfoldEach` (fused concatMap)
  check "unfoldEach" (runList (unfoldEach Data.Unfold.fromList (fromList [[1, 2], [3]]))
                       == [1, 2, 3])
  -- `unfold` from an `Unfold`
  check "unfold" (runList (unfold Data.Unfold.fromList [4, 5, 6]) == [4, 5, 6])
  -- `concatMap` (via StreamK bridge)
  check "concatMap" (runList (concatMap (fun x => fromList [x, x]) (fromList [1, 2]))
                      == [1, 1, 2, 2])
  -- `fold` with a `Fold`
  check "fold toList" (Id.run (fold Data.Fold.toList s) == [1, 2, 3])
  check "fold take" (Id.run (fold (Data.Fold.take 2 Data.Fold.toList) s) == [1, 2])
  -- `eqBy`
  check "eqBy eq" (Id.run (eqBy (· == ·) (fromList [1, 2]) (fromList [1, 2])) == true)
  check "eqBy neq" (Id.run (eqBy (· == ·) (fromList [1, 2]) (fromList [1, 3])) == false)
  -- `StreamK` round trip
  check "fromStreamK/toStreamK" (runList (fromStreamK (toStreamK s)) == [1, 2, 3])
  -- `Nested` monad do-notation
  let bound : Nested Id Nat := do
    let x ← Nested.mk (fromList [1, 2])
    Nested.mk (fromList [x, x + 10])
  check "Nested bind" (runList bound.unNested == [1, 11, 2, 12])
  let applied : Nested Id Nat :=
    (· + ·) <$> Nested.mk (fromList [1, 2]) <*> Nested.mk (fromList [10, 20])
  check "Nested applicative" (runList applied.unNested == [11, 21, 12, 22])

end Tests.Data.Stream
