/-
  Tests for `Data.Stream.Transform`.

  Transforms are total (except `reverse`), but the `toList` driver used to
  observe them is `unsafe`, so checks run inside `#eval show IO Unit from do …`.
-/
import Linen.Data.Stream.Transform

open Data.Stream Data.Stream.Stream

namespace Tests.Data.Stream.Transform

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Transform test failed: {name}")

private unsafe def runList (t : Stream Id a) : List a := Id.run (toList t)

#eval show IO Unit from do
  let s : Stream Id Nat := fromList [1, 2, 3, 4]
  check "sequence" (runList (sequence (fromList [pure 1, pure 2] : Stream Id (Id Nat))) == [1, 2])
  check "filter" (runList (filter (fun x => x % 2 == 0) s) == [2, 4])
  check "mapMaybe" (runList (mapMaybe (fun x => if x > 2 then some (x * 10) else none) s)
                    == [30, 40])
  check "catMaybes" (runList (catMaybes (fromList [some 1, none, some 3] : Stream Id (Option Nat)))
                     == [1, 3])
  check "drop" (runList (drop 2 s) == [3, 4])
  check "dropWhile" (runList (dropWhile (· < 3) s) == [3, 4])
  check "intersperse" (runList (intersperse 0 s) == [1, 0, 2, 0, 3, 0, 4])
  check "intersperse single" (runList (intersperse 0 (fromList [1] : Stream Id Nat)) == [1])
  check "indexed" (runList (indexed (fromList [10, 20] : Stream Id Nat)) == [(0, 10), (1, 20)])
  check "findIndices" (runList (findIndices (fun x => x % 2 == 0) s) == [1, 3])
  check "elemIndices" (runList (elemIndices 3 s) == [2])
  check "catLefts" (runList (catLefts (fromList [.inl 1, .inr "x", .inl 2]
                    : Stream Id (Nat ⊕ String))) == [1, 2])
  check "catRights" (runList (catRights (fromList [.inl 1, .inr "x", .inl 2]
                    : Stream Id (Nat ⊕ String))) == ["x"])
  check "uniq" (runList (uniq (fromList [1, 1, 2, 2, 2, 3, 1] : Stream Id Nat)) == [1, 2, 3, 1])
  check "rollingMap" (runList (rollingMap (fun p x => match p with
                        | some q => x - q | none => x) (fromList [1, 3, 6] : Stream Id Nat))
                      == [1, 2, 3])
  -- scanning
  check "scanl'" (runList (scanl' (· + ·) 0 s) == [0, 1, 3, 6, 10])
  check "scan" (runList (scan Data.Fold.toList (fromList [1, 2, 3] : Stream Id Nat))
                == [[], [1], [1, 2], [1, 2, 3]])
  check "postscan" (runList (postscan Data.Fold.toList (fromList [1, 2, 3] : Stream Id Nat))
                    == [[1], [1, 2], [1, 2, 3]])
  check "reverse" (runList (reverse s) == [4, 3, 2, 1])

end Tests.Data.Stream.Transform
