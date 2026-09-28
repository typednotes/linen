/-
  Tests for `Data.Array.Unboxed` (immutable unboxed-array combinators).
-/
import Linen.Data.Array.Unboxed
import Linen.Data.Stream.Type
import Linen.Data.Stream.Eliminate

open Data.Array.Unboxed
open Data.Array (Unboxed)

namespace Tests.Data.Array.UnboxedCombinators

-- ── Folds / queries over an array ─────────────────────────────────────────────

#guard foldl' (· + ·) 0 (Unboxed.fromList [1, 2, 3, 4] : Unboxed UInt8) == 10
#guard null (Unboxed.empty : Unboxed UInt8) == true
#guard null (Unboxed.fromList [1] : Unboxed UInt8) == false
#guard head (Unboxed.fromList [5, 6, 7] : Unboxed UInt8) == some 5
#guard head (Unboxed.empty : Unboxed UInt8) == none
#guard last (Unboxed.fromList [5, 6, 7] : Unboxed UInt8) == some 7
#guard last (Unboxed.empty : Unboxed UInt8) == none

-- ── getSlice ──────────────────────────────────────────────────────────────────

#guard (getSlice 1 2 (Unboxed.fromList [1, 2, 3, 4, 5] : Unboxed UInt8)).toList == [2, 3]
#guard (getSlice 3 10 (Unboxed.fromList [1, 2, 3, 4, 5] : Unboxed UInt8)).toList == [4, 5]

-- ── Streaming ─────────────────────────────────────────────────────────────────

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Array.Unboxed test failed: {name}")

#eval show IO Unit from do
  -- `readRev` streams elements in reverse.
  let xs ← Data.Stream.Stream.toList (readRev (Unboxed.fromList [1, 2, 3] : Unboxed UInt8))
  check "readRev" (xs == [3, 2, 1])
  -- `fromStreamN`/`fromStream` build immutable arrays from a stream.
  let src : Data.Stream.Stream IO UInt8 := Data.Stream.Stream.fromList [1, 2, 3, 4]
  let a ← fromStreamN 2 src
  check "fromStreamN" (a.toList == [1, 2])
  let b ← fromStream src
  check "fromStream" (b.toList == [1, 2, 3, 4])
  -- `slicerFromLen` cuts an array into fixed-length sub-arrays.
  let arr : Unboxed UInt8 := Unboxed.fromList [1, 2, 3, 4, 5, 6]
  let pieces ← Data.Stream.Stream.toList (Data.Stream.Stream.unfold (slicerFromLen 0 3) arr)
  check "slicer" (pieces.map (·.toList) == [[1, 2, 3], [4, 5, 6]])

end Tests.Data.Array.UnboxedCombinators
