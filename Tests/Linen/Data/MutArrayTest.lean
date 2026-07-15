/-
  Tests for `Data.MutArray` (mutable-array combinators).
-/
import Linen.Data.MutArray
import Linen.Data.Stream.Type
import Linen.Data.Stream.Eliminate

open Data.MutArray
open Data (MutArray)

namespace Tests.Data.MutArrayCombinators

-- ── In-place transforms ───────────────────────────────────────────────────────

#guard (reverse (MutArray.fromList [1, 2, 3, 4] : MutArray UInt8)).toList == [4, 3, 2, 1]
#guard (reverse (MutArray.fromList [1, 2, 3] : MutArray UInt8)).toList == [3, 2, 1]
#guard (modify (· * 2) (MutArray.fromList [1, 2, 3] : MutArray UInt8)).toList == [2, 4, 6]
#guard (swapIndices 0 2 (MutArray.fromList [1, 2, 3] : MutArray UInt8)).toList == [3, 2, 1]
#guard (swapIndices 0 9 (MutArray.fromList [1, 2, 3] : MutArray UInt8)).toList == [1, 2, 3]

-- ── Comparison ────────────────────────────────────────────────────────────────

#guard eq (MutArray.fromList [1, 2] : MutArray UInt8) (MutArray.fromList [1, 2]) == true
#guard eq (MutArray.fromList [1, 2] : MutArray UInt8) (MutArray.fromList [2, 1]) == false

-- ── Slice indexers ────────────────────────────────────────────────────────────

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"MutArray combinator test failed: {name}")

#eval show IO Unit from do
  -- `fromStreamN`/`fromStream` build arrays from a stream.
  let src : Data.Stream.Stream IO UInt8 := Data.Stream.Stream.fromList [1, 2, 3, 4, 5]
  let a ← fromStreamN 3 src
  check "fromStreamN" (a.toList == [1, 2, 3])
  let b ← fromStream src
  check "fromStream" (b.toList == [1, 2, 3, 4, 5])
  -- `splitterFromLen` cuts the array into fixed-length sub-arrays.
  let arr : MutArray UInt8 := MutArray.fromList [1, 2, 3, 4, 5, 6]
  let u := splitterFromLen 0 2
  let pieces ← Data.Stream.Stream.toList (Data.Stream.Stream.unfold u arr)
  check "splitter count" (pieces.length == 3)
  check "splitter chunks" (pieces.map (·.toList) == [[1, 2], [3, 4], [5, 6]])
  -- `indexerFromLen` gives the byte offsets/lengths.
  let idxs ← Data.Stream.Stream.toList (Data.Stream.Stream.unfold (indexerFromLen 0 2) arr)
  check "indexer" (idxs == [(0, 2), (2, 2), (4, 2)])

end Tests.Data.MutArrayCombinators
