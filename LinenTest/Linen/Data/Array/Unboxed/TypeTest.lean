/-
  Tests for `Data.Array.Unboxed.Type` (the immutable unboxed array).
-/
import Linen.Data.Array.Unboxed.Type
import Linen.Data.MutArray.Type
import Linen.Data.Stream.Type
import Linen.Data.Stream.Eliminate

open Data.Array.Unboxed
open Data.Array (Unboxed)
open Data (MutArray)

namespace Tests.Data.Array.Unboxed

-- ── fromList / toList / length ────────────────────────────────────────────────

#guard (Unboxed.fromList [10, 20, 30] : Unboxed UInt8).toList == [10, 20, 30]
#guard (Unboxed.fromList [10, 20, 30] : Unboxed UInt8).length == 3
#guard (Unboxed.fromListN 2 [10, 20, 30] : Unboxed UInt8).toList == [10, 20]
#guard (Unboxed.fromList [1000, 70000] : Unboxed UInt32).toList == [1000, 70000]
#guard (Unboxed.empty : Unboxed UInt8).length == 0

-- ── freeze / thaw ─────────────────────────────────────────────────────────────

#guard (Unboxed.unsafeFreeze (MutArray.fromList [1, 2, 3] : MutArray UInt8)).toList == [1, 2, 3]
#guard (Unboxed.unsafeThaw (Unboxed.fromList [1, 2, 3] : Unboxed UInt8)).toList == [1, 2, 3]

-- ── Indexing / slicing ────────────────────────────────────────────────────────

#guard getIndex 1 (Unboxed.fromList [5, 6, 7] : Unboxed UInt8) == some 6
#guard getIndex 5 (Unboxed.fromList [5, 6, 7] : Unboxed UInt8) == none
#guard (unsafeSliceOffLen 1 2 (Unboxed.fromList [1, 2, 3, 4] : Unboxed UInt8)).toList == [2, 3]

-- ── splice / append ───────────────────────────────────────────────────────────

#guard (splice (Unboxed.fromList [1, 2] : Unboxed UInt8) (Unboxed.fromList [3, 4])).toList
        == [1, 2, 3, 4]
#guard ((Unboxed.fromList [1, 2] : Unboxed UInt8) ++ Unboxed.fromList [3, 4]).toList
        == [1, 2, 3, 4]

-- ── Instances ─────────────────────────────────────────────────────────────────

#guard (Unboxed.fromList [1, 2] : Unboxed UInt8) == (Unboxed.fromList [1, 2] : Unboxed UInt8)
#guard toString (Unboxed.fromList [1, 2] : Unboxed UInt8) == "fromList [1, 2]"

-- ── Streaming / folds ─────────────────────────────────────────────────────────

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Array.Unboxed.Type test failed: {name}")

#eval show IO Unit from do
  let xs ← Data.Stream.Stream.toList (read (Unboxed.fromList [3, 1, 4] : Unboxed UInt8))
  check "read" (xs == [3, 1, 4])
  let src : Data.Stream.Stream IO UInt8 := Data.Stream.Stream.fromList [7, 8, 9]
  let a ← Data.Stream.Stream.fold (createOf 2) src
  check "createOf" (a.toList == [7, 8])
  let b ← Data.Stream.Stream.fold create src
  check "create" (b.toList == [7, 8, 9])

end Tests.Data.Array.Unboxed
