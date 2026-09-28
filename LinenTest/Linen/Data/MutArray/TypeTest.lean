/-
  Tests for `Data.MutArray.Type` (the growable unboxed mutable array).

  Element type is `UInt8`/`UInt32` (both `Unbox`). Low-level ops are pure, so
  most checks are `#guard`; the `Fold`/`Stream` drivers run inside
  `#eval show IO Unit` (they use the `unsafe` stream driver).
-/
import Linen.Data.MutArray.Type
import Linen.Data.Stream.Type
import Linen.Data.Stream.Eliminate

open Data.MutArray
open Data (MutArray)

namespace Tests.Data.MutArray

-- ── Power-of-two policy ───────────────────────────────────────────────────────

#guard roundUpToPower2 0 == 1
#guard roundUpToPower2 1 == 1
#guard roundUpToPower2 4 == 4
#guard roundUpToPower2 5 == 8
#guard roundUpToPower2 7 == 8
#guard isPower2 8 == true
#guard isPower2 6 == false
#guard isPower2 0 == false

-- ── Construction / capacity ───────────────────────────────────────────────────

-- `emptyOf 4 : MutArray UInt8` reserves 4 bytes, empty.
#guard (emptyOf 4 : MutArray UInt8).length == 0
#guard (emptyOf 4 : MutArray UInt8).capacity == 4
#guard (emptyOf 3 : MutArray UInt32).byteCapacity == 12

-- ── snoc / grow ───────────────────────────────────────────────────────────────

-- Appending within capacity keeps the buffer; length grows.
#guard (unsafeSnoc (emptyOf 2 : MutArray UInt8) 7).length == 1
#guard (unsafeSnoc (unsafeSnoc (emptyOf 2 : MutArray UInt8) 7) 9).toList == [7, 9]

-- `snoc` grows an empty (zero-capacity) array automatically.
#guard (snoc (snoc (snoc empty (1 : UInt8)) 2) 3).toList == [1, 2, 3]
#guard ((List.range 10).foldl (fun a i => snoc a i.toUInt8) empty).toList
        == [0,1,2,3,4,5,6,7,8,9]

-- `snocMay` refuses when the buffer is full.
#guard (snocMay (emptyOf 0 : MutArray UInt8) 5).isNone
#guard (snocMay (emptyOf 1 : MutArray UInt8) 5).isSome

-- ── fromList / toList round trip ──────────────────────────────────────────────

#guard (MutArray.fromList [10, 20, 30] : MutArray UInt8).toList == [10, 20, 30]
#guard (MutArray.fromListN 2 [10, 20, 30] : MutArray UInt8).toList == [10, 20]
#guard (MutArray.fromList [1000, 2000, 70000] : MutArray UInt32).toList == [1000, 2000, 70000]

-- ── Indexing ──────────────────────────────────────────────────────────────────

#guard getIndex 1 (MutArray.fromList [5, 6, 7] : MutArray UInt8) == some 6
#guard getIndex 9 (MutArray.fromList [5, 6, 7] : MutArray UInt8) == none
#guard (putIndex 0 (MutArray.fromList [5, 6, 7] : MutArray UInt8) 99).toList == [99, 6, 7]
#guard (putIndex 9 (MutArray.fromList [5, 6, 7] : MutArray UInt8) 99).toList == [5, 6, 7]
#guard (modifyIndex 2 (· + 1) (MutArray.fromList [5, 6, 7] : MutArray UInt8)).toList
        == [5, 6, 8]

-- ── Slicing ─────────────────────────────────────────────────────────────────

#guard (sliceOffLen 1 2 (MutArray.fromList [1, 2, 3, 4, 5] : MutArray UInt8)).toList == [2, 3]
#guard (sliceOffLen 3 10 (MutArray.fromList [1, 2, 3, 4, 5] : MutArray UInt8)).toList == [4, 5]

-- ── BEq ───────────────────────────────────────────────────────────────────────

#guard (MutArray.fromList [1, 2] : MutArray UInt8) == (MutArray.fromList [1, 2] : MutArray UInt8)
#guard ((MutArray.fromList [1, 2] : MutArray UInt8) == (MutArray.fromList [1, 3])) == false

-- ── Streaming / folds (unsafe drivers) ────────────────────────────────────────

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"MutArray test failed: {name}")

#eval show IO Unit from do
  -- `read` streams the elements; `Stream.toList` drives it.
  let xs ← Data.Stream.Stream.toList (read (MutArray.fromList [3, 1, 4] : MutArray UInt8))
  check "read" (xs == [3, 1, 4])
  -- `createOf`/`create` build arrays by folding a stream.
  let src : Data.Stream.Stream IO UInt8 := Data.Stream.Stream.fromList [7, 8, 9]
  let a ← Data.Stream.Stream.fold (createOf 2) src
  check "createOf caps" (a.toList == [7, 8])
  let b ← Data.Stream.Stream.fold create src
  check "create all" (b.toList == [7, 8, 9])

end Tests.Data.MutArray
