/-
  Tests for `Linen.Data.Array.Shaped.Operators.Mapping` — `map`, `zipWith`,
  `+^`/`-^`/`*^`/`/^`, and the `Structured` class (`smap`/`szipWith`).
-/
import Linen.Data.Array.Shaped.Operators.Mapping
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Operators.Mapping

private def a1 : Manifest DIM1 Int := Manifest.fromList (ix1 3) [1, 2, 3]
private def a2 : Manifest DIM1 Int := Manifest.fromList (ix1 3) [10, 20, 30]

#guard toList (map (· * 2) a1) == [2, 4, 6]
#guard toList (zipWith (· + ·) a1 a2) == [11, 22, 33]

#guard toList (a1 +^ a2) == [11, 22, 33]
#guard toList (a2 -^ a1) == [9, 18, 27]
#guard toList (a1 *^ a2) == [10, 40, 90]
#guard toList (a2 /^ a1) == [10, 10, 10]

-- `smap`/`szipWith` on a `Cursored` array preserve the `Cursored` representation.
private def carr : Cursored DIM1 Int :=
  makeCursored Int (Source.extent a1)
    (fun ix => Shape.toIndex (Source.extent a1) ix)
    (fun sh off => off + Shape.toIndex sh (ix1 1))
    (fun off => a1.elems.getD off.toNat 0)

#guard toList (Structured.smap (arr1 := Cursored) (· * 10) carr) == [10, 20, 30]
#guard toList (Structured.szipWith (arr1 := Cursored) (· + ·) a2 carr) == [11, 22, 33]

-- `smap` on a `Partitioned` array applies to each sub-array separately.
private def range : Range DIM1 :=
  ⟨ix1 0, ix1 3, fun ix => match ix with | Z.Z :. x => x < 1⟩

private def parr : Partitioned Manifest Manifest DIM1 Int := ⟨ix1 3, range, a1, a2⟩

#guard toList (Structured.smap (arr1 := Partitioned Manifest Manifest) (· * 2) parr) ==
  [2, 40, 60]

end Tests.Data.Array.Shaped.Operators.Mapping
