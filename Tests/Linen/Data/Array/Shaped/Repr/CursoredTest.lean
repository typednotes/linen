/-
  Tests for `Linen.Data.Array.Shaped.Repr.Cursored` — the `Cursored` array
  representation and `makeCursored`.

  Uses the flat linear offset (an `Int`) as the cursor, so that shifting the
  cursor by a row/column offset is cheap addition rather than a full
  `toIndex` recomputation — the same motivation stencils use this
  representation for upstream.
-/
import Linen.Data.Array.Shaped.Repr.Cursored
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Repr.Cursored

private def src : Manifest DIM2 Nat :=
  Manifest.fromList (ix2 2 3) [1, 2, 3, 4, 5, 6]

private def carr : Cursored DIM2 Nat :=
  makeCursored Int (Source.extent src)
    (fun ix => Shape.toIndex (Source.extent src) ix)
    (fun sh off => off + Shape.toIndex sh (ix2 0 1))
    (fun off => src.elems.getD off.toNat 0)

#guard index carr (ix2 0 0) == 1
#guard index carr (ix2 1 2) == 6
#guard toList carr == [1, 2, 3, 4, 5, 6]

end Tests.Data.Array.Shaped.Repr.Cursored
