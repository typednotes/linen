/-
  Tests for `Linen.Data.Array.Shaped.Shape` — the `Shape` class, via the
  `DIM2` instance from `Linen.Data.Array.Shaped.Index`.
-/
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Shape

#guard Shape.rank (ix2 3 4) == 2
#guard Shape.size (ix2 3 4) == 12
#guard Shape.zeroDim == ix2 0 0
#guard Shape.unitDim == ix2 1 1
#guard Shape.intersectDim (ix2 3 4) (ix2 5 2) == ix2 3 2
#guard Shape.addDim (ix2 3 4) (ix2 5 2) == ix2 8 6
#guard Shape.toIndex (ix2 3 4) (ix2 1 2) == 6
#guard Shape.fromIndex (ix2 3 4) 6 == ix2 1 2
#guard inShape (ix2 3 4) (ix2 1 2) == true
#guard inShape (ix2 3 4) (ix2 3 0) == false
#guard Shape.listOfShape (ix2 3 4) == [4, 3]
#guard (Shape.shapeOfList [4, 3] : DIM2) == ix2 3 4
#guard showShape (ix2 3 4) == "Z :. 3 :. 4"

end Tests.Data.Array.Shaped.Shape
