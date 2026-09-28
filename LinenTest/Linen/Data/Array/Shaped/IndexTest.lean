/-
  Tests for `Linen.Data.Array.Shaped.Index` — `Z`, `:.`, `DIM0`-`DIM5`,
  `ix1`-`ix5`.
-/
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Index

#guard (Z.Z : DIM0) == Z.Z
#guard ix1 5 == Z.Z :. 5
#guard ix2 3 4 == Z.Z :. 3 :. 4
#guard ix3 2 3 4 == Z.Z :. 2 :. 3 :. 4
#guard ix4 1 2 3 4 == Z.Z :. 1 :. 2 :. 3 :. 4
#guard ix5 0 1 2 3 4 == Z.Z :. 0 :. 1 :. 2 :. 3 :. 4

-- Round trip through the flat linear index for a DIM2 shape.
#guard Shape.fromIndex (ix2 3 4) (Shape.toIndex (ix2 3 4) (ix2 2 1)) == ix2 2 1

-- `size` matches the product of the dimensions.
#guard Shape.size (ix3 2 3 4) == 24

end Tests.Data.Array.Shaped.Index
