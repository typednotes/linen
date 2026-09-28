/-
  Tests for `Linen.Data.Array.Shaped.Slice` — `All`, `Any`, and the `Slice`
  class mapping full-shape indices to slice indices and back.
-/
import Linen.Data.Array.Shaped.Slice

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Slice

-- Fix the row (y), keep the column (x): `Any :. (1 : Int) :. All`.
#guard
  Slice.sliceOfFull (Any.Any (sh := Z) :. (1 : Int) :. All.All) (ix2 1 4) == ix1 4

-- Reconstructing the full index needs the slice value and re-supplies the
-- fixed coordinate from the specification itself.
#guard
  Slice.fullOfSlice (Any.Any (sh := Z) :. (1 : Int) :. All.All) (ix1 4) == ix2 1 4

end Tests.Data.Array.Shaped.Slice
