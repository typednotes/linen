/-
  Tests for `Linen.Data.Array.Shaped.Stencil.Partition` — `regionOfSize`,
  `offsetRegion`, and `partitionForStencil`.
-/
import Linen.Data.Array.Shaped.Stencil.Partition

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Stencil.Partition

#guard regionOfSize ⟨4, 3⟩ == Region.mk 0 0 4 3
#guard offsetRegion ⟨1, 2⟩ (regionOfSize ⟨4, 3⟩) == Region.mk 1 2 4 3

-- A 5x5 array with a centered 3x3 stencil (focus at its middle, offset (1, 1)).
private def regions := partitionForStencil ⟨5, 5⟩ ⟨3, 3⟩ ⟨1, 1⟩

#guard regions.length == 5
#guard regions[0]! == Region.mk 1 1 3 3   -- inner
#guard regions[1]! == Region.mk 0 0 5 1   -- north
#guard regions[2]! == Region.mk 0 4 5 1   -- south
#guard regions[3]! == Region.mk 0 1 1 3   -- west
#guard regions[4]! == Region.mk 4 1 1 3   -- east

end Tests.Data.Array.Shaped.Stencil.Partition
