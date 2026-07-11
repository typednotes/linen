/-
  Linen.Data.Array.Shaped.Stencil.Partition — partitioning a 2D region for
  stencil application

  Ported from Haskell's `Data.Array.Repa.Stencil.Partition` (package
  `repa`). Pure 2D geometry with no `Source`/GHC dependency; ported
  faithfully.
-/

namespace Data.Array.Shaped

/-- An offset in the 2D plane. -/
structure Offset where
  x : Int
  y : Int

/-- Size of a region in the 2D plane. -/
structure Size where
  w : Int
  h : Int

/-- A region in the 2D plane. -/
structure Region where
  x : Int
  y : Int
  width : Int
  height : Int
  deriving BEq, Repr, Inhabited

/-- Create a new region of the given size, positioned at the origin. -/
def regionOfSize (s : Size) : Region :=
  ⟨0, 0, s.w, s.h⟩

/-- Offset a region. -/
def offsetRegion (o : Offset) (r : Region) : Region :=
  ⟨r.x + o.x, r.y + o.y, r.width, r.height⟩

/-- Partition a region into inner and border regions for the given stencil.
    Returns `[inner, north, south, west, east]`. -/
def partitionForStencil (arrSize krnSize : Size) (focus : Offset) : List Region :=
  let gapNorth := focus.y
  let gapSouth := krnSize.h - focus.y - 1
  let gapWest := focus.x
  let gapEast := krnSize.w - focus.x - 1
  let innerW := arrSize.w - gapWest - gapEast
  let innerH := arrSize.h - gapNorth - gapSouth
  let regionInner := offsetRegion ⟨gapWest, gapNorth⟩ (regionOfSize ⟨innerW, innerH⟩)
  let regionNorth := regionOfSize ⟨arrSize.w, gapNorth⟩
  let regionSouth := offsetRegion ⟨0, gapNorth + innerH⟩ (regionOfSize ⟨arrSize.w, gapSouth⟩)
  let regionWest := offsetRegion ⟨0, gapNorth⟩ (regionOfSize ⟨gapWest, innerH⟩)
  let regionEast := offsetRegion ⟨gapWest + innerW, gapNorth⟩ (regionOfSize ⟨gapEast, innerH⟩)
  [regionInner, regionNorth, regionSouth, regionWest, regionEast]

end Data.Array.Shaped
