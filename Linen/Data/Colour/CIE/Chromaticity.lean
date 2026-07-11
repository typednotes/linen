/-
  Linen.Data.Colour.CIE.Chromaticity — CIE xy chromaticity coordinates

  ## Haskell equivalent
  `Data.Colour.CIE.Chromaticity` from
  https://hackage.haskell.org/package/colour

  ## Design
  As with `Chan` and `Matrix`, upstream is generic over any
  `Fractional`/`Real` representation; every module that uses it in practice
  instantiates it at a floating-point type, so `Chromaticity` is specialized
  to `Float` directly, and `chromaConvert` (upstream's representation
  converter) is dropped as a no-op. Upstream's custom `Show`/`Read`
  instances (round-tripping through `mkChromaticity x y` syntax) are
  likewise dropped in favour of the derived `Repr`.
-/

namespace Data.Colour.CIE

/-- The CIE little-*x*, little-*y* chromaticity coordinates for the 2°
    standard (colourimetric) observer. -/
structure Chromaticity where
  x : Float
  y : Float
  deriving Repr, BEq

namespace Chromaticity

/-- Constructs a `Chromaticity` from its little-*x*, little-*y*
    coordinates. -/
def of (x y : Float) : Chromaticity := ⟨x, y⟩

/-- The little-*x*, little-*y*, little-*z* coordinates, where
    $z = 1 - x - y$. -/
def coords (c : Chromaticity) : Float × Float × Float := (c.x, c.y, 1 - c.x - c.y)

/-- The little-*z* coordinate, where $z = 1 - x - y$. -/
def z (c : Chromaticity) : Float := 1 - c.x - c.y

end Chromaticity
end Data.Colour.CIE
