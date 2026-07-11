/-
  Tests for `Linen.Data.Colour` — the human perception of colour.
-/
import Linen.Data.Colour

open Data.Colour

namespace Tests.Data.Colour

#guard Colour.black == (⟨⟨0⟩, ⟨0⟩, ⟨0⟩⟩ : Colour)
#guard AlphaColour.alphaChannel (AlphaColour.«opaque» Colour.black) == 1
#guard AlphaColour.alphaChannel AlphaColour.transparent == 0
#guard AlphaColour.colourChannel (AlphaColour.withOpacity Colour.black 0.5) == Colour.black
#guard AlphaColour.alphaChannel (AlphaColour.withOpacity Colour.black 0.5) == 0.5
#guard AlphaColour.dissolve 0.5 (AlphaColour.«opaque» Colour.black)
     == AlphaColour.withOpacity Colour.black 0.5
#guard AlphaColour.atop AlphaColour.transparent (AlphaColour.«opaque» (SRGB.Linear.rgb 1 0 0))
     == AlphaColour.«opaque» (SRGB.Linear.rgb 1 0 0)

end Tests.Data.Colour
