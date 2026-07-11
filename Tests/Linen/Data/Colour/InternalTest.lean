/-
  Tests for `Linen.Data.Colour.Internal` — the human perception of colour.
-/
import Linen.Data.Colour.Internal

open Data.Colour

namespace Tests.Data.Colour.Internal

def red : Colour := ⟨⟨1⟩, ⟨0⟩, ⟨0⟩⟩
def blue : Colour := ⟨⟨0⟩, ⟨0⟩, ⟨1⟩⟩

/-! ### Colour -/

#guard Colour.black == (⟨⟨0⟩, ⟨0⟩, ⟨0⟩⟩ : Colour)
#guard Colour.add red blue == (⟨⟨1⟩, ⟨0⟩, ⟨1⟩⟩ : Colour)
#guard Colour.sum [red, blue] == (⟨⟨1⟩, ⟨0⟩, ⟨1⟩⟩ : Colour)
#guard Colour.darken 0.5 red == (⟨⟨0.5⟩, ⟨0⟩, ⟨0⟩⟩ : Colour)
#guard Colour.blend 0.5 red blue == (⟨⟨0.5⟩, ⟨0⟩, ⟨0.5⟩⟩ : Colour)

/-! ### AlphaColour -/

#guard AlphaColour.transparent == (⟨Colour.black, ⟨0⟩⟩ : AlphaColour)
#guard AlphaColour.«opaque» red == (⟨red, ⟨1⟩⟩ : AlphaColour)
#guard AlphaColour.alphaChannel (AlphaColour.«opaque» red) == 1
#guard AlphaColour.colourChannel (AlphaColour.«opaque» red) == red
#guard AlphaColour.withOpacity red 0.5 == (⟨⟨⟨0.5⟩, ⟨0⟩, ⟨0⟩⟩, ⟨0.5⟩⟩ : AlphaColour)
#guard Colour.over (AlphaColour.«opaque» red) blue == red
#guard AlphaColour.over (AlphaColour.«opaque» red) (AlphaColour.«opaque» blue) == AlphaColour.«opaque» red
#guard AlphaColour.over AlphaColour.transparent (AlphaColour.«opaque» blue) == AlphaColour.«opaque» blue

end Tests.Data.Colour.Internal
