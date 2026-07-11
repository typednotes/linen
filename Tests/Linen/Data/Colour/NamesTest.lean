/-
  Tests for `Linen.Data.Colour.Names` — names for colours.
-/
import Linen.Data.Colour.Names

open Data.Colour
open Data.Colour.Names

namespace Tests.Data.Colour.Names

#guard red == SRGB.sRGB24 255 0 0
#guard green == SRGB.sRGB24 0 128 0
#guard blue == SRGB.sRGB24 0 0 255
#guard black == Colour.black
#guard white == SRGB.sRGB24 255 255 255

#guard readColourName "red" == some red
#guard readColourName "black" == some Colour.black
#guard readColourName "not-a-colour" == none

end Tests.Data.Colour.Names
