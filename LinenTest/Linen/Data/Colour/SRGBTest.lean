/-
  Tests for `Linen.Data.Colour.SRGB` — sRGB colours.
-/
import Linen.Data.Colour.SRGB

open Data.Colour

namespace Tests.Data.Colour.SRGB

def approxColour (u v : Colour) : Bool :=
  let eps := 1e-9
  let a := SRGB.Linear.toRGB u
  let b := SRGB.Linear.toRGB v
  (a.r - b.r).abs < eps && (a.g - b.g).abs < eps && (a.b - b.b).abs < eps

#guard SRGB.sRGB24 0 0 0 == SRGB.sRGB 0 0 0
#guard SRGB.sRGB24 255 255 255 == SRGB.sRGB 1 1 1
#guard approxColour (SRGB.sRGB 0.5 0.5 0.5) (SRGB.Linear.rgb (SRGB.invTransferFunction 0.5) (SRGB.invTransferFunction 0.5) (SRGB.invTransferFunction 0.5))

#guard SRGB.toSRGB24 (SRGB.sRGB24 0 128 255) == (⟨0, 128, 255⟩ : RGB UInt8)
#guard SRGB.sRGB24show (SRGB.sRGB24 0 170 255) == "#00aaff"
#guard SRGB.sRGB24read? "#00aaff" == some (SRGB.sRGB24 0 170 255)
#guard SRGB.sRGB24read? "00aaff" == some (SRGB.sRGB24 0 170 255)
#guard SRGB.sRGB24read? "bogus" == none

#guard SRGB.sRGBSpace.gamut == SRGB.Linear.sRGBGamut
#guard SRGB.sRGBSpace.transferFunction.transfer 1 == 1

end Tests.Data.Colour.SRGB
