/-
  Tests for `Linen.Graphics.Image.ColorSpace.CMYK` — the `CMYK`/`CMYKA`
  colour spaces, their `Pixel`/`ColorSpace`/`AlphaSpace` instances, and
  `PixelCMYK`'s component-wise arithmetic instances.

  There is no `RGB ↔ CMYK` conversion to test here: as documented in
  `Linen/Graphics/Image/ColorSpace/CMYK.lean`'s own doc-comment, upstream's
  actual conversion (`ToRGB CMYK`/`ToCMYK RGB`) is not defined in
  `Graphics/Image/ColorSpace/CMYK.hs` at all — it lives in
  `Graphics/Image/ColorSpace.hs` (module #12 in the plan), as one instance
  among a full conversion matrix spanning every colour space. That
  conversion — and its tests — is deferred to module #12's own port and test
  file, once every colour space it converts between exists.

  Fixture/example names are prefixed `csCMYK`/`csCMYKA` to avoid clashing
  with any other test file's identifiers in the shared `Tests` namespace (in
  particular `Tests.Linen.Graphics.Image.ColorSpace.YTest`/`RGBTest`/
  `HSITest`, whose `ColorSpace` operations share the same imported names).
-/
import Linen.Graphics.Image.ColorSpace.CMYK

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace channels toComponents fromComponents
  promote getPxC setPxC mapPxC liftPx liftPx2 foldlPx2 getAlpha addAlpha dropAlpha)
open Graphics.Image.ColorSpace.CMYK

-- ── `CMYK`/`PixelCMYK` — `ColorSpace` operations ──

def csCMYKPx : PixelCMYK Int := ⟨3, 4, 5, 6⟩

#guard (channels (cs := CMYK) (e := Int)) == [CMYK.cyan, CMYK.magenta, CMYK.yellow, CMYK.black]
#guard (toComponents (cs := CMYK) (e := Int) csCMYKPx) == (3, 4, 5, 6)
#guard (fromComponents (cs := CMYK) (e := Int) ((3, 4, 5, 6) : Int × Int × Int × Int)) == csCMYKPx
#guard (promote (cs := CMYK) (7 : Int)) == (⟨7, 7, 7, 7⟩ : PixelCMYK Int)
#guard getPxC (cs := CMYK) (e := Int) csCMYKPx CMYK.cyan == 3
#guard getPxC (cs := CMYK) (e := Int) csCMYKPx CMYK.magenta == 4
#guard getPxC (cs := CMYK) (e := Int) csCMYKPx CMYK.yellow == 5
#guard getPxC (cs := CMYK) (e := Int) csCMYKPx CMYK.black == 6
#guard (setPxC (cs := CMYK) (e := Int) csCMYKPx CMYK.cyan 9) == ⟨9, 4, 5, 6⟩
#guard (setPxC (cs := CMYK) (e := Int) csCMYKPx CMYK.magenta 9) == ⟨3, 9, 5, 6⟩
#guard (setPxC (cs := CMYK) (e := Int) csCMYKPx CMYK.yellow 9) == ⟨3, 4, 9, 6⟩
#guard (setPxC (cs := CMYK) (e := Int) csCMYKPx CMYK.black 9) == ⟨3, 4, 5, 9⟩
#guard (mapPxC (cs := CMYK) (e := Int) (fun _ v => v + 1) csCMYKPx) == ⟨4, 5, 6, 7⟩
#guard (liftPx (cs := CMYK) (e := Int) (· + 1) csCMYKPx) == ⟨4, 5, 6, 7⟩
#guard (liftPx2 (cs := CMYK) (e := Int) (· + ·) csCMYKPx ⟨1, 1, 1, 1⟩) == ⟨4, 5, 6, 7⟩
#guard (foldlPx2 (cs := CMYK) (e := Int) (β := Int) (· + · + ·) 0
  csCMYKPx ⟨1, 1, 1, 1⟩) == 22

-- ── `CMYKA`/`PixelCMYKA` — `ColorSpace` operations ──

def csCMYKAPx : PixelCMYKA Int := ⟨3, 4, 5, 6, 7⟩

#guard (channels (cs := CMYKA) (e := Int)) ==
  [CMYKA.cyan, CMYKA.magenta, CMYKA.yellow, CMYKA.black, CMYKA.alpha]
#guard (toComponents (cs := CMYKA) (e := Int) csCMYKAPx) == (3, 4, 5, 6, 7)
#guard (fromComponents (cs := CMYKA) (e := Int)
  ((3, 4, 5, 6, 7) : Int × Int × Int × Int × Int)) == csCMYKAPx
#guard (promote (cs := CMYKA) (7 : Int)) == (⟨7, 7, 7, 7, 7⟩ : PixelCMYKA Int)
#guard getPxC (cs := CMYKA) (e := Int) csCMYKAPx CMYKA.cyan == 3
#guard getPxC (cs := CMYKA) (e := Int) csCMYKAPx CMYKA.magenta == 4
#guard getPxC (cs := CMYKA) (e := Int) csCMYKAPx CMYKA.yellow == 5
#guard getPxC (cs := CMYKA) (e := Int) csCMYKAPx CMYKA.black == 6
#guard getPxC (cs := CMYKA) (e := Int) csCMYKAPx CMYKA.alpha == 7
#guard (setPxC (cs := CMYKA) (e := Int) csCMYKAPx CMYKA.cyan 9) == ⟨9, 4, 5, 6, 7⟩
#guard (setPxC (cs := CMYKA) (e := Int) csCMYKAPx CMYKA.alpha 9) == ⟨3, 4, 5, 6, 9⟩
#guard (mapPxC (cs := CMYKA) (e := Int) (fun _ v => v + 1) csCMYKAPx) == ⟨4, 5, 6, 7, 8⟩
#guard (liftPx (cs := CMYKA) (e := Int) (· + 1) csCMYKAPx) == ⟨4, 5, 6, 7, 8⟩
#guard (liftPx2 (cs := CMYKA) (e := Int) (· + ·) csCMYKAPx ⟨1, 1, 1, 1, 1⟩) == ⟨4, 5, 6, 7, 8⟩
#guard (foldlPx2 (cs := CMYKA) (e := Int) (β := Int) (· + · + ·) 0
  csCMYKAPx ⟨1, 1, 1, 1, 1⟩) == 30

-- ── `AlphaSpace` between `CMYKA` and `CMYK` ──

#guard getAlpha (cs := CMYKA) (e := Int) csCMYKAPx == 7
#guard addAlpha (cs := CMYKA) (e := Int) (7 : Int) csCMYKPx == csCMYKAPx
#guard dropAlpha (cs := CMYKA) (e := Int) csCMYKAPx == csCMYKPx

-- ── Component-wise arithmetic on `PixelCMYK` ──

#guard csCMYKPx + (⟨1, 1, 1, 1⟩ : PixelCMYK Int) == ⟨4, 5, 6, 7⟩
#guard csCMYKPx - (⟨1, 1, 1, 1⟩ : PixelCMYK Int) == ⟨2, 3, 4, 5⟩
#guard csCMYKPx * (⟨2, 2, 2, 2⟩ : PixelCMYK Int) == ⟨6, 8, 10, 12⟩
#guard (⟨12, 12, 12, 12⟩ : PixelCMYK Int) / (⟨4, 3, 2, 1⟩ : PixelCMYK Int) == ⟨3, 4, 6, 12⟩
#guard -csCMYKPx == ⟨-3, -4, -5, -6⟩
#guard (0 : PixelCMYK Int) == ⟨0, 0, 0, 0⟩
#guard (1 : PixelCMYK Int) == ⟨1, 1, 1, 1⟩
