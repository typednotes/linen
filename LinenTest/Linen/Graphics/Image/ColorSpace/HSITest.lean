/-
  Tests for `Linen.Graphics.Image.ColorSpace.HSI` — the `HSI`/`HSIA` colour
  spaces, their `Pixel`/`ColorSpace`/`AlphaSpace` instances, and `PixelHSI`'s
  component-wise arithmetic instances.

  There is no `RGB ↔ HSI` conversion to test here: as documented in
  `Linen/Graphics/Image/ColorSpace/HSI.lean`'s own doc-comment, upstream's
  actual conversion (`ToRGB HSI`/`ToHSI RGB`, involving `atan2`/`cos`
  trigonometry) is not defined in `Graphics/Image/ColorSpace/HSI.hs` at all —
  it lives in `Graphics/Image/ColorSpace.hs` (module #12 in the plan), as one
  instance among a full conversion matrix spanning every colour space. That
  conversion — and its tests, ideally against known fixed points such as pure
  red/green/blue/white/black/grey — is deferred to module #12's own port and
  test file, once every colour space it converts between exists.

  Fixture/example names are prefixed `csHSI` to avoid clashing with any other
  test file's identifiers in the shared `Tests` namespace (in particular
  `Tests.Linen.Graphics.Image.ColorSpace.YTest`/`RGBTest`, whose `ColorSpace`
  operations share the same imported names).
-/
import Linen.Graphics.Image.ColorSpace.HSI

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace channels toComponents fromComponents
  promote getPxC setPxC mapPxC liftPx liftPx2 foldlPx2 getAlpha addAlpha dropAlpha)
open Graphics.Image.ColorSpace.HSI

-- ── `HSI`/`PixelHSI` — `ColorSpace` operations ──

def csHSIPx : PixelHSI Int := ⟨3, 4, 5⟩

#guard (channels (cs := HSI) (e := Int)) == [HSI.hue, HSI.sat, HSI.int]
#guard (toComponents (cs := HSI) (e := Int) csHSIPx) == (3, 4, 5)
#guard (fromComponents (cs := HSI) (e := Int) ((3, 4, 5) : Int × Int × Int)) == csHSIPx
#guard (promote (cs := HSI) (7 : Int)) == (⟨7, 7, 7⟩ : PixelHSI Int)
#guard getPxC (cs := HSI) (e := Int) csHSIPx HSI.hue == 3
#guard getPxC (cs := HSI) (e := Int) csHSIPx HSI.sat == 4
#guard getPxC (cs := HSI) (e := Int) csHSIPx HSI.int == 5
#guard (setPxC (cs := HSI) (e := Int) csHSIPx HSI.hue 9) == ⟨9, 4, 5⟩
#guard (setPxC (cs := HSI) (e := Int) csHSIPx HSI.sat 9) == ⟨3, 9, 5⟩
#guard (setPxC (cs := HSI) (e := Int) csHSIPx HSI.int 9) == ⟨3, 4, 9⟩
#guard (mapPxC (cs := HSI) (e := Int) (fun _ v => v + 1) csHSIPx) == ⟨4, 5, 6⟩
#guard (liftPx (cs := HSI) (e := Int) (· + 1) csHSIPx) == ⟨4, 5, 6⟩
#guard (liftPx2 (cs := HSI) (e := Int) (· + ·) csHSIPx ⟨1, 1, 1⟩) == ⟨4, 5, 6⟩
#guard (foldlPx2 (cs := HSI) (e := Int) (β := Int) (· + · + ·) 0 csHSIPx ⟨1, 1, 1⟩) == 15

-- ── `HSIA`/`PixelHSIA` — `ColorSpace` operations ──

def csHSIAPx : PixelHSIA Int := ⟨3, 4, 5, 6⟩

#guard (channels (cs := HSIA) (e := Int)) == [HSIA.hue, HSIA.sat, HSIA.int, HSIA.alpha]
#guard (toComponents (cs := HSIA) (e := Int) csHSIAPx) == (3, 4, 5, 6)
#guard (fromComponents (cs := HSIA) (e := Int) ((3, 4, 5, 6) : Int × Int × Int × Int)) == csHSIAPx
#guard (promote (cs := HSIA) (7 : Int)) == (⟨7, 7, 7, 7⟩ : PixelHSIA Int)
#guard getPxC (cs := HSIA) (e := Int) csHSIAPx HSIA.hue == 3
#guard getPxC (cs := HSIA) (e := Int) csHSIAPx HSIA.sat == 4
#guard getPxC (cs := HSIA) (e := Int) csHSIAPx HSIA.int == 5
#guard getPxC (cs := HSIA) (e := Int) csHSIAPx HSIA.alpha == 6
#guard (setPxC (cs := HSIA) (e := Int) csHSIAPx HSIA.hue 9) == ⟨9, 4, 5, 6⟩
#guard (setPxC (cs := HSIA) (e := Int) csHSIAPx HSIA.alpha 9) == ⟨3, 4, 5, 9⟩
#guard (mapPxC (cs := HSIA) (e := Int) (fun _ v => v + 1) csHSIAPx) == ⟨4, 5, 6, 7⟩
#guard (liftPx (cs := HSIA) (e := Int) (· + 1) csHSIAPx) == ⟨4, 5, 6, 7⟩
#guard (liftPx2 (cs := HSIA) (e := Int) (· + ·) csHSIAPx ⟨1, 1, 1, 1⟩) == ⟨4, 5, 6, 7⟩
#guard (foldlPx2 (cs := HSIA) (e := Int) (β := Int) (· + · + ·) 0
  csHSIAPx ⟨1, 1, 1, 1⟩) == 22

-- ── `AlphaSpace` between `HSIA` and `HSI` ──

#guard getAlpha (cs := HSIA) (e := Int) csHSIAPx == 6
#guard addAlpha (cs := HSIA) (e := Int) (6 : Int) csHSIPx == csHSIAPx
#guard dropAlpha (cs := HSIA) (e := Int) csHSIAPx == csHSIPx

-- ── Component-wise arithmetic on `PixelHSI` ──

#guard csHSIPx + (⟨1, 1, 1⟩ : PixelHSI Int) == ⟨4, 5, 6⟩
#guard csHSIPx - (⟨1, 1, 1⟩ : PixelHSI Int) == ⟨2, 3, 4⟩
#guard csHSIPx * (⟨2, 2, 2⟩ : PixelHSI Int) == ⟨6, 8, 10⟩
#guard (⟨12, 12, 12⟩ : PixelHSI Int) / (⟨4, 3, 2⟩ : PixelHSI Int) == ⟨3, 4, 6⟩
#guard -csHSIPx == ⟨-3, -4, -5⟩
#guard (0 : PixelHSI Int) == ⟨0, 0, 0⟩
#guard (1 : PixelHSI Int) == ⟨1, 1, 1⟩
