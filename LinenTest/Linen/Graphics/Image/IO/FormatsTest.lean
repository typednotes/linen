/-
  Tests for `Linen.Graphics.Image.IO.Formats`.

  Most of this module is a pure re-export of `IO.Formats.JuicyPixels` (#22)
  and `IO.Formats.Netpbm` (#23) — already tested in full in their own
  `Tests/` counterparts — so these tests check only (a) that both families
  are indeed reachable through a plain `import Linen.Graphics.Image.IO.
  Formats` with no further re-export step, via one representative smoke test
  into each, and (b) this module's own genuine new content, `InputFormat`/
  `OutputFormat`'s `ImageFormat` instances.

  Fixture/instance names are prefixed `ff` to avoid cross-file `Tests`
  namespace collisions.
-/
import Linen.Graphics.Image.IO.Formats

open Graphics.Image.Interface (makeImage dims unsafeIndex)
open Graphics.Image.IO.Base (decode encode ext exts)
open Graphics.Image.IO.Formats
open Graphics.Image.IO.Formats.JuicyPixels (BMP)
open Graphics.Image.IO.Formats.Netpbm (PBM)
open Graphics.Image.ColorSpace.RGB (RGB PixelRGB)
open Graphics.Image.ColorSpace.X (X)
open Graphics.Image.ColorSpace.Binary (Bit)

-- ── Reachability: `IO.Formats.JuicyPixels` (#22), via this facade's import ──

private def ffRGBImg : Graphics.Image.Interface.Image RGB UInt8 :=
  makeImage (2, 2) (fun (i, j) =>
    (⟨(i * 40).toNat.toUInt8, (j * 40).toNat.toUInt8, 10⟩ : PixelRGB UInt8))

#guard match decode BMP.mk (encode BMP.mk ([] : List Empty) ffRGBImg) with
  | .ok (img : Graphics.Image.Interface.Image RGB UInt8) =>
    dims img == dims ffRGBImg ∧ unsafeIndex img (1, 1) == unsafeIndex ffRGBImg (1, 1)
  | .error _ => false

-- ── Reachability: `IO.Formats.Netpbm` (#23), via this facade's import ──

private def ffPBMBytes : ByteArray := "P1\n2 2\n0 1\n1 0\n".toUTF8

#guard match decode PBM.mk ffPBMBytes with
  | .ok (img : Graphics.Image.Interface.Image X Bit) => dims img == (2, 2)
  | .error _ => false

-- ── `InputFormat` ──

#guard ext InputFormat.bmp == ".bmp"
#guard ext InputFormat.gif == ".gif"
#guard ext InputFormat.hdr == ".hdr"
#guard exts InputFormat.hdr == [".hdr", ".pic"]
#guard ext InputFormat.jpg == ".jpg"
#guard exts InputFormat.jpg == [".jpg", ".jpeg"]
#guard ext InputFormat.png == ".png"
#guard ext InputFormat.tif == ".tif"
#guard ext InputFormat.tga == ".tga"
-- `pnm` collapses `PBM`/`PGM`/`PPM`: `ext` prefers `.ppm`, `exts` lists all three.
#guard ext InputFormat.pnm == ".ppm"
#guard exts InputFormat.pnm == [".pbm", ".pgm", ".ppm"]

-- ── `OutputFormat` — no `pnm` tag, matching upstream (netpbm has no encoder) ──

#guard ext OutputFormat.bmp == ".bmp"
#guard ext OutputFormat.gif == ".gif"
#guard ext OutputFormat.hdr == ".hdr"
#guard ext OutputFormat.jpg == ".jpg"
#guard ext OutputFormat.png == ".png"
#guard ext OutputFormat.tif == ".tif"
#guard ext OutputFormat.tga == ".tga"
