/-
  Tests for `Linen.Graphics.Image.IO.Formats.JuicyPixels`.

  Every format except `PNG` has pure `Readable`/`Writable` instances (see
  that module's doc-comment for why `PNG` alone needs plain `IO`-returning
  functions instead), so most round trips are checked with plain `#guard`;
  the `PNG` round trips use `#eval show IO Unit from do ...`, matching
  `Tests/Linen/Codec/Picture/PngTest.lean`'s own convention for `IO`-backed
  code. `JPG`'s round trip only checks dimensions, since JPEG is a lossy
  codec (matching `JpgTest.lean`'s own choice not to assert exact pixel
  equality through a lossy encode/decode pair).

  Fixture/instance names are prefixed `jp` to avoid cross-file `Tests`
  namespace collisions.
-/
import Linen.Graphics.Image.IO.Formats.JuicyPixels

open Graphics.Image.Interface (makeImage dims unsafeIndex)
open Graphics.Image.IO.Base (decode encode)
open Graphics.Image.IO.Formats.JuicyPixels
open Graphics.Image.ColorSpace.Y (Y YA PixelY PixelYA)
open Graphics.Image.ColorSpace.RGB (RGB RGBA PixelRGB PixelRGBA)
open Graphics.Image.ColorSpace.YCbCr (YCbCr PixelYCbCr)
open Graphics.Image.ColorSpace.CMYK (CMYK PixelCMYK)

-- ── Shared fixtures ──

private def jpRGBImg : Graphics.Image.Interface.Image RGB UInt8 :=
  makeImage (3, 4) (fun (i, j) =>
    (⟨(i * 40 + j * 10).toNat.toUInt8, (i * 20).toNat.toUInt8, (j * 30).toNat.toUInt8⟩ :
      PixelRGB UInt8))

private def jpRGBAImg : Graphics.Image.Interface.Image RGBA UInt8 :=
  makeImage (2, 3) (fun (i, j) =>
    (⟨(i * 50).toNat.toUInt8, (j * 40).toNat.toUInt8, ((i + j) * 20).toNat.toUInt8, 200⟩ :
      PixelRGBA UInt8))

private def jpYImg : Graphics.Image.Interface.Image Y UInt8 :=
  makeImage (2, 5) (fun (i, j) => (⟨(i * 10 + j * 3).toNat.toUInt8⟩ : PixelY UInt8))

-- ── `BMP` ──

#guard match decode BMP.mk (encode BMP.mk ([] : List Empty) jpRGBImg) with
  | .ok (img : Graphics.Image.Interface.Image RGB UInt8) =>
    dims img == dims jpRGBImg ∧
      unsafeIndex img (1, 2) == unsafeIndex jpRGBImg (1, 2)
  | .error _ => false

-- `Readable (Image Y UInt8) BMP` is ported for type-level fidelity with
-- upstream, but this codebase's own `decodeBitmap` (`Codec.Picture.Bitmap`)
-- always expands an indexed/palette bitmap to true-colour on decode (see
-- that module's own `decodeBitmapWithMetadata`), so `decodeBitmap` never
-- actually returns `.y8` and this instance can never succeed against a real
-- encoded bitmap -- confirmed here, then round-tripped through the `RGB`
-- instance instead, which the truecolour-expanded pixels do agree with.
#guard match decode BMP.mk (encode BMP.mk ([] : List Empty) jpYImg) with
  | .ok (_ : Graphics.Image.Interface.Image Y UInt8) => false
  | .error _ => true

#guard match decode BMP.mk (encode BMP.mk ([] : List Empty) jpYImg) with
  | .ok (img : Graphics.Image.Interface.Image RGB UInt8) =>
    dims img == dims jpYImg ∧ (unsafeIndex img (1, 2)).r == (unsafeIndex jpYImg (1, 2)).y
  | .error _ => false

-- ── `TGA` ──

#guard match decode TGA.mk (encode TGA.mk ([] : List Empty) jpRGBAImg) with
  | .ok (img : Graphics.Image.Interface.Image RGBA UInt8) =>
    dims img == dims jpRGBAImg ∧
      unsafeIndex img (0, 0) == unsafeIndex jpRGBAImg (0, 0)
  | .error _ => false

-- ── `TIF` ──

#guard match decode TIF.mk (encode TIF.mk ([] : List Empty) jpRGBImg) with
  | .ok (img : Graphics.Image.Interface.Image RGB UInt8) => img == jpRGBImg
  | .error _ => false

private def jpCMYKImg : Graphics.Image.Interface.Image CMYK UInt8 :=
  makeImage (2, 2) (fun (i, j) =>
    (⟨(i * 10).toNat.toUInt8, (j * 10).toNat.toUInt8, 5, 250⟩ : PixelCMYK UInt8))

#guard match decode TIF.mk (encode TIF.mk ([] : List Empty) jpCMYKImg) with
  | .ok (img : Graphics.Image.Interface.Image CMYK UInt8) => img == jpCMYKImg
  | .error _ => false

-- ── `HDR` ──

private def jpHDRImg : Graphics.Image.Interface.Image RGB Float32 :=
  makeImage (2, 2) (fun (i, j) =>
    (⟨(i.toNat.toFloat32 * 0.5), (j.toNat.toFloat32 * 0.25), 1.0⟩ : PixelRGB Float32))

#guard match decode HDR.mk (encode HDR.mk ([] : List Empty) jpHDRImg) with
  | .ok (img : Graphics.Image.Interface.Image RGB Float32) => dims img == dims jpHDRImg
  | .error _ => false

-- ── `GIF` (single-colour, so quantisation is lossless: one palette entry) ──

private def jpGIFImg : Graphics.Image.Interface.Image RGB UInt8 :=
  makeImage (3, 3) (fun _ => (⟨10, 20, 30⟩ : PixelRGB UInt8))

#guard match decode GIF.mk (encode GIF.mk ([] : List GIFSaveOption) jpGIFImg) with
  | .ok (img : Graphics.Image.Interface.Image RGB UInt8) => img == jpGIFImg
  | .error _ => false

-- ── `JPG` (lossy: only dimensions are checked) ──

#guard match decode JPG.mk (encode JPG.mk ([.quality 90] : List JPGSaveOption) jpRGBImg) with
  | .ok (img : Graphics.Image.Interface.Image RGB UInt8) => dims img == dims jpRGBImg
  | .error _ => false

#guard match decode JPG.mk (encode JPG.mk ([] : List JPGSaveOption) jpYImg) with
  | .ok (img : Graphics.Image.Interface.Image Y UInt8) => dims img == dims jpYImg
  | .error _ => false

-- ── `PNG` (plain `IO`-returning functions, not `Readable`/`Writable`) ──

#eval show IO Unit from do
  let encoded ← encodePNGImageRGB8 jpRGBImg
  match ← decodePNGImageRGB8 encoded with
  | .error e => throw (IO.userError s!"rgb8 PNG round trip failed to decode: {e}")
  | .ok img =>
    unless img == jpRGBImg do
      throw (IO.userError "rgb8 PNG round trip: pixel mismatch")

#eval show IO Unit from do
  let encoded ← encodePNGImageY8 jpYImg
  match ← decodePNGImageY8 encoded with
  | .error e => throw (IO.userError s!"y8 PNG round trip failed to decode: {e}")
  | .ok img =>
    unless img == jpYImg do
      throw (IO.userError "y8 PNG round trip: pixel mismatch")

#eval show IO Unit from do
  let encoded ← encodePNGImageRGBA8 jpRGBAImg
  match ← decodePNGImageRGBA8 encoded with
  | .error e => throw (IO.userError s!"rgba8 PNG round trip failed to decode: {e}")
  | .ok img =>
    unless img == jpRGBAImg do
      throw (IO.userError "rgba8 PNG round trip: pixel mismatch")

-- ── `ImageFormat` extension vocabulary ──

#guard Graphics.Image.IO.Base.ext BMP.mk == ".bmp"
#guard Graphics.Image.IO.Base.ext PNG.mk == ".png"
#guard Graphics.Image.IO.Base.exts JPG.mk == [".jpg", ".jpeg"]
#guard Graphics.Image.IO.Base.exts TIF.mk == [".tif", ".tiff"]
#guard Graphics.Image.IO.Base.isFormat ".pic" HDR.mk == true
