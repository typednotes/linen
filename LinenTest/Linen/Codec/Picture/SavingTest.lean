/-
  Tests for `Linen.Codec.Picture.Saving`: dispatch a small hand-built image
  (in a handful of representative `DynamicImage` variants) through each
  supported format's save path (`imageToBitmap`/`imageToRadiance`/
  `imageToJpg`/`imageToPng`/`imageToTiff`/`imageToGif`) and check the
  resulting bytes are non-empty and, where cheap, round-trip through that
  format's own decoder.

  Fixture names are prefixed `saving` to avoid cross-file `Tests` namespace
  collisions.
-/
import Linen.Codec.Picture.Saving

open Codec.Picture

namespace Tests.Codec.Picture.Saving

-- ── A `Data.ByteString` → `ByteArray` helper (encode output → decode input) ──

def savingToByteArray (bs : Data.ByteString) : ByteArray :=
  ByteArray.mk bs.unpack.toArray

-- ── Fixture images, one per representative pixel type ──

def savingRgb8Img : Image PixelRGB8 :=
  generateImage (fun x y =>
    if x < 2 ∧ y < 2 then (⟨255, 0, 0⟩ : PixelRGB8)
    else if x ≥ 2 ∧ y < 2 then (⟨0, 255, 0⟩ : PixelRGB8)
    else if x < 2 ∧ y ≥ 2 then (⟨0, 0, 255⟩ : PixelRGB8)
    else (⟨255, 255, 0⟩ : PixelRGB8)) 4 4

def savingY8Img : Image Pixel8 :=
  generateImage (fun x _ => (x * 60 : Nat).toUInt8) 4 4

def savingRgba8Img : Image PixelRGBA8 :=
  generateImage (fun x y => ⟨(x * 60 : Nat).toUInt8, (y * 60 : Nat).toUInt8, 128, 255⟩) 4 4

def savingCmyk8Img : Image PixelCMYK8 :=
  generateImage (fun x y => ⟨(x * 40 : Nat).toUInt8, (y * 40 : Nat).toUInt8, 10, 5⟩) 4 4

def savingYcbcr8Img : Image PixelYCbCr8 :=
  generateImage (fun x y => ⟨(x * 40 : Nat).toUInt8, 128, (y * 20 : Nat).toUInt8⟩) 4 4

def savingRgbFImg : Image PixelRGBF :=
  generateImage (fun x y => ⟨(x.toFloat32) / 4.0, (y.toFloat32) / 4.0, 0.5⟩) 4 4

def savingY16Img : Image Pixel16 :=
  generateImage (fun x _ => (x * 6000 : Nat).toUInt16) 4 4

-- ── `imageToBitmap` (pure, total) ──

#guard (imageToBitmap (.rgb8 savingRgb8Img)).len > 0
#guard (imageToBitmap (.y8 savingY8Img)).len > 0
#guard (imageToBitmap (.rgba8 savingRgba8Img)).len > 0
#guard (imageToBitmap (.cmyk8 savingCmyk8Img)).len > 0
#guard (imageToBitmap (.ycbcr8 savingYcbcr8Img)).len > 0
#guard (imageToBitmap (.rgbF savingRgbFImg)).len > 0
#guard (imageToBitmap (.y16 savingY16Img)).len > 0

-- Round trip an RGB8 image through the bitmap encoder/decoder.
#guard match decodeBitmap (savingToByteArray (imageToBitmap (.rgb8 savingRgb8Img))) with
  | .ok (.rgb8 img) => img.width == 4 ∧ img.height == 4 ∧
      img.getPixel 0 0 == savingRgb8Img.getPixel 0 0 ∧
      img.getPixel 3 3 == savingRgb8Img.getPixel 3 3
  | _ => false

-- Round trip a grayscale image, converted down from CMYK, through the
-- bitmap encoder/decoder (`imageToBitmap`'s `cmyk8` branch → RGB8).
#guard match decodeBitmap (savingToByteArray (imageToBitmap (.cmyk8 savingCmyk8Img))) with
  | .ok (.rgb8 img) => img.width == 4 ∧ img.height == 4
  | _ => false

-- ── `imageToRadiance` (pure, total) ──

#guard (imageToRadiance (.rgbF savingRgbFImg)).len > 0
#guard (imageToRadiance (.rgb8 savingRgb8Img)).len > 0
#guard (imageToRadiance (.y8 savingY8Img)).len > 0
#guard (imageToRadiance (.rgba8 savingRgba8Img)).len > 0
#guard (imageToRadiance (.cmyk8 savingCmyk8Img)).len > 0
#guard (imageToRadiance (.ycbcr8 savingYcbcr8Img)).len > 0
#guard (imageToRadiance (.y16 savingY16Img)).len > 0

-- Round trip an RGBF image through the Radiance HDR encoder/decoder.
#guard match decodeHDR (savingToByteArray (imageToRadiance (.rgbF savingRgbFImg))) with
  | .ok (.rgbF img) => img.width == 4 ∧ img.height == 4
  | _ => false

-- ── `imageToJpg` (pure, total) ──

#guard (imageToJpg 80 (.ycbcr8 savingYcbcr8Img)).len > 0
#guard (imageToJpg 80 (.rgb8 savingRgb8Img)).len > 0
#guard (imageToJpg 80 (.y8 savingY8Img)).len > 0
#guard (imageToJpg 80 (.rgba8 savingRgba8Img)).len > 0
#guard (imageToJpg 80 (.cmyk8 savingCmyk8Img)).len > 0
#guard (imageToJpg 80 (.rgbF savingRgbFImg)).len > 0
#guard (imageToJpg 80 (.y16 savingY16Img)).len > 0

-- Round trip an RGB8 image (converted to YCbCr8 internally) through the
-- JPEG encoder/decoder.
#guard match decodeJpeg (imageToJpg 90 (.rgb8 savingRgb8Img)).unpack with
  | .ok (.ycbcr8 img) => img.width == 4 ∧ img.height == 4
  | _ => false

-- ── `imageToPng` (`IO`, total) ──

-- `imageToPng` is `IO`-returning (matching `Png.lean`'s `encodePng`; see
-- `Saving.lean`'s module doc-comment), so — matching `PngTest.lean`'s own
-- convention — this round trip is checked with `#eval show IO Unit from
-- do ... unless ... throw (IO.userError ...)` rather than a plain `#guard`.
#eval show IO Unit from do
  let ycbcr8Bytes ← imageToPng (.ycbcr8 savingYcbcr8Img)
  let rgb8Bytes ← imageToPng (.rgb8 savingRgb8Img)
  let y8Bytes ← imageToPng (.y8 savingY8Img)
  let rgba8Bytes ← imageToPng (.rgba8 savingRgba8Img)
  let cmyk8Bytes ← imageToPng (.cmyk8 savingCmyk8Img)
  let rgbFBytes ← imageToPng (.rgbF savingRgbFImg)
  let y16Bytes ← imageToPng (.y16 savingY16Img)
  unless ycbcr8Bytes.len > 0 ∧ rgb8Bytes.len > 0 ∧ y8Bytes.len > 0 ∧ rgba8Bytes.len > 0 ∧
      cmyk8Bytes.len > 0 ∧ rgbFBytes.len > 0 ∧ y16Bytes.len > 0 do
    throw (IO.userError "imageToPng: some branch produced empty bytes")
  match ← decodePng (savingToByteArray rgb8Bytes) with
  | .error e => throw (IO.userError s!"imageToPng rgb8 round trip failed to decode: {e}")
  | .ok (.rgb8 img) =>
    unless img.width == 4 ∧ img.height == 4 ∧
        img.getPixel 0 0 == savingRgb8Img.getPixel 0 0 ∧
        img.getPixel 3 3 == savingRgb8Img.getPixel 3 3 do
      throw (IO.userError "imageToPng rgb8 round trip: pixel mismatch")
  | .ok _ => throw (IO.userError "imageToPng rgb8 round trip: wrong DynamicImage variant")

-- ── `imageToTiff` (pure, total) ──

#guard (imageToTiff (.ycbcr8 savingYcbcr8Img)).len > 0
#guard (imageToTiff (.rgb8 savingRgb8Img)).len > 0
#guard (imageToTiff (.y8 savingY8Img)).len > 0
#guard (imageToTiff (.rgba8 savingRgba8Img)).len > 0
#guard (imageToTiff (.cmyk8 savingCmyk8Img)).len > 0
#guard (imageToTiff (.rgbF savingRgbFImg)).len > 0
#guard (imageToTiff (.y16 savingY16Img)).len > 0

-- Round trip an RGB8 image through the TIFF encoder/decoder.
#guard match decodeTiff (savingToByteArray (imageToTiff (.rgb8 savingRgb8Img))) with
  | .ok (.rgb8 img) => img.width == 4 ∧ img.height == 4 ∧
      img.getPixel 0 0 == savingRgb8Img.getPixel 0 0 ∧
      img.getPixel 3 3 == savingRgb8Img.getPixel 3 3
  | _ => false

-- ── `imageToGif` (pure, `Except`-returning) ──

#guard match imageToGif (.rgb8 savingRgb8Img) with | .ok bs => bs.len > 0 | .error _ => false
#guard match imageToGif (.y8 savingY8Img) with | .ok bs => bs.len > 0 | .error _ => false
#guard match imageToGif (.rgba8 savingRgba8Img) with | .ok bs => bs.len > 0 | .error _ => false
#guard match imageToGif (.cmyk8 savingCmyk8Img) with | .ok bs => bs.len > 0 | .error _ => false
#guard match imageToGif (.ycbcr8 savingYcbcr8Img) with | .ok bs => bs.len > 0 | .error _ => false
#guard match imageToGif (.rgbF savingRgbFImg) with | .ok bs => bs.len > 0 | .error _ => false
#guard match imageToGif (.y16 savingY16Img) with | .ok bs => bs.len > 0 | .error _ => false

-- Round trip an RGB8 image (colour-quantised on encode) through the GIF
-- encoder/decoder.
#guard match imageToGif (.rgb8 savingRgb8Img) with
  | .ok bs =>
      (match decodeGif (savingToByteArray bs) with
        | .ok (.rgb8 img) => img.width == 4 ∧ img.height == 4
        | _ => false)
  | .error _ => false

-- Round trip a grayscale image through the GIF encoder/decoder (the
-- `greyPalette`-backed `y8` branch).
#guard match imageToGif (.y8 savingY8Img) with
  | .ok bs =>
      (match decodeGif (savingToByteArray bs) with
        | .ok (.rgb8 img) => img.width == 4 ∧ img.height == 4
        | _ => false)
  | .error _ => false
