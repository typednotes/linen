/-
  Tests for `Linen.Codec.Picture` — the top-level facade.

  Covers: `convertRGB8`/`convertRGB16`/`convertRGBA8`, `dynamicPixelMap`,
  `generateFoldImage`, `decodeImage`'s try-every-format dispatch (magic-byte
  prefixes alone should not decode; a real encoded file of any supported
  format should decode as that format and no other), a `decodeImage`
  end-to-end round trip per pure-encoded format plus one through the
  `IO`-returning PNG path, and a real on-disk `save*`/`read*` round trip.

  `decodeImage`/`readImage`/`savePngImage` etc. are `IO`-returning (see the
  module doc-comment's universe-wrinkle section), so those checks use
  `#eval show IO Unit from do ... unless ... throw (IO.userError ...)`,
  matching `Tests/Linen/Codec/Picture/PngTest.lean`'s convention.

  Fixture names are prefixed `pic` to avoid cross-file `Tests` namespace
  collisions.
-/
import Linen.Codec.Picture

open Codec.Picture

namespace Tests.Codec.Picture

-- ── A `Data.ByteString` → `ByteArray` helper (encode output → decode input) ──

private def picToByteArray (bs : Data.ByteString) : ByteArray :=
  bs.data.extract bs.off (bs.off + bs.len)

-- `Image` (unlike its pixel types) has no `BEq` instance of its own (its
-- `Component` is an `outParam`, so a blanket `deriving BEq` isn't available);
-- compare the underlying fields directly instead.
private def picImgEq [Pixel α Component] [BEq Component]
    (img₁ img₂ : @Image α Component _) : Bool :=
  img₁.width == img₂.width ∧ img₁.height == img₂.height ∧ img₁.data == img₂.data

-- ── Magic-byte prefixes for every supported format ──
-- (`decodeImage` never derives these into a separate sniff table of its own
-- — each format's own decoder checks its own magic as its first parse step,
-- see the module doc-comment — so these fixtures exercise that indirectly:
-- a bare magic prefix, with no valid body following it, must fail every
-- format's decoder and so must fail `decodeImage` as a whole.)

private def picPngMagic : ByteArray := ByteArray.mk #[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
private def picGif87aMagic : ByteArray := ByteArray.mk "GIF87a".toUTF8.toList.toArray
private def picGif89aMagic : ByteArray := ByteArray.mk "GIF89a".toUTF8.toList.toArray
private def picBmpMagic : ByteArray := ByteArray.mk "BM".toUTF8.toList.toArray
private def picTiffLEMagic : ByteArray := ByteArray.mk #[0x49, 0x49, 0x2A, 0x00]
private def picTiffBEMagic : ByteArray := ByteArray.mk #[0x4D, 0x4D, 0x00, 0x2A]
private def picJpegMagic : ByteArray := ByteArray.mk #[0xFF, 0xD8]
private def picRadianceMagic : ByteArray := ByteArray.mk "#?RADIANCE".toUTF8.toList.toArray
private def picRgbeMagic : ByteArray := ByteArray.mk "#?RGBE".toUTF8.toList.toArray

#eval show IO Unit from do
  for (name, magic) in
      [("PNG", picPngMagic), ("GIF87a", picGif87aMagic), ("GIF89a", picGif89aMagic),
       ("BMP", picBmpMagic), ("TIFF-LE", picTiffLEMagic), ("TIFF-BE", picTiffBEMagic),
       ("JPEG", picJpegMagic), ("Radiance", picRadianceMagic), ("RGBE", picRgbeMagic)] do
    match ← decodeImage magic with
    | .error _ => pure ()
    | .ok _ => throw (IO.userError s!"decodeImage unexpectedly decoded a bare {name} magic prefix")

-- ── Test images ──

private def picRgbImg : Image PixelRGB8 :=
  generateImage (fun x y => (⟨(x * 40 + 10).toUInt8, (y * 50 + 5).toUInt8, ((x + y) * 20).toUInt8⟩ :
    PixelRGB8)) 3 2

private def picRgbaImg : Image PixelRGBA8 :=
  generateImage (fun x y => (⟨(x * 30).toUInt8, (y * 60).toUInt8, 128, (x + y).toUInt8⟩ : PixelRGBA8)) 3 3

private def picY8Img : Image Pixel8 :=
  generateImage (fun x y => (x * 10 + y * 3).toUInt8) 4 2

private def picRgbFImg : Image PixelRGBF :=
  generateImage (fun x y =>
    (⟨(x.toFloat32 + 1) * 0.1, (y.toFloat32 + 1) * 0.2, (x.toFloat32 + y.toFloat32 + 1) * 0.05⟩ :
      PixelRGBF)) 3 2

-- ── `decodeImage` end-to-end round trips: one per pure-encoded format ──

-- Bitmap (`imageToBitmap` is pure).
#eval show IO Unit from do
  let bytes := picToByteArray (imageToBitmap (.rgb8 picRgbImg))
  match ← decodeImage bytes with
  | .error e => throw (IO.userError s!"decodeImage(bitmap) failed to decode: {e}")
  | .ok (.rgb8 img) =>
    unless img.width == 3 ∧ img.height == 2 ∧ img.getPixel 2 1 == picRgbImg.getPixel 2 1 do
      throw (IO.userError "decodeImage(bitmap): pixel mismatch")
  | .ok _ => throw (IO.userError "decodeImage(bitmap): wrong DynamicImage variant")

-- TIFF (`imageToTiff` is pure).
#eval show IO Unit from do
  let bytes := picToByteArray (imageToTiff (.rgb8 picRgbImg))
  match ← decodeImage bytes with
  | .error e => throw (IO.userError s!"decodeImage(tiff) failed to decode: {e}")
  | .ok (.rgb8 img) =>
    unless img.width == 3 ∧ img.height == 2 ∧ img.getPixel 0 0 == picRgbImg.getPixel 0 0 do
      throw (IO.userError "decodeImage(tiff): pixel mismatch")
  | .ok _ => throw (IO.userError "decodeImage(tiff): wrong DynamicImage variant")

-- Radiance HDR (`imageToRadiance` is pure; RGBE is lossy, so this checks
-- approximate rather than exact equality).
private def picCloseF32 (a b : Float32) : Bool := Float32.abs (a - b) < 0.02

#eval show IO Unit from do
  let bytes := picToByteArray (imageToRadiance (.rgbF picRgbFImg))
  match ← decodeImage bytes with
  | .error e => throw (IO.userError s!"decodeImage(hdr) failed to decode: {e}")
  | .ok (.rgbF img) =>
    let p := img.getPixel 2 1
    let q := picRgbFImg.getPixel 2 1
    unless img.width == 3 ∧ img.height == 2 ∧
        picCloseF32 p.r q.r ∧ picCloseF32 p.g q.g ∧ picCloseF32 p.b q.b do
      throw (IO.userError "decodeImage(hdr): pixel mismatch")
  | .ok _ => throw (IO.userError "decodeImage(hdr): wrong DynamicImage variant")

-- JPEG (`imageToJpg` is pure; lossy, so this only checks shape).
#eval show IO Unit from do
  let bytes := picToByteArray (imageToJpg 90 (.ycbcr8 (pixelMap ColorSpaceConvertible.convertPixel picRgbImg)))
  match ← decodeImage bytes with
  | .error e => throw (IO.userError s!"decodeImage(jpeg) failed to decode: {e}")
  | .ok (.ycbcr8 img) => unless img.width == 3 ∧ img.height == 2 do
      throw (IO.userError "decodeImage(jpeg): wrong shape")
  | .ok _ => throw (IO.userError "decodeImage(jpeg): wrong DynamicImage variant")

-- PNG (`imageToPng` is `IO`-returning): exercises `decodeImage`'s own `IO`
-- path all the way through.
#eval show IO Unit from do
  let encoded ← imageToPng (.rgba8 picRgbaImg)
  match ← decodeImage (picToByteArray encoded) with
  | .error e => throw (IO.userError s!"decodeImage(png) failed to decode: {e}")
  | .ok (.rgba8 img) =>
    unless img.width == 3 ∧ img.height == 3 ∧ img.getPixel 1 2 == picRgbaImg.getPixel 1 2 do
      throw (IO.userError "decodeImage(png): pixel mismatch")
  | .ok _ => throw (IO.userError "decodeImage(png): wrong DynamicImage variant")

-- ── `convertRGB8`/`convertRGB16`/`convertRGBA8` ──

#guard picImgEq (convertRGB8 (.rgb8 picRgbImg)) picRgbImg
#guard (convertRGB8 (.y8 picY8Img)).getPixel 1 1 ==
  (⟨picY8Img.getPixel 1 1, picY8Img.getPixel 1 1, picY8Img.getPixel 1 1⟩ : PixelRGB8)
#guard (convertRGB8 (.rgba8 picRgbaImg)).getPixel 0 0 ==
  (⟨picRgbaImg.getPixel 0 0 |>.r, picRgbaImg.getPixel 0 0 |>.g, picRgbaImg.getPixel 0 0 |>.b⟩ : PixelRGB8)

#guard (convertRGBA8 (.rgb8 picRgbImg)).getPixel 2 1 ==
  (⟨(picRgbImg.getPixel 2 1).r, (picRgbImg.getPixel 2 1).g, (picRgbImg.getPixel 2 1).b, 255⟩ : PixelRGBA8)
#guard picImgEq (convertRGBA8 (.rgba8 picRgbaImg)) picRgbaImg

#guard (convertRGB16 (.rgb8 picRgbImg)).width == 3 ∧ (convertRGB16 (.rgb8 picRgbImg)).height == 2
#guard (convertRGB16 (.y8 picY8Img)).getPixel 0 0 ==
  (⟨ColorConvertible.promotePixel (picY8Img.getPixel 0 0),
    ColorConvertible.promotePixel (picY8Img.getPixel 0 0),
    ColorConvertible.promotePixel (picY8Img.getPixel 0 0)⟩ : PixelRGB16)

-- ── `dynamicPixelMap` ──

#guard match dynamicPixelMap (fun img => img) (.y8 picY8Img) with
  | .y8 img => picImgEq img picY8Img
  | _ => false

-- ── `generateFoldImage` ──
-- (`f` runs top-to-bottom, left-to-right: (x,y) pairs for a 3×2 image, in
-- order, are (0,0) (1,0) (2,0) (0,1) (1,1) (2,1), whose `x + y` values sum
-- to 0+1+2+1+2+3 = 9.)

#guard
  let (total, img) : Nat × Image Pixel8 :=
    generateFoldImage (fun acc x y => (acc + x + y, (x + y).toUInt8)) 0 3 2
  total == 9 ∧ img.width == 3 ∧ img.height == 2 ∧ img.getPixel 2 1 == 3

-- ── Real on-disk `save*`/`read*` round trip ──

#eval show IO Unit from do
  let path : System.FilePath := "/tmp/linen_picture_test.bmp"
  saveBmpImage path (.rgb8 picRgbImg)
  match ← readBitmap path with
  | .error e =>
    IO.FS.removeFile path
    throw (IO.userError s!"save/readBitmap round trip failed to decode: {e}")
  | .ok (.rgb8 img) =>
    IO.FS.removeFile path
    unless img.width == 3 ∧ img.height == 2 ∧ img.getPixel 1 1 == picRgbImg.getPixel 1 1 do
      throw (IO.userError "save/readBitmap round trip: pixel mismatch")
  | .ok _ =>
    IO.FS.removeFile path
    throw (IO.userError "save/readBitmap round trip: wrong DynamicImage variant")

end Tests.Codec.Picture
