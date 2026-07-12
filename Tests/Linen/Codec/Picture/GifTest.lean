/-
  Tests for `Linen.Codec.Picture.Gif`. Decode/encode are both pure
  (`Except String X` / `Except String Data.ByteString` — see that module's
  doc-comment for why this module never needs `IO`), so round trips are
  checked with plain `#guard`.

  Fixture names are prefixed `gif` to avoid cross-file `Tests` namespace
  collisions (bare names like `img`/`bytes` have collided across sibling
  test files before).
-/
import Linen.Codec.Picture.Gif

open Codec.Picture

-- ── A `Data.ByteString` → `ByteArray` helper (encode output → decode input) ──

def gifToByteArray (bs : Data.ByteString) : ByteArray :=
  ByteArray.mk bs.unpack.toArray

-- ── Round trip: a small true-colour image, quantised on encode ──

/-- A 4×4 image with a handful of distinct colours, small enough that
    median-cut quantisation reproduces every colour exactly in the
    resulting palette. -/
def gifRgbImg : Image PixelRGB8 :=
  generateImage (fun x y =>
    if x < 2 ∧ y < 2 then (⟨255, 0, 0⟩ : PixelRGB8)
    else if x ≥ 2 ∧ y < 2 then (⟨0, 255, 0⟩ : PixelRGB8)
    else if x < 2 ∧ y ≥ 2 then (⟨0, 0, 255⟩ : PixelRGB8)
    else (⟨255, 255, 0⟩ : PixelRGB8)) 4 4

def gifRgbBytes : ByteArray :=
  match encodeGifImage gifRgbImg with
  | .ok bs => gifToByteArray bs
  | .error _ => ByteArray.empty

#guard match decodeGif gifRgbBytes with
  | .ok (.rgb8 img) => img.width == 4 ∧ img.height == 4 ∧
      img.getPixel 0 0 == gifRgbImg.getPixel 0 0 ∧
      img.getPixel 3 0 == gifRgbImg.getPixel 3 0 ∧
      img.getPixel 0 3 == gifRgbImg.getPixel 0 3 ∧
      img.getPixel 3 3 == gifRgbImg.getPixel 3 3
  | _ => false

-- ── Round trip: an already-paletted image with an explicit global palette ──

def gifPalette4 : Palette := listToPalette [⟨0, 0, 0⟩, ⟨255, 0, 0⟩, ⟨0, 255, 0⟩, ⟨0, 0, 255⟩]

def gifIndexedImg : Image Pixel8 := generateImage (fun x y => ((x + y) % 4).toUInt8) 4 4

def gifIndexedBytes : ByteArray :=
  match encodeGifImageWithPalette gifIndexedImg gifPalette4 with
  | .ok bs => gifToByteArray bs
  | .error _ => ByteArray.empty

#guard match decodeGifWithPaletteAndMetadata gifIndexedBytes with
  | .ok (.inr pal, _) =>
      pal.indexedImage.width == 4 ∧ pal.indexedImage.height == 4 ∧
      pal.indexedImage.getPixel 0 0 == gifIndexedImg.getPixel 0 0 ∧
      pal.indexedImage.getPixel 3 3 == gifIndexedImg.getPixel 3 3
  | _ => false

-- ── Round trip: a frame with a local colour table and transparency ──

def gifLocalPalette : Palette := listToPalette [⟨10, 20, 30⟩, ⟨200, 210, 220⟩]

def gifLocalFrame : GifFrame :=
  { pixels := generateImage (fun x _ => (x % 2).toUInt8) 2 2
    localPalette := some gifLocalPalette
    transparent := some 0 }

def gifLocalSpec : GifEncode :=
  { screenWidth := 2, screenHeight := 2, frames := [gifLocalFrame] }

def gifLocalBytes : ByteArray :=
  match encodeComplexGifImage gifLocalSpec with
  | .ok bs => gifToByteArray bs
  | .error _ => ByteArray.empty

-- Index `0` is declared transparent, so its RGBA alpha channel must read `0`;
-- index `1` keeps the local palette's opaque colour.
#guard match decodeGif gifLocalBytes with
  | .ok (.rgba8 img) =>
      (img.getPixel 0 0).a == 0 ∧ (img.getPixel 1 0).a != 0 ∧
      (img.getPixel 1 0).r == 200 ∧ (img.getPixel 1 0).g == 210 ∧ (img.getPixel 1 0).b == 220
  | _ => false

-- ── A hand-crafted GIF byte stream, decoded independent of this module's own encoder ──

-- The pixel-index stream `[0, 1, 0, 1]` for a 2×2 image, LZW-compressed via
-- module 19's `lzwEncode` directly (not through any of this module's own
-- `encode*` functions), then assembled by hand into a complete GIF87a file:
-- signature, logical screen descriptor with a 2-colour global table, a
-- single image descriptor with no local table, the LZW data as one
-- sub-block, and the trailer.
def gifHandCraftedBytes : ByteArray :=
  let lzwData := (lzwEncode 2 #[0, 1, 0, 1]).toList
  ByteArray.mk (
    -- "GIF87a"
    [0x47, 0x49, 0x46, 0x38, 0x37, 0x61] ++
    -- Logical screen descriptor: 2×2, global map present, table size 2^1 = 2
    [0x02, 0x00, 0x02, 0x00, 0x80, 0x00, 0x00] ++
    -- Global colour table: black, white
    [0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF] ++
    -- Image descriptor: left 0, top 0, width 2, height 2, no local map
    [0x2C, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x02, 0x00, 0x00] ++
    -- LZW minimum code size, one data sub-block, sub-block terminator
    ([0x02] ++ [UInt8.ofNat lzwData.length] ++ lzwData ++ [0x00]) ++
    -- Trailer
    [0x3B]).toArray

#guard match decodeGif gifHandCraftedBytes with
  | .ok (.rgb8 img) =>
      img.width == 2 ∧ img.height == 2 ∧
      img.getPixel 0 0 == (⟨0, 0, 0⟩ : PixelRGB8) ∧
      img.getPixel 1 0 == (⟨255, 255, 255⟩ : PixelRGB8) ∧
      img.getPixel 0 1 == (⟨0, 0, 0⟩ : PixelRGB8) ∧
      img.getPixel 1 1 == (⟨255, 255, 255⟩ : PixelRGB8)
  | _ => false

-- `getDelaysGifImages`/`decodeGifImages` on the same single-frame stream:
-- one frame, delay `0` (no graphic control extension present).
#guard match decodeGifImages gifHandCraftedBytes, getDelaysGifImages gifHandCraftedBytes with
  | .ok [.rgba8 img], .ok [d] => img.width == 2 ∧ img.height == 2 ∧ d == 0
  | _, _ => false
