/-
  Tests for `Linen.Codec.Picture.HDR` — checks `RGBE.toFloat`/`RGBE.ofFloat`
  round trips and full encode→decode round trips for both the uncompressed
  and new-style-RLE Radiance encoders.
-/
import Linen.Codec.Picture.HDR

open Codec.Picture

-- ── `RGBE` ↔ `PixelRGBF` ──

#guard (RGBE.toFloat { r := 0, g := 0, b := 0, e := 0 }).r == 0
#guard (RGBE.ofFloat { r := 0, g := 0, b := 0 } : RGBE) == { r := 0, g := 0, b := 0, e := 0 }

-- Round-tripping a mid-range colour through `RGBE` should stay close to the
-- original (RGBE is lossy, so this checks approximate, not exact, equality).
#guard
  let p : PixelRGBF := { r := 0.5, g := 0.25, b := 0.75 }
  let p' := RGBE.toFloat (RGBE.ofFloat p)
  let close (a b : Float32) := Float32.abs (a - b) < 0.01
  close p.r p'.r ∧ close p.g p'.g ∧ close p.b p'.b

-- ── Encode → decode round trips ──

/-- A small 4×3 HDR test image, all pixels distinct so row/column order bugs
    show up. -/
def hdrImg : Image PixelRGBF :=
  generateImage (fun x y =>
    (⟨(x.toFloat32 + 1) * 0.1, (y.toFloat32 + 1) * 0.2, (x.toFloat32 + y.toFloat32 + 1) * 0.05⟩ : PixelRGBF)) 4 3

private def closeF32 (a b : Float32) : Bool := Float32.abs (a - b) < 0.02

private def closePixel (p q : PixelRGBF) : Bool := closeF32 p.r q.r ∧ closeF32 p.g q.g ∧ closeF32 p.b q.b

def hdrRawBytes : ByteArray := ByteArray.mk (encodeRawHDR hdrImg).unpack.toArray

#guard match decodeHDR hdrRawBytes with
  | .ok (.rgbF img) => img.width == 4 ∧ img.height == 3 ∧
      closePixel (img.getPixel 0 0) (hdrImg.getPixel 0 0) ∧ closePixel (img.getPixel 3 2) (hdrImg.getPixel 3 2)
  | _ => false

def hdrRLEBytes : ByteArray := ByteArray.mk (encodeRLENewStyleHDR hdrImg).unpack.toArray

#guard match decodeHDR hdrRLEBytes with
  | .ok (.rgbF img) => img.width == 4 ∧ img.height == 3 ∧
      closePixel (img.getPixel 1 1) (hdrImg.getPixel 1 1) ∧ closePixel (img.getPixel 2 0) (hdrImg.getPixel 2 0)
  | _ => false

/-- A uniform-colour scanline exercises the old-style run-marker path (all
    pixels in a scanline identical after RLE-marker auto-detection falls back
    to old-style for a non-`(2,2,·,·)` leading quad). -/
def hdrFlatImg : Image PixelRGBF := generateImage (fun _ _ => (⟨0.3, 0.3, 0.3⟩ : PixelRGBF)) 5 2

def hdrFlatBytes : ByteArray := ByteArray.mk (encodeRawHDR hdrFlatImg).unpack.toArray

#guard match decodeHDR hdrFlatBytes with
  | .ok (.rgbF img) => closePixel (img.getPixel 4 1) (hdrFlatImg.getPixel 4 1)
  | _ => false
