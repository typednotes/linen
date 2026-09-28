/-
  Tests for `Linen.Graphics.Image.IO.Formats.Netpbm`.

  As documented in that module's own doc-comment, this format is decode-only
  (neither upstream's `hip` nor this codebase's own `Linen.Graphics.Netpbm`
  ever provided a PBM/PGM/PPM encoder), so these tests hand-write small
  literal netpbm byte strings as fixtures and check `decode` against them
  directly, rather than round-tripping through `encode`.

  Fixture/instance names are prefixed `pnm` to avoid cross-file `Tests`
  namespace collisions.
-/
import Linen.Graphics.Image.IO.Formats.Netpbm

open Graphics.Image.Interface (dims unsafeIndex)
open Graphics.Image.IO.Base (decode)
open Graphics.Image.IO.Formats.Netpbm
open Graphics.Image.ColorSpace.X (PixelX)
open Graphics.Image.ColorSpace.Binary (Bit)
open Graphics.Image.ColorSpace.Y (PixelY PixelYA)
open Graphics.Image.ColorSpace.RGB (PixelRGB PixelRGBA)

-- ── `PBM` (ASCII, `P1`): a 2×2 bitmap, "0 1 / 1 0" — `0` is white, `1` is
-- black (see `Linen.Graphics.Netpbm`'s own `PbmPixel` doc-comment) ──

private def pnmPBMBytes : ByteArray := "P1\n2 2\n0 1\n1 0\n".toUTF8

#guard match decode PBM.mk pnmPBMBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.X.X Bit) =>
    dims img == (2, 2) ∧
      (unsafeIndex img (0, 0) : PixelX Bit).x.b == true ∧   -- `0` ↦ white ↦ `Bit` on
      (unsafeIndex img (0, 1) : PixelX Bit).x.b == false ∧  -- `1` ↦ black ↦ `Bit` off
      (unsafeIndex img (1, 0) : PixelX Bit).x.b == false ∧
      (unsafeIndex img (1, 1) : PixelX Bit).x.b == true
  | .error _ => false

-- The canonical `Float`-precision family reads the same bitmap through
-- `Convertible`/`ToY`: white (`Bit` on) is luma `1.0`, black is `0.0`.
#guard match decode PBM.mk pnmPBMBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.Y.Y Float) =>
    (unsafeIndex img (0, 0) : PixelY Float).y == 1.0 ∧
      (unsafeIndex img (0, 1) : PixelY Float).y == 0.0
  | .error _ => false

-- ── `PGM` (ASCII, `P2`): a 2×2 8-bit greymap ──

private def pnmPGMAsciiBytes : ByteArray := "P2\n2 2\n255\n0 128 255 64\n".toUTF8

#guard match decode PGM.mk pnmPGMAsciiBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.Y.Y UInt8) =>
    dims img == (2, 2) ∧
      (unsafeIndex img (0, 0) : PixelY UInt8).y == 0 ∧
      (unsafeIndex img (0, 1) : PixelY UInt8).y == 128 ∧
      (unsafeIndex img (1, 0) : PixelY UInt8).y == 255 ∧
      (unsafeIndex img (1, 1) : PixelY UInt8).y == 64
  | .error _ => false

#guard match decode PGM.mk pnmPGMAsciiBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.Y.Y Float) =>
    (unsafeIndex img (0, 1) : PixelY Float).y == 128.0 / 255.0
  | .error _ => false

-- ── `PGM` (binary, `P5`): a 1×1 8-bit greymap, exercising the raw-byte body
-- parser rather than the ASCII one ──

private def pnmPGMBinaryBytes : ByteArray :=
  "P5\n1 1\n255\n".toUTF8 ++ ByteArray.mk #[200]

#guard match decode PGM.mk pnmPGMBinaryBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.Y.Y UInt8) =>
    dims img == (1, 1) ∧ (unsafeIndex img (0, 0) : PixelY UInt8).y == 200
  | .error _ => false

-- ── `PPM` (ASCII, `P3`): a 1×2 8-bit pixmap ──

private def pnmPPMBytes : ByteArray := "P3\n2 1\n255\n10 20 30 40 50 60\n".toUTF8

#guard match decode PPM.mk pnmPPMBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.RGB.RGB UInt8) =>
    dims img == (1, 2) ∧
      (unsafeIndex img (0, 0) : PixelRGB UInt8) == ⟨10, 20, 30⟩ ∧
      (unsafeIndex img (0, 1) : PixelRGB UInt8) == ⟨40, 50, 60⟩
  | .error _ => false

#guard match decode PPM.mk pnmPPMBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.RGB.RGB Float) =>
    (unsafeIndex img (0, 1) : PixelRGB Float) ==
      ⟨40.0 / 255.0, 50.0 / 255.0, 60.0 / 255.0⟩
  | .error _ => false

-- `Convertible RGBA Float`'s alpha channel is always fully opaque for a
-- source with no alpha of its own.
#guard match decode PPM.mk pnmPPMBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.RGB.RGBA Float) =>
    (unsafeIndex img (0, 0) : PixelRGBA Float).a == 1.0
  | .error _ => false

#guard match decode PPM.mk pnmPPMBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.Y.YA Float) =>
    (unsafeIndex img (0, 0) : PixelYA Float).a == 1.0
  | .error _ => false

-- ── `PPM` (binary, `P6`): a 1×2 16-bit pixmap, exercising the raw-byte body
-- parser and 16-bit component width ──

private def pnmPPM16BinaryBytes : ByteArray :=
  "P6\n2 1\n65535\n".toUTF8 ++
    ByteArray.mk #[0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xff, 0xff, 0x00, 0x00, 0x10, 0x20]

#guard match decode PPM.mk pnmPPM16BinaryBytes with
  | .ok (img : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.RGB.RGB UInt16) =>
    dims img == (1, 2) ∧
      (unsafeIndex img (0, 0) : PixelRGB UInt16) == ⟨0x0102, 0x0304, 0x0506⟩ ∧
      (unsafeIndex img (0, 1) : PixelRGB UInt16) == ⟨0xffff, 0x0000, 0x1020⟩
  | .error _ => false

-- ── Mismatched-colour-space decode errors ──

-- A greymap decoded through the `RGB` instance cannot succeed.
#guard match decode PPM.mk pnmPGMAsciiBytes with
  | .ok (_ : Graphics.Image.Interface.Image Graphics.Image.ColorSpace.RGB.RGB UInt8) => false
  | .error _ => true

-- ── `ImageFormat` extension vocabulary ──

#guard Graphics.Image.IO.Base.ext PBM.mk == ".pbm"
#guard Graphics.Image.IO.Base.ext PGM.mk == ".pgm"
#guard Graphics.Image.IO.Base.ext PPM.mk == ".ppm"
#guard Graphics.Image.IO.Base.isFormat ".ppm" PPM.mk == true
#guard Graphics.Image.IO.Base.isFormat ".pgm" PPM.mk == false
