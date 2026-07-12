/-
  Tests for `Linen.Graphics.Image.IO.Base` — `Convertible`, `ImageFormat`
  (with its `exts`/`isFormat` defaults), `Readable`/`Writable`, and the
  `Writable (Image cs (Complex Float)) format` instance built on
  `leftToRight`/`realPartImg`/`imagPartImg`.

  Fixture/example/instance names are prefixed `iobase` to avoid clashing with
  any other test file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.IO.Base
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (makeImage dims unsafeIndex)
open Graphics.Image.IO.Base
open Graphics.Image.ColorSpace (toImageY toImageYA toImageRGB toImageRGBA)
open Graphics.Image.ColorSpace.Y (Y YA PixelY PixelYA)
open Graphics.Image.ColorSpace.RGB (RGB RGBA PixelRGB PixelRGBA)
open Data (Complex)

-- ── `ImageFormat`: a dummy format with only `ext` given, exercising the
-- `exts`/`isFormat` defaults ──

/-- A dummy single-extension format, with no `SaveOption` of its own
(`Unit`). -/
inductive IobaseFormat where
  | mk
deriving BEq

instance : ImageFormat IobaseFormat Unit where
  ext _ := ".iob"

#guard ext IobaseFormat.mk == ".iob"
#guard exts IobaseFormat.mk == [".iob"]
#guard isFormat ".iob" IobaseFormat.mk == true
#guard isFormat ".nope" IobaseFormat.mk == false

-- A format overriding the `exts`/`isFormat` defaults directly.
inductive IobaseJpegLikeFormat where
  | mk

instance : ImageFormat IobaseJpegLikeFormat Unit where
  ext _ := ".jpeg"
  exts _ := [".jpeg", ".jpg"]

#guard exts IobaseJpegLikeFormat.mk == [".jpeg", ".jpg"]
#guard isFormat ".jpg" IobaseJpegLikeFormat.mk == true
#guard isFormat ".jpeg" IobaseJpegLikeFormat.mk == true
#guard isFormat ".png" IobaseJpegLikeFormat.mk == false

-- ── `Seq`: a plain wrapper tagging a format as "sequence of images" ──

-- `Seq` is qualified below since Lean's core `Seq` (the `<*>`-style class) shares the
-- bare name.
#guard (Graphics.Image.IO.Base.Seq.mk IobaseFormat.mk).unSeq == IobaseFormat.mk

-- ── `Readable`/`Writable`: a dummy codec round-tripping a byte count as `Int` ──

instance : Readable Int IobaseFormat where
  decode _ bytes := .ok (Int.ofNat bytes.size)

instance : Writable Int IobaseFormat where
  encode _ _ n := ByteArray.mk (Array.replicate n.toNat (0 : UInt8))

#guard match decode IobaseFormat.mk (ByteArray.mk #[1, 2, 3]) with
  | .ok (n : Int) => n == 3
  | .error _ => false
#guard (encode IobaseFormat.mk ([] : List Unit) (4 : Int)).size == 4
#guard match decode IobaseFormat.mk (encode IobaseFormat.mk ([] : List Unit) (5 : Int)) with
  | .ok (n : Int) => n == 5
  | .error _ => false

-- ── `Convertible`: normalising a `Y Float` source image to each canonical target ──

private def iobaseYImg : Graphics.Image.Interface.Image Y Float :=
  makeImage (1, 1) (fun _ => (⟨0.5⟩ : PixelY Float))

#guard convert (cs := Y) (e := Float) iobaseYImg == toImageY iobaseYImg
#guard convert (cs := YA) (e := Float) iobaseYImg == toImageYA iobaseYImg
#guard convert (cs := RGB) (e := Float) iobaseYImg == toImageRGB iobaseYImg
#guard convert (cs := RGBA) (e := Float) iobaseYImg == toImageRGBA iobaseYImg

-- ── `Writable (Image cs (Complex Float)) format`: real part left of imaginary part ──

-- A `Writable (Image Y Float) IobaseFormat` instance that encodes each pixel's value
-- as one byte (truncated), in row-major order — enough to observe
-- `leftToRight`'s pixel arrangement through `encode`.
instance : Writable (Graphics.Image.Interface.Image Y Float) IobaseFormat where
  encode _ _ img := ByteArray.mk (img.elems.map (fun (p : PixelY Float) => p.y.toUInt8))

private def iobaseComplexImg : Graphics.Image.Interface.Image Y (Complex Float) :=
  makeImage (1, 2) (fun (_, j) => (⟨if j == 0 then (3.0 : Float) else 4.0, 9.0⟩ : PixelY (Complex Float)))

-- The real part (`3, 4`) is placed to the left of the imaginary part (`9, 9`):
-- a `1×2` complex image becomes a `1×4` encoded byte sequence `[3, 4, 9, 9]`.
#guard encode IobaseFormat.mk ([] : List Unit) iobaseComplexImg ==
  ByteArray.mk #[(3 : UInt8), 4, 9, 9]
