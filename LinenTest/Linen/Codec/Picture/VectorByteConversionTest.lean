/-
  Tests for `Linen.Codec.Picture.VectorByteConversion` — checks the
  `Array UInt8` ↔ `ByteArray` round-trip and building an `Image` from a raw
  byte buffer.
-/
import Linen.Codec.Picture.VectorByteConversion

open Codec.Picture

-- ── `toByteArray` / `ofByteArray` ──

#guard toByteArray #[1, 2, 3] == ByteArray.mk #[1, 2, 3]
#guard ofByteArray (ByteArray.mk #[1, 2, 3]) == #[1, 2, 3]
#guard ofByteArray (toByteArray #[10, 20, 30, 40]) == #[10, 20, 30, 40]
#guard toByteArray #[] == ByteArray.empty

-- ── `imageFromByteArray` ──

private def img : Image PixelRGB8 :=
  imageFromByteArray 2 1 (ByteArray.mk #[1, 2, 3, 4, 5, 6])

#guard img.getPixel 0 0 == (⟨1, 2, 3⟩ : PixelRGB8)
#guard img.getPixel 1 0 == (⟨4, 5, 6⟩ : PixelRGB8)
