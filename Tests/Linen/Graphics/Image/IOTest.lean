/-
  Tests for `Linen.Graphics.Image.IO`.

  `guessFormat` is checked with plain `#guard` (pure). `readImageExact`/
  `readImageExact'`/`writeImageExact` do real file IO, so they are checked
  with `#eval show IO Unit from do ... unless ... throw (IO.userError ...)`
  writing to and reading back from `/tmp`, matching
  `Tests/Linen/Codec/PictureTest.lean`'s own "real on-disk save/read round
  trip" convention.

  Fixture names are prefixed `io` to avoid cross-file `Tests` namespace
  collisions.
-/
import Linen.Graphics.Image.IO

open Graphics.Image.IO
open Graphics.Image.Interface (makeImage dims unsafeIndex)
open Graphics.Image.IO.Formats (InputFormat OutputFormat)
open Graphics.Image.IO.Formats.JuicyPixels (BMP TIF)
open Graphics.Image.ColorSpace.RGB (RGB PixelRGB)

-- ── `guessFormat` ──

#guard guessFormat allInputFormats (path := "picture.bmp") == some InputFormat.bmp
#guard guessFormat allInputFormats (path := "picture.png") == some InputFormat.png
#guard guessFormat allInputFormats (path := "picture.jpeg") == some InputFormat.jpg
#guard guessFormat allInputFormats (path := "picture.ppm") == some InputFormat.pnm
#guard guessFormat allInputFormats (path := "picture") == none
#guard guessFormat allInputFormats (path := "picture.xyz") == none
#guard guessFormat allOutputFormats (path := "picture.tiff") == some OutputFormat.tif
-- Netpbm has no encoder, so `OutputFormat` has no `pnm` tag at all (see
-- `IO.Formats`'s own doc-comment); a `.ppm` extension therefore guesses
-- nothing on the *output* side, unlike the input side above.
#guard guessFormat allOutputFormats (path := "picture.ppm") == none

-- ── `readImageExact`/`readImageExact'`/`writeImageExact`: real on-disk round trip ──

private def ioRgbImg : Graphics.Image.Interface.Image RGB UInt8 :=
  makeImage (3, 4) (fun (i, j) =>
    (⟨(i * 40 + j * 10).toNat.toUInt8, (i * 20).toNat.toUInt8, (j * 30).toNat.toUInt8⟩ :
      PixelRGB UInt8))

#eval show IO Unit from do
  let path : System.FilePath := "/tmp/linen_graphics_image_io_test.bmp"
  writeImageExact BMP.mk ([] : List Empty) path ioRgbImg
  match ← readImageExact BMP.mk path with
  | .error e =>
    IO.FS.removeFile path
    throw (IO.userError s!"writeImageExact/readImageExact BMP round trip failed to decode: {e}")
  | .ok (img : Graphics.Image.Interface.Image RGB UInt8) =>
    IO.FS.removeFile path
    unless dims img == dims ioRgbImg ∧ unsafeIndex img (1, 2) == unsafeIndex ioRgbImg (1, 2) do
      throw (IO.userError "writeImageExact/readImageExact BMP round trip: pixel mismatch")

#eval show IO Unit from do
  let path : System.FilePath := "/tmp/linen_graphics_image_io_test.tif"
  writeImageExact TIF.mk ([] : List Empty) path ioRgbImg
  let img ← readImageExact' TIF.mk (img := Graphics.Image.Interface.Image RGB UInt8) path
  IO.FS.removeFile path
  unless img == ioRgbImg do
    throw (IO.userError "writeImageExact/readImageExact' TIF round trip: pixel mismatch")

-- `readImageExact` turns a missing file into an `Except`-level error rather
-- than raising an uncaught `IO` exception (the `try ... catch` wrapping
-- described in the module doc-comment).
#eval show IO Unit from do
  match ← readImageExact BMP.mk (img := Graphics.Image.Interface.Image RGB UInt8)
      "/tmp/linen_graphics_image_io_test_missing.bmp" with
  | .error _ => pure ()
  | .ok _ => throw (IO.userError "readImageExact: expected a missing-file error")
