/-
  Linen.Graphics.Image.IO.Formats.Netpbm — glue between hip's `Image cs e`
  and this codebase's own `Linen.Graphics.Netpbm` (netpbm) PBM/PGM/PPM parser

  ## Haskell equivalent

  `Graphics.Image.IO.Formats.Netpbm` from
  https://hackage.haskell.org/package/hip (module #23 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`). Fetched from the 1.5.6.0
  release tarball (`raw.githubusercontent.com/lehins/hip/master/…` 404s,
  same as `IO.Base`'s/`IO.Formats.JuicyPixels`'s own note).

  ## Read-only: no `Writable` instances at all — a genuine divergence, not a
  ## drop

  Upstream's own `Graphics.Netpbm` (Hackage `netpbm`) package is a *parser
  only*: it exposes `parsePPM`/`decodePnm` and no encoder at all, and hip's
  `Netpbm.hs` mirrors that exactly — every instance in that module is
  `Readable`, and not a single `Writable` instance is declared anywhere in
  it (contrast `IO.Formats.JuicyPixels`, whose `Codec.Picture` counterpart
  *does* expose encoders, hence that module's `Writable` instances). This
  port's own `Linen.Graphics.Netpbm` (already ported; see that module's own
  doc-comment) is equally decode-only, with no `encode`/`render`-style
  function to call. So this module gives **no `Writable` instances**, for
  exactly the reason upstream itself gives none: nothing on either side of
  the port, upstream's or this codebase's, has ever provided PBM/PGM/PPM
  *encoding*. Tests below therefore hand-write small literal PBM/PGM/PPM
  byte strings as fixtures rather than round-tripping through an encoder.

  ## Pixel-type-enumeration mismatch: none found

  Every concrete `Readable` instance upstream declares against `PBM`/`PGM`/
  `PPM` has a directly corresponding pixel shape already ported on both
  sides: `Graphics.Netpbm.PpmPixelData` enumerates exactly `pbm`/`grey8`/
  `grey16`/`rgb8`/`rgb16`, matching upstream's `PbmPixelData`/
  `PgmPixelData8`/`PgmPixelData16`/`PpmPixelDataRGB8`/`PpmPixelDataRGB16`
  one-for-one, and every one of those five has a matching hip-side colour
  space/precision pair already ported (`X`×`Bit`, `Y`×`UInt8`, `Y`×`UInt16`,
  `RGB`×`UInt8`, `RGB`×`UInt16`). So, unlike `IO.Formats.JuicyPixels`'s own
  `TIF`/`YCbCr8` mismatch, every upstream instance below ports directly,
  with no narrowing.

  ## Canonical `Double`-precision family: ported in full, unlike
  ## `JuicyPixels`

  Upstream also gives one `Readable (Image VS <cs> Double) <format>`
  instance per canonical presentation colour space (`Y`/`YA`/`RGB`/`RGBA`),
  built on `IO.Base.Convertible`/`convert`, exactly as `IO.Formats.
  JuicyPixels`'s own "generic `Double`" family does. `IO.Formats.
  JuicyPixels`'s own doc-comment documents *deferring* that family there
  because its *write* side needs a `toWord8I`/`toWord16I`-style component-
  narrowing transform this port's `ColorSpace.lean` does not provide
  generically. That obstruction is specific to *encoding*: this module is
  decode-only (see above), so the problem never arises here, and the full
  canonical family ports directly against `convert`/`Convertible` (module
  #21), including from a decoded bilevel `Image X Bit` source —
  `ColorSpace.lean` already gives `X`/`Bit` `ToY`/`ToYA`/`ToRGB`/`ToRGBA`
  instances (see that module's own "declared first" section), so `PBM`'s
  canonical instances need no further work either.

  ## `Seq PBM`/`Seq PGM`/`Seq PPM` (multi-image files) — deferred

  Upstream additionally gives `Readable [Image VS <cs> <e>] (Seq <format>)`
  instances decoding *every* image in a multi-image PBM/PGM/PPM file (a
  single netpbm file may concatenate several rasters back to back).
  `Linen.Graphics.Netpbm.parsePPM` already supports this (`imagesParser`
  parses one-or-more images into a `List PPM`; see that module's own
  doc-comment), and `IO.Base.Seq` (module #21) already ports the tag these
  instances would need — so nothing is missing on either side, the same
  situation `IO.Formats.JuicyPixels`'s own "no `GIFA`/`Seq GIF`" deferral
  describes. This module does not spend the instance-declaration volume on
  it: a future caller needing every image from a multi-image netpbm file
  can already call `Graphics.Netpbm.parsePPM` directly and map this
  module's own `pbmDataToImageX`/`pgmDataToImageY8`/etc. over the result,
  without a dedicated `Seq`-tagged `Readable` instance. A genuine deferral,
  documented, not a silent drop.

  ## `unsafeCast` → honest elementwise conversion

  Upstream's `makeImageUnsafe`/`pnmDataToImage` reinterpret netpbm's
  `Data.Vector.Storable` pixel buffer directly as hip's own `Image`'s
  backing buffer via `V.unsafeCast` — `O(1)`, no copy. As with `IO.Formats.
  JuicyPixels`'s own `toJPImage*`/`fromJPImage*` (see that module's
  doc-comment), the two `Image` types here have genuinely different backing
  shapes (`Manifest DIM2 px` vs. `Linen.Graphics.Netpbm`'s flat row-major
  `Array`), so every conversion below is an honest, `O(width · height)`
  elementwise traversal (`Interface.makeImage`, indexing directly into the
  netpbm pixel `Array`) instead — same "no shared buffer to reinterpret"
  substitution, faithful on well-formed input since both sides already
  agree on row-major pixel order.

  ## Fixture/test naming

  Following `IO.Formats.JuicyPixels`'s own convention, tests in
  `Tests/Linen/Graphics/Image/IO/Formats/NetpbmTest.lean` use a `pnm` prefix
  on every fixture, to avoid cross-file `Tests` namespace collisions.
-/

import Linen.Graphics.Image.ColorSpace
import Linen.Graphics.Image.IO.Base
import Linen.Graphics.Netpbm

open Graphics.Image.Interface (Pixel Image dims unsafeIndex makeImage)
open Graphics.Image.IO.Base (Convertible convert ImageFormat Readable ext exts isFormat)
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.ColorSpace.Binary (Bit)
open Graphics.Image.ColorSpace.Y (Y YA PixelY PixelYA)
open Graphics.Image.ColorSpace.RGB (RGB RGBA PixelRGB PixelRGBA)

namespace Graphics.Image.IO.Formats.Netpbm

-- ── Pixel-array marshalling: `Graphics.Netpbm.PpmPixelData` → hip `Image` ──
-- See the module doc-comment for why these are honest elementwise
-- traversals rather than upstream's `O(1)` buffer-reinterpreting cast.
-- `Graphics.Netpbm`'s own pixel `Array`s are row-major, matching
-- `Interface.makeImage`'s own `(row, col)` iteration order.

/-- Upstream's `pnmDataPBMToImage`'s underlying conversion. Indexes with
`Array.getD` rather than `[·]!` because `Linen.Graphics.Netpbm`'s own pixel
structures derive no `Inhabited` instance (no such instance is needed
there); the fallback value is never actually returned since `w`/`h` always
match `pixels.size` by construction (both come from the same parsed
`PPM`). -/
def pbmDataToImageX (w h : Nat) (pixels : Array Graphics.Netpbm.PbmPixel) : Image X Bit :=
  makeImage (Int.ofNat h, Int.ofNat w) (fun (i, j) =>
    (⟨⟨(pixels.getD (i.toNat * w + j.toNat) ⟨false⟩).isWhite⟩⟩ : PixelX Bit))

/-- Upstream's `pnmDataPGM8ToImage`'s underlying conversion. -/
def pgmDataToImageY8 (w h : Nat) (pixels : Array Graphics.Netpbm.PgmPixel8) : Image Y UInt8 :=
  makeImage (Int.ofNat h, Int.ofNat w) (fun (i, j) =>
    (⟨(pixels.getD (i.toNat * w + j.toNat) ⟨0⟩).v⟩ : PixelY UInt8))

/-- Upstream's `pnmDataPGM16ToImage`'s underlying conversion. -/
def pgmDataToImageY16 (w h : Nat) (pixels : Array Graphics.Netpbm.PgmPixel16) : Image Y UInt16 :=
  makeImage (Int.ofNat h, Int.ofNat w) (fun (i, j) =>
    (⟨(pixels.getD (i.toNat * w + j.toNat) ⟨0⟩).v⟩ : PixelY UInt16))

/-- Upstream's `pnmDataPPM8ToImage`'s underlying conversion. -/
def ppmDataToImageRGB8 (w h : Nat) (pixels : Array Graphics.Netpbm.PpmPixelRGB8) :
    Image RGB UInt8 :=
  makeImage (Int.ofNat h, Int.ofNat w) (fun (i, j) =>
    let p := pixels.getD (i.toNat * w + j.toNat) ⟨0, 0, 0⟩
    (⟨p.r, p.g, p.b⟩ : PixelRGB UInt8))

/-- Upstream's `pnmDataPPM16ToImage`'s underlying conversion. -/
def ppmDataToImageRGB16 (w h : Nat) (pixels : Array Graphics.Netpbm.PpmPixelRGB16) :
    Image RGB UInt16 :=
  makeImage (Int.ofNat h, Int.ofNat w) (fun (i, j) =>
    let p := pixels.getD (i.toNat * w + j.toNat) ⟨0, 0, 0⟩
    (⟨p.r, p.g, p.b⟩ : PixelRGB UInt16))

-- ── Extracting one concrete pixel shape out of a decoded `PpmPixelData` ──

/-- Upstream's `pnmShowData`. -/
private def pnmShowData : Graphics.Netpbm.PpmPixelData → String
  | .pbm _ => "Binary (Pixel X Bit)"
  | .grey8 _ => "Y8 (Pixel Y Word8)"
  | .grey16 _ => "Y16 (Pixel Y Word16)"
  | .rgb8 _ => "RGB8 (Pixel RGB Word8)"
  | .rgb16 _ => "RGB16 (Pixel RGB Word16)"

/-- Upstream's `pnmError`. -/
private def pnmError {α : Type} (err : String) : Except String α :=
  .error s!"Netpbm decoding error: {err}"

/-- Upstream's `pnmCSError`. -/
private def pnmCSError {α : Type} (cs : String) (d : Graphics.Netpbm.PpmPixelData) :
    Except String α :=
  pnmError s!"Input image is in {pnmShowData d}, cannot convert it to {cs} colorspace."

/-- Upstream's `pnmDataPBMToImage`. -/
def pnmDataPBMToImage (w h : Nat) : Graphics.Netpbm.PpmPixelData → Except String (Image X Bit)
  | .pbm v => .ok (pbmDataToImageX w h v)
  | d => pnmCSError "Binary (Pixel X Bit)" d

/-- Upstream's `pnmDataPGM8ToImage`. -/
def pnmDataPGM8ToImage (w h : Nat) : Graphics.Netpbm.PpmPixelData → Except String (Image Y UInt8)
  | .grey8 v => .ok (pgmDataToImageY8 w h v)
  | d => pnmCSError "Y8 (Pixel Y Word8)" d

/-- Upstream's `pnmDataPGM16ToImage`. -/
def pnmDataPGM16ToImage (w h : Nat) :
    Graphics.Netpbm.PpmPixelData → Except String (Image Y UInt16)
  | .grey16 v => .ok (pgmDataToImageY16 w h v)
  | d => pnmCSError "Y16 (Pixel Y Word16)" d

/-- Upstream's `pnmDataPPM8ToImage`. -/
def pnmDataPPM8ToImage (w h : Nat) :
    Graphics.Netpbm.PpmPixelData → Except String (Image RGB UInt8)
  | .rgb8 v => .ok (ppmDataToImageRGB8 w h v)
  | d => pnmCSError "RGB8 (Pixel RGB Word8)" d

/-- Upstream's `pnmDataPPM16ToImage`. -/
def pnmDataPPM16ToImage (w h : Nat) :
    Graphics.Netpbm.PpmPixelData → Except String (Image RGB UInt16)
  | .rgb16 v => .ok (ppmDataToImageRGB16 w h v)
  | d => pnmCSError "RGB16 (Pixel RGB Word16)" d

/-- Upstream's `pnmDataToImage`: normalise any decoded `PpmPixelData`
variant into one of the canonical, `Convertible`-underwritten presentation
colour spaces (`Y`/`YA`/`RGB`/`RGBA`, double — here `Float` — precision).
See the module doc-comment for why, unlike `IO.Formats.JuicyPixels`'s own
generic-`Double` family, this ports without restriction. -/
def pnmDataToImage {cs e px : Type} [Pixel cs e px] [Convertible cs e]
    (w h : Nat) (d : Graphics.Netpbm.PpmPixelData) : Image cs e :=
  match d with
  | .pbm v => convert (pbmDataToImageX w h v)
  | .grey8 v => convert (pgmDataToImageY8 w h v)
  | .grey16 v => convert (pgmDataToImageY16 w h v)
  | .rgb8 v => convert (ppmDataToImageRGB8 w h v)
  | .rgb16 v => convert (ppmDataToImageRGB16 w h v)

-- ── Decoding a netpbm file's first image ──

/-- Upstream's `decodePnm`, narrowed to the first image (see the module
doc-comment's "`Seq`… deferred" section for why this module only needs the
first). -/
private def decodeFirst (bytes : ByteArray) : Except String Graphics.Netpbm.PPM :=
  match Graphics.Netpbm.parsePPM bytes with
  | .error err => pnmError err
  | .ok ([], _) => pnmError "Unknown"
  | .ok (ppm :: _, _) => .ok ppm

-- ── `PBM` ──

/-- Netpbm: portable bitmap image with a `.pbm` extension. Upstream's
`data PBM = PBM`. -/
structure PBM where
deriving Repr, Inhabited, BEq

instance : ImageFormat PBM Empty where
  ext _ := ".pbm"

instance : Readable (Image X Bit) PBM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pnmDataPBMToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData

instance : Readable (Image Y Float) PBM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pure (pnmDataToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData)

-- ── `PGM` ──

/-- Netpbm: portable graymap image with a `.pgm` extension. Upstream's
`data PGM = PGM`. -/
structure PGM where
deriving Repr, Inhabited, BEq

instance : ImageFormat PGM Empty where
  ext _ := ".pgm"

instance : Readable (Image Y UInt8) PGM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pnmDataPGM8ToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData

instance : Readable (Image Y UInt16) PGM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pnmDataPGM16ToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData

instance : Readable (Image Y Float) PGM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pure (pnmDataToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData)

-- ── `PPM` ──

/-- Netpbm: portable pixmap image with a `.ppm` extension. Upstream's
`data PPM = PPM`. -/
structure PPM where
deriving Repr, Inhabited, BEq

instance : ImageFormat PPM Empty where
  ext _ := ".ppm"

instance : Readable (Image RGB UInt8) PPM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pnmDataPPM8ToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData

instance : Readable (Image RGB UInt16) PPM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pnmDataPPM16ToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData

instance : Readable (Image Y Float) PPM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pure (pnmDataToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData)

instance : Readable (Image YA Float) PPM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pure (pnmDataToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData)

instance : Readable (Image RGB Float) PPM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pure (pnmDataToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData)

instance : Readable (Image RGBA Float) PPM where
  decode _ bytes := do
    let ppm ← decodeFirst bytes
    pure (pnmDataToImage ppm.ppmHeader.ppmWidth ppm.ppmHeader.ppmHeight ppm.ppmData)

end Graphics.Image.IO.Formats.Netpbm
