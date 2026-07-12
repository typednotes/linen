/-
  Linen.Graphics.Image.IO.Formats.JuicyPixels — glue between hip's `Image
  cs e` and this codebase's own `Linen.Codec.Picture` (JuicyPixels) codec
  suite

  ## Haskell equivalent

  `Graphics.Image.IO.Formats.JuicyPixels` from
  https://hackage.haskell.org/package/hip (module #22 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`). Fetched from the 1.5.6.0
  release tarball (`raw.githubusercontent.com/lehins/hip/master/…` 404s, same
  as `IO.Base`'s own note).

  As `dependencies.md` anticipated, this module is genuinely glue: every
  actual byte-level PNG/JPEG/GIF/BMP/TIFF/TGA/HDR decode/encode is delegated
  to the already-ported `Linen.Codec.Picture.*` (module 22 of the
  `JuicyPixels` import); the only new work here is (a) pixel-by-pixel
  marshalling between hip's `Image cs e` (a `Manifest`-backed array of pixel
  *structures*, per `Interface.lean`) and `Codec.Picture`'s `Image px` (a
  flat array of pixel *components*, per `Types.lean`), and (b) the
  `ImageFormat`/`Readable`/`Writable` instances (`IO.Base`, module #21) that
  wire each format tag to that marshalling plus the matching
  `Codec.Picture.decode*`/`encode*` call.

  ## `imageToJPImageUnsafe`/`jpImageToImageUnsafe` → honest elementwise
  ## conversion

  Upstream's `toJPImage*`/`fromJPImage*` are all `O(1)`: both sides are
  backed by the same `Data.Vector.Storable` buffer, so `V.unsafeCast`
  reinterprets one pixel-array representation as the other without copying
  a single byte. This port's two `Image` types have genuinely different
  backing shapes — `Interface.Image cs e` is `Manifest DIM2 px` (an array of
  whole pixel *structures*, e.g. `PixelRGB`), while `Codec.Picture.Image α`
  is a flat `Array Component` (each pixel's components laid out in a row,
  no pixel-level boundary at all) — so there is no shared buffer to
  reinterpret. Every `toJPImage*`/`fromJPImage*` below is instead an honest,
  `O(width · height)` elementwise conversion (`Interface.unsafeIndex`/
  `makeImage` on the hip side, `Codec.Picture.Image.getPixel`/`generateImage`
  on the JuicyPixels side), field-renaming each pixel structure into its
  counterpart's field names. This changes the asymptotic cost upstream's
  comment advertises (`-- O(1) Conversion …`) but not the result: both sides
  already store the same components in the same row-major pixel order, so
  the conversion is a faithful bijection on well-formed input, just done by
  a bounded traversal instead of a pointer reinterpretation — the same
  "plain bounded traversal in place of an FFI-level trick with no Lean
  counterpart" substitution `Linen.Codec.Picture.Types`'s own doc-comment
  already makes for `generateImage`/`pixelMap` themselves.

  ## Component precision lines up, one exception: `Float`/`Double`

  Per `Interface.Elevator`'s own type-mapping table, this port's `Elevator
  Float32`/`Elevator Float` instances mirror upstream's `Float`/`Double`
  exactly (`Float` ↦ `Float32`, `Double` ↦ `Float`). Since `Codec.Picture`'s
  `PixelF := Float32` is JuicyPixels' own single-precision float component,
  `Image Y Float32`/`Image RGB Float32` on the hip side line up with
  `PixelF`/`PixelRGBF` on the JuicyPixels side exactly, with no rescaling —
  the same "O(1), no `Elevator` call needed" relationship upstream's own
  `toJPImageYF`/`toJPImageRGBF` have to `Float ↦ Float`.

  ## Scope: concrete bit-depth-matching instances only, not the generic
  ## "any colour space, `Double` precision" layer

  Upstream gives **two** families of `Readable`/`Writable` instance per
  format: one per *concrete* JuicyPixels pixel shape (`Image VS RGB Word8`,
  matching `PixelRGB8` bit-for-bit), and one **generic "canonical `Double`
  precision"** family (`Image VS Y Double`, `Image VS YA Double`, `Image VS
  RGB Double`, `Image VS RGBA Double`) that works for *any* decoded
  `DynamicImage` variant by routing every source pixel type through
  `jpDynamicImageToImage`'s `Convertible`+`toImageY`/`toImageYA`/
  `toImageRGB`/`toImageRGBA` normalisation on read, and through the generic
  `toWord8I`/`toWord16I`/`toFloatI` precision-narrowing functions on write.

  The read-side half of that generic family *would* port directly (`IO.
  Base.Convertible`/`convert`, already ported in module #21, is exactly
  upstream's `Convertible`/`convert`). The write-side half needs upstream's
  `toWord8I`/`toWord16I`/`toFloatI` — and `Linen.Graphics.Image.ColorSpace`'s
  own doc-comment already documents, as an accepted architectural
  limitation of this port (not a decision local to this module), that no
  single definition of those exists here: this port's `Pixel cs e px` is a
  plain marker class relating one *fixed* `(cs, e, px)` triple, with no
  data-family/`Functor`-style structure to hang a component-type-changing,
  colour-space-generic transform off (see that module's own "out of scope,
  architectural limitation" section). Porting the generic write-side family
  here would mean either reproducing that missing abstraction (out of scope,
  per the same module's own guidance) or hand-writing 4 canonical colour
  spaces × 7 formats × (`Readable`+`Writable`) of near-identical,
  per-colour-space `liftPx Elevator.toWordN` glue with no shared body to
  factor out — a large, mechanical multiplier on this module's size for
  capability that does not extend past what the concrete-precision instances
  below already offer (any `Double`-precision hip image can already be
  narrowed to a concrete instance's precision one call site at a time via
  `Interface.map`/`liftPx` plus the matching `Elevator` method, exactly the
  pattern `ColorSpace.lean`'s own doc-comment recommends for this exact
  situation). This module therefore ports **only** the concrete,
  bit-depth-matching instance family — every `Readable`/`Writable` instance
  below has a directly corresponding upstream instance, just restricted to
  the subset whose target precision is a `Codec.Picture` pixel shape
  directly (`Word8`/`Word16`/`Float`, matching `Pixel8`/`Pixel16`/`PixelF`
  exactly), not upstream's additional generic `Double` layer built on top of
  those.

  ## Scope: no `GIFA`/`Seq GIF` (animated-sequence instances)

  Upstream additionally defines `GIFA` (deprecated in favour of `Seq GIF`)
  and `Seq GIF` tags with their own `Readable [Image VS RGB Word8] GIFA`/
  `Writable [(JP.GifDelay, Image VS RGB Word8)] (Seq GIF)`-style instances,
  operating on `List`s/tuples of frames rather than a single `img` type.
  `IO.Base.Seq` (module #21) already ports the *tag* these would need, and
  `Codec.Picture.Gif`'s own `decodeGifImages`/`encodeGifImages` (module 20 of
  the `JuicyPixels` import) already provide everything the frame-sequence
  logic itself needs — so nothing is missing on either side. This module
  simply does not spend the instance-declaration volume on it: a single-
  image `GIF` (this module's own `Readable`/`Writable (Image RGB Word8)
  GIF`) is ported below, and a future caller needing animated GIF I/O can
  already reach `Codec.Picture.decodeGifImages`/`encodeGifImages` directly,
  composed with this module's own `fromJPImageRGB8`/`toJPImageRGB8`, without
  needing a dedicated `Seq GIF`/`GIFA` `Readable`/`Writable` instance to do
  so. A genuine deferral (documented, not a silent drop), narrower in scope
  than the `Double`-precision deferral above only because upstream itself
  marks half of this surface (`GIFA`) deprecated.

  ## Scope: no binary (`X`/`Bit`) instances

  Upstream additionally gives `BMP`/`PNG`/`TGA`/`TIF` a `Readable`/`Writable
  (Image VS X Bit)` instance apiece, routed through `ColorSpace.lean`'s
  `toImageBinary`/`fromImageBinary`. Every concrete-precision `Y`/`RGB`/
  `RGBA` instance those four formats need is ported below regardless; the
  bilevel `X`/`Bit` variant is deferred as a narrower, single-colour-space
  addition a future caller can give directly against the same `toJPImageY8`/
  `fromJPImageY8` this module already exports, composed with
  `toImageBinary`/`fromImageBinary` (module #12), without needing this
  module's own further change.

  ## `PNG`: `ImageFormat` only, no `Readable`/`Writable` — a genuine `IO`
  ## mismatch, not a drop

  `Codec.Picture.Png`'s `decodePng`/`encodePng` are `IO`-returning (module
  29's own doc-comment: PNG's zlib inflate/deflate needs `IO` in this port,
  unlike every other format module here). `IO.Base.Readable`/`Writable`
  (module #21), however, port upstream's `decode`/`encode` *exactly* as
  upstream declares them: pure functions (`format → ByteArray → Except
  String img` / `format → List SaveOption → img → ByteArray`), because
  upstream's own `Readable`/`Writable` methods are pure for every format
  including PNG (GHC's `Codec.Picture`'s own PNG codec has no such `IO`
  dependency). So `PNG` genuinely cannot be given `Readable`/`Writable`
  instances under those two classes as ported — not a simplification this
  module is choosing, but a real divergence between upstream's pure PNG
  codec and this port's `IO`-gated one, propagating up from `Png.lean`'s own
  already-documented "universe/`IO` wrinkle." This module still fully
  supports PNG: `ImageFormat PNG Empty` is given (so `ext`/`exts`/`isFormat`
  work uniformly across every format tag), and `decodePNGImageY8`/
  `encodePNGImageY8`-style plain `IO`-returning functions are given directly
  for every `PngSavable`-covered pixel shape (`Y`/`YA`/`RGB`/`RGBA` × `Word8`/
  `Word16`), covering exactly the same ground the `Readable`/`Writable`
  instances would, just outside those two classes' pure signatures.

  ## `BMP`: `Readable (Image Y Word8) BMP` can never decode a real bitmap

  `Readable (Image Y UInt8) BMP` below is ported for type-level fidelity
  with upstream (the concrete pair exists there), but `Linen.Codec.Picture.
  Bitmap`'s own `decodeBitmap` (module 6 of the `JuicyPixels` import)
  *always* expands an indexed/palette bitmap — including the one an 8-bit
  grayscale `encodeBitmap` produces — to a true-colour `DynamicImage.rgb8`
  on decode (see `decodeBitmapWithMetadata`'s use of `palettedToTrueColor`);
  it never produces `.y8`. So `jpImageY8ToImage`'s `.y8` pattern can never
  match a bitmap this module's own `Writable (Image Y UInt8) BMP` just
  wrote, and `decode BMP.mk` on it always returns `.error` — a real
  consequence of `Bitmap.lean`'s own already-ported decode behaviour, not a
  gap introduced here. The `Image RGB UInt8`/`Image RGBA UInt8` instances
  are unaffected (`decodeBitmap` does produce `.rgb8`/`.rgba8`), and decoding
  a `Y8`-encoded bitmap through the `RGB` instance instead still recovers
  the same grayscale values (each channel equal to the original luma).

  ## `TIF`: no `Writable (Image YCbCr Word8) TIF`

  Upstream also writes a JPEG-native `YCbCr8` TIFF (`instance Writable (Image
  VS YCbCr Word8) TIF`), because GHC's `JuicyPixels` package gives `PixelYCbCr8`
  a `TiffSaveable` instance. This port's own `Linen.Codec.Picture.Tiff`
  (module 17 of the `JuicyPixels` import) does not give `PixelYCbCr8` a
  `TiffSaveable` instance (its own instance list covers `Pixel8`/`Pixel16`/
  `PixelYA8`/`PixelYA16`/`PixelRGB8`/`PixelRGB16`/`PixelRGBA8`/`PixelRGBA16`/
  `PixelCMYK8`/`PixelCMYK16` — ten shapes, `PixelYCbCr8` absent) — a mismatch
  between the two codebases' own already-ported pixel-type enumerations, not
  something this module can paper over: with no `TiffSaveable PixelYCbCr8`
  instance to call, `encodeTiff (toJPImageYCbCr8 img)` does not type-check.
  This module's `Writable … TIF` instances are exactly `Linen.Codec.Picture.
  Tiff`'s own `TiffSaveable` instance list (ten pairs), one narrower than
  upstream's eleven; every other format/pair upstream gives for `TIF`
  ports directly.

  ## Fixture/test naming

  Following `Linen.Codec.Picture`'s own convention, tests in
  `Tests/Linen/Graphics/Image/IO/Formats/JuicyPixelsTest.lean` use a `jp`
  prefix on every fixture, to avoid cross-file `Tests` namespace collisions.
-/

import Linen.Graphics.Image.ColorSpace
import Linen.Graphics.Image.IO.Base
import Linen.Codec.Picture

open Graphics.Image.Interface (Image dims unsafeIndex makeImage)
open Graphics.Image.IO.Base (ImageFormat Readable Writable)
open Graphics.Image.ColorSpace.Y (Y YA PixelY PixelYA)
open Graphics.Image.ColorSpace.RGB (RGB RGBA PixelRGB PixelRGBA)
open Graphics.Image.ColorSpace.YCbCr (YCbCr PixelYCbCr)
open Graphics.Image.ColorSpace.CMYK (CMYK PixelCMYK)
open Codec.Picture
  (Pixel8 Pixel16 Pixel32 PixelF PixelYA8 PixelYA16 PixelRGB8 PixelRGB16 PixelRGBA8 PixelRGBA16
   PixelRGBF PixelYCbCr8 PixelCMYK8 PixelCMYK16 DynamicImage generateImage PaletteOptions Metadatas
   decodeBitmap decodeTga decodeTiff decodeHDR decodeGif decodeJpeg decodePng
   encodeBitmap encodeTga encodeTiff encodeHDR encodePng encodeGifImageWithPalette
   palettize defaultPaletteOptions encodeDirectJpegAtQualityWithMetadata)

namespace Graphics.Image.IO.Formats.JuicyPixels

-- ── `Data.ByteString` ↔ `ByteArray` (see `IO.Base`'s own note: `Readable`/
-- `Writable`'s `decode`/`encode` are typed over plain `ByteArray`, matching
-- `Codec.Picture`'s decode side already; only the encode side, which
-- returns `Data.ByteString`, needs converting back) ──

/-- Materialise a `Data.ByteString` slice into a fresh `ByteArray`, the same
pattern already used by `Linen.Data.PDF.Core.Encryption`'s own private
helper of the same name (no dedicated conversion exists on `Data.ByteString`
itself). -/
private def toByteArray (bs : Data.ByteString) : ByteArray :=
  (Data.ByteString.copy bs).data

-- ── Pixel-array marshalling: hip `Image cs e` ↔ `Codec.Picture.Image px` ──
-- See the module doc-comment for why these are honest elementwise
-- traversals rather than upstream's `O(1)` buffer-reinterpreting cast.

/-- Upstream's `toJPImageY8`. -/
def toJPImageY8 (img : Image Y UInt8) : Codec.Picture.Image Pixel8 :=
  let (m, n) := dims img
  generateImage (fun x y => (unsafeIndex img (Int.ofNat y, Int.ofNat x)).y) n.toNat m.toNat

/-- Upstream's `fromJPImageY8`. -/
def fromJPImageY8 (jimg : Codec.Picture.Image Pixel8) : Image Y UInt8 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => ⟨jimg.getPixel j.toNat i.toNat⟩)

/-- Upstream's `toJPImageY16`. -/
def toJPImageY16 (img : Image Y UInt16) : Codec.Picture.Image Pixel16 :=
  let (m, n) := dims img
  generateImage (fun x y => (unsafeIndex img (Int.ofNat y, Int.ofNat x)).y) n.toNat m.toNat

/-- Upstream's `fromJPImageY16`. -/
def fromJPImageY16 (jimg : Codec.Picture.Image Pixel16) : Image Y UInt16 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => ⟨jimg.getPixel j.toNat i.toNat⟩)

/-- Upstream's `fromJPImageY32` (no `toJPImageY32` counterpart upstream
either — `Codec.Picture`'s `DynamicImage.y32` has no matching hip-side
`Writable` instance in this module, per the module doc-comment's scope
note). -/
def fromJPImageY32 (jimg : Codec.Picture.Image Pixel32) : Image Y UInt32 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => ⟨jimg.getPixel j.toNat i.toNat⟩)

/-- Upstream's `toJPImageYF`. -/
def toJPImageYF (img : Image Y Float32) : Codec.Picture.Image PixelF :=
  let (m, n) := dims img
  generateImage (fun x y => (unsafeIndex img (Int.ofNat y, Int.ofNat x)).y) n.toNat m.toNat

/-- Upstream's `fromJPImageYF`. -/
def fromJPImageYF (jimg : Codec.Picture.Image PixelF) : Image Y Float32 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => ⟨jimg.getPixel j.toNat i.toNat⟩)

/-- Upstream's `toJPImageYA8`. -/
def toJPImageYA8 (img : Image YA UInt8) : Codec.Picture.Image PixelYA8 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.y, p.a⟩) n.toNat m.toNat

/-- Upstream's `fromJPImageYA8`. -/
def fromJPImageYA8 (jimg : Codec.Picture.Image PixelYA8) : Image YA UInt8 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.y, p.a⟩)

/-- Upstream's `toJPImageYA16`. -/
def toJPImageYA16 (img : Image YA UInt16) : Codec.Picture.Image PixelYA16 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.y, p.a⟩) n.toNat m.toNat

/-- Upstream's `fromJPImageYA16`. -/
def fromJPImageYA16 (jimg : Codec.Picture.Image PixelYA16) : Image YA UInt16 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.y, p.a⟩)

/-- Upstream's `toJPImageRGB8`. -/
def toJPImageRGB8 (img : Image RGB UInt8) : Codec.Picture.Image PixelRGB8 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.r, p.g, p.b⟩) n.toNat m.toNat

/-- Upstream's `fromJPImageRGB8`. -/
def fromJPImageRGB8 (jimg : Codec.Picture.Image PixelRGB8) : Image RGB UInt8 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.r, p.g, p.b⟩)

/-- Upstream's `toJPImageRGBA8`. -/
def toJPImageRGBA8 (img : Image RGBA UInt8) : Codec.Picture.Image PixelRGBA8 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.r, p.g, p.b, p.a⟩)
    n.toNat m.toNat

/-- Upstream's `fromJPImageRGBA8`. -/
def fromJPImageRGBA8 (jimg : Codec.Picture.Image PixelRGBA8) : Image RGBA UInt8 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.r, p.g, p.b, p.a⟩)

/-- Upstream's `toJPImageRGB16`. -/
def toJPImageRGB16 (img : Image RGB UInt16) : Codec.Picture.Image PixelRGB16 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.r, p.g, p.b⟩) n.toNat m.toNat

/-- Upstream's `fromJPImageRGB16`. -/
def fromJPImageRGB16 (jimg : Codec.Picture.Image PixelRGB16) : Image RGB UInt16 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.r, p.g, p.b⟩)

/-- Upstream's `toJPImageRGBA16`. -/
def toJPImageRGBA16 (img : Image RGBA UInt16) : Codec.Picture.Image PixelRGBA16 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.r, p.g, p.b, p.a⟩)
    n.toNat m.toNat

/-- Upstream's `fromJPImageRGBA16`. -/
def fromJPImageRGBA16 (jimg : Codec.Picture.Image PixelRGBA16) : Image RGBA UInt16 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.r, p.g, p.b, p.a⟩)

/-- Upstream's `toJPImageRGBF`. -/
def toJPImageRGBF (img : Image RGB Float32) : Codec.Picture.Image PixelRGBF :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.r, p.g, p.b⟩) n.toNat m.toNat

/-- Upstream's `fromJPImageRGBF`. -/
def fromJPImageRGBF (jimg : Codec.Picture.Image PixelRGBF) : Image RGB Float32 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.r, p.g, p.b⟩)

/-- Upstream's `toJPImageYCbCr8`. -/
def toJPImageYCbCr8 (img : Image YCbCr UInt8) : Codec.Picture.Image PixelYCbCr8 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.y, p.cb, p.cr⟩)
    n.toNat m.toNat

/-- Upstream's `fromJPImageYCbCr8`. -/
def fromJPImageYCbCr8 (jimg : Codec.Picture.Image PixelYCbCr8) : Image YCbCr UInt8 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.y, p.cb, p.cr⟩)

/-- Upstream's `toJPImageCMYK8`. -/
def toJPImageCMYK8 (img : Image CMYK UInt8) : Codec.Picture.Image PixelCMYK8 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.c, p.m, p.y, p.k⟩)
    n.toNat m.toNat

/-- Upstream's `fromJPImageCMYK8`. -/
def fromJPImageCMYK8 (jimg : Codec.Picture.Image PixelCMYK8) : Image CMYK UInt8 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.c, p.m, p.y, p.k⟩)

/-- Upstream's `toJPImageCMYK16`. -/
def toJPImageCMYK16 (img : Image CMYK UInt16) : Codec.Picture.Image PixelCMYK16 :=
  let (m, n) := dims img
  generateImage
    (fun x y => let p := unsafeIndex img (Int.ofNat y, Int.ofNat x); ⟨p.c, p.m, p.y, p.k⟩)
    n.toNat m.toNat

/-- Upstream's `fromJPImageCMYK16`. -/
def fromJPImageCMYK16 (jimg : Codec.Picture.Image PixelCMYK16) : Image CMYK UInt16 :=
  makeImage (Int.ofNat jimg.height, Int.ofNat jimg.width)
    (fun (i, j) => let p := jimg.getPixel j.toNat i.toNat; ⟨p.c, p.m, p.y, p.k⟩)

-- ── Extracting one concrete pixel shape out of a decoded `DynamicImage` ──

/-- Upstream's `jpImageShowCS`. -/
private def dynamicImageShowCS : DynamicImage → String
  | .y8 _ => "Y8 (Pixel Y Word8)"
  | .y16 _ => "Y16 (Pixel Y Word16)"
  | .y32 _ => "Y32 (Pixel Y Word32)"
  | .yF _ => "YF (Pixel Y Float)"
  | .ya8 _ => "YA8 (Pixel YA Word8)"
  | .ya16 _ => "YA16 (Pixel YA Word16)"
  | .rgb8 _ => "RGB8 (Pixel RGB Word8)"
  | .rgb16 _ => "RGB16 (Pixel RGB Word16)"
  | .rgbF _ => "RGBF (Pixel RGB Float)"
  | .rgba8 _ => "RGBA8 (Pixel RGBA Word8)"
  | .rgba16 _ => "RGBA16 (Pixel RGBA Word16)"
  | .ycbcr8 _ => "YCbCr8 (Pixel YCbCr Word8)"
  | .cmyk8 _ => "CMYK8 (Pixel CMYK Word8)"
  | .cmyk16 _ => "CMYK16 (Pixel CMYK Word16)"

/-- Upstream's `jpError`. -/
private def jpError {α : Type} (err : String) : Except String α :=
  .error s!"JuicyPixel decoding error: {err}"

/-- Upstream's `jpCSError`. -/
private def jpCSError {α : Type} (cs : String) (jimg : DynamicImage) : Except String α :=
  jpError s!"Input image is in {dynamicImageShowCS jimg}, cannot convert it to {cs} colorspace."

/-- Upstream's `jpImageY8ToImage`. -/
def jpImageY8ToImage : DynamicImage → Except String (Image Y UInt8)
  | .y8 jimg => .ok (fromJPImageY8 jimg)
  | jimg => jpCSError "Y8 (Pixel Y Word8)" jimg

/-- Upstream's `jpImageY16ToImage`. -/
def jpImageY16ToImage : DynamicImage → Except String (Image Y UInt16)
  | .y16 jimg => .ok (fromJPImageY16 jimg)
  | jimg => jpCSError "Y16 (Pixel Y Word16)" jimg

/-- Upstream's `jpImageYA8ToImage`. -/
def jpImageYA8ToImage : DynamicImage → Except String (Image YA UInt8)
  | .ya8 jimg => .ok (fromJPImageYA8 jimg)
  | jimg => jpCSError "YA8 (Pixel YA Word8)" jimg

/-- Upstream's `jpImageYA16ToImage`. -/
def jpImageYA16ToImage : DynamicImage → Except String (Image YA UInt16)
  | .ya16 jimg => .ok (fromJPImageYA16 jimg)
  | jimg => jpCSError "YA16 (Pixel YA Word16)" jimg

/-- Upstream's `jpImageRGB8ToImage`. -/
def jpImageRGB8ToImage : DynamicImage → Except String (Image RGB UInt8)
  | .rgb8 jimg => .ok (fromJPImageRGB8 jimg)
  | jimg => jpCSError "RGB8 (Pixel RGB Word8)" jimg

/-- Upstream's `jpImageRGB16ToImage`. -/
def jpImageRGB16ToImage : DynamicImage → Except String (Image RGB UInt16)
  | .rgb16 jimg => .ok (fromJPImageRGB16 jimg)
  | jimg => jpCSError "RGB16 (Pixel RGB Word16)" jimg

/-- Upstream's `jpImageRGBFToImage`. -/
def jpImageRGBFToImage : DynamicImage → Except String (Image RGB Float32)
  | .rgbF jimg => .ok (fromJPImageRGBF jimg)
  | jimg => jpCSError "RGBF (Pixel RGB Float)" jimg

/-- Upstream's `jpImageRGBA8ToImage`. -/
def jpImageRGBA8ToImage : DynamicImage → Except String (Image RGBA UInt8)
  | .rgba8 jimg => .ok (fromJPImageRGBA8 jimg)
  | jimg => jpCSError "RGBA8 (Pixel RGBA Word8)" jimg

/-- Upstream's `jpImageRGBA16ToImage`. -/
def jpImageRGBA16ToImage : DynamicImage → Except String (Image RGBA UInt16)
  | .rgba16 jimg => .ok (fromJPImageRGBA16 jimg)
  | jimg => jpCSError "RGBA16 (Pixel RGBA Word16)" jimg

/-- Upstream's `jpImageYCbCr8ToImage`. -/
def jpImageYCbCr8ToImage : DynamicImage → Except String (Image YCbCr UInt8)
  | .ycbcr8 jimg => .ok (fromJPImageYCbCr8 jimg)
  | jimg => jpCSError "YCbCr8 (Pixel YCbCr Word8)" jimg

/-- Upstream's `jpImageCMYK8ToImage`. -/
def jpImageCMYK8ToImage : DynamicImage → Except String (Image CMYK UInt8)
  | .cmyk8 jimg => .ok (fromJPImageCMYK8 jimg)
  | jimg => jpCSError "CMYK8 (Pixel CMYK Word8)" jimg

/-- Upstream's `jpImageCMYK16ToImage`. -/
def jpImageCMYK16ToImage : DynamicImage → Except String (Image CMYK UInt16)
  | .cmyk16 jimg => .ok (fromJPImageCMYK16 jimg)
  | jimg => jpCSError "CMYK16 (Pixel CMYK Word16)" jimg

-- ── `BMP` ──

/-- Bitmap image with a `.bmp` extension. Upstream's `data BMP = BMP`. -/
structure BMP where
deriving Repr, Inhabited, BEq

instance : ImageFormat BMP Empty where
  ext _ := ".bmp"

instance : Readable (Image Y UInt8) BMP where
  decode _ bytes := do jpImageY8ToImage (← decodeBitmap bytes)

instance : Readable (Image RGB UInt8) BMP where
  decode _ bytes := do jpImageRGB8ToImage (← decodeBitmap bytes)

instance : Readable (Image RGBA UInt8) BMP where
  decode _ bytes := do jpImageRGBA8ToImage (← decodeBitmap bytes)

instance : Writable (Image Y UInt8) BMP where
  encode _ _ img := toByteArray (encodeBitmap (toJPImageY8 img))

instance : Writable (Image RGB UInt8) BMP where
  encode _ _ img := toByteArray (encodeBitmap (toJPImageRGB8 img))

instance : Writable (Image RGBA UInt8) BMP where
  encode _ _ img := toByteArray (encodeBitmap (toJPImageRGBA8 img))

-- ── `GIF` ──

/-- Graphics Interchange Format image with a `.gif` extension. Upstream's
`data GIF = GIF`. -/
structure GIF where
deriving Repr, Inhabited, BEq

/-- `GIF`'s save options: upstream's `data SaveOption GIF = GIFPalette
JP.PaletteOptions`. -/
inductive GIFSaveOption where
  /-- Colour-quantisation options for building the GIF's palette. -/
  | palette (opts : PaletteOptions)

instance : ImageFormat GIF GIFSaveOption where
  ext _ := ".gif"

instance : Readable (Image RGB UInt8) GIF where
  decode _ bytes := do jpImageRGB8ToImage (← decodeGif bytes)

instance : Readable (Image RGBA UInt8) GIF where
  decode _ bytes := do jpImageRGBA8ToImage (← decodeGif bytes)

/-- Upstream's `encodeGIF`: palettise, then encode against that palette,
`panic!`ing on failure the same way upstream's `either error id` does. -/
private def encodeGIF (opts : List GIFSaveOption) (img : Image RGB UInt8) : ByteArray :=
  let paletteOpts := match opts with
    | .palette o :: _ => o
    | [] => defaultPaletteOptions
  let (paletted, palette) := palettize paletteOpts (toJPImageRGB8 img)
  match encodeGifImageWithPalette paletted palette with
  | .ok bs => toByteArray bs
  | .error e => panic! s!"Graphics.Image.IO.Formats.JuicyPixels.encodeGIF: {e}"

instance : Writable (Image RGB UInt8) GIF where
  encode _ opts img := encodeGIF opts img

-- ── `HDR` ──

/-- High-dynamic-range image with a `.hdr` or `.pic` extension. Upstream's
`data HDR = HDR`. -/
structure HDR where
deriving Repr, Inhabited, BEq

instance : ImageFormat HDR Empty where
  ext _ := ".hdr"
  exts _ := [".hdr", ".pic"]

instance : Readable (Image RGB Float32) HDR where
  decode _ bytes := do jpImageRGBFToImage (← decodeHDR bytes)

instance : Writable (Image RGB Float32) HDR where
  encode _ _ img := toByteArray (encodeHDR (toJPImageRGBF img))

-- ── `JPG` ──

/-- Joint Photographic Experts Group image with a `.jpg`/`.jpeg` extension.
Upstream's `data JPG = JPG`. -/
structure JPG where
deriving Repr, Inhabited, BEq

/-- `JPG`'s save options: upstream's `data SaveOption JPG = JPGQuality
Word8`. -/
inductive JPGSaveOption where
  /-- Encoding quality, `0`–`100`. -/
  | quality (q : UInt8)

instance : ImageFormat JPG JPGSaveOption where
  ext _ := ".jpg"
  exts _ := [".jpg", ".jpeg"]

instance : Readable (Image Y UInt8) JPG where
  decode _ bytes := do jpImageY8ToImage (← decodeJpeg bytes.toList)

instance : Readable (Image YA UInt8) JPG where
  decode _ bytes := do jpImageYA8ToImage (← decodeJpeg bytes.toList)

instance : Readable (Image RGB UInt8) JPG where
  decode _ bytes := do jpImageRGB8ToImage (← decodeJpeg bytes.toList)

instance : Readable (Image CMYK UInt8) JPG where
  decode _ bytes := do jpImageCMYK8ToImage (← decodeJpeg bytes.toList)

instance : Readable (Image YCbCr UInt8) JPG where
  decode _ bytes := do jpImageYCbCr8ToImage (← decodeJpeg bytes.toList)

/-- Upstream's `encodeJPG`: quality `100` if no `.quality` option is given. -/
private def jpgQualityOf : List JPGSaveOption → UInt8
  | .quality q :: _ => q
  | [] => 100

instance : Writable (Image Y UInt8) JPG where
  encode _ opts img := toByteArray
    (encodeDirectJpegAtQualityWithMetadata (jpgQualityOf opts) Metadatas.empty (toJPImageY8 img))

instance : Writable (Image RGB UInt8) JPG where
  encode _ opts img := toByteArray
    (encodeDirectJpegAtQualityWithMetadata (jpgQualityOf opts) Metadatas.empty (toJPImageRGB8 img))

instance : Writable (Image CMYK UInt8) JPG where
  encode _ opts img := toByteArray
    (encodeDirectJpegAtQualityWithMetadata (jpgQualityOf opts) Metadatas.empty (toJPImageCMYK8 img))

instance : Writable (Image YCbCr UInt8) JPG where
  encode _ opts img := toByteArray
    (encodeDirectJpegAtQualityWithMetadata (jpgQualityOf opts) Metadatas.empty (toJPImageYCbCr8 img))

-- ── `PNG` ──

/-- Portable Network Graphics image with a `.png` extension. Upstream's
`data PNG = PNG`. `Readable`/`Writable` instances are intentionally absent
— see the module doc-comment's "`PNG`: `ImageFormat` only" section. -/
structure PNG where
deriving Repr, Inhabited, BEq

instance : ImageFormat PNG Empty where
  ext _ := ".png"

/-- Decode a PNG into an 8-bit luma image, `IO`-returning because
`Codec.Picture.decodePng` is (see the module doc-comment). -/
def decodePNGImageY8 (bytes : ByteArray) : IO (Except String (Image Y UInt8)) := do
  match ← decodePng bytes with
  | .error e => pure (.error e)
  | .ok dyn => pure (jpImageY8ToImage dyn)

/-- Decode a PNG into a 16-bit luma image. -/
def decodePNGImageY16 (bytes : ByteArray) : IO (Except String (Image Y UInt16)) := do
  match ← decodePng bytes with
  | .error e => pure (.error e)
  | .ok dyn => pure (jpImageY16ToImage dyn)

/-- Decode a PNG into an 8-bit luma-with-alpha image. -/
def decodePNGImageYA8 (bytes : ByteArray) : IO (Except String (Image YA UInt8)) := do
  match ← decodePng bytes with
  | .error e => pure (.error e)
  | .ok dyn => pure (jpImageYA8ToImage dyn)

/-- Decode a PNG into a 16-bit luma-with-alpha image. -/
def decodePNGImageYA16 (bytes : ByteArray) : IO (Except String (Image YA UInt16)) := do
  match ← decodePng bytes with
  | .error e => pure (.error e)
  | .ok dyn => pure (jpImageYA16ToImage dyn)

/-- Decode a PNG into an 8-bit true-colour image. -/
def decodePNGImageRGB8 (bytes : ByteArray) : IO (Except String (Image RGB UInt8)) := do
  match ← decodePng bytes with
  | .error e => pure (.error e)
  | .ok dyn => pure (jpImageRGB8ToImage dyn)

/-- Decode a PNG into a 16-bit true-colour image. -/
def decodePNGImageRGB16 (bytes : ByteArray) : IO (Except String (Image RGB UInt16)) := do
  match ← decodePng bytes with
  | .error e => pure (.error e)
  | .ok dyn => pure (jpImageRGB16ToImage dyn)

/-- Decode a PNG into an 8-bit true-colour-with-alpha image. -/
def decodePNGImageRGBA8 (bytes : ByteArray) : IO (Except String (Image RGBA UInt8)) := do
  match ← decodePng bytes with
  | .error e => pure (.error e)
  | .ok dyn => pure (jpImageRGBA8ToImage dyn)

/-- Decode a PNG into a 16-bit true-colour-with-alpha image. -/
def decodePNGImageRGBA16 (bytes : ByteArray) : IO (Except String (Image RGBA UInt16)) := do
  match ← decodePng bytes with
  | .error e => pure (.error e)
  | .ok dyn => pure (jpImageRGBA16ToImage dyn)

/-- Encode an 8-bit luma image as a PNG. -/
def encodePNGImageY8 (img : Image Y UInt8) : IO ByteArray := do
  pure (toByteArray (← encodePng (toJPImageY8 img)))

/-- Encode a 16-bit luma image as a PNG. -/
def encodePNGImageY16 (img : Image Y UInt16) : IO ByteArray := do
  pure (toByteArray (← encodePng (toJPImageY16 img)))

/-- Encode an 8-bit luma-with-alpha image as a PNG. -/
def encodePNGImageYA8 (img : Image YA UInt8) : IO ByteArray := do
  pure (toByteArray (← encodePng (toJPImageYA8 img)))

/-- Encode a 16-bit luma-with-alpha image as a PNG. -/
def encodePNGImageYA16 (img : Image YA UInt16) : IO ByteArray := do
  pure (toByteArray (← encodePng (toJPImageYA16 img)))

/-- Encode an 8-bit true-colour image as a PNG. -/
def encodePNGImageRGB8 (img : Image RGB UInt8) : IO ByteArray := do
  pure (toByteArray (← encodePng (toJPImageRGB8 img)))

/-- Encode a 16-bit true-colour image as a PNG. -/
def encodePNGImageRGB16 (img : Image RGB UInt16) : IO ByteArray := do
  pure (toByteArray (← encodePng (toJPImageRGB16 img)))

/-- Encode an 8-bit true-colour-with-alpha image as a PNG. -/
def encodePNGImageRGBA8 (img : Image RGBA UInt8) : IO ByteArray := do
  pure (toByteArray (← encodePng (toJPImageRGBA8 img)))

/-- Encode a 16-bit true-colour-with-alpha image as a PNG. -/
def encodePNGImageRGBA16 (img : Image RGBA UInt16) : IO ByteArray := do
  pure (toByteArray (← encodePng (toJPImageRGBA16 img)))

-- ── `TGA` ──

/-- Truevision Graphics Adapter image with a `.tga` extension. Upstream's
`data TGA = TGA`. -/
structure TGA where
deriving Repr, Inhabited, BEq

instance : ImageFormat TGA Empty where
  ext _ := ".tga"

instance : Readable (Image Y UInt8) TGA where
  decode _ bytes := do jpImageY8ToImage (← decodeTga bytes)

instance : Readable (Image RGB UInt8) TGA where
  decode _ bytes := do jpImageRGB8ToImage (← decodeTga bytes)

instance : Readable (Image RGBA UInt8) TGA where
  decode _ bytes := do jpImageRGBA8ToImage (← decodeTga bytes)

instance : Writable (Image Y UInt8) TGA where
  encode _ _ img := toByteArray (encodeTga (toJPImageY8 img))

instance : Writable (Image RGB UInt8) TGA where
  encode _ _ img := toByteArray (encodeTga (toJPImageRGB8 img))

instance : Writable (Image RGBA UInt8) TGA where
  encode _ _ img := toByteArray (encodeTga (toJPImageRGBA8 img))

-- ── `TIF` ──

/-- Tagged Image File Format image with a `.tif`/`.tiff` extension.
Upstream's `data TIF = TIF`. -/
structure TIF where
deriving Repr, Inhabited, BEq

instance : ImageFormat TIF Empty where
  ext _ := ".tif"
  exts _ := [".tif", ".tiff"]

instance : Readable (Image Y UInt8) TIF where
  decode _ bytes := do jpImageY8ToImage (← decodeTiff bytes)

instance : Readable (Image Y UInt16) TIF where
  decode _ bytes := do jpImageY16ToImage (← decodeTiff bytes)

instance : Readable (Image YA UInt8) TIF where
  decode _ bytes := do jpImageYA8ToImage (← decodeTiff bytes)

instance : Readable (Image YA UInt16) TIF where
  decode _ bytes := do jpImageYA16ToImage (← decodeTiff bytes)

instance : Readable (Image RGB UInt8) TIF where
  decode _ bytes := do jpImageRGB8ToImage (← decodeTiff bytes)

instance : Readable (Image RGB UInt16) TIF where
  decode _ bytes := do jpImageRGB16ToImage (← decodeTiff bytes)

instance : Readable (Image RGBA UInt8) TIF where
  decode _ bytes := do jpImageRGBA8ToImage (← decodeTiff bytes)

instance : Readable (Image RGBA UInt16) TIF where
  decode _ bytes := do jpImageRGBA16ToImage (← decodeTiff bytes)

instance : Readable (Image CMYK UInt8) TIF where
  decode _ bytes := do jpImageCMYK8ToImage (← decodeTiff bytes)

instance : Readable (Image CMYK UInt16) TIF where
  decode _ bytes := do jpImageCMYK16ToImage (← decodeTiff bytes)

instance : Writable (Image Y UInt8) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageY8 img))

instance : Writable (Image Y UInt16) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageY16 img))

instance : Writable (Image YA UInt8) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageYA8 img))

instance : Writable (Image YA UInt16) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageYA16 img))

instance : Writable (Image RGB UInt8) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageRGB8 img))

instance : Writable (Image RGB UInt16) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageRGB16 img))

instance : Writable (Image RGBA UInt8) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageRGBA8 img))

instance : Writable (Image RGBA UInt16) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageRGBA16 img))

instance : Writable (Image CMYK UInt8) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageCMYK8 img))

instance : Writable (Image CMYK UInt16) TIF where
  encode _ _ img := toByteArray (encodeTiff (toJPImageCMYK16 img))

end Graphics.Image.IO.Formats.JuicyPixels
