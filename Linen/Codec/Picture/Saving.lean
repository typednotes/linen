import Linen.Codec.Picture.Bitmap
import Linen.Codec.Picture.HDR
import Linen.Codec.Picture.Png
import Linen.Codec.Picture.Tiff
import Linen.Codec.Picture.Gif
import Linen.Codec.Picture.Jpg
import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Metadata

/-!
  Port of `Codec.Picture.Saving` from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 28 of 29): format-
  agnostic "save a `DynamicImage` as format `X`" dispatch, doing whatever
  pixel-type coercion each target format needs (colour-space conversion,
  8-, 16-, or 32-bit component narrowing, alpha dropping/adding, palette
  quantisation) exactly as upstream's `imageToJpg`/`imageToPng`/`imageToGif`/
  `imageToBitmap`/`imageToTiff`/`imageToRadiance` do, on top of modules 8
  (`Bitmap`), 10 (`HDR`), 14 (`Png`), 17 (`Tiff`), 20 (`Gif`), 27 (`Jpg`).

  ## Sum type dispatched over

  Every `imageTo*` function below pattern-matches on
  `Linen.Codec.Picture.Types.DynamicImage` (module 1's port of upstream's
  `DynamicImage`) — the same type every decoder in this package already
  returns (`decodeBitmap`, `decodePng`, `decodeGif`, `decodeTiff`,
  `decodeJpeg`, `decodeHDR`), so a value straight out of any of those
  decoders can be handed to any `imageTo*` function here unchanged, matching
  upstream's own "`imageTo*` is `decodeImage`'s inverse" framing.

  ## `IO` vs. pure, per target format

  Each `imageTo*` function's effect exactly matches its underlying
  format encoder's, per format:

  - `imageToPng : DynamicImage → IO Data.ByteString` — `Png.lean`'s
    `encodePng` is `IO`-returning (its deflate step goes through
    `Crypto.Zlib.compress`), so every conversion path that bottoms out in
    `encodePng` must run in `IO` too.
  - `imageToJpg`, `imageToBitmap`, `imageToTiff`, `imageToRadiance` are pure
    (`DynamicImage → Data.ByteString`) — `Jpg.lean`/`Bitmap.lean`/
    `Tiff.lean`/`HDR.lean`'s encoders are all pure, and (like upstream) none
    of these four ever fail: every `DynamicImage` variant has *some*
    admissible pixel coercion into the target format, so there is no
    `Except`/`Either` in the result, exactly matching upstream's
    `DynamicImage → L.ByteString` signatures for these four.
  - `imageToGif : DynamicImage → Except String Data.ByteString` — matches
    upstream's `DynamicImage → Either String L.ByteString`: `Gif.lean`'s
    `encodeGifImage`/`encodeGifImageWithPalette` are pure but
    `Except`-returning (a paletted image can be malformed), so the failure
    threads through here unchanged; nothing above `encodeGifImage*` in the
    conversion chain can itself fail.

  None of the six write to a file path — exactly as upstream's own
  `imageTo*` functions only ever produce a `L.ByteString`/`Either String
  L.ByteString`/`IO`-nothing-more-than-that; upstream's actual
  `saveJpgImage`/`saveTiffImage`/etc. *file*-writing wrappers live in
  `Codec.Picture` (module 29, not yet ported), not in `Saving.hs` itself.
  This module stops at producing bytes, leaving `IO.FS.writeBinFile`-style
  file writing to whatever calls it, matching that module boundary.

  ## Pixel-coercion helpers

  `Types.lean` (module 1) already provides `ColorConvertible.promotePixel`
  (lossless widening, e.g. grey → RGB or 8-bit → 16-bit),
  `ColorSpaceConvertible.convertPixel` (colour-space change, e.g. RGB ↔
  YCbCr ↔ CMYK) and `TransparentPixel.dropAlphaLayer` (discard alpha) —
  every coercion below reuses one of those three instead of re-deriving a
  formula, wherever an instance already exists. A handful of coercions
  upstream performs have no existing instance to reuse, exactly mirroring
  how upstream's own `Saving.hs` defines several *local* helpers
  (`componentToLDR`, `toStandardDef`, `greyScaleToStandardDef`, `from16to8`,
  `from32to8`, `from32to16`, `from16toFloat`) rather than adding instances to
  `Types.hs` for them:

  - **`componentToLDR`/`toStandardDef`/`greyScaleToStandardDef`** (tone-map a
    float component/pixel down to 8-bit) are ported directly, unchanged.
  - **`from16to8`/`from32to8`/`from32to16`** upstream are a single
    `PixelBaseComponent`-indexed combinator working on any pixel shape via a
    raw `Vector`-level `unsafeShiftR`/`map`. This port's `Types.lean` has no
    equivalent "same shape, narrower component" type-family combinator (see
    `Types.lean`'s own module doc-comment on why `Component` is an
    `outParam`, not a family other code can range over generically), so each
    needed shape gets its own small pixel-level function below (`y16to8`,
    `ya16to8`, `rgb16to8`, `rgba16to8`, `y32to8`, `y32to16`) built from the
    same `>>> 8`/`>>> 16`/`>>> 24` truncation upstream's `from16to8`/
    `from32to8`/`from32to16` use, then lifted over an `Image` with
    `pixelMap` — the same numeric result, computed pixel-at-a-time instead of
    as one raw-array pass (this package's `VectorByteConversion.lean` already
    documents dropping the analogous raw-buffer-reinterpretation trick
    elsewhere for the same reason: Lean's `Array`/`ByteArray` give no aliasing
    to exploit, so there is no performance the pixel-at-a-time version is
    actually leaving on the table beyond what every other decoder/encoder in
    this package already accepts).
  - **`from16toFloat`** (16-bit → float, used for Radiance HDR export) is
    likewise given as `rgb16ToRgbF` below, plus the `y16ToRgbF`/`y32ToRgbF`/
    `ya16ToRgbF`/`rgba16ToRgbF` variants upstream's `imageToRadiance` needs
    for its non-RGB16 branches (`ImageY16`/`ImageY32`/`ImageYA16`/
    `ImageRGBA16`), each following upstream's own inline `PixelRGBF v v v`/
    `fromIntegral v / 65536.0` formulas for those branches.
  - **`cmyk16ToRgb16`** ports upstream's `ColorSpaceConvertible PixelCMYK16
    PixelRGB16` instance (`Types.hs`): `r = ((65535 - c) * (65535 - k)) >>>
    16`, etc. This port's `Types.lean` never added that instance (only the
    8-bit `ColorSpaceConvertible PixelCMYK8 PixelRGB8` exists there), so it
    is given here as a local pixel-level function instead — exactly the
    same treatment `Jpg.lean` already gives `ycckArrayToCmyk` for a
    different missing upstream instance (`ColorSpaceConvertible
    PixelYCbCrK8 PixelCMYK8`), for the same reason: needed only by one
    format-facing module, not general enough to justify a new `Types.lean`
    instance.
  - **`rgb8ToRgbF`/`yFToRgbF`** cover upstream's `ColorConvertible PixelRGB8
    PixelRGBF`/`ColorConvertible PixelF PixelRGBF` instances (used by
    `imageToRadiance`'s `promoteImage` calls), neither of which exists in
    this port's `Types.lean` either (its `ColorConvertible` instances only
    ever widen *component width*, e.g. `Pixel8 → Pixel16`, never change
    *component type* to `PixelF`, except for the single `Pixel8 → PixelF`
    instance `rgb8ToRgbF` reuses component-wise below). Given locally here
    for the same reason as `cmyk16ToRgb16` above.

  ## Recursive dispatch flattened to one pass per branch

  Upstream's `imageToJpg`/`imageToPng`/`imageToGif`/`imageToBitmap`/
  `imageToTiff`/`imageToRadiance` are *self-recursive*: e.g.
  `imageToJpg quality (ImageCMYK8 img) = imageToJpg quality . ImageRGB8 $
  convertImage img` re-enters the very same function on a smaller-in-spirit
  but not structurally-smaller `DynamicImage` (there is no decreasing
  measure on `DynamicImage` itself — `ImageCMYK16` reduces to `ImageRGB16`,
  which reduces to `ImageRGB8`, which is a *different* constructor, not a
  smaller instance of the same one). Rather than construct a well-founded
  recursion measure purely to dodge Lean's termination checker on what is,
  operationally, always a short, finite, statically-known chain of coercions
  per constructor, every branch below inlines its whole coercion chain
  directly (e.g. the `cmyk16` branch of `imageToJpg` performs `cmyk16ToRgb16`
  then `rgb16to8` then `ColorSpaceConvertible.convertPixel` to `PixelYCbCr8`
  in one match arm) — the exact same sequence of coercions upstream's
  recursive calls perform, just written as a straight-line pipeline instead
  of a self-call, so no `partial def` and no termination proof is needed at
  all.

  ## Scope: no `imageToTga`

  Upstream's `Saving.hs` also exports `imageToTga`. `Codec.Picture.Tga`
  (module 9) is not one of this module's stated dependencies (see
  `docs/imports/JuicyPixels/dependencies.md`'s module 28 entry: "on #8, #10,
  #14, #17, #20, #27" — module 9 is conspicuously absent even though it sits
  earlier in the topological order and was already ported), so `imageToTga`
  is deliberately left out of this port, deferred to whenever that
  dependency listing is revisited.
-/

namespace Codec.Picture

-- ── Local pixel-coercion helpers (no reusable `Types.lean` instance) ──

/-- Tone-map a linear float component down to an 8-bit sample, clamping to
    `[0, 1]` first (upstream's `componentToLDR`). -/
def componentToLDR (f : PixelF) : Pixel8 :=
  (255.0 * (min (1.0 : PixelF) (max (0.0 : PixelF) f))).toUInt8

/-- Tone-map a floating-point RGB pixel down to 8-bit RGB (upstream's
    `toStandardDef`, lifted to a single pixel here; see `toStandardDef`
    below for the `Image`-level version). -/
def toStandardDefPixel (p : PixelRGBF) : PixelRGB8 :=
  ⟨componentToLDR p.r, componentToLDR p.g, componentToLDR p.b⟩

/-- Tone-map a floating-point RGB image down to 8-bit RGB (upstream's
    `toStandardDef`). -/
def toStandardDef (img : Image PixelRGBF) : Image PixelRGB8 :=
  pixelMap toStandardDefPixel img

/-- Tone-map a floating-point grayscale image down to 8-bit grayscale
    (upstream's `greyScaleToStandardDef`). -/
def greyScaleToStandardDef (img : Image PixelF) : Image Pixel8 :=
  pixelMap componentToLDR img

/-- Narrow a 16-bit sample to 8 bits by truncation (upstream's
    `from16to8`, specialised to one component). -/
def y16to8 (v : Pixel16) : Pixel8 := (v >>> 8).toUInt8

/-- Narrow a 16-bit grey+alpha pixel to 8-bit (upstream's `from16to8` at the
    `PixelYA16 → PixelYA8` shape). -/
def ya16to8 (p : PixelYA16) : PixelYA8 := ⟨y16to8 p.y, y16to8 p.a⟩

/-- Narrow a 16-bit RGB pixel to 8-bit (upstream's `from16to8` at the
    `PixelRGB16 → PixelRGB8` shape). -/
def rgb16to8 (p : PixelRGB16) : PixelRGB8 := ⟨y16to8 p.r, y16to8 p.g, y16to8 p.b⟩

/-- Narrow a 16-bit RGBA pixel to 8-bit (upstream's `from16to8` at the
    `PixelRGBA16 → PixelRGBA8` shape). -/
def rgba16to8 (p : PixelRGBA16) : PixelRGBA8 :=
  ⟨y16to8 p.r, y16to8 p.g, y16to8 p.b, y16to8 p.a⟩

/-- Narrow a 32-bit grey sample to 8 bits by truncation (upstream's
    `from32to8`). -/
def y32to8 (v : Pixel32) : Pixel8 := (v >>> 24).toUInt8

/-- Narrow a 32-bit grey sample to 16 bits by truncation (upstream's
    `from32to16`). -/
def y32to16 (v : Pixel32) : Pixel16 := (v >>> 16).toUInt16

/-- Convert a 16-bit CMYK pixel to 16-bit RGB (upstream's
    `ColorSpaceConvertible PixelCMYK16 PixelRGB16` instance — not present in
    this port's `Types.lean`; see the module doc-comment). -/
def cmyk16ToRgb16 (p : PixelCMYK16) : PixelRGB16 :=
  let ik : UInt32 := 65535 - p.k.toUInt32
  let conv (x : Pixel16) : Pixel16 := (((65535 - x.toUInt32) * ik) >>> 16).toUInt16
  ⟨conv p.c, conv p.m, conv p.y⟩

/-- Convert a 16-bit RGB pixel to floating-point RGB (upstream's
    `from16toFloat`, at the RGB shape). -/
def rgb16ToRgbF (p : PixelRGB16) : PixelRGBF :=
  ⟨p.r.toFloat32 / 65536.0, p.g.toFloat32 / 65536.0, p.b.toFloat32 / 65536.0⟩

/-- Convert an 8-bit RGB pixel to floating-point RGB, reusing the existing
    `ColorConvertible Pixel8 PixelF` instance component-wise (upstream's
    `ColorConvertible PixelRGB8 PixelRGBF` instance; see the module
    doc-comment). -/
def rgb8ToRgbF (p : PixelRGB8) : PixelRGBF :=
  ⟨ColorConvertible.promotePixel p.r, ColorConvertible.promotePixel p.g,
    ColorConvertible.promotePixel p.b⟩

/-- Replicate a floating-point grey sample across RGB (upstream's
    `ColorConvertible PixelF PixelRGBF` instance; see the module
    doc-comment). -/
def yFToRgbF (v : PixelF) : PixelRGBF := ⟨v, v, v⟩

/-- Replicate a 16-bit grey sample across floating-point RGB, matching
    upstream's `imageToRadiance` inline formula for `ImageY16`
    (`fromIntegral v / 65536.0`). -/
def y16ToRgbF (v : Pixel16) : PixelRGBF :=
  let f := v.toFloat32 / 65536.0
  ⟨f, f, f⟩

/-- Replicate a 32-bit grey sample across floating-point RGB, matching
    upstream's `imageToRadiance` inline formula for `ImageY32`
    (`fromIntegral v / 4294967296.0`). -/
def y32ToRgbF (v : Pixel32) : PixelRGBF :=
  let f := v.toFloat32 / 4294967296.0
  ⟨f, f, f⟩

/-- Replicate a 16-bit grey+alpha pixel's grey sample across floating-point
    RGB, dropping alpha (upstream's `imageToRadiance` inline formula for
    `ImageYA16`). -/
def ya16ToRgbF (p : PixelYA16) : PixelRGBF := y16ToRgbF p.y

/-- Convert a 16-bit RGBA pixel to floating-point RGB, dropping alpha
    (upstream's `imageToRadiance` inline formula for `ImageRGBA16`). -/
def rgba16ToRgbF (p : PixelRGBA16) : PixelRGBF := rgb16ToRgbF ⟨p.r, p.g, p.b⟩

-- ── Radiance HDR ──

/-- Encode a dynamic image as Radiance HDR, converting as needed (upstream's
    `imageToRadiance`). Every branch bottoms out at `encodeHDR` on a
    `PixelRGBF` image; none of the coercions used to get there can fail, so
    (matching upstream) this is total. -/
def imageToRadiance : DynamicImage → Data.ByteString
  | .cmyk8 img =>
      encodeHDR (pixelMap (fun p => rgb8ToRgbF (ColorSpaceConvertible.convertPixel p)) img)
  | .cmyk16 img =>
      encodeHDR (pixelMap (fun p => rgb16ToRgbF (cmyk16ToRgb16 p)) img)
  | .ycbcr8 img =>
      encodeHDR (pixelMap (fun p => rgb8ToRgbF (ColorSpaceConvertible.convertPixel p)) img)
  | .rgb8 img => encodeHDR (pixelMap rgb8ToRgbF img)
  | .rgbF img => encodeHDR img
  | .rgba8 img =>
      encodeHDR (pixelMap (fun p => rgb8ToRgbF (TransparentPixel.dropAlphaLayer p)) img)
  | .y8 img =>
      encodeHDR (pixelMap (fun p => rgb8ToRgbF (ColorConvertible.promotePixel p)) img)
  | .yF img => encodeHDR (pixelMap yFToRgbF img)
  | .ya8 img =>
      encodeHDR (pixelMap
        (fun p => rgb8ToRgbF (ColorConvertible.promotePixel (TransparentPixel.dropAlphaLayer p))) img)
  | .y16 img => encodeHDR (pixelMap y16ToRgbF img)
  | .y32 img => encodeHDR (pixelMap y32ToRgbF img)
  | .ya16 img => encodeHDR (pixelMap ya16ToRgbF img)
  | .rgb16 img => encodeHDR (pixelMap rgb16ToRgbF img)
  | .rgba16 img => encodeHDR (pixelMap rgba16ToRgbF img)

-- ── JPEG ──

/-- Encode a dynamic image as JPEG at the given quality (`0..100`),
    converting as needed (upstream's `imageToJpg`). YCbCr and Y/YA images
    are encoded directly; every other colour space is converted to RGB then
    YCbCr (matching upstream's own doc-comment: "Save Y or YCbCr Jpeg only,
    all other colorspaces are converted"). Total: every branch bottoms out
    at a pure JPEG encoder. -/
def imageToJpg (quality : UInt8) : DynamicImage → Data.ByteString
  | .ycbcr8 img => encodeJpegAtQuality quality img
  | .cmyk8 img =>
      let rgb : Image PixelRGB8 := pixelMap ColorSpaceConvertible.convertPixel img
      encodeJpegAtQuality quality (pixelMap ColorSpaceConvertible.convertPixel rgb)
  | .cmyk16 img =>
      let rgb16 : Image PixelRGB16 := pixelMap cmyk16ToRgb16 img
      let rgb8 : Image PixelRGB8 := pixelMap rgb16to8 rgb16
      encodeJpegAtQuality quality (pixelMap ColorSpaceConvertible.convertPixel rgb8)
  | .rgb8 img => encodeJpegAtQuality quality (pixelMap ColorSpaceConvertible.convertPixel img)
  | .rgbF img =>
      encodeJpegAtQuality quality (pixelMap ColorSpaceConvertible.convertPixel (toStandardDef img))
  | .rgba8 img =>
      let rgb : Image PixelRGB8 := pixelMap TransparentPixel.dropAlphaLayer img
      encodeJpegAtQuality quality (pixelMap ColorSpaceConvertible.convertPixel rgb)
  | .yF img => encodeDirectJpegAtQualityWithMetadata quality Metadatas.empty (greyScaleToStandardDef img)
  | .y8 img => encodeDirectJpegAtQualityWithMetadata quality Metadatas.empty img
  | .ya8 img =>
      encodeDirectJpegAtQualityWithMetadata quality Metadatas.empty
        (pixelMap TransparentPixel.dropAlphaLayer img)
  | .y16 img => encodeDirectJpegAtQualityWithMetadata quality Metadatas.empty (pixelMap y16to8 img)
  | .ya16 img =>
      let ya8 : Image PixelYA8 := pixelMap ya16to8 img
      encodeDirectJpegAtQualityWithMetadata quality Metadatas.empty
        (pixelMap TransparentPixel.dropAlphaLayer ya8)
  | .y32 img => encodeDirectJpegAtQualityWithMetadata quality Metadatas.empty (pixelMap y32to8 img)
  | .rgb16 img =>
      let rgb8 : Image PixelRGB8 := pixelMap rgb16to8 img
      encodeJpegAtQuality quality (pixelMap ColorSpaceConvertible.convertPixel rgb8)
  | .rgba16 img =>
      let rgba8 : Image PixelRGBA8 := pixelMap rgba16to8 img
      let rgb8 : Image PixelRGB8 := pixelMap TransparentPixel.dropAlphaLayer rgba8
      encodeJpegAtQuality quality (pixelMap ColorSpaceConvertible.convertPixel rgb8)

-- ── PNG ──

/-- Encode a dynamic image as PNG, converting as needed (upstream's
    `imageToPng`). `IO`-returning because `Png.lean`'s `encodePng` is (its
    deflate step runs through `Crypto.Zlib.compress`). Total: every branch
    bottoms out at `encodePng` on a `PngSavable` pixel type. -/
def imageToPng : DynamicImage → IO Data.ByteString
  | .ycbcr8 img => encodePng (pixelMap ColorSpaceConvertible.convertPixel img : Image PixelRGB8)
  | .cmyk8 img => encodePng (pixelMap ColorSpaceConvertible.convertPixel img : Image PixelRGB8)
  | .cmyk16 img => encodePng (pixelMap cmyk16ToRgb16 img : Image PixelRGB16)
  | .rgb8 img => encodePng img
  | .rgbF img => encodePng (toStandardDef img)
  | .rgba8 img => encodePng img
  | .y8 img => encodePng img
  | .yF img => encodePng (greyScaleToStandardDef img)
  | .ya8 img => encodePng img
  | .y16 img => encodePng img
  | .y32 img => encodePng (pixelMap y32to16 img)
  | .ya16 img => encodePng img
  | .rgb16 img => encodePng img
  | .rgba16 img => encodePng img

-- ── TIFF ──

/-- Encode a dynamic image as TIFF, converting as needed (upstream's
    `imageToTiff`). Two branches deviate from upstream's direct
    `encodeTiff img` because `Tiff.lean`'s `TiffSaveable` deliberately has no
    `PixelYCbCr8`/`Pixel32` instance (see `Tiff.lean`'s own module
    doc-comment): `ycbcr8` is converted to RGB8 first (matching every other
    format in this file), and `y32` is narrowed to 16-bit first via
    `y32to16` (matching `imageToPng`'s identical `ImageY32` treatment
    above). `ya8`/`ya16` drop alpha before encoding, matching upstream's own
    (seemingly deliberate) choice to do so even though a `TiffSaveable
    PixelYA8`/`PixelYA16` instance exists both upstream and here. Total:
    every branch bottoms out at the pure `encodeTiff`. -/
def imageToTiff : DynamicImage → Data.ByteString
  | .ycbcr8 img => encodeTiff (pixelMap ColorSpaceConvertible.convertPixel img : Image PixelRGB8)
  | .cmyk8 img => encodeTiff img
  | .cmyk16 img => encodeTiff img
  | .rgb8 img => encodeTiff img
  | .rgbF img => encodeTiff (toStandardDef img)
  | .rgba8 img => encodeTiff img
  | .y8 img => encodeTiff img
  | .yF img => encodeTiff (greyScaleToStandardDef img)
  | .ya8 img => encodeTiff (pixelMap TransparentPixel.dropAlphaLayer img : Image Pixel8)
  | .y16 img => encodeTiff img
  | .y32 img => encodeTiff (pixelMap y32to16 img)
  | .ya16 img => encodeTiff (pixelMap TransparentPixel.dropAlphaLayer img : Image Pixel16)
  | .rgb16 img => encodeTiff img
  | .rgba16 img => encodeTiff img

-- ── Bitmap (BMP) ──

/-- Encode a dynamic image as a Windows bitmap, converting as needed
    (upstream's `imageToBitmap`). Total: every branch bottoms out at the
    pure `encodeBitmap`. -/
def imageToBitmap : DynamicImage → Data.ByteString
  | .ycbcr8 img => encodeBitmap (pixelMap ColorSpaceConvertible.convertPixel img : Image PixelRGB8)
  | .cmyk8 img => encodeBitmap (pixelMap ColorSpaceConvertible.convertPixel img : Image PixelRGB8)
  | .cmyk16 img =>
      let rgb16 : Image PixelRGB16 := pixelMap cmyk16ToRgb16 img
      encodeBitmap (pixelMap rgb16to8 rgb16)
  | .rgbF img => encodeBitmap (toStandardDef img)
  | .rgb8 img => encodeBitmap img
  | .rgba8 img => encodeBitmap img
  | .y8 img => encodeBitmap img
  | .yF img => encodeBitmap (greyScaleToStandardDef img)
  | .ya8 img => encodeBitmap (pixelMap ColorConvertible.promotePixel img : Image PixelRGBA8)
  | .y16 img => encodeBitmap (pixelMap y16to8 img)
  | .y32 img => encodeBitmap (pixelMap y32to8 img)
  | .ya16 img =>
      let ya8 : Image PixelYA8 := pixelMap ya16to8 img
      encodeBitmap (pixelMap ColorConvertible.promotePixel ya8 : Image PixelRGBA8)
  | .rgb16 img => encodeBitmap (pixelMap rgb16to8 img)
  | .rgba16 img => encodeBitmap (pixelMap rgba16to8 img)

-- ── GIF ──

/-- Encode a dynamic image as GIF, converting/quantising as needed
    (upstream's `imageToGif`). `Except`-returning because
    `Gif.lean`'s `encodeGifImage`/`encodeGifImageWithPalette` are (a
    palettised image can be malformed); nothing upstream of that call can
    itself fail.

    Every grayscale-family branch (`y8`/`yF`/`ya8`/`y16`/`y32`/`ya16`) uses
    `encodeGifImageWithPalette _ greyPalette` rather than this port's
    `encodeGifImage`: `Gif.lean`'s `encodeGifImage` takes an `Image
    PixelRGB8` (it colour-quantises internally), unlike upstream's
    `encodeGifImage :: Image Pixel8 -> L.ByteString` (which takes an
    already-indexed image and pairs it with upstream's exported
    `greyPalette`, a 256-entry grayscale ramp — ported here unchanged as
    `Gif.lean`'s own `greyPalette`). Passing an already-indexed `Pixel8`
    image plus `greyPalette` to `encodeGifImageWithPalette` reproduces
    upstream's exact `encodeGifImage` behaviour without needing a second,
    RGB8-colour-quantising code path for what is already a losslessly
    indexed image. -/
def imageToGif : DynamicImage → Except String Data.ByteString
  | .ycbcr8 img =>
      let rgb : Image PixelRGB8 := pixelMap ColorSpaceConvertible.convertPixel img
      let (indexed, pal) := palettize defaultPaletteOptions rgb
      encodeGifImageWithPalette indexed pal
  | .cmyk8 img =>
      let rgb : Image PixelRGB8 := pixelMap ColorSpaceConvertible.convertPixel img
      let (indexed, pal) := palettize defaultPaletteOptions rgb
      encodeGifImageWithPalette indexed pal
  | .cmyk16 img =>
      let rgb16 : Image PixelRGB16 := pixelMap cmyk16ToRgb16 img
      let rgb8 : Image PixelRGB8 := pixelMap rgb16to8 rgb16
      let (indexed, pal) := palettize defaultPaletteOptions rgb8
      encodeGifImageWithPalette indexed pal
  | .rgbF img =>
      let rgb8 := toStandardDef img
      let (indexed, pal) := palettize defaultPaletteOptions rgb8
      encodeGifImageWithPalette indexed pal
  | .rgb8 img =>
      let (indexed, pal) := palettize defaultPaletteOptions img
      encodeGifImageWithPalette indexed pal
  | .rgba8 img =>
      let rgb8 : Image PixelRGB8 := pixelMap TransparentPixel.dropAlphaLayer img
      let (indexed, pal) := palettize defaultPaletteOptions rgb8
      encodeGifImageWithPalette indexed pal
  | .y8 img => encodeGifImageWithPalette img greyPalette
  | .yF img => encodeGifImageWithPalette (greyScaleToStandardDef img) greyPalette
  | .ya8 img =>
      encodeGifImageWithPalette (pixelMap TransparentPixel.dropAlphaLayer img : Image Pixel8) greyPalette
  | .y16 img => encodeGifImageWithPalette (pixelMap y16to8 img) greyPalette
  | .y32 img => encodeGifImageWithPalette (pixelMap y32to8 img) greyPalette
  | .ya16 img =>
      let ya8 : Image PixelYA8 := pixelMap ya16to8 img
      encodeGifImageWithPalette (pixelMap TransparentPixel.dropAlphaLayer ya8 : Image Pixel8) greyPalette
  | .rgb16 img =>
      let rgb8 : Image PixelRGB8 := pixelMap rgb16to8 img
      let (indexed, pal) := palettize defaultPaletteOptions rgb8
      encodeGifImageWithPalette indexed pal
  | .rgba16 img =>
      let rgba8 : Image PixelRGBA8 := pixelMap rgba16to8 img
      let rgb8 : Image PixelRGB8 := pixelMap TransparentPixel.dropAlphaLayer rgba8
      let (indexed, pal) := palettize defaultPaletteOptions rgb8
      encodeGifImageWithPalette indexed pal

end Codec.Picture
