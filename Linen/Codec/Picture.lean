import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Metadata
import Linen.Codec.Picture.ColorQuant
import Linen.Codec.Picture.Bitmap
import Linen.Codec.Picture.Tga
import Linen.Codec.Picture.HDR
import Linen.Codec.Picture.Png
import Linen.Codec.Picture.Tiff
import Linen.Codec.Picture.Gif
import Linen.Codec.Picture.Jpg
import Linen.Codec.Picture.Saving

/-!
  Port of `Codec.Picture` from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 29 of 29, the **last**
  module of this import): the public-facing facade over the whole codec
  suite — format-sniffing `decodeImage`, `readImage`/`save*Image`/`write*`
  file-I/O convenience wrappers, and the `convertRGB8`/`convertRGB16`/
  `convertRGBA8` "give me *some* concrete pixel format" helpers — built on
  top of every earlier module (1 `Types`, 5 `Metadata`, 7 `ColorQuant`, 8
  `Bitmap`, 9 `Tga`, 10 `HDR`, 14 `Png`, 17 `Tiff`, 20 `Gif`, 27 `Jpg`, 28
  `Saving`).

  ## Re-export strategy

  Upstream's `Codec.Picture` is, for the most part, a re-export module
  (`module Codec.Picture (module Codec.Picture.Bitmap, ...)`). Every module
  in this port that a Haskell reader would expect `Codec.Picture` to
  re-export from — `Types.lean`, `Bitmap.lean`, `Tga.lean`, `HDR.lean`,
  `Png.lean`, `Tiff.lean`, `Gif.lean`, `Jpg.lean`, `Saving.lean`,
  `Metadata.lean`, `ColorQuant.lean` — already opens `namespace Codec.Picture`
  itself (not e.g. `Linen.Codec.Picture.Png`), exactly as this file does. So
  a plain `import Linen.Codec.Picture` (which transitively imports all of the
  above) already puts every one of their declarations in scope as
  `Codec.Picture.foo` (or bare `foo` under `open Codec.Picture`) with **no
  further aliasing needed** — `decodePng`, `encodeGifImage`, `palettize`,
  `DynamicImage`, `Pixel8`, etc. are all already reachable exactly as
  upstream's re-export list promises. Only the handful of names upstream's
  `Codec.Picture.hs` itself *defines* (rather than re-exports) need new code
  here: the `decodeImage`/`readImage`/`save*`/`write*` family below, plus
  `convertRGB8`/`convertRGB16`/`convertRGBA8`/`dynamicPixelMap`/
  `generateFoldImage`.

  ## File I/O primitives

  Every read/write wrapper below goes through `Data.ByteString.readFile`/
  `Data.ByteString.writeFile` (`Linen/Data/ByteString.lean`), the same
  `IO.FS.readBinFile`/`IO.FS.writeBinFile`-backed helpers already used by
  `Network.WebApp.Server.SendFile`/`StreamFile` and by every `*Test.lean` in
  this package's own test suite for round-tripping encoder output back
  through a decoder — no new I/O primitive is introduced. `readImage`-family
  functions additionally wrap the read in `try ... catch e => pure (.error
  (toString e))` (matching `Linen.Database.SQL.Pool`/`Session`/`Linen.CDP.
  Runtime`'s existing `IO` exception-handling convention in this codebase)
  in place of upstream's `Exc.catch ... (\e -> return . Left $ show (e ::
  Exc.IOException))`.

  ## `decodeImage`: faithful "try every decoder in turn," not magic-byte
  ## sniffing

  Despite the common informal description of `decodeImage` as "format
  sniffing," upstream's actual `Codec.Picture.hs` source (fetched fresh for
  this port) contains **no byte-signature dispatch table** at all — no
  `isPng`/`isBitmap`/`isGif`/... predicates anywhere in this module. Its
  `decodeImageWithPaletteAndMetadata` is `eitherLoad`: try each format's own
  `decode*WithPaletteAndMetadata` decoder in a fixed order (`Jpeg, PNG,
  Bitmap, GIF, HDR, Tiff, TGA`), keep the first success, and otherwise
  concatenate every format's own error message. Each format's decoder
  already validates its own magic bytes as the very first step of parsing
  (`Png.lean`'s 8-byte PNG signature check, `Bitmap.lean`'s `"BM"` check,
  `Gif.lean`'s `GIF87a`/`GIF89a` check, `Jpg.lean`'s `0xFFD8` check,
  `Tiff.lean`'s `II*\0`/`MM\0*` check, `HDR.lean`'s `#?RADIANCE`/`#?RGBE`
  check) and fails fast on a mismatch — so the *effect* of "sniffing" is
  already achieved by delegating to those checks in sequence, without this
  module needing to duplicate a second, separate byte-prefix table. This
  port follows upstream's actual `eitherLoad` algorithm faithfully rather
  than inventing sniffing logic upstream itself does not have.

  ## `decodeImage`'s `IO`, and the TGA gap it does *not* need to fix

  `Png.lean`'s `decodePng` is `IO`-returning (its inflate step runs through
  `Crypto.Zlib.decompress`; see that module's own doc-comment), so any
  aggregate decoder that tries PNG among its candidates must itself run in
  `IO` — hence `decodeImage`/`readImage` below are `IO`-returning, unlike
  upstream's pure `Either String DynamicImage`. Every other
  individual-format decoder in this file (`decodeBitmap`, `decodeGif`,
  `decodeHDR`, `decodeTiff`, `decodeJpeg`, `decodeTga`) stays pure, and
  `readBitmap`/`readGif`/`readHDR`/`readTiff`/`readJpeg`/`readTGA`/
  `readGifImages` below stay `IO (Except String _)` only because reading
  the file itself is `IO`, not because decoding is. `Saving.lean` (module
  28) deliberately left `imageToTga` out of scope (its own doc-comment: TGA
  is not one of its stated dependencies); this module does not need
  `imageToTga` either — `decodeImageWithPaletteAndMetadata` only needs
  `decodeTgaWithPaletteAndMetadata` (present, module 9), and there is no
  `saveTgaImage` in upstream's own export list to begin with.

  ## `Metadatas`/`IO`, and the deferred `readImageWithMetadata`

  `Metadatas` (`Metadata.lean`, module 5) is a `Type 1` value (it stores an
  existential `Elem` — a typed key paired with a value of that same,
  existentially-quantified type — so it cannot itself live in `Type 0`).
  Lean's `IO` is `IO : Type → Type`, fixed at `Type 0`, so **no type built
  from `Metadatas` can ever appear as `IO`'s type argument, at any nesting
  depth** — this is `Png.lean`'s own already-documented "universe wrinkle,"
  and it applies just as much here. `decodeImageWithPaletteAndMetadata`/
  `decodeImageWithMetadata` below take a plain `ByteArray` (already in
  memory) as input, so they can stay *pure* functions that return
  `Except String (Metadatas × IO (Except String _))` — `Metadatas` sits in
  the outer, non-`IO` `Except`, exactly mirroring
  `decodePngWithPaletteAndMetadata`'s own shape, and only the
  metadata-free final decode result goes through `IO`.

  `readImageWithMetadata`, however, would need to *read the file* before it
  can produce any `Metadatas` value at all — unlike the byte-array-input
  variants, there is no way to compute it "before touching `IO`," so no
  restructuring can keep `Metadatas` outside `IO`'s type argument here: any
  signature for it (e.g. `FilePath → IO (Except String (DynamicImage ×
  Metadatas))`, or any nesting thereof) is rejected by Lean's universe
  checker, full stop. A continuation-passing reformulation (the caller
  supplies `Metadatas → DynamicImage → IO γ` for their own `Type 0` result
  `γ`, so `Metadatas` only ever appears as a *function argument* type, never
  as `IO`'s own type parameter) would type-check, but changes this
  function's calling convention enough that it is deferred rather than
  folded into this already-large module — a genuine Lean-vs-Haskell
  universe mismatch, not a shortcut around proof work, in the same spirit as
  `Png.lean`'s own accepted "universe wrinkle" scope note. `readImage`
  (no metadata) is unaffected and fully ported below.

  ## `convertRGB8`/`convertRGB16`/`convertRGBA8`: flattened, not through a
  ## `Decimable` typeclass

  Upstream dispatches its bit-depth-narrowing step through a `Decimable px1
  px2` typeclass (`decimateBitDepth`) resolved once per `DynamicImage`
  constructor. That typeclass exists purely to let `decimateBitDepth`'s
  *type* pick the right raw-`Vector`-level narrowing function
  (`decimateWord16`/`decimateWord3216`/`decimateWord32`/`decimateFloat`/
  `decimateFloat16`); it is not itself recursive and carries no termination
  question. This port skips introducing the typeclass and instead calls the
  needed narrowing helper directly in each branch — exactly the "flatten
  dispatch into one pass per branch" treatment `Saving.lean` already applies
  to its own `imageTo*` functions, for the same reason (a handful of
  statically-known coercion chains, not worth a class hierarchy). Most
  narrowing helpers already exist in `Saving.lean` (`y16to8`, `y32to8`,
  `y32to16`, `ya16to8`, `rgb16to8`, `rgba16to8`, `cmyk16ToRgb16`,
  `toStandardDef`, `greyScaleToStandardDef`) and are reused verbatim; the
  handful missing there (needed only here) are added locally below:

  - **`cmyk16to8`** (`PixelCMYK16 → PixelCMYK8`, componentwise `y16to8`) —
    upstream's `decimateWord16` instantiated at the one pixel shape
    `Saving.lean` never needed.
  - **`componentToHDR16`** (`PixelF → Pixel16`, `⌊65535 · clamp₀¹(v)⌋`) —
    upstream's `decimateFloat16`, the 16-bit sibling of `Saving.lean`'s
    `componentToLDR` (`decimateFloat`, 8-bit).
  - **`y16ToRgb16`** (`Pixel16 → PixelRGB16`, replicate) — upstream's
    `ColorConvertible Pixel16 PixelRGB16` instance (lossless widen, same bit
    depth); not present in `Types.lean` (whose `ColorConvertible` instances
    only ever widen from an 8-bit source), so given here the same way
    `Saving.lean` gives `yFToRgbF`/`y16ToRgbF` for its own missing
    instances.
  - **`rgbFTo16`** (`PixelRGBF → PixelRGB16`, componentwise
    `componentToHDR16`) — the 16-bit sibling of `Saving.lean`'s
    `toStandardDefPixel`.

  Every other coercion `convertRGB8`/`convertRGB16`/`convertRGBA8` need
  (`ColorConvertible.promotePixel`, `ColorSpaceConvertible.convertPixel`,
  `TransparentPixel.dropAlphaLayer`) is an existing `Types.lean`/`Saving.lean`
  instance or helper, composed inline per branch (e.g. `PixelYCbCr8 →
  PixelRGB8 → PixelRGB16` for `convertRGB16`'s `ycbcr8` branch is two
  existing single-hop conversions applied in sequence, not a new instance).

  ## Deferred: no Lean equivalent, or genuinely out of scope

  - **`Traversal`/`imagePixels`/`imageIPixels`** (lens compatibility) — these
    exist upstream purely to let the `lens` package's `Control.Lens`
    combinators traverse an `Image`'s pixels; `linen` has no lens library and
    porting one just to give these two definitions a home is out of scope.
    `pixelMap`/`pixelMapXY`/`pixelFold` (`Types.lean`) already give direct,
    non-lens ways to traverse/transform an image's pixels.
  - **`withImage`** — upstream pokes raw `Word8`s into a mutable `Ptr`
    during construction, an FFI/`Storable`-level API with no meaningful
    translation into Lean's aliasing-free `Array`; `generateImage`/
    `generateFoldImage` below already cover "build an image pixel-by-pixel,"
    the only capability `withImage` actually adds beyond `generateImage`
    being direct byte-level `Ptr` access mid-construction.
  - **`imageFromUnsafePtr`** — likewise raw-pointer-based; `VectorByteConversion.
    lean`'s `imageFromByteArray` (module 2) is this port's already-established
    safe substitute (see that module's own doc-comment), reused as-is rather
    than re-exposed under this second, unsafe-sounding name.

  ## Fixture/test naming

  Test fixtures use a `pic`-prefix, matching this package's established
  per-module convention (`Tests/Linen/Codec/Picture/PngTest.lean`'s `png`
  prefix, `Tests/Linen/Codec/Picture/SavingTest.lean`'s `saving` prefix) to
  avoid cross-file `Tests` namespace collisions.
-/

namespace Codec.Picture

-- ── Pixel-coercion helpers not already provided by `Saving.lean` ──

/-- Narrow a 16-bit CMYK pixel to 8-bit, componentwise (upstream's
    `decimateWord16` instantiated at `PixelCMYK16 → PixelCMYK8`; see the
    module doc-comment). -/
def cmyk16to8 (p : PixelCMYK16) : PixelCMYK8 :=
  ⟨y16to8 p.c, y16to8 p.m, y16to8 p.y, y16to8 p.k⟩

/-- Tone-map a linear float component up to a 16-bit sample, clamping to
    `[0, 1]` first (upstream's `decimateFloat16`). -/
def componentToHDR16 (f : PixelF) : Pixel16 :=
  (65535.0 * (min (1.0 : PixelF) (max (0.0 : PixelF) f))).toUInt16

/-- Replicate a 16-bit grey sample across 16-bit RGB (upstream's
    `ColorConvertible Pixel16 PixelRGB16` instance; see the module
    doc-comment). -/
def y16ToRgb16 (v : Pixel16) : PixelRGB16 := ⟨v, v, v⟩

/-- Tone-map a floating-point RGB pixel up to 16-bit RGB, componentwise
    (upstream's `decimateFloat16` instantiated at the RGB shape). -/
def rgbFTo16 (p : PixelRGBF) : PixelRGB16 :=
  ⟨componentToHDR16 p.r, componentToHDR16 p.g, componentToHDR16 p.b⟩

-- ── `convertRGBA8`/`convertRGB8`/`convertRGB16` ──

/-- Convert any `DynamicImage` to 8-bit true-colour RGBA, losing precision
    when narrowing from 16/32-bit or floating-point sources (upstream's
    `convertRGBA8`). Total: every `DynamicImage` constructor has an
    admissible coercion into `PixelRGBA8`. -/
def convertRGBA8 : DynamicImage → Image PixelRGBA8
  | .y8 img => pixelMap ColorConvertible.promotePixel img
  | .y16 img => pixelMap ColorConvertible.promotePixel (pixelMap y16to8 img : Image Pixel8)
  | .y32 img => pixelMap ColorConvertible.promotePixel (pixelMap y32to8 img : Image Pixel8)
  | .yF img => pixelMap ColorConvertible.promotePixel (greyScaleToStandardDef img)
  | .ya8 img => pixelMap ColorConvertible.promotePixel img
  | .ya16 img => pixelMap ColorConvertible.promotePixel (pixelMap ya16to8 img : Image PixelYA8)
  | .rgb8 img => pixelMap ColorConvertible.promotePixel img
  | .rgb16 img => pixelMap ColorConvertible.promotePixel (pixelMap rgb16to8 img : Image PixelRGB8)
  | .rgbF img => pixelMap ColorConvertible.promotePixel (toStandardDef img)
  | .rgba8 img => img
  | .rgba16 img => pixelMap rgba16to8 img
  | .ycbcr8 img =>
      pixelMap ColorConvertible.promotePixel
        (pixelMap ColorSpaceConvertible.convertPixel img : Image PixelRGB8)
  | .cmyk8 img =>
      pixelMap ColorConvertible.promotePixel
        (pixelMap ColorSpaceConvertible.convertPixel img : Image PixelRGB8)
  | .cmyk16 img =>
      pixelMap ColorConvertible.promotePixel
        (pixelMap ColorSpaceConvertible.convertPixel (pixelMap cmyk16to8 img : Image PixelCMYK8) :
          Image PixelRGB8)

/-- Convert any `DynamicImage` to 8-bit true-colour RGB, dropping any alpha
    channel and losing precision when narrowing from 16/32-bit or
    floating-point sources (upstream's `convertRGB8`). Total. -/
def convertRGB8 : DynamicImage → Image PixelRGB8
  | .y8 img => pixelMap ColorConvertible.promotePixel img
  | .y16 img => pixelMap ColorConvertible.promotePixel (pixelMap y16to8 img : Image Pixel8)
  | .y32 img => pixelMap ColorConvertible.promotePixel (pixelMap y32to8 img : Image Pixel8)
  | .yF img => pixelMap ColorConvertible.promotePixel (greyScaleToStandardDef img)
  | .ya8 img => pixelMap ColorConvertible.promotePixel img
  | .ya16 img => pixelMap ColorConvertible.promotePixel (pixelMap ya16to8 img : Image PixelYA8)
  | .rgb8 img => img
  | .rgb16 img => pixelMap rgb16to8 img
  | .rgbF img => toStandardDef img
  | .rgba8 img => pixelMap TransparentPixel.dropAlphaLayer img
  | .rgba16 img =>
      pixelMap TransparentPixel.dropAlphaLayer (pixelMap rgba16to8 img : Image PixelRGBA8)
  | .ycbcr8 img => pixelMap ColorSpaceConvertible.convertPixel img
  | .cmyk8 img => pixelMap ColorSpaceConvertible.convertPixel img
  | .cmyk16 img =>
      pixelMap ColorSpaceConvertible.convertPixel (pixelMap cmyk16to8 img : Image PixelCMYK8)

/-- Convert any `DynamicImage` to 16-bit true-colour RGB, dropping any alpha
    channel and losing precision when narrowing from 32-bit or
    floating-point sources (upstream's `convertRGB16`). Total. -/
def convertRGB16 : DynamicImage → Image PixelRGB16
  | .y8 img =>
      pixelMap (fun p => (ColorConvertible.promotePixel (ColorConvertible.promotePixel p :
        PixelRGB8) : PixelRGB16)) img
  | .y16 img => pixelMap y16ToRgb16 img
  | .y32 img => pixelMap (y16ToRgb16 ∘ y32to16) img
  | .yF img => pixelMap (y16ToRgb16 ∘ componentToHDR16) img
  | .ya8 img =>
      pixelMap (fun p => (ColorConvertible.promotePixel (ColorConvertible.promotePixel p :
        PixelRGB8) : PixelRGB16)) img
  | .ya16 img => pixelMap (fun p => y16ToRgb16 p.y) img
  | .rgb8 img => pixelMap ColorConvertible.promotePixel img
  | .rgb16 img => img
  | .rgbF img => pixelMap rgbFTo16 img
  | .rgba8 img =>
      pixelMap (fun p => TransparentPixel.dropAlphaLayer
        (ColorConvertible.promotePixel p : PixelRGBA16)) img
  | .rgba16 img => pixelMap TransparentPixel.dropAlphaLayer img
  | .ycbcr8 img =>
      pixelMap (fun p => (ColorConvertible.promotePixel
        (ColorSpaceConvertible.convertPixel p : PixelRGB8) : PixelRGB16)) img
  | .cmyk8 img =>
      pixelMap (fun p => (ColorConvertible.promotePixel
        (ColorSpaceConvertible.convertPixel p : PixelRGB8) : PixelRGB16)) img
  | .cmyk16 img => pixelMap cmyk16ToRgb16 img

-- ── `dynamicPixelMap`/`generateFoldImage` ──

/-- Apply a pixel-preserving-shape transform to whichever concrete image a
    `DynamicImage` holds, re-wrapping the result in the same constructor
    (upstream's `dynamicPixelMap`). -/
def dynamicPixelMap
    (f : {α Component : Type} → [Pixel α Component] → @Image α Component _ → @Image α Component _) :
    DynamicImage → DynamicImage
  | .y8 img => .y8 (f img)
  | .y16 img => .y16 (f img)
  | .y32 img => .y32 (f img)
  | .yF img => .yF (f img)
  | .ya8 img => .ya8 (f img)
  | .ya16 img => .ya16 (f img)
  | .rgb8 img => .rgb8 (f img)
  | .rgb16 img => .rgb16 (f img)
  | .rgbF img => .rgbF (f img)
  | .rgba8 img => .rgba8 (f img)
  | .rgba16 img => .rgba16 (f img)
  | .ycbcr8 img => .ycbcr8 (f img)
  | .cmyk8 img => .cmyk8 (f img)
  | .cmyk16 img => .cmyk16 (f img)

/-- Build an image by threading an accumulator through each pixel, left to
    right, top to bottom (upstream's `generateFoldImage`). -/
def generateFoldImage [Pixel α Component] (f : β → Nat → Nat → β × α) (initAcc : β)
    (width height : Nat) : β × @Image α Component _ :=
  Id.run do
    let mut acc := initAcc
    let mut data := Array.mkEmpty (Pixel.componentCount α * width * height)
    for y in [0:height] do
      for x in [0:width] do
        let (acc', p) := f acc x y
        acc := acc'
        data := data ++ Pixel.toComponents p
    pure (acc, { width, height, data })

-- ── `decodeImage` family (format detection by trying each decoder) ──

/-- Try to decode `input` in every supported format, in upstream's own
    order (`Jpeg, PNG, Bitmap, GIF, HDR, Tiff, TGA`), returning either the
    first format whose header structurally parses (with its metadata, and
    an action to run its palette-preserving decode) or every format's
    accumulated header-parse error (upstream's
    `decodeImageWithPaletteAndMetadata`, built on upstream's `eitherLoad`;
    see the module doc-comment on why this is a try-every-decoder fallback,
    not byte-signature sniffing).

    **`Metadatas`/`IO` universe wrinkle, one level up from `Png.lean`'s
    own:** this returns `Except String (Metadatas × IO (Except String _))`,
    exactly mirroring `decodePngWithPaletteAndMetadata`'s own shape
    (`Metadatas` outside `IO`, never inside it — `Metadatas` is `Type 1`,
    and `IO`'s single type argument must be `Type 0`, so it can never
    contain a `Metadatas` anywhere inside it, at any depth). Every format
    here except PNG is fully pure (no `IO` needed to know both its
    metadata *and* its final result), so those branches just wrap their
    already-known result as `pure (.ok _)`; only the PNG branch defers to
    its own genuinely `IO`-gated action (`Crypto.Zlib.decompress`). One
    consequence: if a byte string's *header* parses as PNG but the PNG
    *body* then fails once actually decoded (a corrupt zlib stream after a
    well-formed `IHDR`), this does not fall through to try Bitmap/GIF/etc.
    next the way upstream's `eitherLoad` would — upstream decides
    similarly, just one step later (after running its own `IO`), and no
    other format's header can also structurally parse as a valid PNG
    header, so the two only disagree on this one corrupted-PNG-body edge
    case. -/
def decodeImageWithPaletteAndMetadata (input : ByteArray) :
    Except String (Metadatas × IO (Except String (Sum DynamicImage PalettedImage))) :=
  match decodeJpegWithMetadata input.toList with
  | .ok (img, m) => .ok (m, pure (.ok (.inl img)))
  | .error e1 =>
  match decodePngWithPaletteAndMetadata input with
  | .ok (m, action) => .ok (m, action)
  | .error e2 =>
  match decodeBitmapWithPaletteAndMetadata input with
  | .ok (r, m) => .ok (m, pure (.ok r))
  | .error e3 =>
  match decodeGifWithPaletteAndMetadata input with
  | .ok (r, m) => .ok (m, pure (.ok r))
  | .error e4 =>
  match decodeHDRWithMetadata input with
  | .ok (img, m) => .ok (m, pure (.ok (.inl img)))
  | .error e5 =>
  match decodeTiffWithPaletteAndMetadata input with
  | .ok (r, m) => .ok (m, pure (.ok r))
  | .error e6 =>
  match decodeTgaWithPaletteAndMetadata input with
  | .ok (r, m) => .ok (m, pure (.ok r))
  | .error e7 =>
  .error s!"Cannot load file\nJpeg {e1}\nPNG {e2}\nBitmap {e3}\nGIF {e4}\nHDR {e5}\nTiff {e6}\nTGA {e7}\n"

/-- Equivalent to `decodeImage`, but also provides whatever metadata the
    winning format's decoder found (upstream's `decodeImageWithMetadata`).
    Collapses any indexed result to true-colour via `palettedToTrueColor`.
    Same `Metadatas`/`IO` shape as `decodeImageWithPaletteAndMetadata`. -/
def decodeImageWithMetadata (input : ByteArray) :
    Except String (Metadatas × IO (Except String DynamicImage)) :=
  match decodeImageWithPaletteAndMetadata input with
  | .error e => .error e
  | .ok (m, action) =>
    .ok (m, do
      match ← action with
      | .error e => pure (.error e)
      | .ok (.inl img) => pure (.ok img)
      | .ok (.inr pal) => pure (.ok (.rgb8 (palettedToTrueColor pal))))

/-- Decode `input` without knowing its format ahead of time, trying every
    supported codec in turn (upstream's `decodeImage`). `IO`-returning
    because the winning format might be PNG, whose actual pixel decode
    runs through `Crypto.Zlib.decompress`; this itself carries no
    `Metadatas`, so it has none of `decodeImageWithMetadata`'s universe
    wrinkle. -/
def decodeImage (input : ByteArray) : IO (Except String DynamicImage) :=
  match decodeImageWithMetadata input with
  | .error e => pure (.error e)
  | .ok (_, action) => action

-- ── `read*` file-decoding wrappers ──

/-- Read `path` and decode it with `decode`, turning any `IO` exception
    raised while reading into an `Except`-level error (upstream's
    `withImageDecoder`, specialised to a pure decoder). -/
private def readAndDecode (path : System.FilePath) (decode : ByteArray → Except String α) :
    IO (Except String α) := do
  try
    let bytes ← IO.FS.readBinFile path
    pure (decode bytes)
  catch e => pure (.error (toString e))

/-- `readAndDecode`, specialised to an `IO`-returning decoder. -/
private def readAndDecodeIO (path : System.FilePath) (decode : ByteArray → IO (Except String α)) :
    IO (Except String α) := do
  try
    let bytes ← IO.FS.readBinFile path
    decode bytes
  catch e => pure (.error (toString e))

/-- Load an image file without knowing its format ahead of time, doing
    everything `decodeImage` does (upstream's `readImage`). -/
def readImage (path : System.FilePath) : IO (Except String DynamicImage) :=
  readAndDecodeIO path decodeImage

/-- Load a PNG file from disk (upstream's `readPng`). -/
def readPng (path : System.FilePath) : IO (Except String DynamicImage) :=
  readAndDecodeIO path decodePng

/-- Load a GIF file from disk, compositing to a single image (upstream's
    `readGif`). -/
def readGif (path : System.FilePath) : IO (Except String DynamicImage) :=
  readAndDecode path decodeGif

/-- Load every frame of an animated (or single-frame) GIF file from disk
    (upstream's `readGifImages`). -/
def readGifImages (path : System.FilePath) : IO (Except String (List DynamicImage)) :=
  readAndDecode path decodeGifImages

/-- Load a TIFF file from disk (upstream's `readTiff`). -/
def readTiff (path : System.FilePath) : IO (Except String DynamicImage) :=
  readAndDecode path decodeTiff

/-- Load a JPEG file from disk; the result is still `YCbCr` if the source
    was (upstream's `readJpeg`). -/
def readJpeg (path : System.FilePath) : IO (Except String DynamicImage) :=
  readAndDecode path (fun bytes => decodeJpeg bytes.toList)

/-- Load a Windows bitmap file from disk (upstream's `readBitmap`). -/
def readBitmap (path : System.FilePath) : IO (Except String DynamicImage) :=
  readAndDecode path decodeBitmap

/-- Load a Radiance/RGBE HDR file from disk (upstream's `readHDR`). -/
def readHDR (path : System.FilePath) : IO (Except String DynamicImage) :=
  readAndDecode path decodeHDR

/-- Load a Truevision TGA file from disk (upstream's `readTGA`). -/
def readTGA (path : System.FilePath) : IO (Except String DynamicImage) :=
  readAndDecode path decodeTga

-- ── `write*` single-format file-encoding wrappers ──

/-- Encode `img` as a Windows bitmap and write it to `path` (upstream's
    `writeBitmap`). -/
def writeBitmap [Pixel pixel Pixel8] [BmpEncodable pixel]
    (path : System.FilePath) (img : Image pixel) : IO Unit :=
  Data.ByteString.writeFile path (encodeBitmap img)

/-- Encode any `DynamicImage` as a Windows bitmap (where supported) and
    write it to `path` (upstream's `writeDynamicBitmap`). -/
def writeDynamicBitmap (path : System.FilePath) (img : DynamicImage) : IO (Except String Unit) :=
  match encodeDynamicBitmap img with
  | .error e => pure (.error e)
  | .ok bs => do
      Data.ByteString.writeFile path bs
      pure (.ok ())

/-- Encode `img` as a PNG and write it to `path` (upstream's `writePng`). -/
def writePng {α Component : Type} [Pixel α Component] [PngSavable α]
    (path : System.FilePath) (img : @Image α Component _) : IO Unit := do
  let bs ← encodePng img
  Data.ByteString.writeFile path bs

/-- Encode any `DynamicImage` as a PNG (where supported) and write it to
    `path` (upstream's `writeDynamicPng`). -/
def writeDynamicPng (path : System.FilePath) (img : DynamicImage) : IO (Except String Unit) := do
  match ← encodeDynamicPng img with
  | .error e => pure (.error e)
  | .ok bs => do
      Data.ByteString.writeFile path bs
      pure (.ok ())

/-- Encode `img` as a Truevision TGA file and write it to `path` (upstream's
    `writeTga`). -/
def writeTga [Pixel pixel Pixel8] [TgaSaveable pixel]
    (path : System.FilePath) (img : Image pixel) : IO Unit :=
  Data.ByteString.writeFile path (encodeTga img)

/-- Encode `img` as a TIFF file and write it to `path` (upstream's
    `writeTiff`). -/
def writeTiff {α Component : Type} [Pixel α Component] [TiffSaveable α]
    (path : System.FilePath) (img : @Image α Component _) : IO Unit :=
  Data.ByteString.writeFile path (encodeTiff img)

/-- Encode `img` as a Radiance/RGBE HDR file and write it to `path`
    (upstream's `writeHDR`). -/
def writeHDR (path : System.FilePath) (img : Image PixelRGBF) : IO Unit :=
  Data.ByteString.writeFile path (encodeHDR img)

/-- Encode a single true-colour image as a one-frame GIF, auto-palettised,
    and write it to `path` (upstream's `writeGifImage`). -/
def writeGifImage (path : System.FilePath) (img : Image PixelRGB8) : IO (Except String Unit) :=
  match encodeGifImage img with
  | .error e => pure (.error e)
  | .ok bs => do
      Data.ByteString.writeFile path bs
      pure (.ok ())

/-- Encode a single already-paletted image against an explicit palette as a
    one-frame GIF, and write it to `path` (upstream's
    `writeGifImageWithPalette`). -/
def writeGifImageWithPalette (path : System.FilePath) (img : Image Pixel8) (palette : Palette) :
    IO (Except String Unit) :=
  match encodeGifImageWithPalette img palette with
  | .error e => pure (.error e)
  | .ok bs => do
      Data.ByteString.writeFile path bs
      pure (.ok ())

/-- Encode several auto-palettised frames sharing one global colour table as
    an animated GIF, and write it to `path` (upstream's `writeGifImages`). -/
def writeGifImages (path : System.FilePath) (looping : GifLooping) (delay : GifDelay)
    (imgs : List (Image PixelRGB8)) : IO (Except String Unit) :=
  match encodeGifImages looping delay imgs with
  | .error e => pure (.error e)
  | .ok bs => do
      Data.ByteString.writeFile path bs
      pure (.ok ())

/-- Encode a full-colour image to a GIF by colour-quantising it (upstream's
    `encodeColorReducedGifImage`; identical to `encodeGifImage`, which
    already auto-palettises via `Linen.Codec.Picture.ColorQuant.palettize`
    — see the module doc-comment). -/
def encodeColorReducedGifImage (img : Image PixelRGB8) : Except String Data.ByteString :=
  encodeGifImage img

/-- Encode a full-colour image to a GIF by colour-quantising it, and write
    it to `path` (upstream's `writeColorReducedGifImage`). -/
def writeColorReducedGifImage (path : System.FilePath) (img : Image PixelRGB8) :
    IO (Except String Unit) :=
  writeGifImage path img

/-- Encode an animated GIF where every frame shares the same delay
    (upstream's `encodeGifAnimation`; a thin re-argument-order wrapper over
    this port's own `encodeGifImages`). -/
def encodeGifAnimation (delay : GifDelay) (looping : GifLooping) (imgs : List (Image PixelRGB8)) :
    Except String Data.ByteString :=
  encodeGifImages looping delay imgs

/-- Encode an animated GIF where every frame shares the same delay, and
    write it to `path` (upstream's `writeGifAnimation`). -/
def writeGifAnimation (path : System.FilePath) (delay : GifDelay) (looping : GifLooping)
    (imgs : List (Image PixelRGB8)) : IO (Except String Unit) :=
  writeGifImages path looping delay imgs

-- ── `save*Image` generic (`DynamicImage`-dispatching) file-writing wrappers ──

/-- Save any `DynamicImage` as a `.jpg` file (upstream's `saveJpgImage`). -/
def saveJpgImage (quality : UInt8) (path : System.FilePath) (img : DynamicImage) : IO Unit :=
  Data.ByteString.writeFile path (imageToJpg quality img)

/-- Save any `DynamicImage` as a `.gif` file, colour-quantising as needed
    (upstream's `saveGifImage`). -/
def saveGifImage (path : System.FilePath) (img : DynamicImage) : IO (Except String Unit) :=
  match imageToGif img with
  | .error e => pure (.error e)
  | .ok bs => do
      Data.ByteString.writeFile path bs
      pure (.ok ())

/-- Save any `DynamicImage` as a `.tiff` file (upstream's `saveTiffImage`). -/
def saveTiffImage (path : System.FilePath) (img : DynamicImage) : IO Unit :=
  Data.ByteString.writeFile path (imageToTiff img)

/-- Save any `DynamicImage` as a `.hdr` (Radiance) file (upstream's
    `saveRadianceImage`). -/
def saveRadianceImage (path : System.FilePath) (img : DynamicImage) : IO Unit :=
  Data.ByteString.writeFile path (imageToRadiance img)

/-- Save any `DynamicImage` as a `.png` file (upstream's `savePngImage`).
    For example, a simple format transcoder:
    ```
    def transcodeToPng (pathIn pathOut : System.FilePath) : IO Unit := do
      match ← readImage pathIn with
      | .error _ => pure ()
      | .ok img => savePngImage pathOut img
    ```
    -/
def savePngImage (path : System.FilePath) (img : DynamicImage) : IO Unit := do
  let bs ← imageToPng img
  Data.ByteString.writeFile path bs

/-- Save any `DynamicImage` as a `.bmp` file (upstream's `saveBmpImage`). -/
def saveBmpImage (path : System.FilePath) (img : DynamicImage) : IO Unit :=
  Data.ByteString.writeFile path (imageToBitmap img)

end Codec.Picture
