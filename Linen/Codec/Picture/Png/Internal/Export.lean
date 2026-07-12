import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Png.Internal.Type
import Linen.Codec.Picture.Png.Internal.Metadata
import Linen.Codec.Picture.Metadata
import Linen.Codec.Picture.VectorByteConversion
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.Png.Internal.Export` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 13 of 29). Turns an
  `Image`/`DynamicImage` into the chunk stream (`IHDR`, ancillary metadata
  chunks, `PLTE`/`tRNS` for paletted images, `IDAT`, `IEND`) of a PNG file.

  ## Scope, verified against the actual fetched upstream source

  Two assumptions going into this port turned out to be wrong once the
  actual `Export.hs` was read, and the scope below reflects what upstream
  genuinely does instead:

  - **There is no filter-choice heuristic here.** Upstream's own module
    haddock says it outright: *"no filtering is applied, but export at least
    valid images"*. `genericEncodePng`/`genericEncode16BitsPng` always
    prepend the filter-type byte `0` (`PngFilterNone`) to every scanline and
    never even construct a `PngFilterSub`/`Up`/`Average`/`Paeth` byte. So
    this port's `rawScanlineBytes8`/`rawScanlineBytes16` below do exactly
    that — tag every row `PngFilter.none` — and there is no five-way
    filter-selection logic to port at all. (Choosing/applying the other four
    filters, needed only by a hypothetical *smarter* encoder, is simply not
    part of upstream `Export.hs`; a future module could add that as a
    genuinely new heuristic, but it would not be a port of anything here.)

  - **Upstream itself calls `Codec.Compression.Zlib.compress` directly** on
    the raw scanline stream to build the `IDAT` payload — it is *not* a
    zlib-free "just filtering" module as the task brief guessed. But this
    library's own zlib port, `Linen.Crypto.Zlib` (see
    `docs/imports/Zlib/dependencies.md`), only implements the **inflate**
    (decompress) direction via FFI — deflate/compress was explicitly marked
    out of scope when that module was ported, and still is. Calling a
    nonexistent `deflate` would mean either faking compression (silently
    breaking every encoded file's validity) or quietly inventing a deflate
    implementation as a byproduct of *this* module, which is really a
    separate, substantial porting task in its own right. So every function
    below that needs to compress the raw scanline stream (`encodePngUsing`,
    `encodePngWithMetadataUsing`, `PngPaletteSaveable`'s methods,
    `encodeDynamicPngUsing`) takes the compressor as an explicit `deflate :
    ByteArray → ByteArray` parameter instead of calling zlib itself. This
    keeps every other design decision upstream makes (chunk order, header
    fields, palette/`tRNS` handling, `DynamicImage` dispatch) faithfully
    ported and independently testable via `deflate := id`, while leaving the
    "supply a real deflate" wiring to whichever later module (module 14,
    `Linen.Codec.Picture.Png`) ends up owning zlib compress support. The
    plain, uncompressed halves (`rawScanlineBytes8`/`rawScanlineBytes16`,
    chunk assembly) are exactly what upstream's `genericEncodePng` computes
    before its own `Z.compress` call, so nothing about *this* module's own
    logic is left unported.

  ## Other design notes

  - `writePng`/`writeDynamicPng` (trivial `IO` file-writers) are dropped,
    matching this library's convention of leaving file I/O to the caller
    (same as `Tga.lean`/`HDR.lean`).
  - Upstream's `PngSavable`/`PngPaletteSaveable` classes expose
    `encodePng`/`encodePngWithMetadata` with no extra argument (because they
    call zlib themselves); here they become `PngSavable`/`PngPaletteSaveable`
    with `encodePalettedPngWithMetadataUsing` taking the `deflate` function,
    and the free functions `encodePngUsing`/`encodePngWithMetadataUsing`/
    `encodePalettedPngUsing`/`encodeDynamicPngUsing` thread it through.
  - Upstream's `TransparentPixel.dropAlphaLayer :: Image a -> Image b`
    operates on a whole palette image directly; `Linen.Codec.Picture.Types`'s
    `TransparentPixel.dropAlphaLayer` (module 1) is instead per-pixel (see
    that module's own doc-comment), so the `PixelRGBA8` palette path below
    applies it via `pixelMap` to get the same "drop every pixel's alpha"
    effect on the whole palette image.
  - `PngPaletteSaveable`'s upstream palette-size/index-range validation
    (`w <= 0 || w > 256 || h /= 1`, "index exceeds palette width") is kept
    verbatim.
-/

namespace Codec.Picture

-- ── Raw (unfiltered) scanline byte streams ──

/-- The raw, filter-tagged scanline byte stream a `Word8`-component pixel
    image's `IDAT` chunk holds *before* deflate: each of `img.height` rows is
    the filter-type byte `0` (`PngFilter.none`) followed by that row's
    `componentCount α * width` raw component bytes, row-major,
    top-to-bottom. This is *all* the "filtering" upstream's own
    `genericEncodePng` does — see the module doc-comment. -/
def rawScanlineBytes8 [Pixel α UInt8] (img : @Image α UInt8 _) : ByteArray :=
  let compCount := Pixel.componentCount α
  let lineSize := compCount * img.width
  Id.run do
    let mut out := ByteArray.empty
    for line in [0:img.height] do
      out := out.push (codeOfPngFilter .none)
      let base := line * lineSize
      for i in [0:lineSize] do
        out := out.push (img.data.getD (base + i) 0)
    pure out

/-- Like `rawScanlineBytes8`, for 16-bit-component pixel types: each
    component is split into two bytes, high byte first (big-endian),
    matching upstream's `genericEncode16BitsPng`. -/
def rawScanlineBytes16 [Pixel α UInt16] (img : @Image α UInt16 _) : ByteArray :=
  let compCount := Pixel.componentCount α
  let lineSize := compCount * img.width
  Id.run do
    let mut out := ByteArray.empty
    for line in [0:img.height] do
      out := out.push (codeOfPngFilter .none)
      let base := line * lineSize
      for i in [0:lineSize] do
        let v := img.data.getD (base + i) 0
        out := out.push ((v >>> 8).toUInt8)
        out := out.push v.toUInt8
    pure out

-- ── PNG header/chunk assembly ──

/-- Build the generic 13-byte `IHDR` header for a non-interlaced image of
    `imgType`/`depth`. -/
private def preparePngHeader [Pixel α Component] (img : @Image α Component _)
    (imgType : PngImageType) (depth : UInt8) : PngIHdr :=
  { width := img.width.toUInt32, height := img.height.toUInt32, bitDepth := depth,
    colourType := imgType, compressionMethod := 0, filterMethod := 0,
    interlaceMethod := .noInterlace }

private def endChunk : PngRawChunk := mkRawChunk iENDSignature ByteArray.empty

private def prepareIDatChunk (compressed : ByteArray) : PngRawChunk :=
  mkRawChunk iDATSignature compressed

private def preparePalette (pal : Palette) : PngRawChunk :=
  mkRawChunk pLTESignature (toByteArray pal.data)

private def preparePaletteAlpha (alphaPal : Array UInt8) : PngRawChunk :=
  mkRawChunk tRNSSignature (toByteArray alphaPal)

/-- Assemble a plain (non-palette) PNG file's bytes from an already-deflated
    `IDAT` payload and its declared header/metadata. -/
private def assemblePng (hdr : PngIHdr) (metas : Metadatas) (compressed : ByteArray) : Data.ByteString :=
  let img : PngRawImage :=
    { header := hdr, chunks := encodeMetadatas metas ++ [prepareIDatChunk compressed, endChunk] }
  (putPngRawImage img).toStrictByteString

/-- Assemble a colour-indexed PNG file's bytes from an already-deflated
    `IDAT` payload, its palette chunk, and an optional `tRNS` chunk. -/
private def assembleIndexedPng (hdr : PngIHdr) (metas : Metadatas) (paletteChunk : PngRawChunk)
    (transpChunk : List PngRawChunk) (compressed : ByteArray) : Data.ByteString :=
  let img : PngRawImage :=
    { header := hdr,
      chunks := encodeMetadatas metas ++ [paletteChunk] ++ transpChunk ++
                [prepareIDatChunk compressed, endChunk] }
  (putPngRawImage img).toStrictByteString

-- ── `PngSavable` (plain, non-palette PNG images) ──

/-- A pixel type that can be encoded as a plain (non-palette) PNG image: its
    `IHDR` colour type/bit depth, and how to turn an image's pixels into a
    raw (pre-deflate) scanline byte stream. -/
class PngSavable (α : Type) {Component : outParam Type} [Pixel α Component] where
  /-- The `IHDR` colour type to declare for this pixel format. -/
  pngImageType : PngImageType
  /-- The `IHDR` bit depth to declare for this pixel format (`8` or `16`). -/
  pngBitDepth : UInt8
  /-- The image's raw, filter-tagged (always `PngFilter.none`) scanline
      bytes, before deflate. -/
  pngRawScanlines : @Image α Component _ → ByteArray

instance : PngSavable Pixel8 where
  pngImageType := .greyscale
  pngBitDepth := 8
  pngRawScanlines := rawScanlineBytes8

instance : PngSavable PixelYA8 where
  pngImageType := .greyscaleWithAlpha
  pngBitDepth := 8
  pngRawScanlines := rawScanlineBytes8

instance : PngSavable PixelRGB8 where
  pngImageType := .trueColour
  pngBitDepth := 8
  pngRawScanlines := rawScanlineBytes8

instance : PngSavable PixelRGBA8 where
  pngImageType := .trueColourWithAlpha
  pngBitDepth := 8
  pngRawScanlines := rawScanlineBytes8

instance : PngSavable Pixel16 where
  pngImageType := .greyscale
  pngBitDepth := 16
  pngRawScanlines := rawScanlineBytes16

instance : PngSavable PixelYA16 where
  pngImageType := .greyscaleWithAlpha
  pngBitDepth := 16
  pngRawScanlines := rawScanlineBytes16

instance : PngSavable PixelRGB16 where
  pngImageType := .trueColour
  pngBitDepth := 16
  pngRawScanlines := rawScanlineBytes16

instance : PngSavable PixelRGBA16 where
  pngImageType := .trueColourWithAlpha
  pngBitDepth := 16
  pngRawScanlines := rawScanlineBytes16

/-- Encode `img` as a PNG file, with metadata, given a `deflate` function to
    compress the raw scanline byte stream (upstream's own
    `Codec.Compression.Zlib.compress` call — see the module doc-comment for
    why this port takes it as an explicit parameter). -/
def encodePngWithMetadataUsing {α Component : Type} [Pixel α Component] [PngSavable α]
    (deflate : ByteArray → ByteArray) (metas : Metadatas) (img : @Image α Component _) : Data.ByteString :=
  let hdr := preparePngHeader img (PngSavable.pngImageType (α := α)) (PngSavable.pngBitDepth (α := α))
  assemblePng hdr metas (deflate (PngSavable.pngRawScanlines (α := α) img))

/-- `encodePngWithMetadataUsing` with no metadata. -/
def encodePngUsing {α Component : Type} [Pixel α Component] [PngSavable α]
    (deflate : ByteArray → ByteArray) (img : @Image α Component _) : Data.ByteString :=
  encodePngWithMetadataUsing deflate Metadatas.empty img

-- ── `PngPaletteSaveable` (colour-indexed PNG images) ──

/-- A palette pixel type (`PixelRGB8` or `PixelRGBA8`) that can encode a
    `Pixel8`-indexed image as a colour-indexed PNG. -/
class PngPaletteSaveable (α : Type) [Pixel α UInt8] where
  /-- Encode `img` (a `Pixel8`-indexed image) against palette `pal`, with
      metadata, given a `deflate` function. Fails if the palette has fewer
      than `1` or more than `256` entries, is not exactly one pixel tall, or
      `img` contains an index the palette doesn't cover. -/
  encodePalettedPngWithMetadataUsing :
    (ByteArray → ByteArray) → Metadatas → Image α → Image Pixel8 → Except String Data.ByteString

instance : PngPaletteSaveable PixelRGB8 where
  encodePalettedPngWithMetadataUsing deflate metas pal img :=
    let w := pal.width
    let h := pal.height
    if w == 0 ∨ w > 256 ∨ h ≠ 1 then .error "Invalid palette"
    else if img.data.any (fun v => v.toNat ≥ w) then
      .error "Image contains indexes absent from the palette"
    else
      let hdr := preparePngHeader img .indexedColor 8
      .ok (assembleIndexedPng hdr metas (preparePalette pal) [] (deflate (rawScanlineBytes8 img)))

instance : PngPaletteSaveable PixelRGBA8 where
  encodePalettedPngWithMetadataUsing deflate metas pal img :=
    let w := pal.width
    let h := pal.height
    if w == 0 ∨ w > 256 ∨ h ≠ 1 then .error "Invalid palette"
    else if img.data.any (fun v => v.toNat ≥ w) then
      .error "Image contains indexes absent from the palette"
    else
      let opaquePalette : Palette := pixelMap TransparentPixel.dropAlphaLayer pal
      let alphaPal := (extractComponent (α := PixelRGBA8) (plane := PlaneAlpha) pal).data
      let hdr := preparePngHeader img .indexedColor 8
      .ok (assembleIndexedPng hdr metas (preparePalette opaquePalette) [preparePaletteAlpha alphaPal]
            (deflate (rawScanlineBytes8 img)))

/-- `encodePalettedPngWithMetadataUsing` with no metadata. -/
def encodePalettedPngUsing {α : Type} [Pixel α UInt8] [PngPaletteSaveable α]
    (deflate : ByteArray → ByteArray) (pal : Image α) (img : Image Pixel8) :
    Except String Data.ByteString :=
  PngPaletteSaveable.encodePalettedPngWithMetadataUsing deflate Metadatas.empty pal img

-- ── `DynamicImage` dispatch ──

/-- Encode a dynamic image as PNG if possible, given a `deflate` function.
    Supported formats: `y8`, `y16`, `ya8`, `ya16`, `rgb8`, `rgb16`, `rgba8`,
    `rgba16` — exactly upstream's `encodeDynamicPng` coverage. -/
def encodeDynamicPngUsing (deflate : ByteArray → ByteArray) : DynamicImage → Except String Data.ByteString
  | .rgb8 img => .ok (encodePngUsing deflate img)
  | .rgba8 img => .ok (encodePngUsing deflate img)
  | .y8 img => .ok (encodePngUsing deflate img)
  | .y16 img => .ok (encodePngUsing deflate img)
  | .ya8 img => .ok (encodePngUsing deflate img)
  | .ya16 img => .ok (encodePngUsing deflate img)
  | .rgb16 img => .ok (encodePngUsing deflate img)
  | .rgba16 img => .ok (encodePngUsing deflate img)
  | _ => .error "Unsupported image format for PNG export"

end Codec.Picture
