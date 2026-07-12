import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Metadata
import Linen.Codec.Picture.Png.Internal.Type
import Linen.Codec.Picture.Png.Internal.Metadata
import Linen.Codec.Picture.Png.Internal.Export
import Linen.Crypto.Zlib.FFI
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.Png` (top-level) from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 14 of 29): the
  decode-side scanline unfiltering, Adam7 de-interlacing, and bit-depth
  unpacking that turn an inflated `IDAT` byte stream into a `DynamicImage`/
  `PalettedImage`, plus the top-level `decodePng*`/`encodePng*` entry points
  wiring modules #11–#13 together with real zlib inflate/deflate
  (`Linen.Crypto.Zlib`).

  ## The `IO`-vs-pure decision

  This library's other image codecs (`decodeHDR`, `decodeTga`, ...) expose
  **pure** `Except String X`-returning decoders. But `Linen.Crypto.Zlib`'s
  `decompress`/`compress` are `IO ByteArray` (they wrap a persistent,
  finalizer-managed `z_stream *` handle via FFI — see
  `Linen.Crypto.Zlib.FFI`'s own module doc-comment), and there is a **direct
  precedent already in this codebase** for how to bridge that into a
  higher-level API: `Linen.Data.PDF.Stream.decompress` (`Data.PDF.Stream`)
  calls `Crypto.Zlib.decompress` and simply stays `IO` itself, rather than
  reaching for `unsafeIO`/`unsafePerformIO`-style pure extraction. This
  module follows that precedent exactly: every top-level function that
  touches zlib — `decodePng`, `decodePngWithMetadata`,
  `decodePngWithPaletteAndMetadata`, `encodePng`, `encodePngWithMetadata`,
  `encodePalettedPng`, `encodePalettedPngWithMetadata`, `encodeDynamicPng` —
  is itself `IO`-returning (`IO (Except String X)` or, where nothing else can
  fail, plain `IO X`). This is a deliberate, documented deviation from
  `decodeHDR`/`decodeTga`'s pure signatures, not an oversight: unlike those
  formats, PNG's payload is compressed, and there is no honest way to expose
  "decompress a `z_stream`" as a pure function in this codebase without
  inventing a *new* unsafe-extraction convention this codebase's own zlib
  caller (`Data.PDF.Stream`) doesn't use either.

  **A universe wrinkle this forces on the decode-with-metadata API.**
  `Metadatas` (`Linen.Codec.Picture.Metadata`) bundles an existential
  `Elem` — `{α : Type}` plus a `Keys α` and an `α` — so `Metadatas` itself
  lives in `Type 1`, one universe above ordinary data. Lean's `IO` is
  monomorphic in `Type 0` (`IO : Type → Type`), so a value of type
  `Metadatas` can never appear *inside* `IO`'s result type — `IO (X ×
  Metadatas)` simply does not type-check, independently of anything about
  zlib. But `Metadatas` extraction is itself always pure (`extractMetadatas`
  and `basicMetadata` never touch zlib; only unpacking the actual pixel data
  does), so `decodePngWithPaletteAndMetadata`/`decodePngWithMetadata` return
  `Except String (Metadatas × IO (Except String X))`: the outer `Except`
  covers parse failures found before any decompression is attempted
  (bad signature, bad `IHDR`, ...), pairing the metadata that is
  available immediately with a still-deferred `IO` action for the actual
  image, which can itself still fail (bad zlib stream, unsupported bit
  depth, ...). `decodePng` (which drops metadata entirely) has no such
  wrinkle and keeps the plain `IO (Except String DynamicImage)` shape.

  ## Scanline unfiltering and Adam7 de-interlacing

  - `unfilterPass` reverses the Sub/Up/Average/Paeth/None filter tag
    prepended to every scanline (module #13's `Export.lean` only ever
    *writes* `PngFilter.none`, but a decoder must handle whatever a real
    encoder produced) via a single `Id.run` double loop: the outer loop
    walks scanlines top-to-bottom carrying the previous row's already-
    unfiltered bytes (needed by `Up`/`Average`/`Paeth`), the inner loop
    walks that scanline's bytes left-to-right carrying the current row's
    own already-unfiltered prefix (needed by `Sub`/`Average`/`Paeth`) —
    exactly `pngFiltering`'s upstream recursion, restated as two bounded
    `for` loops since every row consumes a statically-known `byteWidth + 1`
    bytes and every column advances the read index by exactly one byte,
    with no data-dependent step size. An out-of-range filter-tag byte falls
    back to `PngFilter.none`, matching upstream's own `case _ -> filterNone`
    fallback in `pngFiltering`'s `lineFilter` (not an error).

  - Adam7 de-interlacing reuses the exact same `unfilterPass` per pass (each
    pass's scanlines are filtered independently, with their own zeroed
    "previous row" at the pass boundary, matching upstream's fresh
    `M.replicate` per `pngFiltering` call) and then scatters that pass's
    unpacked samples into the full-size output using upstream's own
    `adam7MatrixInfo` start-row/start-col/row-increment/column-increment
    tables (`adam7StartRow`/`adam7StartCol`/`adam7RowIncr`/`adam7ColIncr`
    below) — a fixed, 7-entry, compile-time-known table, so the whole
    pass/row/column walk is three nested bounded `for` loops
    (`decodeSamplesGeneric`) with no recursion or termination proof needed.
    The non-interlaced case is simply the one-pass table `[(0, 0, 1, 1)]`,
    so both interlacing methods share one code path.

  - **Adam7 is decode-only, matching upstream.** Upstream's own encoder
    (`Codec.Picture.Png.Internal.Export`'s `genericEncodePng`, ported as
    module #13's `preparePngHeader`) always declares `PngNoInterlace` and
    never interleaves its output — there is no "encode-side Adam7" to port.
    `encodePng`/`encodePalettedPng` below inherit that from module #13
    unchanged.

  ## Bit-depth unpacking

  All five colour types (`greyscale`, `greyscaleWithAlpha`, `trueColour`,
  `trueColourWithAlpha`, `indexedColor`) and every bit depth the PNG spec
  allows for each (`1`/`2`/`4`/`8` for greyscale and indexed, `8`/`16` for
  every other type) are covered — the ten combinations upstream's own
  `unparse` handles. `unpackComponentNat` is one generic function covering
  all of them: for `bitDepth = 8` it reads a component directly, for
  `bitDepth = 16` it reads a big-endian pair, and for `bitDepth ∈ {1, 2, 4}`
  (only ever reached with `sampleCount = 1`, since only greyscale/indexed
  images ever use a sub-byte depth) it extracts the MSB-first packed sample
  — the same arithmetic upstream's `bitUnpacker`/`twoBitsUnpacker`/
  `halfByteUnpacker` hand-roll separately per depth.

  Greyscale images with `bitDepth < 8` are converted to an indexed
  (`PalettedImage`) representation using a synthetic grey ramp palette
  (`generateGreyscalePalette`), exactly matching upstream's own `unparse`
  recursing into the `PngIndexedColor` branch for those depths.

  ## `PalettedImage`/`tRNS` and the RGB8-only-palette deviation

  `Linen.Codec.Picture.Types`'s `PalettedImage` (module #1) always carries an
  RGB8 palette plus a `hasAlpha : Bool` flag — unlike upstream's genuine
  `PalettedRGB8`/`PalettedRGBA8` sum, it has no slot for storing the
  per-index alpha a `tRNS` chunk contributes. This is not a new decision:
  `Linen.Codec.Picture.Tga`'s and `Linen.Codec.Picture.Bitmap`'s
  colour-mapped decode paths already hit the same wall and resolved it the
  same way (see `Tga.lean`'s own module doc-comment) — drop the alpha
  values, but still set `hasAlpha := true` so a caller knows the source
  image declared per-index transparency it can no longer recover from the
  returned `PalettedImage`. This module's indexed-colour decode path follows
  that exact precedent for a PNG `tRNS` chunk paired with a `PLTE` chunk.

  ## What is still out of scope

  - `zTXt` metadata remains unextracted, exactly as module #12's own
    doc-comment already documents and defers (`extractMetadatas` never looks
    at a `zTXt` chunk); wiring it in is a small, independent follow-up to
    `Linen.Codec.Picture.Png.Internal.Metadata`, not something this module's
    own scope requires touching.
  - `writePng`/`writeDynamicPng` (trivial `IO` file-writers) are dropped,
    matching every other codec module's convention of leaving file I/O to
    the caller.
-/

namespace Codec.Picture

open Data.ByteString (Builder)

-- ── Adam7 pass tables ──

/-- The seven Adam7 passes' starting row, in image-row units. -/
private def adam7StartRow : Array Nat := #[0, 0, 4, 0, 2, 0, 1]

/-- The seven Adam7 passes' starting column, in image-column units. -/
private def adam7StartCol : Array Nat := #[0, 4, 0, 2, 0, 1, 0]

/-- The seven Adam7 passes' row increment (vertical stride between two
    consecutive rows of the *same* pass). -/
private def adam7RowIncr : Array Nat := #[8, 8, 8, 4, 4, 2, 2]

/-- The seven Adam7 passes' column increment (horizontal stride between two
    consecutive pixels of the *same* pass). -/
private def adam7ColIncr : Array Nat := #[8, 8, 4, 4, 2, 2, 1]

/-- One decode pass: `(startRow, startCol, rowIncrement, colIncrement)`. -/
private abbrev PassInfo := Nat × Nat × Nat × Nat

/-- The seven Adam7 passes, in file order. -/
private def adam7Passes : List PassInfo :=
  (List.range 7).map (fun i =>
    (adam7StartRow.getD i 0, adam7StartCol.getD i 0, adam7RowIncr.getD i 1, adam7ColIncr.getD i 1))

/-- The pass table to use for a given `IHDR` interlace method: a single,
    identity pass for `noInterlace`, or the seven Adam7 passes. -/
private def passesOf : PngInterlaceMethod → List PassInfo
  | .noInterlace => [(0, 0, 1, 1)]
  | .interlaceAdam7 => adam7Passes

/-- The number of pixels a pass covers along one dimension of size `dim`,
    starting at `begin` and stepping by `incr` (upstream's `sizer`): `0` if
    the pass starts past the end of the dimension, otherwise `⌈(dim - begin)
    / incr⌉`. -/
private def adam7PassSize (dim begin' incr : Nat) : Nat :=
  if dim ≤ begin' then 0 else (dim - begin' + incr - 1) / incr

/-- The number of bytes one packed scanline of `dim` samples at `bitDepth`
    bits, `sampleCount` samples per pixel, occupies (upstream's
    `byteSizeOfBitLength`): `⌈bitDepth * sampleCount * dim / 8⌉`. -/
private def byteWidthOf (bitDepth sampleCount dim : Nat) : Nat :=
  (bitDepth * sampleCount * dim + 7) / 8

-- ── Scanline unfiltering ──

/-- An out-of-range filter-tag byte falls back to `PngFilter.none`, matching
    upstream's own `case _ -> filterNone` fallback (not an error). -/
private def filterOfTag (b : UInt8) : PngFilter :=
  match pngFilterOfCode b with
  | .ok f => f
  | .error _ => .none

/-- The PNG Paeth predictor (Annex A.4 / W3C recommendation): whichever of
    `a`, `b`, `a + b - c` is closest to the true gradient prediction. -/
private def paethPredictor (a b c : UInt8) : UInt8 :=
  let a' : Int := a.toNat
  let b' : Int := b.toNat
  let c' : Int := c.toNat
  let p := a' + b' - c'
  let pa := (p - a').natAbs
  let pb := (p - b').natAbs
  let pc := (p - c').natAbs
  if pa ≤ pb ∧ pa ≤ pc then a else if pb ≤ pc then b else c

/-- Reverse the per-scanline filter tag/prediction for `rowCount` scanlines
    of `byteWidth` bytes each (plus their leading filter-tag byte), starting
    at `offset` in `bytes`. `stride` is the distance (in bytes) back to the
    "same-component" predictor byte (`1` for sub-byte bit depths, matching
    upstream's `strideInfo`; `sampleCount * (bitDepth / 8)` otherwise).
    Returns the flat `rowCount * byteWidth` unfiltered bytes, plus the
    offset just past the consumed input. -/
private def unfilterPass (stride byteWidth rowCount : Nat) (bytes : ByteArray) (offset : Nat) :
    Except String (ByteArray × Nat) :=
  if byteWidth == 0 ∨ rowCount == 0 then
    .ok (ByteArray.empty, offset)
  else if offset + rowCount * (byteWidth + 1) > bytes.size then
    .error "Truncated PNG scanline data"
  else
    let out := Id.run do
      let mut prevRow : ByteArray := ByteArray.mk (Array.replicate byteWidth 0)
      let mut out : ByteArray := ByteArray.mk (Array.replicate (rowCount * byteWidth) 0)
      let mut pos := offset
      for row in [0:rowCount] do
        let filt := filterOfTag (bytes.get! pos)
        pos := pos + 1
        let mut curRow : ByteArray := ByteArray.mk (Array.replicate byteWidth 0)
        for idx in [0:byteWidth] do
          let x := bytes.get! (pos + idx)
          let a := if idx < stride then 0 else curRow.get! (idx - stride)
          let b := prevRow.get! idx
          let c := if idx < stride then 0 else prevRow.get! (idx - stride)
          let v : UInt8 :=
            match filt with
            | .none => x
            | .sub => x + a
            | .up => x + b
            | .average => x + UInt8.ofNat ((a.toNat + b.toNat) / 2)
            | .paeth => x + paethPredictor a b c
          curRow := curRow.set! idx v
        pos := pos + byteWidth
        for idx in [0:byteWidth] do
          out := out.set! (row * byteWidth + idx) (curRow.get! idx)
        prevRow := curRow
      pure out
    .ok (out, offset + rowCount * (byteWidth + 1))

-- ── Bit-depth unpacking ──

/-- Read the `sample`-th component (`0`-based) of the `pixelIndex`-th pixel
    (`0`-based) of an unfiltered scanline `row`, for `bitDepth` bits per
    component and `sampleCount` components per pixel. Covers all five PNG
    bit depths (`1`/`2`/`4`/`8`/`16`); `1`/`2`/`4` are only ever reached with
    `sampleCount = 1` (greyscale/indexed), matching the PNG specification. -/
private def unpackComponentNat (bitDepth sampleCount : Nat) (row : ByteArray)
    (pixelIndex sample : Nat) : Nat :=
  if bitDepth == 16 then
    let ci := pixelIndex * sampleCount + sample
    let hi := (row.get! (2 * ci)).toNat
    let lo := (row.get! (2 * ci + 1)).toNat
    hi * 256 + lo
  else if bitDepth == 8 then
    (row.get! (pixelIndex * sampleCount + sample)).toNat
  else
    let samplesPerByte := 8 / bitDepth
    let byteIdx := pixelIndex / samplesPerByte
    let sampleInByte := pixelIndex % samplesPerByte
    let shift := bitDepth * (samplesPerByte - 1 - sampleInByte)
    ((row.get! byteIdx).toNat / 2 ^ shift) % (2 ^ bitDepth)

/-- Unfilter and unpack every pass of `passes` (a single identity pass for a
    non-interlaced image, the seven Adam7 passes otherwise) from `bytes`,
    scattering every pass's samples into their final position in a flat
    `width * height * sampleCount` array of component values (as `Nat`, so a
    single implementation covers both 8- and 16-bit depths uniformly; the
    caller narrows to `UInt8`/`UInt16` once the colour type/bit depth is
    known). -/
private def decodeSamplesGeneric (bitDepth sampleCount width height : Nat) (passes : List PassInfo)
    (bytes : ByteArray) : Except String (Array Nat) :=
  let stride := if bitDepth < 8 then 1 else sampleCount * (bitDepth / 8)
  let result := Id.run do
    let mut out : Array Nat := Array.replicate (width * height * sampleCount) 0
    let mut offset := 0
    let mut err : Option String := none
    for p in passes do
      if err.isNone then
        let (startRow, startCol, rowIncr, colIncr) := p
        let passWidth := adam7PassSize width startCol colIncr
        let passHeight := adam7PassSize height startRow rowIncr
        if passWidth > 0 ∧ passHeight > 0 then
          let byteWidth := byteWidthOf bitDepth sampleCount passWidth
          match unfilterPass stride byteWidth passHeight bytes offset with
          | .error e => err := some e
          | .ok (unfiltered, newOffset) =>
            offset := newOffset
            for rowInPass in [0:passHeight] do
              let rowArr := unfiltered.extract (rowInPass * byteWidth) (rowInPass * byteWidth + byteWidth)
              let realRow := startRow + rowInPass * rowIncr
              for pixelIndex in [0:passWidth] do
                let realCol := startCol + pixelIndex * colIncr
                for sample in [0:sampleCount] do
                  let v := unpackComponentNat bitDepth sampleCount rowArr pixelIndex sample
                  out := out.set! ((realRow * width + realCol) * sampleCount + sample) v
    pure (out, err)
  match result.2 with
  | some e => .error e
  | none => .ok result.1

-- ── Synthetic grey-ramp palette (for greyscale images with `bitDepth < 8`) ──

/-- A synthetic RGB8 palette of `2 ^ bits` grey ramp entries, matching
    upstream's `generateGreyscalePalette`: entry `i` is `(v, v, v)` with
    `v = i * (255 / (2 ^ bits - 1))`. Used to reinterpret a `bitDepth < 8`
    greyscale image as an indexed one, exactly as upstream's `unparse`
    recurses into the `PngIndexedColor` branch for those depths. -/
private def generateGreyscalePalette (bits : Nat) : Palette :=
  let maxValue := 2 ^ bits - 1
  generateImage
    (fun x _ =>
      let v : UInt8 := UInt8.ofNat (x * (255 / maxValue))
      (⟨v, v, v⟩ : PixelRGB8))
    (maxValue + 1) 1

-- ── Assembling a `DynamicImage`/`PalettedImage` from decoded samples ──

/-- Every colour type's component count (upstream's
    `sampleCountOfImageType`). -/
private def sampleCountOf : PngImageType → Nat
  | .greyscale => 1
  | .trueColour => 3
  | .indexedColor => 1
  | .greyscaleWithAlpha => 2
  | .trueColourWithAlpha => 4

/-- Turn an already-inflated `IDAT` byte stream into a `DynamicImage` (plain
    colour types) or `PalettedImage` (indexed colour, or greyscale with
    `bitDepth < 8` — see the module doc-comment), given the image's `IHDR`,
    its `PLTE` chunk if any, and its `tRNS` chunk payload(s) if any
    (upstream's `unparse`). -/
private def unparsePngImage (ihdr : PngIHdr) (palette : Option PngRawChunk)
    (transparency : List ByteArray) (inflated : ByteArray) :
    Except String (Sum DynamicImage PalettedImage) := do
  let w := ihdr.width.toNat
  let h := ihdr.height.toNat
  let depth := ihdr.bitDepth.toNat
  let passes := passesOf ihdr.interlaceMethod
  match ihdr.colourType with
  | .greyscale =>
    if depth == 1 ∨ depth == 2 ∨ depth == 4 then
      let samples ← decodeSamplesGeneric depth 1 w h passes inflated
      pure (.inr { indexedImage := { width := w, height := h, data := samples.map UInt8.ofNat },
                   palette := generateGreyscalePalette depth, hasAlpha := false })
    else if depth == 8 then
      let samples ← decodeSamplesGeneric 8 1 w h passes inflated
      pure (.inl (.y8 { width := w, height := h, data := samples.map UInt8.ofNat }))
    else if depth == 16 then
      let samples ← decodeSamplesGeneric 16 1 w h passes inflated
      pure (.inl (.y16 { width := w, height := h, data := samples.map UInt16.ofNat }))
    else throw "Invalid bit depth for greyscale PNG"
  | .trueColour =>
    if depth == 8 then
      let samples ← decodeSamplesGeneric 8 3 w h passes inflated
      pure (.inl (.rgb8 { width := w, height := h, data := samples.map UInt8.ofNat }))
    else if depth == 16 then
      let samples ← decodeSamplesGeneric 16 3 w h passes inflated
      pure (.inl (.rgb16 { width := w, height := h, data := samples.map UInt16.ofNat }))
    else throw "Invalid bit depth for truecolour PNG"
  | .greyscaleWithAlpha =>
    if depth == 8 then
      let samples ← decodeSamplesGeneric 8 2 w h passes inflated
      pure (.inl (.ya8 { width := w, height := h, data := samples.map UInt8.ofNat }))
    else if depth == 16 then
      let samples ← decodeSamplesGeneric 16 2 w h passes inflated
      pure (.inl (.ya16 { width := w, height := h, data := samples.map UInt16.ofNat }))
    else throw "Invalid bit depth for greyscale-with-alpha PNG"
  | .trueColourWithAlpha =>
    if depth == 8 then
      let samples ← decodeSamplesGeneric 8 4 w h passes inflated
      pure (.inl (.rgba8 { width := w, height := h, data := samples.map UInt8.ofNat }))
    else if depth == 16 then
      let samples ← decodeSamplesGeneric 16 4 w h passes inflated
      pure (.inl (.rgba16 { width := w, height := h, data := samples.map UInt16.ofNat }))
    else throw "Invalid bit depth for truecolour-with-alpha PNG"
  | .indexedColor =>
    match palette with
    | none => throw "no valid palette found"
    | some plteChunk =>
      let pal ← parsePalette plteChunk
      let samples ← decodeSamplesGeneric depth 1 w h passes inflated
      pure (.inr { indexedImage := { width := w, height := h, data := samples.map UInt8.ofNat },
                   palette := pal, hasAlpha := !transparency.isEmpty })

-- ── Top-level decode ──

/-- Decode a PNG file with, possibly, separated palette: `Sum.inl` for a
    plain colour image, `Sum.inr` for an indexed (or `bitDepth < 8`
    greyscale) one (upstream's `decodePngWithPaletteAndMetadata`). Also
    extracts every `pHYs`/`gAMA`/`tEXt`-derived metadata found (module #12),
    plus this image's basic width/height/format metadata. Returns
    `Except String (Metadatas × IO (Except String X))` rather than
    `IO (Except String (X × Metadatas))` — see the module doc-comment's
    "universe wrinkle" section for why `Metadatas` can never appear inside
    `IO`'s result type at all, regardless of the zlib/`IO` decision. -/
def decodePngWithPaletteAndMetadata (input : ByteArray) :
    Except String (Metadatas × IO (Except String (Sum DynamicImage PalettedImage))) :=
  match parseRawPngImage input.toList with
  | .error e => .error e
  | .ok rawImg =>
    let ihdr := rawImg.header
    let metas := (basicMetadata .png ihdr.width.toNat ihdr.height.toNat).union (extractMetadatas rawImg)
    let action : IO (Except String (Sum DynamicImage PalettedImage)) := do
      let compressed := (chunksWithSig rawImg iDATSignature).foldl (· ++ ·) ByteArray.empty
      -- `1` compression-method byte + `1` flags byte + `4`-byte trailing
      -- CRC, the minimum a genuinely nonempty zlib stream can take up
      -- (upstream's own `zlibHeaderSize` check).
      if compressed.size ≤ 6 then
        pure (.error "Invalid data size")
      else
        let inflated ← Crypto.Zlib.decompress compressed
        let paletteChunk := rawImg.chunks.find? (fun c => c.chunkType == pLTESignature)
        let transparency := chunksWithSig rawImg tRNSSignature
        pure (unparsePngImage ihdr paletteChunk transparency inflated)
    .ok (metas, action)

/-- Decode a PNG file, collapsing any indexed result down to a true-colour
    image via `palettedToTrueColor` (upstream's `decodePngWithMetadata`). -/
def decodePngWithMetadata (input : ByteArray) :
    Except String (Metadatas × IO (Except String DynamicImage)) :=
  match decodePngWithPaletteAndMetadata input with
  | .error e => .error e
  | .ok (metas, action) =>
    .ok (metas, do
      match ← action with
      | .error e => pure (.error e)
      | .ok (.inl img) => pure (.ok img)
      | .ok (.inr pal) => pure (.ok (.rgb8 (palettedToTrueColor pal))))

/-- Decode a PNG file, discarding its metadata (upstream's `decodePng`). No
    universe wrinkle here — with no `Metadatas` in the result, this keeps
    the plain `IO (Except String DynamicImage)` shape. -/
def decodePng (input : ByteArray) : IO (Except String DynamicImage) :=
  match decodePngWithMetadata input with
  | .error e => pure (.error e)
  | .ok (_, action) => action

-- ── Top-level encode ──

/-- Encode `img` as a PNG file with metadata, deflating its raw scanline
    bytes (module #13's `pngRawScanlines`) via `Crypto.Zlib.compress`.
    `IO`-returning for the same reason `decodePng*` is — see the module
    doc-comment. -/
def encodePngWithMetadata {α Component : Type} [Pixel α Component] [PngSavable α]
    (metas : Metadatas) (img : @Image α Component _) : IO Data.ByteString := do
  let compressed ← Crypto.Zlib.compress (PngSavable.pngRawScanlines (α := α) img)
  pure (encodePngWithMetadataUsing (fun _ => compressed) metas img)

/-- `encodePngWithMetadata` with no metadata (upstream's `encodePng`, one
    instance per `PngSavable` type upstream). -/
def encodePng {α Component : Type} [Pixel α Component] [PngSavable α] (img : @Image α Component _) :
    IO Data.ByteString :=
  encodePngWithMetadata Metadatas.empty img

/-- Encode `img` (a `Pixel8`-indexed image) against palette `pal` as a
    colour-indexed PNG file with metadata, deflating its raw scanline bytes
    via `Crypto.Zlib.compress`. Fails under the same conditions as module
    #13's `encodePalettedPngWithMetadataUsing` (invalid palette size, or an
    index absent from the palette). -/
def encodePalettedPngWithMetadata {α : Type} [Pixel α UInt8] [PngPaletteSaveable α]
    (metas : Metadatas) (pal : Image α) (img : Image Pixel8) : IO (Except String Data.ByteString) := do
  let compressed ← Crypto.Zlib.compress (rawScanlineBytes8 img)
  pure (PngPaletteSaveable.encodePalettedPngWithMetadataUsing (fun _ => compressed) metas pal img)

/-- `encodePalettedPngWithMetadata` with no metadata (upstream's
    `encodePalettedPng`). -/
def encodePalettedPng {α : Type} [Pixel α UInt8] [PngPaletteSaveable α] (pal : Image α)
    (img : Image Pixel8) : IO (Except String Data.ByteString) :=
  encodePalettedPngWithMetadata Metadatas.empty pal img

/-- Encode a dynamic image as PNG if possible (upstream's `encodeDynamicPng`).
    Supported formats: `y8`, `y16`, `ya8`, `ya16`, `rgb8`, `rgb16`, `rgba8`,
    `rgba16` — exactly module #13's `encodeDynamicPngUsing` coverage. -/
def encodeDynamicPng : DynamicImage → IO (Except String Data.ByteString)
  | .rgb8 img => Except.ok <$> encodePng img
  | .rgba8 img => Except.ok <$> encodePng img
  | .y8 img => Except.ok <$> encodePng img
  | .y16 img => Except.ok <$> encodePng img
  | .ya8 img => Except.ok <$> encodePng img
  | .ya16 img => Except.ok <$> encodePng img
  | .rgb16 img => Except.ok <$> encodePng img
  | .rgba16 img => Except.ok <$> encodePng img
  | _ => pure (.error "Unsupported image format for PNG export")

end Codec.Picture
