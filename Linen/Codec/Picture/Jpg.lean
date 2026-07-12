import Linen.Codec.Picture.BitWriter
import Linen.Codec.Picture.Jpg.Internal.Types
import Linen.Codec.Picture.Jpg.Internal.Common
import Linen.Codec.Picture.Jpg.Internal.Progressive
import Linen.Codec.Picture.Jpg.Internal.DefaultTable
import Linen.Codec.Picture.Jpg.Internal.FastDct
import Linen.Codec.Picture.Jpg.Internal.Metadata
import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Metadata
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.Jpg` (top-level, **not** any `Internal.*` module)
  from the `JuicyPixels` package (see `docs/imports/JuicyPixels/dependencies.md`,
  module 27 of 29): the public JPEG decode/encode entry points
  (`decodeJpeg`/`decodeJpegWithMetadata`/`encodeJpeg`/`encodeJpegAtQuality`/
  `encodeJpegAtQualityWithMetadata`/`encodeDirectJpegAtQualityWithMetadata`)
  and the `JpgEncodable` typeclass, tying together modules 21–26 (`Types`,
  `Common`, `FastDct`/`FastIdct`, `Metadata`, `Progressive`): marker-structure
  parsing/writing, baseline (`SOF0`) vs. progressive (`SOF2`) frame dispatch,
  chroma-subsampled image reconstruction, and Adobe/JFIF/Exif metadata.

  ## Design and scope decisions

  - **Pure (`Except String α`), not `IO`.** Unlike `Png.lean` (which needs
    `IO` for `zlib`), JPEG decoding here needs no external codec: every
    building block (`Common.lean`'s Huffman/IDCT pipeline, `Progressive.lean`'s
    scan-accumulator machinery) is already pure, matching `Tiff.lean`/
    `Gif.lean`/`Progressive.lean`'s own precedent.
  - **Chroma upsampling always goes through the one generic,
    already-bounds-checked `Common.unpackMacroBlock`.** Upstream's
    `unpack444Y`/`unpack444Ycbcr`/`unpack421Ycbcr` are perf-only
    specializations of the same generic `unpackMacroBlock` fallback upstream
    itself falls back to at every MCU-row/column boundary; `Progressive.lean`
    already established the precedent of dropping them entirely for this
    port. `decodeBaselineImage` below does the same, so there is exactly one
    pixel-unpacking code path for both baseline and progressive decode.
  - **Baseline decode reuses `Progressive.lean`'s `JpgUnpackerParameter`,
    `decodeFirstDC`, and `decodeFirstAc` directly**, rather than re-deriving a
    simpler one-entry-per-component parameter shape. Upstream's own
    `scanSpecifier` shows why: whenever a scan interleaves more than one
    component (`scanCount > 1`, the common case for a baseline image's single
    combined luma+chroma scan) it expands to *one parameter per
    (component, sub-block)* pair, exactly like a progressive scan — a
    one-entry-per-component shape would silently mis-decode any baseline
    image whose components have sampling factors greater than `1` (e.g. an
    ordinary 4:2:0 baseline JPEG). Baseline's per-block step is
    `decodeFirstDC` immediately followed by `decodeFirstAc` on the same
    parameter with `coefficientRange` overridden to start at `1` (baseline
    never re-decodes the DC coefficient as an AC one, and never carries an
    end-of-band run across blocks, so `eobrun` is always fed/discarded as
    `0`) — upstream's own dedicated `decompressMacroBlock`/
    `acCoefficientsDecode` collapse into that same two-call sequence.
  - **Colour space.** YCbCr (3 components, the common case), grayscale/`Y`
    (1 component), grayscale+alpha/`YA` (2 components), RGB (3 components, no
    chroma subsampling), and CMYK/YCCK (4 components, via an Adobe `APP14`
    marker) are all dispatched exactly as upstream's
    `colorSpaceOfComponentStr`/`colorSpaceOfAdobe`/`dynamicOfColorSpace` do.
    **`ycckArrayToCmyk` replaces upstream's
    `ColorSpaceConvertible PixelYCbCrK8 PixelCMYK8` instance** (which this
    port's `Types.lean` never introduced — `PixelYCbCrK8` exists there only
    as a `Pixel` instance, with no conversion instances at all): upstream's
    own instance is, once its every-byte pre-inversion step is accounted for,
    mathematically identical to this port's already-existing
    `ColorSpaceConvertible PixelYCbCr8 PixelRGB8` instance (`1.402 * 128 =
    179.456`, etc.), so `ycckArrayToCmyk` inverts the four raw bytes, reuses
    that existing instance's `convertPixel` for the YCbCr→RGB step, then
    inverts the RGB result into CMY (leaving the already-inverted `K`
    unchanged) directly on the flat pixel-component array — the same net
    effect as upstream's `Image`-level `convertImage`, without needing a new
    `Image`-level `convertImage` combinator or a second near-duplicate
    conversion formula.
  - **`Image PixelYCbCr8` as a generic flat byte buffer.** Exactly as
    upstream's `decodeImage`/`progressiveUnpack` write into a
    `MutableImage s PixelYCbCr8` whose declared "3 components" is irrelevant
    to `unpackMacroBlock`'s explicit `compCount` parameter (a 1-, 3-, or
    4-component image is decoded into the very same buffer type, with
    `compCount` threaded at every call site instead of trusted from the
    pixel type), this port's `decodeBaselineImage`/`Progressive.progressiveUnpack`
    both produce an `Image PixelYCbCr8` used purely as a `width × height`
    `Array UInt8` buffer of `compCount * width * height` bytes. Once decode
    finishes, `dynamicOfColorSpace` below re-wraps that same `.data` array,
    unchanged, into whichever concrete `Image` type the detected colour space
    actually calls for (`Image Pixel8`, `Image PixelYA8`, `Image PixelRGB8`,
    `Image PixelCMYK8`, or `Image PixelYCbCr8` itself) — matching upstream's
    own reuse of one raw `VS.Vector Word8` across every `dynamicOfColorSpace`
    branch.
  - **`decodeRestartInterval`'s call sites are ported for their
    byte-alignment/counter-reset *scaffolding* only.** As documented in
    `Common.lean`'s own doc-comment, `decodeRestartInterval` is upstream's own
    dead code (it unconditionally returns `-1`; the real "detect and skip an
    `RSTn` marker" implementation is commented out in `Common.hs` itself).
    `decodeBaselineImage` below calls it at the same point upstream's
    `decodeImage` does, for the same reason `Progressive.lean` already does:
    faithfully porting upstream's *observable* behaviour, not upstream's
    intent.
  - **The encoder never emits a restart interval.** `encodeDirectJpegAtQualityWithMetadata`
    below never writes a `DRI` marker or splits its output into multiple
    scan blobs, matching upstream's own `encodeDirectJpegAtQualityWithMetadata`
    (which likewise never constructs a `JpgIntervalRestart` frame or calls
    `decodeRestartInterval`'s writer-side counterpart). This is upstream's
    own scope, not a reduction introduced here.
  - **JPEG byte-stuffing (`0xFF` → `0xFF 0x00`) is already handled inside
    `BitWriter.writeBits'`** (see its own `dumpByteMSB` helper), so
    `serializeMacroBlock`/`finalizeBoolWriter` below need no separate
    stuffing pass — the entropy-coded bytes `finalizeBoolWriter` returns are
    already exactly what a `JpgFrame.scanBlob`'s `ecs` field expects.
  - **Canonical Huffman *writer* codes (`HuffmanWriterCode`) are derived
    directly from a `HuffmanTable`** by the standard JPEG Annex C linear-code
    procedure (`huffmanWriterCodeOfTable`: walk code lengths `1..16`
    low-to-high, assigning consecutive codes within each length and left
    -shifting the running code between lengths), rather than by building
    upstream's `HuffmanPackedTree` and inverting it (`makeInverseTable`).
    `DefaultTable.lean` already deliberately dropped both of those for this
    port's decode side; the two procedures produce identical `(bitCount,
    code)` pairs for any length-sorted `HuffmanTable`; deriving them directly
    avoids reintroducing either.
-/

namespace Codec.Picture

open Codec.Picture.Jpg.Internal
open Data.ByteString (Builder)

-- ── Colour space ──

/-- The colour space a decoded frame's component identifiers (and, if
    present, its Adobe `APP14` marker) resolve to. Ports upstream's
    `JpgColorSpace`. -/
inductive JpgColorSpace where
  | y
  | ya
  | ycbcr
  | ycc
  | rgb
  | ycca
  | rgba
  | cmyk
  | ycck
  deriving Repr, DecidableEq

/-- Guess a colour space from a frame's raw component-identifier bytes,
    treating them the way upstream treats its `String` (`"RGB"`, `"YCbCr"`,
    …) special cases before falling back to a plain component-count guess.
    Ports upstream's `colorSpaceOfComponentStr`. -/
def colorSpaceOfComponentStr (ids : List UInt8) : Option JpgColorSpace :=
  match ids with
  | [_] => some .y
  | [_, _] => some .ya
  | [0, 1, 2] => some .ycbcr
  | [1, 2, 3] => some .ycbcr
  | [82, 71, 66] => some .rgb -- "RGB"
  | [89, 67, 99] => some .ycc -- "YCc"
  | [_, _, _] => some .ycbcr
  | [82, 71, 66, 65] => some .rgba -- "RGBA"
  | [89, 67, 99, 65] => some .ycca -- "YCcA"
  | [67, 77, 89, 75] => some .cmyk -- "CMYK"
  | [89, 67, 99, 75] => some .ycck -- "YCcK"
  | [_, _, _, _] => some .cmyk
  | _ => none

/-- Resolve a colour space from an Adobe `APP14` marker's declared component
    count and transform. Ports upstream's `colorSpaceOfAdobe`. -/
def colorSpaceOfAdobe (compCount : Nat) (app : JpgAdobeApp14) : Option JpgColorSpace :=
  match compCount, app.colorTransform with
  | 3, .ycbcr => some .ycbcr
  | 1, .unknown => some .y
  | 3, .unknown => some .rgb
  | 4, .ycck => some .ycck
  | _, _ => none

-- ── Decoder state ──

/-- Decoding state threaded across every `JpgFrame` in a file: the current
    Huffman/quantization tables, the current frame header and component
    index mapping, and enough bookkeeping (`isProgressive`, the maximum
    sampling resolution, `seenBlobs`) to build each scan blob's
    `JpgUnpackerParameter` list. Ports upstream's `JpgDecoderState`.
    `app1ExifMarker` is dropped: `Jpg.Internal.Metadata.extractJpgMetadatas`
    (module 25) scans a `JpgImage`'s whole frame list directly for `appFrame
    1` Exif segments instead of this state accumulating one as it walks the
    frames, so no field is needed here to carry it. -/
structure JpgDecoderState where
  dcDecoderTables : Array HuffmanTree
  acDecoderTables : Array HuffmanTree
  quantizationMatrices : Array (MacroBlock Int16)
  currentRestartInterv : Int
  currentFrame : Option JpgFrameHeader
  app14Marker : Option JpgAdobeApp14
  app0JFifMarker : Option JpgJFIFApp0
  componentIndexMapping : List (UInt8 × Nat)
  isProgressive : Bool
  maximumHorizontalResolution : Nat
  maximumVerticalResolution : Nat
  seenBlobs : Nat

/-- The initial decoder state: the four default Huffman trees (luma/chroma
    DC/AC) at destinations `0`/`1`/`2`/`3` (upstream seeds `2`/`3` with the
    same luma/chroma trees as `0`/`1`), and quantization tables that default
    to "multiply by `1`". Ports upstream's `emptyDecoderState`. -/
def emptyJpgDecoderState : JpgDecoderState :=
  let dcLuma := buildHuffmanTree defaultDcLumaHuffmanTable
  let dcChroma := buildHuffmanTree defaultDcChromaHuffmanTable
  let acLuma := buildHuffmanTree defaultAcLumaHuffmanTable
  let acChroma := buildHuffmanTree defaultAcChromaHuffmanTable
  { dcDecoderTables := #[dcLuma, dcChroma, dcLuma, dcChroma]
    acDecoderTables := #[acLuma, acChroma, acLuma, acChroma]
    quantizationMatrices := Array.replicate 4 (Array.replicate dctBlockSize (1 : Int16))
    currentRestartInterv := -1
    currentFrame := none
    app14Marker := none
    app0JFifMarker := none
    componentIndexMapping := []
    isProgressive := false
    maximumHorizontalResolution := 0
    maximumVerticalResolution := 0
    seenBlobs := 0 }

private def defaultJpgComponent : JpgComponent :=
  { identifier := 0, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
    quantizationTableDest := 0 }

/-- Build every `JpgUnpackerParameter` one `JpgScanSpecification` within a
    scan blob expands to: `1` entry per component for a lone single-component
    scan, or `horizontalSamplingFactor × verticalSamplingFactor` entries
    (one per sub-block) for any multi-component scan or any progressive scan.
    Ports upstream's `jpgMachineStep (JpgScanBlob …)`'s inner `scanSpecifier`. -/
def scanSpecifierParams (state : JpgDecoderState) (scanCount : Nat) (approxLow approxHigh
    selectionLow selectionHigh blobId : Nat) (scanSpec : JpgScanSpecification) :
    Except String (List JpgUnpackerParameter) := do
  let comp ← match state.componentIndexMapping.lookup scanSpec.componentSelector with
    | some v => pure v
    | none => .error "Jpg decoding error - bad component selector in blob."
  let frameInfo ← match state.currentFrame with
    | some v => pure v
    | none => .error "Jpg decoding error - no previous frame"
  let compDesc := frameInfo.components.getD comp defaultJpgComponent
  let xSampling := compDesc.horizontalSamplingFactor.toNat
  let ySampling := compDesc.verticalSamplingFactor.toNat
  let componentSubSampling :=
    (state.maximumHorizontalResolution - xSampling + 1, state.maximumVerticalResolution - ySampling + 1)
  let (xCount, yCount) :=
    if scanCount > 1 || state.isProgressive then (xSampling, ySampling) else (1, 1)
  let maximumHuffmanTable := 4
  let dcIndex := min (maximumHuffmanTable - 1) scanSpec.dcEntropyCodingTable.toNat
  let acIndex := min (maximumHuffmanTable - 1) scanSpec.acEntropyCodingTable.toNat
  let dcTree := state.dcDecoderTables.getD dcIndex .empty
  let acTree := state.acDecoderTables.getD acIndex .empty
  pure <| Id.run do
    let mut result : List JpgUnpackerParameter := []
    for y in [0:yCount] do
      for x in [0:xCount] do
        result := result ++
          [{ dcHuffmanTree := dcTree, acHuffmanTree := acTree, componentIndex := comp,
             restartInterval := state.currentRestartInterv, componentWidth := xSampling,
             componentHeight := ySampling, subSampling := componentSubSampling,
             successiveApprox := (approxLow, approxHigh), readerIndex := blobId,
             indiceVector := if scanCount == 1 then 0 else 1,
             coefficientRange := (selectionLow, selectionHigh), blockIndex := y * xSampling + x,
             blockMcuX := x, blockMcuY := y }]
    pure result

private def placeHuffmanTree (state : JpgDecoderState) (spec : JpgHuffmanTableSpec) :
    JpgDecoderState :=
  let idx := spec.destination.toNat
  let tree := buildHuffmanTree spec.codes
  match spec.huffmanClass with
  | .dcComponent =>
      if idx < state.dcDecoderTables.size then
        { state with dcDecoderTables := state.dcDecoderTables.set! idx tree } else state
  | .acComponent =>
      if idx < state.acDecoderTables.size then
        { state with acDecoderTables := state.acDecoderTables.set! idx tree } else state

private def placeQuantTable (state : JpgDecoderState) (spec : JpgQuantTableSpec) :
    JpgDecoderState :=
  let idx := spec.destination.toNat
  if idx < state.quantizationMatrices.size then
    { state with quantizationMatrices := state.quantizationMatrices.set! idx spec.quantTable }
  else state

/-- One step of the decoder-state fold: update `state` for one `JpgFrame`,
    and, for a `scanBlob`, also emit its `(params, ecs)` pair. Ports
    upstream's `jpgMachineStep`. -/
def jpgMachineStep (state : JpgDecoderState) (frame : JpgFrame) :
    Except String (JpgDecoderState × Option (List JpgUnpackerParameter × ByteArray)) :=
  match frame with
  | .adobe14Frame app14 => pure ({ state with app14Marker := some app14 }, none)
  | .jfifFrame app0 => pure ({ state with app0JFifMarker := some app0 }, none)
  | .appFrame .. => pure (state, none)
  | .extensionFrame .. => pure (state, none)
  | .quantTableFrame tables => pure (tables.foldl placeQuantTable state, none)
  | .huffmanTableFrame tables => pure (tables.foldl placeHuffmanTree state, none)
  | .intervalRestart v => pure ({ state with currentRestartInterv := (v.toNat : Int) }, none)
  | .scanFrame kind hdr =>
      let compMapping := (hdr.components.zip (List.range hdr.components.length)).map
        (fun (c, ix) => (c.identifier, ix))
      let hs := hdr.components.map (·.horizontalSamplingFactor.toNat)
      let vs := hdr.components.map (·.verticalSamplingFactor.toNat)
      pure ({ state with
        currentFrame := some hdr, componentIndexMapping := compMapping,
        isProgressive := kind == .progressiveDCTHuffman,
        maximumHorizontalResolution := hs.foldl max 0,
        maximumVerticalResolution := vs.foldl max 0 }, none)
  | .scanBlob hdr ecs => do
      let scanCount := hdr.scans.length
      let paramLists ← hdr.scans.mapM
        (scanSpecifierParams state scanCount hdr.successiveApproxLow.toNat
          hdr.successiveApproxHigh.toNat hdr.spectralSelectionStart.toNat
          hdr.spectralSelectionEnd.toNat state.seenBlobs)
      pure ({ state with seenBlobs := state.seenBlobs + 1 }, some (paramLists.flatten, ecs))

/-- Fold `jpgMachineStep` over every frame in a `JpgImage`, collecting the
    final decoder state plus the ordered list of every scan blob's
    `(params, ecs)` pair. Ports upstream's `execRWS (mapM_ jpgMachineStep …)`. -/
def buildJpgDecoderState (frames : List JpgFrame) :
    Except String (JpgDecoderState × List (List JpgUnpackerParameter × ByteArray)) :=
  frames.foldlM
    (fun (st, acc) frame => do
      let (st', scan) ← jpgMachineStep st frame
      pure (st', match scan with | some s => acc ++ [s] | none => acc))
    (emptyJpgDecoderState, [])

-- ── Baseline decode ──

/-- Decode a full baseline-mode JPEG scan sequence into an image. Each scan
    blob's params/`ByteArray` pair is decoded MCU by MCU: every parameter's
    block is fully Huffman-decoded in one pass (`decodeFirstDC` then
    `decodeFirstAc`, discarding both calls' `eobrun` — baseline never uses
    one), dequantized/IDCT'd, and unpacked via the generic
    `Common.unpackMacroBlock` (see the module doc-comment for why no
    specialized fast path is ported). Ports upstream's `decodeImage`. -/
def decodeBaselineImage (frame : JpgFrameHeader) (quants : Array (MacroBlock Int16))
    (scans : List (List JpgUnpackerParameter × ByteArray)) : Image PixelYCbCr8 :=
  Id.run do
    let components := frame.components.toArray
    let compCount := components.size
    let imgWidth := frame.width.toNat
    let imgHeight := frame.height.toNat
    let elementCount := imgWidth * imgHeight * compCount
    let mut img : Image PixelYCbCr8 :=
      { width := imgWidth, height := imgHeight, data := Array.replicate elementCount (0 : UInt8) }
    let mut dcArray : Array Int16 := Array.replicate compCount 0
    let restartIntervalValue : Int :=
      match scans with
      | (p :: _, _) :: _ => p.restartInterval
      | _ => -1
    let mut resetCounter : Int := restartIntervalValue

    for scanEntry in scans do
      let (params, ecs) := scanEntry
      let paramsArr := params.toArray
      let maxiSubSampW := (paramsArr.map (·.subSampling.1)).foldl max 0
      let maxiSubSampH := (paramsArr.map (·.subSampling.2)).foldl max 0
      let (maxiW, maxiH) :=
        if paramsArr.size > 1 then
          ((paramsArr.map (·.componentWidth)).foldl max 0,
           (paramsArr.map (·.componentHeight)).foldl max 0)
        else (maxiSubSampW, maxiSubSampH)
      let imageBlockWidth := toBlockSize imgWidth
      let imageBlockHeight := toBlockSize imgHeight
      let imageMcuWidth := (imageBlockWidth + maxiW - 1) / maxiW
      let imageMcuHeight := (imageBlockHeight + maxiH - 1) / maxiH
      let mut reader := initBoolStateJpg ecs

      for y in [0:imageMcuHeight] do
        for x in [0:imageMcuWidth] do
          if resetCounter == 0 then
            dcArray := Array.replicate compCount 0
            let (_, st') := runBoolReaderWith reader (do byteAlignJpg; discard decodeRestartInterval)
            reader := st'
            resetCounter := restartIntervalValue - 1
          else
            resetCounter := resetCounter - 1

          for param in paramsArr do
            let compIdx := param.componentIndex
            let compDesc := components.getD compIdx defaultJpgComponent
            let quantId := compDesc.quantizationTableDest.toNat
            let qTable := quants.getD (min 3 quantId) (Array.replicate dctBlockSize (1 : Int16))
            let (subX, subY) := param.subSampling
            let blockZero : MacroBlock Int16 := Array.replicate dctBlockSize (0 : Int16)
            let ((dcArray', blockDc, _), reader1) :=
              runBoolReaderWith reader (decodeFirstDC param dcArray blockZero 0)
            let acParam := { param with coefficientRange := (1, param.coefficientRange.2) }
            let ((blockFull, _), reader2) :=
              runBoolReaderWith reader1 (decodeFirstAc acParam blockDc 0)
            dcArray := dcArray'
            reader := reader2
            let transformed := decodeMacroBlock qTable blockFull
            img := unpackMacroBlock compCount subX subY compIdx
              (x * maxiW + param.blockMcuX) (y * maxiH + param.blockMcuY) img transformed
    pure img

-- ── Top-level decode ──

private def isDctFrameKind (k : JpgFrameKind) : Bool :=
  match k with
  | .baselineDCTHuffman | .progressiveDCTHuffman | .extendedSequentialDCTHuffman => true
  | _ => false

/-- Whether a parsed frame list uses baseline or progressive DCT coding.
    Ports upstream's `gatherImageKind`. -/
inductive JpgImageKind where
  | baseLineDCT
  | progressiveDCT

def gatherImageKind (frames : List JpgFrame) : Option JpgImageKind :=
  let kinds : List JpgFrameKind := frames.filterMap fun f =>
    match f with
    | .scanFrame k _ => if isDctFrameKind k then some k else none
    | _ => none
  match kinds with
  | [.progressiveDCTHuffman] => some .progressiveDCT
  | [.baselineDCTHuffman] => some .baseLineDCT
  | [.extendedSequentialDCTHuffman] => some .baseLineDCT
  | _ => none

/-- Resolve the colour space of a fully-folded decoder state: prefer an
    Adobe `APP14` marker if present, else guess from the component
    identifiers. Ports upstream's `colorSpaceOfState`. -/
def colorSpaceOfState (state : JpgDecoderState) : Option JpgColorSpace :=
  match state.currentFrame with
  | none => none
  | some hdr =>
      let compIds := hdr.components.map (·.identifier)
      let viaAdobe := state.app14Marker.bind (colorSpaceOfAdobe compIds.length)
      match viaAdobe with
      | some c => some c
      | none => colorSpaceOfComponentStr compIds

/-- Invert a raw `YCCK` (inverted-YCbCr-plus-`K`) pixel-component array into
    `CMYK`. See the module doc-comment for why this replaces upstream's
    `ColorSpaceConvertible PixelYCbCrK8 PixelCMYK8` instance. -/
def ycckArrayToCmyk (arr : Array UInt8) : Array UInt8 :=
  Id.run do
    let n := arr.size / 4
    let mut out : Array UInt8 := Array.mkEmpty arr.size
    for i in [0:n] do
      let yv : UInt8 := 255 - arr.getD (i * 4) 0
      let cb : UInt8 := 255 - arr.getD (i * 4 + 1) 0
      let cr : UInt8 := 255 - arr.getD (i * 4 + 2) 0
      let kv : UInt8 := 255 - arr.getD (i * 4 + 3) 0
      let rgb : PixelRGB8 := ColorSpaceConvertible.convertPixel (⟨yv, cb, cr⟩ : PixelYCbCr8)
      out := out.push (255 - rgb.r)
      out := out.push (255 - rgb.g)
      out := out.push (255 - rgb.b)
      out := out.push kv
    pure out

/-- Re-wrap a decoded raw pixel-component buffer as whichever concrete
    `DynamicImage` variant its colour space calls for. Ports upstream's
    `dynamicOfColorSpace`. -/
def dynamicOfColorSpace (color : Option JpgColorSpace) (w h : Nat) (arr : Array UInt8) :
    Except String DynamicImage :=
  match color with
  | none => .error "Unknown color space"
  | some .cmyk => .ok (.cmyk8 { width := w, height := h, data := arr })
  | some .ycck => .ok (.cmyk8 { width := w, height := h, data := ycckArrayToCmyk arr })
  | some .ycbcr => .ok (.ycbcr8 { width := w, height := h, data := arr })
  | some .rgb => .ok (.rgb8 { width := w, height := h, data := arr })
  | some .ya => .ok (.ya8 { width := w, height := h, data := arr })
  | some .y => .ok (.y8 { width := w, height := h, data := arr })
  | some other => .error s!"Wrong color space : {repr other}"

/-- The `Type 0` portion of decode: parse the marker structure, fold the
    decoder state, and dispatch to baseline/progressive reconstruction,
    without touching `Metadatas` (which lives in `Type 1` — it has an
    existential field — and so can't be bound inside the same `do` block as
    this `Type 0` decode data; see `decodeJpegWithMetadata` below, which
    mirrors `Linen.Codec.Picture.Gif`'s `decodeGifCore`/`decodeGifWithMetadata`
    split for the identical universe reason). -/
private def decodeJpegCore (bytes : List UInt8) :
    Except String (JpgImage × JpgDecoderState × Array UInt8 × Nat × Nat) := do
  let img : JpgImage ← parseJpgImage bytes
  let scanHdrOpt := img.frames.findSome? (fun f =>
    match f with
    | .scanFrame _ h => some h
    | _ => none)
  let scanInfoHdr ← match scanHdrOpt with
    | some h => pure h
    | none => .error "Unknown JPG kind"
  let imgWidth := scanInfoHdr.width.toNat
  let imgHeight := scanInfoHdr.height.toNat
  let (state, scans) ← buildJpgDecoderState img.frames
  let frameHeader ← match state.currentFrame with
    | some h => pure h
    | none => .error "Unknown JPG kind"
  let arr ←
    match gatherImageKind img.frames with
    | some .baseLineDCT =>
        pure (decodeBaselineImage frameHeader state.quantizationMatrices scans).data
    | some .progressiveDCT =>
        pure (progressiveUnpack state.maximumHorizontalResolution
          state.maximumVerticalResolution frameHeader state.quantizationMatrices scans).data
    | none => .error "Unknown JPG kind"
  pure (img, state, arr, imgWidth, imgHeight)

/-- Equivalent to `decodeJpeg`, but also extracts JFIF/Exif metadata (dpi,
    Exif tags, plus `basicMetadata`'s width/height/format). Ports upstream's
    `decodeJpegWithMetadata`. -/
def decodeJpegWithMetadata (bytes : List UInt8) :
    Except String (DynamicImage × Metadatas) :=
  match decodeJpegCore bytes with
  | .error e => .error e
  | .ok (img, state, arr, imgWidth, imgHeight) =>
      match dynamicOfColorSpace (colorSpaceOfState state) imgWidth imgHeight arr with
      | .error e => .error e
      | .ok dynImg =>
          let sizeMeta := basicMetadata .jpeg imgWidth imgHeight
          let jfifMeta := match state.app0JFifMarker with
            | some j =>
                (Metadatas.singleton .dpiX j.dpiX.toNat).union (Metadatas.singleton .dpiY j.dpiY.toNat)
            | none => Metadatas.empty
          let combinedMeta := (jfifMeta.union (extractJpgMetadatas img.frames)).union sizeMeta
          .ok (dynImg, combinedMeta)

/-- Decode a JPEG file into a `DynamicImage`. The colour space is still
    `YCbCr` if the source was; convert with `ColorSpaceConvertible` for RGB.
    Ports upstream's `decodeJpeg`. -/
def decodeJpeg (bytes : List UInt8) : Except String DynamicImage :=
  (decodeJpegWithMetadata bytes).map Prod.fst

-- ── Encoding: DCT/quantize/Huffman-writer helpers ──

/-- Quantize one already-DCT'd, zigzag-ordered macroblock: rounded integer
    division by the (also zigzag-ordered) quantization table. Ports
    upstream's `quantize`. -/
def quantize (table : MacroBlock Int16) (block : MacroBlock Int) : MacroBlock Int :=
  Array.ofFn (n := dctBlockSize) fun i =>
    let q : Int := (table.getD i.val 1).toInt
    let v := block.getD i.val 0
    Int.tdiv (v + Int.fdiv q 2) q

/-- The `SSSS` "category" of a signed coefficient: the number of bits needed
    to represent `|n|`, or `0` for `n = 0`. Ports upstream's `powerOf`. -/
def powerOf (n : Int) : Nat :=
  let val := n.natAbs
  Id.run do
    let mut range := 1
    let mut i := 0
    let mut done := false
    for _ in [0:32] do
      if !done then
        if val < range then done := true
        else range := 2 * range; i := i + 1
    pure i

/-- Write one coefficient's `SSSS`-bit value (sign-and-magnitude, JPEG's
    convention: a positive value's bit pattern is `n`, a negative value's is
    `n - 1` reinterpreted over `ssss` bits). Ports upstream's `encodeInt`. -/
def encodeInt (ssss : Nat) (n : Int) : BoolWriter Unit :=
  if n > 0 then writeBits' n.toNat.toUInt32 ssss
  else writeBits' (n - 1).toNat.toUInt32 ssss

/-- A symbol's canonical Huffman writer code: `(bitCount, code)`, indexed by
    symbol byte value (`0..255`; DC categories only use `0..11`, AC
    `RRRR``SSSS` bytes use the full range). -/
structure HuffmanWriterCode where
  size : Array UInt8
  code : Array UInt32

/-- Derive the canonical (JPEG Annex C) Huffman writer code for every symbol
    in a `HuffmanTable`, by the standard length-ordered linear-code
    procedure. See the module doc-comment for why this replaces upstream's
    `HuffmanPackedTree`/`makeInverseTable`. -/
def huffmanWriterCodeOfTable (table : HuffmanTable) : HuffmanWriterCode :=
  Id.run do
    let mut sizes : Array UInt8 := Array.replicate 256 (0 : UInt8)
    let mut codes : Array UInt32 := Array.replicate 256 (0 : UInt32)
    let mut code : UInt32 := 0
    for i in [0:table.length] do
      let grp := table.getD i []
      for val in grp do
        sizes := sizes.set! val.toNat (UInt8.ofNat (i + 1))
        codes := codes.set! val.toNat code
        code := code + 1
      code := code <<< 1
    pure { size := sizes, code := codes }

/-- Extract one 8×8 macroblock from `img`'s plane `plane` (of `sampCount`
    total planes), averaging over the given sampling factors when the
    component is subsampled (`xSampling = ySampling = 1` naturally
    degenerates to a plain 1-for-1 copy). Out-of-range source pixels clamp to
    the image's last row/column. Ports upstream's `extractBlock` (both of its
    fast-path and general-averaging clauses collapse into this one, per the
    module doc-comment's "always the generic path" precedent). -/
def extractBlock {α : Type} [Pixel α Pixel8] (img : Image α)
    (xSampling ySampling sampCount plane blockBx blockBy : Nat) : MacroBlock Int16 :=
  let w := img.width
  let h := img.height
  let accessPixel (x y : Nat) : Nat :=
    let xc := if x < w then x else w - 1
    let yc := if y < h then y else h - 1
    (img.data.getD ((yc * w + xc) * sampCount + plane) 0).toNat
  let pixelPerCoeff := max 1 (xSampling * ySampling)
  let blockXBegin := blockBx * dctBlockSize * xSampling
  let blockYBegin := blockBy * dctBlockSize * ySampling
  Array.ofFn (n := dctBlockSize) fun i =>
    let y := i.val / blockDim
    let x := i.val % blockDim
    let xBase := blockXBegin + x * xSampling
    let yBase := blockYBegin + y * ySampling
    let total := Id.run do
      let mut s := 0
      for dy in [0:ySampling] do
        for dx in [0:xSampling] do
          s := s + accessPixel (xBase + dx) (yBase + dy)
      pure s
    Int16.ofNat (total / pixelPerCoeff)

/-- DCT, zigzag-reorder, and quantize one extracted macroblock, then subtract
    off the running per-component DC predictor. Returns the new (absolute)
    DC coefficient plus the difference-coded, quantized, zigzag-ordered
    block ready for `serializeMacroBlock`. Ports upstream's
    `encodeMacroBlock`. -/
def encodeMacroBlock (quantTable : MacroBlock Int16) (prevDc : Int) (block : MacroBlock Int16) :
    Int × MacroBlock Int :=
  let dctBlk := fastDctLibJpeg block
  let zigzagged := zigZagReorderForward dctBlk
  let quantized := quantize quantTable zigzagged
  let dc := quantized.getD 0 0
  (dc, quantized.set! 0 (dc - prevDc))

/-- Round `n` up to the next multiple of `divisor`. Ports upstream's
    `divUpward`. -/
def divUpward (n divisor : Nat) : Nat :=
  (n + divisor - 1) / divisor

/-- Serialize one already difference-coded/quantized/zigzag-ordered
    macroblock's 64 coefficients as Huffman-coded bits: the DC coefficient
    (category code + extra bits), then every AC coefficient as a
    `(zeroRunLength, category)` pair (with `0xF0` "skip 16 zeros" escapes and
    a `(0, 0)` end-of-block code once the remaining coefficients are all
    zero). Ports upstream's `serializeMacroBlock`. -/
def serializeMacroBlock (dcCode acCode : HuffmanWriterCode) (block : MacroBlock Int) :
    BoolWriter Unit := do
  let encodeDc (n : Int) : BoolWriter Unit := do
    let ssss := powerOf n
    writeBits' (dcCode.code.getD ssss 0) (dcCode.size.getD ssss 0).toNat
    if ssss != 0 then encodeInt ssss n
  let encodeAc (zeroCount : Nat) (n : Int) : BoolWriter Unit := do
    if zeroCount == 0 && n == 0 then
      writeBits' (acCode.code.getD 0 0) (acCode.size.getD 0 0).toNat
    else if zeroCount ≥ 16 then do
      writeBits' (acCode.code.getD 0xF0 0) (acCode.size.getD 0xF0 0).toNat
      -- The recursive "skip another 16 zeros" step is bounded by
      -- `dctBlockSize`: `zeroCount < dctBlockSize` always (it counts
      -- skipped positions within one 64-coefficient block), so at most
      -- `dctBlockSize / 16` further escapes can ever be emitted.
      pure ()
    else
      let ssss := powerOf n
      let rrrrssss := zeroCount * 16 + ssss
      writeBits' (acCode.code.getD rrrrssss 0) (acCode.size.getD rrrrssss 0).toNat
      encodeInt ssss n
  encodeDc (block.getD 0 0)
  -- Walk AC coefficients 1..63, accumulating a zero run; on a nonzero
  -- coefficient, flush the run (with `0xF0` escapes for runs ≥ 16) and the
  -- coefficient together; at index 63, always flush (a trailing run of
  -- zeros collapses to a single end-of-block code).
  let mut zeroRun := 0
  for i in [1:dctBlockSize] do
    let n := block.getD i 0
    if i == dctBlockSize - 1 then
      if n == 0 then
        encodeAc 0 0
      else
        for _ in [0:zeroRun / 16] do encodeAc 16 1
        encodeAc (zeroRun % 16) n
    else if n == 0 then
      zeroRun := zeroRun + 1
    else
      for _ in [0:zeroRun / 16] do encodeAc 16 1
      encodeAc (zeroRun % 16) n
      zeroRun := 0

-- ── Default tables at a given quality ──

/-- The four default Huffman tables (luma/chroma × DC/AC), as
    `JpgHuffmanTableSpec`s ready to be written into a `huffmanTableFrame`.
    Ports upstream's `defaultHuffmanTables`. -/
def defaultHuffmanTables : List JpgHuffmanTableSpec :=
  [ { huffmanClass := .dcComponent, destination := 0, codes := defaultDcLumaHuffmanTable }
  , { huffmanClass := .acComponent, destination := 0, codes := defaultAcLumaHuffmanTable }
  , { huffmanClass := .dcComponent, destination := 1, codes := defaultDcChromaHuffmanTable }
  , { huffmanClass := .acComponent, destination := 1, codes := defaultAcChromaHuffmanTable }
  ]

/-- Scale one quantization coefficient by `coeff / 100`, floor-divided and
    clamped to `[1, 255]`. -/
private def scaleQuantCoeff (coeff : Int) (i : UInt8) : UInt8 :=
  let v : Int := Int.fdiv ((i.toNat : Int) * coeff) 100
  (max 1 (min 255 v)).toNat.toUInt8

/-- Scale a quantization table for a given IJG/libjpeg-style quality factor
    (`0..100`, higher is better quality). Ports upstream's
    `scaleQuantisationMatrix`. -/
def scaleQuantisationMatrix (quality : Int) (table : QuantificationTable) : QuantificationTable :=
  if quality ≤ 0 then table.map (scaleQuantCoeff 10000)
  else if quality < 50 then table.map (scaleQuantCoeff (Int.fdiv 5000 quality))
  else table.map (scaleQuantCoeff (200 - quality * 2))

/-- The default luminance quantization table scaled for `qual`. Ports
    upstream's `lumaQuantTableAtQuality`. -/
def lumaQuantTableAtQuality (qual : Nat) : QuantificationTable :=
  scaleQuantisationMatrix (qual : Int) defaultLumaQuantizationTable

/-- The default chrominance quantization table scaled for `qual`. Ports
    upstream's `chromaQuantTableAtQuality`. -/
def chromaQuantTableAtQuality (qual : Nat) : QuantificationTable :=
  scaleQuantisationMatrix (qual : Int) defaultChromaQuantizationTable

private def quantTableU8ToInt16 (t : QuantificationTable) : MacroBlock Int16 :=
  t.map (fun u => Int16.ofNat u.toNat)

/-- Both quantization tables (luma at destination `0`, chroma at destination
    `1`), zigzag-reordered and scaled for `qual`. Ports upstream's
    `zigzaggedQuantificationSpec`. -/
def zigzaggedQuantificationSpec (qual : Nat) : List JpgQuantTableSpec :=
  [ { precision := 0, destination := 0,
      quantTable := zigZagReorderForward (quantTableU8ToInt16 (lumaQuantTableAtQuality qual)) }
  , { precision := 0, destination := 1,
      quantTable := zigZagReorderForward (quantTableU8ToInt16 (chromaQuantTableAtQuality qual)) }
  ]

-- ── `JpgEncodable` ──

/-- Everything needed to encode one image component: which plane it reads
    from, its block-grid size within one "meta-block" (`maximumSubSamplingOf`
    × `maximumSubSamplingOf` MCU), its (already zigzag-reordered)
    quantization table, and its DC/AC Huffman writer codes. Ports upstream's
    `EncoderState`. -/
structure JpgEncoderState where
  componentIndex : Nat
  blockWidth : Nat
  blockHeight : Nat
  quantTable : MacroBlock Int16
  dcHuffman : HuffmanWriterCode
  acHuffman : HuffmanWriterCode

/-- A pixel type that can be encoded to JPEG: its per-component encoder
    state, its `JpgComponent`/`JpgScanSpecification` declarations, and any
    extra marker segments (e.g. an Adobe `APP14` colour-transform marker) its
    format needs. Ports upstream's `JpgEncodable`. -/
class JpgEncodable (pixel : Type) [Pixel pixel Pixel8] where
  /-- Extra marker segments to place right after the metadata blocks. -/
  additionalBlocks : List JpgFrame := []
  /-- The `SOF0` frame's declared components. -/
  componentsOfColorSpace : List JpgComponent
  /-- Per-component encoder state, at a given quality factor. -/
  encodingState : Nat → Array JpgEncoderState
  /-- The Huffman tables to write into the `DHT` segment. -/
  imageHuffmanTables : List JpgHuffmanTableSpec := defaultHuffmanTables
  /-- The `SOS` header's per-component DC/AC table selectors. -/
  scanSpecificationOfColorSpace : List JpgScanSpecification
  /-- The quantization tables to write into the `DQT` segment. -/
  quantTableSpec : Nat → List JpgQuantTableSpec := fun qual => (zigzaggedQuantificationSpec qual).take 1
  /-- The largest component sampling factor, i.e. how many 8×8 blocks make up
      one side of a "meta-block" (MCU). -/
  maximumSubSamplingOf : Nat := 1

instance : JpgEncodable Pixel8 where
  scanSpecificationOfColorSpace :=
    [{ componentSelector := 1, dcEntropyCodingTable := 0, acEntropyCodingTable := 0 }]
  componentsOfColorSpace :=
    [{ identifier := 1, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
       quantizationTableDest := 0 }]
  imageHuffmanTables :=
    [ { huffmanClass := .dcComponent, destination := 0, codes := defaultDcLumaHuffmanTable }
    , { huffmanClass := .acComponent, destination := 0, codes := defaultAcLumaHuffmanTable } ]
  encodingState qual :=
    #[{ componentIndex := 0, blockWidth := 1, blockHeight := 1,
        quantTable := zigZagReorderForward (quantTableU8ToInt16 (lumaQuantTableAtQuality qual)),
        dcHuffman := huffmanWriterCodeOfTable defaultDcLumaHuffmanTable,
        acHuffman := huffmanWriterCodeOfTable defaultAcLumaHuffmanTable }]

instance : JpgEncodable PixelYCbCr8 where
  maximumSubSamplingOf := 2
  quantTableSpec := zigzaggedQuantificationSpec
  scanSpecificationOfColorSpace :=
    [ { componentSelector := 1, dcEntropyCodingTable := 0, acEntropyCodingTable := 0 }
    , { componentSelector := 2, dcEntropyCodingTable := 1, acEntropyCodingTable := 1 }
    , { componentSelector := 3, dcEntropyCodingTable := 1, acEntropyCodingTable := 1 } ]
  componentsOfColorSpace :=
    [ { identifier := 1, horizontalSamplingFactor := 2, verticalSamplingFactor := 2,
        quantizationTableDest := 0 }
    , { identifier := 2, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
        quantizationTableDest := 1 }
    , { identifier := 3, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
        quantizationTableDest := 1 } ]
  encodingState qual :=
    let lumaState : JpgEncoderState :=
      { componentIndex := 0, blockWidth := 2, blockHeight := 2,
        quantTable := zigZagReorderForward (quantTableU8ToInt16 (lumaQuantTableAtQuality qual)),
        dcHuffman := huffmanWriterCodeOfTable defaultDcLumaHuffmanTable,
        acHuffman := huffmanWriterCodeOfTable defaultAcLumaHuffmanTable }
    let chromaState : JpgEncoderState :=
      { componentIndex := 1, blockWidth := 1, blockHeight := 1,
        quantTable := zigZagReorderForward (quantTableU8ToInt16 (chromaQuantTableAtQuality qual)),
        dcHuffman := huffmanWriterCodeOfTable defaultDcChromaHuffmanTable,
        acHuffman := huffmanWriterCodeOfTable defaultAcChromaHuffmanTable }
    #[lumaState, chromaState, { chromaState with componentIndex := 2 }]

instance : JpgEncodable PixelRGB8 where
  additionalBlocks :=
    [.adobe14Frame { dctVersion := 100, transformFlag0 := 0, transformFlag1 := 0,
                      colorTransform := .unknown }]
  imageHuffmanTables :=
    [ { huffmanClass := .dcComponent, destination := 0, codes := defaultDcLumaHuffmanTable }
    , { huffmanClass := .acComponent, destination := 0, codes := defaultAcLumaHuffmanTable } ]
  scanSpecificationOfColorSpace :=
    [82, 71, 66].map fun c =>
      { componentSelector := c, dcEntropyCodingTable := 0, acEntropyCodingTable := 0 }
  componentsOfColorSpace :=
    [82, 71, 66].map fun c =>
      { identifier := c, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
        quantizationTableDest := 0 }
  encodingState qual :=
    (Array.range 3).map fun ix =>
      { componentIndex := ix, blockWidth := 1, blockHeight := 1,
        quantTable := zigZagReorderForward (quantTableU8ToInt16 (lumaQuantTableAtQuality qual)),
        dcHuffman := huffmanWriterCodeOfTable defaultDcLumaHuffmanTable,
        acHuffman := huffmanWriterCodeOfTable defaultAcLumaHuffmanTable }

instance : JpgEncodable PixelCMYK8 where
  -- Upstream's own `PixelCMYK8` instance builds an Adobe `APP14` (`YCCK`)
  -- marker in a `where` clause but its `additionalBlocks` never returns it
  -- (`additionalBlocks _ = []`) — upstream's own dead code, ported exactly:
  -- no Adobe marker is ever emitted here either.
  additionalBlocks := []
  imageHuffmanTables :=
    [ { huffmanClass := .dcComponent, destination := 0, codes := defaultDcLumaHuffmanTable }
    , { huffmanClass := .acComponent, destination := 0, codes := defaultAcLumaHuffmanTable } ]
  scanSpecificationOfColorSpace :=
    [67, 77, 89, 75].map fun c =>
      { componentSelector := c, dcEntropyCodingTable := 0, acEntropyCodingTable := 0 }
  componentsOfColorSpace :=
    [67, 77, 89, 75].map fun c =>
      { identifier := c, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
        quantizationTableDest := 0 }
  encodingState qual :=
    (Array.range 4).map fun ix =>
      { componentIndex := ix, blockWidth := 1, blockHeight := 1,
        quantTable := zigZagReorderForward (quantTableU8ToInt16 (lumaQuantTableAtQuality qual)),
        dcHuffman := huffmanWriterCodeOfTable defaultDcLumaHuffmanTable,
        acHuffman := huffmanWriterCodeOfTable defaultAcLumaHuffmanTable }

-- ── Top-level encode ──

/-- Encode an image to JPEG at a given quality, allowing a colour space other
    than `PixelYCbCr8` and attaching `metas`' `DpiX`/`DpiY` (as a `JFIF`
    block) and Exif metadata. Ports upstream's
    `encodeDirectJpegAtQualityWithMetadata`. -/
def encodeDirectJpegAtQualityWithMetadata {pixel : Type} [Pixel pixel Pixel8] [JpgEncodable pixel]
    (quality : UInt8) (metas : Metadatas) (img : Image pixel) : Data.ByteString :=
  let w := img.width
  let h := img.height
  let outputComponentCount := (JpgEncodable.componentsOfColorSpace (pixel := pixel)).length
  let scanHeader : JpgScanHeader :=
    { scanLength := UInt16.ofNat (6 + 2 * outputComponentCount)
      componentCount := UInt8.ofNat outputComponentCount
      scans := JpgEncodable.scanSpecificationOfColorSpace (pixel := pixel)
      spectralSelectionStart := 0, spectralSelectionEnd := 63
      successiveApproxHigh := 0, successiveApproxLow := 0 }
  let hdr : JpgFrameHeader :=
    { frameHeaderLength := UInt16.ofNat (8 + 3 * outputComponentCount)
      samplePrecision := 8, height := UInt16.ofNat h, width := UInt16.ofNat w
      componentCount := UInt8.ofNat outputComponentCount
      components := JpgEncodable.componentsOfColorSpace (pixel := pixel) }
  let maxSampling := JpgEncodable.maximumSubSamplingOf (pixel := pixel)
  let horizontalMetaBlockCount := divUpward w (dctBlockSize * maxSampling)
  let verticalMetaBlockCount := divUpward h (dctBlockSize * maxSampling)
  let componentDef := JpgEncodable.encodingState (pixel := pixel) quality.toNat

  let encodedImage : ByteArray := Id.run do
    let mut dcTable : Array Int := Array.replicate outputComponentCount 0
    let mut writer : BoolWriteState := newWriteStateRef
    for my in [0:verticalMetaBlockCount] do
      for mx in [0:horizontalMetaBlockCount] do
        for enc in componentDef do
          let xSamplingFactor := maxSampling - enc.blockWidth + 1
          let ySamplingFactor := maxSampling - enc.blockHeight + 1
          for subY in [0:enc.blockHeight] do
            for subX in [0:enc.blockWidth] do
              let blockY := my * enc.blockHeight + subY
              let blockX := mx * enc.blockWidth + subX
              let prevDc := dcTable.getD enc.componentIndex 0
              let extracted := extractBlock img xSamplingFactor ySamplingFactor
                outputComponentCount enc.componentIndex blockX blockY
              let (dcCoeff, neoBlock) := encodeMacroBlock enc.quantTable prevDc extracted
              dcTable := dcTable.set! enc.componentIndex dcCoeff
              let ((), writer') :=
                (serializeMacroBlock enc.dcHuffman enc.acHuffman neoBlock).run writer
              writer := writer'
    let (bytes, _) := finalizeBoolWriter.run writer
    pure bytes

  let finalImage : JpgImage :=
    { frames :=
        encodeJpgMetadatas metas ++
        JpgEncodable.additionalBlocks (pixel := pixel) ++
        [ .quantTableFrame (JpgEncodable.quantTableSpec (pixel := pixel) quality.toNat)
        , .scanFrame .baselineDCTHuffman hdr
        , .huffmanTableFrame (JpgEncodable.imageHuffmanTables (pixel := pixel))
        , .scanBlob scanHeader encodedImage ] }
  (putJpgImage finalImage).toStrictByteString

/-- Equivalent to `encodeJpegAtQuality`, but also attaches `metas`'
    `DpiX`/`DpiY` (as a `JFIF` block). Ports upstream's
    `encodeJpegAtQualityWithMetadata`. -/
def encodeJpegAtQualityWithMetadata (quality : UInt8) (metas : Metadatas)
    (img : Image PixelYCbCr8) : Data.ByteString :=
  encodeDirectJpegAtQualityWithMetadata quality metas img

/-- Encode an image to JPEG at a given quality factor (`0..100`, `100` best).
    Ports upstream's `encodeJpegAtQuality`. -/
def encodeJpegAtQuality (quality : UInt8) (img : Image PixelYCbCr8) : Data.ByteString :=
  encodeJpegAtQualityWithMetadata quality Metadatas.empty img

/-- Encode an image to JPEG at a reasonable default quality (`50`). Ports
    upstream's `encodeJpeg`. -/
def encodeJpeg (img : Image PixelYCbCr8) : Data.ByteString :=
  encodeJpegAtQuality 50 img

end Codec.Picture
