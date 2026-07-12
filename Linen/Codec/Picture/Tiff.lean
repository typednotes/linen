import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Metadata
import Linen.Codec.Picture.Tiff.Internal.Types
import Linen.Codec.Picture.Tiff.Internal.Metadata
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.Tiff` (top-level) from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 17 of 29): whole-file
  TIFF parsing (the byte-order header, the IFD chain, and — module 15's
  explicitly deferred piece — resolving each `ImageFileDirectory` entry's
  out-of-line `ifdOffset` into its actual value), strip decompression,
  predictor undoing, pixel unpacking into a `DynamicImage`/`PalettedImage`,
  and the top-level `decodeTiff`/`decodeTiffWithMetadata`/
  `decodeTiffWithPaletteAndMetadata`/`encodeTiff` entry points (upstream's
  actual export list — there is no `encodeTiffWithMetadata`: upstream's own
  `encodeTiff` never touches `Codec.Picture.Tiff.Internal.Metadata` at all,
  so this port doesn't either).

  ## IFD-chain-traversal termination strategy

  A TIFF file's IFDs form a singly linked list: each IFD block ends with a
  4-byte "offset of the next IFD" field, `0` terminating the chain. That
  offset is an arbitrary absolute file position supplied by the file itself,
  so nothing about its *value* bounds how many times this can loop — only
  the fact that **the file has finitely many byte positions** does. This
  port makes that argument the actual termination measure: `parseIfdChain`
  calls a `fuel`-decreasing helper seeded with `fileBytes.size + 1` — a
  genuine upper bound on how many *distinct* IFD offsets a file of that size
  can contain — and additionally tracks every offset already visited,
  erroring out on a repeat (a malicious/corrupt "IFD offset 100 points back
  to IFD offset 100" cycle) rather than silently trusting the fuel bound to
  paper over one. The fuel argument itself is a plain `Nat` matched via
  `n + 1`/`0`, so Lean accepts the recursion structurally with no
  `decreasing_by` or well-founded-recursion machinery needed — but the bound
  it decreases against is tied to `fileBytes.size`, not an arbitrary
  constant, exactly as the porting brief asks for. Running out of fuel (which
  can only happen if the visited-offset check somehow failed to catch a
  cycle) is reported as an error, never silently truncated.

  ## Compression scope: `.none` and `.packBit` only

  `Linen.Codec.Picture.Tiff.Internal.Types`' `TiffCompression` (module 15)
  has **no `deflate`/zlib code point at all** — `unpackCompression` only ever
  produces `.none`, `.modifiedRLE`, `.lzw`, `.jpeg`, `.packBit`, mirroring
  upstream's own `Internal/Types.hs`, which likewise never maps TIFF
  compression tag `8` (Adobe/zlib deflate) to anything. So, unlike
  `Linen.Codec.Picture.Png` (module 14), **this module never touches
  `Crypto.Zlib` and stays entirely pure** (`Except String α`, no `IO`) —
  there is no "IO-vs-pure decision" to make here the way there was for PNG,
  because the zlib wrinkle that forced PNG's decision simply does not arise
  for any compression scheme this port's `TiffCompression` can even name.
  Of the five schemes it *can* name:
  - `.none` (uncompressed) and `.packBit` (Apple PackBits RLE) are fully
    supported (`decompressStrip`/`unpackPackBits`).
  - `.lzw` is **deferred**: upstream's own decoder for it
    (`Codec.Picture.Tiff`'s `uncompressAt CompressionLZW`) delegates to
    `Codec.Picture.Gif.Internal.LZW`'s `decodeLzwTiff` — module 18 of this
    port's plan, not yet ported (this is module 17). `decompressStrip`
    returns a clear error for `.lzw` rather than silently misdecoding or
    inlining a half-hearted LZW implementation; revisiting TIFF-LZW support
    once module 18 exists is a natural, self-contained follow-up.
  - `.modifiedRLE` (CCITT Group 3) and `.jpeg` (old-style JPEG-in-TIFF) are
    **unsupported here, matching upstream's own de facto behaviour**:
    upstream's `uncompressAt` has no case for either and falls through to
    `uncompressAt _ = error "Unhandled compression"` — a Haskell runtime
    error, i.e. upstream itself has no working decoder for these schemes
    either. This port reports the same condition as a clean `Except` error
    instead of a partial-function crash.

  ## Pixel-format scope

  Every combination this port's `unpackTiff` handles is uniform-bit-depth
  (all `BitsPerSample` entries equal), `TiffSampleFormat.uint`-only, and
  `TiffPlanarConfiguration.contig`-only:

  - `.monochrome`/`.monochromeWhite0`: 1 sample (`Y`) or 2 samples (`YA`,
    the extra sample being alpha), depth `8` or `16`; `.monochromeWhite0`
    additionally inverts every luma sample (`maxBound - v`), matching
    upstream's dedicated `TiffMonochromeWhite0` branch.
  - `.rgb`: 3 samples (`RGB`) or 4 samples (`RGBA`, the 4th being alpha),
    depth `8` or `16`.
  - `.cmyk`: 4 samples, depth `8` or `16`.
  - `.paletted`: 1 sample, depth `8`, using the `ColorMap` tag (three
    consecutive `2^BitsPerSample`-length blocks of 16-bit red/green/blue
    values) — downshifted to an RGB8 palette via `>>> 8`, the exact
    RGB8-only-palette convention `Linen.Codec.Picture.Png`/`Tga`/`Bitmap`
    already established (see their own module doc-comments) for this
    codebase's `PalettedImage`.

  **Deferred, and documented as such rather than silently unhandled** (every
  one of these causes `unpackTiff` to return a clear `Except` error, never a
  wrong answer): sub-byte bit depths (`2`/`4` bits per sample, upstream's
  `Pack2`/`Pack4` machinery), the `12`-bit packed depth (`Pack12`), `32`-bit
  integer/float samples (`ImageY32`/`ImageYF`), `.ycbcr` (chroma
  subsampling), `.transparencyMask`/`.cieLab`, `.separate` planar
  configuration, and any `TiffSampleFormat` other than `.uint`. Upstream
  handles most of the sub-byte/12-bit/YCbCr cases via a family of
  `Unpackable` type-class instances (`Pack2`/`Pack4`/`Pack12`/
  `YCbCrSubsampling`) built around GHC's mutable, `Storable`-backed
  `STVector` — machinery this port does not carry over at all (see the next
  section); porting those specific combinations faithfully would mean
  reintroducing exactly that machinery for a handful of increasingly niche
  combinations, so they are left as a follow-up rather than attempted here.

  ## Architectural simplification: no `Unpackable`/`STVector` machinery

  Upstream's decode pipeline is built around a `Unpackable` type class
  (`outAlloc`/`allocTempBuffer`/`offsetStride`/`mergeBackTempBuffer`) whose
  instances (`Word8`, `Word16`, `Word32`, `Float`, `Pack2`, `Pack4`,
  `Pack12`, `YCbCrSubsampling`) exist to let `gatherStrips` decompress
  straight into a shared, mutable `STVector` buffer without a second copy —
  a memory-layout optimisation with no Lean counterpart worth
  reconstructing (this library has no mutable-region/`ST` story at all, per
  every earlier codec module's own "no `MutableImage`" note). This port
  instead follows `Linen.Codec.Picture.Png`'s own precedent
  (`decodeSamplesGeneric`, module 14): `gatherSamples` decompresses every
  strip and unpacks it into one flat, immutable `Array Nat` of
  `width * height * sampleCount` component values (large enough to hold
  either an 8- or 16-bit sample uniformly), and `unpackTiff`'s per-format
  branches narrow that down to the concrete `UInt8`/`UInt16` pixel type only
  once the format is known. `applyPredictor` undoes horizontal differencing
  on that same flat array, exactly mirroring `gatherStrips`'s post-unpack
  `when (tiffPredictor nfo == PredictorHorizontalDifferencing)` loop, but as
  one pass over `Array Nat` instead of the `MutableImage` `readPixel`/
  `writePixel` pair upstream uses.

  ## Offset resolution (module 15's deferred piece)

  `resolveIfdExtended` is exactly the piece module 15's own doc-comment
  named and deferred to this module: given the whole file's bytes and an
  already-parsed `ImageFileDirectory` whose `ifdCount * ifdTypeByteSize
  ifdType` exceeds 4 bytes (so its value cannot be inline), it seeks to
  `ifdOffset` in the file buffer and decodes `ifdCount` values of `ifdType`
  into an `ExifData` payload. It covers `.short`/`.long`/`.ascii`/
  `.undefined`/`.rational`(count `1`)/`.signedRational`(count `1`) — every
  `ExifData` constructor this codebase's `ExifData` (module 5) actually has
  a multi-value or scalar slot for. `.sbyte`/`.signedShort`/`.signedLong`/
  `.float`/`.double`, and any `.rational`/`.signedRational` entry with
  `count > 1`, have no corresponding `ExifData` array constructor to decode
  into at all (there is no `ExifData.rationals`), so `resolveIfdExtended`
  leaves those entries' `ifdExtended` as `.none` — a genuinely-out-of-scope
  case inherited from module 5's own `ExifData`, not a shortcut invented
  here. **Nested Exif/GPS sub-IFD dereferencing (a `TagExifOffset`/
  `TagGPSInfo` entry's `ifdOffset` pointing at a second, nested IFD block,
  upstream's `ExifIFD` case of `fetchExtended`) is deferred entirely** — this
  port never re-enters IFD parsing from inside `resolveIfdExtended`, so such
  an entry's `ifdExtended` simply stays `.none`. This means module 16's
  `extractTiffMetadata`'s `.exifOffset, .ifd lst` branch is dead code when
  fed this module's output (it will only ever see `.exifOffset, .none`,
  which its own fallback `_, _ => .empty` already handles gracefully) — a
  documented, deliberate scope decision consistent with module 15/16's own
  "seeking to resolve one more level of indirection belongs to whichever
  module actually has the file-buffer-and-cursor framework" boundary,
  carried one level further than modules 15/16 needed to.

  `findIfdExt`/`findIfdExtDefault` below are direct ports of upstream's
  `findIFDExt`/`findIFDExtDefaultData`, **including their asymmetric
  behaviour**: `findIfdExt`'s `ifdCount = 1` special case for `.short`/
  `.long` reads straight from `ifdOffset` (truncating to 16 bits for
  `.short`, faithfully reproducing upstream's `fromIntegral :: Word32 ->
  Word16`, which is only correct for little-endian files — the same
  asymmetry module 16's own doc-comment already flags for a different
  function), while `findIfdExt`'s otherwise-branch and `findIfdExtDefault`
  read only `ifdExtended`, silently falling back to a caller-supplied
  default when an inline (`ifdExtended = .none`) tag is present but not
  handled by the `ifdCount = 1` special case (`findIfdExtDefault` has no
  such special case at all, unlike `findIfdExt`). Both quirks are upstream's
  own; this port carries them forward unmodified rather than "fixing" a
  cross-function inconsistency it did not introduce.

  ## Encode side

  `encodeTiff` mirrors upstream's own `encodeTiff` exactly in scope: it
  always writes a **single, uncompressed strip**, `.contig` planar
  configuration, `Predictor.none` — upstream's encoder never compresses, never
  splits into multiple strips, and never predicts either, so there is no
  richer encode path being left out here. `TiffSaveable` (upstream's
  `TiffSaveable` class) becomes a plain class with `colorSpace`/
  `extraSample`/`sampleFormat` fields (no method argument needed — Lean's
  instance resolution on `α` already picks the right one, unlike Haskell's
  `colorSpaceOfPixel :: px -> TiffColorspace` needing a `px` value purely to
  drive type-class dispatch) plus `componentByteSize`/`putComponent` (this
  port's stand-in for upstream's `Foreign.Storable.sizeOf`/serialisation,
  since Lean's `Pixel` class, module 1, has no notion of a component's
  on-disk byte width or endianness-aware serialisation of its own).
  `PixelF`/`Pixel32`/`PixelYCbCr8` have no `TiffSaveable` instance here,
  matching this module's decode-side scope (float/32-bit/YCbCr samples are
  not supported for reading either, so there is nothing to round-trip).
  Since only a single strip is ever written and every tag except
  `BitsPerSample` (for `sampleCount > 2`) fits inline, the "lay out and
  offset a list of IFDs plus their extended data for writing" step module
  15 dropped (`setupIfdOffsets`/`cleanImageFileDirectory`) is reimplemented
  here from scratch, scoped to exactly what `encodeTiff`'s fixed IFD entry
  list needs (`layoutIfdExtended`): entries needing out-of-line storage get
  it placed, in entry order, right after the (fixed-size, one-strip) IFD
  block.
-/

namespace Codec.Picture

open Data.ByteString (Builder)

-- ── Offset resolution (module 15's deferred piece) ──

/-- The on-disk byte size of an IFD entry's whole value (`count` values of
    `ifdType`, each `ifdTypeByteSize ifdType` bytes). `≤ 4` means the value
    is inline in `ifdOffset` itself; `> 4` means `ifdOffset` is a file
    offset to seek to. -/
private def ifdValueByteSize (ifd : ImageFileDirectory) : Nat :=
  ifd.ifdCount.toNat * ifdTypeByteSize ifd.ifdType

/-- Read `n` values from `bytes` using `step`, in order. Structural
    recursion on `n`. -/
private def readValuesLoop {α : Type} (step : List UInt8 → Except String (α × List UInt8)) :
    Nat → List UInt8 → Except String (Array α)
  | 0, _ => .ok #[]
  | n + 1, bytes => do
      let (v, rest) ← step bytes
      let arr ← readValuesLoop step n rest
      pure (#[v] ++ arr)

/-- Decode `n` out-of-line values of type `ty` from `bytes` (exactly
    `n * ifdTypeByteSize ty` bytes) into the `ExifData` constructor that can
    hold them, or `none` for a type/count combination `ExifData` (module 5)
    has no array slot for — see the module doc-comment. -/
private def readExtendedValues (endian : TiffEndianness) (ty : IfdType) (n : Nat)
    (bytes : List UInt8) : Option ExifData :=
  match ty with
  | .short =>
      match readValuesLoop (readU16 endian) n bytes with
      | .ok arr => some (.shorts arr)
      | .error _ => none
  | .long =>
      match readValuesLoop (readU32 endian) n bytes with
      | .ok arr => some (.longs arr)
      | .error _ => none
  | .ascii => some (.string (ByteArray.mk (bytes.take n).toArray))
  | .undefined => some (.undefined (ByteArray.mk (bytes.take n).toArray))
  | .rational =>
      if n == 1 then
        match readU32 endian bytes with
        | .error _ => none
        | .ok (num, rest) =>
          match readU32 endian rest with
          | .ok (den, _) => some (.rational num den)
          | .error _ => none
      else none
  | .signedRational =>
      if n == 1 then
        match readU32 endian bytes with
        | .error _ => none
        | .ok (num, rest) =>
          match readU32 endian rest with
          | .ok (den, _) => some (.signedRational num den)
          | .error _ => none
      else none
  | .byte | .sbyte | .signedShort | .signedLong | .float | .double => none

/-- Resolve one `ImageFileDirectory` entry's `ifdExtended` field, seeking
    into the whole file's bytes if the entry's value doesn't fit inline.
    Leaves `ifdExtended` as `.none` (unchanged) for inline entries, for
    entries whose type/count has no `ExifData` array slot, and for entries
    whose `ifdOffset` points outside the file (a defensively-tolerated
    malformed file, matching this port's general "don't crash on a bad
    offset" stance). -/
private def resolveIfdExtended (endian : TiffEndianness) (fileBytes : ByteArray)
    (ifd : ImageFileDirectory) : ImageFileDirectory :=
  let n := ifd.ifdCount.toNat
  let byteSize := ifdValueByteSize ifd
  if byteSize ≤ 4 then ifd
  else
    let off := ifd.ifdOffset.toNat
    if off + byteSize > fileBytes.size then ifd
    else
      let slice := (fileBytes.extract off (off + byteSize)).toList
      match readExtendedValues endian ifd.ifdType n slice with
      | some ed => { ifd with ifdExtended := ed }
      | none => ifd

-- ── IFD-chain traversal ──

/-- Parse the IFD chain starting at `offset`, resolving every entry's
    `ifdExtended` as each block is parsed. `fuel` is a decreasing `Nat`
    bound to `fileBytes.size + 1` by the caller (`parseIfdChain`) — a real
    upper bound on how many distinct IFD offsets a file that size can hold —
    plus a `visited` list catching a repeated offset (a cyclic chain)
    before fuel would otherwise be exhausted. See the module doc-comment for
    why this, rather than an arbitrary fuel constant, is the termination
    argument. -/
private def parseIfdChainAux (endian : TiffEndianness) (fileBytes : ByteArray) :
    Nat → List UInt32 → UInt32 → Except String (List (List ImageFileDirectory))
  | 0, _, _ => .error "TIFF IFD chain too long (likely a corrupt or cyclic file)"
  | _ + 1, _, 0 => .ok []
  | fuel + 1, visited, offset =>
      if visited.contains offset then
        .error "Cyclic TIFF IFD chain"
      else if offset.toNat ≥ fileBytes.size then
        .error "TIFF IFD offset out of range"
      else do
        let slice := (fileBytes.extract offset.toNat fileBytes.size).toList
        let (ifds, nextOffset, _) ← parseImageFileDirectoryList endian slice
        let resolved := ifds.map (resolveIfdExtended endian fileBytes)
        let rest ← parseIfdChainAux endian fileBytes fuel (offset :: visited) nextOffset
        pure (resolved :: rest)

/-- Parse the whole IFD chain of a TIFF file, starting at `offset` (the
    header's own first-IFD offset). -/
def parseIfdChain (endian : TiffEndianness) (fileBytes : ByteArray) (offset : UInt32) :
    Except String (List (List ImageFileDirectory)) :=
  parseIfdChainAux endian fileBytes (fileBytes.size + 1) [] offset

-- ── `TiffInfo` (upstream's `TiffInfo`) ──

/-- Every field this module's decoder needs from a TIFF file's flattened IFD
    entries, mirroring upstream's `TiffInfo` (minus fields this port's scope
    drops — `tiffYCbCrSubsampling`, not needed since `.ycbcr` decoding is
    out of scope; see the module doc-comment). Deliberately excludes a
    `Metadatas` field: `Metadatas` (module 6) bundles an existential `Elem`
    and so lives in `Type 1`, one universe above ordinary data (the same
    "universe wrinkle" `Linen.Codec.Picture.Png`'s own doc-comment already
    documents) — embedding it here would push `TiffInfo`, and every
    `Except String TiffInfo` built from it, into `Type 1` too, which breaks
    universe-uniform `do`-block bind chaining against the ordinary `Type 0`
    values (`UInt32`, etc.) the rest of this pipeline traffics in. Metadata
    extraction (`extractTiffMetadata`) is instead run separately, directly
    in `decodeTiffWithPaletteAndMetadata`, exactly mirroring how
    `Linen.Codec.Picture.Png` itself keeps metadata extraction out of its
    own inner (`IO`-typed, for unrelated reasons) decode pipeline. -/
structure TiffInfo where
  header : TiffHeader
  width : UInt32
  height : UInt32
  colorspace : TiffColorspace
  sampleCount : UInt32
  rowPerStrip : UInt32
  planarConfiguration : TiffPlanarConfiguration
  sampleFormats : List TiffSampleFormat
  bitsPerSample : Array UInt32
  compression : TiffCompression
  stripByteCounts : Array UInt32
  stripOffsets : Array UInt32
  palette : Option (Array UInt16)
  extraSample : Option ExtraSample
  predictor : Predictor

/-- The first entry of `ifds` carrying tag `t`, if any. -/
private def findIfd (t : ExifTag) (ifds : List ImageFileDirectory) : Option ImageFileDirectory :=
  ifds.find? (·.ifdIdentifier == t)

/-- `findIFDData`: a mandatory tag's inline `ifdOffset` value. -/
private def findData (msg : String) (t : ExifTag) (ifds : List ImageFileDirectory) :
    Except String UInt32 :=
  match findIfd t ifds with
  | some ifd => .ok ifd.ifdOffset
  | none => .error msg

/-- `findIFDDefaultData`: an optional tag's inline `ifdOffset` value, or `d`
    if absent. -/
private def findDataDefault (d : UInt32) (t : ExifTag) (ifds : List ImageFileDirectory) : UInt32 :=
  match findIfd t ifds with
  | some ifd => ifd.ifdOffset
  | none => d

/-- `unLong`: widen any of `ExifData`'s scalar/array integer constructors
    into a uniform `Array UInt32`. -/
private def unLong : ExifData → Option (Array UInt32)
  | .long v => some #[v]
  | .short v => some #[v.toUInt32]
  | .shorts v => some (v.map (·.toUInt32))
  | .longs v => some v
  | _ => none

/-- `findIFDExt`: a mandatory tag's fully-resolved array value — see the
    module doc-comment for why the `ifdCount = 1` special case (bypassing
    `ifdExtended` even when set) is ported faithfully, asymmetry included. -/
private def findIfdExt (msg : String) (t : ExifTag) (ifds : List ImageFileDirectory) :
    Except String (Array UInt32) :=
  match findIfd t ifds with
  | none => .error msg
  | some ifd =>
      if ifd.ifdCount == 1 ∧ ifd.ifdType == .short then .ok #[ifd.ifdOffset.toUInt16.toUInt32]
      else if ifd.ifdCount == 1 ∧ ifd.ifdType == .long then .ok #[ifd.ifdOffset]
      else
        match unLong ifd.ifdExtended with
        | some arr => .ok arr
        | none => .error s!"Can't parse tag: {msg}"

/-- `findIFDExtDefaultData`: an optional tag's fully-resolved array value,
    or `d` if absent *or* if the tag is present but its value was never
    resolved into `ifdExtended` (see the module doc-comment: unlike
    `findIfdExt`, this has no `ifdCount = 1` special case, matching
    upstream). -/
private def findIfdExtDefault (d : Array UInt32) (t : ExifTag) (ifds : List ImageFileDirectory) :
    Except String (Array UInt32) :=
  match findIfd t ifds with
  | none => .ok d
  | some ifd =>
      match ifd.ifdExtended with
      | .none => .ok d
      | ext =>
        match unLong ext with
        | some arr => .ok arr
        | none => .error "Can't parse tag"

/-- `findPalette`: the `ColorMap` tag's resolved value, if present as a
    `.shorts` array (three consecutive `paletteSize`-length red/green/blue
    blocks). -/
private def findPalette (ifds : List ImageFileDirectory) : Option (Array UInt16) :=
  match findIfd .colorMap ifds with
  | some ifd =>
      match ifd.ifdExtended with
      | .shorts v => some v
      | _ => none
  | none => none

/-- The `ExtraSample` tag's decoded value, if present. -/
private def findExtraSample (ifds : List ImageFileDirectory) : Except String (Option ExtraSample) :=
  match findIfd .extraSample ifds with
  | none => .ok none
  | some ifd => do
      let es ← extraSampleOfCode ifd.ifdOffset.toUInt16
      pure (some es)

/-- Build a `TiffInfo` from a TIFF file's flattened, already-resolved IFD
    entries (upstream's `BinaryParam ByteString TiffInfo`'s `getP`). -/
def buildTiffInfo (header : TiffHeader) (cleaned : List ImageFileDirectory) :
    Except String TiffInfo := do
  let width ← findData "Can't find width" .imageWidth cleaned
  let height ← findData "Can't find height" .imageLength cleaned
  let colorspaceCode ← findData "Can't find color space" .photometricInterpretation cleaned
  let colorspace ← unpackPhotometricInterpretation colorspaceCode
  let sampleCount ← findData "Can't find sample per pixel" .samplesPerPixel cleaned
  let rowPerStrip ← findData "Can't find row per strip" .rowPerStrip cleaned
  let planarConfiguration ← planarConfgOfConstant (findDataDefault 1 .planarConfiguration cleaned)
  let sampleFormatWords ← findIfdExtDefault #[1] .sampleFormat cleaned
  let sampleFormats ← sampleFormatWords.toList.mapM unpackSampleFormat
  let bitsPerSample ← findIfdExt "Can't find bit per sample" .bitsPerSample cleaned
  let compressionCode ← findData "Can't find Compression" .compression cleaned
  let compression ← unpackCompression compressionCode
  let stripByteCounts ← findIfdExt "Can't find byte counts" .stripByteCounts cleaned
  let stripOffsets ← findIfdExt "Strip offsets missing" .stripOffsets cleaned
  let extraSample ← findExtraSample cleaned
  let predictor ← predictorOfConstant (findDataDefault 1 .predictor cleaned)
  pure
    { header, width, height, colorspace, sampleCount, rowPerStrip, planarConfiguration
      sampleFormats, bitsPerSample, compression, stripByteCounts, stripOffsets
      palette := findPalette cleaned, extraSample, predictor }

-- ── Strip decompression ──

/-- Apple PackBits RLE decompression: a control byte `v` (interpreted as
    `Int8`) followed either by `v + 1` literal bytes (`v ≥ 0`), or one byte
    to repeat `1 - v` times (`v ∈ [-127, -1]`), or nothing (`v = -128`, a
    no-op). Bounded `for` loop over at most `input.size` steps (each real
    step advances the read cursor `i` by at least `1`, so `input.size`
    iterations always suffice to exhaust the input; extra iterations after
    that are no-ops guarded by `i < input.size`) — no recursion, no
    termination proof needed, matching upstream's `unpackPackBit`. -/
def unpackPackBits (input : ByteArray) : ByteArray :=
  Id.run do
    let mut out : Array UInt8 := #[]
    let mut i := 0
    for _ in [0:input.size] do
      if i < input.size then
        let ctrl := (input.get! i).toNat
        let v : Int := if ctrl ≥ 128 then (ctrl : Int) - 256 else (ctrl : Int)
        if v ≥ 0 then
          let len := v.toNat + 1
          for j in [0:len] do
            if i + 1 + j < input.size then
              out := out.push (input.get! (i + 1 + j))
          i := i + 1 + len
        else if v ≥ -127 then
          let count := (-v).toNat + 1
          if i + 1 < input.size then
            let byte := input.get! (i + 1)
            for _ in [0:count] do
              out := out.push byte
          i := i + 2
        else
          i := i + 1
    pure (ByteArray.mk out)

/-- Decompress one strip (`offset`, `size` bytes) of `fileBytes` according
    to `compression` — see the module doc-comment for exactly which schemes
    this covers vs. defers. -/
def decompressStrip (compression : TiffCompression) (fileBytes : ByteArray) (offset size : Nat) :
    Except String ByteArray :=
  if offset + size > fileBytes.size then .error "TIFF strip out of range"
  else
    let raw := fileBytes.extract offset (offset + size)
    match compression with
    | .none => .ok raw
    | .packBit => .ok (unpackPackBits raw)
    | .lzw =>
        .error "TIFF LZW compression is not supported yet (needs Codec.Picture.Gif.Internal.LZW, module 18)"
    | .modifiedRLE => .error "TIFF CCITT Group 3 (modified RLE) compression is not supported"
    | .jpeg => .error "TIFF old-style JPEG-in-TIFF compression is not supported"

-- ── Sample gathering (upstream's `gatherStrips`, minus the `Unpackable`/`STVector` machinery) ──

/-- Decompress every strip and unpack it into one flat `width * height *
    sampleCount` array of component values (`.contig` planar configuration
    only — see the module doc-comment). `bitDepth` is `8` or `16`; a strip's
    last, possibly-partial set of rows is handled the same way upstream's
    own "some files declare the wrong `RowPerStrip`" comment does (bounded
    by the image's remaining rows, never by the strip's own declared
    length). -/
def gatherSamples (info : TiffInfo) (fileBytes : ByteArray) (bitDepth sampleCount : Nat) :
    Except String (Array Nat) :=
  let width := info.width.toNat
  let height := info.height.toNat
  let rowPerStrip := if info.rowPerStrip.toNat == 0 then height else info.rowPerStrip.toNat
  let endian := info.header.endianness
  let byteWidth := (bitDepth / 8) * sampleCount * width
  let stripCount := info.stripOffsets.size
  let result := Id.run do
    let mut out : Array Nat := Array.replicate (width * height * sampleCount) 0
    let mut err : Option String := none
    let mut rowsWritten := 0
    for stripIdx in [0:stripCount] do
      if err.isNone ∧ rowsWritten < height then
        let off := (info.stripOffsets.getD stripIdx 0).toNat
        let sz := (info.stripByteCounts.getD stripIdx 0).toNat
        match decompressStrip info.compression fileBytes off sz with
        | .error e => err := some e
        | .ok raw =>
            let rowsHere := min rowPerStrip (height - rowsWritten)
            for r in [0:rowsHere] do
              let rowStart := r * byteWidth
              if rowStart + byteWidth ≤ raw.size then
                for pix in [0:width] do
                  for s in [0:sampleCount] do
                    let idx := (rowsWritten + r) * width * sampleCount + pix * sampleCount + s
                    let v :=
                      if bitDepth == 16 then
                        let byteBase := rowStart + (pix * sampleCount + s) * 2
                        let b0 := (raw.get! byteBase).toNat
                        let b1 := (raw.get! (byteBase + 1)).toNat
                        match endian with
                        | .little => b0 + b1 * 256
                        | .big => b0 * 256 + b1
                      else
                        (raw.get! (rowStart + pix * sampleCount + s)).toNat
                    out := out.set! idx v
            rowsWritten := rowsWritten + rowsHere
    pure (out, err)
  match result.2 with
  | some e => .error e
  | none => .ok result.1

/-- Undo horizontal-differencing prediction in place over a flat `width *
    height * sampleCount` sample array: every pixel's sample becomes the
    running sum (mod `2 ^ bitDepth`, matching `UInt8`/`UInt16` wraparound)
    of itself and every earlier pixel's same sample in its row, exactly
    upstream's `mixWith (\_ c1 c2 -> c1 + c2)` pass over adjacent pixels. -/
def applyPredictor (predictor : Predictor) (bitDepth sampleCount width height : Nat)
    (data : Array Nat) : Array Nat :=
  match predictor with
  | .none => data
  | .horizontalDifferencing =>
      let modBase := 2 ^ bitDepth
      Id.run do
        let mut out := data
        for y in [0:height] do
          for x in [1:width] do
            for s in [0:sampleCount] do
              let curIdx := (y * width + x) * sampleCount + s
              let prevIdx := (y * width + (x - 1)) * sampleCount + s
              let v := (out.getD prevIdx 0 + out.getD curIdx 0) % modBase
              out := out.set! curIdx v
        pure out

-- ── Pixel unpacking ──

/-- Build a `sampleCount`-component, 8-bit-per-component image from a flat
    `width * height * sampleCount` sample array. -/
private def buildImage8 {α : Type} [Pixel α UInt8] (width height sampleCount : Nat)
    (samples : Array Nat) : Image α :=
  generateImage
    (fun x y => Pixel.fromComponents (α := α)
      (Array.ofFn (n := sampleCount) fun i =>
        UInt8.ofNat (samples.getD ((y * width + x) * sampleCount + i.1) 0)))
    width height

/-- Build a `sampleCount`-component, 16-bit-per-component image from a flat
    `width * height * sampleCount` sample array. -/
private def buildImage16 {α : Type} [Pixel α UInt16] (width height sampleCount : Nat)
    (samples : Array Nat) : Image α :=
  generateImage
    (fun x y => Pixel.fromComponents (α := α)
      (Array.ofFn (n := sampleCount) fun i =>
        UInt16.ofNat (samples.getD ((y * width + x) * sampleCount + i.1) 0)))
    width height

/-- Whether every entry of `bitsPerSample` is the same value (this port only
    supports uniform-bit-depth images), returning that common value. -/
private def uniformBitDepth (bitsPerSample : Array UInt32) : Option Nat :=
  match bitsPerSample[0]? with
  | none => none
  | some first => if bitsPerSample.all (· == first) then some first.toNat else none

/-- Build the RGB8 palette a `.paletted` image's `ColorMap` tag encodes
    (three consecutive `paletteSize`-length red/green/blue blocks of 16-bit
    values, downshifted to 8-bit — see the module doc-comment). -/
private def buildPalette8 (cmap : Array UInt16) : Palette :=
  let paletteSize := cmap.size / 3
  generateImage
    (fun x _ =>
      let at8 (block : Nat) : UInt8 := ((cmap.getD (block * paletteSize + x) 0) >>> 8).toUInt8
      (⟨at8 0, at8 1, at8 2⟩ : PixelRGB8))
    paletteSize 1

/-- Unpack an already-decompressed, predictor-undone TIFF into a
    `DynamicImage`/`PalettedImage` (upstream's `unpack`), given its parsed
    `TiffInfo`. See the module doc-comment's "pixel-format scope" section
    for exactly which colourspace/sample-count/bit-depth combinations this
    covers. -/
def unpackTiff (fileBytes : ByteArray) (info : TiffInfo) :
    Except String (Sum DynamicImage PalettedImage) := do
  let width := info.width.toNat
  let height := info.height.toNat
  let sc := info.sampleCount.toNat
  let allUint := info.sampleFormats.all (· == .uint)
  if !allUint then throw "Unsupported TIFF: non-integer sample format"
  let depth ← match uniformBitDepth info.bitsPerSample with
    | some d => pure d
    | none => throw "Unsupported TIFF: non-uniform bits-per-sample"
  if info.planarConfiguration != .contig then
    throw "Unsupported TIFF: separate planar configuration"
  if depth != 8 ∧ depth != 16 then
    throw s!"Unsupported TIFF bit depth ({depth})"
  match info.colorspace with
  | .paletted =>
      if depth != 8 ∨ sc != 1 then throw "Unsupported paletted TIFF layout"
      else
        match info.palette with
        | none => throw "Missing TIFF ColorMap for paletted image"
        | some cmap =>
            let samples ← gatherSamples info fileBytes depth sc
            let samples := applyPredictor info.predictor depth sc width height samples
            pure (.inr
              { indexedImage := { width, height, data := samples.map UInt8.ofNat }
                palette := buildPalette8 cmap, hasAlpha := false })
  | .monochrome | .monochromeWhite0 =>
      let samples ← gatherSamples info fileBytes depth sc
      let samples := applyPredictor info.predictor depth sc width height samples
      let invert8 (v : Pixel8) : Pixel8 := 255 - v
      let invert16 (v : Pixel16) : Pixel16 := 65535 - v
      let invertYA8 (p : PixelYA8) : PixelYA8 := { p with y := invert8 p.y }
      let invertYA16 (p : PixelYA16) : PixelYA16 := { p with y := invert16 p.y }
      let inverting := info.colorspace == .monochromeWhite0
      match sc, depth with
      | 1, 8 =>
          let img := buildImage8 (α := Pixel8) width height 1 samples
          pure (.inl (.y8 (if inverting then pixelMap invert8 img else img)))
      | 1, 16 =>
          let img := buildImage16 (α := Pixel16) width height 1 samples
          pure (.inl (.y16 (if inverting then pixelMap invert16 img else img)))
      | 2, 8 =>
          let img := buildImage8 (α := PixelYA8) width height 2 samples
          pure (.inl (.ya8 (if inverting then pixelMap invertYA8 img else img)))
      | 2, 16 =>
          let img := buildImage16 (α := PixelYA16) width height 2 samples
          pure (.inl (.ya16 (if inverting then pixelMap invertYA16 img else img)))
      | _, _ => throw "Unsupported TIFF: monochrome with unsupported sample count"
  | .rgb =>
      let samples ← gatherSamples info fileBytes depth sc
      let samples := applyPredictor info.predictor depth sc width height samples
      match sc, depth with
      | 3, 8 => pure (.inl (.rgb8 (buildImage8 (α := PixelRGB8) width height 3 samples)))
      | 3, 16 => pure (.inl (.rgb16 (buildImage16 (α := PixelRGB16) width height 3 samples)))
      | 4, 8 => pure (.inl (.rgba8 (buildImage8 (α := PixelRGBA8) width height 4 samples)))
      | 4, 16 => pure (.inl (.rgba16 (buildImage16 (α := PixelRGBA16) width height 4 samples)))
      | _, _ => throw "Unsupported TIFF: RGB with unsupported sample count"
  | .cmyk =>
      let samples ← gatherSamples info fileBytes depth sc
      let samples := applyPredictor info.predictor depth sc width height samples
      match sc, depth with
      | 4, 8 => pure (.inl (.cmyk8 (buildImage8 (α := PixelCMYK8) width height 4 samples)))
      | 4, 16 => pure (.inl (.cmyk16 (buildImage16 (α := PixelCMYK16) width height 4 samples)))
      | _, _ => throw "Unsupported TIFF: CMYK with unsupported sample count"
  | .transparencyMask | .ycbcr | .cieLab =>
      throw "Unsupported TIFF colorspace"

-- ── Top-level decode ──

/-- The `Type 0` core of `decodeTiffWithPaletteAndMetadata`: everything up
    to (and including) pixel unpacking, plus the flattened, resolved IFD
    entry list metadata extraction needs. Kept separate from metadata
    extraction itself so that this whole pipeline's `do`-block bind chain
    stays within a single, uniform `Type 0` `Monad (Except String)`
    instance — see `TiffInfo`'s own doc-comment for why a `Metadatas`
    (`Type 1`) value can't be threaded through the *same* `do` block as
    ordinary (`Type 0`) decode data. -/
private def decodeTiffCore (input : ByteArray) :
    Except String (Sum DynamicImage PalettedImage × List ImageFileDirectory) := do
  let (header, _) ← parseTiffHeader input.toList
  let ifdLists ← parseIfdChain header.endianness input header.offset
  let cleaned := ifdLists.flatten
  let info ← buildTiffInfo header cleaned
  let img ← unpackTiff input info
  pure (img, cleaned)

/-- Decode a TIFF file, with its palette (if any) kept separate, plus its
    metadata (upstream's `decodeTiffWithPaletteAndMetadata`). Pure — see the
    module doc-comment for why this module never needs `IO`, unlike
    `Linen.Codec.Picture.Png`. -/
def decodeTiffWithPaletteAndMetadata (input : ByteArray) :
    Except String (Sum DynamicImage PalettedImage × Metadatas) :=
  match decodeTiffCore input with
  | .error e => .error e
  | .ok (img, cleaned) => .ok (img, extractTiffMetadata cleaned)

/-- Decode a TIFF file, collapsing any indexed result down to a true-colour
    image via `palettedToTrueColor` (upstream's `decodeTiffWithMetadata`). -/
def decodeTiffWithMetadata (input : ByteArray) : Except String (DynamicImage × Metadatas) :=
  match decodeTiffWithPaletteAndMetadata input with
  | .error e => .error e
  | .ok (.inl img, m) => .ok (img, m)
  | .ok (.inr pal, m) => .ok (.rgb8 (palettedToTrueColor pal), m)

/-- Decode a TIFF file, discarding its metadata (upstream's `decodeTiff`). -/
def decodeTiff (input : ByteArray) : Except String DynamicImage :=
  match decodeTiffWithMetadata input with
  | .error e => .error e
  | .ok (img, _) => .ok img

-- ── Top-level encode ──

/-- Which pixel types can be serialized as a TIFF file (upstream's
    `TiffSaveable` class) — see the module doc-comment for why this has no
    `PixelF`/`Pixel32`/`PixelYCbCr8` instance. -/
class TiffSaveable (α : Type) {Component : outParam Type} [Pixel α Component] where
  /-- The photometric interpretation to declare. -/
  colorSpace : TiffColorspace
  /-- The extra-sample tag to declare, if `α` carries one beyond its colour
      model's own samples (e.g. alpha). -/
  extraSample : Option ExtraSample := none
  /-- The sample format to declare. -/
  sampleFormat : TiffSampleFormat := .uint
  /-- A component's on-disk byte width (this port's stand-in for upstream's
      `Foreign.Storable.sizeOf`). -/
  componentByteSize : Nat
  /-- Serialise one component, honouring `endian`. -/
  putComponent : TiffEndianness → Component → Builder

instance : TiffSaveable Pixel8 where
  colorSpace := .monochrome
  componentByteSize := 1
  putComponent _ v := Builder.word8 v

instance : TiffSaveable Pixel16 where
  colorSpace := .monochrome
  componentByteSize := 2
  putComponent endian v := putU16 endian v

instance : TiffSaveable PixelYA8 where
  colorSpace := .monochrome
  extraSample := some .unassociatedAlpha
  componentByteSize := 1
  putComponent _ v := Builder.word8 v

instance : TiffSaveable PixelYA16 where
  colorSpace := .monochrome
  extraSample := some .unassociatedAlpha
  componentByteSize := 2
  putComponent endian v := putU16 endian v

instance : TiffSaveable PixelRGB8 where
  colorSpace := .rgb
  componentByteSize := 1
  putComponent _ v := Builder.word8 v

instance : TiffSaveable PixelRGB16 where
  colorSpace := .rgb
  componentByteSize := 2
  putComponent endian v := putU16 endian v

instance : TiffSaveable PixelRGBA8 where
  colorSpace := .rgb
  extraSample := some .unassociatedAlpha
  componentByteSize := 1
  putComponent _ v := Builder.word8 v

instance : TiffSaveable PixelRGBA16 where
  colorSpace := .rgb
  extraSample := some .unassociatedAlpha
  componentByteSize := 2
  putComponent endian v := putU16 endian v

instance : TiffSaveable PixelCMYK8 where
  colorSpace := .cmyk
  componentByteSize := 1
  putComponent _ v := Builder.word8 v

instance : TiffSaveable PixelCMYK16 where
  colorSpace := .cmyk
  componentByteSize := 2
  putComponent endian v := putU16 endian v

/-- `ifdSingleLong`: a single inline `.long` entry. -/
private def ifdSingleLong (tag : ExifTag) (v : UInt32) : ImageFileDirectory :=
  { ifdIdentifier := tag, ifdType := .long, ifdCount := 1, ifdOffset := v, ifdExtended := .none }

/-- `ifdMultiLong`: a `.long`-typed entry, inlined if it holds a single
    value, out-of-line otherwise. -/
private def ifdMultiLong (tag : ExifTag) (v : Array UInt32) : ImageFileDirectory :=
  if v.size > 1 then
    { ifdIdentifier := tag, ifdType := .long, ifdCount := v.size.toUInt32
      ifdOffset := 0, ifdExtended := .longs v }
  else
    { ifdIdentifier := tag, ifdType := .long, ifdCount := 1
      ifdOffset := v.getD 0 0, ifdExtended := .none }

/-- `ifdMultiShort`: a `.short`-typed entry, inlined (one or two values,
    honouring `endian`'s left-justification convention for a lone value —
    see module 15's own `ImageFileDirectory.ifdOffset` doc-comment) or
    out-of-line (three or more values). -/
private def ifdMultiShort (endian : TiffEndianness) (tag : ExifTag) (v : Array UInt32) :
    ImageFileDirectory :=
  let size := v.size
  if size > 2 then
    { ifdIdentifier := tag, ifdType := .short, ifdCount := size.toUInt32
      ifdOffset := 0, ifdExtended := .shorts (v.map (·.toUInt16)) }
  else if size == 2 then
    let v1 := v.getD 0 0
    let v2 := v.getD 1 0
    let combined := match endian with
      | .little => (v2 <<< 16) ||| v1
      | .big => (v1 <<< 16) ||| v2
    { ifdIdentifier := tag, ifdType := .short, ifdCount := 2, ifdOffset := combined, ifdExtended := .none }
  else
    let v1 := v.getD 0 0
    let ofs := match endian with
      | .little => v1
      | .big => v1 <<< 16
    { ifdIdentifier := tag, ifdType := .short, ifdCount := 1, ifdOffset := ofs, ifdExtended := .none }

/-- `ifdSingleShort`: a single inline `.short` entry. -/
private def ifdSingleShort (endian : TiffEndianness) (tag : ExifTag) (v : UInt16) :
    ImageFileDirectory :=
  ifdMultiShort endian tag #[v.toUInt32]

/-- The on-disk byte size of an `ExifData` value as `serializeExtended`
    writes it (only the constructors `ifdMultiShort`/`ifdMultiLong` ever
    produce). -/
private def extendedByteSize : ExifData → Nat
  | .shorts v => v.size * 2
  | .longs v => v.size * 4
  | _ => 0

/-- Serialise an out-of-line `ExifData` value, honouring `endian`. -/
private def serializeExtended (endian : TiffEndianness) : ExifData → Builder
  | .shorts v => v.foldl (fun acc s => acc ++ putU16 endian s) Builder.empty
  | .longs v => v.foldl (fun acc s => acc ++ putU32 endian s) Builder.empty
  | _ => Builder.empty

/-- Assign every entry with a non-`.none` `ifdExtended` an `ifdOffset`
    starting at `startOffset`, in entry order (this module's from-scratch
    "lay out a fixed IFD entry list plus its extended data for writing" —
    see the module doc-comment for why module 15's own version of this
    doesn't apply here). -/
private def layoutIfdExtended (startOffset : UInt32) (entries : List ImageFileDirectory) :
    List ImageFileDirectory :=
  Id.run do
    let mut cursor := startOffset
    let mut out : List ImageFileDirectory := []
    for e in entries do
      if e.ifdExtended == ExifData.none then
        out := out ++ [e]
      else
        out := out ++ [{ e with ifdOffset := cursor }]
        cursor := cursor + (extendedByteSize e.ifdExtended).toUInt32
    pure out

/-- Encode `img` as a single-strip, uncompressed, little-endian TIFF file
    (upstream's `encodeTiff` — see the module doc-comment for why the encode
    side has no richer scope to port). -/
def encodeTiff {α Component : Type} [Pixel α Component] [TiffSaveable α]
    (img : @Image α Component _) : Data.ByteString :=
  let endian : TiffEndianness := .little
  let width := img.width.toUInt32
  let height := img.height.toUInt32
  let sampleCount := (Pixel.componentCount α).toUInt32
  let compByteSize := TiffSaveable.componentByteSize α
  let bitPerSample : UInt32 := (compByteSize * 8).toUInt32
  let headerSize : UInt32 := 8
  let imageSize : UInt32 := width * height * sampleCount * compByteSize.toUInt32
  let baseEntries : List ImageFileDirectory :=
    [ ifdSingleLong .imageWidth width
    , ifdSingleLong .imageLength height
    , ifdMultiShort endian .bitsPerSample (Array.replicate sampleCount.toNat bitPerSample)
    , ifdSingleLong .samplesPerPixel sampleCount
    , ifdSingleLong .rowPerStrip height
    , ifdSingleShort endian .photometricInterpretation (packPhotometricInterpretation (TiffSaveable.colorSpace α))
    , ifdSingleShort endian .planarConfiguration (constantOfPlanarConfg .contig)
    , ifdMultiLong .sampleFormat #[packSampleFormat (TiffSaveable.sampleFormat α)]
    , ifdSingleShort endian .compression (packCompression .none)
    , ifdMultiLong .stripOffsets #[headerSize]
    , ifdMultiLong .stripByteCounts #[imageSize] ]
  let entries := baseEntries ++
    (match TiffSaveable.extraSample α with
     | some es => [ifdSingleShort endian .extraSample (codeOfExtraSample es)]
     | none => [])
  let ifdBlockOffset := headerSize + imageSize
  let ifdBlockSize : UInt32 := (2 + entries.length * 12 + 4).toUInt32
  let extendedStart := ifdBlockOffset + ifdBlockSize
  let laidOutEntries := layoutIfdExtended extendedStart entries
  let header : TiffHeader := { endianness := endian, offset := ifdBlockOffset }
  let pixelBuilder := img.data.foldl (fun acc c => acc ++ TiffSaveable.putComponent α endian c) Builder.empty
  let ifdBuilder := putImageFileDirectoryList endian laidOutEntries 0
  let extendedBuilder := laidOutEntries.foldl
    (fun acc e => acc ++ serializeExtended endian e.ifdExtended) Builder.empty
  (putTiffHeader header ++ pixelBuilder ++ ifdBuilder ++ extendedBuilder).toStrictByteString

end Codec.Picture
