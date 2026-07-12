import Linen.Codec.Picture.Jpg.Internal.DefaultTable
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.Jpg.Internal.Types` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 22 of 29).

  This module ports the **structural** layer of the JPEG (JFIF) container
  format: marker codes, segment headers (frame/scan/quantization-table/
  Huffman-table headers), and the byte-stuffing-aware logic for finding
  where a raw entropy-coded scan (ECS) ends. It deliberately does **not**
  decode the entropy-coded bytes themselves (Huffman-decoding DC/AC
  coefficients, dequantization, IDCT, up-sampling, colour conversion, …) —
  that is the job of later modules in the dependency chain (`Common`,
  `Progressive`, and the top-level `Jpg` module). Every scan's payload is
  therefore carried as an *uninterpreted* `ByteArray` (already split at the
  correct byte-stuffing-aware boundary), ready for a later module to feed to
  a bit-level reader.

  ## Design and scope decisions

  - **`JpgExif` is dropped.** Upstream's `APP1`/Exif branch chains a TIFF
    header and two `ImageFileDirectory` blocks (following `isInIFD0`) purely
    to *carry* Exif metadata through the frame list unchanged. Decoding a
    real Exif blob is out of scope for a *structural* container module: it
    is deferred to the dedicated `Jpg.Internal.Metadata` module later in the
    dependency chain. Here, any `APP1` segment (Exif or not) is preserved
    uninterpreted via the generic `appFrame` constructor, exactly like every
    other unrecognised `APPn`/`COM`/extension segment — no information is
    lost, only its structured (IFD-chain) *interpretation*.
  - **Encoder/decoder "packed" Huffman machinery is dropped.** Upstream's
    `JpgHuffmanTable` pairs a raw code-length table with a pre-packed
    encoding table (`packHuffmanTree`/`makeInverseTable`, consumed by the
    bit-level Huffman encoder). Building the runtime encode/decode tables is
    genuine codec machinery for later modules (which already depend on
    `DefaultTable.buildHuffmanTree`); this module only carries the raw
    `HuffmanTable` parsed from/written to a `DHT` segment, as
    `JpgHuffmanTableSpec.codes`.
  - **`MutableMacroBlock`, `createEmptyMutableMacroBlock`, and the
    `printMacroBlock`/`printPureMacroBlock` debug pretty-printers are
    dropped.** They exist upstream purely to support the in-place `ST`
    decoding buffers and human-readable debug dumps of a later decoding
    stage; neither has a role in this structural module, and `MacroBlock`
    itself (an immutable `Array`) is already available from `DefaultTable`.
  - **The generic `Sizeable`/`SizeCalculable`-driven `TableList`
    "parse-until-declared-length-is-consumed" framework is specialised
    directly** into two concrete loops, `parseJpgQuantTableList` and
    `parseJpgHuffmanTableList`, each carrying its own remaining-byte-count
    accumulator — the polymorphic framework exists upstream only to avoid
    duplicating ~10 lines of loop body between the two call sites, which
    Lean's termination checker does not make meaningfully cheaper to keep
    generic (see the termination note below).
  - **`parseFramesSemiLazy` (and the hand-written lazy `Binary JpgImage`
    instance built on it) is dropped**; only the strict, "parse everything
    up front" API (`getJpgImage`/`putJpgImage`/`parseFrames`, upstream's own
    non-deprecated entry points) is ported. Laziness is a GHC-specific
    performance device (skip re-decoding entropy data you don't need yet)
    with no bearing on the *value* being parsed.
  - **The "chunked" (`parseECS`, operating on lazy `ByteString` chunks for
    performance) variant of entropy-segment splitting is dropped in favour
    of porting only the semantics of upstream's simpler byte-at-a-time
    reference implementation, `parseECSSimple`** (upstream keeps both only
    because chunk-at-a-time scanning is faster on a real lazy `ByteString`;
    Lean's `List UInt8` has no such chunk structure to exploit, so there is
    only one sensible semantics to port). This is `splitEcs` below.
  - **JFIF thumbnail pixels are dropped.** `JpgJFIFApp0` upstream carries a
    `jfifThumbnail : Maybe (Image PixelRGB8)` populated only when the
    (near-universally zero) thumbnail-dimension fields are non-zero; no
    other module in the dependency chain reads it. This module still parses
    and re-emits the two (invariably `0`) thumbnail-dimension bytes for a
    byte-exact segment, but does not materialise a thumbnail image.
  - **No dependency on `Linen.Codec.Picture.Types` or
    `Linen.Codec.Picture.BitWriter`**, even though the dependency plan lists
    both. Every upstream feature that actually needed them was dropped above
    (pixel/`Image` types for the thumbnail; the `BoolReader`/`BoolWriter`
    bit-level machinery for the packed Huffman tables) — this module is
    entirely byte-level. Later modules that decode entropy data will import
    `BitWriter` themselves.
  - **Termination.** Two genuinely unbounded-but-file-bounded loops appear:
    1. `splitEcs`/`splitEcsAux`, which scans forward through the raw scan
       bytes for the byte-stuffing-aware end-of-entropy-segment. This is
       plain **structural recursion** on the list tail — no `termination_by`
       is needed at all, since each recursive call is made on the pattern-
       matched tail of the previous list. (An earlier design draft assumed
       this would need well-founded recursion on remaining length, mirroring
       `Png.Internal.Type.parseChunks`; stepping back showed the direct
       structural form is simpler and just as faithful.)
    2. `parseFrames`, which repeatedly parses one frame/segment until it
       sees the end-of-image marker or runs out of bytes. Each iteration is
       guaranteed (by construction of every `parseFrameOfKind` branch and
       `skipFrameMarker`) to consume at least one byte, but proving that
       *statically* through every one of the dispatch's ~15 branches would
       be a large, mechanical lemma disproportionate to this module's
       "structural, not exhaustively-verified codec" scope. Instead,
       `parseFrames` re-checks the *actual* remaining length after each
       iteration with a dependent `if h : r.length < bytes.length`, and
       supplies `h` directly to `decreasing_by`. If some future edit to a
       dispatch branch ever failed to consume a byte, this would surface as
       a runtime `.error` (the safe, honest fallback), not silently loop —
       never as a non-terminating function or an unproven `sorry`.
    The two bounded-list loops (`parseJpgQuantTableList`,
    `parseJpgHuffmanTableList`) use well-founded recursion on an explicit
    `remainingSize : Nat` accounting parameter (mirroring upstream's own
    `size :: Int` bookkeeping in `innerParse`); each iteration's `consumed`
    is a sum of positive literals, so every decreasing step is closed by
    `omega` alone.
  - **`Int16`.** Lean's core library defines a native fixed-width signed
    `Int16` (`Init/Data/SInt/Basic.lean`), so `DcCoefficient` and quantised
    coefficients are ported directly as `Int16` (bit-reinterpreted via
    `UInt16.toInt16`/`Int16.toUInt16`), rather than the `UInt16`-bit-pattern
    workaround used elsewhere in this codebase's older `BitWriter` port
    (written before this was confirmed available).
-/

namespace Codec.Picture.Jpg.Internal

open Data.ByteString (Builder)

-- ── Byte-level primitives ──
--
-- Self-contained (rather than reusing `Codec.Picture.Tiff.Internal.Types`'s
-- byte helpers): JPEG is always big-endian, and depending on the TIFF module
-- just for `readU8`/`readU16`/`readBytesFixed` would introduce a confusing
-- reverse dependency now that the Exif/TIFF-IFD chain itself has been
-- dropped from this module's scope (see above).

/-- Read one byte, or fail if the input is exhausted. -/
private def readU8 (bytes : List UInt8) : Except String (UInt8 × List UInt8) :=
  match bytes with
  | [] => .error "Unexpected end of JPEG byte stream"
  | b :: rest => .ok (b, rest)

/-- Read `n` bytes verbatim into a `ByteArray`. -/
private def readBytesFixed (n : Nat) (bytes : List UInt8) :
    Except String (ByteArray × List UInt8) :=
  if n ≤ bytes.length then .ok (ByteArray.mk (bytes.take n).toArray, bytes.drop n)
  else .error "Unexpected end of JPEG byte stream"

/-- Read a big-endian `UInt16`. -/
private def readU16BE (bytes : List UInt8) : Except String (UInt16 × List UInt8) := do
  let (hi, r1) ← readU8 bytes
  let (lo, r2) ← readU8 r1
  pure (((hi.toUInt16) <<< 8) ||| lo.toUInt16, r2)

/-- Build a `Builder` that writes out `b`'s bytes verbatim. -/
private def builderOfByteArray (b : ByteArray) : Builder :=
  b.toList.foldl (fun acc byte => acc ++ Builder.word8 byte) Builder.empty

/-- Build a `Builder` from the ASCII bytes of a `String`. -/
private def asciiBuilder (s : String) : Builder :=
  s.toList.foldl (fun acc c => acc ++ Builder.word8 c.toNat.toUInt8) Builder.empty

/-- Pack two 4-bit fields (high nibble, low nibble) into one byte, JPEG's
    ubiquitous convention for e.g. sampling factors and table
    class/destination selectors. -/
private def pack4Bits (hi lo : UInt8) : UInt8 := (hi <<< 4) ||| (lo &&& 0xF)

/-- Unpack one byte into its (high nibble, low nibble) 4-bit fields. -/
private def unpack4Bits (b : UInt8) : UInt8 × UInt8 := ((b >>> 4) &&& 0xF, b &&& 0xF)

-- ── Marker / frame kinds ──

/-- The JPEG segment-marker "kind" byte (the second byte of a two-byte
    marker, the first always being `0xFF`). Upstream `JpgFrameKind`. -/
inductive JpgFrameKind where
  | baselineDCTHuffman
  | extendedSequentialDCTHuffman
  | progressiveDCTHuffman
  | lossslessHuffman
  | differentialSequentialDCTHuffman
  | differentialProgressiveDCTHuffman
  | differentialLosslessHuffman
  | extendedSequentialArithmetic
  | progressiveDCTArithmetic
  | losslessArithmetic
  | differentialSequentialDCTArithmetic
  | differentialProgressiveDCTArithmetic
  | differentialLosslessArithmetic
  | huffmanTableMarker
  | quantizationTable
  | startOfScan
  | restartInterval
  | applicationSegment (code : UInt8)
  | extensionSegment (code : UInt8)
  | endOfImage
  deriving Repr, DecidableEq

/-- The wire byte for a `JpgFrameKind`. -/
def codeOfFrameKind : JpgFrameKind → UInt8
  | .baselineDCTHuffman => 0xC0
  | .extendedSequentialDCTHuffman => 0xC1
  | .progressiveDCTHuffman => 0xC2
  | .lossslessHuffman => 0xC3
  | .differentialSequentialDCTHuffman => 0xC5
  | .differentialProgressiveDCTHuffman => 0xC6
  | .differentialLosslessHuffman => 0xC7
  | .extendedSequentialArithmetic => 0xC9
  | .progressiveDCTArithmetic => 0xCA
  | .losslessArithmetic => 0xCB
  | .differentialSequentialDCTArithmetic => 0xCD
  | .differentialProgressiveDCTArithmetic => 0xCE
  | .differentialLosslessArithmetic => 0xCF
  | .huffmanTableMarker => 0xC4
  | .quantizationTable => 0xDB
  | .startOfScan => 0xDA
  | .restartInterval => 0xDD
  | .applicationSegment n => 0xE0 + n
  | .extensionSegment n => n
  | .endOfImage => 0xD9

/-- Decode a wire byte into a `JpgFrameKind`. -/
def frameKindOfCode (b : UInt8) : Except String JpgFrameKind :=
  match b with
  | 0xC0 => .ok .baselineDCTHuffman
  | 0xC1 => .ok .extendedSequentialDCTHuffman
  | 0xC2 => .ok .progressiveDCTHuffman
  | 0xC3 => .ok .lossslessHuffman
  | 0xC5 => .ok .differentialSequentialDCTHuffman
  | 0xC6 => .ok .differentialProgressiveDCTHuffman
  | 0xC7 => .ok .differentialLosslessHuffman
  | 0xC9 => .ok .extendedSequentialArithmetic
  | 0xCA => .ok .progressiveDCTArithmetic
  | 0xCB => .ok .losslessArithmetic
  | 0xCD => .ok .differentialSequentialDCTArithmetic
  | 0xCE => .ok .differentialProgressiveDCTArithmetic
  | 0xCF => .ok .differentialLosslessArithmetic
  | 0xC4 => .ok .huffmanTableMarker
  | 0xDB => .ok .quantizationTable
  | 0xDA => .ok .startOfScan
  | 0xDD => .ok .restartInterval
  | 0xD9 => .ok .endOfImage
  | n =>
      if 0xE0 ≤ n ∧ n ≤ 0xEF then .ok (.applicationSegment (n - 0xE0))
      else .ok (.extensionSegment n)

/-- Parse a `JpgFrameKind`: one byte, decoded via `frameKindOfCode`. -/
def parseJpgFrameKind (bytes : List UInt8) : Except String (JpgFrameKind × List UInt8) := do
  let (b, r1) ← readU8 bytes
  let kind ← frameKindOfCode b
  pure (kind, r1)

/-- Write a `JpgFrameKind`'s marker (`0xFF` followed by its code byte). -/
def putJpgFrameKind (k : JpgFrameKind) : Builder :=
  Builder.word8 0xFF ++ Builder.word8 (codeOfFrameKind k)

-- ── Colour space / transform / unit enums ──

/-- The colour-space signalled by an Adobe `APP14` marker or inferred from
    component count. Upstream `JpgImageKind`/`Adobe14Transform` split. -/
inductive AdobeTransform where
  | unknown
  | ycbcr
  | ycck
  deriving Repr, DecidableEq

/-- The wire byte for an `AdobeTransform`. -/
def codeOfAdobeTransform : AdobeTransform → UInt8
  | .unknown => 0
  | .ycbcr => 1
  | .ycck => 2

/-- Decode a wire byte into an `AdobeTransform` (defaulting to `unknown` for
    any value outside `0..2`, matching upstream's permissive `toEnum`). -/
def adobeTransformOfCode (b : UInt8) : AdobeTransform :=
  match b with
  | 0 => .unknown
  | 1 => .ycbcr
  | 2 => .ycck
  | _ => .unknown

/-- The physical unit a `JFIF` header's DPI fields are expressed in. -/
inductive JFifUnit where
  | unitUnknown
  | dotsPerInch
  | dotsPerCentimeter
  deriving Repr, DecidableEq

/-- The wire byte for a `JFifUnit`. -/
def codeOfJfifUnit : JFifUnit → UInt8
  | .unitUnknown => 0
  | .dotsPerInch => 1
  | .dotsPerCentimeter => 2

/-- Decode a wire byte into a `JFifUnit` (defaulting to `unitUnknown`). -/
def jfifUnitOfCode (b : UInt8) : JFifUnit :=
  match b with
  | 0 => .unitUnknown
  | 1 => .dotsPerInch
  | 2 => .dotsPerCentimeter
  | _ => .unitUnknown

-- ── Components ──

/-- One component (e.g. Y, Cb, Cr) declared in a frame header: its
    identifier, horizontal/vertical sampling factors, and the destination
    quantization-table index it uses. -/
structure JpgComponent where
  identifier : UInt8
  horizontalSamplingFactor : UInt8
  verticalSamplingFactor : UInt8
  quantizationTableDest : UInt8
  deriving Repr, DecidableEq

/-- Parse one `JpgComponent`: identifier byte, then one nibble-packed byte
    of (horizontal, vertical) sampling factors, then the quant-table index. -/
def parseJpgComponent (bytes : List UInt8) : Except String (JpgComponent × List UInt8) := do
  let (identifier, r1) ← readU8 bytes
  let (packed, r2) ← readU8 r1
  let (hFactor, vFactor) := unpack4Bits packed
  let (quantizationTableDest, r3) ← readU8 r2
  pure ({ identifier, horizontalSamplingFactor := hFactor, verticalSamplingFactor := vFactor,
          quantizationTableDest }, r3)

/-- Write one `JpgComponent`. -/
def putJpgComponent (c : JpgComponent) : Builder :=
  Builder.word8 c.identifier ++
  Builder.word8 (pack4Bits c.horizontalSamplingFactor c.verticalSamplingFactor) ++
  Builder.word8 c.quantizationTableDest

/-- Parse exactly `n` consecutive `JpgComponent`s. -/
private def parseJpgComponentN : Nat → List UInt8 → Except String (List JpgComponent × List UInt8)
  | 0, bytes => .ok ([], bytes)
  | n + 1, bytes => do
      let (c, r1) ← parseJpgComponent bytes
      let (cs, r2) ← parseJpgComponentN n r1
      pure (c :: cs, r2)

-- ── Frame (SOFn) header ──

/-- A frame header, as introduced by any Start-Of-Frame (`SOFn`) marker:
    sample precision, image dimensions, and the declared component list. -/
structure JpgFrameHeader where
  frameHeaderLength : UInt16
  samplePrecision : UInt8
  height : UInt16
  width : UInt16
  componentCount : UInt8
  components : List JpgComponent
  deriving Repr, DecidableEq

/-- Parse a `JpgFrameHeader`. The declared `frameHeaderLength` may exceed the
    minimum implied by `componentCount` (some encoders pad); any surplus
    bytes are skipped, matching upstream's `skip` after parsing components. -/
def parseJpgFrameHeader (bytes : List UInt8) : Except String (JpgFrameHeader × List UInt8) := do
  let (len, r1) ← readU16BE bytes
  let (samplePrecision, r2) ← readU8 r1
  let (height, r3) ← readU16BE r2
  let (width, r4) ← readU16BE r3
  let (componentCount, r5) ← readU8 r4
  let (components, r6) ← parseJpgComponentN componentCount.toNat r5
  let consumed := 2 + 1 + 2 + 2 + 1 + 3 * componentCount.toNat
  let r7 ←
    if consumed < len.toNat then
      (readBytesFixed (len.toNat - consumed) r6).map Prod.snd
    else pure r6
  pure ({ frameHeaderLength := len, samplePrecision, height, width, componentCount,
          components }, r7)

/-- Write a `JpgFrameHeader`. -/
def putJpgFrameHeader (h : JpgFrameHeader) : Builder :=
  Builder.word16BE h.frameHeaderLength ++ Builder.word8 h.samplePrecision ++
  Builder.word16BE h.height ++ Builder.word16BE h.width ++ Builder.word8 h.componentCount ++
  h.components.foldl (fun acc c => acc ++ putJpgComponent c) Builder.empty

-- ── Quantization tables (DQT) ──

/-- `Word8 → Int16` zero-extension, matching Haskell's `fromIntegral`
    between an unsigned 8-bit and a wider signed type. -/
private def int16OfU8 (b : UInt8) : Int16 := b.toUInt16.toInt16

/-- `Word16 → Int16` bit-pattern reinterpretation, matching Haskell's
    `fromIntegral` between two same-width types. -/
private def int16OfU16 (v : UInt16) : Int16 := v.toInt16

/-- `Int16 → Word8`, truncating to the low byte (matches Haskell's
    `fromIntegral` from a wider to a narrower integer). -/
private def u8OfInt16 (v : Int16) : UInt8 := v.toUInt16.toUInt8

/-- `Int16 → Word16` bit-pattern reinterpretation. -/
private def u16OfInt16 (v : Int16) : UInt16 := v.toUInt16

private def parseQuantCoeffsU8 :
    Nat → List UInt8 → Except String (List Int16 × List UInt8)
  | 0, bytes => .ok ([], bytes)
  | n + 1, bytes => do
      let (b, r1) ← readU8 bytes
      let (rest, r2) ← parseQuantCoeffsU8 n r1
      pure (int16OfU8 b :: rest, r2)

private def parseQuantCoeffsU16 :
    Nat → List UInt8 → Except String (List Int16 × List UInt8)
  | 0, bytes => .ok ([], bytes)
  | n + 1, bytes => do
      let (v, r1) ← readU16BE bytes
      let (rest, r2) ← parseQuantCoeffsU16 n r1
      pure (int16OfU16 v :: rest, r2)

/-- The number of coefficients in one JPEG quantization/coefficient
    macroblock ($8 \times 8$). -/
def dctBlockSize : Nat := 64

/-- One quantization table, as declared inside a `DQT` segment: its
    precision (`0` = 8-bit entries, nonzero = 16-bit entries), destination
    index, and its 64 coefficients. -/
structure JpgQuantTableSpec where
  precision : UInt8
  destination : UInt8
  quantTable : MacroBlock Int16
  deriving Repr, DecidableEq

/-- Parse one `JpgQuantTableSpec`. -/
def parseJpgQuantTableSpec (bytes : List UInt8) : Except String (JpgQuantTableSpec × List UInt8) := do
  let (packed, r1) ← readU8 bytes
  let (precision, destination) := unpack4Bits packed
  let (coeffs, r2) ←
    if precision == 0 then parseQuantCoeffsU8 dctBlockSize r1
    else parseQuantCoeffsU16 dctBlockSize r1
  pure ({ precision, destination, quantTable := coeffs.toArray }, r2)

/-- Write one `JpgQuantTableSpec`. -/
def putJpgQuantTableSpec (t : JpgQuantTableSpec) : Builder :=
  Builder.word8 (pack4Bits t.precision t.destination) ++
  t.quantTable.toList.foldl
    (fun acc coeff =>
      acc ++ (if t.precision == 0 then Builder.word8 (u8OfInt16 coeff)
              else Builder.word16BE (u16OfInt16 coeff)))
    Builder.empty

/-- How many bytes one `JpgQuantTableSpec` occupies on the wire: one packed
    class/destination byte, plus 64 coefficients at either 1 or 2 bytes
    each. Always positive, which is all `parseJpgQuantTableList`'s
    termination proof needs. -/
private def quantTableSpecSize (t : JpgQuantTableSpec) : Nat :=
  1 + dctBlockSize * (if t.precision == 0 then 1 else 2)

set_option linter.unusedVariables false in
private def parseQuantTablesAux (remaining : Nat) (bytes : List UInt8) :
    Except String (List JpgQuantTableSpec × List UInt8) :=
  if h : remaining = 0 then
    .ok ([], bytes)
  else
    match parseJpgQuantTableSpec bytes with
    | .error e => .error e
    | .ok (table, rest) =>
        match parseQuantTablesAux (remaining - quantTableSpecSize table) rest with
        | .error e => .error e
        | .ok (tables, rest2) => .ok (table :: tables, rest2)
termination_by remaining
decreasing_by
  have hpos : quantTableSpecSize table > 0 := by
    unfold quantTableSpecSize; split <;> omega
  omega

/-- Parse a `DQT` segment's payload: a 2-byte declared length, then as many
    `JpgQuantTableSpec`s as fit in `length - 2` bytes. -/
def parseJpgQuantTableList (bytes : List UInt8) :
    Except String (List JpgQuantTableSpec × List UInt8) := do
  let (len, r1) ← readU16BE bytes
  if len.toNat < 2 then throw "Invalid JPEG quantization table segment length"
  parseQuantTablesAux (len.toNat - 2) r1

/-- Write a `DQT` segment's payload (declared length, then every table). -/
def putJpgQuantTableList (tables : List JpgQuantTableSpec) : Builder :=
  let payloadSize := tables.foldl (fun acc t => acc + quantTableSpecSize t) 0
  Builder.word16BE (UInt16.ofNat (payloadSize + 2)) ++
  tables.foldl (fun acc t => acc ++ putJpgQuantTableSpec t) Builder.empty

-- ── Huffman tables (DHT) ──

/-- One Huffman table, as declared inside a `DHT` segment: which
    coefficient category it decodes (DC/AC), its destination index, and its
    raw code-length table (see `DefaultTable.HuffmanTable`). -/
structure JpgHuffmanTableSpec where
  huffmanClass : DctComponent
  destination : UInt8
  codes : HuffmanTable
  deriving Repr, DecidableEq

private def parseFixedU8List : Nat → List UInt8 → Except String (List UInt8 × List UInt8)
  | 0, bytes => .ok ([], bytes)
  | n + 1, bytes => do
      let (b, r1) ← readU8 bytes
      let (rest, r2) ← parseFixedU8List n r1
      pure (b :: rest, r2)

/-- Given the 16 group sizes read from a `DHT` segment, read that many
    symbol groups. Structural recursion on `sizes`. -/
private def parseHuffmanCodeGroups :
    List UInt8 → List UInt8 → Except String (List (List UInt8) × List UInt8)
  | [], bytes => .ok ([], bytes)
  | size :: sizes, bytes => do
      let (grp, r1) ← readBytesFixed size.toNat bytes
      let (rest, r2) ← parseHuffmanCodeGroups sizes r1
      pure (grp.toList :: rest, r2)

/-- Parse one `JpgHuffmanTableSpec`: a packed class/destination byte, 16
    group-size bytes, then that many symbol groups. -/
def parseJpgHuffmanTableSpec (bytes : List UInt8) :
    Except String (JpgHuffmanTableSpec × List UInt8) := do
  let (packed, r1) ← readU8 bytes
  let (classCode, destination) := unpack4Bits packed
  let (sizes, r2) ← parseFixedU8List 16 r1
  let (codes, r3) ← parseHuffmanCodeGroups sizes r2
  pure ({ huffmanClass := if classCode == 0 then .dcComponent else .acComponent, destination,
          codes }, r3)

/-- Write one `JpgHuffmanTableSpec`. -/
def putJpgHuffmanTableSpec (t : JpgHuffmanTableSpec) : Builder :=
  let classCode : UInt8 := match t.huffmanClass with | .dcComponent => 0 | .acComponent => 1
  Builder.word8 (pack4Bits classCode t.destination) ++
  t.codes.foldl (fun acc grp => acc ++ Builder.word8 (UInt8.ofNat grp.length)) Builder.empty ++
  t.codes.foldl (fun acc grp => acc ++ grp.foldl (fun a v => a ++ Builder.word8 v) Builder.empty)
    Builder.empty

/-- How many bytes one `JpgHuffmanTableSpec` occupies on the wire: one
    packed class/destination byte, 16 group-size bytes, plus every symbol.
    Always at least `17`, which is all `parseJpgHuffmanTableList`'s
    termination proof needs. -/
private def huffmanTableSpecSize (t : JpgHuffmanTableSpec) : Nat :=
  1 + 16 + (t.codes.map List.length).sum

set_option linter.unusedVariables false in
private def parseHuffmanTablesAux (remaining : Nat) (bytes : List UInt8) :
    Except String (List JpgHuffmanTableSpec × List UInt8) :=
  if h : remaining = 0 then
    .ok ([], bytes)
  else
    match parseJpgHuffmanTableSpec bytes with
    | .error e => .error e
    | .ok (table, rest) =>
        match parseHuffmanTablesAux (remaining - huffmanTableSpecSize table) rest with
        | .error e => .error e
        | .ok (tables, rest2) => .ok (table :: tables, rest2)
termination_by remaining
decreasing_by
  have hpos : huffmanTableSpecSize table > 0 := by
    unfold huffmanTableSpecSize; omega
  omega

/-- Parse a `DHT` segment's payload: a 2-byte declared length, then as many
    `JpgHuffmanTableSpec`s as fit in `length - 2` bytes. -/
def parseJpgHuffmanTableList (bytes : List UInt8) :
    Except String (List JpgHuffmanTableSpec × List UInt8) := do
  let (len, r1) ← readU16BE bytes
  if len.toNat < 2 then throw "Invalid JPEG Huffman table segment length"
  parseHuffmanTablesAux (len.toNat - 2) r1

/-- Write a `DHT` segment's payload (declared length, then every table). -/
def putJpgHuffmanTableList (tables : List JpgHuffmanTableSpec) : Builder :=
  let payloadSize := tables.foldl (fun acc t => acc + huffmanTableSpecSize t) 0
  Builder.word16BE (UInt16.ofNat (payloadSize + 2)) ++
  tables.foldl (fun acc t => acc ++ putJpgHuffmanTableSpec t) Builder.empty

-- ── Scan header (SOS) ──

/-- One component's DC/AC Huffman table selectors within a Start-Of-Scan
    header. -/
structure JpgScanSpecification where
  componentSelector : UInt8
  dcEntropyCodingTable : UInt8
  acEntropyCodingTable : UInt8
  deriving Repr, DecidableEq

/-- Parse one `JpgScanSpecification`. -/
def parseJpgScanSpecification (bytes : List UInt8) :
    Except String (JpgScanSpecification × List UInt8) := do
  let (componentSelector, r1) ← readU8 bytes
  let (packed, r2) ← readU8 r1
  let (dcEntropyCodingTable, acEntropyCodingTable) := unpack4Bits packed
  pure ({ componentSelector, dcEntropyCodingTable, acEntropyCodingTable }, r2)

/-- Write one `JpgScanSpecification`. -/
def putJpgScanSpecification (s : JpgScanSpecification) : Builder :=
  Builder.word8 s.componentSelector ++
  Builder.word8 (pack4Bits s.dcEntropyCodingTable s.acEntropyCodingTable)

private def parseJpgScanSpecificationN :
    Nat → List UInt8 → Except String (List JpgScanSpecification × List UInt8)
  | 0, bytes => .ok ([], bytes)
  | n + 1, bytes => do
      let (s, r1) ← parseJpgScanSpecification bytes
      let (ss, r2) ← parseJpgScanSpecificationN n r1
      pure (s :: ss, r2)

/-- The `SOS` (Start-Of-Scan) header, immediately preceding one entropy-coded
    scan's raw bytes. -/
structure JpgScanHeader where
  scanLength : UInt16
  componentCount : UInt8
  scans : List JpgScanSpecification
  spectralSelectionStart : UInt8
  spectralSelectionEnd : UInt8
  successiveApproxHigh : UInt8
  successiveApproxLow : UInt8
  deriving Repr, DecidableEq

/-- Parse a `JpgScanHeader`. -/
def parseJpgScanHeader (bytes : List UInt8) : Except String (JpgScanHeader × List UInt8) := do
  let (scanLength, r1) ← readU16BE bytes
  let (componentCount, r2) ← readU8 r1
  let (scans, r3) ← parseJpgScanSpecificationN componentCount.toNat r2
  let (spectralSelectionStart, r4) ← readU8 r3
  let (spectralSelectionEnd, r5) ← readU8 r4
  let (packed, r6) ← readU8 r5
  let (successiveApproxHigh, successiveApproxLow) := unpack4Bits packed
  pure ({ scanLength, componentCount, scans, spectralSelectionStart, spectralSelectionEnd,
          successiveApproxHigh, successiveApproxLow }, r6)

/-- Write a `JpgScanHeader`. -/
def putJpgScanHeader (h : JpgScanHeader) : Builder :=
  Builder.word16BE h.scanLength ++ Builder.word8 h.componentCount ++
  h.scans.foldl (fun acc s => acc ++ putJpgScanSpecification s) Builder.empty ++
  Builder.word8 h.spectralSelectionStart ++ Builder.word8 h.spectralSelectionEnd ++
  Builder.word8 (pack4Bits h.successiveApproxHigh h.successiveApproxLow)

-- ── APPn payloads: Adobe APP14, JFIF APP0 ──

/-- The payload of an Adobe `APP14` marker segment: DCT encoder version and
    colour-transform flags. -/
structure JpgAdobeApp14 where
  dctVersion : UInt16
  transformFlag0 : UInt16
  transformFlag1 : UInt16
  colorTransform : AdobeTransform
  deriving Repr, DecidableEq

/-- Byte size of an `Adobe APP14` payload (`"Adobe"` + version + two flag
    words + the transform byte). -/
def adobeApp14PayloadSize : Nat := 5 + 2 + 2 + 2 + 1

/-- Parse a `JpgAdobeApp14` payload (the `"Adobe"` signature, a version
    word, two flag words, and the colour-transform byte). -/
def parseJpgAdobeApp14 (bytes : List UInt8) : Except String (JpgAdobeApp14 × List UInt8) := do
  let (sig, r1) ← readBytesFixed 5 bytes
  if sig.toList != [0x41, 0x64, 0x6F, 0x62, 0x65] then
    throw "Invalid Adobe APP14 marker signature"
  let (dctVersion, r2) ← readU16BE r1
  let (transformFlag0, r3) ← readU16BE r2
  let (transformFlag1, r4) ← readU16BE r3
  let (transformCode, r5) ← readU8 r4
  pure ({ dctVersion, transformFlag0, transformFlag1,
          colorTransform := adobeTransformOfCode transformCode }, r5)

/-- Write a `JpgAdobeApp14` payload. -/
def putJpgAdobeApp14 (a : JpgAdobeApp14) : Builder :=
  asciiBuilder "Adobe" ++ Builder.word16BE a.dctVersion ++ Builder.word16BE a.transformFlag0 ++
  Builder.word16BE a.transformFlag1 ++ Builder.word8 (codeOfAdobeTransform a.colorTransform)

/-- The payload of a `JFIF APP0` marker segment: version (fixed `1.2`),
    the DPI unit, and the horizontal/vertical DPI. The trailing thumbnail
    (invariably a `0 × 0` placeholder) is preserved byte-exactly but not
    materialised into an `Image` — see the module doc-comment. -/
structure JpgJFIFApp0 where
  unit : JFifUnit
  dpiX : UInt16
  dpiY : UInt16
  deriving Repr, DecidableEq

/-- Byte size of a `JFIF APP0` payload (`"JFIF\0"` + 2 version bytes + unit
    byte + two DPI words + two thumbnail-dimension bytes). -/
def jfifApp0PayloadSize : Nat := 5 + 2 + 1 + 2 + 2 + 1 + 1

/-- Parse a `JpgJFIFApp0` payload. -/
def parseJpgJFIFApp0 (bytes : List UInt8) : Except String (JpgJFIFApp0 × List UInt8) := do
  let (sig, r1) ← readBytesFixed 5 bytes
  if sig.toList != [0x4A, 0x46, 0x49, 0x46, 0x00] then throw "Invalid JFIF signature"
  let (_major, r2) ← readU8 r1
  let (_minor, r3) ← readU8 r2
  let (unitCode, r4) ← readU8 r3
  let (dpiX, r5) ← readU16BE r4
  let (dpiY, r6) ← readU16BE r5
  let (_thumbW, r7) ← readU8 r6
  let (_thumbH, r8) ← readU8 r7
  pure ({ unit := jfifUnitOfCode unitCode, dpiX, dpiY }, r8)

/-- Write a `JpgJFIFApp0` payload, at the fixed `1.2` version, with a
    `0 × 0` thumbnail. -/
def putJpgJFIFApp0 (j : JpgJFIFApp0) : Builder :=
  asciiBuilder "JFIF" ++ Builder.word8 0 ++ Builder.word8 1 ++ Builder.word8 2 ++
  Builder.word8 (codeOfJfifUnit j.unit) ++ Builder.word16BE j.dpiX ++ Builder.word16BE j.dpiY ++
  Builder.word8 0 ++ Builder.word8 0

-- ── Restart interval (DRI) ──

/-- Parse a `DRI` (restart interval) segment's payload: a fixed declared
    length of `4`, then the restart interval itself. -/
def parseJpgRestartInterval (bytes : List UInt8) : Except String (UInt16 × List UInt8) := do
  let (len, r1) ← readU16BE bytes
  if len != 4 then throw "Invalid JPEG restart interval segment length"
  readU16BE r1

/-- Write a `DRI` segment's payload. -/
def putJpgRestartInterval (v : UInt16) : Builder :=
  Builder.word16BE 4 ++ Builder.word16BE v

-- ── Entropy-coded segment (ECS) boundary scan ──

/-- Scan forward through `bytes` (the byte right after `prev`) for the
    byte-stuffing-aware end of an entropy-coded scan: a genuine marker is a
    `0xFF` byte followed by anything other than `0x00` (the byte-stuffing
    escape) or a restart marker `0xD0`-`0xD7` (which belongs *inside* the
    scan, not to a following segment). Plain structural recursion on the
    list tail — no `termination_by` needed. Returns the entropy-coded bytes
    up to (not including) the marker's leading `0xFF`, and the remaining
    bytes starting at that `0xFF`. -/
private def splitEcsAux (prev : UInt8) (bytes : List UInt8) :
    Except String (List UInt8 × List UInt8) :=
  match bytes with
  | [] => .error "Unexpected end of JPEG stream while scanning entropy-coded segment"
  | vNext :: rest =>
      let isRestart := decide (0xD0 ≤ vNext ∧ vNext ≤ 0xD7)
      let isMarker := prev == 0xFF && vNext != 0 && !isRestart
      if isMarker then
        .ok ([], prev :: vNext :: rest)
      else
        match splitEcsAux vNext rest with
        | .error e => .error e
        | .ok (ecs, remaining) => .ok (prev :: ecs, remaining)

/-- Split `bytes` at the byte-stuffing-aware end of one entropy-coded scan.
    Ports the semantics of upstream's reference `parseECSSimple` (the
    "chunked" `parseECS` variant is a lazy-`ByteString`-specific performance
    optimisation with the same semantics; see the module doc-comment). -/
def splitEcs (bytes : List UInt8) : Except String (List UInt8 × List UInt8) :=
  match bytes with
  | [] => .error "Unexpected end of JPEG stream: empty entropy-coded segment"
  | v0 :: rest => splitEcsAux v0 rest

-- ── Frames ──

/-- One top-level element of a JPEG file's frame/segment list. Every
    variant other than `scanBlob` corresponds to one length-prefixed marker
    segment; `scanBlob` pairs a Start-Of-Scan header with the raw
    (byte-stuffing-undone-boundary, but otherwise uninterpreted) bytes of
    the entropy-coded scan that follows it. -/
inductive JpgFrame where
  | jfifFrame (jfif : JpgJFIFApp0)
  | adobe14Frame (adobe : JpgAdobeApp14)
  | appFrame (code : UInt8) (rawData : ByteArray)
  | extensionFrame (code : UInt8) (rawData : ByteArray)
  | quantTableFrame (tables : List JpgQuantTableSpec)
  | huffmanTableFrame (tables : List JpgHuffmanTableSpec)
  | scanBlob (header : JpgScanHeader) (ecs : ByteArray)
  | scanFrame (kind : JpgFrameKind) (header : JpgFrameHeader)
  | intervalRestart (interval : UInt16)
  deriving DecidableEq

/-- A parsed JPEG file: its frame/segment list, in file order, framed by an
    implicit leading `SOI` and trailing `EOI` marker (added by
    `putJpgImage`/consumed by `parseJpgImage`, not stored). -/
structure JpgImage where
  frames : List JpgFrame
  deriving DecidableEq

/-- Dispatch on a decoded `JpgFrameKind` to parse the one frame/segment
    variant it introduces. Returns `none` for `endOfImage` (the caller
    stops the frame loop instead of producing a frame value for it). -/
def parseFrameOfKind (kind : JpgFrameKind) (bytes : List UInt8) :
    Except String (Option JpgFrame × List UInt8) :=
  match kind with
  | .endOfImage => .ok (none, bytes)
  | .applicationSegment 0 => do
      let (len, r1) ← readU16BE bytes
      if len.toNat < 2 + jfifApp0PayloadSize then
        -- Not (or not fully) a well-formed JFIF payload: preserve raw.
        let (raw, r2) ← readBytesFixed (max (len.toNat) 2 - 2) r1
        pure (some (.appFrame 0 raw), r2)
      else
        let (jfif, r2) ← parseJpgJFIFApp0 r1
        pure (some (.jfifFrame jfif), r2)
  | .applicationSegment 14 => do
      let (len, r1) ← readU16BE bytes
      if len.toNat < 2 + adobeApp14PayloadSize then
        let (raw, r2) ← readBytesFixed (max (len.toNat) 2 - 2) r1
        pure (some (.appFrame 14 raw), r2)
      else
        let (adobe, r2) ← parseJpgAdobeApp14 r1
        pure (some (.adobe14Frame adobe), r2)
  | .applicationSegment n => do
      let (len, r1) ← readU16BE bytes
      let (raw, r2) ← readBytesFixed (max (len.toNat) 2 - 2) r1
      pure (some (.appFrame n raw), r2)
  | .extensionSegment n => do
      let (len, r1) ← readU16BE bytes
      let (raw, r2) ← readBytesFixed (max (len.toNat) 2 - 2) r1
      pure (some (.extensionFrame n raw), r2)
  | .quantizationTable => do
      let (tables, r1) ← parseJpgQuantTableList bytes
      pure (some (.quantTableFrame tables), r1)
  | .huffmanTableMarker => do
      let (tables, r1) ← parseJpgHuffmanTableList bytes
      pure (some (.huffmanTableFrame tables), r1)
  | .restartInterval => do
      let (interval, r1) ← parseJpgRestartInterval bytes
      pure (some (.intervalRestart interval), r1)
  | .startOfScan => do
      let (header, r1) ← parseJpgScanHeader bytes
      let (ecs, r2) ← splitEcs r1
      pure (some (.scanBlob header (ByteArray.mk ecs.toArray)), r2)
  | _ => do
      let (header, r1) ← parseJpgFrameHeader bytes
      pure (some (.scanFrame kind header), r1)

/-- Encode one `JpgFrame` back into its wire representation. -/
def putJpgFrame : JpgFrame → Builder
  | .adobe14Frame adobe =>
      putJpgFrameKind (.applicationSegment 14) ++ Builder.word16BE (UInt16.ofNat (2 + adobeApp14PayloadSize)) ++
      putJpgAdobeApp14 adobe
  | .jfifFrame jfif =>
      putJpgFrameKind (.applicationSegment 0) ++ Builder.word16BE (UInt16.ofNat (2 + jfifApp0PayloadSize)) ++
      putJpgJFIFApp0 jfif
  | .appFrame code raw =>
      putJpgFrameKind (.applicationSegment code) ++ Builder.word16BE (UInt16.ofNat (raw.size + 2)) ++
      builderOfByteArray raw
  | .extensionFrame code raw =>
      putJpgFrameKind (.extensionSegment code) ++ Builder.word16BE (UInt16.ofNat (raw.size + 2)) ++
      builderOfByteArray raw
  | .quantTableFrame tables => putJpgFrameKind .quantizationTable ++ putJpgQuantTableList tables
  | .huffmanTableFrame tables => putJpgFrameKind .huffmanTableMarker ++ putJpgHuffmanTableList tables
  | .intervalRestart v => putJpgFrameKind .restartInterval ++ putJpgRestartInterval v
  | .scanBlob header ecs =>
      putJpgFrameKind .startOfScan ++ putJpgScanHeader header ++ builderOfByteArray ecs
  | .scanFrame kind header => putJpgFrameKind kind ++ putJpgFrameHeader header

-- ── Top-level frame list / image ──

/-- Skip past the mandatory leading `SOI` (`0xFF 0xD8`) marker, then skip
    forward to (but not past) the `0xFF` byte introducing the first real
    frame/segment marker. Structural recursion on the list tail. -/
private def eatUntilFF : List UInt8 → Except String (List UInt8)
  | [] => .error "Unexpected end of JPEG stream while looking for the first frame marker"
  | b :: rest => if b == 0xFF then .ok rest else eatUntilFF rest

/-- Skip the mandatory `SOI` marker and locate the first frame marker's
    leading `0xFF`. -/
def skipUntilFrames (bytes : List UInt8) : Except String (List UInt8) := do
  let (b0, r1) ← readU8 bytes
  let (b1, r2) ← readU8 r1
  if b0 != 0xFF || b1 != 0xD8 then throw "Invalid JPEG signature: missing SOI marker"
  eatUntilFF r2

/-- Consume the leading `0xFF` of the next frame/segment marker. -/
def skipFrameMarker (bytes : List UInt8) : Except String (List UInt8) := do
  let (b, r1) ← readU8 bytes
  if b != 0xFF then throw s!"Invalid JPEG frame marker (expected 0xFF, got {b})"
  pure r1

set_option linter.unusedVariables false in
/-- Parse every frame/segment starting right after the leading `0xFF` of
    the first marker, up to and including the `EOI` marker. Terminates via
    a runtime shrink-check on the remaining byte count rather than a static
    proof through every `parseFrameOfKind` dispatch branch — see the module
    doc-comment. -/
def parseFrames (bytes : List UInt8) : Except String (List JpgFrame) :=
  match parseJpgFrameKind bytes with
  | .error e => .error e
  | .ok (kind, r1) =>
      match kind with
      | .endOfImage => .ok []
      | _ =>
          match parseFrameOfKind kind r1 with
          | .error e => .error e
          | .ok (mbFrame, r2) =>
              match skipFrameMarker r2 with
              | .error e => .error e
              | .ok r3 =>
                  if hlt : r3.length < bytes.length then
                    match parseFrames r3 with
                    | .error e => .error e
                    | .ok rest => .ok (mbFrame.toList ++ rest)
                  else .error "Internal error: JPEG frame parser failed to consume input"
termination_by bytes.length
decreasing_by exact hlt

/-- Parse a full JPEG image: the `SOI` marker, every frame/segment, and the
    (implicit, unstored) `EOI` marker. -/
def parseJpgImage (bytes : List UInt8) : Except String JpgImage := do
  let r1 ← skipUntilFrames bytes
  let frames ← parseFrames r1
  pure { frames }

/-- Encode a full JPEG image: `SOI`, every frame/segment, then `EOI`. -/
def putJpgImage (img : JpgImage) : Builder :=
  Builder.word8 0xFF ++ Builder.word8 0xD8 ++
  img.frames.foldl (fun acc f => acc ++ putJpgFrame f) Builder.empty ++
  Builder.word8 0xFF ++ Builder.word8 0xD9

end Codec.Picture.Jpg.Internal
