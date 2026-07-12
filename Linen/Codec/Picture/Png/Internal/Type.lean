import Linen.Codec.Picture.Types
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.Png.Internal.Type` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 11 of 29). The PNG
  chunk structure (`PngIHdr`, `PngRawChunk`, `PngRawChunk`-stream parsing),
  the fixed chunk-signature byte constants, the PNG filter/interlace/colour
  enumerations, the APNG animation-control structures, and the CRC-32
  implementation (Annex D of the PNG specification) every other chunk's
  `Binary` instance relies on.

  ## Design

  - Upstream's `Binary` type-class methods (`get :: Get a` / `put :: a ->
    Put`) become plain functions here: `parseX : List UInt8 → Except String
    (X × List UInt8)` for decoding (mirroring `Linen.Codec.Picture.HDR`'s
    hand-rolled `Except`-returning scanners over `List UInt8`, rather than
    `Std.Internal.Parsec` — PNG chunks are length-prefixed binary records,
    not something `Parsec`'s combinators buy much over direct list
    destructuring for), and `putX : X → Builder` for encoding.

  - **CRC asymmetry, preserved faithfully from upstream.** `PngIHdr`'s own
    decoder reads its trailing 4-byte CRC field and discards it without
    checking it (upstream's `_crc <- getWord32be`), whereas the generic
    `PngRawChunk` decoder used for every *other* chunk type computes the
    chunk's CRC-32 and fails if it disagrees with the on-disk value. This
    port keeps both `parsePngIHdr` (silently discarding its CRC) and
    `parseOneChunk` (verifying it) exactly as upstream does — it is not a
    bug to fix, just an upstream quirk to reproduce.

  - `PngPalette = Palette' PixelRGB8` is ported against this library's own
    `Palette := Image PixelRGB8` (`Linen.Codec.Picture.Types`, matching how
    `Linen.Codec.Picture.Bitmap`'s `paletteFromTriples` and
    `Linen.Codec.Picture.Tga`'s colour-mapped decode path already build a
    palette): `parsePalette` returns a `Palette` image of the chunk's RGB8
    triples, `pixelCount` wide and one pixel tall, rather than a generic
    `Palette'` wrapper this codebase has no other use for.

  - `parseChunks`'s "parse repeated chunks until the `IEND` terminator,
    unknown count in advance" recursion cannot be written as direct
    structural recursion on the input list, because each chunk's on-disk
    length is itself data read from that same input (unlike, say,
    `Codec.Picture.HDR`'s scanline decoders, which always consume at least
    one list element per recursive call and can pattern-match directly on
    `::`). It is instead well-founded recursion on the *length* of the
    remaining input: `parseOneChunk` always consumes at least 12 bytes (the
    4-byte length, 4-byte type, and 4-byte CRC fields, even when the
    chunk's own data length is zero) plus that chunk's data length, and
    `parseOneChunk_consumed_le` proves `0 < consumed ∧ consumed ≤
    bytes.length` for every successful parse, which is exactly the fact
    `termination_by remaining.length` / `decreasing_by` needs (via
    `List.length_drop` and `omega`) to show `remaining.drop consumed`
    strictly shrinks.

  - The APNG animation types (`APngAnimationControl`, `APngFrameDisposal`,
    `APngBlendOp`, `APngFrameControl`) are ported as plain structures/
    inductives even though nothing in this module (or, yet, this library)
    decodes an `acTL`/`fcTL` chunk into one — they are part of upstream's
    exported surface, kept for later APNG-aware modules to consume.

  - `PngChunk` and `PngLowLevel` (upstream's "already-decoded high-level
    chunk" and "decoded image plus raw chunk list" wrappers) are not ported:
    nothing in `JuicyPixels` itself constructs a `PngChunk` value (it is
    exported but unused even upstream), and `PngLowLevel` is a thin,
    never-internally-used public convenience alias over `Image` plus
    `List PngChunk` that later PNG modules do not build on.
-/

namespace Codec.Picture

open Data.ByteString (Builder)

-- ── Chunk signatures ──

/-- A 4-byte PNG chunk-type tag (e.g. `"IHDR"`, `"IDAT"`). -/
abbrev ChunkSignature := ByteArray

/-- Pack an ASCII string literal into a `ChunkSignature`/byte constant. -/
private def asciiBytes (s : String) : ByteArray :=
  ByteArray.mk (s.toList.map (fun c => c.toNat.toUInt8)).toArray

/-- Signature identifying the start of a PNG bit stream. -/
def pngSignature : ByteArray := ByteArray.mk #[137, 80, 78, 71, 13, 10, 26, 10]

/-- Signature for the header chunk of a PNG image (must be the first). -/
def iHDRSignature : ChunkSignature := asciiBytes "IHDR"

/-- Signature for a palette chunk. Must occur before `IDAT`. -/
def pLTESignature : ChunkSignature := asciiBytes "PLTE"

/-- Signature for a data chunk (holds compressed image data). -/
def iDATSignature : ChunkSignature := asciiBytes "IDAT"

/-- Signature for the last chunk of a PNG image, signalling the end. -/
def iENDSignature : ChunkSignature := asciiBytes "IEND"

/-- Signature for a transparency chunk. -/
def tRNSSignature : ChunkSignature := asciiBytes "tRNS"

/-- Signature for a gamma-correction chunk. -/
def gammaSignature : ChunkSignature := asciiBytes "gAMA"

/-- Signature for a physical-pixel-dimensions chunk. -/
def pHYsSignature : ChunkSignature := asciiBytes "pHYs"

/-- Signature for an uncompressed textual-metadata chunk. -/
def tEXtSignature : ChunkSignature := asciiBytes "tEXt"

/-- Signature for a compressed textual-metadata chunk. -/
def zTXtSignature : ChunkSignature := asciiBytes "zTXt"

/-- Signature for an APNG animation-control chunk. -/
def animationControlSignature : ChunkSignature := asciiBytes "acTL"

-- ── Byte-level decoding primitives ──

/-- Read `n` bytes from the front of `l`, returning them plus the remaining
    tail. -/
private def getBytesFixed (n : Nat) (l : List UInt8) : Except String (ByteArray × List UInt8) :=
  if n ≤ l.length then .ok (ByteArray.mk (l.take n).toArray, l.drop n)
  else .error "Unexpected end of PNG chunk stream"

private theorem getBytesFixed_ok {n : Nat} {l : List UInt8} {b : ByteArray} {r : List UInt8}
    (h : getBytesFixed n l = .ok (b, r)) : l.length = r.length + n := by
  unfold getBytesFixed at h
  split at h
  · rename_i hn
    simp only [Except.ok.injEq, Prod.mk.injEq] at h
    obtain ⟨_, h2⟩ := h
    subst h2
    rw [List.length_drop]
    omega
  · simp at h

/-- Interpret a 4-byte big-endian buffer as a `UInt32`. -/
private def beU32OfBytes (b : ByteArray) : UInt32 :=
  (b.get! 0).toUInt32 <<< 24 ||| (b.get! 1).toUInt32 <<< 16 ||| (b.get! 2).toUInt32 <<< 8 ||| (b.get! 3).toUInt32

/-- Read a single big-endian `UInt32`. -/
private def getU32BE (l : List UInt8) : Except String (UInt32 × List UInt8) :=
  match getBytesFixed 4 l with
  | .ok (b, r) => .ok (beU32OfBytes b, r)
  | .error e => .error e

private theorem getU32BE_ok {l : List UInt8} {v : UInt32} {r : List UInt8}
    (h : getU32BE l = .ok (v, r)) : l.length = r.length + 4 := by
  unfold getU32BE at h
  cases hb : getBytesFixed 4 l with
  | error e => simp [hb] at h
  | ok p =>
    obtain ⟨b, r'⟩ := p
    simp only [hb, Except.ok.injEq, Prod.mk.injEq] at h
    obtain ⟨_, h2⟩ := h
    subst h2
    exact getBytesFixed_ok hb

/-- Read a single byte. -/
private def getU8 (l : List UInt8) : Except String (UInt8 × List UInt8) :=
  match getBytesFixed 1 l with
  | .ok (b, r) => .ok (b.get! 0, r)
  | .error e => .error e

private theorem getU8_ok {l : List UInt8} {v : UInt8} {r : List UInt8}
    (h : getU8 l = .ok (v, r)) : l.length = r.length + 1 := by
  unfold getU8 at h
  cases hb : getBytesFixed 1 l with
  | error e => simp [hb] at h
  | ok p =>
    obtain ⟨b, r'⟩ := p
    simp only [hb, Except.ok.injEq, Prod.mk.injEq] at h
    obtain ⟨_, h2⟩ := h
    subst h2
    exact getBytesFixed_ok hb

-- ── CRC-32 (Annex D of the PNG specification) ──

/-- Fold the CRC-32 update step (polynomial `0xedb88320`) eight times, once
    per bit. -/
private def crcTableEntry (c : UInt32) : UInt32 :=
  (List.range 8).foldl
    (fun acc _ => if acc &&& 1 != 0 then (0xedb88320 : UInt32) ^^^ (acc >>> 1) else acc >>> 1)
    c

/-- The 256-entry CRC-32 lookup table. -/
def pngCrcTable : Array UInt32 := (Array.range 256).map (fun i => crcTableEntry i.toUInt32)

/-- Compute the CRC-32 of the concatenation of `buffers`, as described in
    Annex D of the PNG specification. -/
def pngComputeCrc (buffers : List ByteArray) : UInt32 :=
  let final : UInt32 :=
    buffers.foldl
      (fun crc buf =>
        buf.foldl
          (fun crc byte =>
            let idx := ((crc ^^^ byte.toUInt32) &&& 0xFF).toNat
            (pngCrcTable.getD idx 0) ^^^ (crc >>> 8))
          crc)
      (0xFFFFFFFF : UInt32)
  0xFFFFFFFF ^^^ final

-- ── `PngUnit` ──

/-- The physical unit a `pHYs` chunk's pixel density is expressed in. -/
inductive PngUnit where
  | unknown
  | meter
  deriving BEq, Repr

private def pngUnitOfCode (v : UInt8) : PngUnit :=
  match v with
  | 0 => .unknown
  | 1 => .meter
  | _ => .unknown

def codeOfPngUnit : PngUnit → UInt8
  | .unknown => 0
  | .meter => 1

def parsePngUnit (bytes : List UInt8) : Except String (PngUnit × List UInt8) := do
  let (v, rest) ← getU8 bytes
  pure (pngUnitOfCode v, rest)

def putPngUnit (u : PngUnit) : Builder := Builder.word8 (codeOfPngUnit u)

-- ── `PngPhysicalDimension` ──

/-- The `pHYs` chunk: pixel density along each axis, plus its unit. -/
structure PngPhysicalDimension where
  dpiX : UInt32
  dpiY : UInt32
  unit : PngUnit
  deriving BEq, Repr

def parsePngPhysicalDimension (bytes : List UInt8) : Except String (PngPhysicalDimension × List UInt8) := do
  let (dpiX, r1) ← getU32BE bytes
  let (dpiY, r2) ← getU32BE r1
  let (unit, r3) ← parsePngUnit r2
  pure ({ dpiX, dpiY, unit }, r3)

def putPngPhysicalDimension (d : PngPhysicalDimension) : Builder :=
  Builder.word32BE d.dpiX ++ Builder.word32BE d.dpiY ++ putPngUnit d.unit

-- ── `PngGamma` ──

/-- The `gAMA` chunk's decoded gamma value (upstream's on-disk encoding is
    the value times `100000`, rounded up). -/
structure PngGamma where
  value : Float
  deriving BEq, Repr

def parsePngGamma (bytes : List UInt8) : Except String (PngGamma × List UInt8) := do
  let (raw, rest) ← getU32BE bytes
  pure ({ value := raw.toNat.toFloat / 100000.0 }, rest)

def putPngGamma (g : PngGamma) : Builder :=
  Builder.word32BE ((g.value * 100000.0).ceil.toUInt32)

-- ── APNG animation types ──

/-- The `acTL` chunk: overall animation frame count and repeat count (`0`
    means "loop forever"). -/
structure APngAnimationControl where
  frameCount : UInt32
  playCount : UInt32
  deriving BEq, Repr

/-- How an APNG frame's region of the output buffer is treated before the
    next frame is rendered. -/
inductive APngFrameDisposal where
  /-- Leave the output buffer's contents as-is. Value `0`. -/
  | none
  /-- Clear the frame's region to fully transparent black. Value `1`. -/
  | background
  /-- Revert the frame's region to its previous contents. Value `2`. -/
  | previous
  deriving BEq, Repr

/-- How an APNG frame's pixels are combined with the output buffer. -/
inductive APngBlendOp where
  /-- Overwrite the output buffer. Value `0`. -/
  | source
  /-- Alpha-blend onto the output buffer. Value `1`. -/
  | over
  deriving BEq, Repr

/-- The `fcTL` chunk: one APNG frame's placement, timing, and
    disposal/blend behaviour. -/
structure APngFrameControl where
  /-- Starting from `0`. -/
  sequenceNum : UInt32
  frameWidth : UInt32
  frameHeight : UInt32
  /-- X position at which to render the frame. -/
  frameLeft : UInt32
  /-- Y position at which to render the frame. -/
  frameTop : UInt32
  delayNumerator : UInt16
  delayDenominator : UInt16
  disposal : APngFrameDisposal
  blending : APngBlendOp
  deriving BEq, Repr

-- ── `PngImageType` ──

/-- What kind of information is encoded in a PNG's `IDAT` chunk(s). -/
inductive PngImageType where
  | greyscale
  | trueColour
  | indexedColor
  | greyscaleWithAlpha
  | trueColourWithAlpha
  deriving BEq, Repr

def imageTypeOfCode : UInt8 → Except String PngImageType
  | 0 => .ok .greyscale
  | 2 => .ok .trueColour
  | 3 => .ok .indexedColor
  | 4 => .ok .greyscaleWithAlpha
  | 6 => .ok .trueColourWithAlpha
  | _ => .error "Invalid png color code"

def codeOfImageType : PngImageType → UInt8
  | .greyscale => 0
  | .trueColour => 2
  | .indexedColor => 3
  | .greyscaleWithAlpha => 4
  | .trueColourWithAlpha => 6

-- ── `PngFilter` ──

/-- The five scanline filters a PNG decoder/encoder may apply, per pixel
    row:

    ```
    +---+---+
    | c | b |
    +---+---+
    | a | x |
    +---+---+
    ```

    (`x` being the current filtered pixel). -/
inductive PngFilter where
  /-- `Filt(x) = Orig(x)`. -/
  | none
  /-- `Filt(x) = Orig(x) - Orig(a)`. -/
  | sub
  /-- `Filt(x) = Orig(x) - Orig(b)`. -/
  | up
  /-- `Filt(x) = Orig(x) - ⌊(Orig(a) + Orig(b)) / 2⌋`. -/
  | average
  /-- `Filt(x) = Orig(x) - PaethPredictor(Orig(a), Orig(b), Orig(c))`. -/
  | paeth
  deriving BEq, Repr

def pngFilterOfCode : UInt8 → Except String PngFilter
  | 0 => .ok .none
  | 1 => .ok .sub
  | 2 => .ok .up
  | 3 => .ok .average
  | 4 => .ok .paeth
  | _ => .error "Invalid scanline filter"

def codeOfPngFilter : PngFilter → UInt8
  | .none => 0
  | .sub => 1
  | .up => 2
  | .average => 3
  | .paeth => 4

-- ── `PngInterlaceMethod` ──

/-- The two known interlacing schemes for a PNG image. -/
inductive PngInterlaceMethod where
  /-- No interlacing: basic top-to-bottom, left-to-right ordering. -/
  | noInterlace
  /-- The Adam7 interleaving scheme. -/
  | interlaceAdam7
  deriving BEq, Repr

def interlaceMethodOfCode : UInt8 → Except String PngInterlaceMethod
  | 0 => .ok .noInterlace
  | 1 => .ok .interlaceAdam7
  | _ => .error "Invalid interlace method"

def codeOfInterlaceMethod : PngInterlaceMethod → UInt8
  | .noInterlace => 0
  | .interlaceAdam7 => 1

-- ── `PngIHdr` ──

/-- The generic 13-byte header describing every PNG image. -/
structure PngIHdr where
  /-- Image width in pixels. -/
  width : UInt32
  /-- Image height in pixels. -/
  height : UInt32
  /-- Number of bits per sample. -/
  bitDepth : UInt8
  /-- Kind of PNG image (greyscale, true colour, indexed, ...). -/
  colourType : PngImageType
  /-- Compression method used. -/
  compressionMethod : UInt8
  /-- Must be `0`. -/
  filterMethod : UInt8
  /-- Whether the image is interlaced (for progressive rendering). -/
  interlaceMethod : PngInterlaceMethod
  deriving BEq, Repr

/-- Parse an `IHDR` chunk. Unlike every other chunk (see `parseOneChunk`
    below), the trailing CRC field is read but **not** verified, exactly
    matching upstream's own `_crc <- getWord32be` (see the module
    doc-comment for the full explanation). -/
def parsePngIHdr (bytes : List UInt8) : Except String (PngIHdr × List UInt8) := do
  let (_size, r1) ← getU32BE bytes
  let (sig, r2) ← getBytesFixed 4 r1
  if sig != iHDRSignature then throw "Invalid PNG file, wrong ihdr"
  let (w, r3) ← getU32BE r2
  let (h, r4) ← getU32BE r3
  let (bd, r5) ← getU8 r4
  let (ctByte, r6) ← getU8 r5
  let colourType ← imageTypeOfCode ctByte
  let (cm, r7) ← getU8 r6
  let (fm, r8) ← getU8 r7
  let (imByte, r9) ← getU8 r8
  let interlaceMethod ← interlaceMethodOfCode imByte
  let (_crc, r10) ← getU32BE r9
  pure ({ width := w, height := h, bitDepth := bd, colourType, compressionMethod := cm,
          filterMethod := fm, interlaceMethod }, r10)

/-- Build a `Builder` that writes out `b`'s bytes verbatim. -/
private def builderOfByteArray (b : ByteArray) : Builder :=
  b.toList.foldl (fun acc byte => acc ++ Builder.word8 byte) Builder.empty

/-- A 32-bit value's big-endian byte encoding, as a plain list. -/
private def u32BEList (v : UInt32) : List UInt8 :=
  [(v >>> 24).toUInt8, (v >>> 16).toUInt8, (v >>> 8).toUInt8, v.toUInt8]

/-- The 13-byte `IHDR` payload (signature plus fields), used both to compute
    the chunk's CRC-32 and to serialise it. -/
private def innerIHdrBytes (h : PngIHdr) : ByteArray :=
  ByteArray.mk
    (("IHDR".toList.map (fun c => c.toNat.toUInt8) ++ u32BEList h.width ++ u32BEList h.height ++
      [h.bitDepth, codeOfImageType h.colourType, h.compressionMethod, h.filterMethod,
       codeOfInterlaceMethod h.interlaceMethod]).toArray)

def putPngIHdr (h : PngIHdr) : Builder :=
  let inner := innerIHdrBytes h
  let crc := pngComputeCrc [inner]
  Builder.word32BE 13 ++ builderOfByteArray inner ++ Builder.word32BE crc

-- ── `PngRawChunk` ──

/-- A parsed-but-undecoded PNG chunk: its declared length, 4-byte type tag,
    CRC-32, and raw payload bytes. -/
structure PngRawChunk where
  chunkLength : UInt32
  chunkType : ChunkSignature
  chunkCRC : UInt32
  chunkData : ByteArray

/-- Build a `PngRawChunk` from a signature and its payload, computing the
    payload's CRC-32. -/
def mkRawChunk (sig : ChunkSignature) (data : ByteArray) : PngRawChunk :=
  { chunkLength := UInt32.ofNat data.size, chunkType := sig, chunkCRC := pngComputeCrc [sig, data],
    chunkData := data }

def putPngRawChunk (c : PngRawChunk) : Builder :=
  Builder.word32BE c.chunkLength ++ builderOfByteArray c.chunkType ++
  (if c.chunkLength != 0 then builderOfByteArray c.chunkData else Builder.empty) ++
  Builder.word32BE c.chunkCRC

/-- Parse a single chunk from the front of `bytes`, returning it together
    with how many bytes it consumed (always `12 + chunkLength`: the 4-byte
    length, 4-byte type, and 4-byte CRC fields, plus the payload). Unlike
    `parsePngIHdr`, this verifies the trailing CRC-32 and fails if it
    disagrees with the computed one — see the module doc-comment for the
    asymmetry this faithfully reproduces from upstream. -/
def parseOneChunk (bytes : List UInt8) : Except String (PngRawChunk × Nat) :=
  match getU32BE bytes with
  | .error e => .error e
  | .ok (size, r1) =>
    match getBytesFixed 4 r1 with
    | .error e => .error e
    | .ok (sig, r2) =>
      match getBytesFixed size.toNat r2 with
      | .error e => .error e
      | .ok (dat, r3) =>
        match getU32BE r3 with
        | .error e => .error e
        | .ok (crc, _r4) =>
          let computed := pngComputeCrc [sig, dat]
          if computed ^^^ crc != 0 then
            .error s!"Invalid CRC : {computed}, {crc}"
          else
            .ok ({ chunkLength := size, chunkType := sig, chunkCRC := crc, chunkData := dat }, 12 + size.toNat)

/-- Every successful `parseOneChunk` consumes a positive number of bytes, no
    more than were available — the fact `parseChunks`'s well-founded
    recursion needs. -/
theorem parseOneChunk_consumed_le {bytes : List UInt8} {chunk : PngRawChunk} {consumed : Nat}
    (h : parseOneChunk bytes = .ok (chunk, consumed)) : 0 < consumed ∧ consumed ≤ bytes.length := by
  unfold parseOneChunk at h
  cases h1 : getU32BE bytes with
  | error e => simp [h1] at h
  | ok p1 =>
    obtain ⟨size, r1⟩ := p1
    cases h2 : getBytesFixed 4 r1 with
    | error e => simp [h1, h2] at h
    | ok p2 =>
      obtain ⟨sig, r2⟩ := p2
      cases h3 : getBytesFixed size.toNat r2 with
      | error e => simp [h1, h2, h3] at h
      | ok p3 =>
        obtain ⟨dat, r3⟩ := p3
        cases h4 : getU32BE r3 with
        | error e => simp [h1, h2, h3, h4] at h
        | ok p4 =>
          obtain ⟨crc, r4⟩ := p4
          simp only [h1, h2, h3, h4] at h
          split at h
          · simp at h
          · simp only [Except.ok.injEq, Prod.mk.injEq] at h
            obtain ⟨hc1, hc2⟩ := h
            subst hc1
            have e1 := getU32BE_ok h1
            have e2 := getBytesFixed_ok h2
            have e3 := getBytesFixed_ok h3
            have e4 := getU32BE_ok h4
            omega

/-- Parse a stream of chunks until (and including) the `IEND` terminator.
    The recursion is well-founded on the remaining input's length: each
    call to `parseOneChunk` consumes at least 12 bytes (see
    `parseOneChunk_consumed_le`), so `remaining.drop consumed` is always
    strictly shorter than `remaining` whenever a chunk was actually
    parsed. -/
def parseChunks (remaining : List UInt8) : Except String (List PngRawChunk) :=
  match h : parseOneChunk remaining with
  | .error e => let _ := h; .error e
  | .ok (chunk, consumed) =>
      if chunk.chunkType == iENDSignature then
        .ok [chunk]
      else
        match parseChunks (remaining.drop consumed) with
        | .error e => .error e
        | .ok rest => .ok (chunk :: rest)
termination_by remaining.length
decreasing_by
  have hc := parseOneChunk_consumed_le h
  simp only [List.length_drop]
  omega

-- ── `PngRawImage` ──

/-- A raw, fully-parsed PNG image: its `IHDR` header plus every chunk in
    file order (including the trailing `IEND`). -/
structure PngRawImage where
  header : PngIHdr
  chunks : List PngRawChunk

/-- Parse the 8-byte PNG signature, the `IHDR` header, and every following
    chunk. -/
def parseRawPngImage (bytes : List UInt8) : Except String PngRawImage := do
  let (sig, r1) ← getBytesFixed 8 bytes
  if sig != pngSignature then throw "Invalid PNG file, signature broken"
  let (ihdr, r2) ← parsePngIHdr r1
  let chunkList ← parseChunks r2
  pure { header := ihdr, chunks := chunkList }

def putPngRawImage (img : PngRawImage) : Builder :=
  builderOfByteArray pngSignature ++ putPngIHdr img.header ++
  img.chunks.foldl (fun acc c => acc ++ putPngRawChunk c) Builder.empty

-- ── Palette ──

/-- Parse a `PLTE` chunk into this library's `Palette` image (an `Image
    PixelRGB8`, `pixelCount` wide and one pixel tall — see the module
    doc-comment for why this substitutes upstream's `Palette' PixelRGB8`). -/
def parsePalette (plte : PngRawChunk) : Except String Palette :=
  if plte.chunkData.size % 3 != 0 then .error "Invalid palette size"
  else
    let pixelCount := plte.chunkData.size / 3
    let triples : Array (UInt8 × UInt8 × UInt8) :=
      (Array.range pixelCount).map (fun i =>
        (plte.chunkData.get! (3 * i), plte.chunkData.get! (3 * i + 1), plte.chunkData.get! (3 * i + 2)))
    .ok (generateImage
      (fun x _ => let (r, g, b) := triples.getD x (0, 0, 0); (⟨r, g, b⟩ : PixelRGB8))
      pixelCount 1)

-- ── Chunk lookup ──

/-- Every chunk's raw payload whose type tag matches `sig`, in file order. -/
def chunksWithSig (img : PngRawImage) (sig : ChunkSignature) : List ByteArray :=
  (img.chunks.filter (fun c => c.chunkType == sig)).map (·.chunkData)

end Codec.Picture
