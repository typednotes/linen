import Linen.Codec.Picture.Metadata.Exif
import Linen.Data.ByteString.Builder

/-!
  Port of `Codec.Picture.Tiff.Internal.Types` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 15 of 29). TIFF's
  core structural types: the byte-order header (`TiffHeader`), the IFD
  (Image File Directory) entry structure (`ImageFileDirectory`), the tag
  vocabulary (reusing `Linen.Codec.Picture.Metadata.Exif`'s `ExifTag`/
  `ExifData`, module 5 — see below), `TiffSampleFormat`, `TiffCompression`,
  `TiffPlanarConfiguration`, `ExtraSample`, `TiffColorspace`, `Predictor`,
  and their endianness-parameterized binary serialisation.

  ## Design

  - **Endianness as a runtime value.** Every previous format module in this
    library (`Bitmap`, `Tga`, `HDR`, the PNG sub-tree) has a single, fixed
    byte order baked into its reader/writer primitives. TIFF is the first
    format where a file declares its own byte order in its first two bytes
    (`"II"` = little-endian, `"MM"` = big-endian), and every multi-byte
    field for the rest of the file must be read according to that
    declaration. This is ported as an explicit `TiffEndianness` inductive
    (`.little` / `.big`, matching upstream's `Endianness`/`EndianLittle`/
    `EndianBig`) threaded as a plain function argument through every
    reading/writing primitive below — `readU16`/`readU32`/`putU16`/
    `putU32` all take a `TiffEndianness` first, mirroring how upstream's
    `BinaryParam Endianness Word16`/`BinaryParam Endianness Word32`
    instances dispatch on it. Nothing here hardcodes a byte order.
  - Upstream's `BinaryParam a b` type class (`getP :: a -> Get b` / `putP
    :: a -> b -> Put`, "a polymorphic `get`/`put` with an extra parameter")
    becomes plain functions taking that parameter explicitly, exactly as
    `Linen.Codec.Picture.Png.Internal.Type`/`Linen.Codec.Picture.Tga`
    already turn upstream's plain `Binary` class into `parseX`/`putX`
    functions — the class buys nothing here since every call site already
    knows its concrete `Endianness` argument.
  - Decoding follows this codebase's `Except String (α × List UInt8)`
    convention over `List UInt8` (as in `Png.Internal.Type`/`HDR`), rather
    than `Std.Internal.Parsec` — TIFF's fixed-width binary records (a 2-byte
    magic, 4-byte offsets, 12-byte IFD entries) are direct list
    destructuring, the same call `Png.Internal.Type`'s module doc-comment
    already makes for PNG's length-prefixed chunk records.
  - `ExifTag`/`ExifData` are **not** redefined here: upstream's own
    `Codec.Picture.Tiff.Internal.Types` module imports
    `Codec.Picture.Metadata.Exif` for exactly this vocabulary (`ExifTag`,
    `ExifData`, `tagOfWord16`, `word16OfTag`), rather than declaring TIFF's
    own tag enumeration — `TiffTag`, mentioned in this port's task
    description, is this same `ExifTag` type, already ported (module 5,
    `Linen.Codec.Picture.Metadata.Exif`). `ExifTag.ofWord16`/`.toWord16`
    stand in for upstream's `tagOfWord16`/`word16OfTag`.
  - **Scope decision: `ImageFileDirectory.extended` is not decoded here.**
    Upstream's `BinaryParam (Endianness, Int, ImageFileDirectory) ExifData`
    instance (the `fetcher`/`align`/`immediateBytes`/`cleanImageFileDirectory`
    /`fetchExtended` machinery) resolves an already-parsed IFD entry's
    `ifdOffset` field into its actual `ExifData` payload by **seeking** —
    forward-only, but to an arbitrary absolute byte offset supplied by the
    entry itself — into the *whole TIFF file's* byte buffer, and, for a
    `TagExifOffset` entry, recursively re-parses and re-resolves an entire
    nested IFD living at that offset (an Exif sub-IFD). This needs the
    complete file buffer plus an absolute read cursor threaded through a
    mutually-recursive decoder (`getP` for one `ImageFileDirectory` calling
    `fetchExtended` over a freshly-read *list* of further
    `ImageFileDirectory`s, each of which may itself need `getP`) — a shape
    this module's per-record `List UInt8`-suffix decoders cannot express
    without first having the surrounding "whole file, absolute offset"
    framework that only exists once a top-level decode is underway. That
    framework, and this dereferencing step, belongs to module 17
    (`Codec.Picture.Tiff`, the top-level decode/encode module) — mirroring
    how `Linen.Codec.Picture.Png.Internal.Type`'s `PngRawChunk` here stays
    "parsed but undecoded" and only `Png.lean` (module 14) decodes chunk
    payloads into pixels. This module ports every *IFD-independent* piece
    faithfully: `TiffHeader`, the plain 4-field `ImageFileDirectory` record
    together with its `getP`/`putP` `Endianness` instance (tag, type,
    count, and the raw 4-byte offset-or-inline-value slot — exactly
    upstream's shape), the `[ImageFileDirectory]` list codec (count prefix
    plus trailing next-IFD offset), and every enum upstream defines
    alongside them. `ifdExtended` is retained as a field (defaulting to
    `ExifData.none` on decode, matching upstream's own `pure ExifNone`
    placeholder before `fetchExtended` runs) so the type shape module 16/17
    build on is unchanged.
  - `paddWrite`'s odd-length zero-padding (needed once `ifdExtended`
    payloads are serialised back out, e.g. an `ASCII`/`UNDEFINED` string
    whose byte count is odd) is ported as `padOddLength`, since it depends
    on nothing IFD-specific and every later module that does serialise
    extended values will need it.
  - `orderIfdByTag`/`setupIfdOffsets`/`cleanImageFileDirectory` (upstream's
    "lay out and offset a *list of IFDs plus their extended data* for
    writing" and "fix up a big-endian, count-1, short-typed entry's value
    which the format itself left-justifies in the 4-byte slot" helpers) are
    dropped for the same reason as the `fetchExtended` family above: both
    only make sense once `ifdExtended` payloads are actually being read or
    written, which is module 16/17's concern.
  - `Predictor`'s `Eq`-only upstream `deriving` clause is widened to `BEq,
    Repr` to match this codebase's default derive set (`Repr` costs
    nothing and every other enum here already carries it).
-/

namespace Codec.Picture

open Data.ByteString (Builder)

-- ── `TiffEndianness` ──

/-- TIFF's declared byte order (the first two bytes of every TIFF file):
    `"II"` for little-endian, `"MM"` for big-endian. See the module
    doc-comment for why this is threaded explicitly through every
    multi-byte reader/writer below, rather than fixed once per module as
    every earlier format in this library does. -/
inductive TiffEndianness where
  | little
  | big
  deriving BEq, Repr

-- ── Byte-level decoding primitives ──

/-- Read a single byte. -/
def readU8 (bytes : List UInt8) : Except String (UInt8 × List UInt8) :=
  match bytes with
  | b :: rest => .ok (b, rest)
  | [] => .error "Unexpected end of TIFF stream"

/-- Read `n` bytes from the front of `bytes`. -/
def readBytesFixed (n : Nat) (bytes : List UInt8) : Except String (ByteArray × List UInt8) :=
  if n ≤ bytes.length then .ok (ByteArray.mk (bytes.take n).toArray, bytes.drop n)
  else .error "Unexpected end of TIFF stream"

/-- Read a 16-bit value, honouring `endian`. -/
def readU16 (endian : TiffEndianness) (bytes : List UInt8) : Except String (UInt16 × List UInt8) := do
  let (b0, r1) ← readU8 bytes
  let (b1, r2) ← readU8 r1
  pure (match endian with
    | .little => b0.toUInt16 ||| (b1.toUInt16 <<< 8)
    | .big => (b0.toUInt16 <<< 8) ||| b1.toUInt16, r2)

/-- Read a 32-bit value, honouring `endian`. -/
def readU32 (endian : TiffEndianness) (bytes : List UInt8) : Except String (UInt32 × List UInt8) := do
  let (b0, r1) ← readU8 bytes
  let (b1, r2) ← readU8 r1
  let (b2, r3) ← readU8 r2
  let (b3, r4) ← readU8 r3
  pure (match endian with
    | .little => b0.toUInt32 ||| (b1.toUInt32 <<< 8) ||| (b2.toUInt32 <<< 16) ||| (b3.toUInt32 <<< 24)
    | .big => (b0.toUInt32 <<< 24) ||| (b1.toUInt32 <<< 16) ||| (b2.toUInt32 <<< 8) ||| b3.toUInt32, r4)

/-- Write a 16-bit value, honouring `endian`. -/
def putU16 (endian : TiffEndianness) (v : UInt16) : Builder :=
  match endian with
  | .little => Builder.word16LE v
  | .big => Builder.word16BE v

/-- Write a 32-bit value, honouring `endian`. -/
def putU32 (endian : TiffEndianness) (v : UInt32) : Builder :=
  match endian with
  | .little => Builder.word32LE v
  | .big => Builder.word32BE v

/-- Pad a byte string's serialisation with one zero byte if its length is
    odd (TIFF requires every field to start on a 2-byte boundary; upstream's
    `paddWrite`). -/
def padOddLength (b : ByteArray) : Builder :=
  (Id.run do
    let mut acc := Builder.empty
    for byte in b.toList do
      acc := acc ++ Builder.word8 byte
    pure acc) ++ (if b.size % 2 == 1 then Builder.word8 0 else Builder.empty)

-- ── `TiffHeader` ──

/-- The fixed 8-byte TIFF file header: a 2-byte byte-order magic (`"II"`/
    `"MM"`), a 2-byte constant `42`, and a 4-byte offset to the first IFD. -/
structure TiffHeader where
  endianness : TiffEndianness
  offset : UInt32
  deriving BEq, Repr

/-- Parse the byte-order magic, itself always exactly two bytes regardless
    of the endianness it declares (`"II"` = `0x4949`, `"MM"` = `0x4D4D` — a
    palindromic byte pair, so no ordering ambiguity). -/
def parseTiffEndianness (bytes : List UInt8) : Except String (TiffEndianness × List UInt8) := do
  let (b0, r1) ← readU8 bytes
  let (b1, r2) ← readU8 r1
  if b0 == 0x49 ∧ b1 == 0x49 then pure (.little, r2)
  else if b0 == 0x4D ∧ b1 == 0x4D then pure (.big, r2)
  else throw "Invalid endian tag value"

def putTiffEndianness : TiffEndianness → Builder
  | .little => Builder.word8 0x49 ++ Builder.word8 0x49
  | .big => Builder.word8 0x4D ++ Builder.word8 0x4D

/-- Parse a `TiffHeader`: the byte-order magic, the mandatory `42` magic
    number (read/written using the header's own declared endianness, as
    upstream's `putP endian (42 :: Word16)` does), and the offset to the
    first IFD. -/
def parseTiffHeader (bytes : List UInt8) : Except String (TiffHeader × List UInt8) := do
  let (endianness, r1) ← parseTiffEndianness bytes
  let (magic, r2) ← readU16 endianness r1
  if magic != 42 then throw "Invalid TIFF magic number"
  let (offset, r3) ← readU32 endianness r2
  pure ({ endianness, offset }, r3)

def putTiffHeader (h : TiffHeader) : Builder :=
  putTiffEndianness h.endianness ++ putU16 h.endianness 42 ++ putU32 h.endianness h.offset

-- ── `TiffPlanarConfiguration` ──

/-- Whether an image's samples are interleaved per pixel (`contig`, e.g.
    `RGBRGBRGB...`) or stored as separate per-component planes
    (`separate`, e.g. `RRR...GGG...BBB...`). -/
inductive TiffPlanarConfiguration where
  | contig
  | separate
  deriving BEq, Repr

/-- `0` and `1` both mean `contig` (some encoders omit the tag, whose
    default value is `1`; upstream tolerates `0` too). -/
def planarConfgOfConstant : UInt32 → Except String TiffPlanarConfiguration
  | 0 => .ok .contig
  | 1 => .ok .contig
  | 2 => .ok .separate
  | v => .error s!"Unknown planar constant ({v})"

def constantOfPlanarConfg : TiffPlanarConfiguration → UInt16
  | .contig => 1
  | .separate => 2

-- ── `TiffCompression` ──

/-- The compression schemes this library's TIFF codec recognises. -/
inductive TiffCompression where
  | none
  | modifiedRLE
  | lzw
  | jpeg
  | packBit
  deriving BEq, Repr

/-- `0` and `1` both mean uncompressed (some encoders write `0`, which is
    not a valid TIFF compression code but is tolerated). -/
def unpackCompression : UInt32 → Except String TiffCompression
  | 0 => .ok .none
  | 1 => .ok .none
  | 2 => .ok .modifiedRLE
  | 5 => .ok .lzw
  | 6 => .ok .jpeg
  | 32773 => .ok .packBit
  | v => .error s!"Unknown compression scheme {v}"

def packCompression : TiffCompression → UInt16
  | .none => 1
  | .modifiedRLE => 2
  | .lzw => 5
  | .jpeg => 6
  | .packBit => 32773

-- ── `IfdType` ──

/-- The C-style storage type of a single IFD entry's value(s), used to
    compute the on-disk byte size of one value (see `ifdTypeByteSize`) and
    hence whether an entry's 4-byte value slot holds its data inline or an
    offset to it elsewhere in the file. -/
inductive IfdType where
  | byte
  | ascii
  | short
  | long
  | rational
  | sbyte
  | undefined
  | signedShort
  | signedLong
  | signedRational
  | float
  | double
  deriving BEq, Repr

/-- Bytes occupied by a single value of this type (TIFF 6.0 §2, "Type"). -/
def ifdTypeByteSize : IfdType → Nat
  | .byte => 1
  | .ascii => 1
  | .short => 2
  | .long => 4
  | .rational => 8
  | .sbyte => 1
  | .undefined => 1
  | .signedShort => 2
  | .signedLong => 4
  | .signedRational => 8
  | .float => 4
  | .double => 8

def ifdTypeOfCode : UInt16 → Except String IfdType
  | 1 => .ok .byte
  | 2 => .ok .ascii
  | 3 => .ok .short
  | 4 => .ok .long
  | 5 => .ok .rational
  | 6 => .ok .sbyte
  | 7 => .ok .undefined
  | 8 => .ok .signedShort
  | 9 => .ok .signedLong
  | 10 => .ok .signedRational
  | 11 => .ok .float
  | 12 => .ok .double
  | _ => .error "Invalid TIF directory type"

def codeOfIfdType : IfdType → UInt16
  | .byte => 1
  | .ascii => 2
  | .short => 3
  | .long => 4
  | .rational => 5
  | .sbyte => 6
  | .undefined => 7
  | .signedShort => 8
  | .signedLong => 9
  | .signedRational => 10
  | .float => 11
  | .double => 12

/-- Parse an `IfdType`, honouring `endian`. -/
def parseIfdType (endian : TiffEndianness) (bytes : List UInt8) : Except String (IfdType × List UInt8) := do
  let (code, rest) ← readU16 endian bytes
  let ty ← ifdTypeOfCode code
  pure (ty, rest)

def putIfdType (endian : TiffEndianness) (t : IfdType) : Builder :=
  putU16 endian (codeOfIfdType t)

-- ── `ExifTag` (endianness-parameterized) ──

/-- Parse an `ExifTag`, honouring `endian` (upstream's `BinaryParam
    Endianness ExifTag`). -/
def parseExifTag (endian : TiffEndianness) (bytes : List UInt8) : Except String (ExifTag × List UInt8) := do
  let (code, rest) ← readU16 endian bytes
  pure (ExifTag.ofWord16 code, rest)

def putExifTag (endian : TiffEndianness) (t : ExifTag) : Builder :=
  putU16 endian t.toWord16

-- ── `Predictor` ──

/-- A horizontal-differencing predictor applied before compression, used to
    improve LZW/PackBits compression ratios. -/
inductive Predictor where
  | none
  | horizontalDifferencing
  deriving BEq, Repr

def predictorOfConstant : UInt32 → Except String Predictor
  | 1 => .ok .none
  | 2 => .ok .horizontalDifferencing
  | v => .error s!"Unknown predictor ({v})"

def constantOfPredictor : Predictor → UInt32
  | .none => 1
  | .horizontalDifferencing => 2

-- ── `TiffSampleFormat` ──

/-- How to interpret a sample's raw bits: unsigned integer, signed integer,
    floating point, or an encoder-specific meaning. -/
inductive TiffSampleFormat where
  | uint
  | int
  | float
  | unknown
  deriving BEq, Repr

def unpackSampleFormat : UInt32 → Except String TiffSampleFormat
  | 1 => .ok .uint
  | 2 => .ok .int
  | 3 => .ok .float
  | 4 => .ok .unknown
  | v => .error s!"Undefined data format ({v})"

def packSampleFormat : TiffSampleFormat → UInt32
  | .uint => 1
  | .int => 2
  | .float => 3
  | .unknown => 4

-- ── `TiffColorspace` ──

/-- The photometric interpretation of an image's samples (TIFF 6.0 §3). -/
inductive TiffColorspace where
  | monochromeWhite0
  | monochrome
  | rgb
  | paletted
  | transparencyMask
  | cmyk
  | ycbcr
  | cieLab
  deriving BEq, Repr

def unpackPhotometricInterpretation : UInt32 → Except String TiffColorspace
  | 0 => .ok .monochromeWhite0
  | 1 => .ok .monochrome
  | 2 => .ok .rgb
  | 3 => .ok .paletted
  | 4 => .ok .transparencyMask
  | 5 => .ok .cmyk
  | 6 => .ok .ycbcr
  | 8 => .ok .cieLab
  | v => .error s!"Unrecognized color space {v}"

def packPhotometricInterpretation : TiffColorspace → UInt16
  | .monochromeWhite0 => 0
  | .monochrome => 1
  | .rgb => 2
  | .paletted => 3
  | .transparencyMask => 4
  | .cmyk => 5
  | .ycbcr => 6
  | .cieLab => 8

-- ── `ExtraSample` ──

/-- What an image's extra (beyond the colour model's own) sample(s)
    represent, e.g. the alpha channel of an `RGBA` image. -/
inductive ExtraSample where
  | unspecified
  | associatedAlpha
  | unassociatedAlpha
  deriving BEq, Repr

def codeOfExtraSample : ExtraSample → UInt16
  | .unspecified => 0
  | .associatedAlpha => 1
  | .unassociatedAlpha => 2

/-- Upstream has no inverse of `codeOfExtraSample` (`ExtraSample`'s only use
    is as a value being written); this port adds one anyway since reading a
    file's own `ExtraSamples` tag back is a natural, faithful extension in
    the same spirit as every other `xOfCode`/`codeOfX` pair in this
    module. -/
def extraSampleOfCode : UInt16 → Except String ExtraSample
  | 0 => .ok .unspecified
  | 1 => .ok .associatedAlpha
  | 2 => .ok .unassociatedAlpha
  | v => .error s!"Unknown extra sample code ({v})"

-- ── `ImageFileDirectory` ──

/-- One IFD (Image File Directory) entry: a tag, its value's storage type,
    how many values it holds, and either the inline value or an offset to
    it elsewhere in the file (whichever `ifdCount * ifdTypeByteSize
    ifdType` selects — ≤ 4 bytes fits inline, more needs a pointer; see the
    module doc-comment for why resolving that indirection into
    `ifdExtended` is deferred to module 17). -/
structure ImageFileDirectory where
  ifdIdentifier : ExifTag
  ifdType : IfdType
  ifdCount : UInt32
  /-- The raw 4-byte value slot: either the inline value (left-justified,
      for values smaller than 4 bytes) or a file offset to the value's
      actual storage. -/
  ifdOffset : UInt32
  /-- The entry's decoded value, once resolved — `.none` until a later
      module resolves it (see the module doc-comment). -/
  ifdExtended : ExifData
  deriving BEq

/-- Parse a single IFD entry's fixed 12-byte tag/type/count/offset fields,
    honouring `endian`. `ifdExtended` is left as `ExifData.none`, matching
    upstream's own `pure ExifNone` placeholder. -/
def parseImageFileDirectory (endian : TiffEndianness) (bytes : List UInt8) :
    Except String (ImageFileDirectory × List UInt8) := do
  let (ifdIdentifier, r1) ← parseExifTag endian bytes
  let (ifdType, r2) ← parseIfdType endian r1
  let (ifdCount, r3) ← readU32 endian r2
  let (ifdOffset, r4) ← readU32 endian r3
  pure ({ ifdIdentifier, ifdType, ifdCount, ifdOffset, ifdExtended := .none }, r4)

/-- Write a single IFD entry's fixed 12-byte tag/type/count/offset fields,
    honouring `endian` (`ifdExtended` is not written here — see the module
    doc-comment). -/
def putImageFileDirectory (endian : TiffEndianness) (ifd : ImageFileDirectory) : Builder :=
  putExifTag endian ifd.ifdIdentifier ++ putIfdType endian ifd.ifdType ++
  putU32 endian ifd.ifdCount ++ putU32 endian ifd.ifdOffset

/-- Read `n` IFD entries in a row. -/
private def parseImageFileDirectoryN (endian : TiffEndianness) :
    Nat → List UInt8 → Except String (List ImageFileDirectory × List UInt8)
  | 0, bytes => .ok ([], bytes)
  | n + 1, bytes => do
      let (ifd, r1) ← parseImageFileDirectory endian bytes
      let (rest, r2) ← parseImageFileDirectoryN endian n r1
      pure (ifd :: rest, r2)

/-- Parse a full IFD: a 2-byte entry count, that many 12-byte entries, and
    the trailing 4-byte offset to the next IFD (`0` if there is none) —
    upstream's `BinaryParam Endianness [ImageFileDirectory]`. Returns the
    entry list together with the next-IFD offset and the unconsumed tail. -/
def parseImageFileDirectoryList (endian : TiffEndianness) (bytes : List UInt8) :
    Except String (List ImageFileDirectory × UInt32 × List UInt8) := do
  let (count, r1) ← readU16 endian bytes
  let (ifds, r2) ← parseImageFileDirectoryN endian count.toNat r1
  let (nextOffset, r3) ← readU32 endian r2
  pure (ifds, nextOffset, r3)

/-- Write a full IFD: entry count, entries, and the trailing next-IFD
    offset. -/
def putImageFileDirectoryList (endian : TiffEndianness) (ifds : List ImageFileDirectory)
    (nextOffset : UInt32) : Builder :=
  putU16 endian ifds.length.toUInt16 ++
  ifds.foldl (fun acc ifd => acc ++ putImageFileDirectory endian ifd) Builder.empty ++
  putU32 endian nextOffset

end Codec.Picture
