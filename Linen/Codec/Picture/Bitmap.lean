import Linen.Codec.Picture.Types
import Linen.Codec.Picture.Metadata
import Linen.Codec.Picture.InternalHelper
import Linen.Data.ByteString.Builder
import Std.Internal.Parsec.ByteArray

/-!
  Port of `Codec.Picture.Bitmap` from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 8 of 29). Decoding and
  encoding of the Windows/OS2 Bitmap (`.bmp`) format, across the
  BITMAPCOREHEADER/INFOHEADER/V2/V3/V4/V5 header variants and the
  1/4/8/16/24/32-bit-per-pixel, RLE and bitfield pixel encodings.

  ## Design

  - Upstream's `decodeBitmapWithPaletteAndMetadata` returns
    `Either DynamicImage PalettedImage`, a genuine either/or between a
    true-colour image and a real `Palette'`-carrying paletted image.
    `Linen.Codec.Picture.Types`'s own `PalettedImage` was modelled (module 1)
    as "always indexed" (an `Image Pixel8` plus its palette), not as a sum
    type, and nothing else in `linen` yet consumes it, so this module
    reproduces upstream's either/or shape locally with the stdlib
    `Sum DynamicImage PalettedImage` rather than changing that structure.

  - Upstream threads an `ST s`/`MVector` scratch buffer through row-by-row
    writers for every decode format. The three "direct" pixel encodings
    (32/24/16-bit RGB(A), 8-bit grayscale, 1/4-bit indexed) have a byte
    offset that depends only on the target pixel's `(x, y)` coordinate, so
    they are ported as pure functions via `generateImage` instead — no
    mutable state needed. The Microsoft RLE encoding (`decodeImageY8RLE`) is
    genuinely sequential (a run-length stream with delta/end-of-line escape
    codes); it is restructured around `List.take`/`List.drop` (already
    known-terminating stdlib functions) instead of porting upstream's
    `writeN`/`copyN` mutual recursion directly, which would otherwise need a
    hand-rolled combined well-founded measure. The one non-obviously
    structural step — slicing off a literal run's bytes via `List.drop` —
    goes through the existing `@[simp]` lemma `List.length_drop`.

  - Bitfields (`Bitfield`/`Bitfields3`/`Bitfields4`) are upstream generic
    over `FiniteBits t` to share code between the 16- and 32-bit formats;
    since upstream's own 16-bit path pre-masks every field mask with
    `0xFFFF` before use, both widths are representationally interchangeable
    once masked, so this port unifies both to plain `UInt32`-based
    structures.

  - Signed 32-bit fields (`width`/`height`/`xResolution`/`yResolution` of
    `BmpV5Header`) are ported as `Int` (Lean has no fixed-width signed
    integer type), read/written via the two's-complement helpers
    `uint32ToInt32`/`int32ToUInt32`.

  - `BmpEncodable`'s upstream methods (`bitsPerPixel`, `hasAlpha`,
    `defaultPalette`) all take an ignored `pixel` witness purely to select
    the instance; since Lean's typeclass resolution can select an instance
    from the pixel type alone, they are plain class fields here instead of
    functions taking an unused argument.

  - `writeBitmap`/`writeDynamicBitmap` (trivial `IO` file-writers) are
    dropped, matching this library's convention of leaving file I/O to the
    caller.

  - A `BmpPaletteEntry` (on-disk order: blue, green, red, alpha) replaces
    upstream's `BmpPalette` newtype around a plain list of such tuples —
    this is the format `putPalette` writes and the encoder's
    `defaultPalette` supplies; it is unrelated to `Linen.Codec.Picture.Types`
    `Palette`/`PalettedImage`, which the *decoder* produces in
    already-reordered `(r, g, b)` form.

  - `getICCProfile`'s upstream extraction guard (`colorSpaceType ==
    ProfileLinked`) and `metadataOfHeader`'s upstream consumption guard
    (`colorSpaceType == ProfileEmbedded`) disagree with each other; this is
    reproduced faithfully (the ICC profile is *read* under `.profileLinked`
    but only ever *attached as metadata* under `.profileEmbedded`), since
    resolving the discrepancy either way would be guessing at upstream's
    intent rather than porting its actual behaviour.
-/

namespace Codec.Picture

open Std.Internal.Parsec
open Std.Internal.Parsec.ByteArray
open Data.ByteString (Builder)

-- ── Binary primitives ──

private def getU8 : Parser UInt8 := any

private def getU16LE : Parser UInt16 := do
  let b0 ← getU8
  let b1 ← getU8
  pure (b0.toUInt16 ||| (b1.toUInt16 <<< 8))

private def getU32LE : Parser UInt32 := do
  let b0 ← getU8
  let b1 ← getU8
  let b2 ← getU8
  let b3 ← getU8
  pure (b0.toUInt32 ||| (b1.toUInt32 <<< 8) ||| (b2.toUInt32 <<< 16) ||| (b3.toUInt32 <<< 24))

/-- Reinterpret a `UInt32` bit pattern as a two's-complement signed 32-bit
    value (Lean has no fixed-width signed integer type). -/
private def uint32ToInt32 (u : UInt32) : Int :=
  if u.toNat < 2147483648 then (u.toNat : Int) else (u.toNat : Int) - 4294967296

/-- The inverse of `uint32ToInt32`. -/
private def int32ToUInt32 (i : Int) : UInt32 :=
  (i % (4294967296 : Int)).toNat.toUInt32

private def getI32LE : Parser Int := uint32ToInt32 <$> getU32LE

private def getBytes (n : Nat) : Parser ByteArray := (·.toByteArray) <$> ByteArray.take n

private def getPos : Parser Nat := fun it => .success it it.pos

private def skipN (n : Nat) : Parser Unit := do
  let _ ← getBytes n
  pure ()

/-- Embed an `Except`-valued computation into the parser pipeline. -/
private def liftExcept : Except String α → Parser α
  | .ok a => pure a
  | .error e => fail e

/-- Number of trailing zero bits, linear scan (Lean's `UInt32` has no
    built-in `countTrailingZeros`). -/
private def ctz32 (u : UInt32) : Nat :=
  go 0 u
where
  go (n : Nat) (u : UInt32) : Nat :=
    if n ≥ 32 then 32
    else if u &&& 1 == 1 then n
    else go (n + 1) (u >>> 1)
  termination_by 32 - n
  decreasing_by all_goals omega

private def builderOfByteArray (b : ByteArray) : Builder :=
  b.toList.foldl (fun acc byte => acc ++ Builder.word8 byte) Builder.empty

-- ── `BmpHeader` ──

/-- Magic identifier at the start of every bitmap file, `"BM"`. -/
def bitmapMagicIdentifier : UInt16 := 0x4D42

/-- The (14-byte) header shared by every bitmap variant. -/
structure BmpHeader where
  magicIdentifier : UInt16
  fileSize : UInt32
  reserved1 : UInt16
  reserved2 : UInt16
  dataOffset : UInt32
  deriving Repr, BEq

private def sizeofBmpHeader : Nat := 14

private def getBmpHeader : Parser BmpHeader := do
  let magic ← getU16LE
  if magic != bitmapMagicIdentifier then fail "Invalid Bitmap magic identifier"
  let fileSize ← getU32LE
  let r1 ← getU16LE
  let r2 ← getU16LE
  let dataOffset ← getU32LE
  pure { magicIdentifier := magic, fileSize, reserved1 := r1, reserved2 := r2, dataOffset }

private def putBmpHeader (hdr : BmpHeader) : Builder :=
  Builder.word16LE hdr.magicIdentifier ++ Builder.word32LE hdr.fileSize ++
  Builder.word16LE hdr.reserved1 ++ Builder.word16LE hdr.reserved2 ++ Builder.word32LE hdr.dataOffset

-- ── `ColorSpaceType` ──

/-- The interpretation of a bitmap's colour-space fields. -/
inductive ColorSpaceType where
  | calibratedRGB
  | deviceDependentRGB
  | deviceDependentCMYK
  | profileEmbedded
  | profileLinked
  | sRGB
  | windowsColorSpace
  | unknownColorSpace (v : UInt32)
  deriving Repr, BEq

private def getColorSpaceType : Parser ColorSpaceType := do
  let v ← getU32LE
  pure <| match v with
    | 0 => .calibratedRGB
    | 1 => .deviceDependentRGB
    | 2 => .deviceDependentCMYK
    | 0x4D424544 => .profileEmbedded
    | 0x4C494E4B => .profileLinked
    | 0x73524742 => .sRGB
    | 0x57696E20 => .windowsColorSpace
    | v => .unknownColorSpace v

private def putColorSpaceType (c : ColorSpaceType) : Builder :=
  Builder.word32LE <| match c with
    | .calibratedRGB => 0
    | .deviceDependentRGB => 1
    | .deviceDependentCMYK => 2
    | .profileEmbedded => 0x4D424544
    | .profileLinked => 0x4C494E4B
    | .sRGB => 0x73524742
    | .windowsColorSpace => 0x57696E20
    | .unknownColorSpace v => v

-- ── `BmpV5Header` ──

private def sizeofBmpCoreHeader : UInt32 := 12
private def sizeofBmpInfoHeader : UInt32 := 40
private def sizeofBmpV2Header : UInt32 := 52
private def sizeofBmpV3Header : UInt32 := 56
private def sizeofBmpV4Header : UInt32 := 108
private def sizeofBmpV5Header : UInt32 := 124
private def sizeofColorProfile : Nat := 48

/-- The BITMAPINFOHEADER-and-beyond header, unified across the
    BITMAPCOREHEADER/INFOHEADER/V2/V3/V4/V5 variants (which fields are
    present is gated by `size`). -/
structure BmpV5Header where
  size : UInt32
  width : Int
  height : Int
  planes : UInt16
  bitPerPixel : UInt16
  bitmapCompression : UInt32
  byteImageSize : UInt32
  xResolution : Int
  yResolution : Int
  colorCount : UInt32
  importantColours : UInt32
  redMask : UInt32
  greenMask : UInt32
  blueMask : UInt32
  alphaMask : UInt32
  colorSpaceType : ColorSpaceType
  colorSpace : ByteArray
  iccIntent : UInt32
  iccProfileData : UInt32
  iccProfileSize : UInt32
  deriving BEq

private def getBmpV5Header : Parser BmpV5Header := do
  let size ← getU32LE
  if size == sizeofBmpCoreHeader then do
    let w ← getU16LE
    let h ← getU16LE
    let planes ← getU16LE
    let bpp ← getU16LE
    pure { size, width := (w.toNat : Int), height := (h.toNat : Int), planes, bitPerPixel := bpp, bitmapCompression := 0, byteImageSize := 0, xResolution := 2835, yResolution := 2835, colorCount := (1 : UInt32) <<< bpp.toUInt32, importantColours := 0, redMask := 0, greenMask := 0, blueMask := 0, alphaMask := 0, colorSpaceType := .deviceDependentRGB, colorSpace := ByteArray.mk #[], iccIntent := 0, iccProfileData := 0, iccProfileSize := 0 }
  else do
    let width ← getI32LE
    let height ← getI32LE
    let planes ← getU16LE
    let bpp ← getU16LE
    let (bitmapCompression, byteImageSize, xResolution, yResolution, colorCount, importantColours) ←
      if size > sizeofBmpCoreHeader then do
        let compression ← getU32LE
        let imgSize ← getU32LE
        let xRes ← getI32LE
        let yRes ← getI32LE
        let cCount ← getU32LE
        let iCount ← getU32LE
        pure (compression, imgSize, xRes, yRes, cCount, iCount)
      else pure (0, 0, 2835, 2835, 0, 0)
    let (redMask, greenMask, blueMask) ←
      if size > sizeofBmpInfoHeader || bitmapCompression == 3 then do
        let r ← getU32LE
        let g ← getU32LE
        let b ← getU32LE
        pure (r, g, b)
      else pure (0, 0, 0)
    let alphaMask ← if size > sizeofBmpV2Header then getU32LE else pure 0
    let (colorSpaceType, colorSpace) ←
      if size > sizeofBmpV3Header then do
        let cst ← getColorSpaceType
        let cs ← getBytes sizeofColorProfile
        pure (cst, cs)
      else pure (ColorSpaceType.deviceDependentRGB, ByteArray.mk #[])
    let (iccIntent, iccProfileData, iccProfileSize) ←
      if size > sizeofBmpV4Header then do
        let intent ← getU32LE
        let profData ← getU32LE
        let profSize ← getU32LE
        let _ ← getU32LE
        pure (intent, profData, profSize)
      else pure (0, 0, 0)
    pure { size, width, height, planes, bitPerPixel := bpp, bitmapCompression, byteImageSize, xResolution, yResolution, colorCount, importantColours, redMask, greenMask, blueMask, alphaMask, colorSpaceType, colorSpace, iccIntent, iccProfileData, iccProfileSize }

private def putBmpV5Header (hdr : BmpV5Header) : Builder :=
  Builder.word32LE hdr.size ++
  (if hdr.size == sizeofBmpCoreHeader then
    Builder.word16LE hdr.width.toNat.toUInt16 ++ Builder.word16LE hdr.height.toNat.toUInt16 ++
    Builder.word16LE hdr.planes ++ Builder.word16LE hdr.bitPerPixel
  else
    Builder.word32LE (int32ToUInt32 hdr.width) ++ Builder.word32LE (int32ToUInt32 hdr.height) ++
    Builder.word16LE hdr.planes ++ Builder.word16LE hdr.bitPerPixel) ++
  (if hdr.size > sizeofBmpCoreHeader then
    Builder.word32LE hdr.bitmapCompression ++ Builder.word32LE hdr.byteImageSize ++
    Builder.word32LE (int32ToUInt32 hdr.xResolution) ++ Builder.word32LE (int32ToUInt32 hdr.yResolution) ++
    Builder.word32LE hdr.colorCount ++ Builder.word32LE hdr.importantColours
  else Builder.empty) ++
  (if hdr.size > sizeofBmpInfoHeader || hdr.bitmapCompression == 3 then
    Builder.word32LE hdr.redMask ++ Builder.word32LE hdr.greenMask ++ Builder.word32LE hdr.blueMask
  else Builder.empty) ++
  (if hdr.size > sizeofBmpV2Header then Builder.word32LE hdr.alphaMask else Builder.empty) ++
  (if hdr.size > sizeofBmpV3Header then
    putColorSpaceType hdr.colorSpaceType ++ builderOfByteArray hdr.colorSpace
  else Builder.empty) ++
  (if hdr.size > sizeofBmpV4Header then
    Builder.word32LE hdr.iccIntent ++ Builder.word32LE hdr.iccProfileData ++
    Builder.word32LE hdr.iccProfileSize ++ Builder.word32LE 0
  else Builder.empty)

-- ── Palette ──

/-- A single BMP palette entry, in on-disk byte order: blue, green, red,
    alpha. -/
abbrev BmpPaletteEntry := UInt8 × UInt8 × UInt8 × UInt8

private def putPalette (p : List BmpPaletteEntry) : Builder :=
  p.foldl (fun acc (b, g, r, a) => acc ++ Builder.word8 b ++ Builder.word8 g ++ Builder.word8 r ++ Builder.word8 a)
    Builder.empty

/-- Read a single 3-byte (B, G, R) palette entry, returning it reordered as
    `(r, g, b)`. -/
private def pixel3Get : Parser (UInt8 × UInt8 × UInt8) := do
  let b ← getU8
  let g ← getU8
  let r ← getU8
  pure (r, g, b)

/-- Read a single 4-byte (B, G, R, A) palette entry, discarding the alpha,
    returning it reordered as `(r, g, b)`. -/
private def pixel4Get : Parser (UInt8 × UInt8 × UInt8) := do
  let b ← getU8
  let g ← getU8
  let r ← getU8
  let _ ← getU8
  pure (r, g, b)

/-- Build an `Image PixelRGB8` palette from parsed `(r, g, b)` triples. -/
private def paletteFromTriples (triples : Array (UInt8 × UInt8 × UInt8)) : Palette :=
  generateImage (fun x _ => let (r, g, b) := triples.getD x (0, 0, 0); (⟨r, g, b⟩ : PixelRGB8))
    triples.size 1

-- ── Sizing helpers ──

/-- Padding bytes needed at the end of every image row so it ends on a
    4-byte boundary. -/
def linePadding (bpp imgWidth : Nat) : Nat :=
  let bytesPerLine := (bpp * imgWidth + 7) / 8
  (4 - bytesPerLine % 4) % 4

/-- Total size, in bytes, of the raw pixel data. -/
def sizeofPixelData (bpp lineWidth nLines : Nat) : Nat :=
  ((bpp * lineWidth + 31) / 32) * 4 * nLines

-- ── `BmpEncodable` ──

private def stridePaddingBuilder (n : Nat) : Builder :=
  (List.range n).foldl (fun acc _ => acc ++ Builder.word8 0) Builder.empty

/-- A pixel type that can be encoded into a bitmap's raw pixel data. -/
class BmpEncodable (pixel : Type) [Pixel pixel Pixel8] where
  /-- Bits used to encode a single pixel. -/
  bitsPerPixel : Nat
  /-- Whether this format carries a usable alpha channel. -/
  hasAlphaFlag : Bool
  /-- Palette written when no explicit palette is supplied. -/
  defaultPalette : List BmpPaletteEntry := []
  /-- Encode an image's raw pixel data, bottom row first (matching the BMP
      on-disk row order). -/
  bmpEncode : Image pixel → Builder

instance : BmpEncodable Pixel8 where
  bitsPerPixel := 8
  hasAlphaFlag := false
  defaultPalette := (List.range 256).map fun x => (x.toUInt8, x.toUInt8, x.toUInt8, (255 : UInt8))
  bmpEncode img :=
    let stride := linePadding 8 img.width
    (List.range img.height).foldl (fun acc l =>
      let line := img.height - 1 - l
      let row := (List.range img.width).foldl (fun b x => b ++ Builder.word8 (img.getPixel x line))
        Builder.empty
      acc ++ row ++ stridePaddingBuilder stride) Builder.empty

instance : BmpEncodable PixelRGBA8 where
  bitsPerPixel := 32
  hasAlphaFlag := true
  bmpEncode img :=
    (List.range img.height).foldl (fun acc l =>
      let line := img.height - 1 - l
      let row := (List.range img.width).foldl (fun b x =>
        let p := img.getPixel x line
        b ++ Builder.word8 p.b ++ Builder.word8 p.g ++ Builder.word8 p.r ++ Builder.word8 p.a)
        Builder.empty
      acc ++ row) Builder.empty

instance : BmpEncodable PixelRGB8 where
  bitsPerPixel := 24
  hasAlphaFlag := false
  bmpEncode img :=
    let stride := linePadding 24 img.width
    (List.range img.height).foldl (fun acc l =>
      let line := img.height - 1 - l
      let row := (List.range img.width).foldl (fun b x =>
        let p := img.getPixel x line
        b ++ Builder.word8 p.b ++ Builder.word8 p.g ++ Builder.word8 p.r)
        Builder.empty
      acc ++ row ++ stridePaddingBuilder stride) Builder.empty

-- ── Bitfields ──

/-- A single colour-channel bitfield: a mask into a pixel word, plus the
    right-shift and scale needed to normalise it to a full 8-bit value. -/
structure Bitfield where
  mask : UInt32
  shift : Nat
  scale : Float
  deriving Repr

def makeBitfield (mask : UInt32) : Bitfield :=
  let shift := ctz32 mask
  let maxVal := mask >>> shift.toUInt32
  { mask, shift, scale := if maxVal == 0 then 1 else 255.0 / maxVal.toNat.toFloat }

def extractBitfield (bf : Bitfield) (word : UInt32) : UInt8 :=
  let field := (word &&& bf.mask) >>> bf.shift.toUInt32
  if bf.scale == 1 then field.toUInt8 else (bf.scale * field.toNat.toFloat).round.toUInt64.toUInt8

/-- Three-channel (R, G, B) bitfields. -/
structure Bitfields3 where
  red : Bitfield
  green : Bitfield
  blue : Bitfield
  deriving Repr

/-- Four-channel (R, G, B, A) bitfields. -/
structure Bitfields4 where
  red : Bitfield
  green : Bitfield
  blue : Bitfield
  alpha : Bitfield
  deriving Repr

def defaultBitfieldsRGB32 : Bitfields3 :=
  { red := makeBitfield 0x00FF0000, green := makeBitfield 0x0000FF00, blue := makeBitfield 0x000000FF }

def defaultBitfieldsRGB16 : Bitfields3 :=
  { red := makeBitfield 0x7C00, green := makeBitfield 0x03E0, blue := makeBitfield 0x001F }

def getBitfield (mask : UInt32) : Except String Bitfield :=
  if mask == 0 then .error "Bitmap decoding error - bitfield mask cannot be 0"
  else .ok (makeBitfield mask)

/-- The bitfield-driven RGBA pixel encodings this decoder supports. -/
inductive RGBABmpFormat where
  | rgba32 (bf : Bitfields4)
  | rgba16 (bf : Bitfields4)

/-- The bitfield-driven and fixed-layout RGB pixel encodings this decoder
    supports. -/
inductive RGBBmpFormat where
  | rgb32 (bf : Bitfields3)
  | rgb24
  | rgb16 (bf : Bitfields3)

/-- The bit-depth-only pixel encodings for indexed images. -/
inductive IndexedBmpFormat where
  | oneBpp
  | fourBpp
  | eightBpp

-- ── Direct (non-RLE) pixel decoding ──

private def flippedRow (height : Int) (y : Nat) : Nat :=
  if height > 0 then height.toNat - 1 - y else y

def decodeImageRGBA8 (fmt : RGBABmpFormat) (wi : Nat) (height : Int) (str : ByteArray) : Image PixelRGBA8 :=
  let hi := height.natAbs
  generateImage (fun x y =>
    let row := flippedRow height y
    match fmt with
    | .rgba32 bf =>
        let base := (row * wi + x) * 4
        let word := (str.get! base).toUInt32 ||| (str.get! (base+1)).toUInt32 <<< 8 |||
          (str.get! (base+2)).toUInt32 <<< 16 ||| (str.get! (base+3)).toUInt32 <<< 24
        (⟨extractBitfield bf.red word, extractBitfield bf.green word, extractBitfield bf.blue word,
          extractBitfield bf.alpha word⟩ : PixelRGBA8)
    | .rgba16 bf =>
        let base := (row * wi + x) * 2
        let word := (str.get! base).toUInt32 ||| (str.get! (base+1)).toUInt32 <<< 8
        (⟨extractBitfield bf.red word, extractBitfield bf.green word, extractBitfield bf.blue word,
          extractBitfield bf.alpha word⟩ : PixelRGBA8))
    wi hi

def decodeImageRGB8 (fmt : RGBBmpFormat) (wi : Nat) (height : Int) (str : ByteArray) : Image PixelRGB8 :=
  let hi := height.natAbs
  generateImage (fun x y =>
    let row := flippedRow height y
    match fmt with
    | .rgb32 bf =>
        let base := (row * wi + x) * 4
        let word := (str.get! base).toUInt32 ||| (str.get! (base+1)).toUInt32 <<< 8 |||
          (str.get! (base+2)).toUInt32 <<< 16 ||| (str.get! (base+3)).toUInt32 <<< 24
        (⟨extractBitfield bf.red word, extractBitfield bf.green word, extractBitfield bf.blue word⟩ : PixelRGB8)
    | .rgb24 =>
        let stride := wi * 3 + linePadding 24 wi
        let base := row * stride + x * 3
        (⟨str.get! (base+2), str.get! (base+1), str.get! base⟩ : PixelRGB8)
    | .rgb16 bf =>
        let base := (row * wi + x) * 2
        let word := (str.get! base).toUInt32 ||| (str.get! (base+1)).toUInt32 <<< 8
        (⟨extractBitfield bf.red word, extractBitfield bf.green word, extractBitfield bf.blue word⟩ : PixelRGB8))
    wi hi

def decodeImageY8 (fmt : IndexedBmpFormat) (wi : Nat) (height : Int) (str : ByteArray) : Image Pixel8 :=
  let hi := height.natAbs
  generateImage (fun x y =>
    let row := flippedRow height y
    match fmt with
    | .eightBpp =>
        let stride := wi + linePadding 8 wi
        str.get! (row * stride + x)
    | .fourBpp =>
        let stride := (wi + 1) / 2 + linePadding 4 wi
        let byte := str.get! (row * stride + x / 2)
        if x % 2 == 0 then byte >>> 4 else byte &&& 0x0F
    | .oneBpp =>
        let stride := (wi + 7) / 8 + linePadding 1 wi
        let byte := str.get! (row * stride + x / 8)
        (byte >>> (7 - (x % 8)).toUInt8) &&& 1)
    wi hi

-- ── RLE (Microsoft Run-Length Encoding) decoding ──

private def bmpRLEWriteByte (wi xMax : Nat) (b : UInt8) (pos : Int × Nat) (arr : Array UInt8) :
    (Int × Nat) × Array UInt8 :=
  let (y, x) := pos
  let idx : Int := y * (wi : Int) + (x : Int)
  let arr' := if 0 ≤ idx ∧ idx.toNat < arr.size then arr.set! idx.toNat b else arr
  ((y, min (x + 1) xMax), arr')

private def bmpRLEWriteRepeated (wi xMax : Nat) (is4bpp : Bool) :
    Nat → UInt8 → (Int × Nat) → Array UInt8 → (Int × Nat) × Array UInt8
  | 0, _, pos, arr => (pos, arr)
  | n + 1, b, pos, arr =>
    if is4bpp && n + 1 > 1 then
      let (pos1, arr1) := bmpRLEWriteByte wi xMax (b >>> 4) pos arr
      let (pos2, arr2) := bmpRLEWriteByte wi xMax (b &&& 0x0F) pos1 arr1
      bmpRLEWriteRepeated wi xMax is4bpp (n - 1) b pos2 arr2
    else
      let (pos1, arr1) := bmpRLEWriteByte wi xMax (if is4bpp then b >>> 4 else b) pos arr
      bmpRLEWriteRepeated wi xMax is4bpp n b pos1 arr1
termination_by n _ _ _ => n
decreasing_by all_goals omega

private def bmpRLEWriteLiteral (wi xMax : Nat) (is4bpp : Bool) :
    List UInt8 → Nat → (Int × Nat) → Array UInt8 → (Int × Nat) × Array UInt8
  | [], _, pos, arr => (pos, arr)
  | _, 0, pos, arr => (pos, arr)
  | b :: rest, n + 1, pos, arr =>
    if is4bpp && n + 1 > 1 then
      let (pos1, arr1) := bmpRLEWriteByte wi xMax (b >>> 4) pos arr
      let (pos2, arr2) := bmpRLEWriteByte wi xMax (b &&& 0x0F) pos1 arr1
      bmpRLEWriteLiteral wi xMax is4bpp rest (n - 1) pos2 arr2
    else
      let (pos1, arr1) := bmpRLEWriteByte wi xMax (if is4bpp then b >>> 4 else b) pos arr
      bmpRLEWriteLiteral wi xMax is4bpp rest n pos1 arr1

/-- The core Microsoft RLE decode loop. `tag :: n :: rest` dispatches: `tag =
    0` is an escape code (end-of-line, end-of-bitmap, delta-move, or a
    literal run of `n` pixels); any other `tag` is a run of `n` copies of a
    single following byte. A malformed/truncated stream (fewer than 2 bytes
    remaining) simply stops, matching upstream's catch-all `inner _ _ =
    return ()`. -/
private def bmpRLEInner (wi xMax : Nat) (is4bpp : Bool) :
    List UInt8 → (Int × Nat) → Array UInt8 → Array UInt8
  | [], _, arr => arr
  | [_], _, arr => arr
  | tag :: n :: rest, pos, arr =>
    if tag == 0 then
      if n == 0 then
        let (y, _) := pos
        bmpRLEInner wi xMax is4bpp rest (y - (wi : Int), 0) arr
      else if n == 1 then
        arr
      else if n == 2 then
        match rest with
        | h :: v :: rest' =>
          let (y, _) := pos
          bmpRLEInner wi xMax is4bpp rest' (y - (wi : Int) * (v.toNat : Int), h.toNat) arr
        | _ => arr
      else
        let cnt := n.toNat
        let isPadded := if is4bpp then (cnt + 3) &&& 0x3 < 2 else cnt % 2 == 1
        let bytesNeeded := if is4bpp then (cnt + 1) / 2 else cnt
        let takeCount := bytesNeeded + (if isPadded then 1 else 0)
        let literalBytes := rest.take bytesNeeded
        let (pos', arr') := bmpRLEWriteLiteral wi xMax is4bpp literalBytes cnt pos arr
        bmpRLEInner wi xMax is4bpp (rest.drop takeCount) pos' arr'
    else
      let (pos', arr') := bmpRLEWriteRepeated wi xMax is4bpp tag.toNat n pos arr
      bmpRLEInner wi xMax is4bpp rest pos' arr'
termination_by bytes _ _ => bytes.length
decreasing_by
  all_goals simp_all [List.length_drop]
  all_goals omega

/-- Decode a Microsoft-RLE-compressed 4-bpp or 8-bpp bitmap. -/
def decodeImageY8RLE (is4bpp : Bool) (hdr : BmpV5Header) (str : ByteArray) : Image Pixel8 :=
  let wi := hdr.width.toNat
  let hi := hdr.height.natAbs
  let sz := min hdr.byteImageSize.toNat str.size
  let bytes := (str.extract 0 sz).toList
  let xMax := if wi = 0 then 0 else wi - 1
  let finalArr := bmpRLEInner wi xMax is4bpp bytes (((hi : Int) - 1) * (wi : Int), 0)
    (Array.replicate (wi * hi) (0 : UInt8))
  { width := wi, height := hi, data := finalArr }

-- ── Metadata ──

/-- Build the metadata associated with a decoded header (resolution, and a
    colour-space entry when one applies). -/
def metadataOfHeader (hdr : BmpV5Header) (iccProfile : Option ByteArray) : Metadatas :=
  let dpiX := dotsPerMeterToDotPerInch (int32ToUInt32 hdr.xResolution).toNat
  let dpiY := dotsPerMeterToDotPerInch (int32ToUInt32 hdr.yResolution).toNat
  let base := simpleMetadata .bitmap hdr.width.toNat hdr.height.natAbs dpiX dpiY
  let csMeta : Metadatas :=
    match hdr.colorSpaceType with
    | .calibratedRGB => Metadatas.singleton .colorSpace (.windowsBitmapColorSpace hdr.colorSpace)
    | .sRGB => Metadatas.singleton .colorSpace .sRGB
    | .profileEmbedded =>
      match iccProfile with
      | some profile => Metadatas.singleton .colorSpace (.iccProfile profile)
      | none => Metadatas.empty
    | _ => Metadatas.empty
  base.union csMeta

-- ── Decoding pipeline ──

private def getICCProfileP (hdr : BmpV5Header) : Parser (Option ByteArray) := do
  if hdr.size >= sizeofBmpV5Header && hdr.colorSpaceType == .profileLinked &&
      hdr.iccProfileData > 0 && hdr.iccProfileSize > 0 then do
    let readSoFar ← getPos
    let target := hdr.iccProfileData.toNat
    if target > readSoFar then skipN (target - readSoFar) else pure ()
    let profile ← getBytes hdr.iccProfileSize.toNat
    pure (some profile)
  else pure none

/-- Parse a `BmpHeader` + `BmpV5Header`, without decoding pixel data. -/
def decodeBitmapWithHeaders : Parser (BmpHeader × BmpV5Header) := do
  let hdr ← getBmpHeader
  let info ← getBmpV5Header
  pure (hdr, info)

private def decodePaletteP (n : Nat) (headerSize : UInt32) : Parser (Array (UInt8 × UInt8 × UInt8)) := do
  let entry : Parser (UInt8 × UInt8 × UInt8) := if headerSize == sizeofBmpCoreHeader then pixel3Get else pixel4Get
  let rec go (i : Nat) (acc : Array (UInt8 × UInt8 × UInt8)) : Parser (Array (UInt8 × UInt8 × UInt8)) := do
    if i ≥ n then pure acc
    else do
      let e ← entry
      go (i + 1) (acc.push e)
  go 0 #[]

/-- Decode the header, palette and pixel data of a bitmap, along with any
    embedded ICC colour profile. Metadata construction happens outside the
    `Parser` monad (in `decodeBitmapWithPaletteAndMetadata`), since
    `Metadatas` (via its dependently-typed `Elem`) lives a universe above
    what `Parser`'s payload type admits. -/
private def decodeBitmapBodyP :
    Parser (Sum DynamicImage PalettedImage × BmpV5Header × Option ByteArray) := do
  let (hdr, info) ← decodeBitmapWithHeaders
  let bytesRead ← getPos
  if bytesRead > hdr.dataOffset.toNat then fail "Invalid bitmap data offset"
  if info.width ≤ 0 then fail "Invalid bitmap width"
  if info.height == 0 then fail "Invalid bitmap height"
  let wi := info.width.toNat
  let hi := info.height.natAbs
  let img ←
    match info.bitPerPixel, info.planes, info.bitmapCompression with
    | 32, 1, 0 => do
        skipN (hdr.dataOffset.toNat - bytesRead)
        let str ← getBytes (sizeofPixelData 32 wi hi)
        pure (.inl (.rgb8 (decodeImageRGB8 (.rgb32 defaultBitfieldsRGB32) wi info.height str)))
    | 32, 1, 3 => do
        let redBf ← liftExcept (getBitfield info.redMask)
        let greenBf ← liftExcept (getBitfield info.greenMask)
        let blueBf ← liftExcept (getBitfield info.blueMask)
        skipN (hdr.dataOffset.toNat - bytesRead)
        let str ← getBytes (sizeofPixelData 32 wi hi)
        if info.alphaMask != 0 then do
          let alphaBf ← liftExcept (getBitfield info.alphaMask)
          pure (.inl (.rgba8 (decodeImageRGBA8 (.rgba32 ⟨redBf, greenBf, blueBf, alphaBf⟩) wi info.height str)))
        else
          pure (.inl (.rgb8 (decodeImageRGB8 (.rgb32 ⟨redBf, greenBf, blueBf⟩) wi info.height str)))
    | 24, 1, 0 => do
        skipN (hdr.dataOffset.toNat - bytesRead)
        let str ← getBytes (sizeofPixelData 24 wi hi)
        pure (.inl (.rgb8 (decodeImageRGB8 .rgb24 wi info.height str)))
    | 16, 1, 0 => do
        skipN (hdr.dataOffset.toNat - bytesRead)
        let str ← getBytes (sizeofPixelData 16 wi hi)
        pure (.inl (.rgb8 (decodeImageRGB8 (.rgb16 defaultBitfieldsRGB16) wi info.height str)))
    | 16, 1, 3 => do
        let redBf ← liftExcept (getBitfield (info.redMask &&& 0xFFFF))
        let greenBf ← liftExcept (getBitfield (info.greenMask &&& 0xFFFF))
        let blueBf ← liftExcept (getBitfield (info.blueMask &&& 0xFFFF))
        skipN (hdr.dataOffset.toNat - bytesRead)
        let str ← getBytes (sizeofPixelData 16 wi hi)
        if info.alphaMask != 0 then do
          let alphaBf ← liftExcept (getBitfield (info.alphaMask &&& 0xFFFF))
          pure (.inl (.rgba8 (decodeImageRGBA8 (.rgba16 ⟨redBf, greenBf, blueBf, alphaBf⟩) wi info.height str)))
        else
          pure (.inl (.rgb8 (decodeImageRGB8 (.rgb16 ⟨redBf, greenBf, blueBf⟩) wi info.height str)))
    | bpp, 1, compression => do
        if bpp != 1 && bpp != 4 && bpp != 8 then fail "Unsupported bitmap bit depth"
        let colorCount := if info.colorCount == 0 then (1 : Nat) <<< bpp.toNat else info.colorCount.toNat
        let triples ← decodePaletteP colorCount info.size
        let palette := paletteFromTriples triples
        skipN (hdr.dataOffset.toNat - (bytesRead + (if info.size == sizeofBmpCoreHeader then 3 else 4) * colorCount))
        let indexedImage ←
          match bpp, compression with
          | 1, 0 => do let str ← getBytes (sizeofPixelData 1 wi hi); pure (decodeImageY8 .oneBpp wi info.height str)
          | 4, 0 => do let str ← getBytes (sizeofPixelData 4 wi hi); pure (decodeImageY8 .fourBpp wi info.height str)
          | 8, 0 => do let str ← getBytes (sizeofPixelData 8 wi hi); pure (decodeImageY8 .eightBpp wi info.height str)
          | 4, 2 => do
              let str ← if info.byteImageSize == 0 then getRemainingBytes else getBytes info.byteImageSize.toNat
              pure (decodeImageY8RLE true info str)
          | 8, 1 => do
              let str ← if info.byteImageSize == 0 then getRemainingBytes else getBytes info.byteImageSize.toNat
              pure (decodeImageY8RLE false info str)
          | _, _ => fail "Unsupported bitmap format"
        pure (.inr { indexedImage, palette, hasAlpha := false })
    | _, _, _ => fail "Unsupported bitmap format"
  let iccProfile ← getICCProfileP info
  pure (img, info, iccProfile)

/-- Decode a bitmap, returning either a true-colour `DynamicImage` or a
    paletted image, plus its metadata. -/
def decodeBitmapWithPaletteAndMetadata (input : ByteArray) :
    Except String (Sum DynamicImage PalettedImage × Metadatas) :=
  match runGetStrict decodeBitmapBodyP input with
  | .error e => .error e
  | .ok (img, hdr, iccProfile) => .ok (img, metadataOfHeader hdr iccProfile)

/-- Decode a bitmap into a true-colour `DynamicImage`, expanding any indexed
    palette. -/
def decodeBitmapWithMetadata (input : ByteArray) : Except String (DynamicImage × Metadatas) :=
  match decodeBitmapWithPaletteAndMetadata input with
  | .error e => .error e
  | .ok (.inl dyn, md) => .ok (dyn, md)
  | .ok (.inr paletted, md) => .ok (.rgb8 (palettedToTrueColor paletted), md)

/-- Decode a bitmap into a true-colour `DynamicImage`. -/
def decodeBitmap (input : ByteArray) : Except String DynamicImage :=
  match decodeBitmapWithMetadata input with
  | .error e => .error e
  | .ok (img, _) => .ok img

-- ── Encoding pipeline ──

/-- Extract `(dpiX, dpiY)` from a `Metadatas` (defaulting to `0` when
    absent), converted from dots-per-inch to dots-per-meter. -/
def extractDpiOfMetadata (metas : Metadatas) : UInt32 × UInt32 :=
  let toDpm (k : Keys Nat) : UInt32 := (dotPerInchToDotsPerMeter ((metas.lookup k).getD 0)).toUInt32
  (toDpm .dpiX, toDpm .dpiY)

/-- Encode an image into a bitmap, with an explicit palette and metadata. -/
def encodeBitmapWithPaletteAndMetadata [Pixel pixel Pixel8] [BmpEncodable pixel]
    (metas : Metadatas) (palette : List BmpPaletteEntry) (img : Image pixel) : Data.ByteString :=
  let imgWidth := img.width
  let imgHeight := img.height
  let (dpiX, dpiY) := extractDpiOfMetadata metas
  let cs := metas.lookup .colorSpace
  let colorType : ColorSpaceType :=
    match cs with
    | some .sRGB => .sRGB
    | some (.windowsBitmapColorSpace _) => .calibratedRGB
    | some (.iccProfile _) => .profileEmbedded
    | _ => .deviceDependentRGB
  let colorSpaceInfo : ByteArray :=
    match cs with
    | some (.windowsBitmapColorSpace bytes) => bytes
    | _ => ByteArray.mk (Array.replicate sizeofColorProfile (0 : UInt8))
  let colorProfileData : Option ByteArray :=
    match cs with
    | some (.iccProfile bytes) => some bytes
    | _ => none
  let hasAlpha := BmpEncodable.hasAlphaFlag (pixel := pixel)
  let headerSize : UInt32 :=
    if colorType == .profileEmbedded then sizeofBmpV5Header
    else if colorType == .calibratedRGB || hasAlpha then sizeofBmpV4Header
    else sizeofBmpInfoHeader
  let paletteSize := palette.length
  let bpp := BmpEncodable.bitsPerPixel (pixel := pixel)
  let profileSize := (colorProfileData.map (·.size)).getD 0
  let imagePixelSize := sizeofPixelData bpp imgWidth imgHeight
  let offsetToData := sizeofBmpHeader + headerSize.toNat + 4 * paletteSize
  let offsetToICCProfile : UInt32 := if colorProfileData.isSome then (offsetToData + imagePixelSize).toUInt32 else 0
  let sizeOfFile := offsetToData + imagePixelSize + profileSize
  let hdr : BmpHeader :=
    { magicIdentifier := bitmapMagicIdentifier, fileSize := sizeOfFile.toUInt32, reserved1 := 0,
      reserved2 := 0, dataOffset := offsetToData.toUInt32 }
  let info : BmpV5Header :=
    { size := headerSize, width := (imgWidth : Int), height := (imgHeight : Int), planes := 1,
      bitPerPixel := bpp.toUInt16, bitmapCompression := if hasAlpha then 3 else 0,
      byteImageSize := imagePixelSize.toUInt32, xResolution := (dpiX.toNat : Int),
      yResolution := (dpiY.toNat : Int), colorCount := paletteSize.toUInt32, importantColours := 0,
      redMask := if hasAlpha then 0x00FF0000 else 0, greenMask := if hasAlpha then 0x0000FF00 else 0,
      blueMask := if hasAlpha then 0x000000FF else 0, alphaMask := if hasAlpha then 0xFF000000 else 0,
      colorSpaceType := colorType, colorSpace := colorSpaceInfo, iccIntent := 0,
      iccProfileData := offsetToICCProfile, iccProfileSize := profileSize.toUInt32 }
  let builder := putBmpHeader hdr ++ putBmpV5Header info ++ putPalette palette ++
    BmpEncodable.bmpEncode img ++
    (match colorProfileData with | some bytes => builderOfByteArray bytes | none => Builder.empty)
  builder.toStrictByteString

/-- Encode an image into a bitmap, with an explicit palette. -/
def encodeBitmapWithPalette [Pixel pixel Pixel8] [BmpEncodable pixel]
    (palette : List BmpPaletteEntry) (img : Image pixel) : Data.ByteString :=
  encodeBitmapWithPaletteAndMetadata Metadatas.empty palette img

/-- Encode an image into a bitmap, with metadata but this pixel type's
    default palette. -/
def encodeBitmapWithMetadata [Pixel pixel Pixel8] [BmpEncodable pixel]
    (metas : Metadatas) (img : Image pixel) : Data.ByteString :=
  encodeBitmapWithPaletteAndMetadata metas (BmpEncodable.defaultPalette (pixel := pixel)) img

/-- Encode an image into a bitmap. -/
def encodeBitmap [Pixel pixel Pixel8] [BmpEncodable pixel] (img : Image pixel) : Data.ByteString :=
  encodeBitmapWithMetadata Metadatas.empty img

/-- Encode one of the pixel formats a bitmap can represent (8-bit grayscale,
    24-bit RGB, or 32-bit RGBA); any other `DynamicImage` variant is
    unsupported. -/
def encodeDynamicBitmap : DynamicImage → Except String Data.ByteString
  | .y8 img => .ok (encodeBitmap img)
  | .rgb8 img => .ok (encodeBitmap img)
  | .rgba8 img => .ok (encodeBitmap img)
  | _ => .error "Unsupported image format for bitmap export"

end Codec.Picture
