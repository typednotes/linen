/-
  Linen.Graphics.Netpbm — parsing the netpbm image formats

  ## Haskell equivalent
  `Graphics.Netpbm` from https://hackage.haskell.org/package/netpbm

  ## Design
  Parses the "portable anymap" family (PBM/PGM/PPM, ASCII and binary
  variants, magic numbers `P1`–`P6`) from a `ByteArray`. Upstream builds
  this on `attoparsec`/`attoparsec-binary`; per
  `docs/imports/netpbm/dependencies.md` this port uses the Lean stdlib's
  `Std.Internal.Parsec.ByteArray` parser combinators instead — no separate
  parser-combinator library import is needed.

  Upstream stores pixel rasters in `Data.Vector.Storable`, with a hand-rolled
  `Storable` instance for every pixel type (`Foreign.Storable.Record`) purely
  to describe their C memory layout for that FFI-backed vector. Lean's
  persistent `Array` needs no such layout descriptor, so pixel rasters are
  plain `Array`s here and the `Storable`/`Unbox` instances (and their
  `storable-record`/`vector-th-unbox` dependencies) are dropped entirely —
  the same simplification already applied to `repa`'s `Manifest` (see
  `Linen/Data/Array/Shaped/Repr/Manifest.lean`).

  `Std.Internal.Parsec`'s repetition/choice combinators (`many`, `<|>`) only
  backtrack when the failing branch consumed no input, whereas `attoparsec`'s
  backtrack unconditionally; every choice/repetition below that could
  otherwise partially consume input before failing is wrapped in `attempt`
  to restore attoparsec's semantics.

  `imagesParser`'s upstream `error` call for "an ASCII image file produced
  more than one image" is dropped: it is unreachable by construction (every
  ASCII body parser below ends by asserting `eof`, so a second image can
  never be found after a first ASCII one) — upstream's own comment
  acknowledges this ("TODO Restructure so that this cannot happen").
-/

import Std.Internal.Parsec.ByteArray

namespace Graphics.Netpbm

open Std.Internal.Parsec
open Std.Internal.Parsec.ByteArray

-- ── Types ──

/-- The netpbm image type of an image. -/
inductive PPMType where
  | P1 -- ^ ASCII bitmap
  | P2 -- ^ ASCII greymap
  | P3 -- ^ ASCII pixmap (color)
  | P4 -- ^ binary bitmap
  | P5 -- ^ binary greymap
  | P6 -- ^ binary pixmap (color)
  deriving BEq, DecidableEq, Repr

/-- Meta information about the image: the exact PPM format and dimensions. -/
structure PPMHeader where
  ppmType : PPMType
  ppmWidth : Nat
  ppmHeight : Nat
  deriving BEq, Repr

/-- A pixel containing three 8-bit color components, RGB. -/
structure PpmPixelRGB8 where
  r : UInt8
  g : UInt8
  b : UInt8
  deriving BEq, Repr

/-- A pixel containing three 16-bit color components, RGB. -/
structure PpmPixelRGB16 where
  r : UInt16
  g : UInt16
  b : UInt16
  deriving BEq, Repr

/-- A pixel containing black or white: `isWhite = false` is black, `true` is white. -/
structure PbmPixel where
  isWhite : Bool
  deriving BEq, Repr

/-- A pixel containing an 8-bit greyscale value. -/
structure PgmPixel8 where
  v : UInt8
  deriving BEq, Repr

/-- A pixel containing a 16-bit greyscale value. -/
structure PgmPixel16 where
  v : UInt16
  deriving BEq, Repr

/-- Image data, either 8 or 16 bits. -/
inductive PpmPixelData where
  | rgb8 (pixels : Array PpmPixelRGB8)   -- ^ For 8-bit PPMs.
  | rgb16 (pixels : Array PpmPixelRGB16) -- ^ For 16-bit PPMs.
  | pbm (pixels : Array PbmPixel)        -- ^ For 1-bit PBMs.
  | grey8 (pixels : Array PgmPixel8)     -- ^ For 8-bit PGMs.
  | grey16 (pixels : Array PgmPixel16)   -- ^ For 16-bit PGMs.
  deriving BEq, Repr

/-- A PPM file with type, dimensions, and image data. -/
structure PPM where
  ppmHeader : PPMHeader
  ppmData : PpmPixelData
  deriving BEq, Repr

/-- Converts a vector of pixels to a list for convenience. -/
def pixelVectorToList {a} (v : Array a) : List a := v.toList

/-- Converts pixel data to a list of `Int`s.

    How big they can become depends on the bit depth of the pixel data. -/
def pixelDataToIntList (d : PpmPixelData) : List Int :=
  match d with
  | .rgb8 v => v.toList.flatMap (fun p => [p.r, p.g, p.b].map (Int.ofNat ·.toNat))
  | .rgb16 v => v.toList.flatMap (fun p => [p.r, p.g, p.b].map (Int.ofNat ·.toNat))
  | .pbm v => v.toList.map (fun p => if p.isWhite then 1 else 0)
  | .grey8 v => v.toList.map (fun p => Int.ofNat p.v.toNat)
  | .grey16 v => v.toList.map (fun p => Int.ofNat p.v.toNat)

-- ── Low-level parsers ──

private def isNotNewline (b : UInt8) : Bool := b ≠ 10 && b ≠ 13

/-- Matches attoparsec's `Data.ByteString.Char8.isSpace_w8`: space, or one of
    tab/newline/vertical-tab/form-feed/carriage-return. -/
private def isSpaceByte (b : UInt8) : Bool := b = 32 || (b ≥ 9 && b ≤ 13)

/-- Parses a netpbm magic number: one of `P1`–`P6`. -/
def magicNumberParser : Parser PPMType := attempt do
  skipByteChar 'P'
  let d ← any
  if d = '1'.toUInt8 then pure .P1
  else if d = '2'.toUInt8 then pure .P2
  else if d = '3'.toUInt8 then pure .P3
  else if d = '4'.toUInt8 then pure .P4
  else if d = '5'.toUInt8 then pure .P5
  else if d = '6'.toUInt8 then pure .P6
  else fail "PPM: unknown PPM format"

private def endOfLine : Parser Unit := skipByteChar '\n' <|> (skipByteChar '\r' *> skipByteChar '\n')

/-- Not written as `skipMany comment` alone (see `sep`) because that would
    allow this parser to consume no input, looping forever inside `many`. -/
private def comment : Parser Unit := skipByteChar '#' *> skipWhile isNotNewline *> endOfLine

private def singleWhitespace : Parser Unit := satisfy isSpaceByte *> pure ()

/-- At least one space, optionally with more space or comments around. -/
private def sep : Parser Unit := do
  discard <| many (attempt comment)
  singleWhitespace
  discard <| many (attempt (singleWhitespace <|> comment))

/-- Skips zero or more whitespace bytes, never failing (matches attoparsec's
    non-failing `takeWhile isSpace_w8`, unlike `Std`'s EOF-sensitive `skipWhile`). -/
private def skipSpaces : Parser Unit := discard <| many (attempt singleWhitespace)

/-- Decimal, possibly with comments interleaved, but starting and ending with a digit. -/
private def decimalC : Parser Nat := do
  let first ← digit
  let rest ← many (attempt (discard (many (attempt comment)) *> digit))
  pure <| (#[first] ++ rest).foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

/-- Parses a byte no larger than the given maxval. -/
private def word8max (m : UInt8) : Parser UInt8 := do
  let b ← any
  if b ≤ m then pure b else fail "pixel data must be smaller than maxval"

/-- Parses a big-endian 16-bit word no larger than the given maxval. -/
private def word16max (m : UInt16) : Parser UInt16 := do
  let hi ← any
  let lo ← any
  let w : UInt16 := (hi.toUInt16 <<< 8) ||| lo.toUInt16
  if w ≤ m then pure w else fail "pixel data must be smaller than maxval"

private def isValidMaxval (v : Nat) : Bool := v > 0 && v < 65536

/-- If the next byte is whitespace, consume all remaining input (matching
    "any junk after the raster is allowed, as long as it starts with
    whitespace"); otherwise consume nothing. -/
private def consumeTrailingJunk : Parser Unit :=
  (attempt (discard (satisfy isSpaceByte) *> discard (many (attempt any)))) <|> pure ()

-- ── Header ──

private def headerParser : Parser PPMHeader := do
  let ty ← magicNumberParser
  sep
  let width ← decimalC
  sep
  let height ← decimalC
  discard <| many (attempt comment) -- comments cannot appear after this point
  pure ⟨ty, width, height⟩

-- ── Binary body parsers ──

/-- Parses a single binary PPM (P6) image body.

    Specification: http://netpbm.sourceforge.net/doc/ppm.html -/
private def ppmBodyParser (header : PPMHeader) : Parser PPM := do
  sep
  let maxColorVal ← decimalC
  if !isValidMaxval maxColorVal then fail s!"PPM: invalid color maxval {maxColorVal}"
  discard <| many (attempt comment)
  singleWhitespace
  let n := header.ppmHeight * header.ppmWidth
  if maxColorVal < 256 then
    let m := UInt8.ofNat maxColorVal
    let pixels ← (List.range n).mapM (fun _ => do
      let r ← word8max m; let g ← word8max m; let b ← word8max m
      pure (⟨r, g, b⟩ : PpmPixelRGB8))
    pure ⟨header, .rgb8 pixels.toArray⟩
  else
    let m := UInt16.ofNat maxColorVal
    let pixels ← (List.range n).mapM (fun _ => do
      let r ← word16max m; let g ← word16max m; let b ← word16max m
      pure (⟨r, g, b⟩ : PpmPixelRGB16))
    pure ⟨header, .rgb16 pixels.toArray⟩

/-- Parses a single binary PGM (P5) image body. -/
private def pgmBodyParser (header : PPMHeader) : Parser PPM := do
  sep
  let maxGreyVal ← decimalC
  if !isValidMaxval maxGreyVal then fail s!"PGM: invalid grey maxval {maxGreyVal}"
  discard <| many (attempt comment)
  singleWhitespace
  let n := header.ppmHeight * header.ppmWidth
  if maxGreyVal < 256 then
    let m := UInt8.ofNat maxGreyVal
    let pixels ← (List.range n).mapM (fun _ => (⟨·⟩ : UInt8 → PgmPixel8) <$> word8max m)
    pure ⟨header, .grey8 pixels.toArray⟩
  else
    let m := UInt16.ofNat maxGreyVal
    let pixels ← (List.range n).mapM (fun _ => (⟨·⟩ : UInt16 → PgmPixel16) <$> word16max m)
    pure ⟨header, .grey16 pixels.toArray⟩

/-- Parses a single binary PBM (P4) image body.

    From http://netpbm.sourceforge.net/doc/pbm.html: "Each row is Width
    bits, packed 8 to a byte, with don't care bits to fill out the last
    byte in the row." -/
private def pbmBodyParser (header : PPMHeader) : Parser PPM := do
  singleWhitespace
  let widthBytes := (header.ppmWidth + 7) / 8
  let byteList ← (List.range (header.ppmHeight * widthBytes)).mapM (fun _ => any)
  let byteArr := byteList.toArray
  -- 1 is black, 0 is white; `testBit` indexes from the right (LSB), hence `not`.
  let pixels := (List.range (header.ppmHeight * header.ppmWidth)).toArray.map (fun i =>
    let row := i / header.ppmWidth
    let col := i % header.ppmWidth
    let i8 := row * widthBytes + col / 8
    let bitN := col % 8
    (⟨!(byteArr[i8]!.toNat.testBit (7 - bitN))⟩ : PbmPixel))
  pure ⟨header, .pbm pixels⟩

-- ── ASCII body parsers ──

private def asciiBit : Parser PbmPixel := do
  let w ← any
  if w = '0'.toUInt8 then pure ⟨true⟩
  else if w = '1'.toUInt8 then pure ⟨false⟩
  else fail "ASCII bit must be '0' or '1'"

/-- Parses a single ASCII PBM (P1) image body.

    See also the notes for `imagesParser`. We ignore the "no line should be
    longer than 70 characters" rule, as it's a "should", not a "must". -/
private def pbmAsciiBodyParser (header : PPMHeader) : Parser PPM := do
  singleWhitespace
  let n := header.ppmHeight * header.ppmWidth
  let pixels ← (List.range n).mapM (fun _ => skipSpaces *> asciiBit)
  consumeTrailingJunk
  eof
  pure ⟨header, .pbm pixels.toArray⟩

/-- Parses a single ASCII PGM (P2) image body. -/
private def pgmAsciiBodyParser (header : PPMHeader) : Parser PPM := do
  sep
  let maxGreyVal ← decimalC
  if !isValidMaxval maxGreyVal then fail s!"PGM: invalid grey maxval {maxGreyVal}"
  discard <| many (attempt comment)
  singleWhitespace
  let n := header.ppmHeight * header.ppmWidth
  if maxGreyVal < 256 then
    let pixels ← (List.range n).mapM (fun _ => skipSpaces *> ((⟨UInt8.ofNat ·⟩ : Nat → PgmPixel8) <$> digits))
    consumeTrailingJunk
    eof
    pure ⟨header, .grey8 pixels.toArray⟩
  else
    let pixels ← (List.range n).mapM (fun _ => skipSpaces *> ((⟨UInt16.ofNat ·⟩ : Nat → PgmPixel16) <$> digits))
    consumeTrailingJunk
    eof
    pure ⟨header, .grey16 pixels.toArray⟩

/-- Parses a single ASCII PPM (P3) image body. -/
private def ppmAsciiBodyParser (header : PPMHeader) : Parser PPM := do
  sep
  let maxColorVal ← decimalC
  if !isValidMaxval maxColorVal then fail s!"PPM: invalid color maxval {maxColorVal}"
  discard <| many (attempt comment)
  singleWhitespace
  let n := header.ppmHeight * header.ppmWidth
  if maxColorVal < 256 then
    let d8 : Parser UInt8 := skipSpaces *> (UInt8.ofNat <$> digits)
    let pixels ← (List.range n).mapM (fun _ => do
      let r ← d8; let g ← d8; let b ← d8
      pure (⟨r, g, b⟩ : PpmPixelRGB8))
    pure ⟨header, .rgb8 pixels.toArray⟩
  else
    let d16 : Parser UInt16 := skipSpaces *> (UInt16.ofNat <$> digits)
    let pixels ← (List.range n).mapM (fun _ => do
      let r ← d16; let g ← d16; let b ← d16
      pure (⟨r, g, b⟩ : PpmPixelRGB16))
    pure ⟨header, .rgb16 pixels.toArray⟩

-- ── Top level ──

private def imageParserOfType (mpN : Option PPMType) : Parser PPM := do
  let header ← headerParser
  match mpN with
  | some pN =>
    if pN ≠ header.ppmType then
      fail "an image in a multi-image file is not of the same type as the first image in the file"
  | none => pure ()
  match header.ppmType with
  | .P1 => pbmAsciiBodyParser header
  | .P2 => pgmAsciiBodyParser header
  | .P3 => ppmAsciiBodyParser header
  | .P4 => pbmBodyParser header
  | .P5 => pgmBodyParser header
  | .P6 => ppmBodyParser header

/-- Parses a full PPM file, containing one or more images.

    From the spec: "A PPM file consists of a sequence of one or more PPM
    images. There are no data, delimiters, or padding before, after, or
    between images." However, files with trailing whitespace (especially a
    final `'\n'`) are found in the wild, so this is allowed. -/
private def imagesParser : Parser (List PPM) := do
  let first ← imageParserOfType none
  skipSpaces
  let others ← many (attempt (imageParserOfType (some first.ppmHeader.ppmType) <* skipSpaces))
  pure (first :: others.toList)

/-- The result of a PPM parse.

    See `parsePPM`. -/
abbrev PpmParseResult := Except String (List PPM × Option ByteArray)

/-- Parses a PPM file from the given `ByteArray`.

    On failure, `.error msg` contains the error message. On success,
    `.ok (images, rest)` contains the parsed images and potentially an
    unparsable rest input. -/
def parsePPM (bs : ByteArray) : PpmParseResult :=
  match imagesParser bs.iter with
  | .success it images =>
    if it.hasNext then .ok (images, some (it.array.extract it.idx it.array.size)) else .ok (images, none)
  | .error it err => .error s!"offset {it.pos}: {err}"

end Graphics.Netpbm
