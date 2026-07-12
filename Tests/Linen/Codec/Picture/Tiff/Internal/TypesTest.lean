/-
  Tests for `Linen.Codec.Picture.Tiff.Internal.Types` — header byte-order
  detection for both `"II"`/`"MM"` magic, IFD-entry round trips in each
  endianness, `ExifTag` code round trips through the endianness-parameterized
  reader/writer, and `TiffCompression`/`TiffSampleFormat`/
  `TiffPlanarConfiguration`/`TiffColorspace`/`IfdType`/`Predictor`/
  `ExtraSample` code round trips.
-/
import Linen.Codec.Picture.Tiff.Internal.Types

open Codec.Picture

-- ── `TiffHeader` byte-order detection ──

def tiffTypeLittleHeaderBytes : List UInt8 := [0x49, 0x49, 42, 0, 8, 0, 0, 0]
def tiffTypeBigHeaderBytes : List UInt8 := [0x4D, 0x4D, 0, 42, 0, 0, 0, 8]

#guard match parseTiffHeader tiffTypeLittleHeaderBytes with
  | .ok (h, []) => h.endianness == .little ∧ h.offset == 8
  | _ => false

#guard match parseTiffHeader tiffTypeBigHeaderBytes with
  | .ok (h, []) => h.endianness == .big ∧ h.offset == 8
  | _ => false

-- Wrong magic bytes are rejected.
#guard match parseTiffEndianness [0x00, 0x00] with | .error _ => true | .ok _ => false

-- `putTiffHeader` round-trips through `parseTiffHeader`, in both
-- endiannesses.
def tiffTypeLittleHeader : TiffHeader := { endianness := .little, offset := 0x1234 }
def tiffTypeBigHeader : TiffHeader := { endianness := .big, offset := 0x1234 }

#guard match parseTiffHeader (putTiffHeader tiffTypeLittleHeader).toStrictByteString.unpack with
  | .ok (h, _) => h == tiffTypeLittleHeader
  | .error _ => false

#guard match parseTiffHeader (putTiffHeader tiffTypeBigHeader).toStrictByteString.unpack with
  | .ok (h, _) => h == tiffTypeBigHeader
  | .error _ => false

-- ── `ImageFileDirectory` round trip, in each endianness ──

def tiffTypeSampleIfd : ImageFileDirectory :=
  { ifdIdentifier := .imageWidth, ifdType := .long, ifdCount := 1, ifdOffset := 640, ifdExtended := .none }

#guard match parseImageFileDirectory .little (putImageFileDirectory .little tiffTypeSampleIfd).toStrictByteString.unpack with
  | .ok (ifd, []) => ifd == tiffTypeSampleIfd
  | _ => false

#guard match parseImageFileDirectory .big (putImageFileDirectory .big tiffTypeSampleIfd).toStrictByteString.unpack with
  | .ok (ifd, []) => ifd == tiffTypeSampleIfd
  | _ => false

-- A full IFD (entry count + entries + next-IFD offset) round-trips too.
def tiffTypeSampleIfdList : List ImageFileDirectory :=
  [ { ifdIdentifier := .imageWidth, ifdType := .long, ifdCount := 1, ifdOffset := 640, ifdExtended := .none }
  , { ifdIdentifier := .imageLength, ifdType := .long, ifdCount := 1, ifdOffset := 480, ifdExtended := .none } ]

#guard match parseImageFileDirectoryList .little
    (putImageFileDirectoryList .little tiffTypeSampleIfdList 0).toStrictByteString.unpack with
  | .ok (ifds, nextOffset, []) => ifds == tiffTypeSampleIfdList ∧ nextOffset == 0
  | _ => false

#guard match parseImageFileDirectoryList .big
    (putImageFileDirectoryList .big tiffTypeSampleIfdList 1234).toStrictByteString.unpack with
  | .ok (ifds, nextOffset, []) => ifds == tiffTypeSampleIfdList ∧ nextOffset == 1234
  | _ => false

-- ── `ExifTag` code round trip, endianness-parameterized ──

#guard match parseExifTag .little (putExifTag .little ExifTag.imageWidth).toStrictByteString.unpack with
  | .ok (t, []) => t == .imageWidth
  | _ => false

#guard match parseExifTag .big (putExifTag .big ExifTag.compression).toStrictByteString.unpack with
  | .ok (t, []) => t == .compression
  | _ => false

-- ── `IfdType` code round trip ──

#guard match ifdTypeOfCode 1 with | .ok t => codeOfIfdType t == 1 | .error _ => false
#guard match ifdTypeOfCode 3 with | .ok t => codeOfIfdType t == 3 | .error _ => false
#guard match ifdTypeOfCode 5 with | .ok t => codeOfIfdType t == 5 | .error _ => false
#guard match ifdTypeOfCode 12 with | .ok t => codeOfIfdType t == 12 | .error _ => false
#guard match ifdTypeOfCode 0 with | .ok _ => false | .error _ => true

#guard ifdTypeByteSize .byte == 1
#guard ifdTypeByteSize .short == 2
#guard ifdTypeByteSize .long == 4
#guard ifdTypeByteSize .rational == 8

-- `parseIfdType` round-trips through `putIfdType`, honouring endianness.
#guard match parseIfdType .little (putIfdType .little .rational).toStrictByteString.unpack with
  | .ok (t, []) => t == .rational
  | _ => false

-- ── `TiffCompression` code round trip ──

#guard match unpackCompression 1 with | .ok c => packCompression c == 1 | .error _ => false
#guard match unpackCompression 0 with | .ok c => c == .none | .error _ => false
#guard match unpackCompression 2 with | .ok c => packCompression c == 2 | .error _ => false
#guard match unpackCompression 5 with | .ok c => packCompression c == 5 | .error _ => false
#guard match unpackCompression 6 with | .ok c => packCompression c == 6 | .error _ => false
#guard match unpackCompression 32773 with | .ok c => packCompression c == 32773 | .error _ => false
#guard match unpackCompression 999 with | .ok _ => false | .error _ => true

-- ── `TiffSampleFormat` code round trip ──

#guard match unpackSampleFormat 1 with | .ok f => packSampleFormat f == 1 | .error _ => false
#guard match unpackSampleFormat 2 with | .ok f => packSampleFormat f == 2 | .error _ => false
#guard match unpackSampleFormat 3 with | .ok f => packSampleFormat f == 3 | .error _ => false
#guard match unpackSampleFormat 4 with | .ok f => packSampleFormat f == 4 | .error _ => false
#guard match unpackSampleFormat 5 with | .ok _ => false | .error _ => true

-- ── `TiffPlanarConfiguration` code round trip ──

#guard match planarConfgOfConstant 0 with | .ok c => c == .contig | .error _ => false
#guard match planarConfgOfConstant 1 with | .ok c => constantOfPlanarConfg c == 1 | .error _ => false
#guard match planarConfgOfConstant 2 with | .ok c => constantOfPlanarConfg c == 2 | .error _ => false
#guard match planarConfgOfConstant 3 with | .ok _ => false | .error _ => true

-- ── `TiffColorspace` code round trip ──

#guard match unpackPhotometricInterpretation 0 with | .ok c => packPhotometricInterpretation c == 0 | .error _ => false
#guard match unpackPhotometricInterpretation 2 with | .ok c => packPhotometricInterpretation c == 2 | .error _ => false
#guard match unpackPhotometricInterpretation 5 with | .ok c => packPhotometricInterpretation c == 5 | .error _ => false
#guard match unpackPhotometricInterpretation 6 with | .ok c => packPhotometricInterpretation c == 6 | .error _ => false
#guard match unpackPhotometricInterpretation 8 with | .ok c => packPhotometricInterpretation c == 8 | .error _ => false
#guard match unpackPhotometricInterpretation 7 with | .ok _ => false | .error _ => true

-- ── `Predictor` code round trip ──

#guard match predictorOfConstant 1 with | .ok p => constantOfPredictor p == 1 | .error _ => false
#guard match predictorOfConstant 2 with | .ok p => constantOfPredictor p == 2 | .error _ => false
#guard match predictorOfConstant 3 with | .ok _ => false | .error _ => true

-- ── `ExtraSample` code round trip ──

#guard match extraSampleOfCode 0 with | .ok e => codeOfExtraSample e == 0 | .error _ => false
#guard match extraSampleOfCode 1 with | .ok e => codeOfExtraSample e == 1 | .error _ => false
#guard match extraSampleOfCode 2 with | .ok e => codeOfExtraSample e == 2 | .error _ => false
#guard match extraSampleOfCode 3 with | .ok _ => false | .error _ => true

-- ── `padOddLength` ──

#guard (padOddLength (ByteArray.mk #[1, 2, 3])).toStrictByteString.unpack == [1, 2, 3, 0]
#guard (padOddLength (ByteArray.mk #[1, 2])).toStrictByteString.unpack == [1, 2]
