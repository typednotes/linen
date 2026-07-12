/-
  Tests for `Linen.Codec.Picture.Tiff.Internal.Metadata` — IFD list ↔
  `Metadatas` conversion (`extractTiffMetadata`/`extractTiffDpiMetadata`/
  `extractTiffStringMetadata` decoding, `encodeTiffStringMetadata`/`makeIfd`
  encoding).
-/
import Linen.Codec.Picture.Tiff.Internal.Metadata

open Codec.Picture

-- ── Fixtures ──

def tiffMetaWidthIfd : ImageFileDirectory :=
  { ifdIdentifier := .imageWidth, ifdType := .long, ifdCount := 1, ifdOffset := 640,
    ifdExtended := .none }

def tiffMetaHeightIfd : ImageFileDirectory :=
  { ifdIdentifier := .imageLength, ifdType := .long, ifdCount := 1, ifdOffset := 480,
    ifdExtended := .none }

def tiffMetaSoftwareIfd : ImageFileDirectory :=
  { ifdIdentifier := .software, ifdType := .ascii, ifdCount := 5, ifdOffset := 0,
    ifdExtended := .string (ByteArray.mk #[76, 105, 110, 101, 110]) } -- "Linen"

def tiffMetaArtistIfd : ImageFileDirectory :=
  { ifdIdentifier := .artist, ifdType := .ascii, ifdCount := 3, ifdOffset := 0,
    ifdExtended := .string (ByteArray.mk #[65, 100, 97]) } -- "Ada"

-- Decoding an `.orientation` entry reads its value from the *low* 16 bits
-- of `ifdOffset` (`ifdToMetadata`'s `exifShort`, i.e. `ifdOffset.toUInt16`
-- with no shift) — note this is *not* the mirror image of `makeIfd`, which
-- inlines a `.short` into the *high* 16 bits on encode (see the `makeIfd`
-- tests below); upstream's own `Metadata.hs` has this same encode/decode
-- asymmetry (`makeIfd`'s hardcoded `unsafeShiftL 16` vs. `exifShort`'s bare
-- `fromIntegral`), ported here faithfully rather than "fixed".
def tiffMetaOrientationIfd : ImageFileDirectory :=
  { ifdIdentifier := .orientation, ifdType := .short, ifdCount := 1, ifdOffset := 1,
    ifdExtended := .none }

def tiffMetaUnknownIfd : ImageFileDirectory :=
  { ifdIdentifier := .unknown 0xBEEF, ifdType := .long, ifdCount := 1, ifdOffset := 42,
    ifdExtended := .long 42 }

def tiffMetaResolutionUnitInchIfd : ImageFileDirectory :=
  { ifdIdentifier := .resolutionUnit, ifdType := .short, ifdCount := 1, ifdOffset := 2,
    ifdExtended := .none }

def tiffMetaResolutionUnitCentimeterIfd : ImageFileDirectory :=
  { ifdIdentifier := .resolutionUnit, ifdType := .short, ifdCount := 1, ifdOffset := 3,
    ifdExtended := .none }

def tiffMetaXResolutionIfd : ImageFileDirectory :=
  { ifdIdentifier := .xResolution, ifdType := .rational, ifdCount := 1, ifdOffset := 0,
    ifdExtended := .rational 300 1 }

def tiffMetaYResolutionIfd : ImageFileDirectory :=
  { ifdIdentifier := .yResolution, ifdType := .rational, ifdCount := 1, ifdOffset := 0,
    ifdExtended := .rational 300 1 }

def tiffMetaIfds : List ImageFileDirectory :=
  [ tiffMetaWidthIfd, tiffMetaHeightIfd, tiffMetaSoftwareIfd, tiffMetaArtistIfd,
    tiffMetaOrientationIfd, tiffMetaUnknownIfd ]

-- ── `extractTiffStringMetadata` ──

#guard (extractTiffStringMetadata tiffMetaIfds).lookup .width == some 640
#guard (extractTiffStringMetadata tiffMetaIfds).lookup .height == some 480
#guard (extractTiffStringMetadata tiffMetaIfds).lookup .software == some "Linen"
#guard (extractTiffStringMetadata tiffMetaIfds).lookup .author == some "Ada"
#guard (extractTiffStringMetadata tiffMetaIfds).lookup .format == some .tiff
#guard (extractTiffStringMetadata tiffMetaIfds).lookup (.exif .orientation) == some (.short 1)
#guard (extractTiffStringMetadata tiffMetaIfds).lookup (.exif (.unknown 0xBEEF)) == some (.long 42)

-- Tags with no metadata counterpart contribute nothing.
def tiffMetaCompressionIfd : ImageFileDirectory :=
  { ifdIdentifier := .compression, ifdType := .short, ifdCount := 1, ifdOffset := 1,
    ifdExtended := .none }

#guard (extractTiffStringMetadata [tiffMetaCompressionIfd]).lookup .width == none

-- ── `extractTiffDpiMetadata` ──

-- No `TagResolutionUnit` entry at all ⇒ no DPI metadata.
#guard (extractTiffDpiMetadata [tiffMetaXResolutionIfd, tiffMetaYResolutionIfd]).lookup .dpiX == none

-- `inch` unit passes the rational value through unchanged.
#guard (extractTiffDpiMetadata
    [tiffMetaResolutionUnitInchIfd, tiffMetaXResolutionIfd, tiffMetaYResolutionIfd]).lookup .dpiX
  == some 300
#guard (extractTiffDpiMetadata
    [tiffMetaResolutionUnitInchIfd, tiffMetaXResolutionIfd, tiffMetaYResolutionIfd]).lookup .dpiY
  == some 300

-- `centimeter` unit converts to DPI.
#guard (extractTiffDpiMetadata
    [tiffMetaResolutionUnitCentimeterIfd, tiffMetaXResolutionIfd]).lookup .dpiX
  == some (dotsPerCentiMeterToDotPerInch 300)

-- ── `extractTiffMetadata` — combines DPI and string/tag metadata ──

def tiffMetaFullIfds : List ImageFileDirectory :=
  tiffMetaResolutionUnitInchIfd :: tiffMetaXResolutionIfd :: tiffMetaIfds

#guard (extractTiffMetadata tiffMetaFullIfds).lookup .dpiX == some 300
#guard (extractTiffMetadata tiffMetaFullIfds).lookup .width == some 640
#guard (extractTiffMetadata tiffMetaFullIfds).lookup .software == some "Linen"

-- ── `makeIfd` — inline vs. extended encoding ──

-- A `.short` value is left-justified into the top 16 bits of `ifdOffset`.
#guard (makeIfd .orientation (.short 1)).ifdOffset == (1 : UInt32) <<< 16
#guard (makeIfd .orientation (.short 1)).ifdExtended == .none

-- A `.long` value is stored inline.
#guard (makeIfd .imageWidth (.long 640)).ifdOffset == 640

-- Two `.short`s combine into a single 32-bit inline value.
#guard (makeIfd .bitsPerSample (.shorts #[8, 8])).ifdOffset == ((8 : UInt32) <<< 16) ||| 8
#guard (makeIfd .bitsPerSample (.shorts #[8, 8])).ifdExtended == .none

-- Three or more `.short`s don't fit inline, so they land in `ifdExtended`.
#guard (makeIfd .bitsPerSample (.shorts #[8, 8, 8])).ifdExtended == .shorts #[8, 8, 8]
#guard (makeIfd .bitsPerSample (.shorts #[8, 8, 8])).ifdCount == 3

-- A single `.long` in a `.longs` array is stored inline.
#guard (makeIfd .stripOffsets (.longs #[100])).ifdOffset == 100
#guard (makeIfd .stripOffsets (.longs #[100])).ifdExtended == .none

-- More than one `.long` doesn't fit inline.
#guard (makeIfd .stripOffsets (.longs #[100, 200])).ifdExtended == .longs #[100, 200]

-- A `.string` value always lands in `ifdExtended`, with its byte length as
-- the entry count.
#guard (makeIfd .software (.string (ByteArray.mk #[76, 105, 110, 101, 110]))).ifdCount == 5
#guard (makeIfd .software (.string (ByteArray.mk #[76, 105, 110, 101, 110]))).ifdOffset == 0

-- An `.undefined` value of more than 4 bytes lands in `ifdExtended`.
#guard (makeIfd .exifOffset (.undefined (ByteArray.mk #[1, 2, 3, 4, 5]))).ifdExtended
  == .undefined (ByteArray.mk #[1, 2, 3, 4, 5])

-- An `.undefined` value of 4 bytes or fewer is left-justified inline
-- (MSB-first, see the module doc-comment for why this differs from
-- upstream's undefined-behaviour shift amounts).
#guard (makeIfd .exifOffset (.undefined (ByteArray.mk #[0x01, 0x02]))).ifdOffset
  == ((0x01 : UInt32) <<< 24) ||| ((0x02 : UInt32) <<< 16)
#guard (makeIfd .exifOffset (.undefined (ByteArray.mk #[0x01, 0x02]))).ifdExtended == .none

-- A `.rational` falls to the generic case, tagged with `.rational`.
#guard (makeIfd .xResolution (.rational 300 1)).ifdType == .rational
#guard (makeIfd .xResolution (.rational 300 1)).ifdExtended == .rational 300 1

-- ── `encodeTiffStringMetadata` ──

def tiffMetaEncodeMetas : Metadatas :=
  (Metadatas.singleton .software "Linen").union
    ((Metadatas.singleton .author "Ada").union
      (Metadatas.singleton (.exif .orientation) (.short 1)))

#guard (encodeTiffStringMetadata tiffMetaEncodeMetas).any
  (fun ifd => ifd.ifdIdentifier == .software ∧ ifd.ifdExtended == .string (ByteArray.mk #[76, 105, 110, 101, 110]))
#guard (encodeTiffStringMetadata tiffMetaEncodeMetas).any
  (fun ifd => ifd.ifdIdentifier == .artist ∧ ifd.ifdExtended == .string (ByteArray.mk #[65, 100, 97]))
#guard (encodeTiffStringMetadata tiffMetaEncodeMetas).any
  (fun ifd => ifd.ifdIdentifier == .orientation ∧ ifd.ifdOffset == (1 : UInt32) <<< 16)

-- The result is sorted by ascending tag code.
#guard (encodeTiffStringMetadata tiffMetaEncodeMetas).map (·.ifdIdentifier.toWord16)
  == ((encodeTiffStringMetadata tiffMetaEncodeMetas).map (·.ifdIdentifier.toWord16)).mergeSort (· <= ·)

-- No relevant metadata ⇒ no entries.
#guard encodeTiffStringMetadata .empty == []

-- ── `exifOffsetIfd` ──

#guard exifOffsetIfd.ifdIdentifier == .exifOffset
#guard exifOffsetIfd.ifdType == .long
