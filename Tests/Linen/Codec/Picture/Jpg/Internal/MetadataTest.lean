/-
  Tests for `Linen.Codec.Picture.Jpg.Internal.Metadata` — JFIF `APP0` DPI
  extraction/encoding (`extractJFIFMetadatas`/`encodeJFIFMetadatas`), `APP1`
  Exif extraction/encoding (`parseExifApp1`/`extractApp1ExifMetadatas`/
  `encodeApp1ExifMetadatas`), and the combined `extractJpgMetadatas`/
  `encodeJpgMetadatas`.
-/
import Linen.Codec.Picture.Jpg.Internal.Metadata

open Codec.Picture
open Codec.Picture.Jpg.Internal
open Data.ByteString (Builder)

-- ── JFIF `APP0` ↔ DPI metadata ──

def jpgMetaJfifInch : JpgJFIFApp0 := { unit := .dotsPerInch, dpiX := 72, dpiY := 96 }
def jpgMetaJfifUnknown : JpgJFIFApp0 := { unit := .unitUnknown, dpiX := 72, dpiY := 96 }
def jpgMetaJfifCentimeter : JpgJFIFApp0 := { unit := .dotsPerCentimeter, dpiX := 100, dpiY := 100 }

#guard (extractJFIFMetadatas jpgMetaJfifInch).lookup .dpiX == some 72
#guard (extractJFIFMetadatas jpgMetaJfifInch).lookup .dpiY == some 96

-- `.unitUnknown` contributes no DPI metadata at all.
#guard (extractJFIFMetadatas jpgMetaJfifUnknown).lookup .dpiX == none

-- `.dotsPerCentimeter` converts to DPI.
#guard (extractJFIFMetadatas jpgMetaJfifCentimeter).lookup .dpiX == some (dotsPerCentiMeterToDotPerInch 100)

-- Round trip: encode then extract yields the original values back, always
-- tagged `.dotsPerInch` (`encodeJFIFMetadatas` always emits inch units).
def jpgMetaDpiMetas : Metadatas := (Metadatas.singleton .dpiX 150).union (Metadatas.singleton .dpiY 300)

#guard match encodeJFIFMetadatas jpgMetaDpiMetas with
  | [.jfifFrame jfif] => (extractJFIFMetadatas jfif).lookup .dpiX == some 150
      ∧ (extractJFIFMetadatas jfif).lookup .dpiY == some 300
  | _ => false

-- No DPI metadata at all ⇒ no frame.
#guard encodeJFIFMetadatas .empty == []

-- ── `APP1` Exif: hand-crafted decode ──

/-- A single `.model` ASCII entry, `"Lean5"` (5 bytes, so it needs
    out-of-line storage: `5 * ifdTypeByteSize .ascii = 5 > 4`), stored right
    after this one-entry IFD block (`8` header bytes + `18` IFD-block bytes
    `= 26`). -/
def jpgMetaHandcraftedIfd : ImageFileDirectory :=
  { ifdIdentifier := .model, ifdType := .ascii, ifdCount := 5, ifdOffset := 26, ifdExtended := .none }

/-- A hand-built `"Exif\0\0"`-prefixed `APP1` payload wrapping a minimal
    little-endian TIFF blob: header, one IFD entry, and its out-of-line
    `"Lean5"` payload (with the mandatory odd-length pad byte). -/
def jpgMetaHandcraftedApp1 : ByteArray :=
  (Data.ByteString.copy
    (Builder.stringUtf8 "Exif" ++ Builder.word8 0 ++ Builder.word8 0 ++
     putTiffHeader { endianness := .little, offset := 8 } ++
     putImageFileDirectoryList .little [jpgMetaHandcraftedIfd] 0 ++
     Builder.stringUtf8 "Lean5" ++ Builder.word8 0).toStrictByteString).data

#guard (parseExifApp1 jpgMetaHandcraftedApp1).isSome

#guard match parseExifApp1 jpgMetaHandcraftedApp1 with
  | some ifds => (extractTiffMetadata ifds).lookup (.exif .model) == some (.string "Lean5".toUTF8)
  | none => false

#guard (extractApp1ExifMetadatas jpgMetaHandcraftedApp1).lookup (.exif .model) == some (.string "Lean5".toUTF8)

-- A payload without the `"Exif\0\0"` marker carries no Exif metadata.
def jpgMetaNonExifApp1 : ByteArray := ByteArray.mk #[0, 1, 2, 3]

#guard parseExifApp1 jpgMetaNonExifApp1 == none
#guard (extractApp1ExifMetadatas jpgMetaNonExifApp1).elems.isEmpty

-- ── `APP1` Exif: encode/decode round trip ──

/-- A well-known string tag (`.software`) plus a generic Exif tag
    (`.exif .make`), both long enough (> 4 bytes) to force out-of-line
    storage and land in the primary IFD (`.software`'s and `.make`'s tag
    codes are both `≤ .copyright`'s, so `ExifTag.isInIFD0` holds for both —
    this fixture never exercises the second-IFD-block path, since nothing in
    the current `ExifTag`/`Metadata` decode side can read that block back
    (see the module doc-comment: `parseIfdChain`, like upstream's own
    decode, only ever returns the first IFD). -/
def jpgMetaRoundTripMetas : Metadatas :=
  (Metadatas.singleton .software "LeanCodecTest").union
    (Metadatas.singleton (.exif .make) (.string "LeanCorpMake".toUTF8))

#guard match encodeApp1ExifMetadatas jpgMetaRoundTripMetas with
  | [.appFrame 1 _] => true
  | _ => false

#guard match encodeApp1ExifMetadatas jpgMetaRoundTripMetas with
  | [.appFrame 1 raw] =>
      let decoded := extractApp1ExifMetadatas raw
      decoded.lookup .software == some "LeanCodecTest"
        ∧ decoded.lookup (.exif .make) == some (.string "LeanCorpMake".toUTF8)
  | _ => false

-- No relevant metadata ⇒ no `APP1` frame.
#guard encodeApp1ExifMetadatas .empty == []

-- ── Combined `extractJpgMetadatas`/`encodeJpgMetadatas` ──

def jpgMetaFrames : List JpgFrame :=
  [.jfifFrame jpgMetaJfifInch, .appFrame 1 jpgMetaHandcraftedApp1, .quantTableFrame []]

#guard (extractJpgMetadatas jpgMetaFrames).lookup .dpiX == some 72
#guard (extractJpgMetadatas jpgMetaFrames).lookup (.exif .model) == some (.string "Lean5".toUTF8)

-- Frames other than `.jfifFrame`/`.appFrame 1 _` contribute no metadata.
#guard (extractJpgMetadatas [.quantTableFrame []]).elems.isEmpty

#guard encodeJpgMetadatas .empty == []
#guard (encodeJpgMetadatas jpgMetaDpiMetas).length == 1
#guard (encodeJpgMetadatas (jpgMetaDpiMetas.union jpgMetaRoundTripMetas)).length == 2
