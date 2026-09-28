/-
  Tests for `Linen.Codec.Picture.Metadata` — checks type-safe lookup/insert/
  delete, `union`, `extractExifMetas`, the DPI conversion helpers, and the
  metadata-set builders.
-/
import Linen.Codec.Picture.Metadata

open Codec.Picture

-- ── Lookup / insert / delete ──

#guard Metadatas.empty.lookup Keys.width == none

#guard (Metadatas.empty.insert .width 640).lookup .width == some 640

-- lookup with a different key than what's stored finds nothing
#guard (Metadatas.empty.insert .width 640).lookup .height == none

-- inserting the same key twice overwrites, rather than accumulating
#guard ((Metadatas.empty.insert .width 640).insert .width 800).lookup .width == some 800
#guard ((Metadatas.empty.insert .width 640).insert .width 800).elems.length == 1

#guard ((Metadatas.empty.insert .width 640).delete .width).lookup .width == none

-- `exif`/`unknown` keys carry their own payload as part of the key identity
#guard (Metadatas.empty.insert (.exif .orientation) (ExifData.short 1)).lookup (.exif .orientation)
    == some (ExifData.short 1)
#guard (Metadatas.empty.insert (.exif .orientation) (ExifData.short 1)).lookup (.exif .flash) == none

#guard (Metadatas.empty.insert (.unknown "foo") (Value.int 1)).lookup (.unknown "bar") == none

#guard (Metadatas.singleton .title "hello").lookup .title == some "hello"

-- ── Union ──

-- a key present in both operands takes the second (right-hand) operand's value
#guard ((Metadatas.singleton .width 640).union (Metadatas.singleton .width 800)).lookup .width
    == some 800

-- keys unique to either side are preserved
#guard (((Metadatas.singleton .width 640).union (Metadatas.singleton .height 480)).lookup .width,
        ((Metadatas.singleton .width 640).union (Metadatas.singleton .height 480)).lookup .height)
    == (some 640, some 480)

-- ── Exif extraction ──

#guard ((Metadatas.empty.insert (.exif .orientation) (ExifData.short 1)).insert .width 640).extractExifMetas
    == [(ExifTag.orientation, ExifData.short 1)]

#guard Metadatas.empty.extractExifMetas == []

-- ── DPI conversions ──

#guard dotsPerMeterToDotPerInch 3937 == 99
#guard dotPerInchToDotsPerMeter 96 == 3779
#guard dotsPerCentiMeterToDotPerInch 100 == 254

-- ── Metadata-set builders ──

#guard (mkDpiMetadata 96).lookup .dpiX == some 96
#guard (mkDpiMetadata 96).lookup .dpiY == some 96

#guard (mkSizeMetadata 640 480).lookup .width == some 640
#guard (mkSizeMetadata 640 480).lookup .height == some 480

#guard (basicMetadata .png 640 480).lookup .format == some SourceFormat.png
#guard (basicMetadata .png 640 480).lookup .width == some 640

#guard (simpleMetadata .jpeg 640 480 72 72).lookup .dpiX == some 72
#guard (simpleMetadata .jpeg 640 480 72 72).lookup .format == some SourceFormat.jpeg
