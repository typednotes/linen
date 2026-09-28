/-
  Tests for `Linen.Codec.Picture.Metadata.Exif` — checks the tag ↔ `Word16`
  round trip, `isInIFD0`, and `ExifData` construction.
-/
import Linen.Codec.Picture.Metadata.Exif

open Codec.Picture

#guard ExifTag.ofWord16 256 == ExifTag.imageWidth
#guard ExifTag.imageWidth.toWord16 == 256
#guard ExifTag.ofWord16 34665 == ExifTag.exifOffset
#guard ExifTag.ofWord16 9999 == ExifTag.unknown 9999
#guard (ExifTag.unknown 9999).toWord16 == 9999

-- round-trip on every named tag
#guard ExifTag.ofWord16 ExifTag.copyright.toWord16 == ExifTag.copyright
#guard ExifTag.ofWord16 ExifTag.gpsInfo.toWord16 == ExifTag.gpsInfo

-- `isInIFD0`: ordinary tags below `copyright`'s tag number are in IFD0
#guard ExifTag.imageWidth.isInIFD0
-- the two redirect tags are also considered part of IFD0
#guard ExifTag.exifOffset.isInIFD0
#guard ExifTag.gpsInfo.isInIFD0
-- a tag with a larger numeric value that isn't a redirect is not
#guard !ExifTag.lightSource.isInIFD0

#guard ExifData.long 42 == ExifData.long 42
#guard ExifData.long 42 != ExifData.long 43
#guard ExifData.ifd [(ExifTag.orientation, ExifData.short 1)]
    == ExifData.ifd [(ExifTag.orientation, ExifData.short 1)]
