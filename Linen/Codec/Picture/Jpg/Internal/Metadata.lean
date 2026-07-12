import Linen.Codec.Picture.Jpg.Internal.Types
import Linen.Codec.Picture.Tiff.Internal.Types
import Linen.Codec.Picture.Tiff.Internal.Metadata
import Linen.Codec.Picture.Tiff

/-!
  Port of `Codec.Picture.Jpg.Internal.Metadata` from the `JuicyPixels`
  package (see `docs/imports/JuicyPixels/dependencies.md`, module 25 of 29).
  Converts between a JPEG file's `JFIF APP0` segment and this library's
  `Metadatas` store (DPI), in both directions, plus the Exif metadata
  actually carried in an `APP1` segment.

  ## Scope: this module also covers the Exif that module 22 deferred here

  Upstream's real `Codec.Picture.Jpg.Internal.Metadata.hs` (fetched from
  `Twinside/Juicy.Pixels`) is tiny: it only converts a `JpgJFIFApp0` to/from
  `Metadatas`' `DpiX`/`DpiY`. The *Exif* side of a JPEG file — recognising an
  `APP1` segment carrying the fixed `"Exif\0\0"` marker followed by a
  TIFF-structured byte stream, and turning that into `Metadatas` — is split
  across two other upstream modules instead: `Jpg.Internal.Types.hs` (its
  `parseExif`/`putExif`, which decode/encode the raw segment bytes into/from
  `[ImageFileDirectory]`) and the top-level `Jpg.hs` (which calls
  `Tiff.Internal.Metadata.extractTiffMetadata`/`encodeTiffStringMetadata` on
  that list). But `Linen.Codec.Picture.Jpg.Internal.Types` (module 22 of this
  port) documented dropping `JpgExif`/`parseExif`/`putExif` entirely,
  deferring "decoding a real Exif blob" to *this* module — every `APP1`
  segment there is preserved uninterpreted via the generic `appFrame`
  constructor. So this module's job is precisely the union of upstream's
  `Jpg.Internal.Metadata.hs` plus the Exif-specific slice of
  `Jpg.Internal.Types.hs`'s `parseExif`/`putExif` that module 22 pushed here.

  - **Reuse, not re-implementation, of Exif-IFD parsing.** Module 5
    (`Linen.Codec.Picture.Metadata.Exif`) supplies only the `ExifTag`/
    `ExifData` *vocabulary*, no byte-level parser (it never did upstream
    either — `Codec.Picture.Metadata.Exif.hs` has no `Binary` instances).
    The actual TIFF-structured IFD-chain walk this module needs already
    exists, faithfully ported, in `Linen.Codec.Picture.Tiff` (module 17)'s
    `parseIfdChain`/`resolveIfdExtended`, and the IFD-list ↔ `Metadatas`
    conversion already exists in `Linen.Codec.Picture.Tiff.Internal.Metadata`
    (module 16)'s `extractTiffMetadata`/`encodeTiffStringMetadata`. An
    `APP1` Exif payload, once its `"Exif\0\0"` marker is stripped, *is* a
    self-contained TIFF file (its own header, its own absolute byte
    offsets) — exactly the "whole file, absolute offset" context
    `parseIfdChain` needs — so this module hands it directly to that
    existing machinery rather than re-parsing IFDs from scratch. This
    reaches past this port's own stated module-5/6 dependency list in
    `docs/imports/JuicyPixels/dependencies.md` (which predates this design
    decision), but follows the AGENTS.md reuse-first precedent to the
    letter: don't port a second copy of something the library already has.
  - **Inherited limitation: Exif sub-IFDs are not resolved.** Module 17's own
    `parseIfdChain` never re-enters IFD parsing for a `TagExifOffset` entry
    (its own doc-comment defers that recursive-dereference case entirely);
    this module inherits that limitation unchanged rather than re-deciding
    it. In practice this only drops Exif tags that live *exclusively* in the
    Exif sub-IFD (e.g. `ExposureTime`) — every tag `ExifTag` (module 5) can
    actually name that a JPEG's Exif blob commonly puts in the *primary* IFD
    (`Make`, `Model`, `Orientation`, `Software`, `DateTime`, the two
    resolution tags, …) is still extracted, via `extractTiffMetadata`'s own
    handling of the first IFD in the chain.
  - **Encode side: a faithful, from-scratch port of upstream's `putExif` and
    its `setupIfdOffsets`/`orderIfdByTag` helpers.** These live in
    `Jpg.Internal.Types.hs` upstream (not `Jpg.Internal.Metadata.hs`) and
    were dropped by module 22 for the same reason as `parseExif`; porting
    them here (rather than reaching into `Linen.Codec.Picture.Tiff`'s
    *private* `layoutIfdExtended`/`serializeExtended`/`extendedByteSize`,
    which solve a similar but not identical problem — a single self
    contained image file, not a two-block IFD0-plus-Exif-sub-IFD chain) is a
    direct, small (~20-line) re-derivation of upstream's own
    `setupIfdOffsets`, not a duplication of a whole ported component.
    `setupIfdOffsets`'s own quirk is ported byte-for-byte: a bare
    `.rational`/`.signedRational` entry's *extended payload* is still
    written to the stream (upstream's `dump`/this module's
    `serializeExtended` writes every entry's `ifdExtended` unconditionally),
    but the accumulator that assigns `ifdOffset` values is never advanced
    past it (upstream's `updater`'s catch-all case) — so such an entry's own
    `ifdOffset` (and any later entry's, if one needs out-of-line storage)
    would not correctly point at it. This is a genuine, deterministic
    upstream quirk (not undefined behaviour, unlike the negative-shift case
    module 16 already declined to replicate), reproduced here as upstream
    wrote it; `encodeTiffStringMetadata`'s own callers in practice never
    hand it a bare `.rational`/`.signedRational` `Metadatas.exif` entry
    (this library's own Exif decode side, `extractTiffMetadata`, never
    *produces* one — its `xResolution`/`yResolution` branches are `.empty`),
    so this is not exercised by this module's own round trip.
  - `scalerOfUnit` (upstream's local helper inside `extractMetadatas`) is
    ported directly: a `JFifUnit`-tagged DPI value converts into DPI, one
    axis at a time, `.unitUnknown` contributing nothing.
-/

namespace Codec.Picture.Jpg.Internal

open Codec.Picture (Metadatas Keys ExifTag ExifData TiffHeader TiffEndianness
  ImageFileDirectory parseTiffHeader putTiffHeader parseIfdChain extractTiffMetadata
  encodeTiffStringMetadata exifOffsetIfd putImageFileDirectoryList putU16 putU32
  padOddLength dotsPerCentiMeterToDotPerInch)
open Data.ByteString (Builder)

-- ── Byte helpers ──

/-- Materialise a `Builder`'s output into a plain `ByteArray` (mirrors
    `Linen.Codec.Picture.Png.Internal.Metadata`'s `bytesOfBuilder`). -/
private def bytesOfBuilder (b : Builder) : ByteArray :=
  (Data.ByteString.copy b.toStrictByteString).data

/-- Build a `Builder` that writes out a list of bytes verbatim. -/
private def builderOfBytes (bs : List UInt8) : Builder :=
  bs.foldl (fun acc b => acc ++ Builder.word8 b) Builder.empty

/-- The fixed 6-byte marker (`"Exif\0\0"`) prefixing an `APP1` segment's
    payload whenever it carries Exif metadata (upstream's `exifHeader`). -/
private def exifMarker : List UInt8 := [0x45, 0x78, 0x69, 0x66, 0x00, 0x00]

-- ── JFIF (`APP0`) ↔ DPI metadata ──

/-- Insert one axis' DPI value, honouring the JFIF segment's declared unit
    (upstream's local `scalerOfUnit`/`inserter`). -/
private def scalerOfUnit (unit : JFifUnit) (k : Keys Nat) (v : UInt16) (m : Metadatas) :
    Metadatas :=
  match unit with
  | .unitUnknown => m
  | .dotsPerInch => m.insert k v.toNat
  | .dotsPerCentimeter => m.insert k (dotsPerCentiMeterToDotPerInch v.toNat)

/-- Extract DPI metadata from a `JFIF APP0` segment (upstream's
    `extractMetadatas`). -/
def extractJFIFMetadatas (jfif : JpgJFIFApp0) : Metadatas :=
  scalerOfUnit jfif.unit .dpiY jfif.dpiY (scalerOfUnit jfif.unit .dpiX jfif.dpiX Metadatas.empty)

/-- Build a `JFIF APP0` frame carrying `metas`' `dpiX`/`dpiY`, if both are
    present (upstream's `encodeMetadatas`). -/
def encodeJFIFMetadatas (metas : Metadatas) : List JpgFrame :=
  match metas.lookup .dpiX, metas.lookup .dpiY with
  | some dx, some dy => [.jfifFrame { unit := .dotsPerInch, dpiX := dx.toUInt16, dpiY := dy.toUInt16 }]
  | _, _ => []

-- ── `APP1` Exif ↔ `Metadatas` (decode) ──

/-- Strip the `"Exif\0\0"` marker from an `APP1` segment's raw payload and
    parse the embedded TIFF-structured Exif blob's first IFD, reusing
    `Linen.Codec.Picture.Tiff`'s `parseIfdChain` for the offset-resolving
    walk (see the module doc-comment). Returns `none` if the payload isn't
    Exif-marked, isn't a valid TIFF blob, or has an empty IFD chain
    (upstream's `parseExif`, minus the `JpgExif` wrapper module 22 dropped). -/
def parseExifApp1 (raw : ByteArray) : Option (List ImageFileDirectory) :=
  let bytes := raw.toList
  if !(exifMarker.isPrefixOf bytes) then none
  else
    let tiffBytes := bytes.drop exifMarker.length
    match parseTiffHeader tiffBytes with
    | .error _ => none
    | .ok (header, _) =>
        match parseIfdChain header.endianness (ByteArray.mk tiffBytes.toArray) header.offset with
        | .error _ => none
        | .ok [] => none
        | .ok (ifds :: _) => some ifds

/-- Extract Exif metadata from an `APP1` segment's raw payload, contributing
    no metadata if it doesn't actually carry an Exif blob (upstream's
    `foldMap extractTiffMetadata $ app1ExifMarker st`, folded over the
    `Option` produced by `parseExifApp1` directly). -/
def extractApp1ExifMetadatas (raw : ByteArray) : Metadatas :=
  match parseExifApp1 raw with
  | none => Metadatas.empty
  | some ifds => extractTiffMetadata ifds

/-- Extract every JFIF-DPI and Exif metadata a JPEG file's frame list
    carries. -/
def extractJpgMetadatas (frames : List JpgFrame) : Metadatas :=
  frames.foldl
    (fun acc f =>
      match f with
      | .jfifFrame jfif => acc.union (extractJFIFMetadatas jfif)
      | .appFrame 1 raw => acc.union (extractApp1ExifMetadatas raw)
      | _ => acc)
    Metadatas.empty

-- ── `Metadatas` → `APP1` Exif (encode) ──

/-- Sort an IFD entry list by tag code (upstream's `orderIfdByTag`; "all the
    IFD must be written in order according to the tag value"). -/
private def orderIfdByTag (ifds : List ImageFileDirectory) : List ImageFileDirectory :=
  ifds.mergeSort (fun a b => a.ifdIdentifier.toWord16 <= b.ifdIdentifier.toWord16)

/-- An out-of-line byte blob's padded size (TIFF requires every field to
    start on a 2-byte boundary; upstream's local `paddedSize`). -/
private def paddedSize (b : ByteArray) : UInt32 :=
  (b.size + (if b.size % 2 == 1 then 1 else 0)).toUInt32

/-- Assign one entry its `ifdOffset` given the running "next free
    out-of-line byte" cursor `ix`, and advance `ix` past whatever out-of-line
    data that entry needs (upstream's local `updater`, including its
    catch-all no-op case for entries whose `ifdExtended` needs no
    bookkeeping here — see the module doc-comment for the one upstream quirk
    this reproduces byte-for-byte). -/
private def ifdOffsetUpdater (ix : UInt32) (ifd : ImageFileDirectory) :
    UInt32 × ImageFileDirectory :=
  if ifd.ifdIdentifier == .exifOffset then
    (ix, { ifd with ifdOffset := ix })
  else
    match ifd.ifdExtended with
    | .undefined b => (ix + paddedSize b, { ifd with ifdOffset := ix })
    | .string b => (ix + paddedSize b, { ifd with ifdOffset := ix })
    | .longs v => if v.size > 1 then (ix + (v.size * 4).toUInt32, { ifd with ifdOffset := ix }) else (ix, ifd)
    | .shorts v => if v.size > 2 then (ix + (v.size * 2).toUInt32, { ifd with ifdOffset := ix }) else (ix, ifd)
    | _ => (ix, ifd)

/-- Fold `ifdOffsetUpdater` across one IFD block, in entry order. Structural
    recursion on the entry list. -/
private def ifdOffsetUpdaterAux (ix : UInt32) :
    List ImageFileDirectory → UInt32 × List ImageFileDirectory
  | [] => (ix, [])
  | ifd :: rest =>
      let (ix1, ifd1) := ifdOffsetUpdater ix ifd
      let (ixFinal, restOut) := ifdOffsetUpdaterAux ix1 rest
      (ixFinal, ifd1 :: restOut)

/-- Patch every entry of one IFD block with its out-of-line `ifdOffset`,
    given `initialOffset` (the file offset this block itself starts at).
    Returns the "next free" offset right after this block's own header and
    out-of-line data (upstream's `setupIfdOffsets`). -/
private def setupIfdOffsets (initialOffset : UInt32) (lst : List ImageFileDirectory) :
    UInt32 × List ImageFileDirectory :=
  let startExtended := initialOffset + (lst.length * 12 + 2 + 4).toUInt32
  ifdOffsetUpdaterAux startExtended lst

/-- Lay out every IFD block in file order, threading each block's start
    offset from the previous block's end (upstream's outer `mapAccumL`).
    Structural recursion on the block list. -/
private def layoutBlocks : UInt32 → List (List ImageFileDirectory) → List (List ImageFileDirectory)
  | _, [] => []
  | ix, b :: bs =>
      let (ixNext, b') := setupIfdOffsets ix b
      b' :: layoutBlocks ixNext bs

/-- Serialise one entry's out-of-line `ExifData` payload, honouring
    `endian` (upstream's local `dump`, called unconditionally for every
    entry: `.none`/`.short`/`.long`/`.ifd` contribute no bytes here, since
    their value already lives inline in `ifdOffset`). -/
private def serializeExtended (endian : TiffEndianness) : ExifData → Builder
  | .string b => padOddLength b
  | .undefined b => padOddLength b
  | .shorts v => v.foldl (fun acc s => acc ++ putU16 endian s) Builder.empty
  | .longs v => v.foldl (fun acc s => acc ++ putU32 endian s) Builder.empty
  | .rational num den => putU32 endian num ++ putU32 endian den
  | .signedRational num den => putU32 endian num ++ putU32 endian den
  | _ => Builder.empty

/-- Build an `APP1` frame carrying a self-contained, big-endian Exif TIFF
    blob from `metas`, reusing `Linen.Codec.Picture.Tiff.Internal.Metadata`'s
    `encodeTiffStringMetadata` (module 16) for the well-known string tags
    plus every `Metadatas.exif` entry (upstream's `putExif`, ported here per
    the module doc-comment). Entries belonging to the primary IFD
    (`ExifTag.isInIFD0`) are written first, followed — if any Exif-only
    entries remain — by a second IFD block holding them, pointed to by a
    `TagExifOffset` entry appended to the first block. Produces no frame at
    all if there is nothing to encode. -/
def encodeApp1ExifMetadatas (metas : Metadatas) : List JpgFrame :=
  match encodeTiffStringMetadata metas with
  | [] => []
  | allIfds =>
      let headerSize : UInt32 := 8
      let (ifd0, ifdExif) := allIfds.partition (fun ifd => ifd.ifdIdentifier.isInIFD0)
      let blocks : List (List ImageFileDirectory) :=
        if ifdExif.isEmpty then [orderIfdByTag ifd0]
        else [orderIfdByTag (ifd0 ++ [exifOffsetIfd]), orderIfdByTag ifdExif]
      let laidOut := layoutBlocks headerSize blocks
      let ifdBuilder :=
        laidOut.foldl (fun acc b => acc ++ putImageFileDirectoryList .big b 0) Builder.empty
      let extBuilder :=
        laidOut.foldl
          (fun acc b => acc ++ b.foldl (fun acc2 e => acc2 ++ serializeExtended .big e.ifdExtended) Builder.empty)
          Builder.empty
      let header : TiffHeader := { endianness := .big, offset := headerSize }
      let full := builderOfBytes exifMarker ++ putTiffHeader header ++ ifdBuilder ++ extBuilder
      [.appFrame 1 (bytesOfBuilder full)]

/-- Build every `APP0`/`APP1` frame that `metas` encodes to. -/
def encodeJpgMetadatas (metas : Metadatas) : List JpgFrame :=
  encodeJFIFMetadatas metas ++ encodeApp1ExifMetadatas metas

end Codec.Picture.Jpg.Internal
