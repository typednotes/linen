import Linen.Codec.Picture.Tiff.Internal.Types
import Linen.Codec.Picture.Metadata

/-!
  Port of `Codec.Picture.Tiff.Internal.Metadata` from the `JuicyPixels`
  package (see `docs/imports/JuicyPixels/dependencies.md`, module 16 of 29).
  Converts between a decoded TIFF file's `ImageFileDirectory` list and this
  library's `Metadatas` store, in both directions: `extractTiffMetadata`
  (decode) and `encodeTiffStringMetadata` (encode).

  ## Scope: this module operates on already-resolved IFDs

  As documented in module 15 (`Linen.Codec.Picture.Tiff.Internal.Types`),
  `ImageFileDirectory.ifdExtended` is only ever populated by *this* module's
  IFD-independent decoder with the placeholder `ExifData.none`; resolving an
  entry's `ifdOffset` into its actual, possibly-out-of-line payload (a
  string, an array of longs/shorts, a nested Exif sub-IFD) requires seeking
  into the whole TIFF file's byte buffer, which needs the "whole file,
  absolute offset" framework only available once a top-level decode is
  underway. That framework — upstream's `fetchExtended` family — is module
  17's concern (`Codec.Picture.Tiff`), not this one's. Upstream's own
  `Metadata.hs` confirms this boundary: it is a pure `[ImageFileDirectory] ->
  Metadatas` (and the reverse) conversion with no `Get`/`Put`, no file
  buffer, and no seeking anywhere in it — every function here assumes each
  `ImageFileDirectory` it is handed already carries its fully-resolved value
  in `ifdExtended` (or, for values that fit inline, in `ifdOffset` itself).
  So this module is a straight, faithful port of upstream as-is: every
  function below takes a `List ImageFileDirectory` (or a `Metadatas`) and
  produces the other, doing no file I/O and no offset arithmetic beyond
  what upstream itself does (e.g. `unitOfIfd`'s reading of `ifdOffset` as an
  inline enum code, which is legitimately inline, not an out-of-line
  pointer).

  ## Design

  - Upstream imports `Codec.Picture.Metadata.Exif` directly and reuses its
    `ExifTag`/`ExifData` as the TIFF tag vocabulary (there is no separate
    `TiffTag` type) — this port does the same, working with
    `Linen.Codec.Picture.Metadata.Exif`'s `ExifTag`/`ExifData` (already
    open via `Linen.Codec.Picture.Tiff.Internal.Types`) throughout.
  - `typeOfData` is upstream's partial function (`error "Impossible"` on
    `ExifNone`/`ExifIFD`, since `makeIfd` never actually calls it on those
    constructors — every call site that reaches the generic `makeIfd t d`
    fallback only ever holds a `.rational`/`.signedRational` value in
    practice). Lean has no partial-function escape hatch consistent with
    this codebase's "no `sorry`, no `partial`" rule, so `typeOfData` is
    made total by mapping the two upstream-impossible constructors to
    `.undefined` (an arbitrary, clearly-labelled placeholder — no caller in
    this module or in practice reaches those branches, matching upstream's
    own claim that they are unreachable).
  - **`makeIfd`'s `ExifUndefined`/≤4-byte-count case has a genuine bug
    upstream**, which this port does not replicate: upstream computes each
    inlined byte's shift amount as `4 - 8 * ix` (`4`, `-4`, `-12`, `-20` for
    `ix = 0..3`), i.e. `unsafeShiftL` with a negative shift count — which
    the `Data.Bits` documentation for `unsafeShiftL` explicitly calls
    undefined behaviour (no particular result is promised, unlike `shiftL`,
    which would simply return `0`). Since there is no well-defined upstream
    semantics here to port faithfully, this is a case of the AGENTS.md
    "genuinely out of scope" carve-out rather than an abusive
    simplification: this port instead left-justifies the (at most four)
    bytes into the 32-bit slot MSB-first with the well-defined shifts
    `24, 16, 8, 0`, the same left-justification convention already
    documented on `ImageFileDirectory.ifdOffset` in module 15 and the same
    style `ExifShorts`' two-value case (both upstream's and this port's)
    already uses for its own combined 32-bit value.
  - **`makeIfd`'s inline `.short` encoding (`v <<< 16`) and
    `ifdToMetadata`'s inline `.orientation` decoding (`ifdOffset.toUInt16`,
    i.e. the *low* 16 bits, no shift) are not mirror images of each other —
    this asymmetry is upstream's own** (`makeIfd`'s hardcoded
    `unsafeShiftL 16` vs. the local `exifShort` helper's bare
    `fromIntegral`), most likely because upstream's actual top-level TIFF
    encoder (`Codec.Picture.Tiff.encodeTiff`) writes with `EndianLittle`
    while `Metadata.hs`'s `makeIfd` — which has no `Endianness` parameter at
    all — inlines as though for `EndianBig` (compare `Codec.Picture.Tiff`'s
    own endian-aware `ifdMultiShort`, which correctly branches on
    `Endianness` for this exact case). This module ports `makeIfd` and the
    decode side exactly as upstream wrote them, asymmetry included, rather
    than "fixing" a cross-module inconsistency that isn't this module's to
    fix.
  - Upstream's `Vector`/`ByteString` operations (`F.length`, `V.!`,
    `B.index`, `BC.length`, `BC.pack`) become `Array.size`/`Array.getD`/
    `ByteArray.get!`/`ByteArray.size`/the local `packLatin1`/`unpackLatin1`
    helpers (a Latin-1 `String` ↔ `ByteArray` pair mirroring
    `Linen.Data.ByteString.Char8`'s `pack`/`unpack`, but landing directly on
    plain `ByteArray` — the type `ExifData.string`/`ExifData.undefined`
    hold — rather than this library's slice-based `Data.ByteString`).
  - Upstream's `Data.List.sortBy (compare `on` word16OfTag . ifdIdentifier)`
    becomes `List.mergeSort` ordered on `ExifTag.toWord16`, matching this
    codebase's general "no bespoke sort helpers" convention.
  - `extractExifMetas`/`Met.singleton`/`Met.insert`/`Met.lookup`/`(<>)` map
    directly onto `Metadatas.extractExifMetas`/`Metadatas.singleton`/
    `Metadatas.insert`/`Metadatas.lookup`/`Metadatas.union`, already ported
    in module 6.
-/

namespace Codec.Picture

-- ── Latin-1 `String` ↔ `ByteArray` helpers ──

/-- Pack a `String` into a `ByteArray` (Latin-1 truncation), mirroring
    `Linen.Data.ByteString.Char8.pack`'s `c2w` but landing directly on the
    `ByteArray` `ExifData.string`/`ExifData.undefined` hold, rather than on
    this library's slice-based `Data.ByteString`. -/
private def packLatin1 (s : String) : ByteArray :=
  ByteArray.mk (s.toList.map (fun c => c.toNat.toUInt8)).toArray

/-- Unpack a `ByteArray` into a `String` (Latin-1 interpretation), the
    inverse of `packLatin1`. -/
private def unpackLatin1 (b : ByteArray) : String :=
  String.ofList (b.toList.map (fun w => Char.ofNat w.toNat))

-- ── The Exif sub-IFD pointer entry ──

/-- A bare `TagExifOffset` entry, used as a placeholder when building the
    Exif sub-IFD pointer entry during encoding. -/
def exifOffsetIfd : ImageFileDirectory :=
  { ifdIdentifier := .exifOffset
    ifdType := .long
    ifdCount := 1
    ifdOffset := 0
    ifdExtended := .none }

-- ── `ExifData` → `IfdType` ──

/-- The `IfdType` a resolved `ExifData` value should be written with. Upstream
    calls this a partial function (`error` on `ExifNone`/`ExifIFD`, since no
    call site ever reaches those branches — see the module doc-comment for
    why the placeholder chosen here, `.undefined`, is never actually
    observed). -/
def typeOfData : ExifData → IfdType
  | .none => .undefined
  | .ifd _ => .undefined
  | .long _ => .long
  | .longs _ => .long
  | .short _ => .short
  | .shorts _ => .short
  | .string _ => .ascii
  | .undefined _ => .undefined
  | .rational .. => .rational
  | .signedRational .. => .signedRational

-- ── `ExifTag × ExifData` → `ImageFileDirectory` ──

/-- Build an `ImageFileDirectory` entry for a resolved `(tag, value)` pair,
    inlining the value into `ifdOffset` when it fits in 4 bytes and stashing
    it in `ifdExtended` otherwise (see the module doc-comment for the one
    place this deviates from upstream — the ≤4-byte `.undefined` case). -/
def makeIfd (t : ExifTag) (d : ExifData) : ImageFileDirectory :=
  match d with
  | .short v =>
      { ifdIdentifier := t, ifdType := .short, ifdCount := 1
        ifdOffset := v.toUInt32 <<< 16, ifdExtended := .none }
  | .long v =>
      { ifdIdentifier := t, ifdType := .long, ifdCount := 1
        ifdOffset := v, ifdExtended := .none }
  | .shorts v =>
      let size := v.size
      if size == 2 then
        let shortAt (i : Nat) : UInt32 := (v.getD i 0).toUInt32
        { ifdIdentifier := t, ifdType := .short, ifdCount := 2
          ifdOffset := (shortAt 0 <<< 16) ||| shortAt 1, ifdExtended := .none }
      else
        { ifdIdentifier := t, ifdType := .short, ifdCount := size.toUInt32
          ifdOffset := 0, ifdExtended := d }
  | .longs v =>
      let size := v.size
      if size == 1 then
        { ifdIdentifier := t, ifdType := .long, ifdCount := 1
          ifdOffset := v.getD 0 0, ifdExtended := .none }
      else
        { ifdIdentifier := t, ifdType := .long, ifdCount := size.toUInt32
          ifdOffset := 0, ifdExtended := d }
  | .string str =>
      { ifdIdentifier := t, ifdType := .ascii, ifdCount := str.size.toUInt32
        ifdOffset := 0, ifdExtended := d }
  | .undefined str =>
      let size := str.size
      if size > 4 then
        { ifdIdentifier := t, ifdType := .undefined, ifdCount := size.toUInt32
          ifdOffset := 0, ifdExtended := d }
      else
        let byteAt (i : Nat) (shift : UInt32) : UInt32 :=
          if i < size then (str.get! i).toUInt32 <<< shift else 0
        let ofs := byteAt 0 24 ||| byteAt 1 16 ||| byteAt 2 8 ||| byteAt 3 0
        { ifdIdentifier := t, ifdType := .undefined, ifdCount := size.toUInt32
          ifdOffset := ofs, ifdExtended := .none }
  | _ =>
      { ifdIdentifier := t, ifdType := typeOfData d, ifdCount := 1
        ifdOffset := 0, ifdExtended := d }

-- ── `Metadatas` → `[ImageFileDirectory]` (encoding) ──

/-- Build one text-tag `ImageFileDirectory`, if `metas` holds a value for
    `key`. -/
private def keyStr (tag : ExifTag) (key : Keys String) (metas : Metadatas) :
    List ImageFileDirectory :=
  match metas.lookup key with
  | none => []
  | some v => [makeIfd tag (.string (packLatin1 v))]

/-- Build the `ImageFileDirectory` list `metas` encodes to for TIFF's
    string-valued well-known tags (copyright/artist/document-name/
    description/software) plus every `Metadatas.exif` entry, sorted by tag
    code (upstream's `sortBy (compare `on` word16OfTag . ifdIdentifier)`). -/
def encodeTiffStringMetadata (metas : Metadatas) : List ImageFileDirectory :=
  let copyright := keyStr .copyright .copyright metas
  let artist := keyStr .artist .author metas
  let title := keyStr .documentName .title metas
  let description := keyStr .imageDescription .description metas
  let software := keyStr .software .software metas
  let allPureExif := metas.extractExifMetas.map (fun (t, d) => makeIfd t d)
  let allTags := copyright ++ artist ++ title ++ description ++ software ++ allPureExif
  allTags.mergeSort (fun a b => a.ifdIdentifier.toWord16 <= b.ifdIdentifier.toWord16)

-- ── `[ImageFileDirectory]` → `Metadatas` (decoding) ──

/-- Insert a string-valued `Keys` entry decoded from an `ExifData.string`
    payload. -/
private def strMeta (k : Keys String) (v : ByteArray) : Metadatas :=
  Metadatas.singleton k (unpackLatin1 v)

/-- Decode a single `ImageFileDirectory` entry into whatever `Metadatas`
    it contributes (`Metadatas.empty` for tags with no metadata
    counterpart), following upstream's `go` helper of
    `extractTiffStringMetadata`. -/
private def ifdToMetadata (ifd : ImageFileDirectory) : Metadatas :=
  match ifd.ifdIdentifier, ifd.ifdExtended with
  | .artist, .string v => strMeta .author v
  | .bitsPerSample, _ => .empty
  | .colorMap, _ => .empty
  | .compression, _ => .empty
  | .copyright, .string v => strMeta .copyright v
  | .documentName, .string v => strMeta .title v
  | .exifOffset, .ifd lst =>
      lst.foldl (fun acc (k, v) => acc.insert (.exif k) v) .empty
  | .imageDescription, .string v => strMeta .description v
  | .imageLength, _ => Metadatas.singleton .height ifd.ifdOffset.toNat
  | .imageWidth, _ => Metadatas.singleton .width ifd.ifdOffset.toNat
  | .jpegACTables, _ => .empty
  | .jpegDCTables, _ => .empty
  | .jpegInterchangeFormat, _ => .empty
  | .jpegInterchangeFormatLength, _ => .empty
  | .jpegLosslessPredictors, _ => .empty
  | .jpegPointTransforms, _ => .empty
  | .jpegQTables, _ => .empty
  | .jpegRestartInterval, _ => .empty
  | .jpegProc, _ => .empty
  | .model, v => Metadatas.singleton (.exif .model) v
  | .make, v => Metadatas.singleton (.exif .make) v
  | .orientation, _ =>
      Metadatas.singleton (.exif .orientation) (.short ifd.ifdOffset.toUInt16)
  | .resolutionUnit, _ => .empty
  | .rowPerStrip, _ => .empty
  | .samplesPerPixel, _ => .empty
  | .software, .string v => strMeta .software v
  | .stripByteCounts, _ => .empty
  | .stripOffsets, _ => .empty
  | .tileByteCount, _ => .empty
  | .tileLength, _ => .empty
  | .tileOffset, _ => .empty
  | .tileWidth, _ => .empty
  | .unknown _, _ => Metadatas.singleton (.exif ifd.ifdIdentifier) ifd.ifdExtended
  | .xResolution, _ => .empty
  | .yCbCrCoeff, _ => .empty
  | .yCbCrPositioning, _ => .empty
  | .yCbCrSubsampling, _ => .empty
  | .yResolution, _ => .empty
  | _, _ => .empty

/-- Decode every `ImageFileDirectory`'s string/tag metadata, tagging the
    result as originating from a TIFF file. -/
def extractTiffStringMetadata (ifds : List ImageFileDirectory) : Metadatas :=
  (ifds.foldl (fun acc ifd => acc.union (ifdToMetadata ifd)) Metadatas.empty).insert .format .tiff

/-- Does this entry carry the given tag? -/
private def byTag (t : ExifTag) (ifd : ImageFileDirectory) : Bool :=
  ifd.ifdIdentifier == t

/-- The unit a `TagResolutionUnit` entry's inline (`ifdOffset`) value
    declares. -/
private inductive TiffResolutionUnit where
  | unknown
  | inch
  | centimeter

/-- Decode a `TagResolutionUnit` entry's inline enum code. -/
private def unitOfIfd (ifd : ImageFileDirectory) : TiffResolutionUnit :=
  match ifd.ifdType, ifd.ifdOffset with
  | .short, 1 => .unknown
  | .short, 2 => .inch
  | .short, 3 => .centimeter
  | _, _ => .unknown

/-- Decode `TagXResolution`/`TagYResolution` DPI metadata, honouring
    `TagResolutionUnit`'s declared unit (`.unknown` contributes no metadata,
    matching upstream). -/
def extractTiffDpiMetadata (ifds : List ImageFileDirectory) : Metadatas :=
  let findDpi (k : Keys Nat) (tag : ExifTag) (toDpi : Nat → Nat) (metas : Metadatas) :
      Metadatas :=
    match ifds.find? (byTag tag) with
    | some { ifdExtended := .rational num den, .. } =>
        metas.insert k (toDpi (num / den).toNat)
    | _ => metas
  match ifds.find? (byTag .resolutionUnit) with
  | none => .empty
  | some ifd =>
      match unitOfIfd ifd with
      | .unknown => .empty
      | .centimeter =>
          findDpi .dpiY .yResolution dotsPerCentiMeterToDotPerInch
            (findDpi .dpiX .xResolution dotsPerCentiMeterToDotPerInch .empty)
      | .inch =>
          findDpi .dpiY .yResolution id (findDpi .dpiX .xResolution id .empty)

/-- Decode every DPI and string/tag metadata a TIFF file's `ImageFileDirectory`
    list carries. -/
def extractTiffMetadata (ifds : List ImageFileDirectory) : Metadatas :=
  (extractTiffDpiMetadata ifds).union (extractTiffStringMetadata ifds)

end Codec.Picture
