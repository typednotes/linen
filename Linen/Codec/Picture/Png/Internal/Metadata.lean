import Linen.Codec.Picture.Png.Internal.Type
import Linen.Codec.Picture.Metadata
import Linen.Data.ByteString.Builder
import Linen.Data.ByteString.Char8

/-!
  Port of `Codec.Picture.Png.Internal.Metadata` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 12 of 29). Converts
  between `PngRawImage`'s ancillary chunks (`pHYs`, `gAMA`, `tEXt`) and this
  library's `Metadatas` store (`Linen.Codec.Picture.Metadata`), in both
  directions: `extractMetadatas` (decode) and `encodeMetadatas` (encode).

  ## Design and scope

  - **`zTXt`/`iTXt` decompressed-content extraction is out of scope for this
    module.** Upstream's `getZTexts` decompresses each `zTXt` chunk's payload
    with `Codec.Compression.Zlib.decompress` before splitting it into a
    keyword/text pair (and upstream never handles `iTXt` at all — it is not
    in the exported surface of `Codec.Picture.Png.Internal.Metadata.hs`
    either). This library's PNG codec has not yet wired up zlib inflate at
    the PNG layer — that integration is module 14 of 29
    (`Linen.Codec.Picture.Png`, "on #11–#13 plus zlib inflate/deflate" per
    `docs/imports/JuicyPixels/dependencies.md`). Wiring `zTXt` decompression
    in prematurely here, ahead of that integration, would mean duplicating
    (and then having to reconcile) the zlib-adapter decision module 14 is
    responsible for. So this port only extracts `pHYs` (DPI), `gAMA`
    (gamma), and `tEXt` (uncompressed text) metadata; `zTXt` chunks are
    simply not looked at by `extractMetadatas`, and `encodeMetadatas` never
    emits one (matching upstream's own encode side, which also never
    produces a `zTXt` chunk — `encodeSingleMetadata` only ever builds `tEXt`
    chunks via upstream's local `txt` helper). A later module revisiting PNG
    text metadata once zlib inflate is available should extend
    `extractMetadatas` to also fold over `zTXtSignature` chunks.

  - Upstream's `PngText`/`PngZText` (`Binary` instances encoding a
    NUL-terminated keyword followed by the remaining payload bytes) are not
    ported as separate structures: only the uncompressed `tEXt` shape is
    needed here, so `parsePngText`/`mkTextChunk` below inline that
    keyword/data split and re-join directly rather than introducing a
    single-use record type.

  - Upstream's `aToMetadata` (shared between `textToMetadata` and
    `ztxtToMetadata`) becomes `metadataOfKeyword`, matching a `tEXt` chunk's
    keyword against the same fixed set of well-known keys already present in
    `Linen.Codec.Picture.Metadata`'s `Keys` inductive
    (`title`/`author`/`description`/`copyright`/`software`/`disclaimer`/
    `warning`/`source`/`comment`) — no new metadata keys are needed, since
    every keyword upstream recognises already has a matching constructor.
    Anything else falls back to `Keys.unknown`/`Value.string`, exactly as
    upstream's `Met.Unknown` / `Met.String` case does.

  - PNG chunk payloads are Latin-1 text (upstream reads/writes them as
    `Data.ByteString.Lazy.Char8`, i.e. one byte per character with no
    multi-byte decoding), so keyword/text bytes are converted via
    `Linen.Data.ByteString.Char8`'s `pack`/`unpack` (already the library's
    established Latin-1 byte-string ↔ `String` convention), not UTF-8.

  - `bytesOfBuilder` (materialising a `Builder`'s strict `Data.ByteString`
    into a plain `ByteArray` for `mkRawChunk`) follows the same pattern as
    the private `toByteArray` helper in `Linen.Data.PDF.Core.Encryption`
    (`Data.ByteString.copy` then `.data`) — renamed to avoid clashing with
    `Codec.Picture.toByteArray` (`Array UInt8 → ByteArray`, from
    `Linen.Codec.Picture.VectorByteConversion`), which is already open in
    this namespace.
-/

namespace Codec.Picture

open Data.ByteString (Builder)

-- ── Byte/string conversion helpers ──

/-- Materialise a `Builder`'s output into a plain `ByteArray`, for
    `mkRawChunk`'s `data` argument. -/
private def bytesOfBuilder (b : Builder) : ByteArray :=
  (Data.ByteString.copy b.toStrictByteString).data

-- ── Decoding: chunk payloads → `Metadatas` ──

/-- Split a `tEXt` chunk's payload at its first NUL byte (`0`) into the
    keyword and the following text data. -/
private def splitAtNul : List UInt8 → Option (List UInt8 × List UInt8)
  | [] => none
  | (0 : UInt8) :: rest => some ([], rest)
  | c :: rest =>
      match splitAtNul rest with
      | none => none
      | some (k, v) => some (c :: k, v)

/-- Parse a `tEXt` chunk's raw payload into its (Latin-1) keyword and text. -/
private def parsePngText (bytes : List UInt8) : Except String (String × String) :=
  match splitAtNul bytes with
  | none => .error "Invalid tEXt chunk: missing NUL separator"
  | some (kw, dat) =>
      .ok (Data.ByteString.Char8.unpack (Data.ByteString.pack kw),
           Data.ByteString.Char8.unpack (Data.ByteString.pack dat))

/-- Map a `tEXt` chunk's keyword/text pair onto the matching well-known
    `Keys` entry, falling back to `Keys.unknown`/`Value.string`. -/
private def metadataOfKeyword (keyword text : String) : Metadatas :=
  match keyword with
  | "Title" => Metadatas.singleton .title text
  | "Author" => Metadatas.singleton .author text
  | "Description" => Metadatas.singleton .description text
  | "Copyright" => Metadatas.singleton .copyright text
  | "Software" => Metadatas.singleton .software text
  | "Disclaimer" => Metadatas.singleton .disclaimer text
  | "Warning" => Metadatas.singleton .warning text
  | "Source" => Metadatas.singleton .source text
  | "Comment" => Metadatas.singleton .comment text
  | other => Metadatas.singleton (.unknown other) (.string text)

/-- Extract gamma metadata from the first `gAMA` chunk, if any. -/
private def getGamma : List ByteArray → Metadatas
  | [] => Metadatas.empty
  | data :: _ =>
      match parsePngGamma data.toList with
      | .error _ => Metadatas.empty
      | .ok (g, _) => Metadatas.singleton .gamma g.value

/-- Extract DPI metadata from the first `pHYs` chunk, if any. Upstream
    defaults to `72` DPI on both axes when the chunk's unit is
    `PngUnitUnknown` (pixel density in an unspecified unit conveys no real
    resolution, so the arbitrary de-facto standard default is used instead),
    and otherwise converts `pHYs`'s dots-per-meter fields via
    `dotsPerMeterToDotPerInch`. -/
private def getDpis : List ByteArray → Metadatas
  | [] => Metadatas.empty
  | data :: _ =>
      match parsePngPhysicalDimension data.toList with
      | .error _ => Metadatas.empty
      | .ok (phy, _) =>
          match phy.unit with
          | .unknown => (Metadatas.singleton .dpiX 72).union (Metadatas.singleton .dpiY 72)
          | .meter =>
              let dpx := dotsPerMeterToDotPerInch phy.dpiX.toNat
              let dpy := dotsPerMeterToDotPerInch phy.dpiY.toNat
              (Metadatas.singleton .dpiX dpx).union (Metadatas.singleton .dpiY dpy)

/-- Extract textual metadata from every `tEXt` chunk (invalid chunks are
    silently skipped, matching upstream's `runGet`-inside-`foldMap`, which
    contributes `mempty` for a `Left` decode result). -/
private def getTexts (chunks : List ByteArray) : Metadatas :=
  chunks.foldl
    (fun acc data =>
      match parsePngText data.toList with
      | .error _ => acc
      | .ok (kw, text) => acc.union (metadataOfKeyword kw text))
    Metadatas.empty

/-- Extract every `pHYs`/`gAMA`/`tEXt`-derived metadata from a parsed PNG
    image (see the module doc-comment for why `zTXt` is not included). -/
def extractMetadatas (img : PngRawImage) : Metadatas :=
  (getDpis (chunksWithSig img pHYsSignature)).union
    ((getGamma (chunksWithSig img gammaSignature)).union
      (getTexts (chunksWithSig img tEXtSignature)))

-- ── Encoding: `Metadatas` → chunks ──

/-- Build a `pHYs` chunk from `metas`' `dpiX`/`dpiY` entries, if both are
    present. -/
def encodePhysicalMetadata (metas : Metadatas) : List PngRawChunk :=
  match metas.lookup .dpiX, metas.lookup .dpiY with
  | some dx, some dy =>
      let dim : PngPhysicalDimension :=
        { dpiX := (dotPerInchToDotsPerMeter dx).toUInt32,
          dpiY := (dotPerInchToDotsPerMeter dy).toUInt32,
          unit := .meter }
      [mkRawChunk pHYsSignature (bytesOfBuilder (putPngPhysicalDimension dim))]
  | _, _ => []

/-- Build a `tEXt` chunk from a Latin-1 keyword/text pair: the keyword,
    followed by a NUL separator, followed by the text data (upstream's
    `PngText`'s `Binary` encoding). -/
private def mkTextChunk (keyword text : String) : PngRawChunk :=
  let kwBytes := (Data.ByteString.Char8.pack keyword).unpack
  let dataBytes := (Data.ByteString.Char8.pack text).unpack
  mkRawChunk tEXtSignature (ByteArray.mk (kwBytes ++ (0 : UInt8) :: dataBytes).toArray)

/-- Build the chunk(s), if any, that a single metadata element encodes to.
    `dpiX`/`dpiY`/`width`/`height`/`format`/`colorSpace`/`exif` elements are
    handled elsewhere (`encodePhysicalMetadata`) or have no PNG chunk
    representation at all, exactly as upstream's `encodeSingleMetadata`
    ignores them (`mempty`); `unknown` elements whose value isn't a
    `Value.string` are likewise dropped, matching upstream's
    `Met.Unknown _ :=> _ -> mempty`. -/
private def elemToChunks (e : Elem) : List PngRawChunk :=
  match e with
  | ⟨.gamma, g⟩ => [mkRawChunk gammaSignature (bytesOfBuilder (putPngGamma { value := g }))]
  | ⟨.title, tx⟩ => [mkTextChunk "Title" tx]
  | ⟨.description, tx⟩ => [mkTextChunk "Description" tx]
  | ⟨.author, tx⟩ => [mkTextChunk "Author" tx]
  | ⟨.copyright, tx⟩ => [mkTextChunk "Copyright" tx]
  | ⟨.software, tx⟩ => [mkTextChunk "Software" tx]
  | ⟨.comment, tx⟩ => [mkTextChunk "Comment" tx]
  | ⟨.disclaimer, tx⟩ => [mkTextChunk "Disclaimer" tx]
  | ⟨.source, tx⟩ => [mkTextChunk "Source" tx]
  | ⟨.warning, tx⟩ => [mkTextChunk "Warning" tx]
  | ⟨.unknown k, .string tx⟩ => [mkTextChunk k tx]
  | _ => []

/-- Build every `gAMA`/`tEXt` chunk (in metadata-list order) that `metas`'
    individual elements encode to. -/
def encodeSingleMetadata (metas : Metadatas) : List PngRawChunk :=
  metas.foldl' (fun acc e => acc ++ elemToChunks e) []

/-- Build every PNG ancillary chunk (`pHYs`, `gAMA`, `tEXt`) that `metas`
    encodes to. -/
def encodeMetadatas (metas : Metadatas) : List PngRawChunk :=
  encodePhysicalMetadata metas ++ encodeSingleMetadata metas

end Codec.Picture
