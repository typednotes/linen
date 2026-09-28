/-
  Tests for `Linen.Codec.Picture.Png.Internal.Metadata` — `pHYs`/`gAMA`/`tEXt`
  chunk extraction into `Metadatas` (`extractMetadatas`), and `Metadatas` back
  into chunks (`encodePhysicalMetadata`/`encodeSingleMetadata`/
  `encodeMetadatas`).
-/
import Linen.Codec.Picture.Png.Internal.Metadata

open Codec.Picture

-- ── Fixtures ──

def pngMetaDummyHeader : PngIHdr :=
  { width := 1, height := 1, bitDepth := 8, colourType := .trueColour,
    compressionMethod := 0, filterMethod := 0, interlaceMethod := .noInterlace }

/-- A `gAMA` chunk whose decoded value is `2.2` (`220000` as a raw big-endian
    `UInt32`, built directly rather than via `putPngGamma` to avoid that
    function's `Float`-rounding `ceil`, which for `2.2` overshoots to
    `220001` — an upstream quirk, not something worth chasing in a fixture
    meant to exercise decoding). -/
def pngMetaGammaChunk : PngRawChunk :=
  mkRawChunk gammaSignature (ByteArray.mk #[0, 3, 91, 96])

/-- A `pHYs` chunk declaring `2834` dots per meter on both axes (`meter`
    unit). -/
def pngMetaPhysChunk : PngRawChunk :=
  mkRawChunk pHYsSignature
    (Data.ByteString.copy
      (putPngPhysicalDimension { dpiX := 2834, dpiY := 2834, unit := .meter }).toStrictByteString).data

/-- A `pHYs` chunk with an unspecified unit (dots-per-*something*, not
    convertible to DPI). -/
def pngMetaPhysUnknownUnitChunk : PngRawChunk :=
  mkRawChunk pHYsSignature
    (Data.ByteString.copy
      (putPngPhysicalDimension { dpiX := 100, dpiY := 100, unit := .unknown }).toStrictByteString).data

private def pngMetaTextChunkBytes (keyword text : String) : ByteArray :=
  keyword.toUTF8 ++ ByteArray.mk #[0] ++ text.toUTF8

def pngMetaTitleChunk : PngRawChunk :=
  mkRawChunk tEXtSignature (pngMetaTextChunkBytes "Title" "A test image")

def pngMetaAuthorChunk : PngRawChunk :=
  mkRawChunk tEXtSignature (pngMetaTextChunkBytes "Author" "Ada")

def pngMetaUnknownKeywordChunk : PngRawChunk :=
  mkRawChunk tEXtSignature (pngMetaTextChunkBytes "SomeApp:Version" "1.0")

def pngMetaMalformedTextChunk : PngRawChunk :=
  mkRawChunk tEXtSignature (ByteArray.mk #[72, 105]) -- "Hi", no NUL separator

def pngMetaImage : PngRawImage :=
  { header := pngMetaDummyHeader,
    chunks := [pngMetaGammaChunk, pngMetaPhysChunk, pngMetaTitleChunk, pngMetaAuthorChunk,
               pngMetaUnknownKeywordChunk, pngMetaMalformedTextChunk] }

-- ── `extractMetadatas` — gamma ──

#guard (extractMetadatas pngMetaImage).lookup .gamma == some 2.2

-- ── `extractMetadatas` — DPI (`meter` unit) ──

#guard (extractMetadatas pngMetaImage).lookup .dpiX == some (dotsPerMeterToDotPerInch 2834)
#guard (extractMetadatas pngMetaImage).lookup .dpiY == some (dotsPerMeterToDotPerInch 2834)

-- `PngUnitUnknown` defaults to 72 DPI on both axes.
def pngMetaImageUnknownUnit : PngRawImage :=
  { header := pngMetaDummyHeader, chunks := [pngMetaPhysUnknownUnitChunk] }

#guard (extractMetadatas pngMetaImageUnknownUnit).lookup .dpiX == some 72
#guard (extractMetadatas pngMetaImageUnknownUnit).lookup .dpiY == some 72

-- No `pHYs` chunk at all ⇒ no DPI metadata.
def pngMetaImageNoPhys : PngRawImage := { header := pngMetaDummyHeader, chunks := [] }

#guard (extractMetadatas pngMetaImageNoPhys).lookup .dpiX == none

-- ── `extractMetadatas` — text chunks ──

#guard (extractMetadatas pngMetaImage).lookup .title == some "A test image"
#guard (extractMetadatas pngMetaImage).lookup .author == some "Ada"

-- An unrecognised keyword becomes an `unknown`-keyed `Value.string`.
#guard (extractMetadatas pngMetaImage).lookup (.unknown "SomeApp:Version") == some (.string "1.0")

-- A malformed `tEXt` chunk (no NUL separator) is silently skipped, not an
-- error, matching upstream's `foldMap`-over-`Either` behaviour.
#guard (extractMetadatas pngMetaImage).lookup .comment == none

-- ── `encodePhysicalMetadata` ──

def pngMetaDpiMetas : Metadatas := (Metadatas.singleton .dpiX 300).union (Metadatas.singleton .dpiY 150)

#guard (encodePhysicalMetadata pngMetaDpiMetas).length == 1
#guard match encodePhysicalMetadata pngMetaDpiMetas with
  | [c] =>
      c.chunkType == pHYsSignature &&
      match parsePngPhysicalDimension c.chunkData.toList with
      | .ok (dim, _) =>
          dim.dpiX == (dotPerInchToDotsPerMeter 300).toUInt32 &&
          dim.dpiY == (dotPerInchToDotsPerMeter 150).toUInt32 &&
          dim.unit == .meter
      | .error _ => false
  | _ => false

-- No `pHYs` chunk is produced when only one of `dpiX`/`dpiY` is present.
#guard (encodePhysicalMetadata (Metadatas.singleton .dpiX 300)).length == 0

-- ── `encodeSingleMetadata` ──

def pngMetaTextMetas : Metadatas :=
  ((Metadatas.singleton .gamma 2.2).union (Metadatas.singleton .title "Hi")).union
    (Metadatas.singleton (.unknown "Foo") (.string "Bar"))

#guard (encodeSingleMetadata pngMetaTextMetas).length == 3
#guard (encodeSingleMetadata pngMetaTextMetas).any (·.chunkType == gammaSignature)
#guard (encodeSingleMetadata pngMetaTextMetas).all (fun c => c.chunkType == gammaSignature || c.chunkType == tEXtSignature)

-- `dpiX`/`width`/`format` elements have no per-element chunk (they are
-- handled by `encodePhysicalMetadata`, or have no PNG representation).
#guard (encodeSingleMetadata (Metadatas.singleton .dpiX 96)).length == 0
#guard (encodeSingleMetadata (Metadatas.singleton .width 10)).length == 0
#guard (encodeSingleMetadata (Metadatas.singleton .format .png)).length == 0

-- An `unknown` element whose value is not a `Value.string` has no chunk
-- representation either.
#guard (encodeSingleMetadata (Metadatas.singleton (.unknown "Foo") (.int 42))).length == 0

-- The `gAMA` chunk `encodeSingleMetadata` builds decodes back to the
-- original value.
#guard match encodeSingleMetadata (Metadatas.singleton .gamma 1.8) with
  | [c] => match parsePngGamma c.chunkData.toList with
    | .ok (g, _) => g.value == 1.8
    | .error _ => false
  | _ => false

-- The `tEXt` chunk `encodeSingleMetadata` builds for a well-known keyword
-- round trips through `extractMetadatas`.
#guard
  let chunks := encodeSingleMetadata (Metadatas.singleton .author "Grace")
  let img : PngRawImage := { header := pngMetaDummyHeader, chunks }
  (extractMetadatas img).lookup .author == some "Grace"

-- ── `encodeMetadatas` ──

#guard (encodeMetadatas pngMetaDpiMetas).length == 1
#guard (encodeMetadatas pngMetaTextMetas).length == 3
#guard (encodeMetadatas (((Metadatas.singleton .dpiX 72).union (Metadatas.singleton .dpiY 72)).union
    (Metadatas.singleton .gamma 1.0))).length == 2
