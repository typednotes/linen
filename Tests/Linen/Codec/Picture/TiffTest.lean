/-
  Tests for `Linen.Codec.Picture.Tiff`. `decodeTiff`/`encodeTiff` are pure
  (`Except String X` / `Data.ByteString` — see that module's doc-comment for
  why, unlike `Linen.Codec.Picture.Png`, this module never needs `IO`), so
  round trips are checked with plain `#guard`.

  Fixture names are prefixed `tiff` to avoid cross-file `Tests` namespace
  collisions (bare names like `img`/`bytes` have collided across sibling test
  files before).
-/
import Linen.Codec.Picture.Tiff

open Codec.Picture

-- ── A `Data.ByteString` → `ByteArray` helper (encode output → decode input) ──

def tiffToByteArray (bs : Data.ByteString) : ByteArray :=
  ByteArray.mk bs.unpack.toArray

-- ── Round trip: 8-bit greyscale ──

/-- A small 4×3 greyscale test image, all pixels distinct so row/column order
    bugs show up. -/
def tiffGreyImg : Image Pixel8 :=
  generateImage (fun x y => (x * 10 + y * 3).toUInt8) 4 3

def tiffGreyBytes : ByteArray := tiffToByteArray (encodeTiff tiffGreyImg)

#guard match decodeTiff tiffGreyBytes with
  | .ok (.y8 img) => img.width == 4 ∧ img.height == 3 ∧
      img.getPixel 0 0 == tiffGreyImg.getPixel 0 0 ∧ img.getPixel 3 2 == tiffGreyImg.getPixel 3 2
  | _ => false

-- ── Round trip: 8-bit RGB ──

/-- A 3×2 RGB test image with distinct channel values per pixel. -/
def tiffRgbImg : Image PixelRGB8 :=
  generateImage (fun x y => (⟨(x * 40 + 10).toUInt8, (y * 50 + 5).toUInt8, ((x + y) * 20).toUInt8⟩ :
    PixelRGB8)) 3 2

def tiffRgbBytes : ByteArray := tiffToByteArray (encodeTiff tiffRgbImg)

#guard match decodeTiff tiffRgbBytes with
  | .ok (.rgb8 img) => img.width == 3 ∧ img.height == 2 ∧
      img.getPixel 0 0 == tiffRgbImg.getPixel 0 0 ∧ img.getPixel 2 1 == tiffRgbImg.getPixel 2 1
  | _ => false

-- ── Round trip: 8-bit RGBA ──

/-- A 2×2 RGBA test image with a non-trivial alpha channel. -/
def tiffRgbaImg : Image PixelRGBA8 :=
  generateImage (fun x y => (⟨(x * 50).toUInt8, (y * 50).toUInt8, 7, (x + y * 2 + 1).toUInt8⟩ :
    PixelRGBA8)) 2 2

def tiffRgbaBytes : ByteArray := tiffToByteArray (encodeTiff tiffRgbaImg)

#guard match decodeTiff tiffRgbaBytes with
  | .ok (.rgba8 img) =>
      img.getPixel 0 0 == tiffRgbaImg.getPixel 0 0 ∧ img.getPixel 1 1 == tiffRgbaImg.getPixel 1 1
  | _ => false

-- ── PackBits round trip ──

#guard unpackPackBits (ByteArray.mk #[2, 10, 20, 30]) == ByteArray.mk #[10, 20, 30]
#guard unpackPackBits (ByteArray.mk #[(256 - 3).toUInt8, 7]) == ByteArray.mk #[7, 7, 7, 7]
#guard unpackPackBits (ByteArray.mk #[128, 5, 9]) == ByteArray.mk #[9]

-- ── Hand-crafted TIFFs decoded independently of `encodeTiff` ──

/-- Build a minimal single-strip, 8-bit greyscale TIFF file by hand, directly
    from module 15's `TiffHeader`/`ImageFileDirectory`/`putTiffHeader`/
    `putImageFileDirectoryList`, independently of this module's own
    `encodeTiff`, as a pipeline sanity check on `decodeTiff` itself.

    Every scalar tag here is declared with `IfdType.long` rather than the
    TIFF-spec-recommended `.short` for tags like `PhotometricInterpretation`/
    `Compression`/`SamplesPerPixel`/`PlanarConfiguration`/`BitsPerSample`.
    This sidesteps this module's own faithfully-ported `findData`/
    `findIfdExt` limitation for a single `.short` value (see the module
    doc-comment's "offset resolution" section): `findData`/`findIfdExt` read
    a mandatory scalar tag's raw `ifdOffset` field directly, with no
    endian-aware unshifting of a genuinely `.short`-typed, left-justified
    single value — exactly upstream's own `findIFDData`/`findIFDExt`
    behaviour, not a limitation invented here. `.long`-typed single values
    have no such ambiguity (a `.long` occupies the whole 4-byte field, read
    consistently for either byte order), so using `.long` here still
    exercises this test's actual goal — decoding a hand-built, independent
    IFD chain in both byte orders — without depending on that separately
    faithfully-preserved bug. -/
def tiffHandBuilt (endian : TiffEndianness) : ByteArray :=
  let width : UInt32 := 2
  let height : UInt32 := 2
  let pixels : Array UInt8 := #[10, 20, 30, 40]
  let headerSize : UInt32 := 8
  let imageSize : UInt32 := 4
  let ifdOffset := headerSize + imageSize
  let mkEntry (tag : ExifTag) (v : UInt32) : ImageFileDirectory :=
    { ifdIdentifier := tag, ifdType := .long, ifdCount := 1, ifdOffset := v, ifdExtended := .none }
  let entries : List ImageFileDirectory :=
    [ mkEntry .imageWidth width
    , mkEntry .imageLength height
    , mkEntry .bitsPerSample 8
    , mkEntry .compression (packCompression .none).toUInt32
    , mkEntry .photometricInterpretation (packPhotometricInterpretation .monochrome).toUInt32
    , mkEntry .stripOffsets headerSize
    , mkEntry .rowPerStrip height
    , mkEntry .stripByteCounts imageSize
    , mkEntry .samplesPerPixel 1
    , mkEntry .planarConfiguration (constantOfPlanarConfg .contig).toUInt32 ]
  let header : TiffHeader := { endianness := endian, offset := ifdOffset }
  let pixelBuilder := pixels.foldl (fun acc b => acc ++ Data.ByteString.Builder.word8 b) Data.ByteString.Builder.empty
  let ifdBuilder := putImageFileDirectoryList endian entries 0
  tiffToByteArray (putTiffHeader header ++ pixelBuilder ++ ifdBuilder).toStrictByteString

#guard match decodeTiff (tiffHandBuilt .little) with
  | .ok (.y8 img) => img.width == 2 ∧ img.height == 2 ∧
      img.getPixel 0 0 == (10 : Pixel8) ∧ img.getPixel 1 1 == (40 : Pixel8)
  | _ => false

#guard match decodeTiff (tiffHandBuilt .big) with
  | .ok (.y8 img) => img.width == 2 ∧ img.height == 2 ∧
      img.getPixel 0 0 == (10 : Pixel8) ∧ img.getPixel 1 1 == (40 : Pixel8)
  | _ => false
