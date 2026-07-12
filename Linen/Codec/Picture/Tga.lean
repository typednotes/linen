import Linen.Codec.Picture.Types
import Linen.Codec.Picture.InternalHelper
import Linen.Codec.Picture.Metadata
import Linen.Codec.Picture.VectorByteConversion
import Linen.Data.ByteString.Builder
import Std.Internal.Parsec.ByteArray

/-!
  Port of `Codec.Picture.Tga` from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 9 of 29). Decoding and
  encoding of the Truevision TGA (`.tga`) format, across its 8/16/24/32-bit
  pixel depths, RLE compression, and colour-mapped (indexed) images.

  ## Design

  - As in `Linen.Codec.Picture.Bitmap`, upstream's `unparse` returns
    `Either String (PalettedImage, Metadatas)` where upstream's
    `PalettedImage` is a genuine sum type (`TrueColorImage DynamicImage`
    plus three `PalettedY8`/`PalettedRGB8`/`PalettedRGBA8` constructors).
    `Linen.Codec.Picture.Types`'s own `PalettedImage` (module 1) is instead
    "always indexed" (an `Image Pixel8` plus an RGB8 palette), so this
    module reproduces upstream's either/or shape locally with
    `Sum DynamicImage PalettedImage`, exactly as `Bitmap.lean` does.

  - Upstream threads an `ST s`/`MVector` scratch buffer through
    `unpackUncompressedTga`/`unpackRLETga`, both genuinely sequential
    (the RLE stream especially so — a run-length code followed by either one
    repeated pixel or a literal run). Both are ported as structural
    recursion over a `List UInt8` of the remaining input, producing a flat
    `Array UInt8` of pixel components; the non-obviously-structural steps
    (`List.drop`-ing consumed input) go through the stdlib `@[simp]` lemma
    `List.length_drop`, the same technique `Bitmap.lean`'s RLE decoder uses.

  - `TGAPixel`'s associated-type-indexed `Unpacked`/`packedByteSize`/
    `tgaUnpack` (an open type family keyed on four uninhabited marker types
    `Depth8`/`Depth15`/`Depth24`/`Depth32`) is ported as a closed
    `TgaPixelFormat` enum plus ordinary functions matching on it — Lean has
    no open type families, and the four depths are a fixed, closed set here.

  - `TgaSaveable`'s upstream methods all take the `Image a` they operate on
    (no ignored witness argument, unlike `BmpEncodable`), so they stay
    plain class methods.

  - `writeTga` (a trivial `IO` file-writer) is dropped, matching this
    library's convention of leaving file I/O to the caller.

  - The colour-mapped decode path decodes the palette bytes as a nested TGA
    image (matching upstream's recursive `unparse` call, simplified here to
    a direct `prepareUnpacker` call since only the decoded pixel data is
    ever used, never its metadata) and reduces whatever depth it decodes to
    (`Pixel8`/`PixelRGB8`/`PixelRGBA8`) down to this library's fixed
    RGB8-palette representation — a grayscale palette is expanded to
    `(v, v, v)`, and an RGBA8 palette's alpha is dropped but recorded via
    `PalettedImage.hasAlpha`.
-/

namespace Codec.Picture

open Std.Internal.Parsec
open Std.Internal.Parsec.ByteArray
open Data.ByteString (Builder)

-- ── Binary primitives ──

private def getU8 : Parser UInt8 := any

private def getU16LE : Parser UInt16 := do
  let b0 ← getU8
  let b1 ← getU8
  pure (b0.toUInt16 ||| (b1.toUInt16 <<< 8))

private def getBytes (n : Nat) : Parser ByteArray := (·.toByteArray) <$> ByteArray.take n

/-- Embed an `Except`-valued computation into the parser pipeline. -/
private def liftExcept : Except String α → Parser α
  | .ok a => pure a
  | .error e => fail e

private def builderOfByteArray (b : ByteArray) : Builder :=
  b.toList.foldl (fun acc byte => acc ++ Builder.word8 byte) Builder.empty

-- ── `TgaColorMapType` ──

/-- Whether a TGA file carries an explicit colour-map (palette) table. -/
inductive TgaColorMapType where
  | withoutTable
  | withTable
  | unknown (v : UInt8)
  deriving BEq

private def getTgaColorMapType : Parser TgaColorMapType := do
  let v ← getU8
  pure <| match v with
    | 0 => .withoutTable
    | 1 => .withTable
    | n => .unknown n

private def putTgaColorMapType : TgaColorMapType → Builder
  | .withoutTable => Builder.word8 0
  | .withTable => Builder.word8 1
  | .unknown v => Builder.word8 v

-- ── `TgaImageType` ──

/-- The four base TGA pixel-data layouts, each optionally RLE-compressed. -/
inductive TgaImageType where
  | noData (rle : Bool)
  | colorMapped (rle : Bool)
  | trueColor (rle : Bool)
  | monochrome (rle : Bool)
  deriving BEq

def isRleEncoded : TgaImageType → Bool
  | .noData rle => rle
  | .colorMapped rle => rle
  | .trueColor rle => rle
  | .monochrome rle => rle

private def imageTypeOfCode (v : UInt8) : Except String TgaImageType :=
  let rle := v &&& 0x08 != 0
  match v &&& 3 with
  | 0 => .ok (.noData rle)
  | 1 => .ok (.colorMapped rle)
  | 2 => .ok (.trueColor rle)
  | 3 => .ok (.monochrome rle)
  | n => .error s!"Unknown TGA image type {n}"

private def codeOfImageType : TgaImageType → UInt8
  | .noData rle => setRle 0 rle
  | .colorMapped rle => setRle 1 rle
  | .trueColor rle => setRle 2 rle
  | .monochrome rle => setRle 3 rle
where
  setRle (v : UInt8) (rle : Bool) : UInt8 := if rle then v ||| 0x08 else v

-- ── `TgaImageDescription` ──

/-- The image-orientation and attribute-bits byte. -/
structure TgaImageDescription where
  xOrigin : Bool
  yOrigin : Bool
  attributeBits : UInt8
  deriving BEq

private def getTgaImageDescription : Parser TgaImageDescription := do
  let v ← getU8
  pure { xOrigin := v &&& 0x10 != 0, yOrigin := v &&& 0x20 == 0, attributeBits := v &&& 0x0F }

private def putTgaImageDescription (d : TgaImageDescription) : Builder :=
  let xOrig : UInt8 := if d.xOrigin then 0x10 else 0
  let yOrig : UInt8 := if !d.yOrigin then 0x20 else 0
  Builder.word8 (xOrig ||| yOrig ||| (d.attributeBits &&& 0x0F))

-- ── `TgaHeader` ──

/-- The fixed 18-byte TGA header. -/
structure TgaHeader where
  idLength : UInt8
  colorMapType : TgaColorMapType
  imageType : TgaImageType
  mapStart : UInt16
  mapLength : UInt16
  mapDepth : UInt8
  xOffset : UInt16
  yOffset : UInt16
  width : UInt16
  height : UInt16
  pixelDepth : UInt8
  imageDescription : TgaImageDescription

private def getTgaHeader : Parser TgaHeader := do
  let idLength ← getU8
  let colorMapType ← getTgaColorMapType
  let typeCode ← getU8
  let imageType ← liftExcept (imageTypeOfCode typeCode)
  let mapStart ← getU16LE
  let mapLength ← getU16LE
  let mapDepth ← getU8
  let xOffset ← getU16LE
  let yOffset ← getU16LE
  let width ← getU16LE
  let height ← getU16LE
  let pixelDepth ← getU8
  let imageDescription ← getTgaImageDescription
  if width == 0 then fail "Width is null or negative"
  if height == 0 then fail "Height is null or negative"
  pure { idLength, colorMapType, imageType, mapStart, mapLength, mapDepth, xOffset, yOffset, width, height, pixelDepth, imageDescription }

private def putTgaHeader (h : TgaHeader) : Builder :=
  Builder.word8 h.idLength ++ putTgaColorMapType h.colorMapType ++ Builder.word8 (codeOfImageType h.imageType) ++
  Builder.word16LE h.mapStart ++ Builder.word16LE h.mapLength ++ Builder.word8 h.mapDepth ++
  Builder.word16LE h.xOffset ++ Builder.word16LE h.yOffset ++ Builder.word16LE h.width ++
  Builder.word16LE h.height ++ Builder.word8 h.pixelDepth ++ putTgaImageDescription h.imageDescription

private def getPalette (hdr : TgaHeader) : Parser ByteArray :=
  if hdr.mapLength == 0 then pure ByteArray.empty
  else getBytes ((hdr.mapDepth.toNat / 8) * hdr.mapLength.toNat)

/-- Parse the header, file-id, palette, and remaining (pixel-data) bytes of
    a TGA file. -/
private def getTgaFile : Parser (TgaHeader × ByteArray × ByteArray × ByteArray) := do
  let hdr ← getTgaHeader
  let fileId ← getBytes hdr.idLength.toNat
  let palette ← getPalette hdr
  let rest ← getRemainingBytes
  pure (hdr, fileId, palette, rest)

-- ── Pixel unpacking ──

/-- The four fixed pixel-data layouts a TGA file's `pixelDepth` may select. -/
private inductive TgaPixelFormat where
  | depth8
  | depth15
  | depth24
  | depth32

/-- Bytes occupied by a single packed pixel on disk. -/
private def packedByteSize : TgaPixelFormat → Nat
  | .depth8 => 1
  | .depth15 => 2
  | .depth24 => 3
  | .depth32 => 4

private theorem packedByteSize_pos (fmt : TgaPixelFormat) : 1 ≤ packedByteSize fmt := by
  cases fmt <;> decide

/-- Components in a single unpacked pixel. -/
private def tgaCompCount : TgaPixelFormat → Nat
  | .depth8 => 1
  | .depth15 => 4
  | .depth24 => 3
  | .depth32 => 4

/-- Unpack a single pixel's components from the front of `bytes` (which must
    hold at least `packedByteSize fmt` bytes). -/
private def unpackPixelFromList (fmt : TgaPixelFormat) (bytes : List UInt8) : Array UInt8 :=
  match fmt with
  | .depth8 => #[bytes.getD 0 0]
  | .depth15 =>
      let v0 := bytes.getD 0 0
      let v1 := bytes.getD 1 0
      let r := (v1 &&& 0x7c) <<< 1
      let g := ((v1 &&& 0x03) <<< 6) ||| ((v0 &&& 0xe0) >>> 2)
      let b := (v0 &&& 0x1f) <<< 3
      #[r, g, b, (255 : UInt8)]
  | .depth24 =>
      let b := bytes.getD 0 0
      let g := bytes.getD 1 0
      let r := bytes.getD 2 0
      #[r, g, b]
  | .depth32 =>
      let b := bytes.getD 0 0
      let g := bytes.getD 1 0
      let r := bytes.getD 2 0
      let a := bytes.getD 3 0
      #[r, g, b, a]

/-- Write `comps` into `arr` starting at `writeIndex`, ignoring any component
    that would fall outside `arr`. -/
private def writeComponents (arr : Array UInt8) (writeIndex : Nat) (comps : Array UInt8) : Array UInt8 :=
  (List.range comps.size).foldl (fun a i =>
    if writeIndex + i < a.size then a.set! (writeIndex + i) (comps.getD i 0) else a) arr

/-- Write `n` copies of `comps` (a single unpacked pixel) starting at
    `writeIndex`, stopping at `limit`. -/
private def tgaWriteRepeated (compCount limit : Nat) :
    Nat → Array UInt8 → Nat → Array UInt8 → Array UInt8
  | 0, _, _, arr => arr
  | n + 1, comps, writeIndex, arr =>
      if writeIndex ≥ limit then arr
      else tgaWriteRepeated compCount limit n comps (writeIndex + compCount) (writeComponents arr writeIndex comps)
termination_by n _ _ _ => n

/-- Copy up to `n` consecutive packed pixels from `bytes`, stopping at
    `limit` or when `bytes` runs out; returns the number of input bytes
    consumed. -/
private def tgaWriteLiteral (unpack : List UInt8 → Array UInt8) (compCount readSize limit : Nat) :
    Nat → List UInt8 → Nat → Nat → Array UInt8 → (Nat × Nat × Array UInt8)
  | 0, _, consumed, writeIndex, arr => (consumed, writeIndex, arr)
  | n + 1, bytes, consumed, writeIndex, arr =>
      if writeIndex ≥ limit then (consumed, writeIndex, arr)
      else
        let chunk := bytes.drop consumed
        if chunk.length < readSize then (consumed, writeIndex, arr)
        else
          let comps := unpack chunk
          let arr' := writeComponents arr writeIndex comps
          tgaWriteLiteral unpack compCount readSize limit n bytes (consumed + readSize) (writeIndex + compCount) arr'
termination_by n _ _ _ _ => n

/-- Decode a Microsoft-style RLE-compressed TGA pixel stream: each leading
    byte is a run-length code (top bit set ⇒ one repeated pixel, else a
    literal run), followed by that many pixels' worth of packed data.
    `readSize` (the on-disk byte width of one pixel) is threaded through as
    an explicit argument together with a proof that it is positive, so the
    termination proof below never has to derive that fact from `fmt` inside
    the recursive equations. -/
private def tgaRLEInner (unpack : List UInt8 → Array UInt8) (compCount readSize : Nat)
    (hpos : 1 ≤ readSize) (maxi : Nat) :
    List UInt8 → Nat → Array UInt8 → Array UInt8
  | bytes, writeIndex, arr =>
      if writeIndex ≥ maxi then arr
      else match bytes with
        | [] => arr
        | code :: rest =>
            let count := (code &&& 0x7F).toNat + 1
            let copyMax := min maxi (writeIndex + count * compCount)
            if code &&& 0x80 != 0 then
              if rest.length < readSize then arr
              else
                let comps := unpack rest
                let arr' := tgaWriteRepeated compCount copyMax count comps writeIndex arr
                tgaRLEInner unpack compCount readSize hpos maxi (rest.drop readSize) copyMax arr'
            else
              let (consumed, writeIndex', arr') := tgaWriteLiteral unpack compCount readSize copyMax count rest 0 writeIndex arr
              tgaRLEInner unpack compCount readSize hpos maxi (rest.drop consumed) writeIndex' arr'
termination_by bytes _ _ => bytes.length
decreasing_by all_goals (simp_all [List.length_drop]; omega)

/-- Decode an uncompressed TGA pixel stream: consecutive packed pixels with
    no run-length codes. -/
private def tgaUncompressedInner (unpack : List UInt8 → Array UInt8) (compCount readSize : Nat)
    (hpos : 1 ≤ readSize) (maxi : Nat) :
    List UInt8 → Nat → Array UInt8 → Array UInt8
  | bytes, writeIndex, arr =>
      if writeIndex ≥ maxi then arr
      else
        if bytes.length < readSize then arr
        else
          let comps := unpack bytes
          let arr' := writeComponents arr writeIndex comps
          tgaUncompressedInner unpack compCount readSize hpos maxi (bytes.drop readSize) (writeIndex + compCount) arr'
termination_by bytes _ _ => bytes.length
decreasing_by all_goals (simp_all [List.length_drop]; omega)

private def tgaDecodeComponents (fmt : TgaPixelFormat) (rle : Bool) (wi hi : Nat) (str : ByteArray) : Array UInt8 :=
  let compCount := tgaCompCount fmt
  let maxi := wi * hi * compCount
  let bytes := str.toList
  let init := Array.replicate maxi (0 : UInt8)
  let readSize := packedByteSize fmt
  let hpos := packedByteSize_pos fmt
  let unpack := unpackPixelFromList fmt
  if rle then tgaRLEInner unpack compCount readSize hpos maxi bytes 0 init
  else tgaUncompressedInner unpack compCount readSize hpos maxi bytes 0 init

private def decodeDepth8 (rle : Bool) (wi hi : Nat) (str : ByteArray) : Image Pixel8 :=
  { width := wi, height := hi, data := tgaDecodeComponents .depth8 rle wi hi str }

private def decodeDepth15 (rle : Bool) (wi hi : Nat) (str : ByteArray) : Image PixelRGBA8 :=
  { width := wi, height := hi, data := tgaDecodeComponents .depth15 rle wi hi str }

private def decodeDepth24 (rle : Bool) (wi hi : Nat) (str : ByteArray) : Image PixelRGB8 :=
  { width := wi, height := hi, data := tgaDecodeComponents .depth24 rle wi hi str }

private def decodeDepth32 (rle : Bool) (wi hi : Nat) (str : ByteArray) : Image PixelRGBA8 :=
  { width := wi, height := hi, data := tgaDecodeComponents .depth32 rle wi hi str }

/-- Flip an image horizontally and/or vertically according to a TGA image's
    origin flags. -/
private def flipTga [Pixel α Component] (desc : TgaImageDescription) (img : @Image α Component _) :
    @Image α Component _ :=
  let w := img.width
  let h := img.height
  if desc.xOrigin && desc.yOrigin then generateImage (fun x y => img.getPixel (w - 1 - x) (h - 1 - y)) w h
  else if desc.xOrigin then generateImage (fun x y => img.getPixel (w - 1 - x) y) w h
  else if desc.yOrigin then generateImage (fun x y => img.getPixel x (h - 1 - y)) w h
  else img

/-- Decode a raw TGA pixel stream into whichever `DynamicImage` variant its
    `pixelDepth` selects, flipped according to `desc`. -/
private def prepareUnpacker (desc : TgaImageDescription) (pixelDepth : Nat) (rle : Bool) (wi hi : Nat)
    (str : ByteArray) : Except String DynamicImage :=
  match pixelDepth with
  | 8 => .ok (.y8 (flipTga desc (decodeDepth8 rle wi hi str)))
  | 16 => .ok (.rgba8 (flipTga desc (decodeDepth15 rle wi hi str)))
  | 24 => .ok (.rgb8 (flipTga desc (decodeDepth24 rle wi hi str)))
  | 32 => .ok (.rgba8 (flipTga desc (decodeDepth32 rle wi hi str)))
  | n => .error s!"Invalid bit depth ({n})"

/-- Reduce a decoded palette image to this library's fixed RGB8-palette
    representation, recording whether the source palette carried alpha. -/
private def dynamicImageToRGB8Palette : DynamicImage → Option (Image PixelRGB8 × Bool)
  | .y8 img => some (pixelMap (fun (p : Pixel8) => (⟨p, p, p⟩ : PixelRGB8)) img, false)
  | .rgb8 img => some (img, false)
  | .rgba8 img => some (pixelMap (fun (p : PixelRGBA8) => (⟨p.r, p.g, p.b⟩ : PixelRGB8)) img, true)
  | _ => none

/-- Decode a parsed TGA file's header, palette bytes, and remaining pixel
    bytes into either a true-colour image or a paletted one, plus its
    metadata. -/
private def unparseTga (hdr : TgaHeader) (palette rest : ByteArray) :
    Except String (Sum DynamicImage PalettedImage × Metadatas) :=
  let wi := hdr.width.toNat
  let hi := hdr.height.toNat
  let desc := hdr.imageDescription
  let rle := isRleEncoded hdr.imageType
  let metas := basicMetadata .tga wi hi
  match hdr.imageType with
  | .noData _ => .error "No data detected in TGA file"
  | .trueColor _ =>
      match prepareUnpacker desc hdr.pixelDepth.toNat rle wi hi rest with
      | .error e => .error e
      | .ok dyn => .ok (.inl dyn, metas)
  | .monochrome _ =>
      match prepareUnpacker desc hdr.pixelDepth.toNat rle wi hi rest with
      | .error e => .error e
      | .ok dyn => .ok (.inl dyn, metas)
  | .colorMapped _ =>
      match prepareUnpacker desc hdr.mapDepth.toNat false hdr.mapLength.toNat 1 palette with
      | .error e => .error e
      | .ok paletteDyn =>
          match dynamicImageToRGB8Palette paletteDyn with
          | none => .error "Unknown pixel type"
          | some (paletteImg, hasAlpha) =>
              match prepareUnpacker desc hdr.pixelDepth.toNat rle wi hi rest with
              | .error e => .error e
              | .ok (.y8 idxImg) => .ok (.inr { indexedImage := idxImg, palette := paletteImg, hasAlpha }, metas)
              | .ok _ => .error "Bad colorspace for image"

-- ── Decoding pipeline ──

/-- Decode a TGA file, returning either a true-colour `DynamicImage` or a
    paletted image, plus its metadata. -/
def decodeTgaWithPaletteAndMetadata (input : ByteArray) :
    Except String (Sum DynamicImage PalettedImage × Metadatas) :=
  match runGetStrict getTgaFile input with
  | .error e => .error e
  | .ok (hdr, _fileId, palette, rest) => unparseTga hdr palette rest

/-- Decode a TGA file into a true-colour `DynamicImage`, expanding any
    indexed palette. -/
def decodeTgaWithMetadata (input : ByteArray) : Except String (DynamicImage × Metadatas) :=
  match decodeTgaWithPaletteAndMetadata input with
  | .error e => .error e
  | .ok (.inl dyn, md) => .ok (dyn, md)
  | .ok (.inr paletted, md) => .ok (.rgb8 (palettedToTrueColor paletted), md)

/-- Decode a TGA file into a true-colour `DynamicImage`. -/
def decodeTga (input : ByteArray) : Except String DynamicImage :=
  match decodeTgaWithMetadata input with
  | .error e => .error e
  | .ok (img, _) => .ok img

-- ── Encoding ──

/-- A pixel type that can be encoded as a TGA file's raw pixel data. -/
class TgaSaveable (pixel : Type) [Pixel pixel Pixel8] where
  /-- The image's raw on-disk pixel bytes. -/
  tgaDataOfImage : Image pixel → ByteArray
  /-- Bits used to encode a single pixel. -/
  tgaPixelDepthOfImage : Image pixel → UInt8
  /-- The TGA image-type byte to use for this pixel format. -/
  tgaTypeOfImage : Image pixel → TgaImageType

instance : TgaSaveable Pixel8 where
  tgaDataOfImage img := toByteArray img.data
  tgaPixelDepthOfImage _ := 8
  tgaTypeOfImage _ := .monochrome false

instance : TgaSaveable PixelRGB8 where
  tgaDataOfImage img := toByteArray (pixelMap (fun (p : PixelRGB8) => (⟨p.b, p.g, p.r⟩ : PixelRGB8)) img).data
  tgaPixelDepthOfImage _ := 24
  tgaTypeOfImage _ := .trueColor false

instance : TgaSaveable PixelRGBA8 where
  tgaDataOfImage img := toByteArray (pixelMap (fun (p : PixelRGBA8) => (⟨p.b, p.g, p.r, p.a⟩ : PixelRGBA8)) img).data
  tgaPixelDepthOfImage _ := 32
  tgaTypeOfImage _ := .trueColor false

/-- Encode an image as a Truevision TGA file. -/
def encodeTga [Pixel pixel Pixel8] [TgaSaveable pixel] (img : Image pixel) : Data.ByteString :=
  let hdr : TgaHeader :=
    { idLength := 0, colorMapType := .withoutTable, imageType := TgaSaveable.tgaTypeOfImage img,
      mapStart := 0, mapLength := 0, mapDepth := 0, xOffset := 0, yOffset := 0,
      width := img.width.toUInt16, height := img.height.toUInt16,
      pixelDepth := TgaSaveable.tgaPixelDepthOfImage img,
      imageDescription := { xOrigin := false, yOrigin := false, attributeBits := 0 } }
  (putTgaHeader hdr ++ builderOfByteArray (TgaSaveable.tgaDataOfImage img)).toStrictByteString

end Codec.Picture
