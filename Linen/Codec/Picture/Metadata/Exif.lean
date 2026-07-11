/-!
  Port of `Codec.Picture.Metadata.Exif` from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 5 of 29). A "totally
  partial and incomplete" vocabulary of Exif tags, used by both TIFF parsing
  and Exif extraction.

  Upstream's `Vector Word16`/`Vector Word32` fields on `ExifData` are ported
  as `Array UInt16`/`Array UInt32`, matching this library's general
  `Data.Vector → Array` convention.

  `ExifSignedRational`'s upstream `Int32` fields are ported as `UInt32`, same
  bit pattern (Lean has no fixed-width signed integer type — see
  `Linen.Codec.Picture.BitWriter`'s `getNextIntJpg` for the same
  simplification and its rationale).
-/

namespace Codec.Picture

-- ── Tags ──

/-- Tag values used for exif fields. Completely incomplete. -/
inductive ExifTag where
  | photometricInterpretation
  | compression -- ^ Short type
  | imageWidth -- ^ Short or long type
  | imageLength -- ^ Short or long type
  | xResolution -- ^ Rational type
  | yResolution -- ^ Rational type
  | resolutionUnit -- ^ Short type
  | rowPerStrip -- ^ Short or long type
  | stripByteCounts -- ^ Short or long
  | stripOffsets -- ^ Short or long
  | bitsPerSample -- ^ Short
  | colorMap -- ^ Short
  | tileWidth
  | tileLength
  | tileOffset
  | tileByteCount
  | samplesPerPixel -- ^ Short
  | artist
  | documentName
  | software
  | planarConfiguration -- ^ Short
  | orientation
  | sampleFormat -- ^ Short
  | inkSet
  | subfileType
  | fillOrder
  | yCbCrCoeff
  | yCbCrSubsampling
  | yCbCrPositioning
  | referenceBlackWhite
  | xPosition
  | yPosition
  | extraSample
  | imageDescription
  | predictor
  | copyright
  | make
  | model
  | dateTime
  | gpsInfo
  | lightSource -- ^ Short
  | flash -- ^ Short
  | jpegProc
  | jpegInterchangeFormat
  | jpegInterchangeFormatLength
  | jpegRestartInterval
  | jpegLosslessPredictors
  | jpegPointTransforms
  | jpegQTables
  | jpegDCTables
  | jpegACTables
  | exifOffset
  | unknown (v : UInt16)
  deriving Repr, BEq

/-- Convert a value to its corresponding Exif tag. Will often be
    `.unknown`. -/
def ExifTag.ofWord16 (v : UInt16) : ExifTag :=
  match v with
  | 255 => .subfileType
  | 256 => .imageWidth
  | 257 => .imageLength
  | 258 => .bitsPerSample
  | 259 => .compression
  | 262 => .photometricInterpretation
  | 266 => .fillOrder
  | 269 => .documentName
  | 270 => .imageDescription
  | 271 => .make
  | 272 => .model
  | 273 => .stripOffsets
  | 274 => .orientation
  | 277 => .samplesPerPixel
  | 278 => .rowPerStrip
  | 279 => .stripByteCounts
  | 282 => .xResolution
  | 283 => .yResolution
  | 284 => .planarConfiguration
  | 286 => .xPosition
  | 287 => .yPosition
  | 296 => .resolutionUnit
  | 305 => .software
  | 306 => .dateTime
  | 315 => .artist
  | 317 => .predictor
  | 320 => .colorMap
  | 322 => .tileWidth
  | 323 => .tileLength
  | 324 => .tileOffset
  | 325 => .tileByteCount
  | 332 => .inkSet
  | 338 => .extraSample
  | 339 => .sampleFormat
  | 529 => .yCbCrCoeff
  | 512 => .jpegProc
  | 513 => .jpegInterchangeFormat
  | 514 => .jpegInterchangeFormatLength
  | 515 => .jpegRestartInterval
  | 517 => .jpegLosslessPredictors
  | 518 => .jpegPointTransforms
  | 519 => .jpegQTables
  | 520 => .jpegDCTables
  | 521 => .jpegACTables
  | 530 => .yCbCrSubsampling
  | 531 => .yCbCrPositioning
  | 532 => .referenceBlackWhite
  | 33432 => .copyright
  | 34665 => .exifOffset
  | 34853 => .gpsInfo
  | 37384 => .lightSource
  | 37385 => .flash
  | vv => .unknown vv

/-- Convert a tag to its corresponding value. -/
def ExifTag.toWord16 (t : ExifTag) : UInt16 :=
  match t with
  | .subfileType => 255
  | .imageWidth => 256
  | .imageLength => 257
  | .bitsPerSample => 258
  | .compression => 259
  | .photometricInterpretation => 262
  | .fillOrder => 266
  | .documentName => 269
  | .imageDescription => 270
  | .make => 271
  | .model => 272
  | .stripOffsets => 273
  | .orientation => 274
  | .samplesPerPixel => 277
  | .rowPerStrip => 278
  | .stripByteCounts => 279
  | .xResolution => 282
  | .yResolution => 283
  | .planarConfiguration => 284
  | .xPosition => 286
  | .yPosition => 287
  | .resolutionUnit => 296
  | .software => 305
  | .dateTime => 306
  | .artist => 315
  | .predictor => 317
  | .colorMap => 320
  | .tileWidth => 322
  | .tileLength => 323
  | .tileOffset => 324
  | .tileByteCount => 325
  | .inkSet => 332
  | .extraSample => 338
  | .sampleFormat => 339
  | .yCbCrCoeff => 529
  | .jpegProc => 512
  | .jpegInterchangeFormat => 513
  | .jpegInterchangeFormatLength => 514
  | .jpegRestartInterval => 515
  | .jpegLosslessPredictors => 517
  | .jpegPointTransforms => 518
  | .jpegQTables => 519
  | .jpegDCTables => 520
  | .jpegACTables => 521
  | .yCbCrSubsampling => 530
  | .yCbCrPositioning => 531
  | .referenceBlackWhite => 532
  | .copyright => 33432
  | .exifOffset => 34665
  | .gpsInfo => 34853
  | .lightSource => 37384
  | .flash => 37385
  | .unknown v => v

/-- Is this tag one that belongs to the primary IFD (as opposed to a
    redirect, like `exifOffset`/`gpsInfo`, into a sub-IFD)? -/
def ExifTag.isInIFD0 (t : ExifTag) : Bool :=
  t.toWord16 <= ExifTag.copyright.toWord16 || t == .exifOffset || t == .gpsInfo

-- ── Values ──

/-- Possible data held by an Exif tag. -/
inductive ExifData where
  | none
  | long (v : UInt32)
  | short (v : UInt16)
  | string (v : ByteArray)
  | undefined (v : ByteArray)
  | shorts (v : Array UInt16)
  | longs (v : Array UInt32)
  | rational (num den : UInt32)
  | signedRational (num den : UInt32)
  | ifd (entries : List (ExifTag × ExifData))
  deriving BEq

end Codec.Picture
