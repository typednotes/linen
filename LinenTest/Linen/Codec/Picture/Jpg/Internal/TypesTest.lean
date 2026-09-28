import Linen.Codec.Picture.Jpg.Internal.Types

/-!
  Tests for `Linen.Codec.Picture.Jpg.Internal.Types`: marker-code constants,
  round trips of the structural pieces of a JPEG file (frame headers,
  quantization/Huffman table segments using `DefaultTable`'s standard
  tables, scan headers), and a byte-stuffing round trip exercising
  `splitEcs`'s entropy-segment boundary scan.
-/

open Codec.Picture.Jpg.Internal
open Data.ByteString (Builder)

-- ── Marker-code constants ──

#guard codeOfFrameKind .baselineDCTHuffman == 0xC0
#guard codeOfFrameKind .progressiveDCTHuffman == 0xC2
#guard codeOfFrameKind .huffmanTableMarker == 0xC4
#guard codeOfFrameKind .quantizationTable == 0xDB
#guard codeOfFrameKind .startOfScan == 0xDA
#guard codeOfFrameKind .restartInterval == 0xDD
#guard codeOfFrameKind .endOfImage == 0xD9
#guard codeOfFrameKind (.applicationSegment 0) == 0xE0
#guard codeOfFrameKind (.applicationSegment 14) == 0xEE

#guard match frameKindOfCode 0xC0 with | .ok .baselineDCTHuffman => true | _ => false
#guard match frameKindOfCode 0xDB with | .ok .quantizationTable => true | _ => false
#guard match frameKindOfCode 0xD9 with | .ok .endOfImage => true | _ => false
#guard match frameKindOfCode 0xE1 with | .ok (.applicationSegment 1) => true | _ => false
#guard match frameKindOfCode 0xFE with | .ok (.extensionSegment 0xFE) => true | _ => false

-- ── `JpgComponent` round trip ──

def jpgTypeComponentY : JpgComponent :=
  { identifier := 1, horizontalSamplingFactor := 2, verticalSamplingFactor := 2,
    quantizationTableDest := 0 }

#guard
  match parseJpgComponent (putJpgComponent jpgTypeComponentY).toStrictByteString.unpack with
  | .ok (c, []) => decide (c = jpgTypeComponentY)
  | _ => false

-- ── `JpgFrameHeader` round trip ──

def jpgTypeFrameHeader : JpgFrameHeader :=
  { frameHeaderLength := 17, samplePrecision := 8, height := 480, width := 640,
    componentCount := 3,
    components :=
      [ { identifier := 1, horizontalSamplingFactor := 2, verticalSamplingFactor := 2,
          quantizationTableDest := 0 }
      , { identifier := 2, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
          quantizationTableDest := 1 }
      , { identifier := 3, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
          quantizationTableDest := 1 } ] }

#guard
  match parseJpgFrameHeader (putJpgFrameHeader jpgTypeFrameHeader).toStrictByteString.unpack with
  | .ok (h, []) => decide (h = jpgTypeFrameHeader)
  | _ => false

-- ── `JpgQuantTableSpec` round trip, using `DefaultTable`'s standard tables ──

def jpgTypeLumaQuantSpec : JpgQuantTableSpec :=
  { precision := 0, destination := 0, quantTable := defaultLumaQuantizationTable.map (fun b => b.toUInt16.toInt16) }

def jpgTypeChromaQuantSpec : JpgQuantTableSpec :=
  { precision := 0, destination := 1,
    quantTable := defaultChromaQuantizationTable.map (fun b => b.toUInt16.toInt16) }

#guard
  match parseJpgQuantTableSpec (putJpgQuantTableSpec jpgTypeLumaQuantSpec).toStrictByteString.unpack with
  | .ok (t, []) => decide (t = jpgTypeLumaQuantSpec)
  | _ => false

#guard
  match parseJpgQuantTableList
      (putJpgQuantTableList [jpgTypeLumaQuantSpec, jpgTypeChromaQuantSpec]).toStrictByteString.unpack with
  | .ok (ts, []) => decide (ts = [jpgTypeLumaQuantSpec, jpgTypeChromaQuantSpec])
  | _ => false

-- ── `JpgHuffmanTableSpec` round trip, using `DefaultTable`'s standard tables ──

def jpgTypeDcLumaHuffmanSpec : JpgHuffmanTableSpec :=
  { huffmanClass := .dcComponent, destination := 0, codes := defaultDcLumaHuffmanTable }

def jpgTypeAcLumaHuffmanSpec : JpgHuffmanTableSpec :=
  { huffmanClass := .acComponent, destination := 0, codes := defaultAcLumaHuffmanTable }

#guard
  match parseJpgHuffmanTableSpec
      (putJpgHuffmanTableSpec jpgTypeDcLumaHuffmanSpec).toStrictByteString.unpack with
  | .ok (t, []) => decide (t = jpgTypeDcLumaHuffmanSpec)
  | _ => false

#guard
  match parseJpgHuffmanTableList
      (putJpgHuffmanTableList [jpgTypeDcLumaHuffmanSpec, jpgTypeAcLumaHuffmanSpec]).toStrictByteString.unpack with
  | .ok (ts, []) => decide (ts = [jpgTypeDcLumaHuffmanSpec, jpgTypeAcLumaHuffmanSpec])
  | _ => false

-- ── `JpgScanHeader` round trip ──

def jpgTypeScanHeader : JpgScanHeader :=
  { scanLength := 12, componentCount := 3,
    scans :=
      [ { componentSelector := 1, dcEntropyCodingTable := 0, acEntropyCodingTable := 0 }
      , { componentSelector := 2, dcEntropyCodingTable := 1, acEntropyCodingTable := 1 }
      , { componentSelector := 3, dcEntropyCodingTable := 1, acEntropyCodingTable := 1 } ],
    spectralSelectionStart := 0, spectralSelectionEnd := 63,
    successiveApproxHigh := 0, successiveApproxLow := 0 }

#guard
  match parseJpgScanHeader (putJpgScanHeader jpgTypeScanHeader).toStrictByteString.unpack with
  | .ok (h, []) => decide (h = jpgTypeScanHeader)
  | _ => false

-- ── `JpgAdobeApp14` / `JpgJFIFApp0` round trip ──

def jpgTypeAdobeApp14 : JpgAdobeApp14 :=
  { dctVersion := 100, transformFlag0 := 0, transformFlag1 := 0, colorTransform := .ycbcr }

#guard
  match parseJpgAdobeApp14 (putJpgAdobeApp14 jpgTypeAdobeApp14).toStrictByteString.unpack with
  | .ok (a, []) => decide (a = jpgTypeAdobeApp14)
  | _ => false

def jpgTypeJFIFApp0 : JpgJFIFApp0 := { unit := .dotsPerInch, dpiX := 72, dpiY := 72 }

#guard
  match parseJpgJFIFApp0 (putJpgJFIFApp0 jpgTypeJFIFApp0).toStrictByteString.unpack with
  | .ok (j, []) => decide (j = jpgTypeJFIFApp0)
  | _ => false

-- ── Byte-stuffing-aware entropy-coded-segment boundary scan ──

-- A synthetic ECS payload containing a stuffed `0xFF 0x00` (a literal
-- `0xFF` data byte), a restart marker `0xFF 0xD1` (which belongs *inside*
-- the scan, not a segment boundary), followed by the real end-of-scan
-- marker `0xFF 0xD9` (EOI) and trailing bytes after it.
def jpgTypeEcsPayload : List UInt8 :=
  [0x12, 0x34, 0xFF, 0x00, 0x56, 0xFF, 0xD1, 0x78, 0x9A]

def jpgTypeAfterEcs : List UInt8 := [0xFF, 0xD9, 0x01, 0x02]

#guard
  match splitEcs (jpgTypeEcsPayload ++ jpgTypeAfterEcs) with
  | .ok (ecs, remaining) => ecs == jpgTypeEcsPayload && remaining == jpgTypeAfterEcs
  | .error _ => false

-- The stuffed `0x00` is dropped from the "logical" entropy bytes handed to
-- a later Huffman-decoding stage? No: `splitEcs` hands back the *raw* bytes
-- (including the stuffing `0x00`), matching upstream's `parseECSSimple`,
-- which also returns the untouched raw span; unstuffing is a bit-level
-- concern for the later `BitWriter`-based reader, not this structural scan.
#guard
  match splitEcs (jpgTypeEcsPayload ++ jpgTypeAfterEcs) with
  | .ok _ => true
  | .error _ => false

-- ── `JpgImage` round trip: SOI, one DQT frame, one DHT frame, EOI ──

def jpgTypeImage : JpgImage :=
  { frames :=
      [ .quantTableFrame [jpgTypeLumaQuantSpec]
      , .huffmanTableFrame [jpgTypeDcLumaHuffmanSpec]
      , .intervalRestart 0 ] }

#guard
  match parseJpgImage (putJpgImage jpgTypeImage).toStrictByteString.unpack with
  | .ok img => decide (img = jpgTypeImage)
  | .error _ => false
