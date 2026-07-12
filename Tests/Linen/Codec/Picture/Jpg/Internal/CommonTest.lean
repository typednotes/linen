import Linen.Codec.Picture.Jpg.Internal.Common

/-!
  Tests for `Linen.Codec.Picture.Jpg.Internal.Common`: the zigzag scan-order
  table (size and spot-checks against the fetched upstream literal table,
  plus a round trip through `zigZagReorderForward`/`zigZagReorder`),
  Huffman-symbol decoding (`huffmanDecode`) against both a hand-built tree
  and one of `DefaultTable`'s standard trees, the DC/AC category/extra-bits
  formula (`decodeInt`, both a positive and a negative case), full DC
  coefficient decoding (`dcCoefficientDecode`, zero and nonzero categories),
  AC run-length/category nibble splitting (`decodeRrrrSsss`), and the small
  numeric helpers (`toBlockSize`, `pixelClamp`, `deQuantize`,
  `unpackMacroBlock`).
-/

open Codec.Picture (BoolReader BoolState Image PixelYCbCr8 runBoolReader runBoolReaderWith
  initBoolStateJpg)
open Codec.Picture.Jpg.Internal

-- ── Zigzag scan order ──

#guard zigZagOrder.size == 64
#guard zigZagOrder.getD 0 999 == 0
#guard zigZagOrder.getD 1 999 == 1
#guard zigZagOrder.getD 7 999 == 28
#guard zigZagOrder.getD 8 999 == 2
#guard zigZagOrder.getD 63 999 == 63
#guard zigZagOrder.getD 35 999 == 32

def jpgCommonSampleBlock : MacroBlock Int16 :=
  Array.ofFn (n := dctBlockSize) fun i => Int16.ofNat i.val

-- Round trip: encoding into zigzag order and decoding back out is the identity.
#guard decide (zigZagReorder (zigZagReorderForward jpgCommonSampleBlock) = jpgCommonSampleBlock)

-- ── Huffman-symbol decoding ──

/-- A hand-built tree: bit `0` selects `0x11`, bit `1` selects `0x23`. -/
def jpgCommonTinyTree : HuffmanTree :=
  .branch (.leaf 0x11) (.leaf 0x23)

private def jpgCommonRunDecode (tree : HuffmanTree) (byte : UInt8) : UInt8 :=
  (runBoolReaderWith (initBoolStateJpg (ByteArray.mk #[byte])) (huffmanDecode tree)).1

#guard jpgCommonRunDecode jpgCommonTinyTree 0x00 == 0x11
#guard jpgCommonRunDecode jpgCommonTinyTree 0x80 == 0x23

-- Against `DefaultTable`'s standard DC luminance tree: its sole length-1
-- code (bit `0`) decodes to symbol `0`.
#guard jpgCommonRunDecode defaultDcLumaHuffmanTree 0x00 == 0

-- ── DC/AC category + extra-bits decoding ──

private def jpgCommonRunDecodeInt (ssss : Nat) (byte : UInt8) : Int :=
  (runBoolReaderWith (initBoolStateJpg (ByteArray.mk #[byte])) (decodeInt ssss)).1

-- `ssss = 3`, sign bit `1`, extra bits `01` (byte `0b101_00000 = 0xA0`):
-- `2^2 + 1 = 5`.
#guard jpgCommonRunDecodeInt 3 0xA0 == 5

-- `ssss = 3`, sign bit `0`, extra bits `11` (byte `0b011_00000 = 0x60`):
-- `1 - 2·2^2 + 3 = -4`.
#guard jpgCommonRunDecodeInt 3 0x60 == -4

-- ── Full DC coefficient decoding ──

private def jpgCommonRunDcDecode (tree : HuffmanTree) (byte : UInt8) : DcCoefficient :=
  (runBoolReaderWith (initBoolStateJpg (ByteArray.mk #[byte])) (dcCoefficientDecode tree)).1

-- Category `0` (`DefaultTable`'s standard DC luminance tree, code bit `0`):
-- the coefficient is `0`, no extra bits consumed.
#guard jpgCommonRunDcDecode defaultDcLumaHuffmanTree 0x00 == 0

-- `DefaultTable`'s standard DC chrominance tree assigns symbol `1`
-- (category `1`) the code `01`; the coefficient's single sign bit follows.
-- Bits `01` + sign bit `1` (byte `0b011_00000 = 0x60`) decode category `1`,
-- then `decodeInt 1` with the sign bit set: `2^0 + 0 = 1`.
#guard jpgCommonRunDcDecode defaultDcChromaHuffmanTree 0x60 == 1

-- Bits `01` + sign bit `0` (byte `0b010_00000 = 0x40`): `decodeInt 1` with
-- the sign bit clear: `1 - 2·2^0 + 0 = -1`.
#guard jpgCommonRunDcDecode defaultDcChromaHuffmanTree 0x40 == (-1 : DcCoefficient)

-- ── AC run-length/category nibble splitting ──

private def jpgCommonRunDecodeRrrrSsss (tree : HuffmanTree) (byte : UInt8) : Nat × Nat :=
  (runBoolReaderWith (initBoolStateJpg (ByteArray.mk #[byte])) (decodeRrrrSsss tree)).1

#guard jpgCommonRunDecodeRrrrSsss jpgCommonTinyTree 0x00 == (1, 1)
#guard jpgCommonRunDecodeRrrrSsss jpgCommonTinyTree 0x80 == (2, 3)

-- ── Restart-interval placeholder ──

#guard runBoolReader decodeRestartInterval == (-1 : Int)

-- ── MCU geometry ──

#guard toBlockSize 0 == 0
#guard toBlockSize 1 == 1
#guard toBlockSize 8 == 1
#guard toBlockSize 9 == 2
#guard toBlockSize 640 == 80

-- ── Pixel clamping ──

#guard pixelClamp (-5 : Int16) == 0
#guard pixelClamp (300 : Int16) == 255
#guard pixelClamp (128 : Int16) == 128

-- ── Dequantization ──

def jpgCommonQuantTable : MacroBlock Int16 := Array.replicate 64 (2 : Int16)
def jpgCommonQuantBlock : MacroBlock Int16 := Array.replicate 64 (3 : Int16)

#guard decide (deQuantize jpgCommonQuantTable jpgCommonQuantBlock = Array.replicate 64 (6 : Int16))

-- ── Final pixel unpacking ──

def jpgCommonBlankImage : Image PixelYCbCr8 :=
  { width := 16, height := 16, data := Array.replicate (3 * 16 * 16) (0 : UInt8) }

def jpgCommonUnpackedImage : Image PixelYCbCr8 :=
  unpackMacroBlock 3 1 1 0 0 0 jpgCommonBlankImage (Array.replicate 64 (100 : Int16))

-- The affected 8×8 top-left region, component `0`, is filled with the
-- clamped decoded value.
#guard jpgCommonUnpackedImage.data.getD ((0 + 0 * 16) * 3 + 0) 255 == 100
#guard jpgCommonUnpackedImage.data.getD ((7 + 7 * 16) * 3 + 0) 255 == 100

-- Component `1` (a different `compIdx`) is untouched.
#guard jpgCommonUnpackedImage.data.getD ((0 + 0 * 16) * 3 + 1) 255 == 0

-- Pixels outside the 8×8 block (here, `(8, 8)`) are untouched.
#guard jpgCommonUnpackedImage.data.getD ((8 + 8 * 16) * 3 + 0) 255 == 0
