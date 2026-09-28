import Linen.Codec.Picture.Jpg.Internal.Progressive

/-!
  Tests for `Linen.Codec.Picture.Jpg.Internal.Progressive`: the four
  progressive-scan decode steps (`decodeFirstDC`, `decodeRefineDc`,
  `decodeFirstAc`, `decodeRefineAc`) exercised in isolation against small
  hand-built Huffman trees, the AC end-of-band-run escape and the
  successive-approximation bit-refinement path, the `runUnpacker` dispatch,
  and the per-MCU-row index-vector geometry (`createMcuLineIndices`,
  `allocateWorkingBlocks`).
-/

open Codec.Picture (BoolReader BoolState runBoolReader runBoolReaderWith initBoolStateJpg)
open Codec.Picture.Jpg.Internal

-- ── Shared fixtures ──

/-- A minimal `JpgUnpackerParameter`: every field defaulted to something
    inert, overridden field-by-field by each test below. -/
def jpgProgDefaultParams : JpgUnpackerParameter :=
  { dcHuffmanTree := .empty
    acHuffmanTree := .empty
    componentIndex := 0
    restartInterval := -1
    componentWidth := 1
    componentHeight := 1
    subSampling := (1, 1)
    coefficientRange := (0, 63)
    successiveApprox := (0, 0)
    readerIndex := 0
    indiceVector := 0
    blockIndex := 0
    blockMcuX := 0
    blockMcuY := 0 }

/-- A tiny two-leaf DC Huffman tree: bit `0` selects category `0` (a zero
    delta, no extra bits), bit `1` selects category `1` (one sign-extra
    bit). -/
def jpgProgDcTree : HuffmanTree :=
  .branch (.leaf 0) (.leaf 1)

def jpgProgZeroBlock : MacroBlock Int16 := Array.replicate dctBlockSize 0

private def jpgProgRun (block : BoolReader α) (byte : UInt8) : α :=
  (runBoolReaderWith (initBoolStateJpg (ByteArray.mk #[byte])) block).1

-- ── `decodeFirstDC` ──

-- Category `0` (bit `0`): the delta is `0`, so the running DC predictor and
-- `block[0]` both stay at the previous value (`5`).
#guard
  let dcCoeffs := (#[5] : Array Int16)
  let (newDc, block, eobrun) :=
    jpgProgRun (decodeFirstDC jpgProgDefaultParams dcCoeffs jpgProgZeroBlock 0) 0x00
  newDc.getD 0 999 == 5 && block.getD 0 999 == 5 && eobrun == 0

-- Category `1` (bit `1`) with sign bit `1` (byte `0b1100_0000 = 0xC0`):
-- `decodeInt 1` with the sign bit set is `1`, so the delta is `1` and the
-- new DC predictor is `5 + 1 = 6`.
#guard
  let dcCoeffs := (#[5] : Array Int16)
  let params := { jpgProgDefaultParams with dcHuffmanTree := jpgProgDcTree }
  let (newDc, block, _) := jpgProgRun (decodeFirstDC params dcCoeffs jpgProgZeroBlock 0) 0xC0
  newDc.getD 0 999 == 6 && block.getD 0 999 == 6

-- With `successiveApprox.1 = 2` (a low bit-position of `2`), the same delta
-- is left-shifted by `2` before being written into `block[0]`, but the
-- running (unscaled) DC predictor is unaffected.
#guard
  let dcCoeffs := (#[5] : Array Int16)
  let params :=
    { jpgProgDefaultParams with dcHuffmanTree := jpgProgDcTree, successiveApprox := (2, 0) }
  let (newDc, block, _) := jpgProgRun (decodeFirstDC params dcCoeffs jpgProgZeroBlock 0) 0xC0
  newDc.getD 0 999 == 6 && block.getD 0 999 == 24

-- ── `decodeRefineDc` ──

-- With `successiveApprox.1 = 0` (`plusOne = 1`) and the refinement bit set,
-- `block[0]` gains bit `0`; `eobrun` passes through unchanged.
#guard
  let params := jpgProgDefaultParams
  let (block, eobrun) := jpgProgRun (decodeRefineDc params jpgProgZeroBlock 3) 0x80
  block.getD 0 999 == 1 && eobrun == 3

-- With the refinement bit clear, `block[0]` is untouched.
#guard
  let (block, _) := jpgProgRun (decodeRefineDc jpgProgDefaultParams jpgProgZeroBlock 0) 0x00
  block.getD 0 999 == 0

-- Refining a coefficient that already has that bit set leaves it alone
-- (the "only OR it in the first time it turns on" guard).
#guard
  let block := jpgProgZeroBlock.set! 0 1
  let (block', _) := jpgProgRun (decodeRefineDc jpgProgDefaultParams block 0) 0x80
  block'.getD 0 999 == 1

-- ── `decodeFirstAc`: EOB-run passthrough ──

-- A positive incoming `eobrun` is just consumed by one, with the block
-- (and the entropy-coded stream) untouched.
#guard
  let (block, eobrun) := jpgProgRun (decodeFirstAc jpgProgDefaultParams jpgProgZeroBlock 3) 0x00
  block == jpgProgZeroBlock && eobrun == 2

-- ── `decodeFirstAc`: end of block ──

-- Symbol `(0, 0)` (a bare `.leaf 0x00` tree: no bits consumed) means "end
-- of block, no further coefficients"; the block is untouched and no new
-- EOB run is declared.
#guard
  let params := { jpgProgDefaultParams with acHuffmanTree := .leaf 0x00 }
  let (block, eobrun) := jpgProgRun (decodeFirstAc params jpgProgZeroBlock 0) 0x00
  block == jpgProgZeroBlock && eobrun == 0

-- ── `decodeFirstAc`: EOB-run escape ──

-- Symbol `(1, 0)` (tree `.leaf 0x10`, so no bit is consumed selecting it)
-- followed by `1` extra bit set (the first stream bit, `0x80`) declares an
-- EOB run of `2^1 - 1 + 1 = 2`.
#guard
  let params := { jpgProgDefaultParams with acHuffmanTree := .leaf 0x10 }
  let (block, eobrun) := jpgProgRun (decodeFirstAc params jpgProgZeroBlock 0) 0x80
  block == jpgProgZeroBlock && eobrun == 2

-- ── `decodeFirstAc`: writing a coefficient ──

-- Symbol `(0, 1)` (tree `.leaf 0x01`) writes a category-`1` coefficient at
-- `idx = coefficientRange.1 (start) + 0 = 0`; sign bit `1` (the stream's
-- first bit, since the `.leaf` Huffman decode itself consumes none)
-- decodes to value `1`.
#guard
  let params := { jpgProgDefaultParams with acHuffmanTree := .leaf 0x01 }
  let (block, eobrun) := jpgProgRun (decodeFirstAc params jpgProgZeroBlock 0) 0x80
  block.getD 0 999 == 1 && eobrun == 0

-- ── `decodeRefineAc`: EOB-run refinement of already-nonzero coefficients ──

-- With an incoming `eobrun > 0`, every already-nonzero coefficient in
-- range gets a correction bit. With `successiveApprox.1 = 1`
-- (`plusOne = 1 <<< 1 = 2`), the already-decoded coefficient `block[0] = 1`
-- (bit `0` already set, bit `1` not yet) reads bit `1` and gains `plusOne`,
-- becoming `3`.
#guard
  let block := jpgProgZeroBlock.set! 0 1
  let params := { jpgProgDefaultParams with successiveApprox := (1, 0) }
  let (block', eobrun') := jpgProgRun (decodeRefineAc params block 1) 0x80
  block'.getD 0 999 == 3 && eobrun' == 0

-- A zero coefficient is left alone by the EOB-run refinement pass, and no
-- stream bit is consumed for it.
#guard
  let (block', _) := jpgProgRun (decodeRefineAc jpgProgDefaultParams jpgProgZeroBlock 1) 0x00
  block' == jpgProgZeroBlock

-- ── `decodeRefineAc`: new-coefficient path ──

-- Symbol `(0, 1)` (tree `.leaf 0x01`) with sign bit `1` (`val = plusOne =
-- 1`) writes `1` at `idx = coefficientRange.1 + 0 = 0`.
#guard
  let params := { jpgProgDefaultParams with acHuffmanTree := .leaf 0x01 }
  let (block, eobrun) := jpgProgRun (decodeRefineAc params jpgProgZeroBlock 0) 0x80
  block.getD 0 999 == 1 && eobrun == 0

-- ── `runUnpacker` dispatch ──

-- `successiveApprox.2 = 0`, `coefficientRange.1 = 0`: dispatches to
-- `decodeFirstDC`.
#guard
  let params := { jpgProgDefaultParams with dcHuffmanTree := jpgProgDcTree }
  let (dcCoeffs, block, _) :=
    jpgProgRun (runUnpacker params (#[5] : Array Int16) jpgProgZeroBlock 0) 0xC0
  dcCoeffs.getD 0 999 == 6 && block.getD 0 999 == 6

-- `successiveApprox.2 ≠ 0`, `coefficientRange.1 = 0`: dispatches to
-- `decodeRefineDc`.
#guard
  let params := { jpgProgDefaultParams with successiveApprox := (0, 1) }
  let (dcCoeffs, block, _) :=
    jpgProgRun (runUnpacker params (#[5] : Array Int16) jpgProgZeroBlock 0) 0x80
  dcCoeffs.getD 0 999 == 5 && block.getD 0 999 == 1

-- `successiveApprox.2 = 0`, `coefficientRange.1 ≠ 0`: dispatches to
-- `decodeFirstAc`.
#guard
  let params :=
    { jpgProgDefaultParams with acHuffmanTree := .leaf 0x00, coefficientRange := (1, 63) }
  let (dcCoeffs, block, eobrun) :=
    jpgProgRun (runUnpacker params (#[5] : Array Int16) jpgProgZeroBlock 0) 0x00
  dcCoeffs.getD 0 999 == 5 && block == jpgProgZeroBlock && eobrun == 0

-- `successiveApprox.2 ≠ 0`, `coefficientRange.1 ≠ 0`: dispatches to
-- `decodeRefineAc`.
#guard
  let params :=
    { jpgProgDefaultParams with
      acHuffmanTree := .leaf 0x01, coefficientRange := (0, 63), successiveApprox := (0, 1) }
  let (dcCoeffs, block, eobrun) :=
    jpgProgRun (runUnpacker params (#[5] : Array Int16) jpgProgZeroBlock 0) 0x80
  dcCoeffs.getD 0 999 == 5 && block.getD 0 999 == 1 && eobrun == 0

-- ── MCU-row geometry ──

/-- A single-sampled (`1×1`) component, matching `jpgProgDefaultParams`'s
    implicit assumptions. -/
def jpgProgComponent : JpgComponent :=
  { identifier := 0, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
    quantizationTableDest := 0 }

-- With `imgWidth = 16` (two 8-pixel blocks wide) and `mcuWidth = 2`, the
-- solo (single-component-scan) index vector is one row of
-- `toBlockSize 16 = 2` raster-order indices.
#guard (createMcuLineIndices jpgProgComponent 16 2).1 == #[0, 1]

-- The multi (interleaved-scan) index vector for a `1×1`-sampled component
-- across `mcuWidth = 2` MCUs is just `[0, 1]` as well (one sub-block per
-- MCU, no sub-sampling to interleave).
#guard (createMcuLineIndices jpgProgComponent 16 2).2 == #[0, 1]

-- `allocateWorkingBlocks` zero-fills `componentBlockCount = 1 * 1 = 1`
-- blocks' worth of accumulator (times upstream's `* 2` headroom factor on
-- the raw slot count), each `dctBlockSize` coefficients of `0`.
#guard
  let cd := allocateWorkingBlocks 16 2 0 jpgProgComponent
  cd.componentBlockCount == 1 && cd.componentId == 0 && cd.componentBlocks.size == 4
    && cd.componentBlocks.getD 0 #[] == jpgProgZeroBlock
