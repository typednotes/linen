import Linen.Codec.Picture.BitWriter
import Linen.Codec.Picture.Jpg.Internal.Common
import Linen.Codec.Picture.Jpg.Internal.DefaultTable
import Linen.Codec.Picture.Jpg.Internal.FastIdct
import Linen.Codec.Picture.Jpg.Internal.Types
import Linen.Codec.Picture.Types

/-!
  Port of `Codec.Picture.Jpg.Internal.Progressive` from the `JuicyPixels`
  package (see `docs/imports/JuicyPixels/dependencies.md`, module 26 of 29).
  Progressive-mode JPEG decoding: unlike baseline JPEG (where each 8×8
  block's 64 DCT coefficients are fully Huffman-decoded in a single pass),
  progressive JPEG spreads the coefficients of every block across several
  *scans* — a first DC scan, then zero or more DC-refinement scans, then one
  or more AC scans, each either covering a spectral-frequency *band*
  (`coefficientRange`, ITU-T.81's "spectral selection") or adding one more
  bit of precision to coefficients already written by an earlier scan
  (`successiveApprox`, "successive approximation"). Every scan is fed to one
  of four decode steps (`decodeFirstDC`/`decodeRefineDc`/`decodeFirstAc`/
  `decodeRefineAc`, selected by `runUnpacker`), which read from/write into a
  **coefficient accumulator** (`ComponentData.componentBlocks`, one
  64-element `MacroBlock Int16` per (component, MCU-row-position) pair) that
  persists and is refined across every scan; only after the *last* scan has
  been processed for a given row of MCUs are those blocks dequantized,
  IDCT'd, and unpacked into pixels (`Common.decodeMacroBlock`/
  `unpackMacroBlock`, modules 23/24).

  ## Termination: every loop here is a bounded `for`, not custom recursion

  This is deliberately **not** a literal transcription of upstream's
  `BoolReader s`-monad-recursive helpers (`unpack`, `performEobRun`,
  `updateCoeffs`, `lineMap`, `rasterMap`, `parseFrames`-style traversal). Every
  one of upstream's "recurse until a data-dependent stopping condition"
  helpers is *also* bounded by a statically-known quantity available before
  decoding starts:

  - The number of scans (`scanCount`), the number of MCU rows/columns
    (`imageMcuHeight`/`imageMcuWidth`), and the number of `JpgUnpackerParameter`s
    in one scan's list are all known from the already-parsed `JpgFrameHeader`/
    `JpgScanHeader`s before a single bit of entropy-coded data is read — so
    the outer "scans × MCU rows × MCU columns × params-per-scan" traversal
    (upstream's `lineMap`/`V.forM_`/`forM_`) is one plain nested `for _ in
    [0:n] do` loop per level, exactly as `Common.lean`'s `rasterMap`/
    `unpackMacroBlock` already do. Lean's `for` over a `Std.Range` needs no
    `termination_by` at all — it is already a total library primitive.
  - The genuinely data-dependent loops — `decodeFirstAc`'s `unpack`,
    `decodeRefineAc`'s `unpack`/`performEobRun`/`updateCoeffs` — all walk a
    coefficient index `idx` through `[coefficientRangeStart, coefficientRangeEnd]`,
    and `coefficientRangeEnd ≤ 63` always (one block has exactly
    `dctBlockSize = 64` coefficients, ITU-T.81's fixed 8×8 block size). Every
    step of every one of these loops is checked, by inspection of upstream's
    own recursive calls, to strictly increase `idx` by at least `1` (the
    smallest advance is `unpack $ idx' + 1` after `updateCoeffs`, which itself
    never decreases `idx`; the largest, the `0xF, 0` "skip 16 zero
    coefficients" escape, increases it by `16`). So each such loop needs at
    most `dctBlockSize` iterations to either exhaust the block or hit its own
    "done" condition (end-of-band marker, EOB-run escape, …) — ported as a
    bounded `for _ in [0:dctBlockSize] do` loop over a mutable `idx`/`done`
    pair (the same "bounded loop with an early-exit flag" pattern already
    used by module 18's GIF LZW decoder and module 20's GIF block parser),
    rather than as open-ended recursion needing a fresh well-founded measure.
    No part of this module needed the "restart-interval-driven resync" escape
    hatch the task description anticipated as a fallback — the coefficient
    range bound above was sufficient everywhere.

  ## Design and scope decisions

  - **`JpgUnpackerParameter` drops `HuffmanPackedTree` for the plain
    `HuffmanTree`.** Per `Common.lean`'s own doc-comment, this port never
    builds the packed/flattened decode table upstream's `packHuffmanTree`
    produces (`DefaultTable.lean` decided the same); `dcHuffmanTree`/
    `acHuffmanTree` here are the unpacked `HuffmanTree`s consumed by
    `Common.huffmanDecode`/`decodeRrrrSsss`/`dcCoefficientDecode`.
  - **The phantom `a`/`Unpacker s` type parameter upstream threads through
    `[([(JpgUnpackerParameter, a)], L.ByteString)]` is dropped.** It exists
    upstream only so `progressiveUnpack` can share its scan-list shape with
    the *baseline* decoder's per-MCU pixel-unpacking callback
    (`Codec.Picture.Jpg`'s `Unpacker s`, module 27); `progressiveUnpack`
    itself never reads or calls it (every one of `decodeFirstDC`/
    `decodeRefineDc`/`decodeFirstAc`/`decodeRefineAc`'s own second argument
    is an ignored `_`/`a`). This port's scan list is therefore the simpler
    `List (List JpgUnpackerParameter × ByteArray)`.
  - **`Int32` eobrun/coefficient-decode results become plain `Int`.** As in
    `Common.lean`'s `decodeInt`, no progressive-mode `RRRR`/`SSSS` category or
    end-of-band run length ever approaches `Int32`'s range for legitimate
    JPEG input (a run length is bounded by `2^15`-ish at the very most,
    for a pathological `SSSS = 15` category), so there is no observable
    wraparound behaviour to preserve.
  - **`decodeInt`'s successive-approximation left shift (`` `unsafeShiftL`
    low ``) reuses `Jpg.Internal.FastIdct.idctAshl`** (already available via
    the `FastIdct` import chain) rather than re-deriving a second `Int`
    left-shift helper — it is exactly `fun n k => n * 2 ^ k`, the same
    operation `FastIdct.lean` already introduced and documented (there, for
    the IDCT's own fixed-point scaling).
  - **`Data.Vector`'s `V.unsafeThaw`/`M.STVector`/mutable `ST` machinery
    collapses to plain persistent `Array`s threaded through `Id.run`
    `let mut` bindings**, per this port's established precedent (`BitWriter`,
    `Common.unpackMacroBlock`, …) — there is no `ST`-region distinction to
    preserve in Lean.
  - **`prepareUnpacker`'s per-scan "which of the four decode steps applies"
    dispatch (`selection`) is ported as `runUnpacker`**, a single function
    pattern-matching on `(successiveApprox, coefficientRange)` exactly as
    upstream's `selection` does, rather than upstream's closure-capturing
    partial application (Lean's dispatch is by value, not by building a
    per-scan function value up front, since nothing here needs to be
    memoised across MCUs the way upstream's one-time `prepareUnpacker` pass
    memoises the *selection*, not its *result*).
  - **Restart-interval handling.** `decodeRestartInterval` (module 23,
    `Common.lean`) is upstream's own dead code — it unconditionally returns
    `-1`, the "real" implementation (checking for eight-set-bits followed by
    a genuine restart marker) being commented out in `Common.hs` itself. This
    module's `processRestartInterval` still ports the *scaffolding* around
    it faithfully (byte-aligning, resetting the per-reader EOB run and the
    global DC predictors when a scan's restart counter reaches `0`, and
    decrementing every scan's counter every MCU column) — only the "detect
    and skip an actual `RSTn` marker in the bitstream" step is the
    already-inert one-liner it is upstream.
  - **`Image` initialisation.** Upstream allocates a fresh
    `MutableImage`/`STVector` filled with `128` (JPEG's neutral mid-grey
    level shift, matching `FastIdct.mutableLevelShift`'s own `+128`) and
    mutates it into the caller-visible result; this port builds the
    equivalent `Image PixelYCbCr8` value (`Types.lean`'s persistent `Array`
    encoding) the same way, via `Id.run`/`Array.set!`.
-/

namespace Codec.Picture.Jpg.Internal

open Codec.Picture (Image PixelYCbCr8)

-- ── Per-scan-component unpacking parameters ──

/-- Everything one progressive-mode scan needs to know to decode (or
    refine) one component's coefficients: which Huffman trees to use, which
    running DC predictor/entropy-coded reader/EOB-run counter it shares with
    its scan, which spectral band or successive-approximation bit it
    contributes, and where in the per-MCU-row coefficient accumulator its
    decoded block belongs. Ports upstream's `JpgUnpackerParameter` (from
    `Codec.Picture.Jpg.Internal.Common`, but placed here since this is the
    first — and, per the dependency plan, only — module that constructs or
    consumes it; see the module doc-comment for why `HuffmanPackedTree`
    becomes the plain `HuffmanTree`). -/
structure JpgUnpackerParameter where
  /-- The DC Huffman tree used by DC-first decoding (`Empty`/unused for
      every other decode step). -/
  dcHuffmanTree : HuffmanTree
  /-- The AC Huffman tree used by AC-first/AC-refine decoding. -/
  acHuffmanTree : HuffmanTree
  /-- Which image component (0-based) this parameter decodes into. -/
  componentIndex : Nat
  /-- The scan's declared restart interval, or `-1` if none. -/
  restartInterval : Int
  /-- The component's horizontal sampling factor. -/
  componentWidth : Nat
  /-- The component's vertical sampling factor. -/
  componentHeight : Nat
  /-- `(maxH - componentWidth + 1, maxV - componentHeight + 1)`: the pixel
      up-sampling factor for this component. -/
  subSampling : Nat × Nat
  /-- `(spectralSelectionStart, spectralSelectionEnd)`: the inclusive
      coefficient-index band this scan covers. -/
  coefficientRange : Nat × Nat
  /-- `(successiveApproxLow, successiveApproxHigh)`. -/
  successiveApprox : Nat × Nat
  /-- Which scan-blob (and therefore which `BoolState`/EOB-run counter) this
      parameter's bits are read from. -/
  readerIndex : Nat
  /-- `0` for a single-component scan (plain raster order), `1` for a
      multi-component scan (interleaved MCU order); selects between
      `ComponentData.indicesSolo`/`indicesMulti`. -/
  indiceVector : Nat
  /-- This component's sub-block offset within one MCU. -/
  blockIndex : Nat
  /-- This component's sub-block column within one MCU. -/
  blockMcuX : Nat
  /-- This component's sub-block row within one MCU. -/
  blockMcuY : Nat
  deriving Repr, DecidableEq

-- ── DC-scan decode steps ──

/-- Decode one DC coefficient in a DC-first scan: read the delta-from-
    previous-block category+value via `dcCoefficientDecode`, add it to the
    running per-component DC predictor, and write the successive-
    approximation-scaled result into `block[0]`. Ports upstream's
    `decodeFirstDC`; `eobrun` passes through unchanged (a DC scan has no EOB
    run of its own). -/
def decodeFirstDC (params : JpgUnpackerParameter) (dcCoeffs : Array Int16)
    (block : MacroBlock Int16) (eobrun : Int) :
    BoolReader (Array Int16 × MacroBlock Int16 × Int) := do
  let dcDelta ← dcCoefficientDecode params.dcHuffmanTree
  let previousDc := dcCoeffs.getD params.componentIndex 0
  let neoDc := previousDc + dcDelta
  let approxLow := params.successiveApprox.1
  let scaledDc := neoDc <<< Int16.ofNat approxLow
  pure (dcCoeffs.set! params.componentIndex neoDc, block.set! 0 scaledDc, eobrun)

/-- Refine an already-decoded DC coefficient by one more successive-
    approximation bit: read one bit, and if set, OR in `1 <<< approxLow`.
    Ports upstream's `decodeRefineDc`. -/
def decodeRefineDc (params : JpgUnpackerParameter) (block : MacroBlock Int16)
    (eobrun : Int) : BoolReader (MacroBlock Int16 × Int) := do
  let plusOne : Int16 := (1 : Int16) <<< Int16.ofNat params.successiveApprox.1
  let bit ← getNextBitJpg
  let block' := if bit then block.set! 0 (block.getD 0 0 ||| plusOne) else block
  pure (block', eobrun)

-- ── AC-scan decode steps ──

/-- Decode one AC-first scan's contribution to `block`: if a previous scan
    already declared an EOB run covering this block, just consume one unit
    of it. Otherwise walk the coefficient range from `coefficientRange.1`,
    reading one `RRRR`/`SSSS` Huffman symbol per step:
    - `(0xF, 0)`: skip 16 zero coefficients (the "band-end run" escape).
    - `(0, 0)`: end of block, no further coefficients this scan.
    - `(r, 0)`, `r ∉ {0, 0xF}`: declare an EOB run of
      `2^r - 1 + (r extra bits)` blocks (including this one) and stop.
    - `(r, s)`, `s ≠ 0`: skip `r` zero coefficients, then write the next
      coefficient (its `s`-bit value, successive-approximation-scaled) at
      the resulting index.
    Bounded to `dctBlockSize` iterations — see the module doc-comment for why
    that many is always enough. Ports upstream's `decodeFirstAc`. -/
def decodeFirstAc (params : JpgUnpackerParameter) (block : MacroBlock Int16)
    (eobrun : Int) : BoolReader (MacroBlock Int16 × Int) := do
  if eobrun > 0 then
    pure (block, eobrun - 1)
  else
    let (startIndex, maxIndex) := params.coefficientRange
    let low := params.successiveApprox.1
    let mut blk := block
    let mut idx := startIndex
    let mut done := false
    let mut resultEobrun : Int := 0
    for _ in [0:dctBlockSize] do
      if !done && idx ≤ maxIndex then
        let (r, s) ← decodeRrrrSsss params.acHuffmanTree
        if r == 0xF && s == 0 then
          idx := idx + 16
        else if r == 0 && s == 0 then
          done := true
        else if s == 0 then
          let lowBits ← unpackInt r
          resultEobrun := idctAshl 1 r - 1 + (lowBits.toNat : Int)
          done := true
        else
          let idx' := idx + r
          let v ← decodeInt s
          if idx' ≤ maxIndex then
            blk := blk.set! idx' (Int16.ofInt (idctAshl v low))
          idx := idx' + 1
    pure (blk, resultEobrun)

/-- Refine coefficients from an earlier AC scan by one more successive-
    approximation bit while `idx ≤ maxIndex`: read a correction bit for
    every already-nonzero coefficient encountered (adding/subtracting
    `1 <<< low` the first time its top approximation bit turns on), leaving
    zero coefficients alone. Always advances `idx` by exactly `1`, so is
    bounded by `dctBlockSize` iterations. Ports upstream's
    `decodeRefineAc.performEobRun`. -/
private def refineAcEobRun (maxIndex : Nat) (plusOne minusOne : Int16)
    (block : MacroBlock Int16) (startIdx : Nat) : BoolReader (MacroBlock Int16) := do
  let mut blk := block
  let mut idx := startIdx
  for _ in [0:dctBlockSize] do
    if idx ≤ maxIndex then
      let coeff := blk.getD idx 0
      if coeff != 0 then
        let bit ← getNextBitJpg
        if bit && coeff &&& plusOne == 0 then
          let newVal := if coeff ≥ 0 then coeff + plusOne else coeff + minusOne
          blk := blk.set! idx newVal
      idx := idx + 1
  pure blk

/-- Skip forward over `r` zero coefficients (refining every nonzero
    coefficient passed over along the way, exactly like `refineAcEobRun`),
    starting from `idx`, stopping once `r` zero coefficients have been found
    or `idx` exceeds `maxIndex`. Returns the index of the `r`-th zero
    coefficient found (or, if `idx` ran out first, `idx` itself) — the
    position `decodeRefineAc`'s caller should write its new coefficient at.
    Bounded by `dctBlockSize` iterations, since `idx` strictly increases
    every step and never exceeds `maxIndex < dctBlockSize`. Ports upstream's
    `decodeRefineAc.updateCoeffs`. -/
private def refineAcUpdateCoeffs (maxIndex : Nat) (plusOne minusOne : Int16)
    (block : MacroBlock Int16) (r idx : Nat) :
    BoolReader (MacroBlock Int16 × Nat) := do
  let mut blk := block
  let mut i := idx
  let mut remaining : Int := (r : Int)
  let mut done := false
  let mut result := idx
  for _ in [0:dctBlockSize] do
    if !done then
      if remaining < 0 then
        result := i - 1
        done := true
      else if i > maxIndex then
        result := i
        done := true
      else
        let coeff := blk.getD i 0
        if coeff != 0 then
          let bit ← getNextBitJpg
          if bit && coeff &&& plusOne == 0 then
            let newVal := if coeff ≥ 0 then coeff + plusOne else coeff + minusOne
            blk := blk.set! i newVal
          i := i + 1
        else
          remaining := remaining - 1
          i := i + 1
  pure (blk, result)

/-- Decode one AC-refinement scan's contribution to `block`. If a previous
    scan already declared an EOB run covering this block, refine every
    already-nonzero coefficient from `coefficientRange.1` onward (via
    `refineAcEobRun`) and consume one unit of the run. Otherwise walk the
    coefficient range reading one `RRRR`/`SSSS` symbol per step:
    - `(0xF, 0)`: skip 15 zero coefficients (refining nonzero ones along the
      way, via `refineAcUpdateCoeffs`), continue from just past them.
    - `(r, 0)` (any `r`, since `(0xF, 0)` above already matched that case):
      declare an EOB run of `2^r - 1 + (r extra bits)`, refine every
      remaining nonzero coefficient in the block, and stop.
    - `(r, s)`, `s ≠ 0`: read this new coefficient's sign bit, skip `r` zero
      coefficients (refining nonzero ones along the way), and if the
      resulting index is still in range, write the new coefficient there.
    Bounded to `dctBlockSize` iterations (`idx` strictly increases by at
    least `1` every step, by inspection of every branch above). Ports
    upstream's `decodeRefineAc`. -/
def decodeRefineAc (params : JpgUnpackerParameter) (block : MacroBlock Int16)
    (eobrun : Int) : BoolReader (MacroBlock Int16 × Int) := do
  let (startIndex, maxIndex) := params.coefficientRange
  let low := params.successiveApprox.1
  let plusOne : Int16 := (1 : Int16) <<< Int16.ofNat low
  let minusOne : Int16 := (-1 : Int16) <<< Int16.ofNat low
  if eobrun != 0 then do
    let blk ← refineAcEobRun maxIndex plusOne minusOne block startIndex
    pure (blk, eobrun - 1)
  else do
    let mut blk := block
    let mut idx := startIndex
    let mut done := false
    let mut resultEobrun : Int := 0
    for _ in [0:dctBlockSize] do
      if !done && idx ≤ maxIndex then
        let (r, s) ← decodeRrrrSsss params.acHuffmanTree
        if r == 0xF && s == 0 then
          let (blk', idx') ← refineAcUpdateCoeffs maxIndex plusOne minusOne blk 0xF idx
          blk := blk'
          idx := idx' + 1
        else if s == 0 then
          let lowBits ← unpackInt r
          resultEobrun := idctAshl 1 r + (lowBits.toNat : Int) - 1
          blk ← refineAcEobRun maxIndex plusOne minusOne blk idx
          done := true
        else
          let bit ← getNextBitJpg
          let val : Int16 := if bit then plusOne else minusOne
          let (blk', idx') ← refineAcUpdateCoeffs maxIndex plusOne minusOne blk r idx
          blk := if idx' ≤ maxIndex then blk'.set! idx' val else blk'
          idx := idx' + 1
    pure (blk, resultEobrun)

-- ── Dispatch ──

/-- Select and run one of the four decode steps above, exactly as upstream's
    `prepareUnpacker.selection` does: `successiveApprox.2 = 0` (approx-high)
    means a "first" scan, otherwise a "refine" scan; `coefficientRange.1 = 0`
    (spectral-selection start) means the DC coefficient, otherwise an AC
    band. -/
def runUnpacker (params : JpgUnpackerParameter) (dcCoeffs : Array Int16)
    (block : MacroBlock Int16) (eobrun : Int) :
    BoolReader (Array Int16 × MacroBlock Int16 × Int) :=
  if params.successiveApprox.2 == 0 then
    if params.coefficientRange.1 == 0 then
      decodeFirstDC params dcCoeffs block eobrun
    else do
      let (blk, eob) ← decodeFirstAc params block eobrun
      pure (dcCoeffs, blk, eob)
  else
    if params.coefficientRange.1 == 0 then do
      let (blk, eob) ← decodeRefineDc params block eobrun
      pure (dcCoeffs, blk, eob)
    else do
      let (blk, eob) ← decodeRefineAc params block eobrun
      pure (dcCoeffs, blk, eob)

-- ── Per-MCU-row coefficient-accumulator geometry ──

/-- The two index-vector variants for finding a component's block within
    the current MCU row's `componentBlocks` accumulator: raster order for a
    single-component scan (`indicesSolo`), interleaved MCU order for a
    multi-component scan (`indicesMulti`). Ports upstream's
    `createMcuLineIndices`. -/
def createMcuLineIndices (comp : JpgComponent) (imgWidth mcuWidth : Nat) :
    Array Nat × Array Nat :=
  Id.run do
    let compW := comp.horizontalSamplingFactor.toNat
    let compH := comp.verticalSamplingFactor.toNat
    let imageBlockSize := toBlockSize imgWidth
    let mut indicesSolo : Array Nat := #[]
    for y in [0:compH] do
      let base := y * mcuWidth * compW
      for x in [0:imageBlockSize] do
        indicesSolo := indicesSolo.push (base + x)
    let mut indicesMulti : Array Nat := #[]
    for mcu in [0:mcuWidth] do
      for y in [0:compH] do
        for x in [0:compW] do
          indicesMulti := indicesMulti.push ((mcu + y * mcuWidth) * compW + x)
    pure (indicesSolo, indicesMulti)

/-- The per-component coefficient accumulator: every partially-decoded
    64-coefficient block for one component's current MCU row, plus the
    index-vector pair used to find a given MCU/sub-block's position within
    it. Ports upstream's `ComponentData`. -/
structure ComponentData where
  indicesSolo : Array Nat
  indicesMulti : Array Nat
  componentBlocks : Array (MacroBlock Int16)
  componentId : Nat
  componentBlockCount : Nat

/-- Allocate one component's (zeroed) coefficient accumulator. The `* 2`
    factor in `blockCount` is upstream's own headroom (its comment gives no
    further rationale beyond the multi-component-scan interleaving above
    needing more than one MCU row's worth of raw slots); ported unchanged,
    not re-derived. Ports upstream's `progressiveUnpack.allocateWorkingBlocks`. -/
def allocateWorkingBlocks (imgWidth mcuWidth : Nat) (ix : Nat) (comp : JpgComponent) :
    ComponentData :=
  let hSample := comp.horizontalSamplingFactor.toNat
  let vSample := comp.verticalSamplingFactor.toNat
  let blockCount := hSample * vSample * mcuWidth * 2
  let (indicesSolo, indicesMulti) := createMcuLineIndices comp imgWidth mcuWidth
  { indicesSolo, indicesMulti
    componentBlocks := Array.replicate blockCount (Array.replicate dctBlockSize (0 : Int16))
    componentId := ix
    componentBlockCount := hSample * vSample }

-- ── Top-level progressive decode ──

/-- Decode a full progressive-mode JPEG scan sequence into an image.

    - `(maxiW, maxiH)`: the frame's maximum horizontal/vertical component
      sampling factors (computed by the caller from `frame.components`).
    - `frame`: the already-parsed frame header (component list, dimensions).
    - `quants`: the quantization tables, indexed by destination (clamped to
      `≤ 3`, matching upstream's `min 3 quantId`).
    - `scans`: one entry per `SOS` scan blob: the `JpgUnpackerParameter`s for
      every (component, MCU-sub-block) pair the scan touches, paired with
      that scan's raw (byte-stuffing-undone-boundary) entropy-coded bytes.

    Ports upstream's `progressiveUnpack`; see the module doc-comment for the
    bounded-loop restructuring used throughout. -/
def progressiveUnpack (maxiW maxiH : Nat) (frame : JpgFrameHeader)
    (quants : Array (MacroBlock Int16))
    (scans : List (List JpgUnpackerParameter × ByteArray)) : Image PixelYCbCr8 :=
  Id.run do
    let components := frame.components.toArray
    let imgComponentCount := components.size
    let imgWidth := frame.width.toNat
    let imgHeight := frame.height.toNat
    let imageBlockWidth := toBlockSize imgWidth
    let imageBlockHeight := toBlockSize imgHeight
    let imageMcuWidth := (imageBlockWidth + maxiW - 1) / maxiW
    let imageMcuHeight := (imageBlockHeight + maxiH - 1) / maxiH

    let defaultComponent : JpgComponent :=
      { identifier := 0, horizontalSamplingFactor := 1, verticalSamplingFactor := 1,
        quantizationTableDest := 0 }
    let defaultComponentData : ComponentData :=
      { indicesSolo := #[], indicesMulti := #[], componentBlocks := #[], componentId := 0,
        componentBlockCount := 0 }

    let mut allBlocks : Array ComponentData := #[]
    for ix in [0:imgComponentCount] do
      let comp := components.getD ix defaultComponent
      allBlocks := allBlocks.push (allocateWorkingBlocks imgWidth imageMcuWidth ix comp)

    let scanArr := scans.toArray
    let scanCount := scanArr.size
    let mut readers : Array BoolState := #[]
    for i in [0:scanCount] do
      let (_, ecs) := scanArr.getD i ([], ByteArray.empty)
      readers := readers.push (initBoolStateJpg ecs)
    let restartIntervalValue : Int :=
      match scans with
      | (p :: _, _) :: _ => p.restartInterval
      | _ => -1
    let mut eobRuns : Array Int := Array.replicate scanCount 0
    let mut restartIntervals : Array Int := Array.replicate scanCount restartIntervalValue
    let mut dcCoeffs : Array Int16 := Array.replicate imgComponentCount 0
    let mut writeIndices : Array Nat := Array.replicate imgComponentCount 0

    let elementCount := imgWidth * imgHeight * imgComponentCount
    let mut img : Image PixelYCbCr8 :=
      { width := imgWidth, height := imgHeight, data := Array.replicate elementCount 128 }

    for mmY in [0:imageMcuHeight] do
      -- Reset every component's coefficient accumulator for this MCU row.
      for ci in [0:imgComponentCount] do
        let cd := allBlocks.getD ci defaultComponentData
        allBlocks := allBlocks.set! ci
          { cd with componentBlocks :=
              Array.replicate cd.componentBlocks.size (Array.replicate dctBlockSize (0 : Int16)) }
      writeIndices := Array.replicate imgComponentCount 0

      for _mmx in [0:imageMcuWidth] do
        -- Restart-interval bookkeeping for every scan (see module doc-comment).
        for scanIx in [0:scanCount] do
          let v := restartIntervals.getD scanIx restartIntervalValue
          if v == 0 then
            if scanIx == 0 then
              dcCoeffs := Array.replicate imgComponentCount 0
            let reader := readers.getD scanIx emptyBoolState
            let (_, updated) := runBoolReaderWith reader (do byteAlignJpg; decodeRestartInterval)
            readers := readers.set! scanIx updated
            eobRuns := eobRuns.set! scanIx 0
            restartIntervals := restartIntervals.set! scanIx (restartIntervalValue - 1)
          else
            restartIntervals := restartIntervals.set! scanIx (v - 1)

        -- Decode this MCU's contribution from every scan's parameters.
        for scanIx in [0:scanCount] do
          let (paramsList, _) := scanArr.getD scanIx ([], ByteArray.empty)
          for param in paramsList do
            let boolState := readers.getD param.readerIndex emptyBoolState
            let eobrun := eobRuns.getD param.readerIndex 0
            let cd := allBlocks.getD param.componentIndex defaultComponentData
            let writeIndex := writeIndices.getD param.componentIndex 0
            let indexVector := if param.indiceVector == 0 then cd.indicesSolo else cd.indicesMulti
            if writeIndex + param.blockIndex < indexVector.size then
              let realIndex := indexVector.getD (writeIndex + param.blockIndex) 0
              let writeBlock := cd.componentBlocks.getD realIndex (Array.replicate dctBlockSize 0)
              let ((newDc, newBlock, newEobrun), newState) :=
                runBoolReaderWith boolState (runUnpacker param dcCoeffs writeBlock eobrun)
              dcCoeffs := newDc
              readers := readers.set! param.readerIndex newState
              eobRuns := eobRuns.set! param.readerIndex newEobrun
              allBlocks := allBlocks.set! param.componentIndex
                { cd with componentBlocks := cd.componentBlocks.set! realIndex newBlock }

        -- Advance every component's write cursor by one MCU's worth of blocks.
        for ci in [0:imgComponentCount] do
          let cd := allBlocks.getD ci defaultComponentData
          let prev := writeIndices.getD cd.componentId 0
          writeIndices := writeIndices.set! cd.componentId (prev + cd.componentBlockCount)

      -- This MCU row is now fully decoded for every component: dequantize,
      -- inverse-DCT, and unpack every accumulated block into pixels.
      for ci in [0:imgComponentCount] do
        let cd := allBlocks.getD ci defaultComponentData
        let comp := components.getD cd.componentId defaultComponent
        let quantId := comp.quantizationTableDest.toNat
        let table := quants.getD (min 3 quantId) (Array.replicate dctBlockSize 1)
        let compW := comp.horizontalSamplingFactor.toNat
        let compH := comp.verticalSamplingFactor.toNat
        let cw8 := maxiW - comp.horizontalSamplingFactor.toNat + 1
        let ch8 := maxiH - comp.verticalSamplingFactor.toNat + 1
        for y in [0:compH] do
          for rx in [0:imageMcuWidth * compW] do
            let ry := mmY * maxiH + y
            let blockIdx := y * imageMcuWidth * compW + rx
            let block := cd.componentBlocks.getD blockIdx (Array.replicate dctBlockSize 0)
            let transformed := decodeMacroBlock table block
            img := unpackMacroBlock imgComponentCount cw8 ch8 cd.componentId (rx * cw8) ry img
              transformed

    pure img

end Codec.Picture.Jpg.Internal
