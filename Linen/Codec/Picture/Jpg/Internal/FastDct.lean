import Linen.Codec.Picture.Jpg.Internal.Types

/-!
  Port of `Codec.Picture.Jpg.Internal.FastDct` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 24 of 29, ported
  alongside its sibling `Jpg.Internal.FastIdct`). The **forward** 8×8 DCT
  used by JPEG *encoding*: `referenceDct`, a slow-but-obviously-correct
  floating-point transcription of ITU-T.81's defining formula (kept upstream
  purely as a correctness reference/test oracle), and `fastDctLibJpeg`, the
  libjpeg-derived fixed-point integer butterfly actually used to encode.

  This module depends on `Jpg.Internal.Types` only (for `MacroBlock`,
  `dctBlockSize`), per the dependency plan; it has no dependency on
  `Jpg.Internal.Common` or any other JPEG decoding module, matching
  upstream's own import list.

  ## Design and scope decisions

  - **`MutableMacroBlock s Int32`/`MutableMacroBlock s Int16` (upstream's
    `ST`-mutable in/out buffers) collapse into plain pure `MacroBlock Int`/
    `MacroBlock Int16` functions**, following this port's established
    precedent (`Common.lean`'s `deQuantize`/`zigZagReorder`, `Types.lean`'s
    `Image`): each upstream function that both reads one buffer and writes
    into another becomes a single `Array`-returning pure function (built via
    `Id.run` and `Array.set!` over the fixed 64-element block, per the task's
    "known iteration bounds, no `partial`" guidance) instead of two
    ST-threaded halves.
  - **`Int32` becomes plain `Int`.** Upstream types every accumulator in
    `fastDctLibJpeg` as `Int32` and uses `unsafeShiftL`/`unsafeShiftR` on it;
    for genuine 8-bit-sample JPEG input (the only input this algorithm is
    ever run on — samples are level-shifted `[-128, 127]`, no smaller and no
    larger), every intermediate sum/product in the butterfly stays several
    orders of magnitude below `Int32`'s `±2^31` range (the largest
    intermediate magnitude is a sum of a handful of ~`128`-scale terms times
    a ~`25000`-scale fixed constant, on the order of `10^7`–`10^8`), so
    `Int32`'s wraparound is never actually observable — exactly the same
    reasoning `Common.lean`'s doc-comment gives for porting `decodeInt`'s
    `Int32` as plain `Int`. Using Lean's arbitrary-precision `Int` avoids a
    fixed-width cast at every arithmetic step for no behavioural difference,
    **provided** right shifts are still given arithmetic (sign-extending,
    round-towards-`-∞`) semantics matching Haskell `Int32`'s `unsafeShiftR`
    — done here via `Int.fdiv` (floor division) by the corresponding power
    of two, since Lean's default `Int` `/` truncates towards zero instead
    (verified: `Int.fdiv (-3) 2 = -2`, matching `(-3 :: Int32) \`unsafeShiftR\` 1`).
  - **`referenceDct`'s implicit Haskell `Float`s (GHC's 32-bit single-
    precision float) are ported as `Float32`**, not Lean's default 64-bit
    `Float`, for bit-width fidelity to what upstream actually computes (this
    module doc-comment follows `Types.lean`'s own `PixelF`/`Float32`
    convention). `referenceDct`'s `truncate` (a `Float -> Int32` conversion
    that rounds towards zero) is ported via a small local
    `truncFloat32ToInt` helper built from `Float32.toUInt32` (itself already
    a round-towards-zero conversion, verified on `3.7 → 3`), applied to the
    absolute value and re-signed — there is no direct `Float32 → Int`
    primitive to call instead.
  - **The row-then-column butterfly structure of `fastDctLibJpeg`'s
    `firstPass`/`secondPass` is transcribed field-for-field**: every
    `tmp*`/`z1`/`FIX_*` constant and every shift amount matches upstream
    exactly (`CONST_BITS = 13`, `PASS1_BITS = 2`, the twelve libjpeg
    `FIX(...)` fixed-point cosine constants, and the `CENTERJSAMPLE = 128`
    level-shift subtraction folded into the DC term of `firstPass`).
  - **`secondPass`'s reversed column-index iteration (`i` counting down from
    `7`, column `= 7 - i`) is straightened into a plain ascending loop over
    the column index directly.** Each column's computation only reads and
    writes within that column (`workData[col + row * 8]` for every row), so
    no column's result depends on any other column having been processed
    yet or not; the traversal order upstream uses is an arbitrary artifact
    of how the recursion was written, not part of the algorithm's contract,
    so this reordering changes nothing observable.
-/

namespace Codec.Picture.Jpg.Internal

-- ── Shared bit-precision constants ──

/-- Upstream's `dctBlockSize` (the *linear* block dimension, `8`); see
    `Common.lean`'s doc-comment for why this port keeps the already-claimed
    name `dctBlockSize` for the coefficient *count* (`64`) and introduces a
    distinct name (`blockDim` there) for the dimension. This module names it
    `fastDctBlockDim` instead of a bare `blockDim`, since `Common.lean`,
    `FastDct.lean`, and `FastIdct.lean` all share one namespace
    (`Codec.Picture.Jpg.Internal`) and a bare `blockDim` would collide with
    `Common.lean`'s own definition. -/
def fastDctBlockDim : Nat := 8

/-- Arithmetic (sign-extending) right shift on an unbounded `Int`, matching
    Haskell `Int32`'s `unsafeShiftR` for values that never approach
    `Int32`'s range (see the module doc-comment). -/
def ashr (n : Int) (k : Nat) : Int := Int.fdiv n (2 ^ k)

/-- Left shift on an unbounded `Int` (plain multiplication by a power of
    two; no sign-extension subtlety applies to left shifts). -/
def ashl (n : Int) (k : Nat) : Int := n * 2 ^ k

-- ── Reference (slow, floating-point) DCT ──

/-- Truncate (round towards zero) a `Float32` to an `Int`, matching
    Haskell's `truncate :: Float -> Int32`; built from `Float32.toUInt32`
    (itself round-towards-zero) applied to the absolute value. -/
def truncFloat32ToInt (x : Float32) : Int :=
  if x < 0 then -(((-x).toUInt32.toNat : Int)) else ((x.toUInt32.toNat : Int))

/-- $\pi$, to `Float32` precision, matching upstream's `pi :: Float`. -/
def piF32 : Float32 := 3.14159265358979323846

/-- The DCT-II orthonormalization coefficient $c(0) = 1/\sqrt2$,
    $c(u) = 1$ for $u > 0$. -/
def dctC (u : Nat) : Float32 := if u == 0 then 1 / Float32.sqrt 2 else 1

/-- Reference implementation of the forward DCT, directly implementing
    ITU-T.81's defining formula
    $$F(u,v) = \tfrac14 c(u) c(v) \sum_{x,y} f(x,y)
      \cos\!\big(\tfrac{(2x+1)u\pi}{16}\big) \cos\!\big(\tfrac{(2y+1)v\pi}{16}\big).$$
    Slow (quadratic in the block size per coefficient) but a direct, obvious
    transcription of the formula — upstream's own stated purpose ("accurate
    and a good reference point"), used here the same way: as a correctness
    oracle to check `fastDctLibJpeg`/`fastIdct` against. -/
def referenceDct (block : MacroBlock Int16) : MacroBlock Int :=
  Id.run do
    let mut workData : MacroBlock Int := Array.replicate dctBlockSize (0 : Int)
    for u in [0:fastDctBlockDim] do
      for v in [0:fastDctBlockDim] do
        let mut toSum : Float32 := 0
        for x in [0:fastDctBlockDim] do
          for y in [0:fastDctBlockDim] do
            let sample : Float32 :=
              Float32.ofInt (block.getD (y * fastDctBlockDim + x) 0).toInt
            let cosX :=
              Float32.cos ((2 * (Float32.ofNat x) + 1) * (Float32.ofNat u) * piF32 / 16)
            let cosY :=
              Float32.cos ((2 * (Float32.ofNat y) + 1) * (Float32.ofNat v) * piF32 / 16)
            toSum := toSum + sample * cosX * cosY
        let val : Float32 := (1 / 4 : Float32) * dctC u * dctC v * toSum
        workData := workData.set! (v * fastDctBlockDim + u) (truncFloat32ToInt val)
    pure workData

-- ── Fast integer DCT (libjpeg-derived) ──

/-- `PASS1_BITS`: the extra fixed-point scaling applied after the row
    (first) pass, removed again by the column (second) pass. -/
def pass1Bits : Nat := 2

/-- `CONST_BITS`: the fixed-point precision of the `FIX_*` cosine
    constants below (13 fractional bits). -/
def constBits : Nat := 13

/-- `FIX(0.298631336)`. -/ def fix0_298631336 : Int := 2446
/-- `FIX(0.390180644)`. -/ def fix0_390180644 : Int := 3196
/-- `FIX(0.541196100)`. -/ def fix0_541196100 : Int := 4433
/-- `FIX(0.765366865)`. -/ def fix0_765366865 : Int := 6270
/-- `FIX(0.899976223)`. -/ def fix0_899976223 : Int := 7373
/-- `FIX(1.175875602)`. -/ def fix1_175875602 : Int := 9633
/-- `FIX(1.501321110)`. -/ def fix1_501321110 : Int := 12299
/-- `FIX(1.847759065)`. -/ def fix1_847759065 : Int := 15137
/-- `FIX(1.961570560)`. -/ def fix1_961570560 : Int := 16069
/-- `FIX(2.053119869)`. -/ def fix2_053119869 : Int := 16819
/-- `FIX(2.562915447)`. -/ def fix2_562915447 : Int := 20995
/-- `FIX(3.072711026)`. -/ def fix3_072711026 : Int := 25172

/-- `CENTERJSAMPLE`: the level-shift JPEG samples are centered around
    (`128`), subtracted from the DC term of the row pass. -/
def centerJSample : Int := 128

/-- Pass 1: process rows. Results are scaled up by $\sqrt8$ compared to a
    true DCT, plus a further `2^PASS1_BITS`. Ports `fastDctLibJpeg`'s
    `firstPass`, transcribing every `tmp*`/`FIX_*` term exactly. -/
def fastDctFirstPass (sampleBlock : MacroBlock Int16) : MacroBlock Int :=
  Id.run do
    let mut dataBlock : MacroBlock Int := Array.replicate dctBlockSize (0 : Int)
    for i in [0:fastDctBlockDim] do
      let baseIdx := i * fastDctBlockDim
      let readAt (k : Nat) : Int := (sampleBlock.getD (baseIdx + k) 0).toInt
      let blk0 := readAt 0
      let blk1 := readAt 1
      let blk2 := readAt 2
      let blk3 := readAt 3
      let blk4 := readAt 4
      let blk5 := readAt 5
      let blk6 := readAt 6
      let blk7 := readAt 7

      let tmp0 := blk0 + blk7
      let tmp1 := blk1 + blk6
      let tmp2 := blk2 + blk5
      let tmp3 := blk3 + blk4

      let tmp10 := tmp0 + tmp3
      let tmp12 := tmp0 - tmp3
      let tmp11 := tmp1 + tmp2
      let tmp13 := tmp1 - tmp2

      let tmp0' := blk0 - blk7
      let tmp1' := blk1 - blk6
      let tmp2' := blk2 - blk5
      let tmp3' := blk3 - blk4

      dataBlock := dataBlock.set! (baseIdx + 0)
        (ashl (tmp10 + tmp11 - (fastDctBlockDim : Int) * centerJSample) pass1Bits)
      dataBlock := dataBlock.set! (baseIdx + 4) (ashl (tmp10 - tmp11) pass1Bits)

      let z1 := (tmp12 + tmp13) * fix0_541196100 + ashl 1 (constBits - pass1Bits - 1)

      dataBlock := dataBlock.set! (baseIdx + 2)
        (ashr (z1 + tmp12 * fix0_765366865) (constBits - pass1Bits))
      dataBlock := dataBlock.set! (baseIdx + 6)
        (ashr (z1 - tmp13 * fix1_847759065) (constBits - pass1Bits))

      let tmp10' := tmp0' + tmp3'
      let tmp11' := tmp1' + tmp2'
      let tmp12' := tmp0' + tmp2'
      let tmp13' := tmp1' + tmp3'
      let z1' := (tmp12' + tmp13') * fix1_175875602 + ashl 1 (constBits - pass1Bits - 1)
      let tmp0'' := tmp0' * fix1_501321110
      let tmp1'' := tmp1' * fix3_072711026
      let tmp2'' := tmp2' * fix2_053119869
      let tmp3'' := tmp3' * fix0_298631336
      let tmp10'' := tmp10' * (-fix0_899976223)
      let tmp11'' := tmp11' * (-fix2_562915447)
      let tmp12'' := tmp12' * (-fix0_390180644) + z1'
      let tmp13'' := tmp13' * (-fix1_961570560) + z1'

      dataBlock := dataBlock.set! (baseIdx + 1)
        (ashr (tmp0'' + tmp10'' + tmp12'') (constBits - pass1Bits))
      dataBlock := dataBlock.set! (baseIdx + 3)
        (ashr (tmp1'' + tmp11'' + tmp13'') (constBits - pass1Bits))
      dataBlock := dataBlock.set! (baseIdx + 5)
        (ashr (tmp2'' + tmp11'' + tmp12'') (constBits - pass1Bits))
      dataBlock := dataBlock.set! (baseIdx + 7)
        (ashr (tmp3'' + tmp10'' + tmp13'') (constBits - pass1Bits))
    pure dataBlock

/-- Pass 2: process columns. Removes the `PASS1_BITS` scaling introduced by
    the first pass, but leaves the result scaled up by an overall factor of
    `8`. Ports `fastDctLibJpeg`'s `secondPass`, straightened into a plain
    ascending column loop (see the module doc-comment for why this is a
    behaviour-preserving reordering). -/
def fastDctSecondPass (dataBlock : MacroBlock Int) : MacroBlock Int :=
  Id.run do
    let mut block := dataBlock
    for c in [0:fastDctBlockDim] do
      let readAt (row : Nat) : Int := block.getD (c + row * fastDctBlockDim) 0

      let blk0 := readAt 0
      let blk1 := readAt 1
      let blk2 := readAt 2
      let blk3 := readAt 3
      let blk4 := readAt 4
      let blk5 := readAt 5
      let blk6 := readAt 6
      let blk7 := readAt 7

      let tmp0 := blk0 + blk7
      let tmp1 := blk1 + blk6
      let tmp2 := blk2 + blk5
      let tmp3 := blk3 + blk4

      let tmp10 := tmp0 + tmp3 + ashl 1 (pass1Bits - 1)
      let tmp12 := tmp0 - tmp3
      let tmp11 := tmp1 + tmp2
      let tmp13 := tmp1 - tmp2

      let tmp0' := blk0 - blk7
      let tmp1' := blk1 - blk6
      let tmp2' := blk2 - blk5
      let tmp3' := blk3 - blk4

      block := block.set! (fastDctBlockDim * 0 + c) (ashr (tmp10 + tmp11) (pass1Bits + 3))
      block := block.set! (fastDctBlockDim * 4 + c) (ashr (tmp10 - tmp11) (pass1Bits + 3))

      let z1 := (tmp12 + tmp13) * fix0_541196100 + ashl 1 (constBits + pass1Bits - 1)

      block := block.set! (fastDctBlockDim * 2 + c)
        (ashr (z1 + tmp12 * fix0_765366865) (constBits + pass1Bits + 3))
      block := block.set! (fastDctBlockDim * 6 + c)
        (ashr (z1 - tmp13 * fix1_847759065) (constBits + pass1Bits + 3))

      let tmp10' := tmp0' + tmp3'
      let tmp11' := tmp1' + tmp2'
      let tmp12' := tmp0' + tmp2'
      let tmp13' := tmp1' + tmp3'
      let z1' := (tmp12' + tmp13') * fix1_175875602 + ashl 1 (constBits + pass1Bits - 1)
      let tmp0'' := tmp0' * fix1_501321110
      let tmp1'' := tmp1' * fix3_072711026
      let tmp2'' := tmp2' * fix2_053119869
      let tmp3'' := tmp3' * fix0_298631336
      let tmp10'' := tmp10' * (-fix0_899976223)
      let tmp11'' := tmp11' * (-fix2_562915447)
      let tmp12'' := tmp12' * (-fix0_390180644) + z1'
      let tmp13'' := tmp13' * (-fix1_961570560) + z1'

      block := block.set! (fastDctBlockDim * 1 + c)
        (ashr (tmp0'' + tmp10'' + tmp12'') (constBits + pass1Bits + 3))
      block := block.set! (fastDctBlockDim * 3 + c)
        (ashr (tmp1'' + tmp11'' + tmp13'') (constBits + pass1Bits + 3))
      block := block.set! (fastDctBlockDim * 5 + c)
        (ashr (tmp2'' + tmp11'' + tmp12'') (constBits + pass1Bits + 3))
      block := block.set! (fastDctBlockDim * 7 + c)
        (ashr (tmp3'' + tmp10'' + tmp13'') (constBits + pass1Bits + 3))
    pure block

/-- Fast DCT extracted from libjpeg: `fastDctFirstPass` (rows) followed by
    `fastDctSecondPass` (columns). Ports upstream's `fastDctLibJpeg`. -/
def fastDctLibJpeg (sampleBlock : MacroBlock Int16) : MacroBlock Int :=
  fastDctSecondPass (fastDctFirstPass sampleBlock)

end Codec.Picture.Jpg.Internal
