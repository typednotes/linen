import Linen.Codec.Picture.Jpg.Internal.Types

/-!
  Port of `Codec.Picture.Jpg.Internal.FastIdct` from the `JuicyPixels`
  package (see `docs/imports/JuicyPixels/dependencies.md`, module 24 of 29,
  ported alongside its sibling `Jpg.Internal.FastDct`). The **inverse** 8×8
  DCT used by JPEG *decoding*: the Chen-Wang algorithm (IEEE ASSP-32,
  pp. 803-816, Aug. 1984), a 32-bit-integer, 11-multiply/29-add fixed-point
  butterfly, plus `mutableLevelShift` (JPEG's `+128` level shift back to
  `[0, 255]` after the IDCT's centered output).

  This module depends on `Jpg.Internal.Types` only (for `MacroBlock`,
  `dctBlockSize`), per the dependency plan; it has no dependency on
  `Jpg.Internal.Common` or any other JPEG decoding module, matching
  upstream's own import list.

  ## Wiring `Common.lean`'s deferred `inverseDirectCosineTransform`/
  `decodeMacroBlock`

  Module 23 (`Common.lean`) deferred these two exports because they call
  straight into `fastIdct`/`mutableLevelShift`, which did not exist until
  this module. Now that `fastIdct`/`mutableLevelShift` are available, this
  is the low-risk, natural-fit case the task calls out: upstream's own
  bodies are one-liners —
  `inverseDirectCosineTransform mBlock = fastIdct mBlock >>= mutableLevelShift`
  and `decodeMacroBlock quant zigZag block = deQuantize quant block >>=
  zigZagReorder zigZag >>= inverseDirectCosineTransform` — that need no
  restructuring to translate into this port's pure-`MacroBlock` style
  (`inverseDirectCosineTransform block := mutableLevelShift (fastIdct
  block)`, `decodeMacroBlock quant block := inverseDirectCosineTransform
  (zigZagReorder (deQuantize quant block))`). So `Common.lean` is edited
  (in this same change) to add both, importing this module.

  ## Design and scope decisions

  - **`MutableMacroBlock s Int16` (upstream's single in-place `ST`-mutable
    buffer, read and written by the same function) becomes a plain pure
    `MacroBlock Int16 → MacroBlock Int16` function**, per this port's
    established `Id.run`/`Array.set!` precedent (see `FastDct.lean`'s
    identical note).
  - **`IDctStage`'s nine fields (upstream: unboxed, strict `Int`, i.e.
    GHC's native *machine* `Int` — 64-bit on any platform this code has
    ever run on) are ported as a plain Lean structure of `Int` fields.**
    Lean's `Int` is arbitrary-precision rather than 64-bit-wrapping, but
    the Chen-Wang algorithm's whole design point is staying within a
    modest fixed-point range for exactly this reason (its own header
    comment: "32-bit integer arithmetic (8 bit coefficients) ... this code
    assumes `>>` to be a two's-complement arithmetic right shift") — no
    intermediate value in `idctRow`/`idctCol` approaches even 32-bit range
    for legitimate JPEG input, let alone 64-bit, so no wraparound is ever
    observable and dropping the width entirely changes nothing. Right
    shifts (`unsafeShiftR`) are still given genuine arithmetic
    (round-towards-`-∞`) semantics via `Int.fdiv` by the corresponding
    power of two — see `FastDct.lean`'s identical note and its verification
    that `Int.fdiv (-3) 2 = -2`, matching a two's-complement arithmetic
    shift of `-3` by one bit.
  - **`idctCol`'s `clip`/`iclip` table collapses to a plain clamp.** Upstream
    builds a 1024-entry lookup vector `iclip` (indices representing
    inputs `-512..511`, each mapped to `clamp(-256, 255, i)`) and a `clip`
    wrapper handling the two out-of-table-range cases separately. Case
    analysis shows this whole apparatus computes exactly
    `clamp(-256, 255, i)` for *every* input `i`, table-indexed or not:
    - `i < -512`: falls into `clip`'s `else` branch of the inner `if`
      (`i > -512` fails), returning `iclip[0] = clamp(-256,255,-512) = -256`
      — matching `clamp(-256,255,i) = -256` for any `i ≤ -256`.
    - `-512 ≤ i < 511`: returns `iclip[i+512] = clamp(-256,255,i)` directly
      by `iclip`'s own definition.
    - `i ≥ 511`: falls into `clip`'s outer `otherwise` branch, returning
      `iclip[1023] = clamp(-256,255,511) = 255` — matching
      `clamp(-256,255,i) = 255` for any `i ≥ 255`.

    So `clip i = max (-256) (min 255 i)` for every `i`, table-free. This
    port uses that closed form directly instead of materialising the
    1024-entry table.
  - **The final `Int16`-narrowing `fromIntegral`s (`idctRow`'s row output,
    `mutableLevelShift`'s `+128`) are ported via `Int16.ofInt`**, matching
    Haskell's `fromIntegral :: Int -> Int16` truncating-to-16-bits
    semantics (both already established at this port's `Common.lean`,
    which uses `Int16.ofInt` for the analogous `decodeInt` narrowing).
    `idctCol`'s output additionally goes through the `clip` clamp above
    before narrowing, so it is always within `[-256, 255]` — safely
    representable in `Int16` with no truncation ever actually biting.
-/

namespace Codec.Picture.Jpg.Internal

-- ── Shared bit-precision helpers ──

/-- The side length, in pixels, of one 8×8 JPEG coefficient block. Named
    `idctBlockDim` (rather than `Common.lean`/`FastDct.lean`'s `blockDim`,
    which names the same `8`) since all three modules share one namespace
    (`Codec.Picture.Jpg.Internal`) and a bare `blockDim` would collide
    across files; see `Common.lean`'s doc-comment for why this module
    cannot instead reuse the already-claimed `Jpg.Internal.Types.dctBlockSize`
    name, which means the coefficient *count* (`64`) instead. -/
def idctBlockDim : Nat := 8

/-- Arithmetic (sign-extending) right shift on an unbounded `Int`, matching
    Haskell's two's-complement `unsafeShiftR` for values that never
    approach a fixed machine-word's range (see the module doc-comment). -/
def idctAshr (n : Int) (k : Nat) : Int := Int.fdiv n (2 ^ k)

/-- Left shift on an unbounded `Int` (plain multiplication by a power of
    two). -/
def idctAshl (n : Int) (k : Nat) : Int := n * 2 ^ k

/-- Clamp to `[-256, 255]`: the closed form of upstream's `iclip`
    table/`clip` wrapper (see the module doc-comment for the case-by-case
    equivalence proof). -/
def idctClip (i : Int) : Int := max (-256) (min 255 i)

-- ── Chen-Wang fixed-point cosine constants ──

/-- `2048·√2·cos(1π/16)`. -/ def w1 : Int := 2841
/-- `2048·√2·cos(2π/16)`. -/ def w2 : Int := 2676
/-- `2048·√2·cos(3π/16)`. -/ def w3 : Int := 2408
/-- `2048·√2·cos(5π/16)`. -/ def w5 : Int := 1609
/-- `2048·√2·cos(6π/16)`. -/ def w6 : Int := 1108
/-- `2048·√2·cos(7π/16)`. -/ def w7 : Int := 565

-- ── The nine-register butterfly state ──

/-- The nine working registers threaded through `idctRow`/`idctCol`'s
    `firstStage`/`secondStage`/`thirdStage` (and `idctRow`'s extra `scaled`
    stage). Ports upstream's `IDctStage` record exactly, field for field. -/
structure IDctStage where
  x0 : Int
  x1 : Int
  x2 : Int
  x3 : Int
  x4 : Int
  x5 : Int
  x6 : Int
  x7 : Int
  x8 : Int
deriving Inhabited

/-- Row (horizontal) IDCT butterfly stages, shared in structure by
    `idctRow`/`idctCol` (each supplies its own shift amounts/fudge factors
    at the call site, exactly matching upstream's two near-duplicate
    definitions of `firstStage`/`secondStage`/`thirdStage`). -/
def idctFirstStage (rowShift : Bool) (c : IDctStage) : IDctStage :=
  let x8' := w7 * (c.x4 + c.x5) + (if rowShift then 0 else 4)
  let x8'' := w3 * (c.x6 + c.x7) + (if rowShift then 0 else 4)
  let shiftBy (v : Int) : Int := if rowShift then v else idctAshr v 3
  { c with
    x4 := shiftBy (x8' + (w1 - w7) * c.x4)
    x5 := shiftBy (x8' - (w1 + w7) * c.x5)
    x6 := shiftBy (x8'' - (w3 - w5) * c.x6)
    x7 := shiftBy (x8'' - (w3 + w5) * c.x7)
    x8 := x8'' }

def idctSecondStage (rowShift : Bool) (c : IDctStage) : IDctStage :=
  let x1' := w6 * (c.x3 + c.x2) + (if rowShift then 0 else 4)
  let x1'' := c.x4 + c.x6
  let shiftBy (v : Int) : Int := if rowShift then v else idctAshr v 3
  { c with
    x0 := c.x0 - c.x1
    x8 := c.x0 + c.x1
    x1 := x1''
    x2 := shiftBy (x1' - (w2 + w6) * c.x2)
    x3 := shiftBy (x1' + (w2 - w6) * c.x3)
    x4 := c.x4 - c.x6
    x6 := c.x5 + c.x7
    x5 := c.x5 - c.x7 }

def idctThirdStage (c : IDctStage) : IDctStage :=
  { c with
    x7 := c.x8 + c.x3
    x8 := c.x8 - c.x3
    x3 := c.x0 + c.x2
    x0 := c.x0 - c.x2
    x2 := idctAshr (181 * (c.x4 + c.x5) + 128) 8
    x4 := idctAshr (181 * (c.x4 - c.x5) + 128) 8 }

/-- `idctRow`'s extra final stage (removed the `PASS`-style scaling by `8`
    bits with no further fudge factor, unlike `idctCol`'s final `>> 14`
    write-out below). Ports upstream's `scaled`. -/
def idctRowScaled (c : IDctStage) : IDctStage :=
  { c with
    x0 := idctAshr (c.x7 + c.x1) 8
    x1 := idctAshr (c.x3 + c.x2) 8
    x2 := idctAshr (c.x0 + c.x4) 8
    x3 := idctAshr (c.x8 + c.x6) 8
    x4 := idctAshr (c.x8 - c.x6) 8
    x5 := idctAshr (c.x0 - c.x4) 8
    x6 := idctAshr (c.x3 - c.x2) 8
    x7 := idctAshr (c.x7 - c.x1) 8 }

-- ── Row (horizontal) pass ──

/-- Row (horizontal) IDCT:
    $$\mathrm{dst}[k] = \sum_{l=0}^{7} c[l] \cdot \mathrm{src}[l] \cdot
      \cos\!\Big(\tfrac\pi8 (k + \tfrac12) l\Big),\quad c[0]=128,\;
      c[1..7]=128\sqrt2.$$
    Reads/writes the 8 samples of one row (`baseIdx = idx`, the row's first
    element), matching upstream's `idctRow` exactly, including its
    permuted read order (`0, 4, 6, 2, 1, 7, 5, 3`). -/
def idctRow (blk : MacroBlock Int16) (idx : Nat) : MacroBlock Int16 :=
  Id.run do
    let readAt (k : Nat) : Int := (blk.getD (idx + k) 0).toInt
    let initial : IDctStage :=
      { x0 := idctAshl (readAt 0) 11 + 128
        x1 := idctAshl (readAt 4) 11
        x2 := readAt 6
        x3 := readAt 2
        x4 := readAt 1
        x5 := readAt 7
        x6 := readAt 5
        x7 := readAt 3
        x8 := 0 }
    let transformed :=
      idctRowScaled (idctThirdStage (idctSecondStage true (idctFirstStage true initial)))
    let mut result := blk
    result := result.set! (idx + 0) (Int16.ofInt transformed.x0)
    result := result.set! (idx + 1) (Int16.ofInt transformed.x1)
    result := result.set! (idx + 2) (Int16.ofInt transformed.x2)
    result := result.set! (idx + 3) (Int16.ofInt transformed.x3)
    result := result.set! (idx + 4) (Int16.ofInt transformed.x4)
    result := result.set! (idx + 5) (Int16.ofInt transformed.x5)
    result := result.set! (idx + 6) (Int16.ofInt transformed.x6)
    result := result.set! (idx + 7) (Int16.ofInt transformed.x7)
    pure result

-- ── Column (vertical) pass ──

/-- Column (vertical) IDCT:
    $$\mathrm{dst}[8k] = \sum_{l=0}^{7} c[l] \cdot \mathrm{src}[8l] \cdot
      \cos\!\Big(\tfrac\pi8 (k + \tfrac12) l\Big),\quad c[0]=1/1024,\;
      c[1..7]=(1/1024)\sqrt2.$$
    Reads/writes one column's 8 samples (`idx` the column index, `0..7`),
    matching upstream's `idctCol` exactly, including its permuted read
    order and the final `idctClip` clamp (see the module doc-comment for
    why the clamp needs no table). -/
def idctCol (blk : MacroBlock Int16) (idx : Nat) : MacroBlock Int16 :=
  Id.run do
    let readAt (rowMul : Nat) : Int := (blk.getD (rowMul * idctBlockDim + idx) 0).toInt
    let initial : IDctStage :=
      { x0 := idctAshl (readAt 0) 8 + 8192
        x1 := idctAshl (readAt 4) 8
        x2 := readAt 6
        x3 := readAt 2
        x4 := readAt 1
        x5 := readAt 7
        x6 := readAt 5
        x7 := readAt 3
        x8 := 0 }
    let f := idctThirdStage (idctSecondStage false (idctFirstStage false initial))
    let write (rowMul : Nat) (v : Int) : MacroBlock Int16 → MacroBlock Int16 :=
      fun b => b.set! (idx + rowMul * idctBlockDim) (Int16.ofInt (idctClip (idctAshr v 14)))
    let mut result := blk
    result := write 0 (f.x7 + f.x1) result
    result := write 1 (f.x3 + f.x2) result
    result := write 2 (f.x0 + f.x4) result
    result := write 3 (f.x8 + f.x6) result
    result := write 4 (f.x8 - f.x6) result
    result := write 5 (f.x0 - f.x4) result
    result := write 6 (f.x3 - f.x2) result
    result := write 7 (f.x7 - f.x1) result
    pure result

/-- Perform the full 8×8 inverse DCT: `idctRow` on every row, then
    `idctCol` on every column. Ports upstream's `fastIdct`. -/
def fastIdct (block : MacroBlock Int16) : MacroBlock Int16 :=
  Id.run do
    let mut blk := block
    for i in [0:8] do
      blk := idctRow blk (8 * i)
    for i in [0:8] do
      blk := idctCol blk i
    pure blk

-- ── Level shift ──

/-- Perform a JPEG level shift (`+128`, back from the IDCT's `[-256, 255]`
    centered output to `[0, 255]`-ish samples ready for clamped pixel
    storage). Ports upstream's `mutableLevelShift`. -/
def mutableLevelShift (block : MacroBlock Int16) : MacroBlock Int16 :=
  block.map fun v => Int16.ofInt (v.toInt + 128)

end Codec.Picture.Jpg.Internal
