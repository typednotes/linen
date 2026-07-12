import Linen.Codec.Picture.BitWriter
import Linen.Codec.Picture.Jpg.Internal.DefaultTable
import Linen.Codec.Picture.Jpg.Internal.FastIdct
import Linen.Codec.Picture.Jpg.Internal.Types
import Linen.Codec.Picture.Types

/-!
  Port of `Codec.Picture.Jpg.Internal.Common` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 23 of 29). Shared
  decoding helpers used by both the baseline and progressive JPEG decoders
  (modules 26/27): Huffman-symbol decoding, the zigzag scan-order table,
  DC/AC "category + extra bits" value decoding, dequantization, and the
  final per-MCU pixel unpacking into the target image.

  Upstream's `Common.hs` exports: `DctCoefficients`, `JpgUnpackerParameter`,
  `decodeInt`, `dcCoefficientDecode`, `deQuantize`, `decodeRrrrSsss`,
  `zigZagReorderForward`, `zigZagReorderForwardv`, `zigZagReorder`,
  `inverseDirectCosineTransform`, `unpackInt`, `unpackMacroBlock`,
  `rasterMap`, `decodeMacroBlock`, `decodeRestartInterval`, `toBlockSize`.

  ## Design and scope decisions

  - **`huffmanPackedDecode` (upstream, defined in `DefaultTable.hs`) is
    replaced by `huffmanDecode`, walking this port's *unpacked*
    `HuffmanTree` (`branch`/`leaf`/`empty`) one bit at a time.** Upstream's
    `HuffmanPackedTree` (a flat `Vector Word16` with sentinel/leaf-tagged
    entries, built by `packHuffmanTree`) exists purely so decoding is one
    array index per bit instead of one tree-pointer dereference; the two
    give identical *results* for any tree returned by `buildHuffmanTree`.
    `DefaultTable.lean` already deliberately dropped `HuffmanPackedTree`/
    `packHuffmanTree`/`makeInverseTable` for this reason (see its own
    doc-comment), so `Common.lean` decodes directly against the plain
    `HuffmanTree`. This is also the module's only genuinely unbounded-
    looking recursion, and it is in fact **structural on the tree**: each
    recursive call is made on a strict substructure (`l`/`r` of a
    `.branch`), so Lean accepts it with no `termination_by` at all — the
    "recurse on the literal substructure" pattern from `HDR.lean`. Hitting
    `.empty` (a code that was never assigned, i.e. malformed input) mirrors
    upstream's own sentinel behaviour (`huffmanPackedDecode` returns `0` for
    an unset table slot) by returning `0` rather than failing.
  - **`decodeInt` is ported to return a plain `Int`, not upstream's `Int32`.**
    JPEG DC/AC categories never exceed 16 bits (an 8×8 block's coefficients
    fit in far fewer), so the decoded magnitude never approaches `Int32`'s
    range and no wraparound behaviour is observable to port; `Int` avoids a
    fixed-width cast at every arithmetic step for no behavioural gain. The
    formula itself — read the sign bit, then `ssss - 1` more bits `w`; if
    the sign bit is set the value is `2^(ssss-1) + w`, otherwise
    `1 - 2·2^(ssss-1) + w` — is transcribed exactly as upstream computes it
    (this is JPEG's standard `EXTEND`/category decoding, equivalent to: read
    the full `ssss`-bit value `V`, and if `V < 2^(ssss-1)` return
    `V - (2^ssss - 1)`, else return `V`).
  - **`JpgUnpackerParameter` is deferred to the progressive decoder (module
    26, `Jpg.Internal.Progressive`), the only place upstream constructs or
    reads it.** It bundles per-scan progressive-decode state (a
    `HuffmanPackedTree` pair, MCU/block indices, coefficient range,
    successive-approximation bits, …) that has no use until a decode loop
    actually consumes it, and it references `HuffmanPackedTree`, a type this
    port does not carry (see above). Introducing it here, unused, would just
    be dead scaffolding; it belongs with its first real consumer.
  - **`inverseDirectCosineTransform` and `decodeMacroBlock` are now wired
    in below.** They call upstream's `Jpg.Internal.FastIdct.fastIdct`/
    `mutableLevelShift`, which did not exist when this module was first
    ported — this was a genuine forward dependency the dependency-plan note
    for module 23 did not originally call out (it listed `Common` as
    depending only on module 22, but upstream's own `Common.hs` imports
    `FastIdct` directly). Now that module 24 (`Jpg.Internal.FastDct`/
    `FastIdct`) has landed and this module imports it, both are ported as
    the same one-liners upstream defines them as:
    `inverseDirectCosineTransform mBlock = fastIdct mBlock >>=
    mutableLevelShift` and `decodeMacroBlock quant zigZag block =
    deQuantize quant block >>= zigZagReorder zigZag >>=
    inverseDirectCosineTransform`, translated into this port's pure-
    `MacroBlock`-returning style (see `deQuantize`/`zigZagReorder` above).
  - **`deQuantize`, `zigZagReorder`, and `zigZagReorderForward` are ported as
    plain pure functions over `Array` (`MacroBlock`), not upstream's
    `ST`-threaded `MutableMacroBlock` mutation.** Per the precedent in
    `BitWriter.lean`/`Types.lean` (no `ST`-region distinction needed in
    Lean), each becomes a single `Array.ofFn` build; `zigZagReorderForwardv`
    — upstream's *immutable*-`Vector`-in/out wrapper around
    `zigZagReorderForward`'s mutable core, needed only because GHC's
    `Data.Vector.Storable`/`.Mutable` split requires an explicit
    thaw/freeze at the boundary — collapses into the very same function
    here, since a Lean `Array` has no such split, and is therefore dropped
    as redundant rather than ported as a second copy.
  - **`unpackMacroBlock` targets this port's plain, persistent `Image`
    (`Codec.Picture.Types`'s collapse of upstream's `MutableImage`/`Image`
    split), read/patched via its public `data : Array Component` field**,
    rather than `MutableImage`'s in-place `MVector` write. The upstream
    function itself hard-codes `PixelYCbCr8` as the target pixel type (a
    JPEG frame is always decoded in YCbCr before the final colour-space
    conversion), so this port does too.
  - **Naming clash with `Jpg.Internal.Types.dctBlockSize`.** That module's
    `dctBlockSize` already claimed the name for the *coefficient count* of
    one block (`8 × 8 = 64`, matching this port's `MacroBlock`/
    `QuantificationTable` being flat 64-element arrays). Upstream's own
    `dctBlockSize` is actually the block's *linear* dimension (`8`), used
    here for `unpackMacroBlock`'s 2-D pixel loop. To avoid silently reusing
    one name for two different numbers, this module introduces `blockDim`
    for upstream's `8`, and keeps using the already-ported `dctBlockSize`
    (`64`) for flat-array sizing.
  - **`rasterMap` is ported as a generic double loop over any `Monad`**,
    matching upstream's polymorphic `(Monad m) => ... -> m ()` signature;
    `unpackMacroBlock` itself is written as a direct `Id.run` loop over
    `Array.set!` rather than built from `rasterMap`, since threading a
    growing/patched `Array` back out of a generic `Monad m`-returning
    callback would need `StateT`/`Id` plumbing upstream's own `ST`-mutation
    version never has to consider; `rasterMap` remains available (and
    tested) as the general-purpose combinator upstream exposes it as.
  - **`decodeRestartInterval` is ported exactly as upstream defines it**:
    the "real" implementation (reading eight `1` bits, then a restart
    marker) is commented out in `Common.hs` itself, and the live definition
    unconditionally returns `-1`. This is upstream's own dead code, not a
    simplification introduced here.
  - **`DcCoefficient`/`DctCoefficients`.** Upstream's `Jpg.Internal.Types`
    defines `type DcCoefficient = Int16` and `Common.hs` re-exports it as
    `type DctCoefficients = DcCoefficient`; this port's `Jpg.Internal.Types`
    module used `Int16` directly at every call site instead of naming the
    alias (see its own doc-comment), so neither name exists yet. Since this
    is the first module that needs `DcCoefficient` as a named type (the
    return type of `dcCoefficientDecode`), both aliases are introduced here.
-/

namespace Codec.Picture.Jpg.Internal

open Codec.Picture (Image PixelYCbCr8 Pixel8)

-- ── Coefficient type aliases ──

/-- A decoded DC (or, after `decodeInt`, AC) JPEG coefficient. -/
abbrev DcCoefficient := Int16

/-- Upstream's synonym for `DcCoefficient`, used for AC coefficients too. -/
abbrev DctCoefficients := DcCoefficient

-- ── Huffman-symbol decoding ──

/-- Decode one Huffman-coded symbol by walking `tree` one bit at a time from
    the entropy-coded bit stream (MSB-first, matching JPEG's `getNextBitJpg`
    convention): a `0` bit takes the `left` branch, a `1` bit the `right`
    branch, until a `leaf` is reached. Structural recursion on `tree` itself
    — every recursive call is made on the strict substructure `l`/`r` of a
    `.branch`, so this needs no explicit termination argument. Reaching
    `.empty` (a code with no assigned symbol, i.e. malformed input) returns
    `0`, mirroring upstream `huffmanPackedDecode`'s own sentinel-slot
    fallback. -/
def huffmanDecode : HuffmanTree → BoolReader UInt8
  | .empty => pure 0
  | .leaf v => pure v
  | .branch l r => do
      let bit ← getNextBitJpg
      if bit then huffmanDecode r else huffmanDecode l

-- ── DC/AC "category + extra bits" decoding ──

/-- Read `n` extra bits (MSB-first) from the entropy-coded stream. -/
def unpackInt (n : Nat) : BoolReader UInt32 := getNextIntJpg n

/-- Decode a signed coefficient value given its Huffman-decoded category
    `ssss` (the bit length of the value): read the sign bit, then `ssss - 1`
    more bits `w`. If the sign bit is set the value is `2^(ssss-1) + w`,
    otherwise `1 - 2·2^(ssss-1) + w`. This is JPEG's standard `EXTEND`
    formula for the variable-length-integer encoding used by both DC and AC
    coefficients (ITU-T.81 §F.2.2.1), transcribed exactly as upstream
    computes it. -/
def decodeInt (ssss : Nat) : BoolReader Int := do
  let signBit ← getNextBitJpg
  let dataRange : Int := (2 : Int) ^ (ssss - 1)
  let leftBitCount := ssss - 1
  let w ← unpackInt leftBitCount
  let wi : Int := (w.toNat : Int)
  pure (if signBit then dataRange + wi else 1 - dataRange * 2 + wi)

/-- Decode one DC coefficient: its category `ssss` from `dcTree`, then (if
    nonzero) the signed value via `decodeInt`. -/
def dcCoefficientDecode (dcTree : HuffmanTree) : BoolReader DcCoefficient := do
  let ssss ← huffmanDecode dcTree
  if ssss == 0 then
    pure 0
  else do
    let v ← decodeInt ssss.toNat
    pure (Int16.ofInt v)

/-- Decode one AC run-length/category byte: the high nibble `RRRR` (the
    number of preceding zero coefficients to skip) and the low nibble `SSSS`
    (the following value's category, `0` meaning "end of block" when
    `RRRR = 0`, or "skip 16 zeroes" otherwise). -/
def decodeRrrrSsss (tree : HuffmanTree) : BoolReader (Nat × Nat) := do
  let rrrrssss ← huffmanDecode tree
  let rrrr := (rrrrssss >>> 4) &&& 0xF
  let ssss := rrrrssss &&& 0xF
  pure (rrrr.toNat, ssss.toNat)

-- ── Restart intervals ──

/-- Upstream's own dead code: the "real" restart-marker-detection logic is
    commented out in `Common.hs` itself, and the live definition
    unconditionally returns `-1` ("no restart marker found here"). Ported
    exactly as upstream defines it, not simplified by this port. -/
def decodeRestartInterval : BoolReader Int := pure (-1)

-- ── Zigzag scan order ──

/-- The JPEG zigzag scan order: index `i` (in zigzag/bitstream order) holds
    the raster (row-major, natural) index of the coefficient stored there.
    Fixed data specified by the JPEG standard (ITU-T.81 Annex A), transcribed
    from upstream's literal 8×8 table. -/
def zigZagOrder : MacroBlock Nat :=
  makeMacroBlock
    [ 0,  1,  5,  6, 14, 15, 27, 28,
      2,  4,  7, 13, 16, 26, 29, 42,
      3,  8, 12, 17, 25, 30, 41, 43,
      9, 11, 18, 24, 31, 40, 44, 53,
     10, 19, 23, 32, 39, 45, 52, 54,
     20, 22, 33, 38, 46, 51, 55, 60,
     21, 34, 37, 47, 50, 56, 59, 61,
     35, 36, 48, 49, 57, 58, 62, 63]

/-- The inverse permutation of `zigZagOrder`: index `i` (in raster order)
    holds the zigzag-scan index of that coefficient. Used when *encoding*
    (reordering a natural-order block into the order the bitstream stores
    it), the dual of `zigZagOrder`'s decode-time use. -/
def zigZagOrderForward : MacroBlock Nat :=
  Array.ofFn (n := dctBlockSize) fun i =>
    (zigZagOrder.findIdx? (· == i.val)).getD 0

/-- Reorder a zigzag-scan-order macroblock into raster (natural) order:
    `result[i] = block[zigZagOrder[i]]`. This is the decode-direction
    reorder (upstream's `zigZagReorder`, ported as a pure `Array` build
    instead of an `ST`-mutable in-place write; see the module doc-comment). -/
def zigZagReorder [Inhabited α] (block : MacroBlock α) : MacroBlock α :=
  Array.ofFn (n := dctBlockSize) fun i => block.getD (zigZagOrder.getD i.val 0) default

/-- Reorder a raster-order macroblock into zigzag-scan order:
    `result[i] = block[zigZagOrderForward[i]]`. This is the encode-direction
    reorder (upstream's `zigZagReorderForward`/`zigZagReorderForwardv`,
    collapsed into one pure function; see the module doc-comment). -/
def zigZagReorderForward [Inhabited α] (block : MacroBlock α) : MacroBlock α :=
  Array.ofFn (n := dctBlockSize) fun i => block.getD (zigZagOrderForward.getD i.val 0) default

-- ── Dequantization ──

/-- Apply a quantization table to a macroblock, coefficient-wise:
    `result[i] = block[i] * table[i]`. Ported as a pure `Array` build
    instead of upstream's `ST`-mutable in-place write. -/
def deQuantize (table block : MacroBlock Int16) : MacroBlock Int16 :=
  Array.ofFn (n := dctBlockSize) fun i => block.getD i.val 0 * table.getD i.val 0

-- ── Inverse DCT and full macroblock decode ──

/-- Perform the inverse DCT and the JPEG level shift back to `[0, 255]`-ish
    samples. Ports upstream's `inverseDirectCosineTransform`, built from
    `Jpg.Internal.FastIdct.fastIdct`/`mutableLevelShift` (module 24). -/
def inverseDirectCosineTransform (block : MacroBlock Int16) : MacroBlock Int16 :=
  mutableLevelShift (fastIdct block)

/-- Decode one fully-quantized/zigzag-ordered macroblock into ready-to-
    unpack pixel samples: dequantize, undo the zigzag scan order, then
    inverse-DCT and level-shift. Ports upstream's `decodeMacroBlock`. -/
def decodeMacroBlock (quantizationTable block : MacroBlock Int16) : MacroBlock Int16 :=
  inverseDirectCosineTransform (zigZagReorder (deQuantize quantizationTable block))

-- ── Generic raster iteration ──

/-- Call `f x y` for every `x` in `[0, width)` and `y` in `[0, height)`, in
    row-major order (outer loop over `y`, inner loop over `x`), in any
    monad. Ports upstream's polymorphic `rasterMap`. -/
def rasterMap {m : Type → Type} [Monad m] (width height : Nat) (f : Nat → Nat → m Unit) :
    m Unit := do
  for y in [0:height] do
    for x in [0:width] do
      f x y

-- ── MCU geometry ──

/-- The side length, in pixels, of one 8×8 JPEG coefficient block. Upstream's
    `dctBlockSize` (value `8`); see the module doc-comment for why this
    module cannot reuse the already-ported `Jpg.Internal.Types.dctBlockSize`
    name, which means the coefficient *count* (`64`) instead. -/
def blockDim : Nat := 8

/-- Round `v` up to the next multiple of `8`: the number of 8-pixel blocks
    needed to cover a dimension of `v` pixels. -/
def toBlockSize (v : Nat) : Nat := (v + 7) / 8

-- ── Final pixel unpacking ──

/-- Clamp a dequantized/IDCT'd `Int16` sample to the `[0, 255]` pixel range
    and truncate to `UInt8`. -/
def pixelClamp (n : Int16) : UInt8 :=
  UInt8.ofNat (max 0 (min 255 n.toInt)).toNat

/-- Scatter one fully-decoded 8×8 macroblock's samples into `img`, at
    component `compIdx` of every pixel, upsampling by `(wCoeff, hCoeff)` to
    account for chroma subsampling (e.g. a `2×2`-subsampled chroma block's
    single decoded sample is replicated across a `2×2` pixel area). Ports
    upstream's `unpackMacroBlock`, patching this port's persistent `Image`
    (via its `data` field) instead of writing into a `MutableImage`. -/
def unpackMacroBlock (compCount wCoeff hCoeff compIdx x y : Nat)
    (img : Image PixelYCbCr8) (block : MacroBlock Int16) : Image PixelYCbCr8 :=
  Id.run do
    let mut data := img.data
    for j in [0:blockDim] do
      for i in [0:blockDim] do
        let yBase := y * blockDim + j * hCoeff
        let compVal := pixelClamp (block.getD (i + j * blockDim) 0)
        for hDup in [0:hCoeff] do
          for wDup in [0:wCoeff] do
            let xBase := x * blockDim + i * wCoeff
            let xPos := xBase + wDup
            let yPos := yBase + hDup
            if xPos < img.width && yPos < img.height then
              let mutableIdx := (xPos + yPos * img.width) * compCount + compIdx
              data := data.set! mutableIdx compVal
    pure { img with data }

end Codec.Picture.Jpg.Internal
