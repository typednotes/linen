import Linen.Codec.Picture.BitWriter

/-!
  Port of `Codec.Picture.Gif.Internal.LZW` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 18 of 29):
  variable-code-width LZW decompression for GIF (`decodeLzw`) and TIFF
  (`decodeLzwTiff`), sharing a single generalised decode loop (upstream's
  `lzw`). The *encoder* (`Codec.Picture.Gif.Internal.LZWEncoding`) is module
  19 and is not ported here.

  ## Bit order (confirmed against upstream)

  Upstream's shared `lzw` core reads each code via `getNextCode`, which
  dispatches on `isNewTiff = (variant == TiffVariant)`:

  ```
  getNextCode s
    | isNewTiff = fromIntegral <$> getNextBitsMSBFirst s
    | otherwise = fromIntegral <$> getNextBitsLSBFirst s
  ```

  So the **GIF variant reads codes LSB-first** (`getNextBitsLSBFirst`,
  seeded via `setDecodedString`), and only the *new-style* TIFF variant
  (`TiffVariant`, as opposed to `OldTiffVariant`) reads MSB-first
  (`getNextBitsMSBFirst`, seeded via `setDecodedStringMSB`).
  `Linen.Codec.Picture.BitWriter` (module 4) already provides both
  `getNextBitsLSBFirst`/`setDecodedString` and
  `getNextBitsMSBFirst`/`setDecodedStringMSB`, so no new bit-reader is
  needed here — this module builds directly on that one, exactly like
  upstream's `import Codec.Picture.BitWriter`.

  ## Termination strategy

  The decode loop's natural shape — "keep reading variable-width codes and
  growing a dictionary until an end-of-information code (or a truncated
  stream) is seen" — has no structural recursion on the input `ByteArray`
  at all: a code's width depends on how many dictionary entries have been
  learned so far, which is itself decode-time state, so the number of codes
  consumed per "step" cannot be read off the input in advance the way
  `Png.Internal.Type`'s `parseChunks` reads a chunk length up front.

  Following the porting brief's second suggested strategy (mirroring
  `Tiff.lean`'s `parseIfdChainAux`): the loop runs as a **fuel-bounded
  `Id.run` `for` loop**, `fuel := bytes.size * 8 + 1`. This is a genuine,
  justified bound, not an arbitrary constant: every LZW code is at least
  `1` bit wide, so no well-formed *or* malformed decode can ever need more
  than `bytes.size * 8` individual code-processing steps before either
  hitting the end-of-information code or exhausting every bit the input
  has to offer. Because Lean's `for _ in [0:fuel]` is a bounded `Range`
  iteration, it needs no `termination_by`/`decreasing_by` at all — the same
  "bounded `for` loop, no proof obligation" shape already used by
  `Tiff.lean`'s `unpackPackBits`/`gatherSamples` (chosen there for
  file-format loops with a similarly-justified, non-recursive bound). A run
  that never reaches its end-of-information code before fuel runs out
  (i.e. a genuinely truncated/malformed stream) is reported as
  `Except.error`, never silently truncated.

  ## Design / scope simplifications

  - Upstream's `lzw` writes into a caller-supplied, fixed-size
    `M.STVector s Word8` (`outVec`, with an `initialWriteIdx` offset for
    `decodeLzwTiff`'s in-place-into-a-larger-buffer use) and represents the
    dictionary as three parallel mutable arrays (`lzwOffsetTable`/
    `lzwSizeTable`/`lzwData`) sharing one big backing buffer purely as a
    GHC-`ST`-level allocation optimisation. This port instead returns a
    freshly-built `Array UInt8` (`Except String (Array UInt8)`, since a
    truncated stream is now a reportable error rather than a silent
    `maxWrite`-bounded stop) and represents the dictionary directly as an
    `Array (Array UInt8)` — one entry per learned code, each holding its
    own decoded byte string. This is representationally different but
    observably identical to upstream's offset/size/data-triple encoding of
    the exact same information; there is no mutable-buffer story in Lean
    worth reconstructing here, matching every earlier codec module's own
    "no `ST`/`MVector`" note (e.g. `Tiff.lean`'s `gatherSamples`).
  - A dictionary reset (the `code == clearCode` branch) is ported as
    literally truncating the `Array (Array UInt8)` back down to
    `firstFreeIndex` entries. This is the direct analogue of what upstream
    *implicitly* achieves by resetting `writeIdx`/`dicWriteIdx` back to
    `firstFreeIndex` without touching the mutable backing arrays: since
    upstream's `addString` always writes at the current `dicWriteIdx`
    (which restarts at `firstFreeIndex` after a clear), any post-clear
    stale entries beyond that point are simply overwritten again as new
    codes are learned and are never read in the meantime (every dictionary
    lookup is gated by `code < writeIdx`, which also restarts at
    `firstFreeIndex`). Truncating the Lean `Array` achieves the identical
    end state without needing to reconstruct that overwrite-in-place
    invariant by hand.
  - `decodeLzwTiff` drops upstream's `outVec`/`initialWriteIdx` parameters
    for the same reason as above (they are purely a "decode into an
    existing shared buffer at some offset" convenience, and this port
    always returns a fresh `Array UInt8`); wiring TIFF's own
    `decompressStrip` (`Linen.Codec.Picture.Tiff`, module 17 — see its own
    doc-comment's "`.lzw` is deferred" note) up to this module's
    `decodeLzwTiff` is left as the natural follow-up that note already
    anticipates, and is out of scope for this module itself.
  - Upstream's `resetArray`/`rangeSetter` initialises `initialElementCount
    = 2 ^ initialKeySize` root entries even for the TIFF variants, where
    `initialKeySize = 9` (so `512` root entries) despite the TIFF root
    alphabet being bytes (`0`–`255`, `256` entries) plus the two fixed
    control codes `256`/`257` — entries `258`–`511` are pre-filled with
    their own (out-of-range-as-a-byte) index purely as harmless filler that
    is never read before being overwritten by a learned dictionary entry
    (`firstFreeIndex = 258`, and every dictionary read is gated by
    `code < writeIdx`, which starts at `258` too). This port only
    initialises the `256` true byte-value roots (indices `0`–`255`) plus
    two never-read placeholder slots at indices `256`/`257`, which is
    observably identical (nothing in either version ever reads indices
    `258` and up before they are freshly written) and avoids inventing a
    dictionary-independent "root count" concept that upstream itself never
    actually relies on for its correctness.
-/

namespace Codec.Picture

-- ── Variant configuration ──

/-- Every per-variant constant `lzwDecode` needs: how many of the low code
    values are genuine byte-value dictionary roots, which two code values
    are the clear/end-of-information controls, the starting code width, the
    first code value a freshly-learned dictionary entry gets assigned, the
    code-width-growth off-by-one (`0` for GIF and old-style TIFF, `1` for
    new-style TIFF — upstream's `switchOffset`), and the bit order to read
    codes in. -/
private structure LzwVariantConfig where
  rootCount : Nat
  clearCode : Nat
  endOfInfo : Nat
  startCodeSize : Nat
  firstFreeIndex : Nat
  switchOffset : Nat
  useMsb : Bool

/-- GIF's LZW Minimum Code Size byte (the GIF-file-format field, distinct
    from `startCodeSize` below) determines the root alphabet size and hence
    every other constant. -/
private def gifVariantConfig (minCodeSize : Nat) : LzwVariantConfig :=
  let rootCount := 2 ^ minCodeSize
  { rootCount
    clearCode := rootCount
    endOfInfo := rootCount + 1
    startCodeSize := minCodeSize + 1
    firstFreeIndex := rootCount + 2
    switchOffset := 0
    useMsb := false }

/-- The two TIFF variants share every constant except `switchOffset`/
    `useMsb` (see `isOldTiffVariant` below for how upstream tells them
    apart). -/
private def tiffVariantConfig (isOld : Bool) : LzwVariantConfig :=
  { rootCount := 256
    clearCode := 256
    endOfInfo := 257
    startCodeSize := 9
    firstFreeIndex := 258
    switchOffset := if isOld then 0 else 1
    useMsb := !isOld }

/-- Upstream's `isOldTiffLZW`: a TIFF file uses the pre-Adobe LZW variant
    (old-style byte order, no code-width-growth off-by-one) exactly when its
    first strip byte is `0` and the second strip byte's low bit is set. -/
private def isOldTiffVariant (bytes : ByteArray) : Bool :=
  bytes.size ≥ 2 ∧ bytes.get! 0 == 0 ∧ (bytes.get! 1 &&& 1) == 1

-- ── Bit-stream setup, shared with `Linen.Codec.Picture.BitWriter` ──

/-- Seed a `BoolState` for the bit order `cfg` selects. -/
private def initLzwBits (cfg : LzwVariantConfig) (bytes : ByteArray) : BoolState :=
  execBoolReader emptyBoolState (if cfg.useMsb then setDecodedStringMSB bytes else setDecodedString bytes)

/-- Read one `width`-bit code, honouring `cfg`'s bit order. -/
private def readLzwCode (cfg : LzwVariantConfig) (width : Nat) (s : BoolState) : UInt32 × BoolState :=
  if cfg.useMsb then (getNextBitsMSBFirst width).run s else (getNextBitsLSBFirst width).run s

-- ── Dictionary ──

/-- A fresh dictionary: `cfg.firstFreeIndex` entries, the first `cfg.rootCount`
    being singleton byte strings (the root alphabet), the rest (the two
    control-code slots) empty placeholders that decoding never reads — see
    the module doc-comment. -/
private def freshDictionary (cfg : LzwVariantConfig) : Array (Array UInt8) :=
  (Array.range cfg.firstFreeIndex).map fun i =>
    if i < cfg.rootCount then #[UInt8.ofNat i] else (#[] : Array UInt8)

-- ── Core decode loop ──

/-- Upstream's shared `lzw`, generalised over `cfg`, decoding `bytes` in
    full. See the module doc-comment for the fuel bound's justification and
    every representational simplification versus upstream's mutable-buffer
    version. -/
private def lzwDecode (cfg : LzwVariantConfig) (maxBitKeySize : Nat) (bytes : ByteArray) :
    Except String (Array UInt8) :=
  let tableEntryCount := 2 ^ (min 12 maxBitKeySize)
  let fuel := bytes.size * 8 + 1
  let (result, sawEnd) := Id.run do
    let mut bits := initLzwBits cfg bytes
    let (code0, bits0) := readLzwCode cfg cfg.startCodeSize bits
    bits := bits0
    let mut dict := freshDictionary cfg
    let mut out : Array UInt8 := #[]
    let mut writeIdx := cfg.firstFreeIndex
    let mut codeSize := cfg.startCodeSize
    let mut oldCode : Nat := 0
    let mut code : Nat := code0.toNat
    let mut done := false
    for _ in [0:fuel] do
      if !done then
        if code == cfg.endOfInfo then
          done := true
        else if code == cfg.clearCode then
          let (toOutputU, bits1) := readLzwCode cfg cfg.startCodeSize bits
          bits := bits1
          let toOutput := toOutputU.toNat
          if toOutput == cfg.endOfInfo then
            done := true
          else
            out := out ++ dict.getD toOutput #[]
            let (nextCodeU, bits2) := readLzwCode cfg cfg.startCodeSize bits
            bits := bits2
            dict := freshDictionary cfg
            writeIdx := cfg.firstFreeIndex
            codeSize := cfg.startCodeSize
            oldCode := toOutput
            code := nextCodeU.toNat
        else
          if code ≥ writeIdx then
            -- The classic "code not yet in the dictionary" case: the
            -- encoder just emitted the code it is about to define.
            let pfx := dict.getD oldCode #[]
            let c := pfx.getD 0 0
            out := out ++ pfx ++ #[c]
            if dict.size < tableEntryCount then dict := dict.push (pfx ++ #[c])
          else
            let outStr := dict.getD code #[]
            out := out ++ outStr
            let c := outStr.getD 0 0
            let pfx := dict.getD oldCode #[]
            if dict.size < tableEntryCount then dict := dict.push (pfx ++ #[c])
          let newCodeSize :=
            if writeIdx + 1 == 2 ^ codeSize - cfg.switchOffset then min 12 (codeSize + 1) else codeSize
          codeSize := newCodeSize
          let (nextCodeU, bits3) := readLzwCode cfg codeSize bits
          bits := bits3
          oldCode := code
          writeIdx := writeIdx + 1
          code := nextCodeU.toNat
    pure (out, done)
  if sawEnd then .ok result else .error "Truncated or malformed LZW stream (no end-of-information code)"

-- ── Public entry points ──

/-- Decode a GIF LZW-compressed data stream (upstream's `decodeLzw`).
    `minCodeSize` is the GIF LZW Minimum Code Size byte that precedes this
    stream in the GIF file format (not itself part of `bytes`); codes are
    read least-significant-bit-first (see the module doc-comment). -/
def decodeLzw (minCodeSize : Nat) (bytes : ByteArray) : Except String (Array UInt8) :=
  lzwDecode (gifVariantConfig minCodeSize) 12 bytes

/-- Decode a TIFF LZW-compressed strip (upstream's `decodeLzwTiff`),
    auto-detecting the pre-Adobe ("old-style") vs. standard byte order from
    the strip's own leading bytes exactly as upstream's `isOldTiffLZW`
    does. -/
def decodeLzwTiff (bytes : ByteArray) : Except String (Array UInt8) :=
  lzwDecode (tiffVariantConfig (isOldTiffVariant bytes)) 12 bytes

end Codec.Picture
