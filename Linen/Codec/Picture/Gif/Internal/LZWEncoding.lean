import Linen.Codec.Picture.BitWriter

/-!
  Port of `Codec.Picture.Gif.Internal.LZWEncoding` from the `JuicyPixels`
  package (see `docs/imports/JuicyPixels/dependencies.md`, module 19 of 29):
  variable-code-width LZW *compression* for GIF (`lzwEncode`), the mirror of
  module 18's `Linen.Codec.Picture.Gif.Internal.LZW` (`decodeLzw`). Codes are
  written least-significant-bit-first via `Linen.Codec.Picture.BitWriter`'s
  `writeBitsGif`/`finalizeBoolWriterGif`, matching upstream's own
  `import Codec.Picture.BitWriter` and its use of `writeBitsGif` throughout
  `lzwEncode`, and matching module 18's confirmed GIF-variant bit order.

  ## Dictionary structure

  Upstream represents the dictionary as a `Data.IntMap.Strict`-backed trie
  (`Trie = IntMap TrieNode`, `TrieNode { trieIndex, trieSub :: Trie }`):
  each trie level maps "next byte value" to a node holding that prefix's
  assigned code and a further sub-trie for extending it by one more byte.
  `lookupUpdate` walks this trie one input byte at a time, descending for as
  long as the next byte is already a child of the current node (extending
  the matched prefix), and on the first byte that is *not* yet a child,
  inserts a fresh child there (assigning it the next free code) and stops.

  This module represents the dictionary instead as a flat
  `Array (Array UInt8 × Nat)` of (byte-string, code) pairs with linear
  lookup (`dictLookup`), rather than reaching for a trie/hash-map structure.
  This is the same design choice offered as acceptable in the porting brief:
  a "straightforward-but-correct" representation, mirroring module 18's own
  "dictionary as `Array (Array UInt8)`" choice inverted to "array of
  (bytes, code) pairs". The complexity tradeoff versus upstream's trie is
  real — `dictLookup` is $O(|\text{dict}|)$ comparisons of up to
  $O(|\text{matched prefix}|)$ each per lookup, versus upstream's $O(1)$
  per byte via `IntMap` descent — but this module is encode-side tooling
  over palette-index streams that are already small (GIF images capped at
  a handful of dictionary resets over `4095`-entry tables), so correctness
  and a clean termination argument matter more here than asymptotic lookup
  cost; nothing above scans more than `dict.size` entries per input byte,
  so the whole encode is polynomial in `bytes.size`, never worse. Grepping
  the codebase for `Linen.Data.IntMap`/`Std.HashMap` precedent confirms
  there is no existing *recursive* trie-shaped map (`IntMap` there maps
  `Nat → v` for a fixed `v`, not `Nat → TrieNode` where `TrieNode` itself
  embeds another `IntMap`), so building one bespoke here purely for this
  module was judged not worth it against the simpler, already-endorsed
  alternative.

  ## Termination strategy

  `longestMatch` (upstream's `lookupUpdate`/`go`) walks forward from a
  starting position in `bytes`, extending the matched prefix one byte at a
  time for as long as the growing byte-string stays in the dictionary. This
  is ported as a single bounded `for i in [start:bytes.size]` loop that
  `break`s at the first byte extending the match out of the dictionary —
  a genuinely bounded `Range` iteration needing no
  `termination_by`/`decreasing_by`, the same "bounded `for` loop" shape used
  throughout this codebase for file-format loops with a justified,
  non-recursive bound (e.g. module 18's `lzwDecode`, `Tiff.lean`'s
  `unpackPackBits`/`gatherSamples`).

  The outer encode loop (upstream's `go` in `lzwEncode` itself, which calls
  `lookupUpdate` once per emitted code and recurses on the returned
  `endIndex`) is ported the same way module 18 ported its own decode loop:
  a **fuel-bounded `Id.run` `for` loop**, but here the fuel bound is even
  tighter and needs no bit-width argument at all. Every dictionary root
  (`initialDict`, covering all `256` byte values) guarantees `longestMatch`
  always matches *at least* the single starting byte before it can ever
  `break`, so each outer iteration advances the read cursor `readIdx` by at
  least `1`. Hence **`fuel := bytes.size + 1`** genuinely bounds the number
  of outer iterations: at most `bytes.size` iterations are needed to consume
  every input byte, plus exactly one final iteration to detect
  `readIdx ≥ bytes.size` and emit the end-of-information code. A `done`
  flag (mirroring module 18's own decode loop) guards every iteration after
  that so the end-of-information code is written exactly once even though
  the bounded `for` loop itself always runs its full `fuel` iterations.

  ## Code-width growth / control-code layout (confirmed against module 18)

  `clearCode := 2 ^ minCodeSize`, `endOfInfo := clearCode + 1`,
  `firstFreeIndex := endOfInfo + 1`, `startCodeSize := minCodeSize + 1` —
  identical to module 18's `gifVariantConfig`, so a round trip through
  `lzwEncode` followed by `decodeLzw` (module 18) reproduces the original
  byte stream exactly (see the accompanying test module). Code width grows
  from `codeSize` to `codeSize + 1` the moment the about-to-be-assigned code
  `writeIdx` reaches `2 ^ codeSize`, capped at `12`; upon the dictionary
  filling completely (`codeSize = 12` and `writeIdx = 4095`, i.e. every code
  in `0 .. 4095` is spoken for) a clear code is emitted at width `12` and
  the dictionary/code-width state resets to its initial values — both
  exactly mirroring upstream's `updateCodeSize`. As upstream's `go` does,
  the dictionary insertion for the *current* step is attempted before this
  reset check runs (not after), so on the very step that triggers a reset,
  the freshly-inserted entry at code `4095` is discarded along with the
  rest of the old dictionary; this is faithful to upstream, not a
  simplification (see `lzwEncodeCore` below).

  ## Scope

  Upstream's `lzwEncode :: Int -> V.Vector Word8 -> L.ByteString` takes a
  `Storable`-`Vector`/lazy-`ByteString` pair; this port takes/returns
  `Array UInt8`/`ByteArray` respectively, matching every other codec
  module's established `Array UInt8`/`ByteArray` convention (and, in
  particular, matching module 18's `decodeLzw : Nat → ByteArray → Except
  String (Array UInt8)` output type, so its output can be fed straight into
  this module's `lzwEncode` for round-trip testing).
-/

namespace Codec.Picture

-- ── Dictionary ──

/-- A fresh dictionary: one entry per byte value `0 .. 255`, each a
    singleton string mapped to its own byte value as a code — the direct
    analogue of upstream's `initialTrie`. As upstream's `initialTrie` always
    covers every byte value regardless of `minCodeSize` (relying on the
    precondition, shared with module 18's decoder, that every input byte is
    a valid palette index `< 2 ^ minCodeSize`), so does this. -/
private def initialDict : Array (Array UInt8 × Nat) :=
  (Array.range 256).map fun i => (#[UInt8.ofNat i], i)

/-- Look up a byte-string's code in the dictionary, if learned. -/
private def dictLookup (dict : Array (Array UInt8 × Nat)) (s : Array UInt8) : Option Nat :=
  (dict.find? (fun p => p.1 == s)).map (·.2)

-- ── Longest dictionary match ──

/-- Starting at `start`, extend a candidate byte-string one byte at a time
    for as long as it stays in `dict`, stopping at the first byte that would
    break the match (or at the end of `bytes`). Returns the matched
    byte-string, its dictionary code, and the index of the first
    byte *not* consumed into the match (upstream's `lookupUpdate`/`go`,
    minus the trie-insertion side effect, which the caller performs itself
    — see `lzwEncodeCore`). See the module doc-comment for why `start <
    bytes.size` guarantees at least one byte is matched. -/
private def longestMatch (dict : Array (Array UInt8 × Nat)) (bytes : Array UInt8) (start : Nat) :
    Array UInt8 × Nat × Nat :=
  Id.run do
    let mut current : Array UInt8 := #[]
    let mut code : Nat := 0
    let mut idx := start
    for i in [start:bytes.size] do
      let candidate := current.push bytes[i]!
      match dictLookup dict candidate with
      | some c => current := candidate; code := c; idx := i + 1
      | none => break
    pure (current, code, idx)

-- ── Core encode loop ──

/-- Upstream's `lzwEncode`'s body: write the initial clear code, then
    repeatedly find the longest dictionary match at the current read
    position, emit its code, learn a new dictionary entry extending that
    match by one byte (unless already at end of input), grow the code width
    or reset the dictionary as needed, and advance. See the module
    doc-comment for the fuel bound's justification. -/
private def lzwEncodeCore (minCodeSize : Nat) (bytes : Array UInt8) : BoolWriter Unit := do
  let clearCode := 2 ^ minCodeSize
  let endOfInfo := clearCode + 1
  let firstFreeIndex := endOfInfo + 1
  let startCodeSize := minCodeSize + 1
  writeBitsGif clearCode.toUInt32 startCodeSize
  let fuel := bytes.size + 1
  let mut dict := initialDict
  let mut codeSize := startCodeSize
  let mut writeIdx := firstFreeIndex
  let mut readIdx := 0
  let mut done := false
  for _ in [0:fuel] do
    if !done then
      if readIdx ≥ bytes.size then
        writeBitsGif endOfInfo.toUInt32 codeSize
        done := true
      else
        let (matched, code, nextIdx) := longestMatch dict bytes readIdx
        writeBitsGif code.toUInt32 codeSize
        -- Attempt the new dictionary entry first, exactly as upstream's
        -- `lookupUpdate` runs before `updateCodeSize` decides whether to
        -- keep or discard it.
        if nextIdx < bytes.size then
          dict := dict.push (matched.push bytes[nextIdx]!, writeIdx)
        if codeSize == 12 ∧ writeIdx == 4095 then
          writeBitsGif clearCode.toUInt32 12
          dict := initialDict
          codeSize := startCodeSize
          writeIdx := firstFreeIndex
        else
          if writeIdx == 2 ^ codeSize then codeSize := min 12 (codeSize + 1)
          writeIdx := writeIdx + 1
        readIdx := nextIdx

-- ── Public entry point ──

/-- Encode `bytes` (a stream of GIF palette-index values, each
    `< 2 ^ minCodeSize`) as a GIF LZW-compressed bit stream (upstream's
    `lzwEncode`). `minCodeSize` is the GIF LZW Minimum Code Size byte that
    precedes this stream in the GIF file format (not itself part of the
    returned `ByteArray`); codes are written least-significant-bit-first,
    matching module 18's `decodeLzw minCodeSize` for a round trip. -/
def lzwEncode (minCodeSize : Nat) (bytes : Array UInt8) : ByteArray :=
  (finalizeBoolWriterGif.run (lzwEncodeCore minCodeSize bytes |>.run newWriteStateRef).2).1

end Codec.Picture
