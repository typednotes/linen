/-
  Tests for `Linen.Codec.Picture.Gif.Internal.LZWEncoding` — encodes small
  palette-index byte streams with `lzwEncode` and checks that decoding the
  result back with module 18's `decodeLzw` (`Linen.Codec.Picture.Gif.Internal.
  LZW`) reproduces the original stream exactly. This encode → decode round
  trip is the primary correctness signal for this module: it independently
  confirms `lzwEncode`'s clear/end-of-information codes, code-width growth
  boundaries, and dictionary layout line up with module 18's decoder, since
  both were ported from the same upstream GIF LZW convention (see both
  modules' doc-comments).
-/
import Linen.Codec.Picture.Gif.Internal.LZW
import Linen.Codec.Picture.Gif.Internal.LZWEncoding

open Codec.Picture

/-- Round-trip helper: encode `bytes` at `minCodeSize`, then decode the
    result and compare against the original. -/
def gifLzwEncRoundTrips (minCodeSize : Nat) (bytes : Array UInt8) : Bool :=
  match decodeLzw minCodeSize (lzwEncode minCodeSize bytes) with
  | .ok arr => arr == bytes
  | .error _ => false

-- ── Empty input ──

#guard gifLzwEncRoundTrips 2 #[]

-- ── A single-pixel stream (no dictionary growth) ──

#guard gifLzwEncRoundTrips 2 #[0]

-- ── A run of identical indices (exercises repeated-match encoding and
--    code-width growth as the dictionary learns longer and longer runs) ──

/-- `16` copies of index `1` at `minCodeSize = 2` (a `4`-colour root
    alphabet): each successive dictionary entry doubles the matched run
    length (`1`, `11`, `111`, ...), so this exercises several new-entry
    insertions and at least one code-width growth step (`3` → `4` bits) in
    a single stream. -/
def gifLzwEncRunOfOnes : Array UInt8 := (Array.range 16).map fun _ => (1 : UInt8)

#guard gifLzwEncRoundTrips 2 gifLzwEncRunOfOnes

-- ── A short varied sequence (exercises mismatches / fresh dictionary
--    entries every step, not just repeated runs) ──

def gifLzwEncVaried : Array UInt8 := #[0, 1, 2, 3, 0, 1, 2, 3, 3, 2, 1, 0, 0, 0, 1, 1, 2, 2, 3, 3]

#guard gifLzwEncRoundTrips 2 gifLzwEncVaried

-- ── A larger varied stream, at a wider root alphabet, to also exercise the
--    `minCodeSize = 8` boundary (root alphabet `256`, matching every byte
--    value, clear code `256`, end-of-information `257`) ──

def gifLzwEncWideAlphabet : Array UInt8 :=
  (Array.range 200).map fun i => UInt8.ofNat (i % 256)

#guard gifLzwEncRoundTrips 8 gifLzwEncWideAlphabet
