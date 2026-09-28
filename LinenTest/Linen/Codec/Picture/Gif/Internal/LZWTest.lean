/-
  Tests for `Linen.Codec.Picture.Gif.Internal.LZW` — decodes small,
  hand-traced GIF LZW code streams (minimum code size `2`, so a `4`-colour
  root alphabet, clear code `4`, end-of-information code `5`) and checks the
  recovered pixel-index bytes.

  Both fixtures below were derived by hand-running the classic LZW encoding
  algorithm (`Clear`, then greedily extend the longest known prefix, adding
  one dictionary entry per emitted code, ending with an explicit
  end-of-information code) over a tiny known pixel-index stream, then
  packing the resulting codes least-significant-bit-first (per the ported
  module's confirmed GIF bit order) into bytes.
-/
import Linen.Codec.Picture.Gif.Internal.LZW

open Codec.Picture

-- ── A single-pixel stream (no dictionary growth) ──

/-- Encoding of the 1-pixel index stream `[0]`: codes `Clear(4,w=3)`,
    `0(w=3)`, `End(5,w=3)` — `9` bits total, LSB-first: `0,0,1, 0,0,0, 1,0,1`,
    packed into bytes `0x44, 0x01` (the remaining `7` trailing bits of the
    second byte are unused padding). -/
def gifLzwSinglePixelBytes : ByteArray := ByteArray.mk #[0x44, 0x01]

#guard match decodeLzw 2 gifLzwSinglePixelBytes with
  | .ok arr => arr == #[0]
  | .error _ => false

-- ── A four-pixel, all-same-index stream (exercises code-width growth) ──

/-- Encoding of the 4-pixel index stream `[1, 1, 1, 1]`. Hand-tracing the
    encoder: `Clear`, emit `1` (learn code `6 = [1,1]`), emit `6` (learn code
    `7 = [1,1,1]`), emit `1` (the final leftover pixel), emit `End`. The
    codes are `Clear(4,w=3)`, `1(w=3)`, `6(w=3)`, `1(w=3)`, `End(5,w=4)` —
    the code width grows from `3` to `4` bits right before the final `End`
    code, since assigning code `7` pushes the next free code to `8 = 2^3`.
    `16` bits total, LSB-first, packed into bytes `0x8C, 0x53` (exactly two
    bytes, no padding). -/
def gifLzwFourPixelBytes : ByteArray := ByteArray.mk #[0x8C, 0x53]

#guard match decodeLzw 2 gifLzwFourPixelBytes with
  | .ok arr => arr == #[1, 1, 1, 1]
  | .error _ => false

-- ── Malformed input ──

-- A stream with no end-of-information code before the bits run out must be
-- reported as an error, not silently truncated.
#guard match decodeLzw 2 (ByteArray.mk #[0x00]) with
  | .error _ => true
  | .ok _ => false
