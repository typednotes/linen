import Linen.Codec.Picture.Jpg.Internal.FastDct

/-!
  Tests for `Linen.Codec.Picture.Jpg.Internal.FastDct`: block-size sanity for
  both `referenceDct`/`fastDctLibJpeg`, the DC-only sanity check on a
  constant (level-128) sample block (which should transform to an all-zero
  block for `fastDctLibJpeg`, since its DC term subtracts exactly the
  level-shift being tested), a spot-check of `referenceDct`'s known
  constant-input DC value, and `truncFloat32ToInt`'s round-towards-zero
  behaviour on both signs.
-/

open Codec.Picture.Jpg.Internal

-- ── Block-size sanity ──

def jpgFastDctRamp : MacroBlock Int16 := Array.ofFn (n := dctBlockSize) fun i => Int16.ofNat i.val

#guard (referenceDct jpgFastDctRamp).size == dctBlockSize
#guard (fastDctLibJpeg jpgFastDctRamp).size == dctBlockSize

-- ── DC-only sanity: a flat (level-128) input has no AC energy ──

def jpgFastDctConstBlock : MacroBlock Int16 := Array.replicate dctBlockSize (128 : Int16)

-- `fastDctLibJpeg` subtracts exactly `dctBlockSize (linear) * 128` from the DC
-- term, so a perfectly flat level-128 block transforms to all zeros.
#guard decide (fastDctLibJpeg jpgFastDctConstBlock = Array.replicate dctBlockSize (0 : Int))

-- `referenceDct` (no implicit level shift) instead produces a nonzero DC term
-- (analytically `1024`, truncated to `1023` by `Float32` rounding) and zero
-- everywhere else, since a nonzero-frequency DCT basis vector sums to zero
-- against a constant signal.
#guard (referenceDct jpgFastDctConstBlock).getD 0 999 == 1023
#guard decide ((referenceDct jpgFastDctConstBlock).eraseIdx! 0 = Array.replicate (dctBlockSize - 1) (0 : Int))

-- ── `truncFloat32ToInt`: round towards zero, both signs ──

#guard truncFloat32ToInt 3.7 == 3
#guard truncFloat32ToInt (-3.7) == -3
#guard truncFloat32ToInt 0.9 == 0
#guard truncFloat32ToInt (-0.9) == 0

-- ── `ashr`/`ashl`: arithmetic (sign-extending) shift semantics ──

-- Right shift of a negative number rounds towards `-∞` (two's-complement
-- arithmetic shift), not towards zero.
#guard ashr (-3) 1 == -2
#guard ashr 3 1 == 1
#guard ashl (-3) 1 == -6
