import Linen.Codec.Picture.Jpg.Internal.FastDct
import Linen.Codec.Picture.Jpg.Internal.FastIdct

/-!
  Tests for `Linen.Codec.Picture.Jpg.Internal.FastIdct`: the classic
  JPEG "DC-only" sanity check (a block with only the DC coefficient set
  IDCTs to a flat/uniform block), `idctClip`'s clamp behaviour at and past
  its documented `[-256, 255]` boundaries, and a forward→inverse DCT/IDCT
  round trip (via `FastDct.fastDctLibJpeg`) checked with a tolerance, since
  the fixed-point fast integer algorithms are not designed to be bit-exact
  inverses of each other.
-/

open Codec.Picture.Jpg.Internal

-- ── DC-only sanity check ──

/-- A macroblock with only the DC coefficient set to `dc`, every AC
    coefficient zero. -/
def jpgFastIdctDcOnly (dc : Int16) : MacroBlock Int16 :=
  (Array.replicate dctBlockSize (0 : Int16)).set! 0 dc

-- A DC-only block IDCTs to a perfectly flat block (every sample equal): the
-- classic JPEG "DC term is the block's average level" sanity check.
#guard decide (fastIdct (jpgFastIdctDcOnly 1024) = Array.replicate dctBlockSize (128 : Int16))
#guard decide
  (mutableLevelShift (fastIdct (jpgFastIdctDcOnly 1024)) = Array.replicate dctBlockSize (256 : Int16))

-- An all-zero block (no DC, no AC) IDCTs to all zeros.
#guard decide (fastIdct (Array.replicate dctBlockSize (0 : Int16)) = Array.replicate dctBlockSize (0 : Int16))

-- ── `mutableLevelShift` ──

#guard decide (mutableLevelShift #[(0 : Int16), 1, -128] = #[128, 129, 0])

-- ── `idctClip`: closed-form clamp, checked at and past its boundaries ──

#guard idctClip 0 == 0
#guard idctClip 255 == 255
#guard idctClip (-256) == -256
#guard idctClip 256 == 255
#guard idctClip (-257) == -256
#guard idctClip 100000 == 255
#guard idctClip (-100000) == -256

-- ── Forward→inverse round trip (approximate, see the module doc-comment) ──

private def jpgFastIdctClose (a b : Int16) : Bool := (a.toInt - b.toInt).natAbs ≤ 4

private def jpgFastIdctCloseBlock (a b : MacroBlock Int16) : Bool :=
  (List.range dctBlockSize).all fun i => jpgFastIdctClose (a.getD i 0) (b.getD i 0)

/-- A non-trivial, non-symmetric 8×8 test block (not a ramp, not constant),
    exercising every branch of the forward/inverse butterfly. -/
def jpgFastIdctSampleBlock : MacroBlock Int16 :=
  makeMacroBlock
    [ 10, 200,  50, 128,   0, 255,  77,  33,
      90,  12, 240,  60, 128, 128,   5, 250,
       1,   2,   3,   4,   5,   6,   7,   8,
      99,  88,  77,  66,  55,  44,  33,  22,
     128, 128, 128, 128, 128, 128, 128, 128,
       0,   0,   0,   0,   0,   0,   0,   0,
     255, 255, 255, 255, 255, 255, 255, 255,
      17,  34,  51,  68,  85, 102, 119, 136]

/-- Round trip a block through `fastDctLibJpeg` then `fastIdct`/
    `mutableLevelShift`, at the "no quantization" identity (matching
    `Common.lean`'s `decodeMacroBlock` pipeline with an all-`1`s
    quantization table and no zigzag reordering, since this test only
    exercises the transform pair itself). `fastDctLibJpeg`'s output is
    already at exactly the fixed-point scale `fastIdct` expects (both
    forward and inverse are scaled by the same overall factor of `8`,
    per each module's own doc-comment), so no explicit rescaling step is
    needed between them. -/
def jpgFastIdctRoundTrip (block : MacroBlock Int16) : MacroBlock Int16 :=
  mutableLevelShift
    (fastIdct (Array.ofFn (n := dctBlockSize) fun i =>
      Int16.ofInt ((fastDctLibJpeg block).getD i.val 0)))

/-- A linear ramp: `block[i] = i`. -/
def jpgFastIdctRampBlock : MacroBlock Int16 := Array.ofFn (n := dctBlockSize) fun i => Int16.ofNat i.val

-- A linear ramp round-trips exactly (its DCT energy concentrates enough
-- that no rounding error is introduced by this particular block).
#guard decide (jpgFastIdctRoundTrip jpgFastIdctRampBlock = jpgFastIdctRampBlock)

-- A non-trivial block round-trips *approximately*: the fast fixed-point
-- DCT/IDCT pair is not designed to be bit-exact (its `CONST_BITS`/
-- `PASS1_BITS` fixed-point scaling and the Chen-Wang IDCT's own internal
-- `>> 3`/`>> 8`/`>> 14` truncations each lose a little precision), so an
-- exact round trip is not expected — only a small per-sample error
-- (observed: at most `3` here, so `4` gives headroom without being so
-- loose it would hide a real regression).
#guard jpgFastIdctCloseBlock (jpgFastIdctRoundTrip jpgFastIdctSampleBlock) jpgFastIdctSampleBlock
