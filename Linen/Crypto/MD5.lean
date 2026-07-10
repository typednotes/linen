/-
  Linen.Crypto.MD5 — RFC 1321 MD5 message-digest algorithm

  A pure, structurally-recursive port of `Crypto.Hash.MD5.hash` from the
  Hackage package [`cryptohash`](https://hackage.haskell.org/package/cryptohash),
  per `docs/imports/Cryptohash/dependencies.md`. `cryptohash` supports ~16
  digests, but only MD5 is ever used by `pdf-toolbox-core`'s Standard Security
  Handler key derivation (ISO 32000 §7.6.3.3, Algorithm 2), so this module is
  scoped to that single algorithm.

  MD5 processes a message in 512-bit (64-byte) blocks through 64 rounds of a
  compression function built from four bitwise round functions ($F$, $G$,
  $H$, $I$), a table of 64 constants derived from $\lfloor 2^{32} \cdot
  |\sin(i+1)| \rfloor$, and a fixed table of per-round left-rotate amounts.
  Because padding fixes the exact number of 64-byte blocks *before* the
  compression loop starts, processing them is a plain `Array.foldl` — no
  `partial def` or fuel parameter is needed.
-/

namespace Crypto.MD5

/-! ── Bitwise primitives ── -/

/-- Rotate a 32-bit word left by `n` bits ($0 < n < 32$ for all uses in this
    module, so no special-casing of `n = 0` is required). -/
@[inline] def rotl32 (x : UInt32) (n : Nat) : UInt32 :=
  (x <<< (UInt32.ofNat n)) ||| (x >>> (UInt32.ofNat (32 - n)))

/-! ── Round constants ── -/

/-- The 64 additive constants $K_i = \lfloor 2^{32} \cdot |\sin(i+1)| \rfloor$,
    as specified by RFC 1321. -/
def K : Array UInt32 := #[
  0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
  0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
  0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
  0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
  0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
  0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
  0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
  0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
  0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
  0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
  0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
  0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
  0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
  0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
  0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
  0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391]

/-- Per-round left-rotate amounts, four groups of 16 (one per round quarter). -/
def S : Array Nat := #[
   7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
   5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
   4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
   6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21]

/-! ── Padding ── -/

/-- Pad a message per RFC 1321 §3.1–3.2: append a `0x80` byte, zero-pad until
    the length is $56 \bmod 64$, then append the original bit-length as a
    64-bit little-endian integer. The result's size is always a positive
    multiple of 64. -/
def pad (msg : ByteArray) : ByteArray :=
  let bitLen : UInt64 := UInt64.ofNat (msg.size * 8)
  let withMarker := msg.push 0x80
  let rem := withMarker.size % 64
  let zerosNeeded := if rem ≤ 56 then 56 - rem else 120 - rem
  let withZeros := (Array.range zerosNeeded).foldl (fun acc _ => acc.push 0) withMarker
  let lenBytes : Array UInt8 :=
    (Array.range 8).map (fun i => (bitLen >>> (UInt64.ofNat (i * 8))).toUInt8)
  lenBytes.foldl (fun acc b => acc.push b) withZeros

/-- Split a padded message (whose size is a multiple of 64) into 64-byte
    blocks. -/
def blocksOf (padded : ByteArray) : Array ByteArray :=
  (Array.range (padded.size / 64)).map (fun i => padded.extract (i * 64) (i * 64 + 64))

/-! ── Compression function ── -/

/-- Read the 16 little-endian 32-bit words of a 64-byte block. -/
def blockWords (block : ByteArray) : Array UInt32 :=
  (Array.range 16).map fun i =>
    let o := i * 4
    (block.get! o).toUInt32 |||
    ((block.get! (o + 1)).toUInt32 <<< 8) |||
    ((block.get! (o + 2)).toUInt32 <<< 16) |||
    ((block.get! (o + 3)).toUInt32 <<< 24)

/-- The MD5 internal state: the four 32-bit registers $A, B, C, D$. -/
abbrev State := UInt32 × UInt32 × UInt32 × UInt32

/-- One of the 64 compression rounds, updating the state from round index `i`
    and the block's 16 message words `m`. Uses the four MD5 round functions
    $F$ (rounds 0–15), $G$ (16–31), $H$ (32–47), $I$ (48–63). -/
@[inline] def round (m : Array UInt32) (st : State) (i : Nat) : State :=
  let (a, b, c, d) := st
  let (f, g) :=
    if i < 16 then
      ((b &&& c) ||| ((~~~b) &&& d), i)
    else if i < 32 then
      ((d &&& b) ||| ((~~~d) &&& c), (5 * i + 1) % 16)
    else if i < 48 then
      (b ^^^ c ^^^ d, (3 * i + 5) % 16)
    else
      (c ^^^ (b ||| (~~~d)), (7 * i) % 16)
  let f' := f + a + K[i]! + m[g]!
  let newB := b + rotl32 f' S[i]!
  (d, newB, b, c)

/-- Compress one 64-byte block into the running state, running all 64 rounds
    via a structural `foldl` over the fixed round-index range. -/
def compressBlock (st : State) (block : ByteArray) : State :=
  let m := blockWords block
  let (a0, b0, c0, d0) := st
  let (a, b, c, d) := (Array.range 64).foldl (round m) st
  (a0 + a, b0 + b, c0 + c, d0 + d)

/-! ── Top level ── -/

/-- The four bytes of a 32-bit word, little-endian. -/
def wordBytesLE (w : UInt32) : Array UInt8 :=
  #[w.toUInt8, (w >>> 8).toUInt8, (w >>> 16).toUInt8, (w >>> 24).toUInt8]

/-- Compute the 16-byte MD5 digest of a message, per RFC 1321. Pads the
    message, then folds the fixed compression function over the resulting
    (statically-known-count) sequence of 64-byte blocks — a structural
    `Array.foldl`, no `partial def` required. -/
def hash (msg : ByteArray) : ByteArray :=
  let blocks := blocksOf (pad msg)
  let initState : State := (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)
  let (a, b, c, d) := blocks.foldl compressBlock initState
  ByteArray.mk (wordBytesLE a ++ wordBytesLE b ++ wordBytesLE c ++ wordBytesLE d)

end Crypto.MD5
