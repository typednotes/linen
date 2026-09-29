/-
  Linen.Crypto.SHA1 — FIPS 180-4 SHA-1 message digest

  A pure, structurally-recursive port of `Crypto.Hash.SHA1.hash` from the
  Hackage package [`cryptohash`](https://hackage.haskell.org/package/cryptohash)
  (see `docs/imports/Cryptohash/dependencies.md`), the sibling of
  `Linen.Crypto.MD5` from the same import.

  ## Why SHA-1

  SHA-1 is broken for collision resistance and must not be used to protect
  anything. It is here because a protocol requires it *as a fixed function*:
  RFC 6455 §4.2.2 derives `Sec-WebSocket-Accept` as
  $\mathrm{base64}(\mathrm{SHA\text{-}1}(key \mathbin\Vert GUID))$, and every
  WebSocket peer checks it. That is a framing handshake, not a security
  property, so a pure implementation (no `IO`, unlike the OpenSSL-backed
  `Crypto.SHA256`) is the right fit: `Network.WebSockets.computeAcceptKey`
  stays a pure `String → String`.

  ## Shape

  SHA-1 processes a message in 512-bit (64-byte) blocks. Each block is
  expanded into an 80-word message schedule
  $W_t = \mathrm{rotl}_1(W_{t-3} \oplus W_{t-8} \oplus W_{t-14} \oplus W_{t-16})$
  and run through 80 rounds over five 32-bit registers. Padding fixes the
  number of blocks *before* compression starts, so both the schedule and the
  rounds are plain folds over fixed ranges — no `partial def`, no fuel.
-/

namespace Crypto.SHA1

-- ── Bitwise primitives ──────────────────────────────────────────────

/-- Rotate a 32-bit word left by `n` bits ($0 < n < 32$ for every use in this
    module). -/
@[inline] def rotl32 (x : UInt32) (n : Nat) : UInt32 :=
  (x <<< (UInt32.ofNat n)) ||| (x >>> (UInt32.ofNat (32 - n)))

-- ── Padding ─────────────────────────────────────────────────────────

/-- Pad a message per FIPS 180-4 §5.1.1: append a `0x80` byte, zero-pad until
    the length is $56 \bmod 64$, then append the original bit-length as a
    64-bit **big-endian** integer (MD5 uses little-endian here). The result's
    size is always a positive multiple of 64. -/
def pad (msg : ByteArray) : ByteArray :=
  let bitLen : UInt64 := UInt64.ofNat (msg.size * 8)
  let withMarker := msg.push 0x80
  let rem := withMarker.size % 64
  let zerosNeeded := if rem ≤ 56 then 56 - rem else 120 - rem
  let withZeros := (Array.range zerosNeeded).foldl (fun acc _ => acc.push 0) withMarker
  let lenBytes : Array UInt8 :=
    (Array.range 8).map (fun i => (bitLen >>> (UInt64.ofNat ((7 - i) * 8))).toUInt8)
  lenBytes.foldl (fun acc b => acc.push b) withZeros

/-- Split a padded message (whose size is a multiple of 64) into 64-byte
    blocks. -/
def blocksOf (padded : ByteArray) : Array ByteArray :=
  (Array.range (padded.size / 64)).map (fun i => padded.extract (i * 64) (i * 64 + 64))

-- ── Message schedule ────────────────────────────────────────────────

/-- Read the 16 big-endian 32-bit words of a 64-byte block. -/
def blockWords (block : ByteArray) : Array UInt32 :=
  (Array.range 16).map fun i =>
    let o := i * 4
    ((block.get! o).toUInt32 <<< 24) |||
    ((block.get! (o + 1)).toUInt32 <<< 16) |||
    ((block.get! (o + 2)).toUInt32 <<< 8) |||
    (block.get! (o + 3)).toUInt32

/-- Expand a block's 16 words into the 80-word schedule
    $W_t = \mathrm{rotl}_1(W_{t-3} \oplus W_{t-8} \oplus W_{t-14} \oplus W_{t-16})$
    for $16 \le t < 80$, as a fold over the fixed range `[16, 80)`. -/
def schedule (block : ByteArray) : Array UInt32 :=
  (Array.range 64).foldl (fun w j =>
    let t := j + 16
    w.push (rotl32 (w[t - 3]! ^^^ w[t - 8]! ^^^ w[t - 14]! ^^^ w[t - 16]!) 1))
    (blockWords block)

-- ── Compression function ────────────────────────────────────────────

/-- The SHA-1 internal state: the five 32-bit registers $a, b, c, d, e$. -/
abbrev State := UInt32 × UInt32 × UInt32 × UInt32 × UInt32

/-- The FIPS 180-4 initial hash value $H^{(0)}$. -/
def initState : State := (0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0)

/-- The round function $f_t$ and constant $K_t$ for round `t` (§4.1.1, §4.2.1):
    $\mathrm{Ch}$ for rounds 0–19, $\mathrm{Parity}$ for 20–39, $\mathrm{Maj}$ for
    40–59, and $\mathrm{Parity}$ again for 60–79. -/
@[inline] def roundFunction (t : Nat) (b c d : UInt32) : UInt32 × UInt32 :=
  if t < 20 then ((b &&& c) ||| ((~~~b) &&& d), 0x5A827999)
  else if t < 40 then (b ^^^ c ^^^ d, 0x6ED9EBA1)
  else if t < 60 then ((b &&& c) ||| (b &&& d) ||| (c &&& d), 0x8F1BBCDC)
  else (b ^^^ c ^^^ d, 0xCA62C1D6)

/-- One of the 80 compression rounds, from round index `t` and schedule `w`. -/
@[inline] def round (w : Array UInt32) (st : State) (t : Nat) : State :=
  let (a, b, c, d, e) := st
  let (f, k) := roundFunction t b c d
  let temp := rotl32 a 5 + f + e + k + w[t]!
  (temp, a, rotl32 b 30, c, d)

/-- Compress one 64-byte block into the running state: 80 rounds as a
    structural fold over the fixed round-index range, then add the result
    into the incoming state word-wise. -/
def compressBlock (st : State) (block : ByteArray) : State :=
  let w := schedule block
  let (a0, b0, c0, d0, e0) := st
  let (a, b, c, d, e) := (Array.range 80).foldl (round w) st
  (a0 + a, b0 + b, c0 + c, d0 + d, e0 + e)

-- ── Top level ───────────────────────────────────────────────────────

/-- The four bytes of a 32-bit word, big-endian. -/
def wordBytesBE (w : UInt32) : Array UInt8 :=
  #[(w >>> 24).toUInt8, (w >>> 16).toUInt8, (w >>> 8).toUInt8, w.toUInt8]

/-- Compute the 20-byte SHA-1 digest of a message, per FIPS 180-4 §6.1.

    $$\mathrm{hash} : \mathrm{ByteArray} \to \mathrm{ByteArray}, \quad
      |\mathrm{hash}(x)| = 20$$ -/
def hash (msg : ByteArray) : ByteArray :=
  let (a, b, c, d, e) := (blocksOf (pad msg)).foldl compressBlock initState
  ByteArray.mk (wordBytesBE a ++ wordBytesBE b ++ wordBytesBE c ++ wordBytesBE d ++ wordBytesBE e)

/-- The digest is always 20 bytes, whatever the input. -/
theorem hash_size (msg : ByteArray) : (hash msg).size = 20 := by
  simp [hash, wordBytesBE, ByteArray.size]

end Crypto.SHA1
