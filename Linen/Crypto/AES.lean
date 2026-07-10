/-
  Linen.Crypto.AES — AES-128 block cipher, CBC decryption, PKCS5 unpadding

  Ports the scoped slice of two Hackage packages needed by the PDF Standard
  Security Handler's `AESV2` crypt filter:

  * [`cipher-aes`](https://hackage.haskell.org/package/cipher-aes)'s
    `Crypto.Cipher.AES`: a real AES-128 block cipher (`initAES`, the standard
    Rijndael key schedule) and `decryptCBC` (CBC-chained single-block
    decrypt). Only the 128-bit key size and the decrypt direction are ported
    — no ECB/CTR/XTS/GCM, no 192/256-bit keys, no encrypt — per
    `docs/imports/CipherAes/dependencies.md`.
  * [`crypto-api`](https://hackage.haskell.org/package/crypto-api)'s
    `Crypto.Padding.unpadPKCS5`, folded into this module (rather than given
    its own namespace) since it is a single small function with a single
    caller, per `docs/imports/CryptoApi/dependencies.md`.

  All functions here operate on fixed-size data (a 16-byte block, an 11-entry
  round-key schedule) or recurse structurally on a `Nat` derived from the
  input length, so no `partial def` and no `sorry` are needed.
-/

namespace Crypto.AES

/-! ── Rijndael S-box / inverse S-box ── -/

/-- The Rijndael forward S-box, used by `subBytes` (key schedule) and
    `SubWord`. `sbox[b]` substitutes byte `b`. -/
def sbox : Array UInt8 :=
  #[0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]

/-- The Rijndael inverse S-box, used by `InvSubBytes`. `invSbox[b]` is the
    unique byte `x` with `sbox[x] = b`. -/
def invSbox : Array UInt8 :=
  #[0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d]

/-- Round constants `Rcon[1..10]` (top byte only; AES-128 needs no more). -/
def rcon : Array UInt8 :=
  #[0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36]

/-! ── `GF(2^8)` arithmetic (for `InvMixColumns`) ── -/

/-- Multiply by `x` (i.e. `02`) in `GF(2^8)` modulo the AES reduction
    polynomial $x^8 + x^4 + x^3 + x + 1$ (`0x11B`). -/
@[inline] def xtime (b : UInt8) : UInt8 :=
  let shifted := b <<< 1
  if b &&& 0x80 != 0 then shifted ^^^ 0x1b else shifted

/-- Multiply two bytes in `GF(2^8)`, by decomposing `b` into its bit-shifted
    multiples of `a` (`a·1, a·2, a·4, …, a·128`) and XOR-ing those selected by
    `b`'s set bits — the standard "Russian peasant" expansion, unrolled since
    a byte has exactly 8 bits. -/
def gfMul (a b : UInt8) : UInt8 :=
  let a1 := a
  let a2 := xtime a1
  let a4 := xtime a2
  let a8 := xtime a4
  let a16 := xtime a8
  let a32 := xtime a16
  let a64 := xtime a32
  let a128 := xtime a64
  (if b &&& 1 != 0 then a1 else 0) ^^^
  (if b &&& 2 != 0 then a2 else 0) ^^^
  (if b &&& 4 != 0 then a4 else 0) ^^^
  (if b &&& 8 != 0 then a8 else 0) ^^^
  (if b &&& 16 != 0 then a16 else 0) ^^^
  (if b &&& 32 != 0 then a32 else 0) ^^^
  (if b &&& 64 != 0 then a64 else 0) ^^^
  (if b &&& 128 != 0 then a128 else 0)

/-! ── Key schedule ── -/

/-- An expanded AES-128 key: 11 round keys (the original key plus one per
    round), each exactly 16 bytes, produced by `initAES`. -/
structure AESKey where
  /-- `roundKeys[r]` is the 16-byte round key for round `r`, `0 ≤ r ≤ 10`. -/
  roundKeys : Array ByteArray

/-- Pack four bytes into a 32-bit word, big-endian (as AES key-schedule words
    are conventionally written). -/
@[inline] def wordOfBytes (b0 b1 b2 b3 : UInt8) : UInt32 :=
  (b0.toUInt32 <<< 24) ||| (b1.toUInt32 <<< 16) ||| (b2.toUInt32 <<< 8) ||| b3.toUInt32

/-- Unpack a 32-bit word into its four big-endian bytes. -/
@[inline] def bytesOfWord (w : UInt32) : Array UInt8 :=
  #[(w >>> 24).toUInt8, (w >>> 16).toUInt8, (w >>> 8).toUInt8, w.toUInt8]

/-- `SubWord`: apply the forward S-box to each byte of a word. -/
@[inline] def subWord (w : UInt32) : UInt32 :=
  let bs := bytesOfWord w
  wordOfBytes (sbox[bs[0]!.toNat]!) (sbox[bs[1]!.toNat]!) (sbox[bs[2]!.toNat]!) (sbox[bs[3]!.toNat]!)

/-- `RotWord`: cyclically rotate a word's bytes left by one (`[a,b,c,d] ↦
    [b,c,d,a]`). -/
@[inline] def rotWord (w : UInt32) : UInt32 :=
  (w <<< 8) ||| (w >>> 24)

/-- One step of the AES-128 key expansion, extending `ws` (already holding
    words `w[0], …, w[i-1]`) with word `w[i]`, per the standard schedule:
    `w[i] = w[i-4] ⊕ temp`, where `temp = w[i-1]` unless `i` is a multiple of
    `4` (the key length in words), in which case `temp = SubWord(RotWord(w[i-1]))
    ⊕ Rcon[i/4]`. -/
def expandStep (ws : Array UInt32) (i : Nat) : Array UInt32 :=
  let prev := ws[i - 1]!
  let temp :=
    if i % 4 == 0 then
      subWord (rotWord prev) ^^^ (rcon[i / 4 - 1]!.toUInt32 <<< 24)
    else
      prev
  ws.push (ws[i - 4]! ^^^ temp)

/-- The standard AES-128 key expansion: turns the 4 initial words (from the
    16-byte key) into the full 44-word schedule `w[0..43]`, by repeatedly
    applying `expandStep` for `i = 4, …, 43`. -/
def expandWords (initial : Array UInt32) : Array UInt32 :=
  (List.range' 4 40).foldl expandStep initial

/-- `initAES : ByteArray → AESKey` — AES-128 key expansion. Splits the
    16-byte key into 4 initial words, expands them into the 44-word
    schedule, and regroups every 4 words into one of the 11 round keys. -/
def initAES (key : ByteArray) : AESKey :=
  let w0 := wordOfBytes (key.get! 0) (key.get! 1) (key.get! 2) (key.get! 3)
  let w1 := wordOfBytes (key.get! 4) (key.get! 5) (key.get! 6) (key.get! 7)
  let w2 := wordOfBytes (key.get! 8) (key.get! 9) (key.get! 10) (key.get! 11)
  let w3 := wordOfBytes (key.get! 12) (key.get! 13) (key.get! 14) (key.get! 15)
  let ws := expandWords #[w0, w1, w2, w3]
  let roundKeys := (Array.range 11).map fun r =>
    let bs := (Array.range 4).flatMap fun j => bytesOfWord ws[4 * r + j]!
    ByteArray.mk bs
  ⟨roundKeys⟩

/-! ── Single-block AES-128 decryption ── -/

/-- `AddRoundKey`: XOR a 16-byte state with a 16-byte round key. -/
def addRoundKey (state key : ByteArray) : ByteArray :=
  ByteArray.mk ((Array.range 16).map fun i => state.get! i ^^^ key.get! i)

/-- `InvSubBytes`: apply the inverse S-box to every byte of the state. -/
def invSubBytes (state : ByteArray) : ByteArray :=
  ByteArray.mk (state.data.map fun b => invSbox[b.toNat]!)

/-- `InvShiftRows`: viewing the 16-byte state as a column-major 4×4 byte
    matrix (byte `r + 4*c` is row `r`, column `c`), cyclically shift row `r`
    right by `r` positions. -/
def invShiftRows (state : ByteArray) : ByteArray :=
  ByteArray.mk ((Array.range 16).map fun i =>
    let r := i % 4
    let c := i / 4
    state.get! (r + 4 * ((c + 4 - r) % 4)))

/-- `InvMixColumns`: multiply each of the 4 state columns by the fixed
    inverse MDS matrix `[[14,11,13,9],[9,14,11,13],[13,9,14,11],[11,13,9,14]]`
    over `GF(2^8)`. -/
def invMixColumns (state : ByteArray) : ByteArray :=
  ByteArray.mk ((Array.range 4).flatMap fun c =>
    let s0 := state.get! (4 * c)
    let s1 := state.get! (4 * c + 1)
    let s2 := state.get! (4 * c + 2)
    let s3 := state.get! (4 * c + 3)
    #[gfMul 14 s0 ^^^ gfMul 11 s1 ^^^ gfMul 13 s2 ^^^ gfMul 9 s3,
      gfMul 9 s0 ^^^ gfMul 14 s1 ^^^ gfMul 11 s2 ^^^ gfMul 13 s3,
      gfMul 13 s0 ^^^ gfMul 9 s1 ^^^ gfMul 14 s2 ^^^ gfMul 11 s3,
      gfMul 11 s0 ^^^ gfMul 13 s1 ^^^ gfMul 9 s2 ^^^ gfMul 14 s3])

/-- The 9 middle rounds of AES-128 decryption (`InvShiftRows`,
    `InvSubBytes`, `AddRoundKey`, `InvMixColumns`, in that order), applied
    for round keys `roundKeys[n], roundKeys[n-1], …, roundKeys[1]` as `n`
    counts down from `9` to `1`. Structurally recursive on `n`. -/
def invMiddleRounds (roundKeys : Array ByteArray) : Nat → ByteArray → ByteArray
  | 0, state => state
  | n + 1, state =>
    let state := invShiftRows state
    let state := invSubBytes state
    let state := addRoundKey state roundKeys[n + 1]!
    let state := invMixColumns state
    invMiddleRounds roundKeys n state

/-- Decrypt a single 16-byte AES-128 block: `AddRoundKey` with the last round
    key, the 9 middle rounds (`invMiddleRounds`), then the final round
    (`InvShiftRows`, `InvSubBytes`, `AddRoundKey` with `roundKeys[0]`, no
    `InvMixColumns`). -/
def decryptBlock (key : AESKey) (block : ByteArray) : ByteArray :=
  let state := addRoundKey block key.roundKeys[10]!
  let state := invMiddleRounds key.roundKeys 9 state
  let state := invShiftRows state
  let state := invSubBytes state
  addRoundKey state key.roundKeys[0]!

/-! ── CBC-mode decryption ── -/

/-- XOR two equal-length `ByteArray`s byte-wise. -/
def xorBytes (a b : ByteArray) : ByteArray :=
  ByteArray.mk ((Array.range a.size).map fun i => a.get! i ^^^ b.get! i)

/-- CBC-chain single-block decryption over `numBlocks` 16-byte blocks of
    `ciphertext`, starting from block `prevCipher` (the IV, or the preceding
    ciphertext block): plaintext block `i` is `decryptBlock(cipher block i) ⊕
    prevCipher`, and the next `prevCipher` is cipher block `i` itself.
    Structurally recursive on `numBlocks`. -/
def decryptBlocks (key : AESKey) (ciphertext prevCipher : ByteArray) :
    Nat → Nat → ByteArray
  | 0, _ => ByteArray.empty
  | n + 1, offset =>
    let cipherBlock := ciphertext.extract offset (offset + 16)
    let plainBlock := xorBytes (decryptBlock key cipherBlock) prevCipher
    plainBlock ++ decryptBlocks key ciphertext cipherBlock n (offset + 16)

/-- `decryptCBC : AESKey → ByteArray → ByteArray → ByteArray` — CBC-mode
    AES-128 decryption. `iv` is 16 bytes; `ciphertext` is a multiple of 16
    bytes. Chains `decryptBlock` over each block, XOR-ing with the previous
    ciphertext block (the `iv` for the first block). -/
def decryptCBC (key : AESKey) (iv ciphertext : ByteArray) : ByteArray :=
  decryptBlocks key ciphertext iv (ciphertext.size / 16) 0

/-! ── PKCS5 unpadding ── -/

/-- `unpadPKCS5 : ByteArray → Option ByteArray` — strip standard PKCS5/PKCS7
    padding: the last byte gives the pad length `n`; valid padding requires
    `1 ≤ n ≤ 16`, `n ≤ length`, and the trailing `n` bytes all equal `n`.
    Returns `none` on any malformed padding (mirroring `crypto-api`'s
    `unpadPKCS5`, but as `Option` rather than raising `error`). -/
def unpadPKCS5 (bs : ByteArray) : Option ByteArray :=
  if bs.size == 0 then
    none
  else
    let n := (bs.get! (bs.size - 1)).toNat
    if n == 0 || n > 16 || n > bs.size then
      none
    else
      let tail := bs.extract (bs.size - n) bs.size
      if (Array.range n).all (fun i => (tail.get! i).toNat == n) then
        some (bs.extract 0 (bs.size - n))
      else
        none

end Crypto.AES
