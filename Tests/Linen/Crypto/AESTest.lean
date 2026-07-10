/-
  Tests for `Linen.Crypto.AES`.

  Uses the FIPS-197 AES-128 known-answer test vector for the single-block
  decrypt and `decryptCBC`, plus round-trip/edge-case checks for
  `unpadPKCS5`.
-/

import Linen.Crypto.AES

open Crypto.AES

namespace Tests.Crypto.AES

/-! ### FIPS-197 AES-128 known-answer test vector -/

/-- FIPS-197 Appendix B key: `000102030405060708090a0b0c0d0e0f`. -/
def fipsKeyBytes : ByteArray :=
  ByteArray.mk #[0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
                 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f]

/-- FIPS-197 Appendix B plaintext: `00112233445566778899aabbccddeeff`. -/
def fipsPlaintext : ByteArray :=
  ByteArray.mk #[0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
                 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]

/-- FIPS-197 Appendix B ciphertext: `69c4e0d86a7b0430d8cdb78070b4c55a`. -/
def fipsCiphertext : ByteArray :=
  ByteArray.mk #[0x69, 0xc4, 0xe0, 0xd8, 0x6a, 0x7b, 0x04, 0x30,
                 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4, 0xc5, 0x5a]

def fipsKey : AESKey := initAES fipsKeyBytes

-- Key expansion produces exactly 11 round keys, each 16 bytes.
#guard fipsKey.roundKeys.size == 11
#guard fipsKey.roundKeys.all (·.size == 16)

-- The first round key is the original key unexpanded.
#guard fipsKey.roundKeys[0]! == fipsKeyBytes

-- Single-block decrypt of the FIPS ciphertext recovers the FIPS plaintext.
#guard decryptBlock fipsKey fipsCiphertext == fipsPlaintext

/-! ### `decryptCBC` -/

/-- All-zero IV, used to reduce CBC decryption of a single block to plain
    single-block decryption. -/
def zeroIV : ByteArray := ByteArray.mk (Array.replicate 16 (0 : UInt8))

-- With a zero IV and a single block, CBC decryption reduces to plain
-- single-block decryption (XOR with an all-zero IV is a no-op).
#guard decryptCBC fipsKey zeroIV fipsCiphertext == fipsPlaintext

/-- A non-zero IV, distinct from `zeroIV`. -/
def wrongIV : ByteArray :=
  ByteArray.mk #[0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
                 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01]

-- Decrypting with the correct (zero) IV differs from decrypting with a
-- different, non-zero IV — the IV genuinely affects the first block.
#guard decryptCBC fipsKey wrongIV fipsCiphertext != fipsPlaintext

/-- Two copies of the FIPS ciphertext block back-to-back, to exercise CBC
    chaining across block boundaries. -/
def twoBlockCiphertext : ByteArray := fipsCiphertext ++ fipsCiphertext

-- CBC decryption over two blocks chains correctly: the second plaintext
-- block is `decryptBlock(secondCipherBlock) ⊕ firstCipherBlock`.
#guard decryptCBC fipsKey zeroIV twoBlockCiphertext ==
  fipsPlaintext ++ xorBytes (decryptBlock fipsKey fipsCiphertext) fipsCiphertext

/-! ### `unpadPKCS5` -/

-- Valid padding: two trailing `0x02` bytes are stripped.
#guard unpadPKCS5 (ByteArray.mk #[0x01, 0x02, 0x03, 0x02, 0x02]) ==
  some (ByteArray.mk #[0x01, 0x02, 0x03])

-- Valid padding: a single trailing `0x01` byte is stripped.
#guard unpadPKCS5 (ByteArray.mk #[0xaa, 0xbb, 0x01]) == some (ByteArray.mk #[0xaa, 0xbb])

-- Edge case: the whole 16-byte block is padding (all bytes equal `16`).
#guard unpadPKCS5 (ByteArray.mk (Array.replicate 16 (16 : UInt8))) == some ByteArray.empty

-- Malformed: pad length `0` is invalid.
#guard unpadPKCS5 (ByteArray.mk #[0x01, 0x02, 0x00]) == none

-- Malformed: pad length exceeds `16`.
#guard unpadPKCS5 (ByteArray.mk #[0x01, 0x02, 17]) == none

-- Malformed: pad length exceeds the array's own length.
#guard unpadPKCS5 (ByteArray.mk #[0x01, 0x02, 0x05]) == none

-- Malformed: the trailing bytes don't all equal the claimed pad length.
#guard unpadPKCS5 (ByteArray.mk #[0x01, 0x02, 0x04, 0x03]) == none

end Tests.Crypto.AES
