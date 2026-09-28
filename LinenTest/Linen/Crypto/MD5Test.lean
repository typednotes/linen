/-
  Tests.Linen.Crypto.MD5Test — tests for `Crypto.MD5.hash`

  Checks `hash` against the well-known RFC 1321 test vectors, comparing
  lowercase-hex renderings of the 16-byte digest.
-/

import Linen.Crypto.MD5

namespace Tests.Linen.Crypto.MD5Test

/-! ── Local hex-encoding helper ── -/

/-- Encode a `ByteArray` as lowercase hexadecimal, e.g. `#[0xd4, 0x1d] ↦
    "d41d"`. Used only to compare digests against the standard hex-string
    test vectors. -/
def toHex (bs : ByteArray) : String :=
  String.join (bs.toList.map fun b =>
    let s := String.ofList (Nat.toDigits 16 b.toNat)
    if s.length < 2 then "0" ++ s else s)

/-- Convert an ASCII string to the `ByteArray` of its bytes (Latin-1
    truncation; all test vectors below are pure ASCII). -/
def ofAscii (s : String) : ByteArray :=
  ByteArray.mk (s.toList.toArray.map fun c => c.toNat.toUInt8)

/-! ── RFC 1321 test vectors ── -/

-- `md5("") = "d41d8cd98f00b204e9800998ecf8427e"`.
#guard toHex (Crypto.MD5.hash (ofAscii "")) = "d41d8cd98f00b204e9800998ecf8427e"

-- `md5("a") = "0cc175b9c0f1b6a831c399e269772661"`.
#guard toHex (Crypto.MD5.hash (ofAscii "a")) = "0cc175b9c0f1b6a831c399e269772661"

-- `md5("abc") = "900150983cd24fb0d6963f7d28e17f72"`.
#guard toHex (Crypto.MD5.hash (ofAscii "abc")) = "900150983cd24fb0d6963f7d28e17f72"

-- `md5("message digest") = "f96b697d7cb7938d525a2f31aaf161d0"`.
#guard toHex (Crypto.MD5.hash (ofAscii "message digest")) = "f96b697d7cb7938d525a2f31aaf161d0"

-- `md5("abcdefghijklmnopqrstuvwxyz") = "c3fcd3d76192e4007dfb496cca67e13b"`.
#guard toHex (Crypto.MD5.hash (ofAscii "abcdefghijklmnopqrstuvwxyz"))
  = "c3fcd3d76192e4007dfb496cca67e13b"

-- `md5("The quick brown fox jumps over the lazy dog") =
-- "9e107d9d372bb6826bd81d3542a419d6"`.
#guard toHex (Crypto.MD5.hash (ofAscii "The quick brown fox jumps over the lazy dog"))
  = "9e107d9d372bb6826bd81d3542a419d6"

-- A 61-byte message, one short of triggering an extra padding block
-- (exercises the `rem ≤ 56` branch of `Crypto.MD5.pad`).
#guard toHex (Crypto.MD5.hash (ofAscii "1234567890123456789012345678901234567890123456789012345678901"))
  = "931844f87f22a0ac1b7167979c8bea99"

-- A 56-byte message (exercises the `rem > 56` branch of `Crypto.MD5.pad`,
-- which needs a whole extra block for the length suffix).
#guard toHex (Crypto.MD5.hash (ofAscii "12345678901234567890123456789012345678901234567890123456"))
  = "49f193adce178490e34d1b3a4ec0064c"

end Tests.Linen.Crypto.MD5Test
