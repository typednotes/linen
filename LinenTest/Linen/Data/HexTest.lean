/-
  Tests for `Linen.Data.Hex`.

  Pure, so everything is checked with `#guard`. The encoding vectors are the
  Base16 examples from RFC 4648 §10, plus the SHA-256 digest of the empty
  string — the case that motivated the module, and the one
  `Data.ByteString.Builder.wordHex` gets wrong by dropping leading zeros.
-/
import Linen.Data.Hex

open Data.Hex

namespace Tests.Data.Hex

private def bytes (ns : List Nat) : ByteArray := ⟨(ns.map UInt8.ofNat).toArray⟩

-- ── Encoding ──

#guard encode ByteArray.empty == ""
#guard encode (bytes [0x00]) == "00"                -- leading zero is kept
#guard encode (bytes [0x0a]) == "0a"                -- the `wordHex` failure case
#guard encode (bytes [0xff]) == "ff"
#guard encode (bytes [0xde, 0xad, 0xbe, 0xef]) == "deadbeef"

-- RFC 4648 §10 Base16 test vectors.
#guard encodeUpper "".toUTF8 == ""
#guard encodeUpper "f".toUTF8 == "66"
#guard encodeUpper "fo".toUTF8 == "666F"
#guard encodeUpper "foo".toUTF8 == "666F6F"
#guard encodeUpper "foob".toUTF8 == "666F6F62"
#guard encodeUpper "fooba".toUTF8 == "666F6F6261"
#guard encodeUpper "foobar".toUTF8 == "666F6F626172"

-- Every byte encodes to exactly two characters.
#guard (encode (bytes (List.range 256))).length == 512

-- ── Decoding ──

#guard decode "" == some ByteArray.empty
#guard decode "deadbeef" == some (bytes [0xde, 0xad, 0xbe, 0xef])
#guard decode "DEADBEEF" == some (bytes [0xde, 0xad, 0xbe, 0xef])   -- case-insensitive
#guard decode "DeAdBeEf" == some (bytes [0xde, 0xad, 0xbe, 0xef])   -- mixed case

-- Rejected, rather than silently truncated or zero-filled.
#guard decode "abc" == none        -- odd length
#guard decode "zz" == none         -- not hex
#guard decode "de ad" == none      -- no separators allowed
#guard decode "0x1f" == none       -- no prefix allowed

-- ── Round trip ──

#guard decode (encode (bytes (List.range 256))) == some (bytes (List.range 256))
#guard decode (encodeUpper (bytes (List.range 256))) == some (bytes (List.range 256))

-- ── The motivating case ──

-- SHA-256 of the empty string, the vector `Tests.Crypto.SHA256` checks with its
-- own private `toHex`; this module now supplies that helper library-wide.
#guard encode (bytes
    [0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8,
     0x99, 0x6f, 0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c,
     0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55])
  == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

/-! ### Signatures -/

example : ByteArray → String := encode
example : ByteArray → String := encodeUpper
example : String → Option ByteArray := decode

end Tests.Data.Hex
