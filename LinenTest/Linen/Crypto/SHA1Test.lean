/-
  Tests for `Linen.Crypto.SHA1`.

  Checks `hash` against the FIPS 180-4 / RFC 3174 test vectors, including the
  inputs whose padding crosses a block boundary (55, 56 and 64 bytes) and a
  multi-block message, since the length field and the block split are where a
  hand-written SHA-1 goes wrong.
-/
import Linen.Crypto.SHA1
import Linen.Data.Hex

open Crypto.SHA1

namespace Tests.Crypto.SHA1

private def sha1Hex (s : String) : String := Data.Hex.encode (Crypto.SHA1.hash s.toUTF8)

-- ── FIPS 180-4 / RFC 3174 vectors ──

#guard sha1Hex "" == "da39a3ee5e6b4b0d3255bfef95601890afd80709"
#guard sha1Hex "abc" == "a9993e364706816aba3e25717850c26c9cd0d89d"
-- 56 bytes: the padding no longer fits, so the message takes two blocks.
#guard sha1Hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
  == "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
#guard sha1Hex "The quick brown fox jumps over the lazy dog"
  == "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"
-- One character changes every bit of the output.
#guard sha1Hex "The quick brown fox jumps over the lazy cog"
  == "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3"

-- ── Block-boundary lengths ──

-- 55 bytes: marker and length still fit in one block.
#guard sha1Hex (String.ofList (List.replicate 55 'a')) == "c1c8bbdc22796e28c0e15163d20899b65621d65a"
-- 64 bytes: exactly one block of message, padding in a second.
#guard sha1Hex (String.ofList (List.replicate 64 'a')) == "0098ba824b5c16427bd7a1122a5a442a25ec644d"
-- 1000 bytes: sixteen blocks.
#guard sha1Hex (String.ofList (List.replicate 1000 'a')) == "291e9a6c66994949b57ba5e650361e98fc36b1ba"

-- ── Structure ──

#guard (Crypto.SHA1.hash ByteArray.empty).size == 20
#guard (pad "abc".toUTF8).size == 64
#guard (pad (String.ofList (List.replicate 56 'a')).toUTF8).size == 128
-- The length field is big-endian: 24 bits = 0x18 in the last byte.
#guard (pad "abc".toUTF8).get! 63 == 0x18

example (m : ByteArray) : (Crypto.SHA1.hash m).size = 20 := hash_size m

end Tests.Crypto.SHA1
