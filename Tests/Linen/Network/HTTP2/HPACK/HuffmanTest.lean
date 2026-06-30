/-
  Tests for `Linen.Network.HTTP2.HPACK.Huffman`.

  The headline checks are the canonical RFC 7541 test vectors (Appendix C),
  which pin the code table to the standard; the rest verify encode/decode
  round-trips (ASCII + multi-byte UTF-8) and the RFC padding/error rules.
-/
import Linen.Network.HTTP2.HPACK.Huffman

open Network.HTTP2.HPACK

namespace Tests.Network.HTTP2.HPACKHuffman

/-! ### RFC 7541 Appendix C test vectors (exact bytes) -/

#guard huffmanEncode "www.example.com"
        == ByteArray.mk #[0xf1, 0xe3, 0xc2, 0xe5, 0xf2, 0x3a, 0x6b, 0xa0, 0xab, 0x90, 0xf4, 0xff]
#guard huffmanEncode "no-cache" == ByteArray.mk #[0xa8, 0xeb, 0x10, 0x64, 0x9c, 0xbf]
#guard huffmanEncode "custom-key" == ByteArray.mk #[0x25, 0xa8, 0x49, 0xe9, 0x5b, 0xa9, 0x7d, 0x7f]
#guard huffmanEncode "custom-value"
        == ByteArray.mk #[0x25, 0xa8, 0x49, 0xe9, 0x5b, 0xb8, 0xe8, 0xb4, 0xbf]

-- …and decoding those exact bytes recovers the originals.
#guard huffmanDecode (ByteArray.mk #[0xf1, 0xe3, 0xc2, 0xe5, 0xf2, 0x3a, 0x6b, 0xa0, 0xab, 0x90, 0xf4, 0xff])
        == some "www.example.com"
#guard huffmanDecode (ByteArray.mk #[0xa8, 0xeb, 0x10, 0x64, 0x9c, 0xbf]) == some "no-cache"

/-! ### Small encodings (with their padding) -/

#guard huffmanEncode "" == ByteArray.empty
#guard huffmanEncode "0" == ByteArray.mk #[0x07]   -- '0' = 00000, pad 111
#guard huffmanEncode "a" == ByteArray.mk #[0x1f]   -- 'a' = 00011, pad 111
#guard huffmanDecode ByteArray.empty == some ""

/-! ### Round-trips (ASCII, headers, multi-byte UTF-8) -/

#guard huffmanDecode (huffmanEncode "www.example.com") == some "www.example.com"
#guard huffmanDecode (huffmanEncode "no-cache") == some "no-cache"
#guard huffmanDecode (huffmanEncode "custom-key") == some "custom-key"
#guard huffmanDecode (huffmanEncode "custom-value") == some "custom-value"
#guard huffmanDecode (huffmanEncode ":method GET") == some ":method GET"
#guard huffmanDecode (huffmanEncode "Mon, 21 Oct 2013 20:13:21 GMT") == some "Mon, 21 Oct 2013 20:13:21 GMT"
#guard huffmanDecode (huffmanEncode "https://www.example.com") == some "https://www.example.com"
#guard huffmanDecode (huffmanEncode "café ☕ 世界") == some "café ☕ 世界"
#guard huffmanDecode (huffmanEncode "Ünïcödé ñ ©") == some "Ünïcödé ñ ©"

/-! ### Compression actually shrinks typical header text -/

#guard (huffmanEncode "www.example.com").size < "www.example.com".toUTF8.size

/-! ### Padding / error rules (RFC 7541 §5.2) -/

-- A full byte of `1`s is 8 padding bits — more than the allowed 7 ⇒ invalid.
#guard huffmanDecode (ByteArray.mk #[0xff]) == none
-- Valid: the 3-bit padding after '0' is all `1`s and ≤ 7 bits.
#guard huffmanDecode (ByteArray.mk #[0x07]) == some "0"
-- Padding that is not all `1`s is invalid (here 5 bits decode '0', then `00` pad).
#guard huffmanDecode (ByteArray.mk #[0x00]) == none

end Tests.Network.HTTP2.HPACKHuffman
