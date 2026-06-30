/-
  Tests for `Linen.Network.HTTP3.QPACK.Decode`.

  All decoders are pure, so behaviour is checked with `#guard` over hand-built
  QPACK wire vectors (RFC 9204, static-table-only mode).
-/
import Linen.Network.HTTP3.QPACK.Decode

open Network.HTTP3.QPACK

namespace Tests.Network.HTTP3.QPACKDecode

/-! ### QPACK integer (RFC 9204 §4.1.1) -/

#guard decodeQInt 8 (ByteArray.mk #[0x00]) 0 == some (0, 1)
#guard decodeQInt 8 (ByteArray.mk #[0x2a]) 0 == some (42, 1)
-- Indexed field line low 6 bits: 0xD1 & 0x3F = 17.
#guard decodeQInt 6 (ByteArray.mk #[0xD1]) 0 == some (17, 1)
-- Multi-byte: prefix saturates at 63, then one continuation byte (37) ⇒ 100.
#guard decodeQInt 6 (ByteArray.mk #[0xFF, 0x25]) 0 == some (100, 2)
#guard decodeQInt 8 ByteArray.empty 0 == none

/-! ### String literal (RFC 9204 §4.1.2) -/

#guard decodeStringLiteral (ByteArray.mk #[0x03] ++ "abc".toUTF8) 0 == some ("abc", 4)
#guard decodeStringLiteral (ByteArray.mk #[0x00]) 0 == some ("", 1)
-- Length claims more bytes than present ⇒ none.
#guard (decodeStringLiteral (ByteArray.mk #[0x05, 0x61]) 0).isNone

/-! ### Header blocks (prefix 0x00 0x00 = RIC 0, Base 0) -/

-- Indexed static: 0xD1 = 11_010001 (indexed, static, index 17 = :method GET).
#guard decodeHeaders (ByteArray.mk #[0x00, 0x00, 0xD1]) == some [(":method", "GET")]
-- Two indexed: index 17 (:method GET) and 23 (:scheme https).
#guard decodeHeaders (ByteArray.mk #[0x00, 0x00, 0xD1, 0xD7])
        == some [(":method", "GET"), (":scheme", "https")]

-- Literal with static name reference: 0x50 (01_01_0000, static, name index 0 = :authority),
-- then value "example.com".
#guard decodeHeaders (ByteArray.mk #[0x00, 0x00, 0x50, 0x0B] ++ "example.com".toUTF8)
        == some [(":authority", "example.com")]

-- Literal with literal name: 0x20 (001_00000), name "x-test", value "v".
#guard decodeHeaders
          (ByteArray.mk #[0x00, 0x00, 0x20, 0x06] ++ "x-test".toUTF8 ++ ByteArray.mk #[0x01] ++ "v".toUTF8)
        == some [("x-test", "v")]

-- Empty block (just the prefix) ⇒ no headers.
#guard decodeHeaders (ByteArray.mk #[0x00, 0x00]) == some []

/-! ### Rejections (static-only mode) -/

-- Required Insert Count ≠ 0 ⇒ none (dynamic table unsupported).
#guard (decodeHeaders (ByteArray.mk #[0x01, 0x00])).isNone
-- Indexed dynamic reference (T = 0, byte 0x80) ⇒ none.
#guard (decodeHeaders (ByteArray.mk #[0x00, 0x00, 0x80])).isNone
-- Out-of-range static index ⇒ none (index 100 ≥ 99).
#guard (decodeHeaders (ByteArray.mk #[0x00, 0x00, 0xFF, 0x25])).isNone

end Tests.Network.HTTP3.QPACKDecode
