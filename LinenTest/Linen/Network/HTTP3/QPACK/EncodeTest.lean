/-
  Tests for `Linen.Network.HTTP3.QPACK.Encode`.

  Encoders are pure, so behaviour is checked with `#guard` — chiefly via
  encode→decode round-trips against `QPACK.Decode`, plus RFC 9204 byte patterns.
-/
import Linen.Network.HTTP3.QPACK.Encode
import Linen.Network.HTTP3.QPACK.Decode

open Network.HTTP3.QPACK

namespace Tests.Network.HTTP3.QPACKEncode

/-! ### Integer primitive (RFC 9204 §4.1.1) -/

#guard encodeQInt 8 0 == ByteArray.mk #[0x00]
#guard encodeQInt 8 42 == ByteArray.mk #[0x2a]
#guard encodeQInt 6 17 0xC0 == ByteArray.mk #[0xD1]          -- indexed-field-line prefix
-- Multi-byte: 100 with a 6-bit prefix (saturates at 63, then continuation 37).
#guard encodeQInt 6 100 == ByteArray.mk #[0x3F, 0x25]
-- Round-trips against the decoder across prefixes/magnitudes.
#guard (decodeQInt 6 (encodeQInt 6 100) 0).map (·.1) == some 100
#guard (decodeQInt 7 (encodeQInt 7 12345) 0).map (·.1) == some 12345
#guard (decodeQInt 8 (encodeQInt 8 1000000) 0).map (·.1) == some 1000000

/-! ### String literal -/

#guard encodeStringLiteral "abc" == ByteArray.mk #[0x03] ++ "abc".toUTF8
#guard (decodeStringLiteral (encodeStringLiteral "hello world") 0).map (·.1) == some "hello world"
#guard (decodeStringLiteral (encodeStringLiteral "") 0).map (·.1) == some ""

/-! ### Header blocks -/

-- An exact static match encodes to the compact indexed form (prefix + 1 byte).
#guard encodeHeaders [(":method", "GET")] == ByteArray.mk #[0x00, 0x00, 0xD1]
#guard encodeHeaders [] == ByteArray.mk #[0x00, 0x00]

/-! ### Round-trips (encode then decode) -/

def roundtrips (hdrs : List HeaderField) : Bool :=
  decodeHeaders (encodeHeaders hdrs) == some hdrs

#guard roundtrips [(":method", "GET")]
#guard roundtrips [(":method", "GET"), (":scheme", "https"), (":status", "200")]
#guard roundtrips [(":authority", "example.com")]              -- name ref + literal value
#guard roundtrips [("x-custom-header", "custom-value")]         -- literal name + literal value
#guard roundtrips [(":status", "200"), ("content-type", "text/html"), ("server", "linen")]
#guard roundtrips []

end Tests.Network.HTTP3.QPACKEncode
