/-
  Tests for `Linen.Network.HTTP2.HPACK.Encode`.

  Encoders are pure, so behaviour is checked with `#guard`.  Many cases verify
  an encode→decode round-trip against `HPACK.Decode`, which is the strongest
  correctness statement available here, alongside the RFC 7541 wire vectors.
-/
import Linen.Network.HTTP2.HPACK.Encode
import Linen.Network.HTTP2.HPACK.Decode

open Network.HTTP2.HPACK

namespace Tests.Network.HTTP2.HPACKEncode

/-! ### Integer primitive (RFC 7541 §5.1) -/

#guard encodeInteger 10 5 == ByteArray.mk #[0x0a]
#guard encodeInteger 42 8 == ByteArray.mk #[0x2a]
-- RFC C.1.3: 1337 with a 5-bit prefix → 1f 9a 0a.
#guard encodeInteger 1337 5 == ByteArray.mk #[0x1f, 0x9a, 0x0a]

-- Round-trips against the decoder, across prefix sizes and magnitudes.
#guard (decodeInteger (encodeInteger 0 5) 0 5).map (·.value) == some 0
#guard (decodeInteger (encodeInteger 30 5) 0 5).map (·.value) == some 30
#guard (decodeInteger (encodeInteger 31 5) 0 5).map (·.value) == some 31
#guard (decodeInteger (encodeInteger 1337 5) 0 5).map (·.value) == some 1337
#guard (decodeInteger (encodeInteger 100000 5) 0 5).map (·.value) == some 100000
#guard (decodeInteger (encodeInteger 12345 7) 0 7).map (·.value) == some 12345
#guard (decodeInteger (encodeInteger 255 8) 0 8).map (·.value) == some 255

/-! ### String primitive -/

#guard encodeString "custom-key" == ByteArray.mk #[0x0a] ++ "custom-key".toUTF8
#guard (decodeString (encodeString "hello world") 0).map (·.value) == some "hello world"
#guard (decodeString (encodeString "") 0).map (·.value) == some ""
#guard (decodeString (encodeString "café ☕") 0).map (·.value) == some "café ☕"

/-! ### Single representations -/

#guard encodeHeaderRep (.indexed 2) == ByteArray.mk #[0x82]
#guard encodeHeaderRep (.indexed 4) == ByteArray.mk #[0x84]
#guard encodeHeaderRep (.tableSizeUpdate 0) == ByteArray.mk #[0x20]
-- Literal, new name: 0x40, then name string, then value string.
#guard encodeHeaderRep (.literalIndexed none "x" "y")
        == ByteArray.mk #[0x40] ++ encodeString "x" ++ encodeString "y"
-- Literal, name from index 1 (:authority): 0x41, then value string.
#guard encodeHeaderRep (.literalIndexed (some 1) ":authority" "example.com")
        == ByteArray.mk #[0x41] ++ encodeString "example.com"

/-! ### Header-block round-trips (encode then decode) -/

def roundtrips (hdrs : List HeaderField) : Bool :=
  (decodeHeaders (DynamicTable.empty 4096)
      (encodeHeaders (DynamicTable.empty 4096) hdrs).1).map (·.1) == some hdrs

#guard roundtrips [(":method", "GET")]
#guard roundtrips [(":method", "GET"), (":path", "/"), (":scheme", "https")]
#guard roundtrips [("custom-key", "custom-value")]
#guard roundtrips [("x-foo", "bar"), ("x-foo", "bar")]      -- repeated → second is indexed
#guard roundtrips [(":status", "200"), ("content-type", "text/html"), ("server", "linen")]
#guard roundtrips []

-- Exact-match fields encode to the compact indexed form (1 byte each).
#guard (encodeHeaders (DynamicTable.empty 4096) [(":method", "GET")]).1 == ByteArray.mk #[0x82]
#guard (encodeHeaders (DynamicTable.empty 4096) [(":method", "GET"), (":scheme", "http")]).1
        == ByteArray.mk #[0x82, 0x86]

end Tests.Network.HTTP2.HPACKEncode
