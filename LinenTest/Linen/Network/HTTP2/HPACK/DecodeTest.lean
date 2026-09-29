/-
  Tests for `Linen.Network.HTTP2.HPACK.Decode`.

  All decoders are pure, so behaviour is checked with `#guard`, using the
  canonical RFC 7541 examples (Appendix C) as wire-format vectors.
-/
import Linen.Network.HTTP2.HPACK.Decode

open Network.HTTP2.HPACK

namespace Tests.Network.HTTP2.HPACKDecode

/-! ### Integer primitive (RFC 7541 §5.1) -/

-- Value fits in the prefix → single byte.
#guard (decodeInteger (ByteArray.mk #[0x0a]) 0 5).map (·.value) == some 10
#guard (decodeInteger (ByteArray.mk #[0x0a]) 0 5).map (·.consumed) == some 1
#guard (decodeInteger (ByteArray.mk #[0x2a]) 0 8).map (·.value) == some 42
-- RFC C.1.3: 1337 with a 5-bit prefix → 1f 9a 0a (multi-byte).
#guard (decodeInteger (ByteArray.mk #[0x1f, 0x9a, 0x0a]) 0 5).map (·.value) == some 1337
#guard (decodeInteger (ByteArray.mk #[0x1f, 0x9a, 0x0a]) 0 5).map (·.consumed) == some 3
#guard (decodeInteger ByteArray.empty 0 5).isNone

/-! ### String primitive (RFC 7541 §5.2) -/

-- Raw (non-Huffman) string: 0x0a = length 10, H bit clear.
def rawStr : ByteArray := ByteArray.mk #[0x0a] ++ "custom-key".toUTF8
#guard (decodeString rawStr 0).map (·.value) == some "custom-key"
#guard (decodeString rawStr 0).map (·.consumed) == some 11

-- Huffman string: H bit set, length 12 → 0x8c, then the Huffman bytes.
def huffStr : ByteArray := ByteArray.mk #[0x8c] ++ huffmanEncode "www.example.com"
#guard (decodeString huffStr 0).map (·.value) == some "www.example.com"
#guard (decodeString huffStr 0).map (·.consumed) == some 13

/-! ### Indexed header fields (RFC 7541 §6.1, static table) -/

#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x82])).map (·.1)
        == some [(":method", "GET")]
-- Several indexed fields: 0x82 0x86 0x84 → :method GET, :scheme http, :path /
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x82, 0x86, 0x84])).map (·.1)
        == some [(":method", "GET"), (":scheme", "http"), (":path", "/")]
-- An indexed field does not grow the dynamic table.
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x82])).map (fun x => x.2.size)
        == some 0
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0xff, 0xff, 0xff, 0xff])).isNone  -- bad index

/-! ### Literal with incremental indexing (RFC 7541 §6.2.1, C.2.1) -/

-- 0x40 (new name) + "custom-key" (len 10, raw) + "custom-header" (len 13, raw).
def litIndexed : ByteArray :=
  ByteArray.mk #[0x40, 0x0a] ++ "custom-key".toUTF8 ++ ByteArray.mk #[0x0d] ++ "custom-header".toUTF8

#guard (decodeHeaders (DynamicTable.empty 4096) litIndexed).map (·.1)
        == some [("custom-key", "custom-header")]
-- …and it IS inserted into the dynamic table.
#guard (decodeHeaders (DynamicTable.empty 4096) litIndexed).map (fun x => x.2.size) == some 1
#guard (decodeHeaders (DynamicTable.empty 4096) litIndexed).map (fun x => x.2.lookup 0)
        == some (some ("custom-key", "custom-header"))

/-! ### Literal without indexing (RFC 7541 §6.2.2, C.2.2), name from index -/

-- 0x04 (name index 4 = :path) + "/sample/path" (len 12, raw).
def litNoIndex : ByteArray := ByteArray.mk #[0x04, 0x0c] ++ "/sample/path".toUTF8

#guard (decodeHeaders (DynamicTable.empty 4096) litNoIndex).map (·.1)
        == some [(":path", "/sample/path")]
-- …and it does NOT grow the dynamic table.
#guard (decodeHeaders (DynamicTable.empty 4096) litNoIndex).map (fun x => x.2.size) == some 0

/-! ### Dynamic table size update (RFC 7541 §6.3) -/

-- 0x20 = 001 00000 → resize to 0 (evicts everything), yields no header fields.
#guard (decodeHeaders (DynamicTable.empty 100) (ByteArray.mk #[0x20])).map (·.1) == some []
#guard (decodeHeaders (DynamicTable.empty 100) (ByteArray.mk #[0x20])).map (fun x => x.2.maxSize)
        == some 0

/-! ### Empty input -/

#guard (decodeHeaders (DynamicTable.empty 4096) ByteArray.empty).map (·.1) == some []

/-! ### Size updates: bounded, and only at the start of a block (§4.2, §6.3) -/

-- 0x3f 0xe1 0x1f = size update to 4096: allowed at the default limit…
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x3f, 0xe1, 0x1f])).isSome
-- …but not above the limit the decoder advertised (0x3f 0xe2 0x1f = 4097).
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x3f, 0xe2, 0x1f])).isNone
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x3f, 0xe2, 0x1f])
          (maxTableSize := 8192)).isSome
-- After a field (0x82 = `:method: GET`), a size update is a decoding error.
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x20, 0x82])).isSome
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x82, 0x20])).isNone

/-! ### Strings that do not decode are errors (§5.2) -/

-- A Huffman string (length 1, H bit set) whose byte is 8 bits of padding:
-- through 1.8.0 this decoded as the raw byte (or ""), not as an error.
#guard (decodeString (ByteArray.mk #[0x81, 0xff]) 0).isNone
-- Padding of zeros, not ones.
#guard (decodeString (ByteArray.mk #[0x81, 0x00]) 0).isNone
-- The EOS symbol (30 bits of ones) inside the string.
#guard (decodeString (ByteArray.mk #[0x84, 0xff, 0xff, 0xff, 0xff]) 0).isNone
-- A raw string that is not UTF-8.
#guard (decodeString (ByteArray.mk #[0x01, 0xff]) 0).isNone
-- A literal field whose value is such a string fails the whole block.
#guard (decodeHeaders (DynamicTable.empty 4096) (ByteArray.mk #[0x04, 0x81, 0xff])).isNone
-- And a valid Huffman string still decodes ("a" = 0x1f).
#guard (decodeString (ByteArray.mk #[0x81, 0x1f]) 0).map (·.value) == some "a"

end Tests.Network.HTTP2.HPACKDecode
