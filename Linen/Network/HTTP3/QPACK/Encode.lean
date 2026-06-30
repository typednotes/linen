/-
  Linen.Network.HTTP3.QPACK.Encode -- QPACK header encoding

  Encodes HTTP header fields using QPACK (RFC 9204).
  This implementation uses static-table-only encoding (no dynamic table updates).

  ## Design

  QPACK encoding supports three representations:
  1. Indexed field line (static table) -- most compact
  2. Literal field line with name reference (static table)
  3. Literal field line with literal name -- least compact

  This implementation does not use the dynamic table or Huffman encoding,
  making it stateless and suitable for simple HTTP/3 usage.

  ## Guarantees

  - Encoding is deterministic for the same input
  - Static table lookups are used when possible for compactness
  - Encoded output is valid QPACK per RFC 9204
  - Total: the integer continuation recurses on a strictly decreasing value
    (`v / 128 < v`), not a `while`/fuel loop

  ## Haskell equivalent
  QPACK encoding from the `http3` package
-/

import Linen.Network.HTTP3.QPACK.Table

namespace Network.HTTP3.QPACK

/-- Emit the continuation bytes of a QPACK integer (the part after the prefix),
    seven bits at a time with the high bit as the continuation flag.
    Well-founded on `v`: each step replaces `v` with `v / 128 < v`. -/
def encodeQIntCont (v : Nat) (acc : ByteArray) : ByteArray :=
  if v < 128 then
    acc.push v.toUInt8
  else
    encodeQIntCont (v / 128) (acc.push ((v % 128 + 128).toUInt8))
termination_by v
decreasing_by omega

/-- Encode a QPACK integer with the given prefix bit width (RFC 9204 Section 4.1.1).
    $$\text{encodeQInt}(n, v) = \text{prefix-encoded integer}$$
    The first byte is OR'd with `firstByteMask` to set prefix bits. -/
def encodeQInt (prefixBits : Nat) (value : Nat) (firstByteMask : UInt8 := 0) : ByteArray :=
  let maxPrefix := (1 <<< prefixBits) - 1
  if value < maxPrefix then
    ByteArray.mk #[firstByteMask ||| value.toUInt8]
  else
    encodeQIntCont (value - maxPrefix) (ByteArray.mk #[firstByteMask ||| maxPrefix.toUInt8])

/-- Encode a string literal without Huffman encoding (RFC 9204 Section 4.1.2).
    The first bit indicates Huffman encoding (0 = no Huffman).
    $$\text{encodeStringLiteral}(s) = \text{0 bit} \| \text{length} \| \text{bytes}$$ -/
def encodeStringLiteral (s : String) : ByteArray :=
  let bytes := s.toUTF8
  let lenEnc := encodeQInt 7 bytes.size 0x00  -- H=0 (no Huffman)
  lenEnc ++ bytes

/-- Encode a list of header fields using QPACK (static-table-only mode).
    Returns the encoded header block (request stream portion).
    The required insert count and delta base are both 0 (static-only mode).
    $$\text{encodeHeaders} : \text{List HeaderField} \to \text{ByteArray}$$ -/
def encodeHeaders (headers : List HeaderField) : ByteArray := Id.run do
  -- Encoded Field Section Prefix: Required Insert Count = 0, Delta Base = 0
  let mut buf := ByteArray.mk #[0x00, 0x00]
  for (name, value) in headers do
    match staticFind name value with
    | some (idx, true) =>
      -- Indexed Field Line (static): 1xxxxxxx with T=1 (static)
      -- Prefix: 1 (indexed) + 1 (static) = top 2 bits = 0b11, 6-bit index
      buf := buf ++ encodeQInt 6 idx 0xC0
    | some (idx, false) =>
      -- Literal Field Line With Name Reference (static)
      -- Prefix: 0101 (4 bits) + N=0, then 4-bit name index
      buf := buf ++ encodeQInt 4 idx 0x50
      buf := buf ++ encodeStringLiteral value
    | none =>
      -- Literal Field Line With Literal Name
      -- Prefix: 0010 (4 bits) + N=0, then 3-bit name
      buf := buf ++ ByteArray.mk #[0x20]
      buf := buf ++ encodeStringLiteral name
      buf := buf ++ encodeStringLiteral value
  return buf

end Network.HTTP3.QPACK
