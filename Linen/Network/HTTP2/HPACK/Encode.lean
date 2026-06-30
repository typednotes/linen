/-
  Linen.Network.HTTP2.HPACK.Encode — HPACK header encoding

  Encodes header lists into HPACK wire format as defined in RFC 7541.

  ## Design

  Supports indexed header field, literal header field with/without indexing,
  and dynamic table size update representations. Integer encoding uses the
  prefix-based variable-length encoding from RFC 7541 Section 5.1.

  ## Guarantees

  - Integer encoding is correct for all prefix sizes 1-8
  - String encoding prepends the length and handles the Huffman flag bit
  - Header list encoding produces a valid HPACK-encoded header block
  - Total: the integer continuation recurses on a strictly decreasing value
    (`v / 128 < v`), not a fuel counter

  ## Haskell equivalent
  `Network.HTTP2.HPACK.Encode` (https://hackage.haskell.org/package/http2)
-/
import Linen.Network.HTTP2.HPACK.Table

namespace Network.HTTP2.HPACK

-- ── Integer encoding (RFC 7541 Section 5.1) ────────────

/-- Emit the continuation bytes of a variable-length integer (the part after
    the prefix), seven bits at a time with the high bit as the continuation
    flag.  Well-founded on `v`: each step replaces `v` with `v / 128 < v`. -/
def encodeIntegerCont (v : Nat) (acc : ByteArray) : ByteArray :=
  if v < 128 then
    acc.push v.toUInt8
  else
    encodeIntegerCont (v / 128) (acc.push (((v % 128) + 128).toUInt8))
termination_by v
decreasing_by omega

/-- Encode an integer with the given prefix size (1-8 bits).
    The first byte has `8 - prefixBits` high bits already set by the caller.

    $$I < 2^N - 1 \Rightarrow \text{one byte: } I$$
    $$I \geq 2^N - 1 \Rightarrow \text{prefix } 2^N - 1, \text{ then } I - (2^N-1) \text{ in 7-bit chunks}$$

    $$\text{encodeInteger} : \text{Nat} \to \text{Nat} \to \text{ByteArray}$$ -/
def encodeInteger (value : Nat) (prefixBits : Nat) : ByteArray :=
  let maxPrefix := (1 <<< prefixBits) - 1
  if value < maxPrefix then
    ByteArray.empty.push value.toUInt8
  else
    encodeIntegerCont (value - maxPrefix) (ByteArray.empty.push maxPrefix.toUInt8)

-- ── String encoding (RFC 7541 Section 5.2) ─────────────

/-- Encode a string literal in HPACK format.
    The first byte's high bit indicates Huffman coding (0 = raw).

    $$\text{encodeString} : \text{String} \to \text{ByteArray}$$ -/
def encodeString (s : String) : ByteArray :=
  let raw := s.toUTF8
  -- Length prefix (7-bit prefix, high bit 0 for no Huffman)
  let lenBytes := encodeInteger raw.size 7
  lenBytes ++ raw

-- ── Header field encoding ──────────────────────────────

/-- HPACK header representation types. -/
inductive HeaderRep where
  /-- Indexed Header Field Representation (Section 6.1).
      References a complete (name, value) pair from the table. -/
  | indexed (index : Nat)
  /-- Literal Header Field with Incremental Indexing (Section 6.2.1).
      The field is added to the dynamic table. -/
  | literalIndexed (nameIndex : Option Nat) (name value : String)
  /-- Literal Header Field without Indexing (Section 6.2.2).
      The field is NOT added to the dynamic table. -/
  | literalNotIndexed (nameIndex : Option Nat) (name value : String)
  /-- Literal Header Field Never Indexed (Section 6.2.3).
      Sensitive value that must never be indexed. -/
  | literalNeverIndexed (nameIndex : Option Nat) (name value : String)
  /-- Dynamic Table Size Update (Section 6.3). -/
  | tableSizeUpdate (newSize : Nat)
  deriving Repr

/-- OR the high bits of the first byte of a ByteArray with a mask.
    Helper for HPACK encoding where the first byte's high bits encode the
    representation type. -/
private def orFirstByte (b : ByteArray) (mask : UInt8) : ByteArray :=
  if b.size == 0 then b
  else
    -- Rebuild: first byte with mask OR'd, then remaining bytes
    let first := b[0]! ||| mask
    let rest := b.extract 1 b.size
    ByteArray.empty.push first ++ rest

/-- Encode a single header representation to HPACK wire format.
    $$\text{encodeHeaderRep} : \text{HeaderRep} \to \text{ByteArray}$$ -/
def encodeHeaderRep : HeaderRep → ByteArray
  | .indexed index =>
    -- High bit 1, 7-bit prefix
    orFirstByte (encodeInteger index 7) 0x80
  | .literalIndexed nameIdx name value =>
    match nameIdx with
    | some idx =>
      -- 01xxxxxx pattern, 6-bit prefix for index
      orFirstByte (encodeInteger idx 6) 0x40 ++ encodeString value
    | none =>
      -- 01000000, then name string, then value string
      ByteArray.empty.push 0x40 ++ encodeString name ++ encodeString value
  | .literalNotIndexed nameIdx _name value =>
    match nameIdx with
    | some idx =>
      -- 0000xxxx pattern, 4-bit prefix for index
      encodeInteger idx 4 ++ encodeString value
    | none =>
      -- 00000000, then name string, then value string
      ByteArray.empty.push 0x00 ++ encodeString _name ++ encodeString value
  | .literalNeverIndexed nameIdx _name value =>
    match nameIdx with
    | some idx =>
      -- 0001xxxx pattern, 4-bit prefix for index
      orFirstByte (encodeInteger idx 4) 0x10 ++ encodeString value
    | none =>
      -- 00010000, then name string, then value string
      ByteArray.empty.push 0x10 ++ encodeString _name ++ encodeString value
  | .tableSizeUpdate newSize =>
    -- 001xxxxx pattern, 5-bit prefix
    orFirstByte (encodeInteger newSize 5) 0x20

/-- Encode a list of header fields using HPACK.
    Uses incremental indexing for all fields (the simplest strategy).
    Returns the encoded header block and the updated dynamic table.

    $$\text{encodeHeaders} : \text{DynamicTable} \to \text{List}(\text{HeaderField}) \to \text{ByteArray} \times \text{DynamicTable}$$ -/
def encodeHeaders (dt : DynamicTable) (headers : List HeaderField) : ByteArray × DynamicTable :=
  headers.foldl (fun (acc, dt) (name, value) =>
    match findInTables dt name value with
    | some (idx, true) =>
      -- Exact match: use indexed representation
      (acc ++ encodeHeaderRep (.indexed idx), dt)
    | some (idx, false) =>
      -- Name match only: literal with incremental indexing, using name index
      let encoded := encodeHeaderRep (.literalIndexed (some idx) name value)
      let dt' := dt.insert name value
      (acc ++ encoded, dt')
    | none =>
      -- No match: literal with incremental indexing, new name
      let encoded := encodeHeaderRep (.literalIndexed none name value)
      let dt' := dt.insert name value
      (acc ++ encoded, dt')
  ) (ByteArray.empty, dt)

end Network.HTTP2.HPACK
