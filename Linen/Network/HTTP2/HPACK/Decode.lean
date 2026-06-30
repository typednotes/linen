/-
  Linen.Network.HTTP2.HPACK.Decode — HPACK header decoding

  Decodes HPACK wire format into header lists as defined in RFC 7541.

  ## Design

  Parses the HPACK integer and string primitives, then dispatches on the
  leading bits of each byte to determine the representation type.

  ## Guarantees

  - Integer decoding handles multi-byte variable-length encoding correctly
  - String decoding handles both raw and Huffman-encoded strings
  - Header block decoding processes all bytes and returns remaining table state
  - Total: the integer continuation is a bounded structural fold and the header
    loop is well-founded on the unconsumed input (no `partial`, no fuel)

  ## Haskell equivalent
  `Network.HTTP2.HPACK.Decode` (https://hackage.haskell.org/package/http2)
-/
import Linen.Network.HTTP2.HPACK.Table
import Linen.Network.HTTP2.HPACK.Huffman

namespace Network.HTTP2.HPACK

-- ── Integer decoding (RFC 7541 Section 5.1) ────────────

/-- Decode result: parsed value and the number of bytes consumed. -/
structure DecodeResult (α : Type) where
  /-- The decoded value. -/
  value : α
  /-- Number of bytes consumed from input. -/
  consumed : Nat
  deriving Repr

/-- Decode an integer with the given prefix size (1-8 bits).
    Returns the decoded integer and number of bytes consumed.

    The variable-length continuation is bounded to 10 bytes (an overflow
    guard), so it is read as a structural fold over `List.range 10` rather than
    the original fuel-bounded recursion.

    $$\text{decodeInteger} : \text{ByteArray} \to \text{Nat} \to \text{Nat} \to \text{Option}(\text{DecodeResult}(\text{Nat}))$$ -/
def decodeInteger (bs : ByteArray) (offset : Nat) (prefixBits : Nat) : Option (DecodeResult Nat) :=
  if offset ≥ bs.size then none
  else
    let maxPrefix := (1 <<< prefixBits) - 1
    let firstByte := bs[offset]! &&& maxPrefix.toUInt8
    if firstByte.toNat < maxPrefix then
      some { value := firstByte.toNat, consumed := 1 }
    else
      -- Continuation bytes start at offset+1; at most 10 (in bounds only).
      let conts : List (Nat × UInt8) := (List.range 10).filterMap (fun i =>
        let p := offset + 1 + i
        if p < bs.size then some (i, bs[p]!) else none)
      -- Accumulate until a byte with the high bit clear; the first component
      -- records the number of continuation bytes consumed.
      match conts.foldl
          (fun (st : Option Nat × Nat × Nat) (ib : Nat × UInt8) =>
            let (done, value, shift) := st
            match done with
            | some _ => st
            | none =>
              let (i, byte) := ib
              let value' := value + ((byte &&& 0x7F).toNat <<< shift)
              if (byte &&& 0x80) == 0 then (some (i + 1), value', shift)
              else (none, value', shift + 7))
          (none, maxPrefix, 0) with
      | (some contConsumed, value, _) => some { value := value, consumed := contConsumed + 1 }
      | (none, _, _) => none

/-- Every successful integer decode consumes at least one byte. -/
theorem decodeInteger_consumed {bs : ByteArray} {offset prefixBits : Nat} {r : DecodeResult Nat}
    (h : decodeInteger bs offset prefixBits = some r) : 0 < r.consumed := by
  rw [decodeInteger] at h
  dsimp only at h
  split at h
  · simp at h
  · split at h
    · injection h with h; subst h; simp
    · split at h
      · injection h with h; subst h; simp
      · simp at h

-- ── String decoding (RFC 7541 Section 5.2) ─────────────

/-- Decode a string from HPACK format. The high bit of the first byte indicates
    Huffman encoding.

    $$\text{decodeString} : \text{ByteArray} \to \text{Nat} \to \text{Option}(\text{DecodeResult}(\text{String}))$$ -/
def decodeString (bs : ByteArray) (offset : Nat) : Option (DecodeResult String) :=
  if offset ≥ bs.size then none
  else
    let firstByte := bs[offset]!
    let isHuffman := (firstByte &&& 0x80) != 0
    match decodeInteger bs offset 7 with
    | none => none
    | some lenResult =>
      let strLen := lenResult.value
      let dataStart := offset + lenResult.consumed
      if dataStart + strLen > bs.size then none
      else
        let raw := bs.extract dataStart (dataStart + strLen)
        let str := if isHuffman then
          match huffmanDecode raw with
          | some s => s
          | none => match String.fromUTF8? raw with
            | some s => s
            | none => ""
        else
          match String.fromUTF8? raw with
          | some s => s
          | none => ""
        some { value := str, consumed := lenResult.consumed + strLen }

-- ── Header block decoding ──────────────────────────────

-- The `h :` match binders below are referenced only in `decreasing_by` (to
-- prove `offset` advances), which the unused-variable linter doesn't see.
set_option linter.unusedVariables false in
/-- Worker for `decodeHeaders`: decode instructions starting at `offset`,
    threading the dynamic table.  Well-founded on `bs.size - offset`; every
    instruction begins with an integer (consuming ≥ 1 byte by
    `decodeInteger_consumed`), so `offset` strictly increases. -/
private def decodeHeadersGo (bs : ByteArray) (offset : Nat) (dt : DynamicTable)
    (acc : List HeaderField) : Option (List HeaderField × DynamicTable) :=
  if offset ≥ bs.size then some (acc.reverse, dt)
  else
    let byte := bs[offset]!
    if (byte &&& 0x80) != 0 then
      -- Indexed Header Field (Section 6.1): 1xxxxxxx
      match h : decodeInteger bs offset 7 with
      | none => none
      | some idxResult =>
        match indexLookup dt idxResult.value with
        | none => none
        | some field => decodeHeadersGo bs (offset + idxResult.consumed) dt (field :: acc)
    else if (byte &&& 0xC0) == 0x40 then
      -- Literal with Incremental Indexing (Section 6.2.1): 01xxxxxx
      match h : decodeInteger bs offset 6 with
      | none => none
      | some idxResult =>
        let pos := offset + idxResult.consumed
        if idxResult.value > 0 then
          match indexLookup dt idxResult.value with
          | none => none
          | some (name, _) =>
            match decodeString bs pos with
            | none => none
            | some valResult =>
              decodeHeadersGo bs (pos + valResult.consumed) (dt.insert name valResult.value)
                ((name, valResult.value) :: acc)
        else
          match decodeString bs pos with
          | none => none
          | some nameResult =>
            let pos' := pos + nameResult.consumed
            match decodeString bs pos' with
            | none => none
            | some valResult =>
              decodeHeadersGo bs (pos' + valResult.consumed)
                (dt.insert nameResult.value valResult.value)
                ((nameResult.value, valResult.value) :: acc)
    else if (byte &&& 0xE0) == 0x20 then
      -- Dynamic Table Size Update (Section 6.3): 001xxxxx
      match h : decodeInteger bs offset 5 with
      | none => none
      | some sizeResult =>
        decodeHeadersGo bs (offset + sizeResult.consumed) (dt.resize sizeResult.value) acc
    else
      -- Literal without Indexing (6.2.2) or Never Indexed (6.2.3): 0000xxxx / 0001xxxx
      match h : decodeInteger bs offset 4 with
      | none => none
      | some idxResult =>
        let pos := offset + idxResult.consumed
        if idxResult.value > 0 then
          match indexLookup dt idxResult.value with
          | none => none
          | some (name, _) =>
            match decodeString bs pos with
            | none => none
            | some valResult =>
              decodeHeadersGo bs (pos + valResult.consumed) dt ((name, valResult.value) :: acc)
        else
          match decodeString bs pos with
          | none => none
          | some nameResult =>
            let pos' := pos + nameResult.consumed
            match decodeString bs pos' with
            | none => none
            | some valResult =>
              decodeHeadersGo bs (pos' + valResult.consumed) dt
                ((nameResult.value, valResult.value) :: acc)
termination_by bs.size - offset
decreasing_by
  all_goals (have hc := decodeInteger_consumed h; simp_wf; omega)

/-- Decode a complete HPACK header block into a list of header fields.
    Updates the dynamic table as fields with indexing are decoded.

    $$\text{decodeHeaders} : \text{DynamicTable} \to \text{ByteArray} \to \text{Option}(\text{List}(\text{HeaderField}) \times \text{DynamicTable})$$ -/
def decodeHeaders (dt : DynamicTable) (bs : ByteArray) : Option (List HeaderField × DynamicTable) :=
  decodeHeadersGo bs 0 dt []

end Network.HTTP2.HPACK
