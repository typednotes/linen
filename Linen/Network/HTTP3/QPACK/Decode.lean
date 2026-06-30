/-
  Linen.Network.HTTP3.QPACK.Decode -- QPACK header decoding

  Decodes QPACK-encoded header fields (RFC 9204).
  This implementation handles static-table-only decoding.

  ## Design

  QPACK decoding recognises the three encoding formats:
  1. Indexed field line (static table)
  2. Literal field line with name reference (static table)
  3. Literal field line with literal name

  Since we only support static-table mode (Required Insert Count = 0),
  dynamic table references will produce an error.

  ## Guarantees

  - Decoding rejects Required Insert Count != 0 (dynamic table not supported)
  - Static table lookups are bounds-checked
  - Total: `decodeQInt`'s continuation is a bounded structural fold and the
    header loop is well-founded on the unconsumed input (no `partial`, no fuel)

  ## Haskell equivalent
  QPACK decoding from the `http3` package
-/

import Linen.Network.HTTP3.QPACK.Table

namespace Network.HTTP3.QPACK

/-- Helper: get byte at offset, returning 0 if out of bounds. -/
@[inline] private def getByte (buf : ByteArray) (i : Nat) : UInt8 :=
  if h : i < buf.size then buf[i] else 0

/-- Decode a QPACK integer with the given prefix bit width (RFC 9204 Section 4.1.1).
    The variable-length continuation is read as a structural fold over the
    in-bounds continuation positions rather than a fuel-bounded recursion.
    $$\text{decodeQInt}(n, \text{buf}, \text{off}) = \text{Option}(\text{value} \times \text{bytesConsumed})$$ -/
def decodeQInt (prefixBits : Nat) (buf : ByteArray) (offset : Nat) : Option (Nat × Nat) :=
  if offset ≥ buf.size then none
  else
    let mask := ((1 <<< prefixBits) - 1).toUInt8
    let maxPfx := mask.toNat
    let prefixVal := (getByte buf offset &&& mask).toNat
    if prefixVal < maxPfx then
      some (prefixVal, 1)
    else
      -- Continuation bytes start at offset+1; accumulate until the high bit
      -- is clear.  `done` records the number of continuation bytes consumed.
      let conts : List (Nat × UInt8) := (List.range (buf.size - offset)).filterMap (fun i =>
        let p := offset + 1 + i
        if p < buf.size then some (i, getByte buf p) else none)
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
          (none, maxPfx, 0) with
      | (some contConsumed, value, _) => some (value, contConsumed + 1)
      | (none, _, _) => none

/-- Every successful QPACK integer decode consumes at least one byte. -/
theorem decodeQInt_consumed {prefixBits : Nat} {buf : ByteArray} {offset v n : Nat}
    (h : decodeQInt prefixBits buf offset = some (v, n)) : 0 < n := by
  rw [decodeQInt] at h
  dsimp only at h
  split at h
  · simp at h
  · split at h
    · simp only [Option.some.injEq, Prod.mk.injEq] at h; omega
    · split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h; omega
      · simp at h

/-- Decode a QPACK string literal (RFC 9204 Section 4.1.2).
    Invalid UTF-8 yields `none` (rather than panicking), matching the decoder's
    `Option` error contract.
    $$\text{decodeStringLiteral} : \text{ByteArray} \to \mathbb{N} \to \text{Option}(\text{String} \times \mathbb{N})$$ -/
def decodeStringLiteral (buf : ByteArray) (offset : Nat) : Option (String × Nat) := do
  let (strLen, lenBytes) ← decodeQInt 7 buf offset
  let strStart := offset + lenBytes
  let strEnd := strStart + strLen
  if strEnd ≤ buf.size then
    let payload := buf.extract strStart strEnd
    let s ← String.fromUTF8? payload
    some (s, lenBytes + strLen)
  else none

set_option linter.unusedVariables false in
/-- Worker for `decodeHeaderEntries`: decode field lines from `pos`.  Well-founded
    on `buf.size - pos`; each instruction advances `pos` by at least one byte
    (a leading `decodeQInt` consuming ≥ 1 by `decodeQInt_consumed`, or the
    literal-name format's explicit `+1`). -/
private def decodeHeaderEntriesGo (buf : ByteArray) (pos : Nat) (acc : List HeaderField) :
    Option (List HeaderField) :=
  if pos ≥ buf.size then some acc
  else
    let firstByte := getByte buf pos
    if (firstByte &&& 0x80) != 0 then
      -- Indexed Field Line: 1Txxxxxx
      let isStatic := (firstByte &&& 0x40) != 0
      if !isStatic then none  -- Dynamic table reference not supported
      else
        match h : decodeQInt 6 buf pos with
        | none => none
        | some (idx, idxLen) =>
          match staticLookup idx with
          | none => none
          | some entry => decodeHeaderEntriesGo buf (pos + idxLen) (acc ++ [entry])
    else if (firstByte &&& 0x40) != 0 then
      -- Literal Field Line With Name Reference: 01NTxxxx
      let isStatic := (firstByte &&& 0x10) != 0
      if !isStatic then none
      else
        match h : decodeQInt 4 buf pos with
        | none => none
        | some (nameIdx, nameIdxLen) =>
          match staticLookup nameIdx with
          | none => none
          | some (name, _) =>
            match decodeStringLiteral buf (pos + nameIdxLen) with
            | none => none
            | some (value, valueLen) =>
              decodeHeaderEntriesGo buf (pos + nameIdxLen + valueLen) (acc ++ [(name, value)])
    else if (firstByte &&& 0x20) != 0 then
      -- Literal Field Line With Literal Name: 001Nxxxx
      match decodeStringLiteral buf (pos + 1) with
      | none => none
      | some (name, nameLen) =>
        match decodeStringLiteral buf (pos + 1 + nameLen) with
        | none => none
        | some (value, valueLen) =>
          decodeHeaderEntriesGo buf (pos + 1 + nameLen + valueLen) (acc ++ [(name, value)])
    else none  -- Not supported in static-only mode
termination_by buf.size - pos
decreasing_by
  all_goals
    first
    | (have hc := decodeQInt_consumed h; simp_wf; omega)
    | (simp_wf; omega)

/-- Decode a list of header fields from a QPACK-encoded header block.
    The input must start with the Encoded Field Section Prefix
    (Required Insert Count + Delta Base).
    $$\text{decodeHeaders} : \text{ByteArray} \to \text{Option}(\text{List HeaderField})$$ -/
def decodeHeaders (buf : ByteArray) : Option (List HeaderField) := do
  -- Decode Required Insert Count (must be 0 for static-only)
  let (reqInsertCount, ricLen) ← decodeQInt 8 buf 0
  if reqInsertCount != 0 then none  -- Dynamic table not supported
  -- Decode Delta Base (ignored when RIC = 0)
  let (_deltaBase, dbLen) ← decodeQInt 7 buf ricLen
  decodeHeaderEntriesGo buf (ricLen + dbLen) []

end Network.HTTP3.QPACK
