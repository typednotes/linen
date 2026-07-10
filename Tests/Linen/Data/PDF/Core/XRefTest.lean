/-
  Tests for `Linen.Data.PDF.Core.XRef`.

  Everything here is `IO`-based (seeking/parsing against a resident
  `Buffer`), so it is checked with `#eval` — a thrown error fails the
  build — following `Tests/Linen/Data/PDF/Core/UtilTest.lean`'s pattern.
-/
import Linen.Data.PDF.Core.XRef

open Data.PDF.Core.Object Data.PDF.Core.Object.Util Data.PDF.Core.XRef

private def bytes (s : String) : ByteArray := String.toUTF8 s

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-- A minimal, well-formed classic xref table for three objects (0
    free, 1 and 2 in use), each row exactly 20 bytes
    (`"nnnnnnnnnn ggggg n \n"`/`"...f \n"`), followed by its trailer. -/
private def xrefTableDoc : String :=
  "xref\n0 3\n" ++
  "0000000000 65535 f \n" ++
  "0000000009 00000 n \n" ++
  "0000000074 00000 n \n" ++
  "trailer\n<< /Size 3 /Root 1 0 R >>\n" ++
  "startxref\n0\n%%EOF"

namespace Tests.Data.PDF.Core.XRef

/-! ### Table detection -/

-- `isTable` recognises `"xref\n"` and consumes it, leaving the stream
-- positioned right after.
#eval show IO Unit from do
  let is ← Data.PDF.Stream.fromByteString (bytes "xref\nrest")
  unless (← isTable is) do
    throw (IO.userError "expected isTable to succeed")
  let rest ← Data.PDF.Stream.readExactly 4 is
  unless rest == bytes "rest" do
    throw (IO.userError s!"unexpected remainder: {rest.toList}")

-- `isTable` fails (and leaves the stream untouched) on anything else.
#eval show IO Unit from do
  let is ← Data.PDF.Stream.fromByteString (bytes "not a table")
  if ← isTable is then
    throw (IO.userError "expected isTable to fail")
  let rest ← Data.PDF.Stream.readExactly 11 is
  unless rest == bytes "not a table" do
    throw (IO.userError s!"isTable consumed input on failure: {rest.toList}")

/-! ### Locating and reading a cross reference -/

-- `readXRef` at a classic table's offset yields `.table`.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes xrefTableDoc)
  match ← readXRef buf 0 with
  | .table off => unless off == 0 do throw (IO.userError s!"unexpected offset: {off}")
  | .stream .. => throw (IO.userError "expected a table xref")

-- `lastXRef` finds the trailing `startxref` marker and reads the xref it
-- points to.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes xrefTableDoc)
  match ← lastXRef buf with
  | .table off => unless off == 0 do throw (IO.userError s!"unexpected offset: {off}")
  | .stream .. => throw (IO.userError "expected a table xref")

/-! ### The trailer dictionary -/

-- `trailer` skips every subsection of a classic table and parses the
-- dictionary that follows it.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes xrefTableDoc)
  let tr ← trailer buf (.table 0)
  match tr.get? (mkName "Size") with
  | some o => unless intValue o == some 3 do throw (IO.userError "unexpected /Size")
  | none => throw (IO.userError "missing /Size in trailer")

-- `prevXRef` follows a trailer's `/Prev` entry to an earlier xref.
#eval show IO Unit from do
  let doc :=
    "xref\n0 1\n0000000000 65535 f \n" ++
    "trailer\n<< /Size 1 /Prev 0 >>"
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes doc)
  match ← prevXRef buf (.table 0) with
  | some (.table off) => unless off == 0 do throw (IO.userError s!"unexpected /Prev offset: {off}")
  | some (.stream ..) => throw (IO.userError "expected a table xref")
  | none => throw (IO.userError "expected a /Prev entry")

-- `prevXRef` reports `none` when there is no `/Prev` entry.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes xrefTableDoc)
  match ← prevXRef buf (.table 0) with
  | none => pure ()
  | some _ => throw (IO.userError "expected no /Prev entry")

/-! ### Looking up entries in an xref table -/

-- `lookupTableEntry` finds an in-use row by object index.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes xrefTableDoc)
  let xref ← readXRef buf 0
  match ← lookupTableEntry buf xref (⟨1, 0⟩ : Ref) with
  | some (.used off gen) =>
    unless off == 9 ∧ gen == 0 do
      throw (IO.userError s!"unexpected entry: offset {off}, generation {gen}")
  | other => throw (IO.userError s!"unexpected lookup result: {reprStr other}")

-- `lookupTableEntry` finds the free-slot row for object `0`. A free
-- row's stored generation column (`65535` here) is only used to decide
-- whether the row is free at all; the entry's `generation` field mirrors
-- upstream in echoing back the *queried* ref's generation, not the file's
-- stored one.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes xrefTableDoc)
  let xref ← readXRef buf 0
  match ← lookupTableEntry buf xref (⟨0, 0⟩ : Ref) with
  | some (.free next gen) =>
    unless next == 0 ∧ gen == 0 do
      throw (IO.userError s!"unexpected free entry: next {next}, generation {gen}")
  | other => throw (IO.userError s!"unexpected lookup result: {reprStr other}")

-- `lookupTableEntry` reports `none` for an object number outside the
-- table's subsections.
#eval show IO Unit from do
  let buf ← Data.PDF.Core.IO.Buffer.fromBytes (bytes xrefTableDoc)
  let xref ← readXRef buf 0
  match ← lookupTableEntry buf xref (⟨99, 0⟩ : Ref) with
  | none => pure ()
  | some other => throw (IO.userError s!"expected no entry, got: {reprStr other}")

/-! ### Looking up entries in an xref stream -/

-- `lookupStreamEntry` decodes a single-object-number, single-`(from, count)`
-- row (`W = [1, 1, 1]`, one byte per field): entry type `1` (in use), offset
-- `9`, generation `0`.
#eval show IO Unit from do
  let dict := Std.HashMap.ofList
    [ (mkName "Size", Object.number 1)
    , (mkName "W", Object.array #[Object.number 1, Object.number 1, Object.number 1]) ]
  let is ← Data.PDF.Stream.fromByteString (ByteArray.mk #[1, 9, 0])
  match ← lookupStreamEntry dict is (⟨0, 0⟩ : Ref) with
  | .ok (some (.used off gen)) =>
    unless off == 9 ∧ gen == 0 do
      throw (IO.userError s!"unexpected entry: offset {off}, generation {gen}")
  | other => throw (IO.userError s!"unexpected lookup result: {reprStr other}")

-- `lookupStreamEntry` surfaces an unrecognized entry-type tag as
-- `Except.error` (see the module doc-comment for why this replaces
-- upstream's `UnknownXRefStreamEntryType` exception).
#eval show IO Unit from do
  let dict := Std.HashMap.ofList
    [ (mkName "Size", Object.number 1)
    , (mkName "W", Object.array #[Object.number 1, Object.number 1, Object.number 1]) ]
  let is ← Data.PDF.Stream.fromByteString (ByteArray.mk #[9, 0, 0])
  match ← lookupStreamEntry dict is (⟨0, 0⟩ : Ref) with
  | .error n => unless n == 9 do throw (IO.userError s!"unexpected error tag: {n}")
  | other => throw (IO.userError s!"expected Except.error, got: {reprStr other}")

end Tests.Data.PDF.Core.XRef
