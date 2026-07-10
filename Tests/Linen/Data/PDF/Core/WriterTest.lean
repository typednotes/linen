/-
  Tests for `Linen.Data.PDF.Core.Writer`.

  Everything here is `IO`-based, so it is checked with `#eval` — a thrown
  error fails the build — following `Tests/Linen/Data/PDF/Core/FileTest.lean`'s
  pattern. Assertions compare against independently hand-written expected
  bytes (classic xref table) or check the well-known structural pieces of
  the format (xref stream, whose payload is binary rather than ASCII), never
  by calling back into the very rendering functions under test. -/
import Linen.Data.PDF.Core.Writer

open Data.PDF.Core.Object Data.PDF.Core.Writer

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-- Decode a `Data.ByteString` as if it were ASCII/Latin-1 (every byte
    produced by `Writer` outside a binary xref-stream section is plain
    ASCII), for readable substring/equality assertions. -/
private def decodeAscii (bs : Data.ByteString) : String :=
  String.ofList (bs.unpack.map (fun w => Char.ofNat w.toNat))

namespace Tests.Data.PDF.Core.Writer

/-! ### `writeHeader` -/

-- `writeHeader` writes exactly the `%PDF-1.7` header line.
#eval show IO Unit from do
  let w ← makeWriter
  writeHeader w
  let out ← w.output
  let s := decodeAscii out.toStrict
  unless s == "%PDF-1.7\n" do throw (IO.userError s!"unexpected header: {s}")

/-! ### `writeObject` -/

-- `writeObject` wraps the object between `obj`/`endobj`, tagged with its ref.
#eval show IO Unit from do
  let w ← makeWriter
  writeObject w (⟨1, 0⟩ : Ref) (Object.number 42)
  let out ← w.output
  let s := decodeAscii out.toStrict
  unless s == "\n1 0 obj\n42\nendobj\n" do throw (IO.userError s!"unexpected object bytes: {s}")

-- `countWritten` reports the position the *next* write will land at.
#eval show IO Unit from do
  let w ← makeWriter
  writeHeader w
  let pos ← countWritten w
  unless pos == "%PDF-1.7\n".length do throw (IO.userError s!"unexpected position: {pos}")
  writeObject w (⟨1, 0⟩ : Ref) (Object.number 1)
  let pos2 ← countWritten w
  unless pos2 == "%PDF-1.7\n".length + "\n1 0 obj\n1\nendobj\n".length do
    throw (IO.userError s!"unexpected position after writeObject: {pos2}")

-- Writing the same index twice is rejected (mirrors upstream's `addElem`).
#eval show IO Unit from do
  let w ← makeWriter
  writeObject w (⟨1, 0⟩ : Ref) (Object.number 1)
  MonadExcept.tryCatch
    (do
      writeObject w (⟨1, 0⟩ : Ref) (Object.number 2)
      throw (IO.userError "expected a duplicate-index error"))
    (fun _ => pure ())

/-! ### `deleteObject` -/

-- `deleteObject` tracks a free entry without writing any object bytes.
#eval show IO Unit from do
  let w ← makeWriter
  deleteObject w (⟨0, 0⟩ : Ref) 0
  let out ← w.output
  unless out.length == 0 do throw (IO.userError "deleteObject should write no bytes")

/-! ### `writeXRefTable` -/

-- A header, one in-use object, one free (deleted) object, and a classic
-- xref table produce exactly the expected byte-for-byte PDF32000-1:2008
-- §7.5.4 layout: a `0 2` section header, a fixed-width 20-byte line per
-- entry (`\r\n`-terminated), then the trailer and `startxref`/`%%EOF` tail.
#eval show IO Unit from do
  let w ← makeWriter
  writeHeader w
  writeObject w (⟨1, 0⟩ : Ref) (Object.number 42)
  deleteObject w (⟨0, 0⟩ : Ref) 0
  let tr : Dict := Std.HashMap.ofList [(mkName "Size", Object.number 2)]
  writeXRefTable w 0 tr
  let out ← w.output
  let s := decodeAscii out.toStrict
  let header := "%PDF-1.7\n"
  let obj := "\n1 0 obj\n42\nendobj\n"
  let off := header.length + obj.length
  let expected :=
    header ++ obj ++
    "xref\n0 2\n" ++
    "0000000000 00000 f\r\n" ++
    "0000000009 00000 n\r\n" ++
    "trailer\n<</Size 2>>\n" ++
    "startxref\n" ++ toString off ++ "\n%%EOF\n"
  unless s == expected do throw (IO.userError s!"unexpected xref-table output:\n{s}\nexpected:\n{expected}")

/-! ### `writeXRefStream` -/

-- A header, one object, and an xref stream (instead of a classic table)
-- produce the expected structural pieces: no classic `xref` table keyword,
-- the xref-stream object's dictionary carries `/Type /XRef`, `/W`, `/Index`
-- and a `/Length` but no `/Filter`, and the output still ends with the
-- mandatory `startxref`/`%%EOF` tail at the right offset.
#eval show IO Unit from do
  let w ← makeWriter
  writeHeader w
  writeObject w (⟨1, 0⟩ : Ref) (Object.number 7)
  let posBeforeXRef ← countWritten w
  let tr : Dict := Std.HashMap.ofList [(mkName "Size", Object.number 3)]
  writeXRefStream w 0 (⟨2, 0⟩ : Ref) tr
  let out ← w.output
  let s := decodeAscii out.toStrict
  unless s.startsWith "%PDF-1.7\n\n1 0 obj\n7\nendobj\n\n2 0 obj\n<<" do
    throw (IO.userError s!"unexpected xref-stream object header:\n{s}")
  -- Note: `s` legitimately contains the substring `"xref\n"` as the tail of
  -- `"startxref\n"`; the classic table's own `xref` keyword is instead always
  -- preceded by a bare newline (`"\nxref\n"`), which `"...startxref\n"` never is.
  unless (s.splitOn "\nxref\n").length == 1 do
    throw (IO.userError "unexpected classic 'xref' keyword in an xref-stream document")
  unless s.contains "/Type" && s.contains "/XRef" do
    throw (IO.userError "missing /Type /XRef in the xref-stream dictionary")
  unless s.contains "/W" do throw (IO.userError "missing /W in the xref-stream dictionary")
  unless s.contains "/Index" do throw (IO.userError "missing /Index in the xref-stream dictionary")
  unless s.contains "/Length" do throw (IO.userError "missing /Length in the xref-stream dictionary")
  unless s.contains "/Filter" == false do
    throw (IO.userError "unexpected /Filter left in the xref-stream dictionary")
  unless s.contains "/Size 3" do throw (IO.userError "missing /Size 3 (carried over from the trailer)")
  unless s.contains "stream\n" && s.contains "\nendstream\nendobj\n" do
    throw (IO.userError "missing stream/endstream wrapper around the xref stream")
  unless s.endsWith s!"\nstartxref\n{posBeforeXRef}\n%%EOF\n" do
    throw (IO.userError s!"unexpected startxref/%%EOF tail:\n{s}")

/-! ### `xrefSections`/`xrefSectionIndex` -/

-- Contiguous indices are grouped into one section.
#guard
  (xrefSections
    [{ index := 0, generation := 0, offset := 0, free := true },
     { index := 1, generation := 0, offset := 10, free := false }]).length == 1

-- A gap in the indices starts a new section.
#guard
  (xrefSections
    [{ index := 0, generation := 0, offset := 0, free := true },
     { index := 5, generation := 0, offset := 10, free := false }]).length == 2

-- `xrefSectionIndex` reports each section's first index and element count.
#guard
  match xrefSections
    [{ index := 2, generation := 0, offset := 0, free := true },
     { index := 3, generation := 0, offset := 10, free := false }] with
  | [sec] => xrefSectionIndex sec == [2, 2]
  | _ => false

end Tests.Data.PDF.Core.Writer
