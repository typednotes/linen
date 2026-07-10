/-
  Tests for `Linen.Data.PDF.Document.FontDict`.

  Builds a tiny, well-formed synthetic PDF byte buffer with a single Type 1
  font dictionary (object 1) and exercises `fontDictSubtype`/
  `fontDictLoadInfo` end-to-end through it. Everything here is `IO`-based,
  so it is checked with `#eval`, following
  `Tests/Linen/Data/PDF/Core/FileTest.lean`'s pattern.
-/
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Document.FontDict

open Data.PDF.Core.Object Data.PDF.Core.Object.Util
open Data.PDF.Document.Pdf (fromBytes)
open Data.PDF.Document.FontDict
open Data.PDF.Content (FontInfo)

private def obj1Text : String :=
  "1 0 obj\n<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>\nendobj\n"

/-- One indirect object (a Type 1 font dictionary at object 1), a
    two-entry classic xref table, and its trailer. -/
private def mkDoc : String :=
  let xrefOff := (String.toUTF8 obj1Text).size
  let xrefTable :=
    "xref\n0 2\n" ++
    "0000000000 65535 f \n" ++
    "0000000000 00000 n \n"
  let trailerText := "trailer\n<< /Size 2 /Root 1 0 R >>\n"
  let startxrefText := "startxref\n" ++ toString xrefOff ++ "\n%%EOF"
  obj1Text ++ xrefTable ++ trailerText ++ startxrefText

private def bytes : ByteArray := String.toUTF8 mkDoc

private def mkFontDict : IO FontDict := do
  let pdf ← fromBytes bytes
  let obj ← Data.PDF.Document.Pdf.lookupObject pdf (⟨1, 0⟩ : Ref)
  match dictValue obj with
  | some d => pure { pdf := pdf, dict := d }
  | none => throw (IO.userError "expected object 1 to be a dictionary")

namespace Tests.Data.PDF.Document.FontDict

-- `fontDictSubtype` classifies a `/Type1` font dictionary as `.type1`.
#eval show IO Unit from do
  let fd ← mkFontDict
  match ← fontDictSubtype fd with
  | .type1 => pure ()
  | other => throw (IO.userError s!"unexpected subtype: {reprStr other}")

-- `fontDictLoadInfo` succeeds on a minimal simple font (no `/Widths`,
-- `/Encoding` or `/FontDescriptor`), producing a `.simple` `FontInfo` with
-- every optional field absent.
#eval show IO Unit from do
  let fd ← mkFontDict
  match ← fontDictLoadInfo fd with
  | .simple fi =>
    unless fi.fiSimpleWidths.isNone do throw (IO.userError "expected no /Widths")
    unless fi.fiSimpleEncoding.isNone do throw (IO.userError "expected no /Encoding")
    unless fi.fiSimpleFontDescriptor.isNone do throw (IO.userError "expected no /FontDescriptor")
  | .composite _ => throw (IO.userError "expected a simple font")

end Tests.Data.PDF.Document.FontDict
