/-
  Tests for `Linen.Data.PDF.Document.Document`.

  Builds a tiny, well-formed synthetic PDF byte buffer (a `/Catalog`
  dictionary at object 1, an info dictionary at object 2, and a trailer
  referencing both plus a direct `/Encrypt` dictionary) and exercises
  `documentCatalog`/`documentInfo`/`documentEncryption` end-to-end through
  it. Everything here is `IO`-based, so it is checked with `#eval`,
  following `Tests/Linen/Data/PDF/Core/FileTest.lean`'s pattern.
-/
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Document.Document

open Data.PDF.Core.Object Data.PDF.Core.Object.Util
open Data.PDF.Document.Pdf (fromBytes)
open Data.PDF.Document.Document

private def obj1Text : String := "1 0 obj\n<< /Type /Catalog >>\nendobj\n"
private def obj2Text : String := "2 0 obj\n<< /Title (hi) >>\nendobj\n"

/-- Two indirect objects (a `/Catalog` dictionary at object 1, an info
    dictionary at object 2), a three-entry classic xref table, and a
    trailer referencing both plus a direct (non-indirect) `/Encrypt`
    dictionary. Offsets are computed from the pieces themselves. -/
private def mkDoc : String :=
  let obj2Off := (String.toUTF8 obj1Text).size
  let body := obj1Text ++ obj2Text
  let xrefOff := (String.toUTF8 body).size
  let pad (n : Nat) (off : Nat) : String :=
    let s := toString off
    String.mk (List.replicate (n - s.length) '0') ++ s
  let xrefTable :=
    "xref\n0 3\n" ++
    "0000000000 65535 f \n" ++
    pad 10 0 ++ " 00000 n \n" ++
    pad 10 obj2Off ++ " 00000 n \n"
  let trailerText :=
    "trailer\n<< /Size 3 /Root 1 0 R /Info 2 0 R /Encrypt << /Filter /Standard >> >>\n"
  let startxrefText := "startxref\n" ++ toString xrefOff ++ "\n%%EOF"
  body ++ xrefTable ++ trailerText ++ startxrefText

private def bytes : ByteArray := String.toUTF8 mkDoc

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

namespace Tests.Data.PDF.Document.Document

/-! ### `documentCatalog` -/

-- `documentCatalog` follows the trailer's `/Root` reference to the
-- `/Catalog` dictionary at object 1.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let doc : Document := { pdf := pdf, dict := ← Data.PDF.Core.File.lastTrailer pdf.file }
  let cat ← documentCatalog doc
  unless cat.ref == (⟨1, 0⟩ : Ref) do
    throw (IO.userError s!"unexpected catalog ref: {reprStr cat.ref}")
  match cat.dict.get? (mkName "Type") with
  | some (.name n) => unless n == mkName "Catalog" do throw (IO.userError "unexpected /Type")
  | _ => throw (IO.userError "missing /Type in catalog")

/-! ### `documentInfo` -/

-- `documentInfo` follows the trailer's `/Info` reference to object 2.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let doc : Document := { pdf := pdf, dict := ← Data.PDF.Core.File.lastTrailer pdf.file }
  match ← documentInfo doc with
  | some info => unless info.ref == (⟨2, 0⟩ : Ref) do
      throw (IO.userError s!"unexpected info ref: {reprStr info.ref}")
  | none => throw (IO.userError "expected a present /Info")

-- A trailer with no `/Info` entry reports `none`.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let doc : Document := { pdf := pdf, dict := (∅ : Dict) }
  match ← documentInfo doc with
  | none => pure ()
  | some _ => throw (IO.userError "expected no /Info")

/-! ### `documentEncryption` -/

-- `documentEncryption` resolves the trailer's direct (non-indirect)
-- `/Encrypt` dictionary.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let doc : Document := { pdf := pdf, dict := ← Data.PDF.Core.File.lastTrailer pdf.file }
  match ← documentEncryption doc with
  | some d =>
    match d.get? (mkName "Filter") with
    | some (.name n) => unless n == mkName "Standard" do
        throw (IO.userError "unexpected /Filter")
    | _ => throw (IO.userError "missing /Filter in encryption dictionary")
  | none => throw (IO.userError "expected a present /Encrypt")

-- A trailer with no `/Encrypt` entry reports `none`.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let doc : Document := { pdf := pdf, dict := (∅ : Dict) }
  match ← documentEncryption doc with
  | none => pure ()
  | some _ => throw (IO.userError "expected no /Encrypt")

end Tests.Data.PDF.Document.Document
