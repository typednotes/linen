/-
  Tests for `Linen.Data.PDF.Document.Catalog`.

  Builds a tiny, well-formed synthetic PDF byte buffer (a `/Catalog`
  dictionary at object 1 pointing at a `/Pages` root node at object 2) and
  exercises `catalogPageNode` end-to-end through it. Everything here is
  `IO`-based, so it is checked with `#eval`, following
  `Tests/Linen/Data/PDF/Core/FileTest.lean`'s pattern.
-/
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Document.Catalog

open Data.PDF.Core.Object Data.PDF.Core.Object.Util
open Data.PDF.Document.Pdf (fromBytes)
open Data.PDF.Document.Catalog

private def obj1Text : String := "1 0 obj\n<< /Type /Catalog /Pages 2 0 R >>\nendobj\n"
private def obj2Text : String := "2 0 obj\n<< /Type /Pages /Kids [] /Count 0 >>\nendobj\n"

/-- A `/Catalog` dictionary at object 1 pointing at a `/Pages` root node at
    object 2, plus a classic xref table and its trailer. Offsets are
    computed from the pieces themselves. -/
private def mkDoc : String :=
  let obj2Off := (String.toUTF8 obj1Text).size
  let body := obj1Text ++ obj2Text
  let xrefOff := (String.toUTF8 body).size
  let pad (n : Nat) (off : Nat) : String :=
    let s := toString off
    String.ofList (List.replicate (n - s.length) '0') ++ s
  let xrefTable :=
    "xref\n0 3\n" ++
    "0000000000 65535 f \n" ++
    pad 10 0 ++ " 00000 n \n" ++
    pad 10 obj2Off ++ " 00000 n \n"
  let trailerText := "trailer\n<< /Size 3 /Root 1 0 R >>\n"
  let startxrefText := "startxref\n" ++ toString xrefOff ++ "\n%%EOF"
  body ++ xrefTable ++ trailerText ++ startxrefText

private def bytes : ByteArray := String.toUTF8 mkDoc

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

private def mkCatalog : IO Catalog := do
  let pdf ← fromBytes bytes
  let obj ← Data.PDF.Document.Pdf.lookupObject pdf (⟨1, 0⟩ : Ref)
  match dictValue obj with
  | some d => pure { pdf := pdf, ref := (⟨1, 0⟩ : Ref), dict := d }
  | none => throw (IO.userError "expected object 1 to be a dictionary")

namespace Tests.Data.PDF.Document.Catalog

-- `catalogPageNode` follows the catalog's `/Pages` reference to the root
-- page-tree node.
#eval show IO Unit from do
  let cat ← mkCatalog
  let root ← catalogPageNode cat
  unless root.ref == (⟨2, 0⟩ : Ref) do
    throw (IO.userError s!"unexpected root node ref: {reprStr root.ref}")
  match root.dict.get? (mkName "Type") with
  | some (.name n) => unless n == mkName "Pages" do throw (IO.userError "unexpected /Type")
  | _ => throw (IO.userError "missing /Type in root node")

-- A catalog with no `/Pages` entry fails with a clear error rather than
-- crashing.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let cat : Catalog := { pdf := pdf, ref := (⟨1, 0⟩ : Ref), dict := (∅ : Dict) }
  let failed ← MonadExcept.tryCatch
    (do let _ ← catalogPageNode cat; pure false)
    (fun _ => pure true)
  unless failed do throw (IO.userError "expected an error for a missing /Pages entry")

end Tests.Data.PDF.Document.Catalog
