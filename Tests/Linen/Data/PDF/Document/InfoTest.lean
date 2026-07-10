/-
  Tests for `Linen.Data.PDF.Document.Info`.

  Builds a tiny, well-formed synthetic PDF byte buffer with an info
  dictionary carrying every field `Data.PDF.Document.Info` reads, and
  exercises each of the six accessors against it (plus a missing-field
  case). Everything here is `IO`-based, so it is checked with `#eval`,
  following `Tests/Linen/Data/PDF/Core/FileTest.lean`'s pattern.
-/
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Document.Info

open Data.PDF.Core.Object
open Data.PDF.Document.Pdf (fromBytes)
open Data.PDF.Document.Info

/-- One indirect object (object 1): an info dictionary with every field
    `Data.PDF.Document.Info` reads, plus one classic xref table and its
    trailer (the trailer's own `/Root`/`/Info` don't matter here — the
    tests build an `Info` handle directly). -/
private def obj1Text : String :=
  "1 0 obj\n<< /Title (My Title) /Author (Ann) /Subject (Sub) " ++
  "/Keywords (kw1 kw2) /Creator (App) /Producer (Prod) >>\nendobj\n"

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

namespace Tests.Data.PDF.Document.Info

private def mkInfo : IO Info := do
  let pdf ← fromBytes bytes
  let obj ← Data.PDF.Document.Pdf.lookupObject pdf (⟨1, 0⟩ : Ref)
  match Data.PDF.Core.Object.Util.dictValue obj with
  | some d => pure { pdf := pdf, ref := (⟨1, 0⟩ : Ref), dict := d }
  | none => throw (IO.userError "expected object 1 to be a dictionary")

/-! ### Present fields -/

#eval show IO Unit from do
  let info ← mkInfo
  match ← infoTitle info with
  | some t => unless t == "My Title" do throw (IO.userError s!"unexpected title: {t}")
  | none => throw (IO.userError "expected a present /Title")

#eval show IO Unit from do
  let info ← mkInfo
  match ← infoAuthor info with
  | some t => unless t == "Ann" do throw (IO.userError s!"unexpected author: {t}")
  | none => throw (IO.userError "expected a present /Author")

#eval show IO Unit from do
  let info ← mkInfo
  match ← infoSubject info with
  | some t => unless t == "Sub" do throw (IO.userError s!"unexpected subject: {t}")
  | none => throw (IO.userError "expected a present /Subject")

#eval show IO Unit from do
  let info ← mkInfo
  match ← infoKeywords info with
  | some t => unless t == "kw1 kw2" do throw (IO.userError s!"unexpected keywords: {t}")
  | none => throw (IO.userError "expected present /Keywords")

#eval show IO Unit from do
  let info ← mkInfo
  match ← infoCreator info with
  | some t => unless t == "App" do throw (IO.userError s!"unexpected creator: {t}")
  | none => throw (IO.userError "expected a present /Creator")

#eval show IO Unit from do
  let info ← mkInfo
  match ← infoProducer info with
  | some t => unless t == "Prod" do throw (IO.userError s!"unexpected producer: {t}")
  | none => throw (IO.userError "expected a present /Producer")

/-! ### A missing field -/

-- An info dictionary with no `/Title` entry reports `none`, rather than
-- throwing.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let info : Info := { pdf := pdf, ref := (⟨1, 0⟩ : Ref), dict := (∅ : Dict) }
  match ← infoTitle info with
  | none => pure ()
  | some t => throw (IO.userError s!"expected no /Title, got: {t}")

end Tests.Data.PDF.Document.Info
