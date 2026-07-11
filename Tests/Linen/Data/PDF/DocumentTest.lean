/-
  Tests for `Linen.Data.PDF.Document` (module 11, the package aggregator).

  This module carries no logic of its own — it is a thin `export`-based
  re-export of names already defined (and already tested) in
  `Data.PDF.Document.Types`, `.Pdf`, `.Document`, `.Catalog`, `.PageNode`,
  `.Page`, `.Info` and `.FontDict` (see that module's doc-comment). So there
  is nothing new to *test* here beyond confirming the re-export actually
  makes those names reachable, unqualified, under the plain
  `Data.PDF.Document` namespace — i.e. that `import Linen.Data.PDF.Document`
  alone (mirroring upstream's `import Pdf.Document`) gives the intended
  surface, with no need to `open`/import every submodule individually.

  Builds one tiny synthetic PDF (a `/Catalog` at object 1, a one-page
  `/Pages` tree at object 2/3, a `/MediaBox` on the root) and drives it
  end-to-end purely through names reached via `open Data.PDF.Document`.
-/
import Linen.Data.PDF.Document

open Data.PDF.Core.Object (Ref)
open Data.PDF.Document

/-- Build a synthetic classic-format PDF from a list of object bodies (the
    text between `N 0 obj\n` and `\nendobj\n` for object `N`, 1-indexed) and
    extra trailer dictionary entries. Offsets are computed from the pieces
    themselves, so the fixture can't silently drift out of sync. -/
private def buildPdf (objBodies : List String) (trailerExtra : String) : ByteArray :=
  let texts := (objBodies.zip (List.range objBodies.length)).map
    fun (body, i) => s!"{i + 1} 0 obj\n{body}\nendobj\n"
  let offsets :=
    (texts.foldl (fun (offs, cur) t => (offs ++ [cur], cur + (String.toUTF8 t).size)) ([], 0)).1
  let body := String.join texts
  let xrefOff := (String.toUTF8 body).size
  let pad (off : Nat) : String :=
    let s := toString off
    String.ofList (List.replicate (10 - s.length) '0') ++ s
  let entries := offsets.map fun off => pad off ++ " 00000 n \n"
  let n := objBodies.length
  let xrefTable := s!"xref\n0 {n + 1}\n0000000000 65535 f \n" ++ String.join entries
  let trailerText := s!"trailer\n<< /Size {n + 1} {trailerExtra} >>\n"
  let startxrefText := s!"startxref\n{xrefOff}\n%%EOF"
  String.toUTF8 (body ++ xrefTable ++ trailerText ++ startxrefText)

-- Object 1: `/Catalog`, pointing at the `/Pages` root (object 2).
-- Object 2: `/Pages` root, one leaf child (object 3), with a `/MediaBox`.
-- Object 3: the page leaf.
private def bytes : ByteArray :=
  buildPdf
    [ "<< /Type /Catalog /Pages 2 0 R >>",
      "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>",
      "<< /Type /Page /Parent 2 0 R >>" ]
    "/Root 1 0 R"

-- `Pdf`/`fromBytes`/`document`/`lookupObject`, re-exported from `.Pdf`, are
-- reachable directly under `Data.PDF.Document`.
#eval show IO Unit from do
  let pdf ← fromBytes bytes
  let doc ← document pdf
  -- `Document`/`documentCatalog`, re-exported from `.Document`, are
  -- reachable directly under `Data.PDF.Document`.
  let cat ← documentCatalog doc
  -- `Catalog`/`catalogPageNode`, re-exported from `.Catalog`, are reachable
  -- directly under `Data.PDF.Document`.
  let root ← catalogPageNode cat
  unless root.ref == (⟨2, 0⟩ : Ref) do
    throw (IO.userError s!"unexpected root node ref: {reprStr root.ref}")
  -- `pageNodePageByNum`, re-exported from `.PageNode`, is reachable directly
  -- under `Data.PDF.Document`.
  let page ← pageNodePageByNum root 0
  unless page.ref == (⟨3, 0⟩ : Ref) do
    throw (IO.userError s!"unexpected page ref: {reprStr page.ref}")
  -- `pageMediaBox`, re-exported from `.Page`, is reachable directly under
  -- `Data.PDF.Document`.
  let box ← pageMediaBox page
  unless box.llx == 0 ∧ box.lly == 0 ∧ box.urx == 100 ∧ box.ury == 100 do
    throw (IO.userError s!"unexpected media box: {reprStr box}")
