/-
  Tests for `Linen.Data.PDF.Document.PageNode`.

  Builds two tiny synthetic PDFs: a well-formed three-node page tree (a
  root `/Pages` node with two `/Page` leaves) exercising
  `pageNodeNKids`/`pageNodeParent`/`pageNodeKids`/`loadPageNode`/
  `pageNodePageByNum`, and a malformed two-node page tree whose `/Kids`
  entries cycle back on each other, confirming `pageNodePageByNum`
  terminates with a clear "cycle detected" error rather than looping
  forever (see the module doc-comment's fuel/visited-set termination
  argument). Everything here is `IO`-based, so it is checked with `#eval`,
  following `Tests/Linen/Data/PDF/Core/FileTest.lean`'s pattern.
-/
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Document.PageNode

open Data.PDF.Core.Object Data.PDF.Core.Object.Util
open Data.PDF.Document.Pdf (fromBytes)
open Data.PDF.Document.PageNode

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
    String.mk (List.replicate (10 - s.length) '0') ++ s
  let entries := offsets.map fun off => pad off ++ " 00000 n \n"
  let n := objBodies.length
  let xrefTable := s!"xref\n0 {n + 1}\n0000000000 65535 f \n" ++ String.join entries
  let trailerText := s!"trailer\n<< /Size {n + 1} {trailerExtra} >>\n"
  let startxrefText := s!"startxref\n{xrefOff}\n%%EOF"
  String.toUTF8 (body ++ xrefTable ++ trailerText ++ startxrefText)

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ### A well-formed three-node page tree ── -/

-- Object 1: root `/Pages` node, two leaf children (objects 2 and 3).
-- Objects 2, 3: `/Page` leaves, `/Parent` pointing back at object 1.
private def wellFormedBytes : ByteArray :=
  buildPdf
    [ "<< /Type /Pages /Kids [2 0 R 3 0 R] /Count 2 >>",
      "<< /Type /Page /Parent 1 0 R >>",
      "<< /Type /Page /Parent 1 0 R >>" ]
    "/Root 1 0 R"

private def mkRoot (bytes : ByteArray) : IO PageNode := do
  let pdf ← fromBytes bytes
  let obj ← Data.PDF.Document.Pdf.lookupObject pdf (⟨1, 0⟩ : Ref)
  match dictValue obj with
  | some d => pure { pdf := pdf, ref := (⟨1, 0⟩ : Ref), dict := d }
  | none => throw (IO.userError "expected object 1 to be a dictionary")

namespace Tests.Data.PDF.Document.PageNode

-- `pageNodeNKids` reads the root node's `/Count`.
#eval show IO Unit from do
  let root ← mkRoot wellFormedBytes
  let n ← pageNodeNKids root
  unless n == 2 do throw (IO.userError s!"unexpected /Count: {n}")

-- `pageNodeKids` reads the root node's `/Kids`.
#eval show IO Unit from do
  let root ← mkRoot wellFormedBytes
  let kids ← pageNodeKids root
  unless kids == [(⟨2, 0⟩ : Ref), (⟨3, 0⟩ : Ref)] do
    throw (IO.userError s!"unexpected /Kids: {reprStr kids}")

-- `loadPageNode` dispatches a leaf reference to `.leaf`.
#eval show IO Unit from do
  let root ← mkRoot wellFormedBytes
  match ← loadPageNode root.pdf (⟨2, 0⟩ : Ref) with
  | .leaf page => unless page.ref == (⟨2, 0⟩ : Ref) do throw (IO.userError "unexpected leaf ref")
  | .node _ => throw (IO.userError "expected a leaf")

-- `pageNodeParent` reports `none` for the root node (it has no `/Parent`).
#eval show IO Unit from do
  let root ← mkRoot wellFormedBytes
  match ← pageNodeParent root with
  | none => pure ()
  | some _ => throw (IO.userError "expected the root node to have no parent")

-- `pageNodePageByNum` finds each leaf page by its 0-based index.
#eval show IO Unit from do
  let root ← mkRoot wellFormedBytes
  let page0 ← pageNodePageByNum root 0
  unless page0.ref == (⟨2, 0⟩ : Ref) do throw (IO.userError "unexpected page 0")
  let page1 ← pageNodePageByNum root 1
  unless page1.ref == (⟨3, 0⟩ : Ref) do throw (IO.userError "unexpected page 1")

-- An out-of-range index reports "Page not found" rather than crashing.
#eval show IO Unit from do
  let root ← mkRoot wellFormedBytes
  let failed ← MonadExcept.tryCatch
    (do let _ ← pageNodePageByNum root 5; pure false)
    (fun _ => pure true)
  unless failed do throw (IO.userError "expected an error for an out-of-range page number")

/-! ### A malformed, cyclic page tree ── -/

-- Objects 1, 2: two `/Pages` interior nodes whose `/Kids` point at each
-- other, with no leaf anywhere — a malformed file with no well-defined
-- answer. Upstream loops forever on this input; the ported
-- `pageNodePageByNum` instead detects the cycle via its visited-`Ref` set
-- (see the module doc-comment) and throws promptly.
private def cyclicBytes : ByteArray :=
  buildPdf
    [ "<< /Type /Pages /Kids [2 0 R] /Count 1 >>",
      "<< /Type /Pages /Kids [1 0 R] /Count 1 >>" ]
    "/Root 1 0 R"

#eval show IO Unit from do
  let root ← mkRoot cyclicBytes
  let msg ← MonadExcept.tryCatch
    (do let _ ← pageNodePageByNum root 0; pure "no error")
    (fun e => pure (toString e))
  unless (msg.splitOn "cycle").length > 1 do
    throw (IO.userError s!"expected a cycle-detected error, got: {msg}")

end Tests.Data.PDF.Document.PageNode
