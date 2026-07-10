/-
  Tests for `Linen.Data.PDF.Document.Page`.

  Builds three tiny synthetic PDFs:

  - A well-formed page (object 2) with no `/MediaBox` of its own but a
    `/Parent` (object 1) that has one, exercising `pageMediaBox`'s ascent;
    plus `/Contents` and `/Resources/Font` accessors.
  - A malformed page whose Form XObject's own `/Resources/XObject` points
    back at itself, confirming `pageXObjects` (and so `pageExtractGlyphs`)
    terminates with a clear "cycle detected" error rather than looping
    forever (see the module doc-comment's fuel/visited-set termination
    argument).
  - A malformed page whose `/Parent` chain cycles back on itself,
    confirming `pageMediaBox` terminates the same way.

  Everything here is `IO`-based, so it is checked with `#eval`, following
  `Tests/Linen/Data/PDF/Core/FileTest.lean`'s pattern.
-/
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Document.Page

open Data.PDF.Core.Object Data.PDF.Core.Object.Util
open Data.PDF.Document.Pdf (fromBytes)
open Data.PDF.Document.Page

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

/-! ### A well-formed page, inheriting its `/MediaBox` from its parent ── -/

-- Object 1: root `/Pages` node with a `/MediaBox` of its own.
-- Object 2: the page itself, `/Parent` object 1, no `/MediaBox` of its own,
--   one content stream (object 3), one font (object 4, via `/Resources`).
-- Object 3: a trivial (empty) content stream.
-- Object 4: a minimal Type 1 font dictionary.
private def wellFormedBytes : ByteArray :=
  buildPdf
    [ "<< /Type /Pages /Kids [2 0 R] /Count 1 /MediaBox [0 0 200 200] >>",
      "<< /Type /Page /Parent 1 0 R /Contents 3 0 R " ++
        "/Resources << /Font << /F1 4 0 R >> >> >>",
      "<< /Length 0 >>\nstream\n\nendstream",
      "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>" ]
    "/Root 1 0 R"

private def mkPage (bytes : ByteArray) (ref : Ref) : IO Page := do
  let pdf ← fromBytes bytes
  let obj ← Data.PDF.Document.Pdf.lookupObject pdf ref
  match dictValue obj with
  | some d => pure { pdf := pdf, ref := ref, dict := d }
  | none => throw (IO.userError "expected a dictionary")

namespace Tests.Data.PDF.Document.Page

-- `pageMediaBox` ascends via `pageParentNode` to find the inherited
-- `/MediaBox`.
#eval show IO Unit from do
  let page ← mkPage wellFormedBytes (⟨2, 0⟩ : Ref)
  let box ← pageMediaBox page
  unless box.llx == 0 ∧ box.lly == 0 ∧ box.urx == 200 ∧ box.ury == 200 do
    throw (IO.userError s!"unexpected media box: {reprStr box}")

-- `pageContents` reads the page's single content-stream reference.
#eval show IO Unit from do
  let page ← mkPage wellFormedBytes (⟨2, 0⟩ : Ref)
  let refs ← pageContents page
  unless refs == [(⟨3, 0⟩ : Ref)] do throw (IO.userError s!"unexpected contents: {reprStr refs}")

-- `pageFontDicts` resolves the page's `/Resources/Font` dictionary.
#eval show IO Unit from do
  let page ← mkPage wellFormedBytes (⟨2, 0⟩ : Ref)
  let fonts ← pageFontDicts page
  unless fonts.length == 1 do throw (IO.userError s!"unexpected font count: {fonts.length}")

-- `pageXObjects` reports no XObjects when `/Resources` has none.
#eval show IO Unit from do
  let page ← mkPage wellFormedBytes (⟨2, 0⟩ : Ref)
  let xobjs ← pageXObjects page
  unless xobjs.isEmpty do throw (IO.userError "expected no XObjects")

-- `pageExtractText`/`pageExtractGlyphs` succeed end-to-end on a page with
-- an empty content stream, producing no text.
#eval show IO Unit from do
  let page ← mkPage wellFormedBytes (⟨2, 0⟩ : Ref)
  let text ← pageExtractText page
  unless text == "" do throw (IO.userError s!"unexpected extracted text: {text}")

/-! ### A malformed page: a `/Parent` chain that cycles ── -/

-- Objects 1, 2: two `/Pages` interior nodes whose `/Parent` point at each
-- other; object 3: a page whose `/Parent` is object 1, with no
-- `/MediaBox` anywhere in the chain. Upstream's `mediaBoxRec` loops
-- forever on this input; the ported `pageMediaBox` instead detects the
-- cycle via its visited-`Ref` set (see the module doc-comment).
private def cyclicParentBytes : ByteArray :=
  buildPdf
    [ "<< /Type /Pages /Parent 2 0 R /Kids [3 0 R] /Count 1 >>",
      "<< /Type /Pages /Parent 1 0 R /Kids [] /Count 0 >>",
      "<< /Type /Page /Parent 1 0 R >>" ]
    "/Root 1 0 R"

#eval show IO Unit from do
  let page ← mkPage cyclicParentBytes (⟨3, 0⟩ : Ref)
  let msg ← MonadExcept.tryCatch
    (do let _ ← pageMediaBox page; pure "no error")
    (fun e => pure (toString e))
  unless (msg.splitOn "cycle").length > 1 do
    throw (IO.userError s!"expected a cycle-detected error, got: {msg}")

/-! ### A malformed page: a nested Form XObject that cycles ── -/

-- Object 1: root `/Pages` node. Object 2: the page, `/Resources/XObject`
-- names object 3. Object 3: a Form XObject whose own `/Resources/XObject`
-- names *itself* — a self-referential nested-XObject cycle. Upstream's
-- `dictXObjects` loops forever building an infinitely-deep `XObject`
-- value on this input; the ported `pageXObjects` instead detects the
-- cycle via its visited-`Ref` set (see the module doc-comment).
private def cyclicXObjectBytes : ByteArray :=
  buildPdf
    [ "<< /Type /Pages /Kids [2 0 R] /Count 1 >>",
      "<< /Type /Page /Parent 1 0 R /Resources << /XObject << /X1 3 0 R >> >> >>",
      "<< /Type /XObject /Subtype /Form /Length 0 " ++
        "/Resources << /XObject << /X1 3 0 R >> >> >>\nstream\n\nendstream" ]
    "/Root 1 0 R"

#eval show IO Unit from do
  let page ← mkPage cyclicXObjectBytes (⟨2, 0⟩ : Ref)
  let msg ← MonadExcept.tryCatch
    (do let _ ← pageXObjects page; pure "no error")
    (fun e => pure (toString e))
  unless (msg.splitOn "cycle").length > 1 do
    throw (IO.userError s!"expected a cycle-detected error, got: {msg}")

end Tests.Data.PDF.Document.Page
