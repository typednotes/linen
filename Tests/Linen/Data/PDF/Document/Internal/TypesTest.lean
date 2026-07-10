/-
  Tests for `Linen.Data.PDF.Document.Internal.Types`.

  `Pdf` (and everything built on top of it: `Document`/`Catalog`/`Info`/
  `PageNode`/`Page`/`FontDict`) carries a low-level `Data.PDF.Core.File.File`
  plus an `IO.Ref` object cache, so even just constructing one is `IO`-based;
  every test below is checked with `#eval`, following
  `Tests/Linen/Data/PDF/Core/FileTest.lean`'s pattern.
-/
import Linen.Data.PDF.Document.Internal.Types
import Linen.Data.PDF.Core.File

open Data.PDF.Core.Object
open Data.PDF.Document.Internal.Types

namespace Tests.Data.PDF.Document.Internal.Types

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

private def sampleDict : Dict := Std.HashMap.ofList [(mkName "Type", Object.name (mkName "Page"))]

private def sampleRef : Ref := ⟨1, 0⟩

/-- A minimal, well-formed synthetic PDF byte buffer (one indirect object,
    a classic xref table, and a trailer), just enough to build a real
    `Data.PDF.Core.File.File` to wrap into a `Pdf` handle. -/
private def mkDoc : String :=
  let obj1 := "1 0 obj\n42\nendobj\n"
  let xrefOff := (String.toUTF8 obj1).size
  let xrefTable :=
    "xref\n0 2\n" ++
    "0000000000 65535 f \n" ++
    "0000000000 00000 n \n"
  let trailerText := "trailer\n<< /Size 2 /Root 1 0 R >>\n"
  let startxrefText := "startxref\n" ++ toString xrefOff ++ "\n%%EOF"
  obj1 ++ xrefTable ++ trailerText ++ startxrefText

/-- Build a fresh `Pdf` handle (a real `File`, plus a disabled/empty
    object cache) to exercise the structures below against. -/
private def mkPdf : IO Pdf := do
  let file ← Data.PDF.Core.File.fromBytes Data.PDF.Core.Stream.knownFilters
    (String.toUTF8 mkDoc)
  let cache ← IO.mkRef ((false, {}) : ObjectCache)
  pure { file := file, cache := cache }

/-! ### `Document`/`Catalog`/`Info`/`PageNode`/`Page`/`FontDict` field access -/

-- `Catalog`/`Info`/`PageNode`/`Page` all carry a `Ref` and a `Dict`
-- alongside their `Pdf` handle; field projections read them back exactly.
#eval show IO Unit from do
  let pdf ← mkPdf
  let c : Catalog := { pdf := pdf, ref := sampleRef, dict := sampleDict }
  unless c.ref == sampleRef && c.dict == sampleDict do
    throw (IO.userError "Catalog field projection mismatch")
  let i : Info := { pdf := pdf, ref := sampleRef, dict := sampleDict }
  unless i.ref == sampleRef && i.dict == sampleDict do
    throw (IO.userError "Info field projection mismatch")
  let n : PageNode := { pdf := pdf, ref := sampleRef, dict := sampleDict }
  unless n.ref == sampleRef && n.dict == sampleDict do
    throw (IO.userError "PageNode field projection mismatch")
  let p : Page := { pdf := pdf, ref := sampleRef, dict := sampleDict }
  unless p.ref == sampleRef && p.dict == sampleDict do
    throw (IO.userError "Page field projection mismatch")
  -- `FontDict`, unlike its siblings above, has no `ref` field.
  let f : FontDict := { pdf := pdf, dict := sampleDict }
  unless f.dict == sampleDict do
    throw (IO.userError "FontDict field projection mismatch")
  -- `Document` carries only a `Pdf` handle and the trailer `Dict`.
  let d : Document := { pdf := pdf, dict := sampleDict }
  unless d.dict == sampleDict do
    throw (IO.userError "Document field projection mismatch")

/-! ### `PageTree` -/

-- `PageTree.node`/`PageTree.leaf` wrap a `PageNode`/`Page` respectively.
#eval show IO Unit from do
  let pdf ← mkPdf
  let n : PageNode := { pdf := pdf, ref := sampleRef, dict := sampleDict }
  match PageTree.node n with
  | .node n' => unless n'.ref == sampleRef do throw (IO.userError "PageTree.node mismatch")
  | .leaf _ => throw (IO.userError "expected PageTree.node")
  let p : Page := { pdf := pdf, ref := sampleRef, dict := sampleDict }
  match PageTree.leaf p with
  | .leaf p' => unless p'.ref == sampleRef do throw (IO.userError "PageTree.leaf mismatch")
  | .node _ => throw (IO.userError "expected PageTree.leaf")

/-! ### `Pdf`, the mutable object cache -/

-- A freshly built `Pdf`'s cache starts out disabled and empty; it can be
-- toggled on and populated, and read back exactly as written.
#eval show IO Unit from do
  let pdf ← mkPdf
  let (useCache, contents) ← pdf.cache.get
  unless useCache == false && contents.isEmpty do
    throw (IO.userError "expected a fresh cache to be disabled and empty")
  pdf.cache.set (true, contents.insert sampleRef (Object.number 42))
  let (useCache', contents') ← pdf.cache.get
  unless useCache' == true && contents'.get? sampleRef == some (Object.number 42) do
    throw (IO.userError "expected the cache write to be observable")

end Tests.Data.PDF.Document.Internal.Types
