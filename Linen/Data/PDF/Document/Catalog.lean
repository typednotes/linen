/-
  Data.PDF.Document.Catalog — the document catalog

  Ports `Pdf.Document.Catalog` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox,
  `document/lib/Pdf/Document/Catalog.hs`, fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/Catalog.hs`),
  module 7 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  A single accessor: `catalogPageNode` follows the catalog's mandatory
  `/Pages` entry (PDF32000-1:2008 §7.7.2, Table 28) to the root node of the
  page tree, checking that it resolves to a dictionary tagged `/Type
  /Pages`. No untrusted-graph recursion here — that begins one level down,
  in `Data.PDF.Document.PageNode` (see that module's doc-comment for the
  visited-`Ref`-set termination argument this batch introduces).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Document.Internal.Types
import Linen.Data.PDF.Document.Internal.Util
import Linen.Data.PDF.Document.Pdf

namespace Data.PDF.Document.Catalog

open Data.PDF.Core.Object (Name)
open Data.PDF.Core.Object.Util (refValue dictValue)
open Data.PDF.Core.Exception (sure)
open Data.PDF.Core.Util (notice)
open Data.PDF.Document.Internal.Types (Catalog PageNode)
open Data.PDF.Document.Internal.Util (ensureType)

export Data.PDF.Document.Internal.Types (Catalog)

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-- Get the root node of the page tree, following the catalog's mandatory
    `/Pages` indirect reference. Mirrors upstream's `catalogPageNode`. -/
def catalogPageNode (cat : Catalog) : IO PageNode := do
  let ref ← sure
    (notice (cat.dict.get? (mkName "Pages") >>= refValue)
      "Pages should be an indirect reference")
  let obj ← Data.PDF.Document.Pdf.lookupObject cat.pdf ref
  let obj' ← Data.PDF.Document.Pdf.deref cat.pdf obj
  let d ← sure (notice (dictValue obj') "Pages should be a dictionary")
  ensureType (mkName "Pages") d
  pure { pdf := cat.pdf, ref := ref, dict := d }

end Data.PDF.Document.Catalog
