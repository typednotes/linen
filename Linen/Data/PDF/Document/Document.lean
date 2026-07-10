/-
  Data.PDF.Document.Document — the PDF document

  Ports `Pdf.Document.Document` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox,
  `document/lib/Pdf/Document/Document.hs`, fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/Document.hs`),
  module 5 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  Three accessors into the trailer dictionary
  (`Data.PDF.Document.Internal.Types.Document.dict`): `documentCatalog`
  resolves the mandatory `/Root` entry, `documentInfo` resolves the
  optional `/Info` entry, and `documentEncryption` resolves the optional
  `/Encrypt` entry, each following exactly one level of indirection through
  `lookupObject`/`deref`.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Document.Internal.Types
import Linen.Data.PDF.Document.Pdf

namespace Data.PDF.Document.Document

open Data.PDF.Core.Object (Name Dict)
open Data.PDF.Core.Object.Util (refValue dictValue)
open Data.PDF.Core.Exception (sure corrupted)
open Data.PDF.Core.Util (notice)
open Data.PDF.Document.Internal.Types (Document Catalog Info)

export Data.PDF.Document.Internal.Types (Document)

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-- Get the document catalog, following the trailer's mandatory `/Root`
    indirect reference. Mirrors upstream's `documentCatalog`. -/
def documentCatalog (doc : Document) : IO Catalog := do
  let ref ← sure
    (notice (doc.dict.get? (mkName "Root") >>= refValue)
      "trailer: Root should be an indirect reference")
  let obj ← Data.PDF.Document.Pdf.lookupObject doc.pdf ref
  let d ← sure (notice (dictValue obj) "catalog should be a dictionary")
  pure { pdf := doc.pdf, ref := ref, dict := d }

/-- Get the document's information dictionary, if present, following the
    trailer's optional `/Info` indirect reference. Mirrors upstream's
    `documentInfo`. -/
def documentInfo (doc : Document) : IO (Option Info) := do
  match doc.dict.get? (mkName "Info") with
  | none => pure none
  | some (.ref ref) => do
    let obj ← Data.PDF.Document.Pdf.lookupObject doc.pdf ref
    let d ← sure (notice (dictValue obj) "info should be a dictionary")
    pure (some { pdf := doc.pdf, ref := ref, dict := d })
  | some _ => throw (corrupted "document Info should be an indirect reference")

/-- Get the document's encryption dictionary, if present, resolving the
    trailer's `/Encrypt` entry (which may be a direct dictionary or an
    indirect reference). Mirrors upstream's `documentEncryption`. -/
def documentEncryption (doc : Document) : IO (Option Dict) := do
  match doc.dict.get? (mkName "Encrypt") with
  | none => pure none
  | some o => do
    let o' ← Data.PDF.Document.Pdf.deref doc.pdf o
    match dictValue o' with
    | some d => pure (some d)
    | none =>
      match o' with
      | .null => pure none
      | _ => throw (corrupted "document Encrypt should be a dictionary")

end Data.PDF.Document.Document
