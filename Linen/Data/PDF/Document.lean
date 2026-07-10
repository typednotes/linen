/-
  Data.PDF.Document — mid-level utilities for processing a PDF file

  Ports `Pdf.Document` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox, `document/lib/Pdf/Document.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document.hs`),
  module 11 (the package's own top-level aggregator, and the last module) of
  the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  ## Design: a thin re-export module

  Exactly as `Data.PDF.Content` re-exports `pdf-toolbox-content`'s own
  top-level aggregator module, this module re-exports upstream's
  `Pdf.Document`, which itself carries no code of its own — only a curated
  re-export of `Pdf.Document.Types`, `Pdf.Document.Pdf`,
  `Pdf.Document.Document`, `Pdf.Document.Catalog`, `Pdf.Document.PageNode`,
  `Pdf.Document.Page`, `Pdf.Document.Info` and `Pdf.Document.FontDict` — so
  a downstream user can `import Pdf.Document` alone rather than every
  submodule individually.

  `Data.PDF.Document` the *namespace* is already the direct parent of each
  of those eight submodules' own namespaces; this module additionally
  declares `namespace Data.PDF.Document` and uses `export` to alias every
  public name each submodule defines, straight into that parent namespace
  (see `Data.PDF.Content`/`Data.PDF.Core`'s doc-comments for the same
  technique and the same non-goal it calls out: this is not a name
  collision with the submodules themselves, since Lean lets a namespace
  hold direct members alongside nested sub-namespaces). Each submodule's
  own public surface is reproduced here exactly:

  - `Pdf.Document.Types`: `Rectangle`, `rectangleFromArray`,
    `rectangleToArray`.
  - `Pdf.Document.Pdf`: `Pdf`, `defaultUserPassword`, `fromFile`,
    `fromHandle`, `fromBytes`, `withPdfFile`, `document`, `lookupObject`,
    `enableCache`, `disableCache`, `streamContent`, `rawStreamContent`,
    `isEncrypted`, `setUserPassword`, `deref`.
  - `Pdf.Document.Document`: `Document`, `documentCatalog`, `documentInfo`,
    `documentEncryption`.
  - `Pdf.Document.Catalog`: `Catalog`, `catalogPageNode`.
  - `Pdf.Document.PageNode`: `PageNode`, `PageTree`, `objectCountBound`,
    `pageNodeNKids`, `pageNodeParent`, `pageNodeKids`, `loadPageNode`,
    `pageNodePageByNum`.
  - `Pdf.Document.Page`: `Page`, `pageParentNode`, `pageContents`,
    `pageFontDicts`, `mediaBoxRecFueled`, `pageMediaBox`, `XObject`,
    `pageXObjects`, `pageExtractGlyphs`, `glyphsToText`, `pageExtractText`.
  - `Pdf.Document.Info`: `Info`, `infoTitle`, `infoAuthor`, `infoSubject`,
    `infoKeywords`, `infoCreator`, `infoProducer`.
  - `Pdf.Document.FontDict`: `FontDict`, `FontSubtype`, `fontDictSubtype`,
    `fontDictLoadInfo`.

  `objectCountBound`/`mediaBoxRecFueled` are additions beyond upstream's own
  export list (which has no equivalent, having no cycle guard at all — see
  `Data.PDF.Document.PageNode`/`Data.PDF.Document.Page`'s doc-comments), but
  are re-exported here too since they are ordinary public definitions of
  their modules, not internal helpers.
-/
import Linen.Data.PDF.Document.Types
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Document.Document
import Linen.Data.PDF.Document.Catalog
import Linen.Data.PDF.Document.PageNode
import Linen.Data.PDF.Document.Page
import Linen.Data.PDF.Document.Info
import Linen.Data.PDF.Document.FontDict

namespace Data.PDF.Document

export Data.PDF.Document.Types (Rectangle rectangleFromArray rectangleToArray)
export Data.PDF.Document.Pdf
  (Pdf defaultUserPassword fromFile fromHandle fromBytes withPdfFile document lookupObject
   enableCache disableCache streamContent rawStreamContent isEncrypted setUserPassword deref)
export Data.PDF.Document.Document (Document documentCatalog documentInfo documentEncryption)
export Data.PDF.Document.Catalog (Catalog catalogPageNode)
export Data.PDF.Document.PageNode
  (PageNode PageTree objectCountBound pageNodeNKids pageNodeParent pageNodeKids loadPageNode
   pageNodePageByNum)
export Data.PDF.Document.Page
  (Page pageParentNode pageContents pageFontDicts mediaBoxRecFueled pageMediaBox XObject
   pageXObjects pageExtractGlyphs glyphsToText pageExtractText)
export Data.PDF.Document.Info
  (Info infoTitle infoAuthor infoSubject infoKeywords infoCreator infoProducer)
export Data.PDF.Document.FontDict (FontDict FontSubtype fontDictSubtype fontDictLoadInfo)

end Data.PDF.Document
