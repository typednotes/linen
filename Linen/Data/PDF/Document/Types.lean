/-
  Data.PDF.Document.Types — various types

  Ports `Pdf.Document.Types` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox, `document/lib/Pdf/Document/Types.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/Types.hs`),
  module 1 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  ## Design: a thin re-export module

  Upstream's entire module body is

  ```haskell
  module Pdf.Document.Types
    ( module Pdf.Core.Types
    )
  where

  import Pdf.Core.Types
  ```

  — a one-line pass-through re-export of everything `Pdf.Core.Types`
  exports, carrying no code of its own. `Pdf.Core.Types` is already ported
  as `Data.PDF.Core.Types` (`Rectangle`, `rectangleFromArray`,
  `rectangleToArray`); this module mirrors upstream's re-export, not a
  duplicate definition, the same way `Data.PDF.Core` re-exports its own
  submodules via `export` (see that module's doc-comment for why `export`
  is the faithful analogue of a Haskell re-export list in Lean's namespace
  model).
-/
import Linen.Data.PDF.Core.Types

namespace Data.PDF.Document.Types

export Data.PDF.Core.Types (Rectangle rectangleFromArray rectangleToArray)

end Data.PDF.Document.Types
