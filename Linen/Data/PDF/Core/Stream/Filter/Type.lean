/-
  Data.PDF.Core.Stream.Filter.Type — the `StreamFilter` type

  Ports `Pdf.Core.Stream.Filter.Type` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox,
  `core/lib/Pdf/Core/Stream/Filter/Type.hs`), module 11 of the
  `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  A `StreamFilter` names a PDF stream filter (e.g. `/FlateDecode`) and
  supplies its decoder: given the filter's `/DecodeParms` dictionary (if
  any) and the still-encoded stream content, produce the decoded content.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Stream

namespace Data.PDF.Core.Stream.Filter.Type

open Data.PDF.Core.Object

/-- A PDF stream filter: its name (as it appears in `/Filter`) and its
    decoder. Mirrors upstream's `StreamFilter { filterName, filterDecode }`
    record exactly. -/
structure StreamFilter where
  /-- The filter's name, as it appears as a `/Filter` dictionary value
      (e.g. `FlateDecode`). -/
  filterName : Data.PDF.Core.Name.Name
  /-- Decode params (the corresponding `/DecodeParms` entry, if any) →
      still-encoded content → decoded content. -/
  filterDecode : Option Dict → Data.PDF.Stream.InputStream → IO Data.PDF.Stream.InputStream

end Data.PDF.Core.Stream.Filter.Type
