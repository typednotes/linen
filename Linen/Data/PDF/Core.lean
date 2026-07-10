/-
  Data.PDF.Core — low-level API for parsing PDF files

  Ports `Pdf.Core` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core.hs`, fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/core/lib/Pdf/Core.hs`),
  module 17 (the package's own top-level aggregator) of the
  `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  ## Design: a thin re-export module

  Upstream's `Pdf.Core` carries no code of its own — it is purely a curated
  public-API export list re-exporting names from `Pdf.Core.Object`,
  `Pdf.Core.File` and `Pdf.Core.Encryption` (its own import list), so that a
  downstream user can `import Pdf.Core` alone rather than every submodule
  individually.

  Lean's namespace model already nests hierarchically — `Data.PDF.Core`
  the *namespace* is simultaneously the direct parent of
  `Data.PDF.Core.Object`, `.File`, `.Encryption`, etc., and a namespace that
  can itself hold direct members. So this module declares `namespace
  Data.PDF.Core` and uses `export` to alias every name upstream's own export
  list names, verbatim, straight into that parent namespace. This is a
  genuine, faithful analogue of upstream's re-export module (not a
  do-nothing marker, and not a name collision with the `Data.PDF.Core.*`
  submodules' own namespaces — Lean allows a namespace to have both nested
  sub-namespaces and its own direct members at once): opening
  `Data.PDF.Core` alone now gives exactly upstream's `import Pdf.Core`
  surface, with every submodule still independently reachable by its own
  full name for anyone who prefers to import narrowly.

  One deviation from upstream's export list: upstream also re-exports
  `Array`, its own `type Array = Vector Object` synonym declared directly in
  `Pdf.Core.Object`. This port has no equivalent named alias to re-export —
  `Data.PDF.Core.Object`'s `array` case is typed directly as Lean's own
  `Array Object` (see that module's doc-comment), with no separate
  PDF-specific `Array` type synonym ever declared — so there is nothing of
  that name to `export` here; Lean's built-in `Array` already serves that
  role directly, unaliased. -/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.File
import Linen.Data.PDF.Core.Encryption

namespace Data.PDF.Core

export Data.PDF.Core.Object (Object Name Dict Ref Stream)
export Data.PDF.Core.File
  (File withPdfFile fromHandle fromBytes fromBuffer lastTrailer findObject streamContent
   rawStreamContent EncryptionStatus encryptionStatus setUserPassword setDecryptor)
export Data.PDF.Core.Encryption (defaultUserPassword)

end Data.PDF.Core
