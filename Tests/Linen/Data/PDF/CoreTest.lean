/-
  Tests for `Linen.Data.PDF.Core` (module 17, the package aggregator).

  This module carries no logic of its own — it is a thin `export`-based
  re-export of names already defined (and already tested) in
  `Data.PDF.Core.Object`, `.File` and `.Encryption` (see that module's
  doc-comment). So there is nothing new to *test* here beyond confirming
  the re-export actually makes those names reachable, unqualified, under
  the plain `Data.PDF.Core` namespace — i.e. that `import
  Linen.Data.PDF.Core` alone (mirroring upstream's `import Pdf.Core`)
  gives the intended surface, with no need to `open`/import every
  submodule individually.
-/
import Linen.Data.PDF.Core

open Data.PDF.Core

namespace Tests.Data.PDF.Core

-- `defaultUserPassword`, re-exported from `.Encryption`, is reachable
-- directly under `Data.PDF.Core`.
#guard defaultUserPassword.size == 32

-- `File`/`withPdfFile`/`fromBytes`/`findObject`, re-exported from
-- `.File`, and `Object`/`Ref`, re-exported from `.Object`, are all
-- reachable directly under `Data.PDF.Core` — this line only needs to
-- elaborate for the re-export to be confirmed.
#eval show IO Unit from do
  let obj1 := "1 0 obj\n7\nendobj\n"
  let xrefOff := (String.toUTF8 obj1).size
  let doc := obj1 ++ "xref\n0 2\n0000000000 65535 f \n0000000000 00000 n \n" ++
    "trailer\n<< /Size 2 >>\nstartxref\n" ++ toString xrefOff ++ "\n%%EOF"
  let file ← fromBytes [] (String.toUTF8 doc)
  let obj ← findObject file (⟨1, 0⟩ : Ref)
  match obj with
  | .number n => unless n.toBoundedInteger == some 7 do
      throw (IO.userError s!"unexpected number: {reprStr n}")
  | other => throw (IO.userError s!"expected a number, got: {reprStr other}")

end Tests.Data.PDF.Core
