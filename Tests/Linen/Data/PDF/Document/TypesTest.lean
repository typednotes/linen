/-
  Tests for `Linen.Data.PDF.Document.Types`.

  A one-line pass-through re-export module (see its own doc-comment); these
  tests just confirm that `Data.PDF.Core.Types.Rectangle`/
  `rectangleFromArray`/`rectangleToArray` are indeed reachable through the
  `Data.PDF.Document.Types` namespace, exactly as through
  `Data.PDF.Core.Types` itself.
-/
import Linen.Data.PDF.Document.Types

open Data.PDF.Core.Object

namespace Tests.Data.PDF.Document.Types

-- `Rectangle` is reachable (and usable) through the re-exporting namespace.
#guard
  let r : Data.PDF.Document.Types.Rectangle Float := ⟨0.0, 0.0, 100.0, 200.0⟩
  r.llx == 0.0 && r.lly == 0.0 && r.urx == 100.0 && r.ury == 200.0

-- `rectangleFromArray`, reached through `Data.PDF.Document.Types`, behaves
-- exactly as `Data.PDF.Core.Types.rectangleFromArray`.
#guard
  match Data.PDF.Document.Types.rectangleFromArray
      #[Object.number 0, Object.number 0, Object.number 100, Object.number 200] with
  | .ok r => r.llx == 0.0 && r.lly == 0.0 && r.urx == 100.0 && r.ury == 200.0
  | .error _ => false

-- `rectangleToArray`, reached the same way, round-trips through
-- `rectangleFromArray`.
#guard
  match Data.PDF.Document.Types.rectangleFromArray
      (Data.PDF.Document.Types.rectangleToArray ⟨1.0, 2.0, 3.0, 4.0⟩) with
  | .ok r => r.llx == 1.0 && r.lly == 2.0 && r.urx == 3.0 && r.ury == 4.0
  | .error _ => false

end Tests.Data.PDF.Document.Types
