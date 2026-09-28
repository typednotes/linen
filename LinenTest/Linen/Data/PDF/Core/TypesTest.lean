/-
  Tests for `Linen.Data.PDF.Core.Types`.
-/
import Linen.Data.PDF.Core.Types

open Data.PDF.Core.Object Data.PDF.Core.Types

namespace Tests.Data.PDF.Core.Types

/-! ### `rectangleFromArray` -/

-- A well-formed array of 4 numbers parses to the expected rectangle.
#guard
  match rectangleFromArray #[Object.number 0, Object.number 0, Object.number 100, Object.number 200] with
  | .ok r => r.llx == 0.0 && r.lly == 0.0 && r.urx == 100.0 && r.ury == 200.0
  | .error _ => false

-- Non-numeric entries are rejected.
#guard
  match rectangleFromArray #[Object.number 0, Object.bool true, Object.number 100, Object.number 200] with
  | .ok _ => false
  | .error _ => true

-- The wrong number of entries is rejected.
#guard
  match rectangleFromArray #[Object.number 0, Object.number 0, Object.number 100] with
  | .ok _ => false
  | .error _ => true

/-! ### `rectangleToArray` -/

-- `rectangleToArray` renders each field as a `number` `Object`, in order.
#guard
  match rectangleToArray ⟨0.0, 0.0, 100.0, 200.0⟩ with
  | #[.number a, .number b, .number c, .number d] =>
    a.toBoundedInteger == some 0 && b.toBoundedInteger == some 0 &&
    c.toBoundedInteger == some 100 && d.toBoundedInteger == some 200
  | _ => false

/-! ### Round-tripping -/

-- `rectangleFromArray ∘ rectangleToArray` recovers the original rectangle.
#guard
  match rectangleFromArray (rectangleToArray ⟨1.0, 2.0, 3.0, 4.0⟩) with
  | .ok r => r.llx == 1.0 && r.lly == 2.0 && r.urx == 3.0 && r.ury == 4.0
  | .error _ => false

end Tests.Data.PDF.Core.Types
