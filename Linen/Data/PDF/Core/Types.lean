/-
  Data.PDF.Core.Types — compound data structures (PDF32000-1:2008 §7.9)

  Ports `Pdf.Core.Types` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Types.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/core/lib/Pdf/Core/Types.hs`),
  module 18 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  ## Design

  - Upstream's `Rectangle a = Rectangle a a a a` is a plain positional
    4-tuple-shaped record, always instantiated at `a = Double` by the rest
    of the package. It is ported as a `structure`, still generic in its
    element type (mirroring upstream's genuine polymorphism), but with
    named fields (`llx`/`lly`/`urx`/`ury`, PDF32000-1:2008 §7.9.5's own
    names for a rectangle's lower-left/upper-right corner coordinates)
    rather than upstream's four bare positional fields — more Lean-idiomatic
    (named projections instead of pattern-matching on a 4-ary constructor
    everywhere) without changing the type's shape or behaviour.

  - `rectangleFromArray`/`rectangleToArray` are ported directly against
    `Data.PDF.Core.Object.Util.realValue` and `Data.Scientific.fromFloatDigits`
    (upstream's `Scientific.realValue`/`Scientific.fromFloatDigits`), at
    `Float` in place of upstream's `Double`, matching every other numeric
    conversion already established in this port
    (`Data.PDF.Core.Object.Util`, `Data.PDF.Core.Object.Builder`).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.Scientific

namespace Data.PDF.Core.Types

open Data.PDF.Core.Object

/-! ── The `Rectangle` type ── -/

/-- A rectangle (PDF32000-1:2008 §7.9.5): the coordinates of its lower-left
    and upper-right corners. Mirrors upstream's `Rectangle a = Rectangle a a
    a a`, with named fields instead of four bare positional ones. -/
structure Rectangle (α : Type u) where
  /-- The lower-left corner's x coordinate. -/
  llx : α
  /-- The lower-left corner's y coordinate. -/
  lly : α
  /-- The upper-right corner's x coordinate. -/
  urx : α
  /-- The upper-right corner's y coordinate. -/
  ury : α
deriving BEq, Repr

/-! ── Conversion to/from a PDF array ── -/

/-- Build a rectangle from an array of 4 numbers. Fails if the array doesn't
    contain exactly 4 real (i.e. `number`) values. Mirrors upstream's
    `rectangleFromArray`. -/
def rectangleFromArray (arr : Array Object) : Except String (Rectangle Float) :=
  match arr.toList.mapM Data.PDF.Core.Object.Util.realValue with
  | none => .error "Rectangle should contain real values"
  | some vals =>
    match vals with
    | [a, b, c, d] => .ok ⟨a, b, c, d⟩
    | _ => .error s!"rectangleFromArray: {reprStr arr}"

/-- Convert a rectangle back into a PDF array of 4 numbers. Mirrors
    upstream's `rectangleToArray`. -/
def rectangleToArray (r : Rectangle Float) : Array Object :=
  #[r.llx, r.lly, r.urx, r.ury].map (fun x => Object.number (Data.Scientific.fromFloatDigits x))

end Data.PDF.Core.Types
