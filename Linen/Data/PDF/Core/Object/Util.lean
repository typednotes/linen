/-
  Data.PDF.Core.Object.Util — safe accessors into `Object`

  Ports `Pdf.Core.Object.Util` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Object/Util.hs`),
  module 6 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  Each `xValue` function tries to view an `Object` as one particular case,
  returning `none` if it is some other case — a total, `Maybe`/`Option`-typed
  alternative to pattern-matching on `Object` at every call site.

  ## Design

  Upstream distinguishes `intValue :: Object -> Maybe Int` (only succeeds for
  *already-integral* numbers, i.e. `Scientific.floatingOrInteger` returning
  `Right`) from `int64Value :: Object -> Maybe Int64` (upstream's comment:
  "for cases, where according to the specs values above 2^29 (Int) have to be
  expected", using `Scientific.toBoundedInteger`, which also only succeeds on
  integral values but at `Int64`'s wider range rather than native `Int`'s
  usually-30-bit-tagged range on 32-bit GHC builds).

  Lean has no fixed-width machine-`Int`-vs-`Int64` distinction (Lean's `Int`
  is already an arbitrary-precision integer type, and `Data.Scientific`'s own
  `toBoundedInteger` — see `Linen/Data/Scientific.lean` — is already defined
  at 64-bit range), so both `intValue` and `int64Value` collapse to the same
  underlying `Data.Scientific.toBoundedInteger` call here: there is no
  narrower-vs-wider distinction left to draw. Both are still ported, under
  their upstream names, since call sites may reasonably use either. -/
import Linen.Data.PDF.Core.Object

namespace Data.PDF.Core.Object.Util

open Data.PDF.Core.Object

/-- Try to view an object as an `Int` (only succeeds for an already-integral
    `number`; a fractional `number` returns `none` rather than truncating).
    See the module doc-comment for why this now coincides with `int64Value`. -/
def intValue (o : Object) : Option Int :=
  match o with
  | .number n => n.toBoundedInteger
  | _ => none

/-- Try to view an object as a (64-bit-range) integer. Upstream's comment:
    "for cases, where according to the specs values above 2^29 (Int) have to
    be expected." See the module doc-comment for why this now coincides with
    `intValue`. -/
def int64Value (o : Object) : Option Int :=
  match o with
  | .number n => n.toBoundedInteger
  | _ => none

/-- Try to view an object as a `Bool`. -/
def boolValue (o : Object) : Option Bool :=
  match o with
  | .bool b => some b
  | _ => none

/-- Try to view an object as a `Float`. An integral `number` is automatically
    converted (unlike `intValue`/`int64Value`, which reject fractional
    numbers but never the reverse). -/
def realValue (o : Object) : Option Float :=
  match o with
  | .number n => some n.toRealFloat
  | _ => none

/-- Try to view an object as a `Name`. -/
def nameValue (o : Object) : Option Name :=
  match o with
  | .name n => some n
  | _ => none

/-- Try to view an object as a string (`Data.ByteString`). -/
def stringValue (o : Object) : Option Data.ByteString :=
  match o with
  | .string s => some s
  | _ => none

/-- Try to view an object as an array. -/
def arrayValue (o : Object) : Option (Array Object) :=
  match o with
  | .array items => some items
  | _ => none

/-- Try to view an object as a stream. -/
def streamValue (o : Object) : Option Stream :=
  match o with
  | .stream s => some s
  | _ => none

/-- Try to view an object as an indirect reference. -/
def refValue (o : Object) : Option Ref :=
  match o with
  | .ref r => some r
  | _ => none

/-- Try to view an object as a dictionary, reconstructing the public
    `Dict = Std.HashMap Name Object` type from `Object`'s internal
    association-array representation (see `Data.PDF.Core.Object`'s module
    doc-comment for why that internal representation isn't itself a
    `Std.HashMap`). -/
def dictValue (o : Object) : Option Dict :=
  match o with
  | .dictRaw entries => some (Std.HashMap.ofList entries.toList)
  | _ => none

end Data.PDF.Core.Object.Util
