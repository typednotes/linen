/-
  Data.PDF.Core.Object — PDF object model (PDF32000-1:2008 §7.3)

  Ports `Pdf.Core.Object` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Object.hs`),
  module 5 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  `Object` is the recursive sum type at the heart of the PDF object model:
  numbers, booleans, names, dictionaries, arrays, strings, streams,
  indirect references, and `null`. Dictionaries (`Dict`) map `Name`s to
  `Object`s; arrays (`Array`, upstream's `Vector Object`) hold `Object`s
  positionally. This recursive shape is ported **faithfully** — no
  flattening, no un-decoding into raw bytes to dodge a termination
  argument, per `AGENTS.md`'s explicit warning against that shortcut and
  the dependency doc's "Scope" note.

  ## Design

  - `Array = Vector Object` maps onto Lean's own `Array Object` directly —
    Lean's `Array` already *is* a boxed, immutable, dynamically-sized
    vector (see `Linen/Data/Vector.lean`'s design note), and nested
    (self-)occurrences of `Array Object` inside an inductive `Object` are
    supported natively by Lean's positivity/structural-recursion checker.

  - `Dict = HashMap Name Object` is exposed with exactly that public type
    (`Std.HashMap Name Object`, per `linen`'s `hashable`→`Std.Hashable`
    substitution already used by `Name`). **Internally**, though, `Object`'s
    `dict` case stores an `Array (Name × Object)` rather than nesting
    `Std.HashMap` itself inside the recursive type: Lean's kernel rejects a
    directly-nested `Std.HashMap Name Object` inside a recursive `Object`
    (`Std.HashMap` bundles an internal well-formedness *proof* indexed by
    its value type, and the kernel's nested-inductive unpacking does not
    handle that dependency when the value type is the very inductive being
    defined — a real elaborator limitation, not a proof anyone dodged).
    `Array (Name × Object)` is accepted natively (same mechanism as the
    `array` case above), so it is used as the faithful recursive
    representation, with `Object.dict`/`Object.Util.dictValue` converting
    to/from the public `Dict = Std.HashMap Name Object` at the boundary.
    One observable consequence: the derived `BEq Object` below compares
    `dict` objects by their internal array representation (order-sensitive)
    rather than upstream's order-independent `HashMap` equality — flagged
    here rather than hidden, and irrelevant to every other function in this
    port, none of which compares `Object`s for equality.

  - `Stream = S Dict Int64` (a dictionary plus a byte offset) and `Object`
    are mutually recursive (`Object` has a `stream` case holding a
    `Stream`; `Stream`'s dictionary holds `Object`s) — a genuine, unavoidable
    mutual recursion, so both are declared together in one `mutual` block.
    Upstream's `Int64` offset is ported as `Nat` (a file offset is never
    negative; see `Data.PDF.Core.IO.Buffer`'s doc-comment for the same
    substitution and rationale).

  - `Ref = R Int Int` (upstream's object index and generation number) is a
    plain, non-recursive pair, ported as an ordinary `structure` with a
    hand-written `Hashable` instance mirroring upstream's
    `hashWithSalt salt (R a b) = hashWithSalt salt (a, b)`.
-/
import Linen.Data.PDF.Core.Name
import Linen.Data.Scientific
import Std.Data.HashMap

namespace Data.PDF.Core.Object

export Data.PDF.Core.Name (Name)

/-- An indirect-object reference: an object index and a generation number
    (PDF32000-1:2008 §7.3.10). Mirrors upstream's `Ref = R Int Int`. -/
structure Ref where
  /-- The object index. -/
  index : Int
  /-- The generation number. -/
  generation : Int
deriving BEq, Ord, Repr

/-- Hashes a `Ref` as the pair of its fields, mirroring upstream's
    `hashWithSalt salt (R a b) = hashWithSalt salt (a, b)`. -/
instance : Hashable Ref where
  hash r := hash (r.index, r.generation)

mutual
  /-- Any PDF object (PDF32000-1:2008 §7.3). See the module doc-comment for
      why `dictRaw`'s payload is an association array rather than a nested
      `Std.HashMap`; use `Object.dict`/`Data.PDF.Core.Object.Util.dictValue`
      to convert to/from the public `Dict = Std.HashMap Name Object` type. -/
  inductive Object where
    /-- A number, PDF32000-1:2008 §7.3.3. -/
    | number (n : Data.Scientific)
    /-- A boolean, PDF32000-1:2008 §7.3.2. -/
    | bool (b : Bool)
    /-- A name, PDF32000-1:2008 §7.3.5. -/
    | name (n : Name)
    /-- A dictionary (internal representation; see the module doc-comment). -/
    | dictRaw (entries : Array (Name × Object))
    /-- An array, PDF32000-1:2008 §7.3.6. -/
    | array (items : Array Object)
    /-- A string, PDF32000-1:2008 §7.3.4. -/
    | string (s : Data.ByteString)
    /-- A stream, PDF32000-1:2008 §7.3.8. -/
    | stream (s : Stream)
    /-- An indirect reference, PDF32000-1:2008 §7.3.10. -/
    | ref (r : Ref)
    /-- The `null` object, PDF32000-1:2008 §7.3.9. -/
    | null
  deriving BEq, Repr

  /-- A stream: its dictionary plus the byte offset (from the start of the
      stream's data, immediately after the `stream` keyword and its
      end-of-line) at which the raw stream data begins. Mirrors upstream's
      `Stream = S Dict Int64`. -/
  inductive Stream where
    /-- `mk entries offset` — see `Data.PDF.Core.Object.Util.Stream.dict`
        for the public `Dict`-typed accessor. -/
    | mk (entries : Array (Name × Object)) (offset : Nat)
  deriving BEq, Repr
end

/-- A dictionary: `Name`s to `Object`s. Mirrors upstream's
    `Dict = HashMap Name Object` exactly (see the module doc-comment for why
    `Object`'s own `dictRaw` case can't nest this type directly). -/
abbrev Dict := Std.HashMap Name Object

namespace Object

/-- Build a dictionary `Object` from a `Dict`. -/
def dict (d : Dict) : Object := .dictRaw d.toArray

end Object

namespace Stream

/-- A stream's dictionary, as the public `Dict` type. -/
def dict (s : Stream) : Dict :=
  match s with
  | .mk entries _ => Std.HashMap.ofList entries.toList

/-- A stream's byte offset. -/
def offset (s : Stream) : Nat :=
  match s with
  | .mk _ off => off

/-- Build a `Stream` from a `Dict` and a byte offset. -/
def mk' (d : Dict) (offset : Nat) : Stream := .mk d.toArray offset

end Stream

end Data.PDF.Core.Object
