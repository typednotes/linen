/-
  Linen.Database.DuckDB.Simple.Generic — hand-written `STRUCT`/`UNION`
  decode combinators

  Module #16 of `docs/imports/duckdb-simple/dependencies.md`, on #5
  (`Linen.Database.DuckDB.Simple.FromField`, for `FieldValue`/`Field`), #2
  (`…LogicalRep`, for `StructValue`/`UnionValue`), #3 (`…Ok`), and #7
  (`…ToField`).

  ## Design

  ### Why not a real generics port

  Upstream's `Database.DuckDB.Simple.Generic` rides GHC's `Generic` class:
  given a derived `Rep a` type built from `:+:`/`:*:`/`M1`/`K1`/`U1`, it
  walks that representation to synthesize `FromField`/`ToField` for whole
  record and sum types automatically (`GStruct`/`GStructDecode` for
  products, `GSum`/`IsSum` for sums), on top of a small `DuckValue` leaf
  class. Lean 4 has no counterpart to GHC's `Generic`/`Rep` machinery: there
  is no way to obtain, from an arbitrary `structure`/`inductive` declaration,
  a term-level description of its fields/constructors that a typeclass
  method could recurse over generically (Lean's own `deriving` handlers are
  compiler-internal elaboration code, not something a library can hook into
  to synthesize *new* typeclass instances for *arbitrary* user types the way
  `GHC.Generics` does). Faking this — e.g. a `Generic` class whose method
  just calls `FromField`/`ToField` right back, or a macro that only "derives"
  for one hard-coded shape — would satisfy nothing upstream actually does,
  so per this batch's task brief this module does not attempt it.

  ### What is ported instead

  `docs/imports/duckdb-simple/dependencies.md`'s own note on this module
  anticipates exactly this outcome: "this reduces to hand-written per-field
  instances." What upstream's generic machinery *produces*, this module
  provides as **combinators a hand-written `FromField` instance calls
  directly** — the genuinely reusable part of the port, without the
  generic-traversal engine:

  - `structField`: decode one named member of an already-decoded
    `StructValue FieldValue` via its `FromField` instance, replicating what
    `GStructDecode`'s generated code does per-record-field (a
    `StructValue.field?` lookup by name, keeping the enclosing `Field`'s
    positional metadata for error messages) but spelled out at each call
    site instead of derived.
  - `unionField`: decode a `UnionValue FieldValue` if (and only if) its
    active label matches a given constructor tag, otherwise `none` —
    replicating `GSum`'s per-constructor dispatch arm.
  - `unionFieldNamed`: as `unionField`, but the active member additionally
    carries its own named `STRUCT` payload (DuckDB's `UNION` members are
    themselves single-field slots, but this project's own worked example
    below models each variant as carrying a full record, matching how a
    real ADT-with-payload sum type is normally used).

  A hand-written `FromField` instance for a record type is then just a
  sequence of `structField`/`unionField` calls composed with `Ok`'s
  `Applicative`, exactly as it would be without any generics support at
  all — these combinators only remove the repetitive `StructValue.field?`
  lookup-and-rewrap boilerplate, they do not (and, per the above, cannot)
  eliminate writing one instance per user type.

  ### `ToField` has no encode-direction counterpart here

  `Linen.Database.DuckDB.Simple.ToField`'s own module doc already records
  that `STRUCT`/`UNION`/`LIST`/`MAP`/`ENUM` values have **no** `ToField`
  instance in this port at all, `duckdb.h` exposing no `duckdb_bind_struct`/
  `duckdb_bind_union`-style entry point — only the boxed `duckdb_bind_value`
  (`Database.DuckDB.FFI.ValueInterface`, out of scope for the whole
  `duckdb-ffi` import per `docs/imports/duckdb-ffi/dependencies.md`). Since
  there is no way to *bind* a `STRUCT`/`UNION` parameter at all, there is
  nothing for a generic encode-direction helper to call into; this module is
  therefore decode-only, matching the leaf-level scope narrowing already
  established one module over.

  ## Worked example

  `Shape`, below, is a two-constructor sum type (`circle`/`rectangle`) whose
  `FromField` instance is hand-written using `unionField`/`structField` —
  the concrete illustration that these combinators decode a real, nested
  `StructValue`/`UnionValue FieldValue` rather than merely type-checking
  against one.

  ## Haskell source
  - `Database.DuckDB.Simple.Generic` (`duckdb-simple` package, version
    0.1.5.1)
-/

import Linen.Database.DuckDB.Simple.FromField
import Linen.Database.DuckDB.Simple.LogicalRep

namespace Database.DuckDB.Simple.Generic

open Database.DuckDB.Simple
open Database.DuckDB.Simple.LogicalRep (StructValue UnionValue StructField)

-- ────────────────────────────────────────────────────────────────────
-- Struct-field decoding
-- ────────────────────────────────────────────────────────────────────

/-- Decode the named member `name` of an already-decoded `StructValue
    FieldValue` via `α`'s `FromField` instance. `column`/`columnLabel`
    are the *enclosing* struct field's own positional metadata, reused so
    a conversion failure inside a nested member still names the outer
    column (matching `Field.withValue`'s same convention one module
    over). Fails with a `.errors` `Ok` if `name` is not present. -/
def structField [FromField α] (s : StructValue FieldValue) (column : Nat)
    (columnLabel : Option String) (name : String) : Ok α :=
  match s.field? name with
  | none => Ok.fail s!"missing struct field {name}"
  | some sf => fromField { result := sf.value, column, columnLabel }

/-- Decode a `StructValue FieldValue` field of a `Field` directly (the
    usual entry point for a hand-written `FromField (Struct α)` instance):
    require the field to actually be a `.struct`, then run `k` against its
    decoded members. -/
def withStruct (f : Field) (k : StructValue FieldValue → Ok α) : Ok α :=
  match f.result with
  | .struct s => k s
  | _ => returnError .conversionFailed f "Struct" "expecting a STRUCT column"

-- ────────────────────────────────────────────────────────────────────
-- Union-member decoding
-- ────────────────────────────────────────────────────────────────────

/-- If `u`'s active member is named `name`, decode its payload via `α`'s
    `FromField` instance and wrap the result in `some`; otherwise `none`
    (letting the caller try the next constructor arm). `column`/
    `columnLabel` are reused from the enclosing `Field`, as in
    `structField`. -/
def unionField [FromField α] (u : UnionValue FieldValue) (column : Nat)
    (columnLabel : Option String) (name : String) : Option (Ok α) :=
  if u.label == name then
    some (fromField { result := u.payload, column, columnLabel })
  else
    none

/-- As `unionField`, but the active member's payload is itself a `STRUCT`
    (the shape a sum-type constructor with several named fields actually
    needs — see the module doc's `Shape` example): decode `name`'s payload
    as a `StructValue FieldValue` and run `k` against it. `k` is expected to
    call `structField` itself against whatever `column`/`columnLabel` its
    own enclosing `Field` carries (see the `Shape` instance below), so
    unlike `unionField` this combinator does not need them. -/
def unionFieldNamed (u : UnionValue FieldValue) (name : String)
    (k : StructValue FieldValue → Ok α) : Option (Ok α) :=
  if u.label == name then
    some <|
      match u.payload with
      | .struct s => k s
      | _ =>
        Ok.fail
          s!"union member {name} expected a STRUCT payload, got {u.payload.typeName}"
  else
    none

/-- Decode a `UnionValue FieldValue` field of a `Field` directly, the
    union-typed counterpart to `withStruct`. -/
def withUnion (f : Field) (k : UnionValue FieldValue → Ok α) : Ok α :=
  match f.result with
  | .union u => k u
  | _ => returnError .conversionFailed f "Union" "expecting a UNION column"

/-- Try each `Option (Ok α)` decode attempt in order (typically a sequence
    of `unionField`/`unionFieldNamed` calls, one per constructor), failing
    with `noMatch` if none of `u`'s label matched any of them — this should
    not happen for a well-formed `UnionValue` decoded from a real DuckDB
    `UNION` column (whose `label` always names one of its own declared
    members), but a hand-written instance must still account for it. -/
def firstMatch (attempts : List (Option (Ok α))) (noMatch : String) : Ok α :=
  match attempts.filterMap id with
  | a :: _ => a
  | [] => Ok.fail noMatch

end Database.DuckDB.Simple.Generic

-- ────────────────────────────────────────────────────────────────────
-- Worked example: a hand-written sum-type `FromField` instance
-- ────────────────────────────────────────────────────────────────────

namespace Database.DuckDB.Simple.Generic.Example

open Database.DuckDB.Simple.Generic

/-- A two-constructor sum type, standing in for the kind of ADT upstream's
    `Generic` machinery would derive `FromField`/`ToField` for
    automatically. Here its `FromField` instance (below) is written by
    hand, using this module's `withUnion`/`unionFieldNamed`/`structField`
    combinators — see the module doc for why no generic derivation is
    possible. -/
inductive Shape where
  | circle (radius : Float)
  | rectangle (width height : Float)
deriving Repr, Inhabited, BEq

/-- Hand-written using this module's combinators: `Shape` decodes from a
    `UNION` column with two members, `"circle"` (a one-field `STRUCT`
    payload) and `"rectangle"` (a two-field `STRUCT` payload) — exactly the
    shape a real `duckdb-simple` consumer would write without generic
    derivation available. -/
instance : FromField Shape where
  fromField f :=
    withUnion f fun u =>
      firstMatch
        [ unionFieldNamed u "circle" fun s => do
            let radius ← structField s f.column f.columnLabel "radius"
            pure (Shape.circle radius),
          unionFieldNamed u "rectangle" fun s => do
            let width ← structField s f.column f.columnLabel "width"
            let height ← structField s f.column f.columnLabel "height"
            pure (Shape.rectangle width height) ]
        s!"unrecognized Shape union member {u.label}"

end Database.DuckDB.Simple.Generic.Example
