/-
  Tests for `Linen.Database.DuckDB.Simple.Generic`.

  As with `FromFieldTest`, these tests build `Field`s directly from
  hand-constructed `FieldValue`s (no live DuckDB connection is needed for a
  decode-direction module) — here `.union`/`.struct` values, decoded through
  `Database.DuckDB.Simple.Generic.Example.Shape`'s hand-written `FromField`
  instance, to exercise `withUnion`/`unionFieldNamed`/`structField`/
  `firstMatch` against a real nested value.
-/
import Linen.Database.DuckDB.Simple.Generic

open Database.DuckDB.Simple
open Database.DuckDB.Simple.Generic
open Database.DuckDB.Simple.Generic.Example
open Database.DuckDB.Simple.LogicalRep (StructValue StructField UnionValue UnionMemberType)

namespace Tests.Database.DuckDB.Simple.Generic

private def field (v : FieldValue) : Field := { result := v, column := 0, columnLabel := none }

private def circleMembers : Array UnionMemberType :=
  #[ { name := "circle", type := .scalar .double }
   , { name := "rectangle", type := .scalar .double } ]

private def circleUnion (radius : Float) : FieldValue :=
  .union
    { index := 0
      label := "circle"
      payload := .struct { fields := #[{ name := "radius", value := .double radius }] }
      members := circleMembers }

private def rectangleUnion (width height : Float) : FieldValue :=
  .union
    { index := 1
      label := "rectangle"
      payload :=
        .struct
          { fields :=
              #[ { name := "width", value := .double width }
               , { name := "height", value := .double height } ] }
      members := circleMembers }

-- A `circle` member decodes via `structField`'s single-field lookup.
#guard fromField (field (circleUnion 2.5)) == Ok.ok (Shape.circle 2.5)

-- A `rectangle` member decodes via two `structField` lookups composed with
-- `Ok`'s `Applicative`/`Monad`.
#guard fromField (field (rectangleUnion 3.0 4.0)) == Ok.ok (Shape.rectangle 3.0 4.0)

-- An unrecognized union label is a genuine decode failure, not a crash.
#guard
  match (fromField
      (field (.union
        { index := 2, label := "triangle"
          payload := .struct { fields := #[] }
          members := circleMembers })) : Ok Shape) with
  | .errors #[_] => true
  | _ => false

-- A non-`STRUCT` payload under a matching label is also a genuine failure.
#guard
  match (fromField
      (field (.union
        { index := 0, label := "circle", payload := .double 1.0
          members := circleMembers })) : Ok Shape) with
  | .errors #[_] => true
  | _ => false

-- `withStruct`/`structField` directly, independent of `Shape`, decoding a
-- plain struct value's single field.
#guard
  structField (α := Float)
    (StructValue.mk #[{ name := "radius", value := .double 9.0 }]) 0 none "radius" ==
    Ok.ok 9.0

#guard
  match (structField (α := Float)
    (StructValue.mk #[{ name := "radius", value := .double 9.0 }]) 0 none "missing") with
  | .errors #[_] => true
  | _ => false

end Tests.Database.DuckDB.Simple.Generic
