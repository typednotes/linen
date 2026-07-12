/-
  Linen.Database.DuckDB.FFI.LogicalTypes — building/inspecting
  `duckdb_logical_type` values

  Mirrors Haskell's `Database.DuckDB.FFI.LogicalTypes` (the `duckdb-ffi`
  package). One of the batch of modules from
  `docs/imports/duckdb-ffi/dependencies.md` depending only on
  `Database.DuckDB.FFI.Types` (module #1).

  Covers constructing primitive/`LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION`/
  `ENUM`/`DECIMAL` logical types and inspecting them (alias, `duckdb_type`
  id, decimal width/scale, enum dictionary, struct/union member
  names/types, list/array/map child types). `duckdb_register_logical_type`
  is excluded: it needs a `duckdb_create_type_info` handle
  (`DuckDBCreateTypeInfo` upstream), which nothing in this batch's scope
  constructs — following the same documented scope-narrowing already used
  elsewhere in this import (e.g. `Configuration`'s "always pass a NULL
  `duckdb_config`").
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.LogicalTypes

open Database.DuckDB.FFI.Types

/-! ── Construction ── -/

/-- Create a logical type from a raw `duckdb_type` code. -/
@[extern "linen_duckdb_create_logical_type"]
opaque createRaw (ty : UInt32) : IO LogicalType

/-- Create a logical type from a primitive `Type_`. Returns an invalid
    logical type if `ty` is `.invalid`, `.decimal`, `.enum`, `.list`,
    `.struct`, `.map`, `.array`, or `.union` (use the dedicated constructors
    below for those). The result must eventually be destroyed with
    `Types.LogicalType`'s owner discipline (its GC finalizer calls
    `duckdb_destroy_logical_type`). -/
def create (ty : Type_) : IO LogicalType :=
  createRaw ty.toUInt32

/-- The alias of `ty`, if one was set via `setAlias`. -/
@[extern "linen_duckdb_logical_type_get_alias"]
opaque getAlias (ty : @& LogicalType) : IO (Option String)

/-- Set the alias of `ty`. -/
@[extern "linen_duckdb_logical_type_set_alias"]
opaque setAlias (ty : @& LogicalType) (alias : @& String) : IO Unit

/-- Create a `LIST` type from its child type. -/
@[extern "linen_duckdb_create_list_type"]
opaque createListType (child : @& LogicalType) : IO LogicalType

/-- Create an `ARRAY` type from its child type and fixed element count. -/
@[extern "linen_duckdb_create_array_type"]
opaque createArrayType (child : @& LogicalType) (arraySize : Idx) : IO LogicalType

/-- Create a `MAP` type from its key and value types. -/
@[extern "linen_duckdb_create_map_type"]
opaque createMapType (keyType : @& LogicalType) (valueType : @& LogicalType) : IO LogicalType

/-- Create a `UNION` type from parallel arrays of member types and names
    (must have the same length). -/
@[extern "linen_duckdb_create_union_type"]
opaque createUnionType (memberTypes : @& Array LogicalType) (memberNames : @& Array String) :
    IO LogicalType

/-- Create a `STRUCT` type from parallel arrays of member types and names
    (must have the same length). -/
@[extern "linen_duckdb_create_struct_type"]
opaque createStructType (memberTypes : @& Array LogicalType) (memberNames : @& Array String) :
    IO LogicalType

/-- Create an `ENUM` type from its member names. -/
@[extern "linen_duckdb_create_enum_type"]
opaque createEnumType (memberNames : @& Array String) : IO LogicalType

/-- Create a `DECIMAL` type with the given width and scale. -/
@[extern "linen_duckdb_create_decimal_type"]
opaque createDecimalType (width : UInt8) (scale : UInt8) : IO LogicalType

/-! ── Inspection ── -/

/-- The raw `duckdb_type` id of `ty`. -/
@[extern "linen_duckdb_get_type_id_raw"]
opaque getTypeIdRaw (ty : @& LogicalType) : IO UInt32

/-- The `duckdb_type` id of `ty`, decoded. -/
def getTypeId (ty : LogicalType) : IO Type_ := do
  pure (Type_.ofUInt32 (← getTypeIdRaw ty))

/-- The width of a `DECIMAL` logical type. -/
@[extern "linen_duckdb_decimal_width"]
opaque decimalWidth (ty : @& LogicalType) : IO UInt8

/-- The scale of a `DECIMAL` logical type. -/
@[extern "linen_duckdb_decimal_scale"]
opaque decimalScale (ty : @& LogicalType) : IO UInt8

/-- The raw internal storage `duckdb_type` id of a `DECIMAL` logical type. -/
@[extern "linen_duckdb_decimal_internal_type_raw"]
opaque decimalInternalTypeRaw (ty : @& LogicalType) : IO UInt32

/-- The internal storage type of a `DECIMAL` logical type, decoded. -/
def decimalInternalType (ty : LogicalType) : IO Type_ := do
  pure (Type_.ofUInt32 (← decimalInternalTypeRaw ty))

/-- The raw internal storage `duckdb_type` id of an `ENUM` logical type. -/
@[extern "linen_duckdb_enum_internal_type_raw"]
opaque enumInternalTypeRaw (ty : @& LogicalType) : IO UInt32

/-- The internal storage type of an `ENUM` logical type, decoded. -/
def enumInternalType (ty : LogicalType) : IO Type_ := do
  pure (Type_.ofUInt32 (← enumInternalTypeRaw ty))

/-- The number of distinct values in an `ENUM` logical type's dictionary. -/
@[extern "linen_duckdb_enum_dictionary_size"]
opaque enumDictionarySize (ty : @& LogicalType) : IO UInt32

/-- The dictionary value at `index` for an `ENUM` logical type. -/
@[extern "linen_duckdb_enum_dictionary_value"]
opaque enumDictionaryValue (ty : @& LogicalType) (index : Idx) : IO (Option String)

/-- The child type of a `LIST` (or `MAP`) logical type. -/
@[extern "linen_duckdb_list_type_child_type"]
opaque listTypeChildType (ty : @& LogicalType) : IO LogicalType

/-- The child type of an `ARRAY` logical type. -/
@[extern "linen_duckdb_array_type_child_type"]
opaque arrayTypeChildType (ty : @& LogicalType) : IO LogicalType

/-- The fixed element count of an `ARRAY` logical type. -/
@[extern "linen_duckdb_array_type_array_size"]
opaque arrayTypeArraySize (ty : @& LogicalType) : IO Idx

/-- The key type of a `MAP` logical type. -/
@[extern "linen_duckdb_map_type_key_type"]
opaque mapTypeKeyType (ty : @& LogicalType) : IO LogicalType

/-- The value type of a `MAP` logical type. -/
@[extern "linen_duckdb_map_type_value_type"]
opaque mapTypeValueType (ty : @& LogicalType) : IO LogicalType

/-- The number of children of a `STRUCT` logical type. -/
@[extern "linen_duckdb_struct_type_child_count"]
opaque structTypeChildCount (ty : @& LogicalType) : IO Idx

/-- The name of the `STRUCT` child at `index`. -/
@[extern "linen_duckdb_struct_type_child_name"]
opaque structTypeChildName (ty : @& LogicalType) (index : Idx) : IO (Option String)

/-- The type of the `STRUCT` child at `index`. -/
@[extern "linen_duckdb_struct_type_child_type"]
opaque structTypeChildType (ty : @& LogicalType) (index : Idx) : IO LogicalType

/-- The number of members of a `UNION` logical type. -/
@[extern "linen_duckdb_union_type_member_count"]
opaque unionTypeMemberCount (ty : @& LogicalType) : IO Idx

/-- The name of the `UNION` member at `index`. -/
@[extern "linen_duckdb_union_type_member_name"]
opaque unionTypeMemberName (ty : @& LogicalType) (index : Idx) : IO (Option String)

/-- The type of the `UNION` member at `index`. -/
@[extern "linen_duckdb_union_type_member_type"]
opaque unionTypeMemberType (ty : @& LogicalType) (index : Idx) : IO LogicalType

/-! ── Destruction ── -/

/-- Destroy `ty`, deallocating all memory associated with it. Idempotent,
    like `Database.DuckDB.FFI.OpenConnect.close`. -/
@[extern "linen_duckdb_destroy_logical_type"]
opaque destroy : LogicalType → IO Unit

end Database.DuckDB.FFI.LogicalTypes
