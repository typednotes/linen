/-
  Linen.Database.DuckDB.FFI.Catalog — catalog inspection

  Mirrors Haskell's `Database.DuckDB.FFI.Catalog` (the `duckdb-ffi` package).
  Module #4 of `docs/imports/duckdb-ffi/dependencies.md`; depends only on
  `Database.DuckDB.FFI.Types` (module #1).

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`. This
  is the smallest of the five modules in this batch — a flat set of six raw
  C entry points, faithfully ported with no upstream simplifications:
  `duckdb_client_context_get_catalog` looks up a named catalog (e.g. the
  database's own catalog, or an attached one) from a client context;
  `duckdb_catalog_get_entry` then looks up a single entry (table/view/schema/…)
  within it by kind, schema and name; the remaining four are plain
  accessors/destructors on the resulting handles.
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.Catalog

open Database.DuckDB.FFI.Types

/-! ── Catalog lookup ── -/

/-- Raw `duckdb_client_context_get_catalog`: looks up the catalog named
    `name` visible from `context`. Returns `none` if no such catalog exists. -/
@[extern "linen_duckdb_client_context_get_catalog"]
opaque clientContextGetCatalogRaw (context : @& ClientContext) (name : @& String) :
    IO (Option Catalog)

/-- Look up the catalog named `name` (e.g. the default database catalog, or
    an attached one) visible from `context`. The resulting `Catalog` must be
    destroyed with `destroyCatalog` (or let its GC finalizer do so). Returns
    `none` if no such catalog exists. -/
def clientContextGetCatalog (context : ClientContext) (name : String) : IO (Option Catalog) :=
  clientContextGetCatalogRaw context name

/-- The backend type name of `catalog` (e.g. `"duckdb"`). -/
@[extern "linen_duckdb_catalog_get_type_name"]
opaque catalogGetTypeName : Catalog → IO String

/-- Raw `duckdb_catalog_get_entry`: looks up the entry of kind `entryType`
    named `name` in `schema` within `catalog`. Returns `none` if no such
    entry exists. -/
@[extern "linen_duckdb_catalog_get_entry"]
opaque catalogGetEntryRaw (catalog : @& Catalog) (context : @& ClientContext)
    (entryType : UInt32) (schema : @& String) (name : @& String) : IO (Option CatalogEntry)

/-- Look up the entry of kind `entryType` named `name` in `schema` within
    `catalog`. The resulting `CatalogEntry` must be destroyed with
    `destroyCatalogEntry` (or let its GC finalizer do so). Returns `none` if
    no such entry exists. -/
def catalogGetEntry (catalog : Catalog) (context : ClientContext) (entryType : CatalogEntryType)
    (schema : String) (name : String) : IO (Option CatalogEntry) :=
  catalogGetEntryRaw catalog context entryType.toUInt32 schema name

/-- Destroy `catalog`, releasing its native resources. Idempotent, like
    `Database.DuckDB.FFI.OpenConnect.close`. -/
@[extern "linen_duckdb_destroy_catalog"]
opaque destroyCatalog : Catalog → IO Unit

/-! ── Catalog entries ── -/

/-- Raw `duckdb_catalog_entry_get_type`. -/
@[extern "linen_duckdb_catalog_entry_get_type"]
opaque catalogEntryGetTypeRaw : CatalogEntry → IO UInt32

/-- The kind (table/view/schema/…) of `entry`. -/
def catalogEntryGetType (entry : CatalogEntry) : IO CatalogEntryType :=
  CatalogEntryType.ofUInt32 <$> catalogEntryGetTypeRaw entry

/-- The name of `entry`. -/
@[extern "linen_duckdb_catalog_entry_get_name"]
opaque catalogEntryGetName : CatalogEntry → IO String

/-- Destroy `entry`, releasing its native resources. Idempotent. -/
@[extern "linen_duckdb_destroy_catalog_entry"]
opaque destroyCatalogEntry : CatalogEntry → IO Unit

end Database.DuckDB.FFI.Catalog
