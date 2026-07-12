/-
  Linen.Database.DuckDB.Simple.Catalog — catalog inspection

  Module #10 of `docs/imports/duckdb-simple/dependencies.md`, on #1
  (`Linen.Database.DuckDB.Simple.Internal`, for `Connection`/`SQLError`/
  `withClientContext`) and `Linen.Database.DuckDB.FFI.Catalog`.

  ## Design

  `Linen.Database.DuckDB.FFI.Catalog` already provides the raw
  `duckdb_client_context_get_catalog`/`duckdb_catalog_get_entry` pair plus
  their handle accessors/destructors — this module is the `Simple`-package
  ergonomic wrapper around them, matching the rest of this batch's shape:
  every call is threaded through a `Connection` (fetching and releasing a
  scratch `ClientContext` via `Internal.withClientContext`, exactly as
  `Internal`'s own module doc describes for other bracketed FFI calls)
  rather than requiring the caller to manage a `ClientContext`/`Catalog`/
  `CatalogEntry` handle triple by hand, and failures are reported as
  `SQLError` via `throwSQLError` rather than upstream's own exception type
  (see `Internal`'s module doc for why).

  `CatalogEntryInfo` is a small snapshot (kind + name) taken eagerly and
  returned after the underlying `CatalogEntry`/`Catalog` handles are
  destroyed, since `Linen.Database.DuckDB.FFI.Catalog`'s handles are
  otherwise only valid for the duration of a single lookup call in this
  port (no long-lived catalog/entry handle is threaded back to the
  caller) — this mirrors `Materialize`'s own "decode into a plain value
  before releasing the handle" pattern rather than a new design.

  ### Scope

  Upstream's `Database.DuckDB.Simple.Catalog` module additionally exposes
  helpers to enumerate every entry of a catalog/schema and to inspect
  entry-kind-specific properties (e.g. a table's columns). Those upstream
  helpers loop over `duckdb_catalog_get_entry`-adjacent enumeration
  functions that live in the excluded modules of
  `docs/imports/duckdb-ffi/dependencies.md`; only the single-entry lookup
  covered by `Linen.Database.DuckDB.FFI.Catalog`'s six exported functions
  is in scope here, matching that module's own documented completeness
  boundary.

  ## Haskell source
  - `Database.DuckDB.Simple.Catalog` (`duckdb-simple` package, version
    0.1.5.1)
-/

import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.Catalog

namespace Database.DuckDB.Simple.Catalog

open Database.DuckDB.FFI.Types (CatalogEntryType)
open Database.DuckDB.Simple (Connection SQLError throwSQLError withClientContext)

-- ────────────────────────────────────────────────────────────────────
-- CatalogEntryInfo
-- ────────────────────────────────────────────────────────────────────

/-- A snapshot of a single catalog entry's kind and name, taken before its
    underlying `CatalogEntry`/`Catalog` handles are released (see the
    module doc). -/
structure CatalogEntryInfo where
  /-- The entry's kind (table/view/schema/…). -/
  entryType : CatalogEntryType
  /-- The entry's name. -/
  name : String
deriving BEq, Repr, Inhabited

-- ────────────────────────────────────────────────────────────────────
-- Lookups
-- ────────────────────────────────────────────────────────────────────

/-- The backend type name (e.g. `"duckdb"`) of the catalog named
    `catalogName` visible from `conn`, or `none` if no such catalog
    exists. -/
def catalogTypeName (conn : Connection) (catalogName : String) : IO (Option String) :=
  withClientContext conn fun ctx => do
    match ← Database.DuckDB.FFI.Catalog.clientContextGetCatalog ctx catalogName with
    | none => pure none
    | some catalog =>
      try
        some <$> Database.DuckDB.FFI.Catalog.catalogGetTypeName catalog
      finally
        Database.DuckDB.FFI.Catalog.destroyCatalog catalog

/-- Look up the entry of kind `entryType` named `name` in `schema` within
    the catalog named `catalogName`, visible from `conn`. Returns `none` if
    either the catalog or the entry does not exist. -/
def lookupEntry (conn : Connection) (catalogName : String) (entryType : CatalogEntryType)
    (schema : String) (name : String) : IO (Option CatalogEntryInfo) :=
  withClientContext conn fun ctx => do
    match ← Database.DuckDB.FFI.Catalog.clientContextGetCatalog ctx catalogName with
    | none => pure none
    | some catalog =>
      try
        match ← Database.DuckDB.FFI.Catalog.catalogGetEntry catalog ctx entryType schema name with
        | none => pure none
        | some entry =>
          try
            let kind ← Database.DuckDB.FFI.Catalog.catalogEntryGetType entry
            let entryName ← Database.DuckDB.FFI.Catalog.catalogEntryGetName entry
            pure (some { entryType := kind, name := entryName })
          finally
            Database.DuckDB.FFI.Catalog.destroyCatalogEntry entry
      finally
        Database.DuckDB.FFI.Catalog.destroyCatalog catalog

/-- Like `lookupEntry`, but throws a `SQLError` (via `throwSQLError`) rather
    than returning `none` when the catalog or entry cannot be found. -/
def getEntry (conn : Connection) (catalogName : String) (entryType : CatalogEntryType)
    (schema : String) (name : String) : IO CatalogEntryInfo := do
  match ← lookupEntry conn catalogName entryType schema name with
  | some info => pure info
  | none =>
    throwSQLError
      { message := s!"no such catalog entry: {catalogName}.{schema}.{name}" : SQLError }

end Database.DuckDB.Simple.Catalog
