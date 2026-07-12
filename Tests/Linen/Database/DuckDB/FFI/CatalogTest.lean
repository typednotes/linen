/-
  Tests for `Linen.Database.DuckDB.FFI.Catalog`.

  `duckdb_client_context_get_catalog`'s own doc comment in `duckdb.h` is
  explicit: "This function can only be called from within the context of an
  active transaction, e.g. during execution of a registered function
  callback. Otherwise returns `nullptr`." A plain FFI call from Lean, made
  outside any such callback, is exactly the "otherwise" case — so the one
  deterministic, real end-to-end behavior this batch can exercise without
  `Database.DuckDB.FFI.TableFunctions`/`ScalarFunctions` (both far out of
  scope for this batch) is that documented "no active transaction" failure
  mode itself: `clientContextGetCatalog` reliably returns `none` here, for
  any catalog name. Fetching a real, non-`none` `Catalog` (and hence a real
  `CatalogEntry`) would need a callback context this batch has no way to
  construct, so `catalogGetTypeName`/`catalogGetEntry`/`catalogEntryGetType`/
  `catalogEntryGetName`/`destroyCatalog`/`destroyCatalogEntry` are left
  untested here for that reason — not because they weren't ported, but
  because there is no real `Catalog`/`CatalogEntry` value this batch can
  legitimately construct to call them on.
-/
import Linen.Database.DuckDB.FFI.Catalog
import Linen.Database.DuckDB.FFI.OpenConnect

open Database.DuckDB.FFI.Catalog
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.Catalog

#eval show IO Unit from do
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")

  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  let ctx ← connectionGetClientContext conn

  -- Outside any registered-function callback, no transaction is active, so
  -- every lookup — whether or not the name would otherwise resolve to a real
  -- catalog — must come back `none`, per `duckdb_client_context_get_catalog`'s
  -- own documented contract.
  let memoryCatalog ← clientContextGetCatalog ctx "memory"
  if memoryCatalog.isSome then
    throw (IO.userError "expected clientContextGetCatalog to return none outside a transaction")

  let bogusCatalog ← clientContextGetCatalog ctx "definitely_not_a_real_catalog_name"
  if bogusCatalog.isSome then
    throw (IO.userError "expected clientContextGetCatalog to return none for an unknown name too")

  destroyClientContext ctx
  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.Catalog
