/-
  Tests for `Linen.Database.DuckDB.Simple.Catalog`.

  `duckdb_client_context_get_catalog`'s own doc comment (faithfully carried
  forward by `Linen.Database.DuckDB.FFI.Catalog`'s module doc, and this
  batch's `Tests/Linen/Database/DuckDB/FFI/CatalogTest.lean`) is explicit:
  it only returns a real catalog from within an active transaction/callback
  context, which a plain top-level `#eval` has no way to construct. So the
  one deterministic, real end-to-end behavior available here is that
  `catalogTypeName`/`lookupEntry` reliably report "no such catalog" (`none`,
  or a thrown `SQLError` for `getEntry`) outside such a context — not
  because `Catalog` wasn't ported, but because there is no real `Catalog`
  this batch can legitimately construct to look an entry up in.
-/
import Linen.Database.DuckDB.Simple.Catalog

open Database.DuckDB.Simple
open Database.DuckDB.Simple.Catalog
open Database.DuckDB.FFI.Types (CatalogEntryType)

namespace Tests.Database.DuckDB.Simple.Catalog

#eval show IO Unit from do
  let conn ← openConnection none -- in-memory database

  -- Outside any registered-function callback, no transaction is active, so
  -- every catalog lookup must come back `none`, per
  -- `duckdb_client_context_get_catalog`'s own documented contract.
  let memoryCatalog ← catalogTypeName conn "memory"
  if memoryCatalog.isSome then
    throw (IO.userError "expected catalogTypeName to return none outside a transaction")

  let entry ← lookupEntry conn "memory" .table "main" "no_such_table"
  if entry.isSome then
    throw (IO.userError "expected lookupEntry to return none outside a transaction")

  -- `getEntry` reports the same "not found" case as a thrown `SQLError`.
  let mut sawError := false
  try
    let _ ← getEntry conn "memory" .table "main" "no_such_table"
    pure ()
  catch _ =>
    sawError := true
  unless sawError do throw (IO.userError "expected getEntry to throw when the entry is missing")

  closeConnection conn

-- `CatalogEntryInfo`'s derived instances.
#guard
  ({ entryType := .table, name := "foo" } : CatalogEntryInfo) ==
    ({ entryType := .table, name := "foo" } : CatalogEntryInfo)
#guard ({ entryType := .view, name := "bar" } : CatalogEntryInfo).name == "bar"

end Tests.Database.DuckDB.Simple.Catalog
