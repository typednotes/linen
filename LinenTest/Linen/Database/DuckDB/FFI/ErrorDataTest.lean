/-
  Tests for `Linen.Database.DuckDB.FFI.ErrorData`.

  Exercises `create`/`errorType`/`message`/`hasError`/`destroy` directly
  (upstream's `duckdb_create_error_data` is self-contained — no `Database`/
  `Connection` needed to build an `ErrorData` from scratch), plus a
  real-world `ErrorData` obtained from `Database.DuckDB.FFI.FileSystem`
  after a failed file open, to confirm the accessors also work on an object
  DuckDB itself populated.
-/
import Linen.Database.DuckDB.FFI.ErrorData
import Linen.Database.DuckDB.FFI.FileSystem
import Linen.Database.DuckDB.FFI.OpenConnect
import LinenTest.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.ErrorData
open Database.DuckDB.FFI.FileSystem
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.ErrorData

#eval show IO Unit from do
  -- A hand-built `ErrorData`.
  let err ← create .invalidInput "boom"
  let hasErr ← hasError err
  unless hasErr do throw (IO.userError "expected hasError to be true")
  let ty ← errorType err
  unless ty == .invalidInput do throw (IO.userError s!"unexpected errorType: {repr ty}")
  let msg ← message err
  unless msg == "boom" do throw (IO.userError s!"unexpected message: {msg}")
  destroy err
  destroy err -- idempotent

  -- A DuckDB-populated `ErrorData`, via a failed file open.
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")
  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")
  let ctx ← connectionGetClientContext conn
  let fs ← getFileSystem ctx
  let opts ← createOpenOptions
  let setFlagState ← setOpenFlag opts .read true
  unless setFlagState.isSuccess do throw (IO.userError "setOpenFlag failed")
  let openResult ← Database.DuckDB.FFI.FileSystem.openFile fs "/no/such/path/linen-test" opts
  match openResult with
  | .ok _ => throw (IO.userError "expected file open to fail for a nonexistent path")
  | .error _ => pure ()
  let fsErr ← Database.DuckDB.FFI.FileSystem.errorData fs
  let fsHasErr ← hasError fsErr
  unless fsHasErr do throw (IO.userError "expected the file system's errorData to report an error")
  let _fsMsg ← message fsErr -- just proves the round-trip works; exact wording is DuckDB-internal
  destroy fsErr
  destroyOpenOptions opts
  destroy fs
  destroyClientContext ctx
  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.ErrorData
