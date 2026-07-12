/-
  Tests for `Linen.Database.DuckDB.FFI.FileSystem`.

  Exercises the full lifecycle against a real temporary file: `open` (with
  `create`/`write` flags) a fresh file, `write` then `sync`/`close`/reopen
  (with `read`), `read` the bytes back, check `tell`/`size`/`seek`, and the
  error path (`open` a nonexistent path without `create`).
-/
import Linen.Database.DuckDB.FFI.FileSystem
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.FileSystem
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.FileSystem

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
  let fs ← getFileSystem ctx

  let (tmpHandle, filePath) ← IO.FS.createTempFile
  tmpHandle.putStr "" -- ensure the handle is flushed before it is dropped
  IO.FS.removeFile filePath -- the file system should create it fresh below

  -- Write path: create + write, requires explicit `write`/`create` flags.
  let writeOpts ← createOpenOptions
  let setWriteState ← setOpenFlag writeOpts .write true
  unless setWriteState.isSuccess do throw (IO.userError "setOpenFlag write failed")
  let setCreateState ← setOpenFlag writeOpts .create true
  unless setCreateState.isSuccess do throw (IO.userError "setOpenFlag create failed")
  let writeOpenResult ← Database.DuckDB.FFI.FileSystem.openFile fs filePath.toString writeOpts
  let writeHandle ← match writeOpenResult with
    | .ok h => pure h
    | .error msg => throw (IO.userError msg)

  let payload := String.toUTF8 "hello, duckdb file system"
  let bytesWritten ← write writeHandle payload
  unless bytesWritten == payload.size.toInt64 do
    throw (IO.userError s!"expected to write {payload.size} bytes, wrote {bytesWritten}")

  let tellPos ← tell writeHandle
  unless tellPos == payload.size.toInt64 do
    throw (IO.userError s!"unexpected tell position: {tellPos}")

  let sizeAfterWrite ← size writeHandle
  unless sizeAfterWrite == payload.size.toInt64 do
    throw (IO.userError s!"unexpected size: {sizeAfterWrite}")

  let syncState ← sync writeHandle
  unless syncState.isSuccess do throw (IO.userError "sync failed")

  let closeState ← close writeHandle
  unless closeState.isSuccess do throw (IO.userError "close (write handle) failed")
  destroyFileHandle writeHandle
  destroyOpenOptions writeOpts

  -- Read path.
  let readOpts ← createOpenOptions
  let setReadState ← setOpenFlag readOpts .read true
  unless setReadState.isSuccess do throw (IO.userError "setOpenFlag read failed")
  let readOpenResult ← Database.DuckDB.FFI.FileSystem.openFile fs filePath.toString readOpts
  let readHandle ← match readOpenResult with
    | .ok h => pure h
    | .error msg => throw (IO.userError msg)

  let (bytesRead, data) ← read readHandle payload.size.toInt64
  unless bytesRead == payload.size.toInt64 do
    throw (IO.userError s!"expected to read {payload.size} bytes, got {bytesRead}")
  unless data == payload do
    throw (IO.userError "read data did not round-trip")

  let seekState ← seek readHandle 0
  unless seekState.isSuccess do throw (IO.userError "seek failed")
  let tellAfterSeek ← tell readHandle
  unless tellAfterSeek == 0 do throw (IO.userError s!"unexpected tell after seek: {tellAfterSeek}")

  let closeReadState ← close readHandle
  unless closeReadState.isSuccess do throw (IO.userError "close (read handle) failed")
  destroyFileHandle readHandle
  destroyOpenOptions readOpts

  -- Error path: opening a nonexistent path without `.create` fails, and
  -- the resulting error can be inspected via `errorData`.
  let missingOpts ← createOpenOptions
  let setMissingReadState ← setOpenFlag missingOpts .read true
  unless setMissingReadState.isSuccess do throw (IO.userError "setOpenFlag read failed")
  let missingResult ← Database.DuckDB.FFI.FileSystem.openFile fs "/no/such/directory/linen-test" missingOpts
  match missingResult with
  | .ok _ => throw (IO.userError "expected open to fail for a nonexistent path")
  | .error _ => pure ()
  destroyOpenOptions missingOpts

  IO.FS.removeFile filePath
  destroy fs
  destroyClientContext ctx
  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.FileSystem
