/-
  Tests for `Linen.Database.DuckDB.Simple.FileSystem`.

  Mirrors `Tests/Linen/Database/DuckDB/FFI/FileSystemTest.lean`'s own
  real-temp-file round trip, but through this module's bracketed
  `Connection`-taking wrappers (`withFileHandle`/`readFileHandleChunk`/
  `writeFileHandleBytes`/`fileHandleTell`/`fileHandleSize`/
  `fileHandleSeek`/`fileHandleSync`) instead of the raw FFI entry points
  directly — write a real file, close it, reopen it for reading, and
  confirm the bytes round-trip exactly.
-/
import Linen.Database.DuckDB.Simple.FileSystem
import Linen.Database.DuckDB.Simple.Internal

open Database.DuckDB.Simple
open Database.DuckDB.Simple.FileSystem
open Database.DuckDB.FFI.Types (FileFlag)

namespace Tests.Database.DuckDB.Simple.FileSystem

#eval show IO Unit from do
  let conn ← openConnection none -- in-memory database

  let (tmpHandle, filePath) ← IO.FS.createTempFile
  tmpHandle.putStr "" -- ensure the handle is flushed before it is dropped
  IO.FS.removeFile filePath -- the file system should create it fresh below
  let path := filePath.toString

  let payload := String.toUTF8 "hello, duckdb-simple file system"

  -- Write path: create + write.
  withFileHandle conn path #[.write, .create] fun handle => do
    writeFileHandleBytes handle payload
    let pos ← fileHandleTell handle
    unless pos == payload.size.toInt64 do
      throw (IO.userError s!"unexpected tell position after write: {pos}")
    let sz ← fileHandleSize handle
    unless sz == payload.size.toInt64 do
      throw (IO.userError s!"unexpected size after write: {sz}")
    fileHandleSync handle

  -- Read path.
  withFileHandle conn path #[.read] fun handle => do
    let data ← readFileHandleChunk handle payload.size.toInt64
    unless data == payload do throw (IO.userError "read data did not round-trip")
    fileHandleSeek handle 0
    let pos ← fileHandleTell handle
    unless pos == 0 do throw (IO.userError s!"unexpected tell position after seek: {pos}")

  -- Opening a nonexistent path without `.create` fails with an `SQLError`.
  let mut sawError := false
  try
    withFileHandle conn "/no/such/directory/linen-duckdb-simple-test" #[.read] fun _ => pure ()
  catch _ =>
    sawError := true
  unless sawError do throw (IO.userError "expected withFileHandle to fail for a missing file")

  IO.FS.removeFile filePath
  closeConnection conn

end Tests.Database.DuckDB.Simple.FileSystem
