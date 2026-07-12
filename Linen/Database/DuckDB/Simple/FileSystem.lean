/-
  Linen.Database.DuckDB.Simple.FileSystem — high-level virtual file access

  Module #12 of `docs/imports/duckdb-simple/dependencies.md`, on #1
  (`Linen.Database.DuckDB.Simple.Internal`, for `Connection`/`SQLError`/
  `withClientContext`) and `Linen.Database.DuckDB.FFI.FileSystem`.

  ## Design

  `Linen.Database.DuckDB.FFI.FileSystem` already provides the raw
  lifecycle/error/I-O entry points (`getFileSystem`/`createOpenOptions`/
  `openFile`/`destroy*`/`read`/`write`/`tell`/`size`/`seek`/`sync`/`close`)
  — this module is the `Simple`-package ergonomic wrapper around them,
  matching the rest of this batch's bracket-style shape (`Internal`'s own
  module doc; `Catalog`/`Config`'s matching wrappers): `withFileSystem`/
  `withFileHandle` fetch and release the underlying `FileSystem`/
  `FileHandle` handles automatically, and every failure is reported as an
  `SQLError` via `throwSQLError` rather than upstream's own exception type.

  Per the fetched upstream source, `Database.DuckDB.Simple.FileSystem`
  itself is built the same way: `withFileHandle` (bracketed open/close),
  `readFileHandleChunk`/`writeFileHandleBytes` (byte-buffer I/O), and
  `fileHandleTell`/`fileHandleSize`/`fileHandleSeek`/`fileHandleSync` (thin,
  error-checked wrappers) — ported here under the same names, minus the
  manual `alloca`/`mallocBytes`/`free` marshalling Lean's GC-managed
  `ByteArray`/handle types make unnecessary (the same simplification
  `Internal`'s own module doc describes for `Connection`/`SQLError`).

  ## Haskell source
  - `Database.DuckDB.Simple.FileSystem` (`duckdb-simple` package, version
    0.1.5.1)
-/
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.FileSystem

namespace Database.DuckDB.Simple.FileSystem

open Database.DuckDB.FFI.Types (FileFlag FileHandle)
open Database.DuckDB.Simple (Connection SQLError throwSQLError withClientContext)

-- ────────────────────────────────────────────────────────────────────
-- Bracketed access
-- ────────────────────────────────────────────────────────────────────

/-- Run `action` against `conn`'s virtual/attached file system, destroying
    the fetched handle afterwards regardless of whether `action`
    succeeded. -/
def withFileSystem (conn : Connection)
    (action : Database.DuckDB.FFI.Types.FileSystem → IO α) : IO α :=
  withClientContext conn fun ctx => do
    let fs ← Database.DuckDB.FFI.FileSystem.getFileSystem ctx
    try
      action fs
    finally
      Database.DuckDB.FFI.FileSystem.destroy fs

/-- Open the file at `path` through `conn`'s file system with `flags`
    enabled, run `action` against the resulting handle, then close and
    destroy it — upstream's `withFileHandle`. -/
def withFileHandle (conn : Connection) (path : String) (flags : Array FileFlag)
    (action : FileHandle → IO α) : IO α :=
  withFileSystem conn fun fs => do
    let options ← Database.DuckDB.FFI.FileSystem.createOpenOptions
    try
      for flag in flags do
        discard <| Database.DuckDB.FFI.FileSystem.setOpenFlag options flag true
      match ← Database.DuckDB.FFI.FileSystem.openFile fs path options with
      | .error msg => throwSQLError { message := msg : SQLError }
      | .ok handle =>
        try
          action handle
        finally
          discard <| Database.DuckDB.FFI.FileSystem.close handle
          Database.DuckDB.FFI.FileSystem.destroyFileHandle handle
    finally
      Database.DuckDB.FFI.FileSystem.destroyOpenOptions options

-- ────────────────────────────────────────────────────────────────────
-- I/O
-- ────────────────────────────────────────────────────────────────────

/-- Read up to `size` bytes from `handle` (upstream's
    `readFileHandleChunk`), throwing an `SQLError` if the underlying read
    failed (a negative `bytesRead`). -/
def readFileHandleChunk (handle : FileHandle) (size : Int64) : IO ByteArray := do
  let (bytesRead, data) ← Database.DuckDB.FFI.FileSystem.read handle size
  if bytesRead < 0 then
    throwSQLError { message := "duckdb_file_handle_read failed" : SQLError }
  else
    pure data

/-- Write every byte of `data` to `handle` (upstream's
    `writeFileHandleBytes`), throwing an `SQLError` if not all of it was
    written. -/
def writeFileHandleBytes (handle : FileHandle) (data : ByteArray) : IO Unit := do
  let written ← Database.DuckDB.FFI.FileSystem.write handle data
  if written ≠ data.size.toInt64 then
    throwSQLError { message := "duckdb_file_handle_write failed" : SQLError }

/-- The current byte position in `handle` (upstream's `fileHandleTell`). -/
def fileHandleTell (handle : FileHandle) : IO Int64 :=
  Database.DuckDB.FFI.FileSystem.tell handle

/-- The size of `handle`'s underlying file, in bytes (upstream's
    `fileHandleSize`). -/
def fileHandleSize (handle : FileHandle) : IO Int64 :=
  Database.DuckDB.FFI.FileSystem.size handle

/-- Seek `handle` to the absolute byte offset `position` (upstream's
    `fileHandleSeek`), throwing an `SQLError` on failure. -/
def fileHandleSeek (handle : FileHandle) (position : Int64) : IO Unit := do
  match ← Database.DuckDB.FFI.FileSystem.seek handle position with
  | .success => pure ()
  | .error => throwSQLError { message := s!"duckdb_file_handle_seek failed at {position}" : SQLError }

/-- Flush `handle`'s buffered writes to stable storage (upstream's
    `fileHandleSync`), throwing an `SQLError` on failure. -/
def fileHandleSync (handle : FileHandle) : IO Unit := do
  match ← Database.DuckDB.FFI.FileSystem.sync handle with
  | .success => pure ()
  | .error => throwSQLError { message := "duckdb_file_handle_sync failed" : SQLError }

end Database.DuckDB.Simple.FileSystem
