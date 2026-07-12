/-
  Linen.Database.DuckDB.FFI.FileSystem — virtual/attached filesystem access

  Mirrors Haskell's `Database.DuckDB.FFI.FileSystem` (the `duckdb-ffi`
  package). Module #9 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1), which declares the
  `FileSystem`/`FileOpenOptions`/`FileHandle` handles this module operates
  on.

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`, and
  splits into three groups, matching `duckdb.h`'s own "File System
  Interface" section:

  - **Lifecycle**: `getFileSystem` fetches a client context's file-system
    instance; `createOpenOptions`/`setOpenFlag` build up an open mode;
    `open` combines a file system + path + options into a `FileHandle`;
    `destroy*` explicitly release each of the three handle kinds early.
  - **Errors**: `errorData`/`handleErrorData` fetch the last structured
    error recorded on a `FileSystem`/`FileHandle` respectively.
  - **I/O**: `read`/`write`/`tell`/`size`/`seek`/`sync`/`close` operate on an
    open `FileHandle`.

  `read`/`write` marshal their buffers as a plain Lean `ByteArray`, the same
  treatment `Database.DuckDB.FFI.Appender.appendBlob`/
  `Database.DuckDB.FFI.BindValues.bindBlob` give their own raw-byte
  parameters — `duckdb_file_handle_read`'s `int64_t size` in/`int64_t`
  bytes-actually-read out become, respectively, the requested `ByteArray`'s
  length in and the returned `ByteArray`'s (possibly shorter) length out. -/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.FileSystem

open Database.DuckDB.FFI.Types

/-! ── Lifecycle ── -/

/-- `duckdb_client_context_get_file_system`: the virtual/attached file
    system associated with `context`. The result must eventually be
    destroyed with `destroy` (or let its GC finalizer do so). -/
@[extern "linen_duckdb_client_context_get_file_system"]
opaque getFileSystem (context : @& ClientContext) : IO Types.FileSystem

/-- `duckdb_destroy_file_system`: release `fileSystem`'s underlying
    resources early. Idempotent. -/
@[extern "linen_duckdb_destroy_file_system"]
opaque destroy : Types.FileSystem → IO Unit

/-- `duckdb_create_file_open_options`: a blank, mutable set of open flags.
    The result must eventually be destroyed with `destroyOpenOptions` (or
    let its GC finalizer do so). -/
@[extern "linen_duckdb_create_file_open_options"]
opaque createOpenOptions : IO FileOpenOptions

/-- Raw `duckdb_file_open_options_set_flag`: takes the raw `duckdb_file_flag`
    code (see `setOpenFlag`). -/
@[extern "linen_duckdb_file_open_options_set_flag"]
opaque setOpenFlagRaw (options : @& FileOpenOptions) (flag : UInt32) (value : Bool) : IO UInt32

/-- Enable or disable `flag` on `options`. -/
def setOpenFlag (options : FileOpenOptions) (flag : FileFlag) (value : Bool) : IO State :=
  State.ofUInt32 <$> setOpenFlagRaw options flag.toUInt32 value

/-- `duckdb_destroy_file_open_options`: release `options`'s underlying
    resources early. Idempotent. -/
@[extern "linen_duckdb_destroy_file_open_options"]
opaque destroyOpenOptions : FileOpenOptions → IO Unit

/-- Raw `duckdb_file_system_open`: `(state, fileHandle?)`. -/
@[extern "linen_duckdb_file_system_open"]
opaque openRaw (fileSystem : @& Types.FileSystem) (path : @& String) (options : @& FileOpenOptions) :
    IO (UInt32 × Option FileHandle)

/-- Open the file at `path` through `fileSystem` with the modes set on
    `options`. On failure, the underlying error can be retrieved via
    `errorData fileSystem`. The resulting `FileHandle` must eventually be
    destroyed with `destroyFileHandle` (or let its GC finalizer do so).

    Named `openFile`, not `open` (upstream's name) — `open` is a Lean
    keyword (the namespace-opening command) and can't be used as an
    ordinary identifier, the same rename `Database.DuckDB.FFI.OpenConnect.openDatabase`
    already applies to `duckdb_open`. -/
def openFile (fileSystem : Types.FileSystem) (path : String) (options : FileOpenOptions) :
    IO (Except String FileHandle) := do
  let (rc, handleOpt) ← openRaw fileSystem path options
  match State.ofUInt32 rc, handleOpt with
  | .success, some handle => pure (.ok handle)
  | _, _ => pure (.error s!"duckdb_file_system_open failed for {path}")

/-- `duckdb_destroy_file_handle`: close (if still open) and release
    `fileHandle`'s underlying resources early. Idempotent. -/
@[extern "linen_duckdb_destroy_file_handle"]
opaque destroyFileHandle : FileHandle → IO Unit

/-! ── Errors ── -/

/-- `duckdb_file_system_error_data`: the last structured error recorded on
    `fileSystem`. The result must eventually be destroyed with
    `Database.DuckDB.FFI.ErrorData.destroy` (or let its GC finalizer do
    so). -/
@[extern "linen_duckdb_file_system_error_data"]
opaque errorData (fileSystem : @& Types.FileSystem) : IO Types.ErrorData

/-- `duckdb_file_handle_error_data`: the last structured error recorded on
    `fileHandle`. -/
@[extern "linen_duckdb_file_handle_error_data"]
opaque handleErrorData (fileHandle : @& FileHandle) : IO Types.ErrorData

/-! ── I/O ── -/

/-- Raw `duckdb_file_handle_read`: reads up to `size` bytes from
    `fileHandle`, returning `(bytesRead, data)` where `data.size ≤ size` (its
    length is `bytesRead` when `bytesRead ≥ 0`, or `0` on error). -/
@[extern "linen_duckdb_file_handle_read"]
opaque read (fileHandle : @& FileHandle) (size : Int64) : IO (Int64 × ByteArray)

/-- Raw `duckdb_file_handle_write`: writes `data` to `fileHandle`, returning
    the number of bytes actually written (negative on error). -/
@[extern "linen_duckdb_file_handle_write"]
opaque write (fileHandle : @& FileHandle) (data : @& ByteArray) : IO Int64

/-- `duckdb_file_handle_tell`: the current byte position in `fileHandle`
    (negative on error). -/
@[extern "linen_duckdb_file_handle_tell"]
opaque tell (fileHandle : @& FileHandle) : IO Int64

/-- `duckdb_file_handle_size`: the size of `fileHandle`'s underlying file, in
    bytes (negative on error). -/
@[extern "linen_duckdb_file_handle_size"]
opaque size (fileHandle : @& FileHandle) : IO Int64

/-- Raw `duckdb_file_handle_seek`. -/
@[extern "linen_duckdb_file_handle_seek"]
opaque seekRaw (fileHandle : @& FileHandle) (position : Int64) : IO UInt32

/-- Seek `fileHandle` to the absolute byte offset `position`. On failure, the
    underlying error can be retrieved via `handleErrorData fileHandle`. -/
def seek (fileHandle : FileHandle) (position : Int64) : IO State :=
  State.ofUInt32 <$> seekRaw fileHandle position

/-- Raw `duckdb_file_handle_sync`. -/
@[extern "linen_duckdb_file_handle_sync"]
opaque syncRaw (fileHandle : @& FileHandle) : IO UInt32

/-- Flush `fileHandle`'s buffered writes to stable storage. -/
def sync (fileHandle : FileHandle) : IO State :=
  State.ofUInt32 <$> syncRaw fileHandle

/-- Raw `duckdb_file_handle_close`. -/
@[extern "linen_duckdb_file_handle_close"]
opaque closeRaw (fileHandle : @& FileHandle) : IO UInt32

/-- Close `fileHandle` without destroying the surrounding handle object
    (`destroyFileHandle` still needs to be called, or its GC finalizer
    relied on, to release the object itself). -/
def close (fileHandle : FileHandle) : IO State :=
  State.ofUInt32 <$> closeRaw fileHandle

end Database.DuckDB.FFI.FileSystem
