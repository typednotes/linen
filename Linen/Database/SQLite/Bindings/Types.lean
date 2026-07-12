/-
  Linen.Database.SQLite.Bindings.Types — low-level SQLite3 opaque handles +
  result-code/column-type enums

  Opaque handles wrapping SQLite's `sqlite3*`/`sqlite3_stmt*` via Lean's
  external-object mechanism (the same pattern as
  `Linen.Database.PostgreSQL.LibPQ.Types`'s `PgConn`/`PgResult`): the runtime
  owns the pointer and a C finalizer releases it (`sqlite3_close_v2`/
  `sqlite3_finalize`) if the caller never explicitly closed/finalized it.

  Upstream (`direct-sqlite`) additionally declares a raw-`Ptr`-level module,
  generated via `hsc2hs` against the bundled `cbits/sqlite3.h`
  (`CDatabase`/`CStatement` phantom types, `newtype CError`/`CColumnType`
  wrapping a raw `CInt`, `CParamIndex`/`CColumnIndex`/`CNumBytes`, the
  `FFIType` conversion class, …). Lean's own FFI has no such intermediate
  "raw pointer, not yet a real Lean value" layer — `@[extern]` declarations
  operate directly on typed external objects — so that raw layer collapses
  into this one module: `Database`/`Statement` below *are* what `CDatabase`/
  `CStatement` + `Ptr` + the external-object wrapper are collectively for
  in Haskell, and `Error`/`ColumnType` decode directly from the raw
  `UInt32` result code (no separate `CError`/`decodeError` pair is needed,
  since there's only one Lean-side representation to convert to).

  Mirrors Haskell's `Database.SQLite3.Bindings.Types` (the `direct-sqlite`
  package).
-/

namespace Database.SQLite3.Bindings.Types

/-! ── Opaque handles ── -/

/-- Opaque SQLite database connection handle (wraps `sqlite3*`; GC finalizer
    calls `sqlite3_close_v2`). -/
opaque DatabaseHandle : NonemptyType

/-- A live (or formerly-live) SQLite database connection. -/
def Database : Type := DatabaseHandle.type
instance : Nonempty Database := DatabaseHandle.property

/-- Opaque SQLite prepared-statement handle (wraps `sqlite3_stmt*`; GC
    finalizer calls `sqlite3_finalize`). -/
opaque StatementHandle : NonemptyType

/-- A prepared (or formerly-prepared) SQL statement. -/
def Statement : Type := StatementHandle.type
instance : Nonempty Statement := StatementHandle.property

/-- Opaque, ephemeral wrapper around a `sqlite3_context*` — the handle
    `sqlite3_create_function`'s callback uses to report a scalar function's
    result. Added for module #16 of `docs/imports/sqlite-simple/
    dependencies.md` (`Linen.Database.SQLite.Simple.Function`), which needs
    `sqlite3_create_function` — not otherwise exposed by modules #1–#4.
    Unlike `Database`/`Statement` above, this wraps a *borrowed* pointer:
    SQLite owns the underlying `sqlite3_context`, which is only valid for the
    duration of one callback invocation, so the finalizer here frees only the
    Lean-side wrapper, never the pointer itself. -/
opaque FuncContextHandle : NonemptyType

/-- A scalar-function callback's result handle, valid only for the duration
    of that one callback invocation. -/
def FuncContext : Type := FuncContextHandle.type
instance : Nonempty FuncContext := FuncContextHandle.property

/-- Opaque, ephemeral wrapper around a callback's `sqlite3_value**` argument
    array (see `FuncContext`'s doc for why this is a borrowed-pointer
    wrapper, not an owning one). -/
opaque FuncArgsHandle : NonemptyType

/-- A scalar-function callback's argument array, valid only for the
    duration of that one callback invocation. -/
def FuncArgs : Type := FuncArgsHandle.type
instance : Nonempty FuncArgs := FuncArgsHandle.property

/-! ── Indices ── -/

/-- Index of a bound parameter in a parameterized query (1-based). See
    <http://www.sqlite.org/lang_expr.html#varparam>. -/
abbrev ParamIndex : Type := UInt32

/-- Index of a column in a result row (0-based). -/
abbrev ColumnIndex : Type := UInt32

/-- Number of columns in a result set. -/
abbrev ColumnCount : Type := UInt32

/-! ── Result codes (`sqlite3_errcode` et al.) ── -/

/-- SQLite result codes. Documentation summarized from
    <http://www.sqlite.org/rescode.html>.

    `other` covers any raw code this enum doesn't otherwise name (e.g. an
    extended result code, or a code introduced by a newer SQLite than the
    one vendored) — the same total-decoding pattern as
    `Linen.Database.PostgreSQL.LibPQ.Types.ConnStatus.other`, chosen instead
    of upstream's partial `decodeError` (which is documented *undefined
    behaviour* on an unrecognized code). -/
inductive Error where
  | ok                    -- ^ Successful result
  | error                 -- ^ SQL error or missing database
  | internal              -- ^ Internal logic error in SQLite
  | permission            -- ^ Access permission denied
  | abort                 -- ^ Callback routine requested an abort
  | busy                  -- ^ The database file is locked
  | locked                -- ^ A table in the database is locked
  | noMemory              -- ^ A memory allocation failed
  | readOnly              -- ^ Attempt to write a readonly database
  | interrupt             -- ^ Operation terminated by `sqlite3_interrupt`
  | io                    -- ^ Some kind of disk I/O error occurred
  | corrupt               -- ^ The database disk image is malformed
  | notFound              -- ^ Unknown opcode in `sqlite3_file_control`
  | full                  -- ^ Insertion failed because database is full
  | cantOpen              -- ^ Unable to open the database file
  | protocol              -- ^ Database lock protocol error
  | empty                 -- ^ Internal use only
  | schema                -- ^ The database schema changed
  | tooBig                -- ^ String or BLOB exceeds size limit
  | constraint            -- ^ Abort due to constraint violation
  | mismatch              -- ^ Data type mismatch
  | misuse                -- ^ Library used incorrectly
  | noLargeFileSupport    -- ^ Uses OS features not supported on host
  | authorization         -- ^ Authorization denied
  | format                -- ^ Not used
  | range                 -- ^ 2nd parameter to `sqlite3_bind` out of range
  | notADatabase          -- ^ File opened that is not a database file
  | row                   -- ^ `sqlite3_step` has another row ready
  | done                  -- ^ `sqlite3_step` has finished executing
  | other (code : UInt32) -- ^ Any other raw result/extended-result code
  deriving BEq, Repr, Inhabited

/-- Decode a raw SQLite primary result code. Total: unrecognized codes
    (including every extended result code, e.g. `SQLITE_IOERR_READ`) decode
    to `.other`. -/
def Error.ofUInt32 : UInt32 → Error
  | 0  => .ok
  | 1  => .error
  | 2  => .internal
  | 3  => .permission
  | 4  => .abort
  | 5  => .busy
  | 6  => .locked
  | 7  => .noMemory
  | 8  => .readOnly
  | 9  => .interrupt
  | 10 => .io
  | 11 => .corrupt
  | 12 => .notFound
  | 13 => .full
  | 14 => .cantOpen
  | 15 => .protocol
  | 16 => .empty
  | 17 => .schema
  | 18 => .tooBig
  | 19 => .constraint
  | 20 => .mismatch
  | 21 => .misuse
  | 22 => .noLargeFileSupport
  | 23 => .authorization
  | 24 => .format
  | 25 => .range
  | 26 => .notADatabase
  | 100 => .row
  | 101 => .done
  | n  => .other n

/-- Encode an `Error` back to its raw primary result code (extended codes
    stored in `.other` round-trip through their original value). -/
def Error.toUInt32 : Error → UInt32
  | .ok => 0
  | .error => 1
  | .internal => 2
  | .permission => 3
  | .abort => 4
  | .busy => 5
  | .locked => 6
  | .noMemory => 7
  | .readOnly => 8
  | .interrupt => 9
  | .io => 10
  | .corrupt => 11
  | .notFound => 12
  | .full => 13
  | .cantOpen => 14
  | .protocol => 15
  | .empty => 16
  | .schema => 17
  | .tooBig => 18
  | .constraint => 19
  | .mismatch => 20
  | .misuse => 21
  | .noLargeFileSupport => 22
  | .authorization => 23
  | .format => 24
  | .range => 25
  | .notADatabase => 26
  | .row => 100
  | .done => 101
  | .other n => n

/-- `SQLITE_OK` is the only success code for a non-`step` operation. -/
def Error.isOk : Error → Bool
  | .ok => true
  | _ => false

theorem Error.ok_isOk : Error.ok.isOk = true := rfl
theorem Error.error_not_isOk : Error.error.isOk = false := rfl

/-! ── Column types (`sqlite3_column_type`) ── -/

/-- The datatype SQLite reports for a result column, per
    <http://www.sqlite.org/c3ref/c_blob.html>. -/
inductive ColumnType where
  | integer
  | float
  | text
  | blob
  | null
  deriving BEq, Repr, Inhabited

/-- Decode a raw `sqlite3_column_type` code. Falls back to `.null` for any
    value outside SQLite's five fundamental datatypes (which the C API never
    actually returns) rather than being partial, per the same total-decoding
    rationale as `Error.ofUInt32`. -/
def ColumnType.ofUInt32 : UInt32 → ColumnType
  | 1 => .integer
  | 2 => .float
  | 3 => .text
  | 4 => .blob
  | 5 => .null
  | _ => .null

/-- Encode a `ColumnType` back to its raw `SQLITE_*` code. -/
def ColumnType.toUInt32 : ColumnType → UInt32
  | .integer => 1
  | .float => 2
  | .text => 3
  | .blob => 4
  | .null => 5

/-! ── `sqlite3_step` outcomes ── -/

/-- The two "successful" outcomes of `sqlite3_step`. -/
inductive StepResult where
  | row   -- ^ Another row of the result set is available.
  | done  -- ^ Execution of the statement finished successfully.
  deriving BEq, Repr, Inhabited

end Database.SQLite3.Bindings.Types
