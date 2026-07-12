/-
  Linen.Database.DuckDB.FFI.ExecutePrepared — execute a bound prepared
  statement

  Mirrors Haskell's `Database.DuckDB.FFI.ExecutePrepared` (the `duckdb-ffi`
  package). Module #8 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1). Upstream itself binds a
  single non-deprecated entry point, `duckdb_execute_prepared` — the
  deprecated `duckdb_execute_prepared_streaming` variant is out of scope,
  matching `docs/imports/duckdb-ffi/dependencies.md`'s blanket exclusion of
  the `Deprecated.*` tree.

  `duckdb_result` (`Types.Result`'s wrapped C type) is a flat by-value
  struct, not a pointer-typedef like every other handle in this batch — see
  `Linen/Database/DuckDB/FFI/Types.lean`'s doc comment on `ResultHandle` for
  how `ffi/duckdb_shim.c` still exposes it as a plain owning handle. -/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.ExecutePrepared

open Database.DuckDB.FFI.Types

/-- Raw `duckdb_execute_prepared`: `(state, result?)`. Per `duckdb.h`'s own
    doc comment, "the result must be freed with `duckdb_destroy_result`" —
    this port's C shim always populates and always returns a `Result`
    wrapper (even on failure, mirroring `duckdb_query`'s documented
    contract that error information also lives on the result object), so
    the caller never has to special-case the failure path to avoid a leak. -/
@[extern "linen_duckdb_execute_prepared"]
opaque executeRaw (preparedStatement : @& PreparedStatement) : IO (UInt32 × Types.Result)

/-- Execute `preparedStatement` with whatever parameters were last bound to
    it (via `Database.DuckDB.FFI.BindValues`), returning a materialized
    query result. May be called multiple times against the same prepared
    statement, with the bound parameters changed between calls. The
    resulting `Result` must eventually be destroyed with `destroy` (or let
    its GC finalizer do so) regardless of whether execution succeeded. -/
def execute (preparedStatement : PreparedStatement) : IO (State × Types.Result) := do
  let (rc, result) ← executeRaw preparedStatement
  pure (State.ofUInt32 rc, result)

/-- `duckdb_destroy_result`: release `result`'s underlying resources early.
    Idempotent — safe to call more than once, and safe to skip entirely (the
    GC finalizer calls it too). -/
@[extern "linen_duckdb_destroy_result"]
opaque destroy : Types.Result → IO Unit

end Database.DuckDB.FFI.ExecutePrepared
