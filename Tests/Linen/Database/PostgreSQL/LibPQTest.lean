/-
  Tests for `Linen.Database.PostgreSQL.LibPQ`.

  Every binding here is a live libpq operation in `IO`, so it cannot be
  exercised by `#guard`/`#eval` without a running PostgreSQL server. These
  `example`s instead pin down the exact signature of each `@[extern]` binding:
  they fail to compile if a binding's type drifts, and — because the module
  is linked against libpq — a successful build also confirms every
  `linen_pg_*` symbol resolves. They double as documentation of the API.
-/
import Linen.Database.PostgreSQL.LibPQ

open Database.PostgreSQL.LibPQ

namespace Tests.Database.LibPQ

/-! ### Connection management -/

example : String → IO PgConn := connect
example : PgConn → IO ConnStatus := status
example : PgConn → IO String := errorMessage
example : PgConn → IO Unit := close

/-! ### Query execution -/

example : PgConn → String → IO PgResult := exec
example : PgConn → String → Array (Option String) → IO PgResult := execParams
example : PgConn → String → String → IO PgResult := prepare
example : PgConn → String → Array (Option String) → IO PgResult := execPrepared

/-! ### Result inspection -/

example : PgResult → IO ExecStatus := resultStatus
example : PgResult → IO String := resultErrorMessage
example : PgResult → IO UInt32 := ntuples
example : PgResult → IO UInt32 := nfields
example : PgResult → UInt32 → UInt32 → IO String := getvalue
example : PgResult → UInt32 → UInt32 → IO Bool := getIsNull
example : PgResult → UInt32 → IO String := fname
example : PgResult → UInt32 → IO UInt32 := ftype
example : PgResult → IO String := cmdTuples

/-! ### Escaping -/

example : PgConn → String → IO String := escapeLiteral
example : PgConn → String → IO String := escapeIdentifier

/-! ### LISTEN / NOTIFY -/

example : PgConn → IO Bool := consumeInput
example : PgConn → IO (Option PgNotification) := notifies

/-! ### Transaction state -/

example : PgConn → IO TransactionStatus := transactionStatus

/-! ### Convenience wrappers -/

example : PgConn → String → IO PgResult := execCheck
example : String → IO PgConn := connectCheck

end Tests.Database.LibPQ
