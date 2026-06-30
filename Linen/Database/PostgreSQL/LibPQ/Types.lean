/-
  Linen.Database.PostgreSQL.LibPQ.Types — low-level libpq opaque handles + statuses

  Opaque handles wrapping libpq's `PGconn*`/`PGresult*` via Lean's external-object
  mechanism (the same pattern as `Network.Socket`'s `RawSocket`): the runtime owns
  the pointer and a C finalizer releases it. The `@[extern]` entry points that
  *use* these handles live in the `LibPQ` module; this file only declares the
  types and the status enums/decoders.

  Mirrors Haskell's `Database.PostgreSQL.LibPQ` (the `postgresql-libpq` package).
-/

namespace Database.PostgreSQL.LibPQ

/-! ── Opaque handles ── -/

/-- Opaque PostgreSQL connection handle (wraps `PGconn*`; GC finalizer calls `PQfinish`). -/
opaque PgConnHandle : NonemptyType

/-- A live (or formerly-live) PostgreSQL connection. -/
def PgConn : Type := PgConnHandle.type
instance : Nonempty PgConn := PgConnHandle.property

/-- Opaque PostgreSQL result handle (wraps `PGresult*`; GC finalizer calls `PQclear`). -/
opaque PgResultHandle : NonemptyType

/-- A query result set. -/
def PgResult : Type := PgResultHandle.type
instance : Nonempty PgResult := PgResultHandle.property

/-! ── Connection status (`PQstatus`) ── -/

/-- Connection status returned by `PQstatus`. -/
inductive ConnStatus where
  | ok
  | bad
  | other (code : UInt8)
  deriving BEq, Repr, Inhabited

/-- Decode a raw `PQstatus` integer. -/
def ConnStatus.ofUInt8 : UInt8 → ConnStatus
  | 0 => .ok
  | 1 => .bad
  | n => .other n

/-! ── Exec status (`PQresultStatus`) ── -/

/-- Result status returned by `PQresultStatus`. -/
inductive ExecStatus where
  | emptyQuery
  | commandOk
  | tuplesOk
  | copyOut
  | copyIn
  | badResponse
  | nonfatalError
  | fatalError
  | copyBoth
  | singleTuple
  | pipelineSync
  | pipelineAbort
  | other (code : UInt8)
  deriving BEq, Repr, Inhabited

/-- Decode a raw `PQresultStatus` integer. -/
def ExecStatus.ofUInt8 : UInt8 → ExecStatus
  | 0  => .emptyQuery
  | 1  => .commandOk
  | 2  => .tuplesOk
  | 3  => .copyOut
  | 4  => .copyIn
  | 5  => .badResponse
  | 6  => .nonfatalError
  | 7  => .fatalError
  | 8  => .copyBoth
  | 9  => .singleTuple
  | 10 => .pipelineSync
  | 11 => .pipelineAbort
  | n  => .other n

/-- Is this a successful exec status? -/
def ExecStatus.isOk : ExecStatus → Bool
  | .commandOk | .tuplesOk | .singleTuple | .emptyQuery => true
  | _ => false

/-! The "ok" statuses are exactly `{commandOk, tuplesOk, singleTuple, emptyQuery}`. -/
theorem ExecStatus.commandOk_isOk : ExecStatus.commandOk.isOk = true := rfl
theorem ExecStatus.tuplesOk_isOk : ExecStatus.tuplesOk.isOk = true := rfl
theorem ExecStatus.singleTuple_isOk : ExecStatus.singleTuple.isOk = true := rfl
theorem ExecStatus.emptyQuery_isOk : ExecStatus.emptyQuery.isOk = true := rfl
theorem ExecStatus.badResponse_not_isOk : ExecStatus.badResponse.isOk = false := rfl
theorem ExecStatus.fatalError_not_isOk : ExecStatus.fatalError.isOk = false := rfl
theorem ExecStatus.nonfatalError_not_isOk : ExecStatus.nonfatalError.isOk = false := rfl

/-! ── Transaction status (`PQtransactionStatus`) ── -/

/-- Transaction status returned by `PQtransactionStatus`. -/
inductive TransactionStatus where
  | idle
  | active
  | inTrans
  | inError
  | unknown
  deriving BEq, Repr, Inhabited

/-- Decode a raw transaction status. -/
def TransactionStatus.ofUInt8 : UInt8 → TransactionStatus
  | 0 => .idle
  | 1 => .active
  | 2 => .inTrans
  | 3 => .inError
  | _ => .unknown

/-! ── Errors & notifications ── -/

/-- A PostgreSQL error returned from libpq. -/
structure PgError where
  message : String
  status : ExecStatus
  deriving BEq, Repr

instance : ToString PgError where
  toString e := s!"PgError({e.status |> repr}): {e.message}"

/-- A LISTEN/NOTIFY notification from PostgreSQL. -/
structure PgNotification where
  channel : String
  payload : String
  pid : UInt32
  deriving BEq, Repr

end Database.PostgreSQL.LibPQ
