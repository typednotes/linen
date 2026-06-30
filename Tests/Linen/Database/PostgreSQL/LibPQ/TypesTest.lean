/-
  Tests for `Linen.Database.PostgreSQL.LibPQ.Types`.

  The opaque `PgConn`/`PgResult` handles can only be produced via FFI, so only
  their `Nonempty`-ness is checked; the status enums/decoders are fully tested.
-/
import Linen.Database.PostgreSQL.LibPQ.Types

open Database.PostgreSQL.LibPQ

namespace Tests.Database.LibPQTypes

/-! ### opaque handles are inhabited -/

example : Nonempty PgConn := inferInstance
example : Nonempty PgResult := inferInstance

/-! ### ConnStatus -/

#guard ConnStatus.ofUInt8 0 == ConnStatus.ok
#guard ConnStatus.ofUInt8 1 == ConnStatus.bad
#guard ConnStatus.ofUInt8 7 == ConnStatus.other 7

/-! ### ExecStatus decode + isOk -/

#guard ExecStatus.ofUInt8 1 == ExecStatus.commandOk
#guard ExecStatus.ofUInt8 2 == ExecStatus.tuplesOk
#guard ExecStatus.ofUInt8 7 == ExecStatus.fatalError
#guard ExecStatus.ofUInt8 9 == ExecStatus.singleTuple
#guard ExecStatus.ofUInt8 99 == ExecStatus.other 99
#guard ExecStatus.commandOk.isOk == true
#guard ExecStatus.tuplesOk.isOk == true
#guard ExecStatus.singleTuple.isOk == true
#guard ExecStatus.emptyQuery.isOk == true
#guard ExecStatus.badResponse.isOk == false
#guard ExecStatus.fatalError.isOk == false
#guard ExecStatus.copyOut.isOk == false

/-! ### TransactionStatus -/

#guard TransactionStatus.ofUInt8 0 == TransactionStatus.idle
#guard TransactionStatus.ofUInt8 3 == TransactionStatus.inError
#guard TransactionStatus.ofUInt8 9 == TransactionStatus.unknown

/-! ### PgError / PgNotification -/

#guard (PgError.mk "boom" .fatalError) == PgError.mk "boom" .fatalError
#guard ((PgError.mk "boom" .fatalError) == PgError.mk "boom" .tuplesOk) == false
#guard (PgError.mk "oops" .fatalError).message == "oops"
#guard (PgNotification.mk "ch" "payload" 42).pid == 42
#guard (PgNotification.mk "ch" "payload" 42) == PgNotification.mk "ch" "payload" 42
#guard ((PgNotification.mk "ch" "p" 1) == PgNotification.mk "ch" "p" 2) == false

/-! ### isOk law (compile-time) -/

example : ExecStatus.tuplesOk.isOk = true := ExecStatus.tuplesOk_isOk
example : ExecStatus.fatalError.isOk = false := ExecStatus.fatalError_not_isOk

end Tests.Database.LibPQTypes
