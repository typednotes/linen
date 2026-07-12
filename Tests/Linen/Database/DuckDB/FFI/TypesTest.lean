/-
  Tests for `Linen.Database.DuckDB.FFI.Types`.
-/
import Linen.Database.DuckDB.FFI.Types

open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.Types

/-! ── `State` round-trips ── -/

#guard State.ofUInt32 0 == .success
#guard State.ofUInt32 1 == .error
#guard State.ofUInt32 9999 == .error -- total: anything but 0 is `.error`

#guard State.toUInt32 .success == 0
#guard State.toUInt32 .error == 1

#guard State.ofUInt32 (State.toUInt32 .success) == .success
#guard State.ofUInt32 (State.toUInt32 .error) == .error

#guard State.success.isSuccess == true
#guard State.error.isSuccess == false

example : State.success.isSuccess = true := rfl
example : State.error.isSuccess = false := rfl

/-! ── `Type_` round-trips ── -/

#guard Type_.ofUInt32 0 == .invalid
#guard Type_.ofUInt32 1 == .boolean
#guard Type_.ofUInt32 4 == .integer
#guard Type_.ofUInt32 5 == .bigInt
#guard Type_.ofUInt32 17 == .varchar
#guard Type_.ofUInt32 32 == .uHugeInt
#guard Type_.ofUInt32 36 == .sqlNull
#guard Type_.ofUInt32 9999 == .other 9999

#guard Type_.toUInt32 .invalid == 0
#guard Type_.toUInt32 .varchar == 17
#guard Type_.toUInt32 .uHugeInt == 32
#guard Type_.toUInt32 (.other 9999) == 9999

#guard Type_.ofUInt32 (Type_.toUInt32 .bigInt) == .bigInt
#guard Type_.toUInt32 (Type_.ofUInt32 25) == 25

/-! ── `ErrorType` round-trips ── -/

#guard ErrorType.ofUInt32 0 == .invalid
#guard ErrorType.ofUInt32 22 == .syntax
#guard ErrorType.ofUInt32 42 == .invalidConfiguration
#guard ErrorType.ofUInt32 9999 == .other 9999

#guard ErrorType.toUInt32 .invalid == 0
#guard ErrorType.toUInt32 .syntax == 22
#guard ErrorType.toUInt32 .invalidConfiguration == 42
#guard ErrorType.toUInt32 (.other 9999) == 9999

#guard ErrorType.ofUInt32 (ErrorType.toUInt32 .catalog) == .catalog
#guard ErrorType.toUInt32 (ErrorType.ofUInt32 17) == 17

/-! ── `QueryProgress` ── -/

#guard ({ percentage := 42.0, rowsProcessed := 10, totalRowsToProcess := 100 } :
  QueryProgress).percentage == 42.0
#guard ({ percentage := -1.0, rowsProcessed := 0, totalRowsToProcess := 0 } :
  QueryProgress) == { percentage := -1.0, rowsProcessed := 0, totalRowsToProcess := 0 }

/-! ── `Idx` ── -/

#guard (5 : Idx) + (3 : Idx) == 8

end Tests.Database.DuckDB.FFI.Types
