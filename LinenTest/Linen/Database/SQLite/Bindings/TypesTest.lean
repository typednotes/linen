/-
  Tests for `Linen.Database.SQLite.Bindings.Types`.
-/
import Linen.Database.SQLite.Bindings.Types

open Database.SQLite3.Bindings.Types

namespace Tests.Database.SQLite3.Bindings.Types

/-! ### `Error` round-trips ── -/

#guard Error.ofUInt32 0 == .ok
#guard Error.ofUInt32 1 == .error
#guard Error.ofUInt32 5 == .busy
#guard Error.ofUInt32 100 == .row
#guard Error.ofUInt32 101 == .done
#guard Error.ofUInt32 9999 == .other 9999

#guard Error.toUInt32 .ok == 0
#guard Error.toUInt32 .busy == 5
#guard Error.toUInt32 .row == 100
#guard Error.toUInt32 .done == 101
#guard Error.toUInt32 (.other 9999) == 9999

#guard Error.ofUInt32 (Error.toUInt32 .constraint) == .constraint
#guard Error.toUInt32 (Error.ofUInt32 19) == 19

/-! ### `Error.isOk` ── -/

#guard Error.ok.isOk == true
#guard Error.error.isOk == false
#guard Error.row.isOk == false
#guard Error.done.isOk == false

/-! ### `ColumnType` round-trips ── -/

#guard ColumnType.ofUInt32 1 == .integer
#guard ColumnType.ofUInt32 2 == .float
#guard ColumnType.ofUInt32 3 == .text
#guard ColumnType.ofUInt32 4 == .blob
#guard ColumnType.ofUInt32 5 == .null

#guard ColumnType.toUInt32 .integer == 1
#guard ColumnType.toUInt32 .float == 2
#guard ColumnType.toUInt32 .text == 3
#guard ColumnType.toUInt32 .blob == 4
#guard ColumnType.toUInt32 .null == 5

#guard ColumnType.ofUInt32 (ColumnType.toUInt32 .blob) == .blob

/-! ### `StepResult` ── -/

#guard (StepResult.row == StepResult.row)
#guard (StepResult.row != StepResult.done)

end Tests.Database.SQLite3.Bindings.Types
