/-
  Tests for `Linen.System.Exit`.

  The `exit*` actions terminate the process, so they are only *type-checked*
  here — never `#eval`'d. The pure `ExitCode` API is checked with `#guard`.
-/
import Linen.System.Exit

open System

namespace Tests.System.Exit

/-! ### numeric representation + success predicate -/

#guard ExitCode.success.toUInt32 == 0
#guard (ExitCode.failure 42).toUInt32 == 42
#guard ExitCode.success.isSuccess == true
#guard (ExitCode.failure 1).isSuccess == false

/-! ### BEq / ToString -/

#guard ExitCode.success == ExitCode.success
#guard (ExitCode.failure 7) == (ExitCode.failure 7)
#guard (ExitCode.success == ExitCode.failure 0) == false
#guard (ExitCode.failure 1 == ExitCode.failure 2) == false
#guard toString ExitCode.success == "ExitSuccess"
#guard toString (ExitCode.failure 7) == "ExitFailure(7)"

/-! ### laws (compile-time) -/

example : ExitCode.success.toUInt32 = 0 := ExitCode.success_toUInt32
example (c : ExitCode) : c.isSuccess = true ↔ c = .success := ExitCode.isSuccess_iff c

/-! ### exit actions type-check at any return type (deliberately never run) -/

example : ExitCode → IO Unit := exitWith
example : IO Unit := exitSuccess
example : IO Unit := exitFailure

end Tests.System.Exit
