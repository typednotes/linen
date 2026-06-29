/-
  Linen.System.Exit — exit codes and process termination

  Haskell's `System.Exit`: a closed `ExitCode` sum plus the
  `exitWith`/`exitSuccess`/`exitFailure` actions, wrapping core
  `IO.Process.exit`. Core has no `ExitCode` type, so it is ported here.

  Note: `IO.Process.exit` takes a `UInt8`, so exit codes above 255 are
  truncated via `UInt32.toUInt8`.
-/

namespace System

/-- Exit codes for process termination.

    $$\text{ExitCode} ::= \text{success} \mid \text{failure}(n : \mathbb{N}_{32})$$ -/
inductive ExitCode where
  /-- Successful termination (code 0). -/
  | success : ExitCode
  /-- Failure with an exit code. -/
  | failure : UInt32 → ExitCode
  deriving BEq, Repr

namespace ExitCode

instance : ToString ExitCode where
  toString
    | .success   => "ExitSuccess"
    | .failure n => s!"ExitFailure({n})"

/-- Numeric representation: $\text{success} \mapsto 0$, $\text{failure}(n) \mapsto n$. -/
@[inline] def toUInt32 : ExitCode → UInt32
  | .success   => 0
  | .failure n => n

/-- Test whether the code represents success. -/
@[inline] def isSuccess : ExitCode → Bool
  | .success   => true
  | .failure _ => false

/-! ── Laws ── -/

/-- Success has code 0. -/
theorem success_toUInt32 : ExitCode.success.toUInt32 = 0 := rfl

/-- `isSuccess` is true only for `success`. -/
theorem isSuccess_iff (c : ExitCode) : c.isSuccess = true ↔ c = .success := by
  cases c with
  | success => simp [isSuccess]
  | failure n => simp [isSuccess]

end ExitCode

/-! ── Termination actions ── -/

/-- Exit the process with the given code (wraps core `IO.Process.exit`).
    Codes above 255 are truncated to a `UInt8`.

    $$\text{exitWith} : \text{ExitCode} \to \text{IO}\ \alpha$$

    The polymorphic return type `IO α` encodes non-return at the type level. -/
def exitWith (code : ExitCode) : IO α :=
  match code with
  | .success   => IO.Process.exit 0
  | .failure n => IO.Process.exit n.toUInt8

/-- Exit successfully (code 0). -/
def exitSuccess : IO α := exitWith .success

/-- Exit with failure code 1. -/
def exitFailure : IO α := exitWith (.failure 1)

end System
