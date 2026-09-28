/-
  Tests for `Data.Conduit` (the `ConduitT` wrapper and its operations).

  `ConduitT` is `unsafe` (see the module docstring), so its values can't
  appear in a kernel-checked `#guard`. Instead every check runs inside
  `#eval show IO Unit from do ...`, matching this codebase's convention for
  effectful/non-reducible tests (e.g. `MVarTest`, `AuthTest`).
-/
import Linen.Data.Conduit.Internal.Conduit

open Data.Conduit
open Control.Monad.Trans.Resource

namespace Tests.Data.Conduit

/-! ### `await` / `yield` / fusion, run over `Id` -/

private unsafe def echoTwice : ConduitT Nat Nat Id Unit := do
  match ← await with
  | none => pure ()
  | some n => do yield n; yield n; echoTwice

private unsafe def sinkListId : ConduitT Nat PEmpty Id (List Nat) := do
  match ← await with
  | none => pure []
  | some n => do let rest ← sinkListId; pure (n :: rest)

#eval show IO Unit from do
  let src : ConduitT PEmpty Nat Id Unit := do yield 1; yield 2; yield 3
  let result := runConduitPure (src .| echoTwice .| sinkListId)
  unless result == [1, 1, 2, 2, 3, 3] do
    throw (IO.userError s!"expected [1,1,2,2,3,3], got {result}")

/-! ### `leftoverC` re-delivers a pushed-back value -/

#eval show IO Unit from do
  let c : ConduitT Nat PEmpty Id (Option Nat × Option Nat) := do
    let a ← await
    match a with
    | none => pure (none, none)
    | some v => do
      leftoverC v
      let b ← await
      pure (a, b)
  let src : ConduitT PEmpty Nat Id Unit := yield 42
  let result := runConduitPure (src .| c)
  unless result == (some 42, some 42) do
    throw (IO.userError s!"leftoverC should redeliver the same value, got {result}")

/-! ### `liftConduit` / `MonadLift` runs an effect from the base monad -/

#eval show IO Unit from do
  let log ← IO.mkRef (#[] : Array Nat)
  let c : ConduitT PEmpty PEmpty IO Unit := do
    liftConduit (log.modify (·.push 1))
    liftConduit (log.modify (·.push 2))
  runConduit c
  unless (← log.get) == #[1, 2] do
    throw (IO.userError "liftConduit/MonadLift should run the underlying IO effect")

/-! ### `runConduitRes` releases resources acquired via `bracketP` -/

#eval show IO Unit from do
  let log ← IO.mkRef (#[] : Array String)
  let c : ConduitT PEmpty PEmpty (ResourceT IO) Unit :=
    bracketP (pure "handle") (fun _ => log.modify (·.push "closed"))
      (fun h => liftConduit (log.modify (·.push s!"used {h}")))
  runConduitRes c
  unless (← log.get) == #["used handle", "closed"] do
    throw (IO.userError s!"bracketP should use then close, got {← log.get}")

end Tests.Data.Conduit
