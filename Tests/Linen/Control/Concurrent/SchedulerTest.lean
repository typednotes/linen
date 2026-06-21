/-
  Tests for `Linen.Control.Concurrent.Scheduler`.

  `PosNat` instances are checked with `#guard`; scheduling is IO/concurrent, so
  it is checked with `#eval` (a thrown error fails the build) using an `IO.Ref`
  to observe whether the scheduled action ran.
-/
import Linen.Control.Concurrent.Scheduler

open Control.Concurrent.Scheduler

namespace Tests.Control.Concurrent.Scheduler

-- PosNat: equality compares the underlying value; ToString shows it.
#guard ((⟨1, by decide⟩ : PosNat) == ⟨1, by decide⟩)
#guard !((⟨1, by decide⟩ : PosNat) == ⟨2, by decide⟩)
#guard toString (⟨3, by decide⟩ : PosNat) == "3"

-- A scheduled thread runs and resolves to `.ok`.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let ref ← IO.mkRef (0 : Nat)
  let thread : GreenThread := { id := ⟨1, by decide⟩, action := do ref.set 42, token := tok }
  match ← IO.wait (← schedule thread) with
  | .ok () => pure ()
  | .error e => throw (IO.userError s!"schedule errored: {e}")
  unless (← ref.get) == 42 do throw (IO.userError "scheduled action did not run")

-- A cancelled thread resolves to `.error` and its action does not run.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  tok.cancel
  let ref ← IO.mkRef (0 : Nat)
  let thread : GreenThread := { id := ⟨2, by decide⟩, action := do ref.set 99, token := tok }
  match ← IO.wait (← schedule thread) with
  | .ok () => throw (IO.userError "cancelled thread should not succeed")
  | .error _ => pure ()
  unless (← ref.get) == 0 do throw (IO.userError "cancelled action should not have run")

end Tests.Control.Concurrent.Scheduler
