/-
  Tests for `Control.Monad.Trans.Resource`.

  `runResourceT` is IO-effecting (it schedules cleanup), so behaviour is
  checked with `#eval` (a thrown error fails the build).
-/
import Linen.Control.Monad.Trans.Resource

open Control.Monad.Trans.Resource

namespace Tests.Control.Monad.Trans.Resource

-- A resource is released exactly once, after the `ResourceT` block completes.
#eval show IO Unit from do
  let log ← IO.mkRef (#[] : Array String)
  runResourceT do
    let (_, a) ← allocate (pure (1 : Nat)) (fun _ => log.modify (·.push "release"))
    unless a == 1 do throw (IO.userError "allocate should hand back the acquired value")
    log.modify (·.push "use")
  let trace ← log.get
  unless trace == #["use", "release"] do
    throw (IO.userError s!"expected [use, release], got {trace}")

-- Cleanup runs even when the `ResourceT` block throws.
#eval show IO Unit from do
  let log ← IO.mkRef (#[] : Array String)
  let threw ← try
      runResourceT do
        let _ ← allocate (pure ()) (fun _ => log.modify (·.push "release"))
        liftM (throw (IO.userError "boom") : IO Unit)
      pure false
    catch _ => pure true
  unless threw do throw (IO.userError "the exception should propagate")
  unless (← log.get) == #["release"] do
    throw (IO.userError "cleanup must still run when the block throws")

-- Multiple resources are released in LIFO order.
#eval show IO Unit from do
  let log ← IO.mkRef (#[] : Array String)
  runResourceT do
    let _ ← allocate (pure ()) (fun _ => log.modify (·.push "first"))
    let _ ← allocate (pure ()) (fun _ => log.modify (·.push "second"))
    let _ ← allocate (pure ()) (fun _ => log.modify (·.push "third"))
    pure ()
  unless (← log.get) == #["third", "second", "first"] do
    throw (IO.userError "cleanup order should be LIFO")

-- An early `release` runs the cleanup immediately and is a no-op if repeated.
#eval show IO Unit from do
  let log ← IO.mkRef (#[] : Array String)
  runResourceT do
    let (key, _) ← allocate (pure ()) (fun _ => log.modify (·.push "released"))
    release key
    release key  -- second release is a no-op
  unless (← log.get) == #["released"] do
    throw (IO.userError "release should run the cleanup exactly once")

/-! ### `ReleaseKey` equality -/

example (k : ReleaseKey) : k = k ↔ k.id = k.id := releaseKey_eq k k

end Tests.Control.Monad.Trans.Resource
