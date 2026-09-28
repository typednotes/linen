/-
  Tests for `Linen.Control.Concurrent.QSem`.

  Semaphore operations are IO/concurrent, so behaviour is checked with `#eval`
  (a thrown error fails the build). Acquisitions only happen when a unit is
  known to be available — acquiring an exhausted semaphore would block the
  single-threaded test.
-/
import Linen.Control.Concurrent.QSem

open Control.Concurrent

namespace Tests.Control.Concurrent.QSem

-- Counting: `wait` consumes units, `signal` returns one (never blocks here).
#eval show IO Unit from do
  let sem ← QSem.new 2
  IO.wait (← sem.wait)    -- 2 → 1
  IO.wait (← sem.wait)    -- 1 → 0
  sem.signal              -- 0 → 1
  IO.wait (← sem.wait)    -- 1 → 0, completes immediately

-- withSem returns the action's result and releases the unit afterwards.
#eval show IO Unit from do
  let sem ← QSem.new 1
  let r ← QSem.withSem sem (pure (99 : Nat))
  unless r == 99 do throw (IO.userError s!"withSem expected 99, got {r}")
  IO.wait (← sem.wait)    -- released, so this completes (else it would block)

-- withSem releases the unit even when the action throws.
#eval show IO Unit from do
  let sem ← QSem.new 1
  let threw ← try
      QSem.withSem sem (throw (IO.userError "boom") : IO Unit)
      pure false
    catch _ =>
      pure true
  unless threw do throw (IO.userError "withSem should re-raise the action's exception")
  IO.wait (← sem.wait)    -- finally released the unit, so this completes

end Tests.Control.Concurrent.QSem
