/-
  Tests for `Linen.Control.Concurrent.QSemN`.

  Semaphore operations are IO/concurrent, so behaviour is checked with `#eval`
  (a thrown error fails the build). Where a `wait` is meant to block, its task
  is only awaited after later `signal`s have released enough units.
-/
import Linen.Control.Concurrent.QSemN

open Control.Concurrent

namespace Tests.Control.Concurrent.QSemN

-- Counting, plus a blocked acquire satisfied by accumulated partial signals.
#eval show IO Unit from do
  let sem ← QSemN.new 5
  IO.wait (← sem.wait 3)    -- 5 → 2
  IO.wait (← sem.wait 2)    -- 2 → 0
  let t ← sem.wait 2        -- count 0 < 2 → pending promise
  sem.signal 1              -- 0 → 1, still < 2, waiter stays
  sem.signal 1              -- 1 → 2, enough → waiter woken, count → 0
  IO.wait t                 -- now resolved, completes

-- withSemN returns the action's result and releases the units afterwards.
#eval show IO Unit from do
  let sem ← QSemN.new 3
  let r ← QSemN.withSemN sem 2 (pure (7 : Nat))
  unless r == 7 do throw (IO.userError s!"withSemN expected 7, got {r}")
  IO.wait (← sem.wait 3)    -- released back to 3, so this completes

-- withSemN releases the units even when the action throws.
#eval show IO Unit from do
  let sem ← QSemN.new 2
  let threw ← try
      QSemN.withSemN sem 2 (throw (IO.userError "boom") : IO Unit)
      pure false
    catch _ =>
      pure true
  unless threw do throw (IO.userError "withSemN should re-raise the action's exception")
  IO.wait (← sem.wait 2)    -- finally released the units, so this completes

end Tests.Control.Concurrent.QSemN
