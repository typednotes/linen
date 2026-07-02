/-
  Tests for `Linen.Control.Concurrent.MVar`.

  The operations are IO/concurrent, so behaviour is checked with `#eval` (a
  thrown error fails the build). Only the non-blocking fast paths are exercised
  — a blocking `take` on an empty MVar (or `put` on a full one) would deadlock a
  single-threaded test.
-/
import Linen.Control.Concurrent.MVar

open Control.Concurrent

namespace Tests.Control.Concurrent.MVar

-- Full MVar: read keeps the value, take empties it, try* reflect emptiness.
#eval show IO Unit from do
  let mv ← MVar.new (42 : Nat)
  let r ← mv.tryRead
  unless r == some 42 do throw (IO.userError s!"tryRead expected some 42, got {r}")
  let v ← mv.takeSync
  unless v == 42 do throw (IO.userError s!"takeSync expected 42, got {v}")
  unless (← mv.isEmpty) do throw (IO.userError "expected empty after take")
  let t ← mv.tryTake
  unless t == none do throw (IO.userError s!"tryTake on empty expected none, got {t}")

-- Empty MVar: tryPut fills it, a second tryPut fails, swap returns the old value.
#eval show IO Unit from do
  let mv ← MVar.newEmpty Nat
  unless (← mv.tryTake) == none do throw (IO.userError "fresh empty should yield none")
  unless (← mv.tryPut 7) do throw (IO.userError "tryPut into empty should succeed")
  unless !(← mv.tryPut 9) do throw (IO.userError "tryPut into full should fail")
  let old ← IO.wait (← mv.swap 100)
  unless old == 7 do throw (IO.userError s!"swap expected old 7, got {old}")
  unless (← mv.tryRead) == some 100 do throw (IO.userError "swap should leave 100")

-- modify_ updates the contents in place.
#eval show IO Unit from do
  let mv ← MVar.new (10 : Nat)
  let _ ← IO.wait (← mv.modify_ (fun a => pure (a + 1)))
  unless (← mv.tryRead) == some 11 do throw (IO.userError "modify_ should yield 11")

-- read on an empty MVar blocks, is woken by a put, and does not consume the
-- value: it is still there for a subsequent read/take.
#eval show IO Unit from do
  let mv ← MVar.newEmpty Nat
  let readTask ← mv.read
  let _ ← mv.put 5
  let v ← IO.wait readTask
  unless v == 5 do throw (IO.userError s!"read expected 5, got {v}")
  unless (← mv.tryRead) == some 5 do throw (IO.userError "read must not remove the value")
  let t ← mv.takeSync
  unless t == 5 do throw (IO.userError s!"take after read expected 5, got {t}")

-- Multiple readers queued on an empty MVar are all woken by the same put
-- (multi-wakeup, matching GHC's `readMVar`), and the value remains after.
#eval show IO Unit from do
  let mv ← MVar.newEmpty Nat
  let r1 ← mv.read
  let r2 ← mv.read
  let _ ← mv.put 99
  let v1 ← IO.wait r1
  let v2 ← IO.wait r2
  unless v1 == 99 && v2 == 99 do
    throw (IO.userError s!"expected both readers to get 99, got {v1}, {v2}")
  unless (← mv.tryRead) == some 99 do
    throw (IO.userError "value should remain in the box after multi-reader wakeup")

end Tests.Control.Concurrent.MVar
