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

end Tests.Control.Concurrent.MVar
