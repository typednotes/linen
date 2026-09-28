/-
  Tests for `Linen.Control.Concurrent.STM.TMVar`.

  Only the non-blocking fast paths (`tryTakeTMVar`/`tryPutTMVar`) exercise the
  empty/full corners directly — a blocking `takeTMVar` on an empty TMVar (or
  `putTMVar` on a full one) would retry forever in a single-threaded test.
-/
import Linen.Control.Concurrent.STM.TMVar

open Control.Monad
open Control.Concurrent.STM

namespace Tests.Control.Concurrent.STM.TMVar

-- Full TMVar: readTMVar keeps the value, takeTMVar empties it.
#eval show IO Unit from do
  let tv ← TMVar.newTMVarIO (42 : Nat)
  let r ← atomically (TMVar.readTMVar tv)
  unless r == 42 do throw (IO.userError s!"readTMVar expected 42, got {r}")
  let v ← atomically (TMVar.takeTMVar tv)
  unless v == 42 do throw (IO.userError s!"takeTMVar expected 42, got {v}")
  let empty ← atomically (TMVar.isEmptyTMVar tv)
  unless empty do throw (IO.userError "expected empty after take")
  let t ← atomically (TMVar.tryTakeTMVar tv)
  unless t == none do throw (IO.userError s!"tryTakeTMVar on empty expected none, got {t}")

-- Empty TMVar: tryPutTMVar fills it, a second tryPutTMVar fails.
#eval show IO Unit from do
  let tv ← TMVar.newEmptyTMVarIO (α := Nat)
  unless (← atomically (TMVar.isEmptyTMVar tv)) do throw (IO.userError "fresh TMVar should be empty")
  unless (← atomically (TMVar.tryPutTMVar tv 7)) do
    throw (IO.userError "tryPutTMVar into empty should succeed")
  unless !(← atomically (TMVar.tryPutTMVar tv 9)) do
    throw (IO.userError "tryPutTMVar into full should fail")
  unless (← atomically (TMVar.readTMVar tv)) == 7 do
    throw (IO.userError "readTMVar should see the value from tryPutTMVar")

end Tests.Control.Concurrent.STM.TMVar
