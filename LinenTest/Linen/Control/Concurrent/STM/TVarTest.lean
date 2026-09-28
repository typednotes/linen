/-
  Tests for `Linen.Control.Concurrent.STM.TVar`.
-/
import Linen.Control.Concurrent.STM.TVar

open Control.Monad
open Control.Concurrent.STM

namespace Tests.Control.Concurrent.STM.TVar

-- newTVarIO / readTVar / writeTVar round trip through atomically.
#eval show IO Unit from do
  let tv ← TVar.newTVarIO (10 : Nat)
  let v ← atomically (TVar.readTVar tv)
  unless v == 10 do throw (IO.userError s!"readTVar expected 10, got {v}")
  atomically (TVar.writeTVar tv 20)
  let v' ← atomically (TVar.readTVar tv)
  unless v' == 20 do throw (IO.userError s!"readTVar after write expected 20, got {v'}")

-- modifyTVar' applies the function to the current value.
#eval show IO Unit from do
  let tv ← TVar.newTVarIO (5 : Nat)
  atomically (TVar.modifyTVar' tv (· * 3))
  let v ← atomically (TVar.readTVar tv)
  unless v == 15 do throw (IO.userError s!"modifyTVar' expected 15, got {v}")

-- newTVar inside a transaction produces an independently readable/writable ref.
#eval show IO Unit from do
  let v ← atomically do
    let tv ← TVar.newTVar (1 : Nat)
    TVar.writeTVar tv 2
    TVar.readTVar tv
  unless v == 2 do throw (IO.userError s!"newTVar-then-write expected 2, got {v}")

end Tests.Control.Concurrent.STM.TVar
