/-
  Tests for `Linen.Control.Concurrent.STM.TQueue`.
-/
import Linen.Control.Concurrent.STM.TQueue

open Control.Monad
open Control.Concurrent.STM

namespace Tests.Control.Concurrent.STM.TQueue

-- FIFO order is preserved across the two-list representation, including
-- after the read end is exhausted and the write end must be reversed.
#eval show IO Unit from do
  let q ← TQueue.newTQueueIO (α := Nat)
  atomically do
    TQueue.writeTQueue q 1
    TQueue.writeTQueue q 2
    TQueue.writeTQueue q 3
  let a ← atomically (TQueue.readTQueue q)
  unless a == 1 do throw (IO.userError s!"readTQueue expected 1, got {a}")
  atomically (TQueue.writeTQueue q 4)
  let rest ← atomically do
    let b ← TQueue.readTQueue q
    let c ← TQueue.readTQueue q
    let d ← TQueue.readTQueue q
    pure [b, c, d]
  unless rest == [2, 3, 4] do throw (IO.userError s!"readTQueue order expected [2,3,4], got {rest}")

-- isEmptyTQueue/tryReadTQueue reflect emptiness without blocking.
#eval show IO Unit from do
  let q ← TQueue.newTQueueIO (α := Nat)
  unless (← atomically (TQueue.isEmptyTQueue q)) do throw (IO.userError "fresh queue should be empty")
  unless (← atomically (TQueue.tryReadTQueue q)) == none do
    throw (IO.userError "tryReadTQueue on empty should yield none")
  atomically (TQueue.writeTQueue q 5)
  unless !(← atomically (TQueue.isEmptyTQueue q)) do throw (IO.userError "queue with an item should not be empty")
  let v ← atomically (TQueue.tryReadTQueue q)
  unless v == some 5 do throw (IO.userError s!"tryReadTQueue expected some 5, got {v}")

-- peekTQueue returns the front without removing it.
#eval show IO Unit from do
  let q ← TQueue.newTQueueIO (α := Nat)
  atomically (TQueue.writeTQueue q 7)
  let peeked ← atomically (TQueue.peekTQueue q)
  unless peeked == 7 do throw (IO.userError s!"peekTQueue expected 7, got {peeked}")
  let read ← atomically (TQueue.readTQueue q)
  unless read == 7 do throw (IO.userError s!"readTQueue after peek expected 7, got {read}")

end Tests.Control.Concurrent.STM.TQueue
