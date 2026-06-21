/-
  Tests for `Linen.Control.Concurrent`.

  Thread operations are IO/concurrent, so behaviour is checked with `#eval` (a
  thrown error fails the build). `waitThread` joins each forked thread, making
  the observed outcomes deterministic.
-/
import Linen.Control.Concurrent

open Control.Concurrent

namespace Tests.Control.Concurrent

-- forkIO runs the action; waitThread joins it.
#eval show IO Unit from do
  let ref ← IO.mkRef (0 : Nat)
  let tid ← forkIO do ref.set 7
  waitThread tid
  unless (← ref.get) == 7 do throw (IO.userError "forkIO action did not run")
  -- ThreadId carries a positive id and renders as ThreadId(n)
  unless tid.id.val ≥ 1 do throw (IO.userError "thread id should be ≥ 1")
  unless (toString tid).startsWith "ThreadId(" do throw (IO.userError s!"bad ThreadId render: {tid}")

-- forkFinally runs the finaliser with the outcome (.ok and .error).
#eval show IO Unit from do
  let ref ← IO.mkRef (0 : Nat)
  let tid ← forkFinally (pure (10 : Nat)) fun
    | .ok n   => ref.set n
    | .error _ => ref.set 999
  waitThread tid
  unless (← ref.get) == 10 do throw (IO.userError "forkFinally .ok finaliser did not run")

  let ref2 ← IO.mkRef (0 : Nat)
  let tid2 ← forkFinally (throw (IO.userError "boom") : IO Nat) fun
    | .ok _   => ref2.set 1
    | .error _ => ref2.set 2
  waitThread tid2
  unless (← ref2.get) == 2 do throw (IO.userError "forkFinally .error finaliser did not run")

-- killThread, threadDelay, and yield run without error.
#eval show IO Unit from do
  let tid ← forkIO (pure ())
  killThread tid
  threadDelay 0
  yield
  -- joining a killed thread may surface the cooperative cancellation; tolerate it
  try waitThread tid catch _ => pure ()

-- forkGreen schedules a fair green thread that can be joined with waitThread.
#eval show IO Unit from do
  let tid ← forkGreen (pure () : Green.Green Unit)
  waitThread tid

end Tests.Control.Concurrent
