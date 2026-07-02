/-
  Tests for `Linen.System.TimeManager`.

  The sweeper runs on a background task on a real clock, so behaviour is
  checked with `#eval` against short (millisecond-scale) intervals rather
  than `#guard`. `HandleState`'s derived `BEq` is checked directly.
-/
import Linen.System.TimeManager

open System.TimeManager

namespace Tests.System.TimeManager

-- An untouched handle times out once its deadline elapses.
-- (50ms deadline/sweep interval, 200ms wait = 4x margin.)
#eval do
  let mgr ← Manager.new (timeoutUs := 50000)
  let fired ← IO.mkRef false
  let _h ← mgr.register (fired.set true)
  IO.sleep 200
  mgr.stop
  unless (← fired.get) do
    throw (IO.userError "expected onTimeout to fire for an untouched handle")

-- Tickling a handle well within its deadline keeps postponing it forever.
#eval do
  let mgr ← Manager.new (timeoutUs := 50000)
  let fired ← IO.mkRef false
  let h ← mgr.register (fired.set true)
  for _ in [0:6] do
    IO.sleep 15
    h.tickle mgr
  mgr.stop
  unless !(← fired.get) do
    throw (IO.userError "expected a tickled handle not to time out")

-- Canceling a handle prevents its callback from ever firing.
#eval do
  let mgr ← Manager.new (timeoutUs := 50000)
  let fired ← IO.mkRef false
  let h ← mgr.register (fired.set true)
  h.cancel
  IO.sleep 200
  mgr.stop
  unless !(← fired.get) do
    throw (IO.userError "expected a canceled handle not to time out")

-- A paused handle is exempt from timeout until resumed.
#eval do
  let mgr ← Manager.new (timeoutUs := 50000)
  let fired ← IO.mkRef false
  let h ← mgr.register (fired.set true)
  h.pause
  IO.sleep 200
  let firedWhilePaused ← fired.get
  h.resume mgr
  IO.sleep 200
  mgr.stop
  unless !firedWhilePaused && (← fired.get) do
    throw (IO.userError "expected pause to suppress timeout and resume to re-enable it")

-- `HandleState`'s derived `BEq`.
#guard HandleState.active 5 == HandleState.active 5
#guard HandleState.active 5 != HandleState.active 6
#guard HandleState.paused != HandleState.canceled

end Tests.System.TimeManager
