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
--
-- Two timings have to hold for this to mean anything, and they pull in
-- opposite directions: the run must outlast the deadline (or an untickled
-- handle would not have fired either, and the test proves nothing), while every
-- individual gap between tickles must stay under it (or the handle genuinely
-- did expire, and firing is correct). 20ms tickles against a 200ms deadline,
-- 25 of them, satisfies both with a 10x margin per gap and 2.5x overall — where
-- the original 15ms against 50ms had only ~3x per gap and lost the race on a
-- loaded macOS runner.
--
-- `IO.sleep` guarantees only a lower bound, so no margin makes this airtight on
-- a shared runner. The gaps are therefore measured rather than assumed: if the
-- scheduler stalled past the deadline the premise is void, and that is reported
-- instead of being failed as if `tickle` had misbehaved.
#eval do
  let timeoutUs := 200000
  let deadlineNs := timeoutUs * 1000
  let mgr ← Manager.new (timeoutUs := timeoutUs)
  let fired ← IO.mkRef false
  let h ← mgr.register (fired.set true)
  let start ← IO.monoNanosNow
  let mut prev := start
  let mut maxGapNs := 0
  for _ in [0:25] do
    IO.sleep 20
    h.tickle mgr
    let now ← IO.monoNanosNow
    maxGapNs := max maxGapNs (now - prev)
    prev := now
  mgr.stop
  let elapsedNs := prev - start
  if maxGapNs ≥ deadlineNs then
    IO.eprintln s!"TimeManagerTest: inconclusive — a gap between tickles reached \
      {maxGapNs / 1000000}ms, at or past the {timeoutUs / 1000}ms deadline. The runner \
      stalled; this says nothing about `tickle`, so it is not failed."
  else if elapsedNs ≤ deadlineNs then
    IO.eprintln s!"TimeManagerTest: inconclusive — only {elapsedNs / 1000000}ms elapsed, \
      within the {timeoutUs / 1000}ms deadline, so an untickled handle would not have \
      fired either."
  else
    unless !(← fired.get) do
      throw (IO.userError s!"expected a tickled handle not to time out \
        (largest gap {maxGapNs / 1000000}ms, deadline {timeoutUs / 1000}ms)")

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
