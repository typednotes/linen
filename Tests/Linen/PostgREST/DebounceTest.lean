/-
  Tests for `Linen.PostgREST.Debounce`.

  `Debouncer.run` is IO-effectful (it reads the monotonic clock and an
  `IO.Ref`), so it is exercised with `#eval show IO Unit from do ...` — a
  thrown error fails the build.
-/
import Linen.PostgREST.Debounce

open PostgREST.Debounce

namespace Tests.PostgREST.Debounce

#eval show IO Unit from do
  let d ← Debouncer.create 60000
  let calls ← IO.mkRef 0
  -- first call always runs
  d.run (calls.modify (· + 1))
  let n1 ← calls.get
  unless n1 == 1 do throw (IO.userError s!"expected first run to fire, got {n1} calls")
  -- immediate second call is within the window, so it is suppressed
  d.run (calls.modify (· + 1))
  let n2 ← calls.get
  unless n2 == 1 do throw (IO.userError s!"expected debounced call to be suppressed, got {n2} calls")

#eval show IO Unit from do
  -- a zero-interval debouncer never suppresses
  let d ← Debouncer.create 0
  let calls ← IO.mkRef 0
  d.run (calls.modify (· + 1))
  d.run (calls.modify (· + 1))
  let n ← calls.get
  unless n == 2 do throw (IO.userError s!"expected zero-interval debouncer to always fire, got {n} calls")

end Tests.PostgREST.Debounce
