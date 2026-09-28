/-
  Tests for `Linen.Control.Concurrent.Chan`.

  Channel operations are IO/concurrent, so behaviour is checked with `#eval` (a
  thrown error fails the build). Reads only happen when the buffer is known to
  be non-empty — reading an empty channel would block the single-threaded test.
-/
import Linen.Control.Concurrent.Chan

open Control.Concurrent

namespace Tests.Control.Concurrent.Chan

-- FIFO: writes are read back in order, then the channel drains.
#eval show IO Unit from do
  let ch ← Chan.new Nat
  unless (← ch.tryRead) == none do throw (IO.userError "fresh channel should be empty")
  ch.write 1
  ch.write 2
  let a ← IO.wait (← ch.read)
  unless a == 1 do throw (IO.userError s!"first read expected 1, got {a}")
  let b ← IO.wait (← ch.read)
  unless b == 2 do throw (IO.userError s!"second read expected 2, got {b}")
  unless (← ch.tryRead) == none do throw (IO.userError "channel should be drained")

-- dup: the copy receives only writes made after the dup; both share future writes.
#eval show IO Unit from do
  let ch ← Chan.new Nat
  ch.write 1                      -- before dup → original only
  let ch2 ← ch.dup
  ch.write 2                      -- after dup → both endpoints
  unless (← IO.wait (← ch.read)) == 1 do throw (IO.userError "original should see 1 first")
  unless (← IO.wait (← ch.read)) == 2 do throw (IO.userError "original should see 2 next")
  unless (← ch2.tryRead) == some 2 do throw (IO.userError "dup should see only the post-dup 2")
  unless (← ch2.tryRead) == none do throw (IO.userError "dup should then be empty")

end Tests.Control.Concurrent.Chan
