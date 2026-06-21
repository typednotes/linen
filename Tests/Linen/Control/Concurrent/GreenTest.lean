/-
  Tests for `Linen.Control.Concurrent.Green`.

  Green computations are IO/concurrent, so behaviour is checked with `#eval`
  (a thrown error fails the build) via `Green.block`. Awaited tasks are already
  resolved (or resolved by prior operations) so the single-threaded test never
  deadlocks.
-/
import Linen.Control.Concurrent.Green

open Control.Concurrent Control.Concurrent.Green

namespace Tests.Control.Concurrent.Green

-- pure, bind, and awaiting an already-resolved task.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let r ← Green.block (pure 42 : Green Nat) tok
  unless r == 42 do throw (IO.userError s!"pure expected 42, got {r}")
  let r2 ← Green.block (do
      let a ← pure (1 : Nat)
      let b ← Green.await (Task.pure 2)
      pure (a + b)) tok
  unless r2 == 3 do throw (IO.userError s!"bind/await expected 3, got {r2}")

-- error propagation through `block`.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let threw ← try
      let _ ← Green.block (throw (IO.userError "boom") : Green Nat) tok
      pure false
    catch _ =>
      pure true
  unless threw do throw (IO.userError "Green.throw should surface through block")

-- MVar / QSem integration on fast paths.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let mv ← MVar.new (5 : Nat)
  let v ← Green.block (Green.takeMVar mv) tok
  unless v == 5 do throw (IO.userError s!"takeMVar expected 5, got {v}")
  let sem ← QSem.new 1
  Green.block (Green.waitSem sem) tok       -- acquires the available unit

-- cooperative cancellation: checkCancelled throws on a cancelled token.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  tok.cancel
  let threw ← try
      Green.block Green.checkCancelled tok
      pure false
    catch _ =>
      pure true
  unless threw do throw (IO.userError "checkCancelled should throw on a cancelled token")

end Tests.Control.Concurrent.Green
