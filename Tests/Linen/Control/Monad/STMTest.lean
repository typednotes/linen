/-
  Tests for `Linen.Control.Monad.STM`.

  Transactions run via `atomically`, so behaviour is checked with `#eval` (a
  thrown error fails the build).
-/
import Linen.Control.Monad.STM

open Control.Monad

namespace Tests.Control.Monad.STM

-- pure/bind: a transaction that only reads/writes succeeds without retrying.
#eval show IO Unit from do
  let r ← atomically do
    let a ← pure 1
    let b ← pure 2
    pure (a + b : Nat)
  unless r == 3 do throw (IO.userError s!"pure/bind expected 3, got {r}")

-- check succeeds silently when the condition holds.
#eval show IO Unit from do
  let r ← atomically do
    STM.check true
    pure "ok"
  unless r == "ok" do throw (IO.userError s!"check true expected ok, got {r}")

-- orElse: the first branch's retry falls through to the second.
#eval show IO Unit from do
  let r ← atomically (STM.orElse STM.retry (pure (99 : Nat)))
  unless r == 99 do throw (IO.userError s!"orElse expected 99, got {r}")

-- orElse: a successful first branch short-circuits the second.
#eval show IO Unit from do
  let r ← atomically (STM.orElse (pure (1 : Nat)) (pure 2))
  unless r == 1 do throw (IO.userError s!"orElse expected 1, got {r}")

end Tests.Control.Monad.STM
