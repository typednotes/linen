/-
  Tests for `Linen.Control.Monad.IO.Unlift`.

  Mirrors Hale's own `Tests/UnliftIO/TestUnliftIO.lean` coverage: the `IO`
  instance, the `ReaderT r IO` instance (environment capture/preservation),
  and `toIO`. All are IO-effectful, so checked with `#eval`.
-/
import Linen.Control.Monad.IO.Unlift

open Control.Monad.IO

namespace Tests.Control.Monad.IO.Unlift

-- The `IO` instance runs the action directly.
#eval show IO Unit from do
  let v ← MonadUnliftIO.withRunInIO (m := IO) fun run => run Nat (pure 42)
  unless v == 42 do
    throw (IO.userError s!"expected 42, got {v}")

-- The `ReaderT r IO` instance captures the environment for `run`.
#eval show IO Unit from do
  let v ← (do
    MonadUnliftIO.withRunInIO (m := ReaderT Nat IO) fun run =>
      run Nat (do let env ← read; pure env)
    : ReaderT Nat IO Nat).run 99
  unless v == 99 do
    throw (IO.userError s!"expected 99, got {v}")

-- The captured environment is preserved across the unlift/run roundtrip.
#eval show IO Unit from do
  let v ← (do
    MonadUnliftIO.withRunInIO (m := ReaderT String IO) fun run =>
      run String read
    : ReaderT String IO String).run "hello"
  unless v == "hello" do
    throw (IO.userError s!"expected \"hello\", got {v}")

-- `toIO` reifies an `m`-action as a plain `IO` action.
#eval show IO Unit from do
  let ioAct ← (MonadUnliftIO.toIO (m := ReaderT Nat IO) (do
    let n ← read
    pure (n + 1)) : ReaderT Nat IO (IO Nat)).run 10
  let v ← ioAct
  unless v == 11 do
    throw (IO.userError s!"expected 11, got {v}")

end Tests.Control.Monad.IO.Unlift
