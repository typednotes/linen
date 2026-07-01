/-
  Tests for `Linen.Control.Monad.Reader`.

  Covers the Haskell `mtl` names built on Lean's `ReaderT`/`read`/`adapt`:
  `ask`, `asks`, `local`, `runReaderT`, `runReader`, `mapReaderT`.
-/
import Linen.Control.Monad.Reader

open Control.Monad.Reader

namespace Tests.Control.Monad.Reader

-- `ask` returns the environment as-is.
#guard runReader (ask : Reader Nat Nat) 5 == 5

-- `asks` projects a function over the environment.
#guard runReader (asks (· * 2) : Reader Nat Nat) 5 == 10

-- `local` runs a computation against a transformed environment.
#guard runReader («local» (· + 1) (ask : Reader Nat Nat)) 5 == 6

-- `local` does not affect what the outer environment sees afterwards.
#guard runReader (do let a ← «local» (· + 1) (ask : Reader Nat Nat); let b ← ask; pure (a, b)) 5 == (6, 5)

-- `runReaderT`/`runReader` thread a pure value straight through.
#guard runReader (pure 3 : Reader Nat Nat) 0 == 3
#guard Id.run (runReaderT (pure 3 : ReaderT Nat Id Nat) 0) == 3

-- `mapReaderT` transforms the underlying computation.
#guard runReader (mapReaderT (fun (a : Id Nat) => (a.run + 1 : Id Nat)) (ask : Reader Nat Nat)) 5 == 6

-- Reduction laws (checked at compile time).
example (env : Nat) : runReaderT (ask : ReaderT Nat Id Nat) env = pure env := ask_run env
example (ma : ReaderT Nat Id Nat) : («local» id ma) = ma := local_id ma
example (a : Nat) (env : Nat) : runReader (pure a : Reader Nat Nat) env = a := runReader_pure a env

end Tests.Control.Monad.Reader
