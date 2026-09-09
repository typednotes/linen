/-
  Tests for `Linen.Control.Monad.Effect.Reader`.

  Covers the reader effect over `Eff`: `ask`, `asks`, `runReader`, `withReader`.
-/
import Linen.Control.Monad.Effect.Reader

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.Reader

namespace Tests.Control.Monad.Effect.Reader

-- `ask` returns the environment as-is.
#guard Eff.run (runReader 5 (ask : Eff [Reader Nat] Nat)) == 5

-- `asks` projects a function over the environment.
#guard Eff.run (runReader 5 (asks (· * 2) : Eff [Reader Nat] Nat)) == 10

-- Every `ask` in a computation sees the same environment.
#guard Eff.run (runReader 7 (do
    let a ← ask
    let b ← ask
    pure (a + b) : Eff [Reader Nat] Nat)) == 14

-- The environment need not be numeric.
#guard Eff.run (runReader "hello" (asks String.length : Eff [Reader String] Nat)) == 5

-- `withReader` runs the computation against a transformed environment.
#guard Eff.run (withReader (· + 1) 5 (ask : Eff [Reader Nat] Nat)) == 6

-- A computation that never asks still runs.
#guard Eff.run (runReader 5 (pure 99 : Eff [Reader Nat] Nat)) == 99

-- ── The row bounds what the computation may do ──────────────────────────────

-- `Eff [Reader Config] α` is a whitelist of one effect: this program may read
-- its environment and nothing else — no state, no IO, no filesystem.
abbrev Config := Nat

example : Eff [Reader Config] Nat := do
  let limit ← ask
  pure (limit * 2)

end Tests.Control.Monad.Effect.Reader
