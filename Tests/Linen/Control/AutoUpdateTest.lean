/-
  Tests for `Linen.Control.AutoUpdate`.

  The cached value is computed eagerly, so a read immediately after
  `mkAutoUpdate` is deterministic and needs no timing assumptions. IO behaviour
  is checked with `#eval` (a thrown error fails the build); the pure default is
  checked with `#guard`.
-/
import Linen.Control.AutoUpdate

open Control

namespace Tests.Control.AutoUpdate

-- `UpdateSettings.default` uses a 1-second (1_000_000 μs) interval.
#guard (UpdateSettings.default (pure (0 : Nat))).updateFreq == 1000000

-- The getter returns the eagerly-computed initial value, and `stop` runs cleanly.
#eval do
  let au ← mkAutoUpdate (UpdateSettings.default (pure (42 : Nat)))
  let v ← au.get
  au.stop
  unless v == 42 do
    throw (IO.userError s!"expected initial cached value 42, got {v}")

-- A custom interval is preserved, and reads are non-blocking/repeatable.
#eval do
  let au ← mkAutoUpdate { updateFreq := 50000, updateAction := pure (7 : Nat) }
  let a ← au.get
  let b ← au.get
  au.stop
  unless a == 7 && b == 7 do
    throw (IO.userError s!"expected stable value 7, got {a}, {b}")

end Tests.Control.AutoUpdate
