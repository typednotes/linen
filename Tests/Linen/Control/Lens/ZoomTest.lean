/-
  Tests for `Linen.Control.Lens.Zoom`.
-/
import Linen.Control.Lens.Zoom
import Linen.Control.Lens.Tuple
import Linen.Control.Monad.State
import Linen.Control.Monad.Reader

open Control.Lens
open Control.Monad.State
open Control.Monad.Reader

namespace Tests.Linen.Control.Lens.Zoom

-- ── zoom ────────────────────────────────────────

-- `zoom _1 get` reads only the first component of a pair-shaped state.
#guard runState (zoom _1 (get : State Nat Nat)) (1, 2) = (1, (1, 2))

-- `zoom _1 (put 42)` writes only the first component, leaving the rest.
#guard execState (zoom _1 (put 42 : State Nat Unit)) (1, 2) = (42, 2)

-- `zoom _2` focuses the second component instead.
#guard execState (zoom _2 (put 99 : State Nat Unit)) (1, 2) = (1, 99)

-- `zoom` composes with ordinary `StateT` combinators run through the lens.
#guard execState (zoom _1 (modify (· + 1) : State Nat Unit)) (1, 2) = (2, 2)

-- Nested `zoom`: zooming twice, through two layers of tuple, reaches the
-- innermost component of a doubly-nested state.
#guard execState (zoom _1 (zoom _1 (put 7 : State Nat Unit) : State (Nat × Nat) Unit))
    ((1, 2), 3) = ((7, 2), 3)

-- ── magnify ─────────────────────────────────────

-- `magnify _1 ask` reads only the first component of a pair-shaped
-- environment.
#guard runReader (magnify _1 (ask : Reader Nat Nat)) (1, 2) = 1

#guard runReader (magnify _2 (ask : Reader Nat Nat)) (1, 2) = 2

-- `magnify` also works with an ordinary projection run inside the smaller
-- `Reader`.
#guard runReader (magnify _1 (asks (· + 100) : Reader Nat Nat)) (1, 2) = 101

end Tests.Linen.Control.Lens.Zoom
