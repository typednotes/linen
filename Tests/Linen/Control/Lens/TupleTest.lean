/-
  Tests for `Linen.Control.Lens.Tuple`.
-/
import Linen.Control.Lens.Tuple

open Control.Lens

namespace Tests.Linen.Control.Lens.Tuple

-- ── `_1` — uniform across every arity ───────────

#guard ((1, 2) ^. _1) = 1
#guard ((1, 2, 3) ^. _1) = 1
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _1) = 1
#guard (_1 .~ "hi") (1, 2) = ("hi", 2)
#guard (over _1 (· + 41)) (1, 2, 3, 4, 5) = (42, 2, 3, 4, 5)

-- ── `_2` — base case (bare pair) vs. recursive ──

#guard ((1, 2) ^. _2) = 2
#guard ((1, 2, 3) ^. _2) = 2
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _2) = 2
#guard (_2 .~ "hi") (1, 2) = (1, "hi")
#guard (_2 .~ "hi") (1, 2, 3) = (1, "hi", 3)

-- ── `_3` .. `_9` — one position each, up to a 9-tuple ─

#guard ((1, 2, 3) ^. _3) = 3
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _3) = 3
#guard ((1, 2, 3, 4) ^. _4) = 4
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _4) = 4
#guard ((1, 2, 3, 4, 5) ^. _5) = 5
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _5) = 5
#guard ((1, 2, 3, 4, 5, 6) ^. _6) = 6
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _6) = 6
#guard ((1, 2, 3, 4, 5, 6, 7) ^. _7) = 7
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _7) = 7
#guard ((1, 2, 3, 4, 5, 6, 7, 8) ^. _8) = 8
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _8) = 8
#guard ((1, 2, 3, 4, 5, 6, 7, 8, 9) ^. _9) = 9

-- `_9 .~` rewrites only the last field of a 9-tuple, leaving the rest alone.
-- Stated as `example ... := rfl` rather than `#guard`, since `#guard`'s
-- automatic `Decidable`-to-`Bool` coercion doesn't find a `DecidableEq`
-- instance for this large a nested `Prod`, even though the equality holds
-- by `rfl` outright (per `AGENTS.md`'s guidance for such cases).
example :
    (_9 .~ (900 : Nat)) (1, 2, 3, 4, 5, 6, 7, 8, 9) = (1, 2, 3, 4, 5, 6, 7, 8, 900) := rfl

-- Reading and writing the same position round-trips (the lens laws hold,
-- since every instance here is a direct `Prod.fst`/`Prod.snd` projection).
example (p : Nat × Nat) : set _1 (view _1 p) p = p := by cases p; rfl
example (p : Nat × Nat × Nat) : set _2 (view _2 p) p = p := by obtain ⟨a, b, c⟩ := p; rfl

end Tests.Linen.Control.Lens.Tuple
