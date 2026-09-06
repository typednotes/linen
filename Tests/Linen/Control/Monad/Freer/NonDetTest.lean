/-
  Tests for `Linen.Control.Monad.Freer.NonDet`.

  Covers `mzero`, `mplus`, `choose`, `select`, `guard`, `makeChoiceA` and
  `makeChoiceFirst`.
-/
import Linen.Control.Monad.Freer.NonDet

open Data.OpenUnion Control.Monad.Freer Control.Monad.Freer.NonDet

namespace Tests.Control.Monad.Freer.NonDet

-- A deterministic computation has exactly one result.
#guard Eff.run (makeChoiceA (pure 1 : Eff [NonDet] Nat)) == [1]

-- `mzero` has none.
#guard Eff.run (makeChoiceA (mzero : Eff [NonDet] Nat)) == []

-- `mplus` collects both branches, left to right.
#guard Eff.run (makeChoiceA (mplus (pure 1) (pure 2) : Eff [NonDet] Nat)) == [1, 2]

-- Nested choices flatten in order.
#guard Eff.run (makeChoiceA (mplus (mplus (pure 1) (pure 2)) (pure 3)
                              : Eff [NonDet] Nat)) == [1, 2, 3]

-- A failing branch contributes nothing but does not kill its sibling.
#guard Eff.run (makeChoiceA (mplus mzero (pure 2) : Eff [NonDet] Nat)) == [2]
#guard Eff.run (makeChoiceA (mplus (pure 1) mzero : Eff [NonDet] Nat)) == [1]

-- `choose` over a list of computations; `[]` is failure.
#guard Eff.run (makeChoiceA (choose [pure 1, pure 2, pure 3] : Eff [NonDet] Nat))
         == [1, 2, 3]
#guard Eff.run (makeChoiceA (choose [] : Eff [NonDet] Nat)) == []

-- `select` picks an element nondeterministically, so every element is a result.
#guard Eff.run (makeChoiceA (select [10, 20, 30] : Eff [NonDet] Nat))
         == [10, 20, 30]

-- The choice is genuinely a branch point: the continuation runs once per
-- branch, so work after the choice is duplicated across results.
#guard Eff.run (makeChoiceA (do
    let x ← select [1, 2, 3]
    pure (x * 10) : Eff [NonDet] Nat)) == [10, 20, 30]

-- Two independent choices give the cartesian product.
#guard Eff.run (makeChoiceA (do
    let x ← select [1, 2]
    let y ← select [10, 20]
    pure (x + y) : Eff [NonDet] Nat)) == [11, 21, 12, 22]

-- ── guard prunes branches ───────────────────────────────────────────────────

#guard Eff.run (makeChoiceA (do
    let x ← select [1, 2, 3, 4, 5, 6]
    guard (x % 2 == 0)
    pure x : Eff [NonDet] Nat)) == [2, 4, 6]

-- A search: pairs from 1..4 summing to 5.
#guard Eff.run (makeChoiceA (do
    let x ← select [1, 2, 3, 4]
    let y ← select [1, 2, 3, 4]
    guard (x + y == 5)
    pure (x, y) : Eff [NonDet] (Nat × Nat)))
  == [(1, 4), (2, 3), (3, 2), (4, 1)]

-- ── makeChoiceFirst ─────────────────────────────────────────────────────────

#guard Eff.run (makeChoiceFirst (select [7, 8, 9] : Eff [NonDet] Nat)) == some 7
#guard Eff.run (makeChoiceFirst (mzero : Eff [NonDet] Nat)) == none

-- ── The row records that a computation searches ─────────────────────────────

-- This computation may branch and fail, and can do nothing else.
example : Eff [NonDet] Nat := do
  let x ← select [1, 2, 3]
  guard (x > 1)
  pure x

end Tests.Control.Monad.Freer.NonDet
