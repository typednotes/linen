/-
  Tests for `Linen.Control.Lens.Level`.
-/
import Linen.Control.Lens.Level

open Control.Lens Control.Lens.Internal

namespace Tests.Linen.Control.Lens.Level

/-- A small self-similar arithmetic-expression tree, used to exercise
    `levels`/`bfs`. Same shape as `Linen.Control.Lens.PlatedTest`'s `Expr`,
    redeclared locally so this test module is self-contained. -/
inductive Expr where
  | lit : Nat → Expr
  | add : Expr → Expr → Expr
deriving Repr, BEq, DecidableEq

/-- `plate` visits `Expr`'s immediate self-similar children: both operands of
    an `add`, none for a `lit`. Given as a top-level `def`/`theorem` pair
    (rather than inline in the `Plated Expr` instance below) so that
    `exprPlate_decreasing`'s proof can refer to `exprPlate` directly by
    name — referring to the class-exported `plate` from inside the very
    instance that defines it makes typeclass resolution unable to find
    `Plated Expr` (it isn't registered yet while its own fields are still
    being elaborated). -/
def exprPlate : Traversal' Expr Expr :=
  fun {F} [Applicative F] f e =>
    match e with
    | .lit n => pure (.lit n)
    | .add l r => .add <$> f l <*> f r

/-- `exprPlate_decreasing` witnesses that both operands of an `add` are
    structurally smaller than the whole `add` — immediate from `sizeOf`'s
    auto-derived definition, which counts `1` for each constructor plus its
    arguments' own sizes. -/
theorem exprPlate_decreasing : ∀ a, ∀ c ∈ toListOf exprPlate a, sizeOf c < sizeOf a := by
  intro a c hc
  cases a with
  | lit n =>
    have h : toListOf exprPlate (Expr.lit n) = [] := rfl
    rw [h] at hc
    cases hc
  | add l r =>
    have h : toListOf exprPlate (Expr.add l r) = [l, r] := rfl
    rw [h] at hc
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hc
    rcases hc with h | h <;> subst h <;> simp +arith

instance : Plated Expr where
  plate := exprPlate
  plate_decreasing := exprPlate_decreasing

-- `(1 + 2) + (3 + 4)`: a balanced tree of depth 2, so breadth-first and
-- depth-first (pre-order) visitation orders genuinely disagree on it.
def e1 : Expr :=
  Expr.add (Expr.add (Expr.lit 1) (Expr.lit 2)) (Expr.add (Expr.lit 3) (Expr.lit 4))

-- ── bfs ──────────────────────────────────────────

-- Breadth-first: the root, then both depth-1 children, then all four
-- depth-2 leaves.
#guard bfs (A := Expr) e1 =
  ([ e1
   , Expr.add (Expr.lit 1) (Expr.lit 2), Expr.add (Expr.lit 3) (Expr.lit 4)
   , Expr.lit 1, Expr.lit 2, Expr.lit 3, Expr.lit 4
   ] : List Expr)

-- Contrast with `universe`'s pre-order (depth-first) listing of the same
-- five nodes, in a different order.
#guard «universe» (A := Expr) e1 =
  ([ e1
   , Expr.add (Expr.lit 1) (Expr.lit 2), Expr.lit 1, Expr.lit 2
   , Expr.add (Expr.lit 3) (Expr.lit 4), Expr.lit 3, Expr.lit 4
   ] : List Expr)

-- Same five nodes either way, just reordered.
#guard (bfs (A := Expr) e1).length = («universe» (A := Expr) e1).length

-- ── levels ───────────────────────────────────────

-- `levels` is `bfs`, additionally indexed by breadth-first position.
def e1Levels : List (Nat × Expr) := itoListOf (levels (A := Expr)) e1

#guard e1Levels = (List.range (bfs (A := Expr) e1).length).zip (bfs (A := Expr) e1)
#guard e1Levels.map Prod.fst = [0, 1, 2, 3, 4, 5, 6]
#guard e1Levels.map Prod.snd = bfs (A := Expr) e1

end Tests.Linen.Control.Lens.Level
