/-
  Tests for `Linen.Control.Lens.Plated`.
-/
import Linen.Control.Lens.Plated

open Control.Lens Control.Lens.Internal

namespace Tests.Linen.Control.Lens.Plated

/-- A small self-similar arithmetic-expression tree, used to exercise
    `Plated`'s combinators. -/
inductive Expr where
  | lit : Nat → Expr
  | add : Expr → Expr → Expr
deriving Repr, BEq, DecidableEq

/-- `plate` visits `Expr`'s immediate self-similar children: both operands of
    an `add`, none for a `lit`. Given as a top-level `def`/`theorem` pair
    (rather than inline in the `Plated Expr` instance below) so that
    `plate_decreasing`'s proof can refer to `exprPlate` directly by name —
    referring to the class-exported `plate` from inside the very instance
    that defines it makes typeclass resolution unable to find `Plated Expr`
    (it isn't registered yet while its own fields are still being
    elaborated). -/
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

-- `(1 + 2) + 3`
def e1 : Expr := .add (.add (.lit 1) (.lit 2)) (.lit 3)

-- ── children ────────────────────────────────────

#guard children e1 = ([Expr.add (Expr.lit 1) (Expr.lit 2), Expr.lit 3] : List Expr)
#guard children (Expr.lit 5) = ([] : List Expr)

-- ── universe / cosmos ────────────────────────────

-- Pre-order: self, then every descendant of each child in turn.
#guard «universe» (A := Expr) e1 =
  ([e1, Expr.add (Expr.lit 1) (Expr.lit 2), Expr.lit 1, Expr.lit 2, Expr.lit 3] : List Expr)

#guard toListOf (cosmos (A := Expr)) e1 = «universe» (A := Expr) e1

-- ── transform ────────────────────────────────────

/-- Double every literal, bottom-up. -/
def double : Expr → Expr
  | .lit n => .lit (n * 2)
  | e => e

#guard transform double e1 = Expr.add (Expr.add (Expr.lit 2) (Expr.lit 4)) (Expr.lit 6)

-- ── rewrite ──────────────────────────────────────

/-- Simplify `x + 0` and `0 + x` to `x`, one rewrite at a time; `rewrite`
    keeps applying it at each node until it stops firing. -/
def dropZero : Expr → Option Expr
  | .add l (.lit 0) => some l
  | .add (.lit 0) r => some r
  | _ => none

/-- `dropZero` only ever replaces a node with one of its own immediate
    operands, so plain `sizeOf` is already a valid strictly-decreasing
    measure for it. -/
theorem dropZero_dec : ∀ a a', dropZero a = some a' → sizeOf a' < sizeOf a := by
  intro a a' h
  unfold dropZero at h
  split at h <;> simp_all <;> simp +arith

#guard rewrite dropZero sizeOf dropZero_dec
    (Expr.add (Expr.add (Expr.lit 0) (Expr.lit 5)) (Expr.lit 0)) = Expr.lit 5

-- With no rewrite left to apply, `rewrite` is the identity.
#guard rewrite dropZero sizeOf dropZero_dec e1 = e1

-- ── para ─────────────────────────────────────────

/-- Count the leaves (`lit`s) of an `Expr` via a paramorphism: a `lit` counts
    as `1`, an `add` sums its children's already-computed counts. -/
def leafCount : Expr → List Nat → Nat
  | .lit _, _ => 1
  | .add .., counts => counts.foldl (· + ·) 0

#guard para leafCount e1 = 3

-- ── holes / contexts ─────────────────────────────

-- `e1 = (1 + 2) + 3` has two immediate children: `1 + 2` and `3`.
#guard (holes e1).map Context.pos =
  ([Expr.add (Expr.lit 1) (Expr.lit 2), Expr.lit 3] : List Expr)

-- Rebuilding the second hole with a replacement only changes that child.
#guard
  (match holes e1 with
    | [_, h2] => h2.peek (Expr.lit 99)
    | _ => Expr.lit 0) =
  Expr.add (Expr.add (Expr.lit 1) (Expr.lit 2)) (Expr.lit 99)

-- `contexts` additionally includes the trivial root context (`id`, `e1`
-- itself), so it has one more entry than `universe` at the same node's depth
-- would need to reach every subterm once.
#guard (contexts e1).length = («universe» (A := Expr) e1).length
#guard (contexts e1).map Context.pos = «universe» (A := Expr) e1

-- ── composOpFold ─────────────────────────────────

-- Sum the (one level deep) sizes of every immediate child.
#guard composOpFold 0 (· + ·) (fun _ => 1) e1 = 2
#guard composOpFold 0 (· + ·) (fun _ => 1) (Expr.lit 1) = 0

end Tests.Linen.Control.Lens.Plated
