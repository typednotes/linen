/-
  Tests for `Linen.Control.Lens` (the top-level facade re-exporting all of
  batch B's `lens` port, plus `Linen.Control.Lens.Profunctor`).

  A handful of `#guard`s spanning many corners of the library — `Lens`,
  `Prism`, `Iso`, `Fold`, `Traversal`, `Each`, `Cons`, `Empty`, `Wrapped`,
  `Plated`, `Level` — confirming the whole facade chain (`Linen.Control.
  Lens` → `.Combinators`/`.Profunctor` → every individual `Control.Lens.*`
  module) resolves through a single bare `import Linen.Control.Lens`. -/
import Linen.Control.Lens

open Control.Lens Control.Lens.Internal Control.Profunctor

namespace Tests.Linen.Control.Lens

-- `Lens`/`Getter`/`Setter`.
def fstL : Lens' (Nat × Nat) Nat := lens Prod.fst (fun s v => (v, s.2))
#guard view fstL (3, 4) == 3
#guard set fstL 9 (3, 4) == (9, 4)

-- `Iso`.
def notIso : Iso' Bool Bool := iso not not
#guard withIso notIso (fun sa _ => sa true) == false

-- `Prism`.
#guard withPrism (_Just (A := Nat) (B := Nat)) (fun bt _ => bt 5) = some (5 : Nat)
#guard review (_Just (A := Nat) (B := Nat)) 5 = some (5 : Nat)

-- `Fold`/`Traversal`/`Each`.
#guard toListOf each ([1, 2, 3] : List Nat) = [1, 2, 3]
#guard ([1, 2, 3] : List Nat) ^.. each = [1, 2, 3]
#guard over each (· + 1) ([1, 2, 3] : List Nat) = [2, 3, 4]

-- `Cons`.
#guard withPrism (_Cons (S := List Nat) (B := Nat)) (fun bt _ => bt (1, [2, 3])) = [1, 2, 3]

-- `Empty`.
#guard withPrism (_Empty (A := List Nat)) (fun _ seta => seta ([] : List Nat)) = Sum.inr ()

-- `Wrapped`.
#guard withIso (_Wrapped' (S := Data.Dual Nat)) (fun sa _ => sa ⟨3⟩) = 3

-- `Plated`/`Level`.
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
    | .add l r => Expr.add <$> f l <*> f r

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

#guard «universe» (A := Expr) (Expr.add (.lit 1) (.lit 2)) =
  [Expr.add (.lit 1) (.lit 2), .lit 1, .lit 2]

-- `Control.Lens.Profunctor`: `fromLens`/`toLens` round-trip through `Star
-- Id`, confirming the `Profunctor`-interoperability layer is also in scope
-- via this same facade.
def viaProfunctor : Nat × Nat :=
  toLens (F := Id) (fromLens (P := Star Id) fstL) (fun a => (a + 1 : Id Nat)) (3, 4)
#guard viaProfunctor = (4, 4)

end Tests.Linen.Control.Lens
