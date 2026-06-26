/-
  Linen.Data.Newtype — Semigroup/Monoid newtype wrappers

  The Haskell `Data.Monoid` / `Data.Semigroup` wrappers (not in Lean core):
  `Dual`, `Endo`, `First`, `Last`, `Sum`, `Product`, `All`, `Any`. Each is a
  single-field structure with an `Append` (semigroup) instance and an
  associativity proof.

  Note: `Data.Sum` / `Data.Product` here are the *monoid* wrappers and are
  distinct from Lean's `Sum` (`⊕`) type — they differ in kind and namespace.
-/

namespace Data

-- ══════════════════════════════════════════════
-- Dual: reverses the semigroup operation
-- ══════════════════════════════════════════════

/-- `Dual α` reverses the `Append` (semigroup) operation:
    $\text{Dual}(a) \mathbin{++} \text{Dual}(b) = \text{Dual}(b \mathbin{++} a)$. -/
structure Dual (α : Type u) where
  getDual : α
deriving BEq, Ord, Repr, Hashable

instance [ToString α] : ToString (Dual α) where
  toString d := s!"Dual({d.getDual})"

instance [Append α] : Append (Dual α) where
  append a b := ⟨b.getDual ++ a.getDual⟩

/-- Associativity of the dual semigroup, given associativity of `++` on `α`. -/
theorem Dual.append_assoc [Append α] [h : Std.Associative (α := α) (· ++ ·)]
    (a b c : Dual α) : a ++ b ++ c = a ++ (b ++ c) := by
  simp [HAppend.hAppend, Append.append]
  exact (h.assoc c.getDual b.getDual a.getDual).symm

-- ══════════════════════════════════════════════
-- Endo: endomorphism monoid under composition
-- ══════════════════════════════════════════════

/-- `Endo α` wraps endomorphisms `α → α`; a monoid under composition:
    $\text{Endo}(f) \mathbin{++} \text{Endo}(g) = \text{Endo}(f \circ g)$. -/
structure Endo (α : Type u) where
  appEndo : α → α

instance : Append (Endo α) where
  append f g := ⟨f.appEndo ∘ g.appEndo⟩

/-- Associativity of endomorphism composition — `rfl`. -/
theorem Endo.append_assoc (a b c : Endo α) : a ++ b ++ c = a ++ (b ++ c) := rfl

-- ══════════════════════════════════════════════
-- First / Last: keep first or last `some` value
-- ══════════════════════════════════════════════

/-- `First α` keeps the leftmost `some` under `Append`. Identity: `First none`. -/
structure First (α : Type u) where
  getFirst : Option α
deriving BEq, Repr

instance [ToString α] : ToString (First α) where
  toString f := s!"First({f.getFirst})"

instance : Append (First α) where
  append a b := ⟨a.getFirst <|> b.getFirst⟩

/-- Associativity of `First`, from associativity of `Option`'s `<|>`. -/
theorem First.append_assoc (a b c : First α) : a ++ b ++ c = a ++ (b ++ c) := by
  simp [HAppend.hAppend, Append.append]
  cases a.getFirst <;> simp

/-- `Last α` keeps the rightmost `some` under `Append`. Identity: `Last none`. -/
structure Last (α : Type u) where
  getLast : Option α
deriving BEq, Repr

instance [ToString α] : ToString (Last α) where
  toString l := s!"Last({l.getLast})"

instance : Append (Last α) where
  append a b := ⟨b.getLast <|> a.getLast⟩

/-- Associativity of `Last`, from associativity of right-biased `<|>`. -/
theorem Last.append_assoc (a b c : Last α) : a ++ b ++ c = a ++ (b ++ c) := by
  simp [HAppend.hAppend, Append.append]
  cases c.getLast <;> simp

-- ══════════════════════════════════════════════
-- Sum / Product: numeric monoids
-- ══════════════════════════════════════════════

/-- `Sum α` is a monoid wrapper under addition. Identity: `Sum 0`. -/
structure Sum (α : Type u) where
  getSum : α
deriving BEq, Ord, Repr, Hashable

instance [ToString α] : ToString (Sum α) where
  toString s := s!"Sum({s.getSum})"

instance [Add α] : Append (Sum α) where
  append a b := ⟨a.getSum + b.getSum⟩

/-- Associativity of `Sum`, given associativity of `+` on `α`. -/
theorem Sum.append_assoc [Add α] [h : Std.Associative (α := α) (· + ·)]
    (a b c : Sum α) : a ++ b ++ c = a ++ (b ++ c) := by
  simp [HAppend.hAppend, Append.append, Sum.mk.injEq]
  exact h.assoc a.getSum b.getSum c.getSum

/-- `Product α` is a monoid wrapper under multiplication. Identity: `Product 1`. -/
structure Product (α : Type u) where
  getProduct : α
deriving BEq, Ord, Repr, Hashable

instance [ToString α] : ToString (Product α) where
  toString p := s!"Product({p.getProduct})"

instance [Mul α] : Append (Product α) where
  append a b := ⟨a.getProduct * b.getProduct⟩

/-- Associativity of `Product`, given associativity of `*` on `α`. -/
theorem Product.append_assoc [Mul α] [h : Std.Associative (α := α) (· * ·)]
    (a b c : Product α) : a ++ b ++ c = a ++ (b ++ c) := by
  simp [HAppend.hAppend, Append.append, Product.mk.injEq]
  exact h.assoc a.getProduct b.getProduct c.getProduct

-- ══════════════════════════════════════════════
-- All / Any: boolean monoids
-- ══════════════════════════════════════════════

/-- `All` is the boolean monoid under conjunction. Identity: `All true`. -/
structure All where
  getAll : Bool
deriving BEq, Ord, Repr, Hashable

instance : ToString All where
  toString a := s!"All({a.getAll})"

instance : Append All where
  append a b := ⟨a.getAll && b.getAll⟩

/-- Associativity of `All` (`&&`). -/
theorem All.append_assoc (a b c : All) : a ++ b ++ c = a ++ (b ++ c) := by
  simp [HAppend.hAppend, Append.append, All.mk.injEq]
  cases a.getAll <;> simp

/-- `Any` is the boolean monoid under disjunction. Identity: `Any false`. -/
structure Any where
  getAny : Bool
deriving BEq, Ord, Repr, Hashable

instance : ToString Any where
  toString a := s!"Any({a.getAny})"

instance : Append Any where
  append a b := ⟨a.getAny || b.getAny⟩

/-- Associativity of `Any` (`||`). -/
theorem Any.append_assoc (a b c : Any) : a ++ b ++ c = a ++ (b ++ c) := by
  simp [HAppend.hAppend, Append.append, Any.mk.injEq]
  cases a.getAny <;> simp

end Data
