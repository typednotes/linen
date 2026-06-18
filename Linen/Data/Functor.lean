/-
  Functor utilities

  Constructions on functors and applicatives that are not in the Lean standard
  library. The identity functor is omitted: Lean's `Id` already provides it
  together with its `Functor`/`Applicative`/`Monad` instances.

  Provided here:
  - `Compose`        — composition of two functors / applicatives
  - `Const`          — the constant functor
  - `Product`        — the product of two functors
  - `FunctorSum`     — the sum (coproduct) of two functors
  - `Contravariant`  — contravariant functors, with `Predicate`/`Equivalence`
-/

namespace Data.Functor

-- ── Compose ───────────────────────────────────────────────────────────

/-- Functor/applicative composition: $(\text{Compose}\;F\;G)\;\alpha = F\,(G\;\alpha)$.

    This witnesses the classical result that **the composition of two functors is a functor**,
    and **the composition of two applicatives is an applicative**. -/
structure Compose (F : Type u → Type v) (G : Type w → Type u) (α : Type w) where
  /-- Unwrap the composed value: $F\,(G\;\alpha)$. -/
  getCompose : F (G α)

namespace Compose

/-- `Functor` instance for `Compose F G`: maps through both layers.

    $$\text{fmap}\;f\;(\text{Compose}\;x) = \text{Compose}\;(\text{fmap}\;(\text{fmap}\;f)\;x)$$ -/
instance [Functor F] [Functor G] : Functor (Compose F G) where
  map f c := ⟨(f <$> ·) <$> c.getCompose⟩

/-- **Identity law** for composed functors: $\text{fmap}\;\text{id} = \text{id}$. -/
theorem map_id [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (x : Compose F G α) :
    (id <$> x) = x := by
  simp [Functor.map, id_map]

/-- **Composition law** for composed functors:
    $\text{fmap}\;(f \circ g) = \text{fmap}\;f \circ \text{fmap}\;g$. -/
theorem map_comp [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (f : β → γ) (g : α → β) (x : Compose F G α) :
    ((f ∘ g) <$> x) = (f <$> (g <$> x)) := by
  simp [Functor.map, comp_map]

/-- `Pure` instance for `Compose F G`: wraps a value in both layers.

    $$\text{pure}\;a = \text{Compose}\;(\text{pure}\;(\text{pure}\;a))$$ -/
instance [Applicative F] [Applicative G] : Pure (Compose F G) where
  pure a := ⟨pure (pure a)⟩

/-- `Seq` instance for `Compose F G`: applies through both layers using
    the applicative structure of $F$ and $G$.

    $$\text{Compose}\;f \mathbin{<*>} \text{Compose}\;x
      = \text{Compose}\;((\mathbin{<*>}) \mathbin{<\$>} f \mathbin{<*>} x)$$ -/
instance [Applicative F] [Applicative G] : Seq (Compose F G) where
  seq f x := ⟨Seq.seq ((· <*> ·) <$> f.getCompose) (fun () => (x ()).getCompose)⟩

/-- `Applicative` instance for `Compose F G`: the composition of two applicatives
    is an applicative. -/
instance [Applicative F] [Applicative G] : Applicative (Compose F G) where

end Compose

-- ── Const ─────────────────────────────────────────────────────────────

/-- The **constant functor** $\text{Const}\;\alpha\;\beta$: carries a value of type $\alpha$,
    with a phantom type parameter $\beta$.

    $$\text{Const}\;\alpha\;\beta \;\cong\; \alpha$$

    As a functor in $\beta$, mapping is a no-op (the $\beta$ is phantom).
    This makes `Const` the key ingredient for implementing `foldMap` via `traverse`. -/
structure Const (α : Type u) (β : Type v) where
  /-- Extract the wrapped value. -/
  getConst : α

namespace Const

/-- `BEq` instance for `Const α β`: compares the underlying $\alpha$ values,
    ignoring the phantom $\beta$. -/
instance [BEq α] : BEq (Const α β) where
  beq a b := a.getConst == b.getConst

/-- `Ord` instance for `Const α β`: orders by the underlying $\alpha$ values. -/
instance [Ord α] : Ord (Const α β) where
  compare a b := compare a.getConst b.getConst

/-- `Repr` instance for `Const α β`: delegates to the underlying $\alpha$ representation. -/
instance [Repr α] : Repr (Const α β) where
  reprPrec c p := Repr.reprPrec c.getConst p

/-- `ToString` instance for `Const α β`: delegates to `ToString α`. -/
instance [ToString α] : ToString (Const α β) where
  toString c := toString c.getConst

/-- `Functor` instance for `Const α`: mapping over the phantom parameter is a no-op.

    $$\text{fmap}\;f\;(\text{Const}\;a) = \text{Const}\;a$$ -/
instance : Functor (Const α) where
  map _ c := ⟨c.getConst⟩

/-- Mapping preserves the underlying value:
    $(\text{fmap}\;f\;c).\text{getConst} = c.\text{getConst}$. -/
theorem map_val (f : β → γ) (c : Const α β) :
    (f <$> c).getConst = c.getConst := rfl

/-- **Identity law:** $\text{fmap}\;\text{id} = \text{id}$ for `Const`. -/
theorem map_id (c : Const α β) :
    (id <$> c) = c := rfl

/-- **Composition law:**
    $\text{fmap}\;(f \circ g) = \text{fmap}\;f \circ \text{fmap}\;g$ for `Const`. -/
theorem map_comp (f : γ → δ) (g : β → γ) (c : Const α β) :
    (f ∘ g) <$> c = f <$> (g <$> c) := rfl

/-- `Pure` instance for `Const α` (requires `Append α` and `Inhabited α`):
    $\text{pure}\;\_= \text{Const}(\text{default})$.

    The value is the monoidal identity (`default`), since `pure` should be
    the identity element for applicative combination. -/
instance [Append α] [Inhabited α] : Pure (Const α) where
  pure _ := ⟨default⟩

end Const

-- ── Product ───────────────────────────────────────────────────────────

/-- The **product functor** $(\text{Product}\;F\;G)\;\alpha = F\;\alpha \times G\;\alpha$.

    Pairs two functorial values at the same type parameter.
    Mapping over a product maps through both components independently.

    $$\text{fmap}\;f\;(\text{Product}\;(a, b)) = \text{Product}\;(\text{fmap}\;f\;a,\;\text{fmap}\;f\;b)$$ -/
structure Product (F G : Type u → Type v) (α : Type u) where
  /-- Unwrap the product to a pair: $F\;\alpha \times G\;\alpha$. -/
  runProduct : F α × G α

namespace Product

/-- `Functor` instance for `Product F G`: maps through both components. -/
instance [Functor F] [Functor G] : Functor (Product F G) where
  map f p := ⟨(f <$> p.runProduct.1, f <$> p.runProduct.2)⟩

/-- `BEq` instance for `Product F G α`: both components must be equal. -/
instance [BEq (F α)] [BEq (G α)] : BEq (Product F G α) where
  beq a b := a.runProduct.1 == b.runProduct.1 && a.runProduct.2 == b.runProduct.2

/-- **Identity law** for product functors: $\text{fmap}\;\text{id} = \text{id}$. -/
theorem map_id [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (x : Product F G α) :
    (id <$> x) = x := by
  simp [Functor.map, id_map]

/-- **Composition law** for product functors:
    $\text{fmap}\;(f \circ g) = \text{fmap}\;f \circ \text{fmap}\;g$. -/
theorem map_comp [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (f : β → γ) (g : α → β) (x : Product F G α) :
    ((f ∘ g) <$> x) = (f <$> (g <$> x)) := by
  simp [Functor.map, comp_map]

end Product

-- ── Sum ───────────────────────────────────────────────────────────────

/-- The **sum (coproduct) functor** $(\text{FunctorSum}\;F\;G)\;\alpha = F\;\alpha + G\;\alpha$.

    Holds either an `F α` (via `inl`) or a `G α` (via `inr`).
    Mapping over a sum maps through whichever branch is inhabited. -/
inductive FunctorSum (F G : Type u → Type v) (α : Type u) where
  /-- Left injection: wraps an $F\;\alpha$. -/
  | inl : F α → FunctorSum F G α
  /-- Right injection: wraps a $G\;\alpha$. -/
  | inr : G α → FunctorSum F G α

namespace FunctorSum

/-- `Functor` instance for `FunctorSum F G`: maps through whichever branch is present. -/
instance [Functor F] [Functor G] : Functor (FunctorSum F G) where
  map f
    | .inl a => .inl (f <$> a)
    | .inr b => .inr (f <$> b)

/-- **Identity law** for sum functors: $\text{fmap}\;\text{id} = \text{id}$. -/
theorem map_id [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (x : FunctorSum F G α) :
    (id <$> x) = x := by
  cases x with
  | inl a => simp [Functor.map, id_map]
  | inr b => simp [Functor.map, id_map]

/-- **Composition law** for sum functors:
    $\text{fmap}\;(f \circ g) = \text{fmap}\;f \circ \text{fmap}\;g$. -/
theorem map_comp [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (f : β → γ) (g : α → β) (x : FunctorSum F G α) :
    ((f ∘ g) <$> x) = (f <$> (g <$> x)) := by
  cases x with
  | inl a => simp [Functor.map, comp_map]
  | inr b => simp [Functor.map, comp_map]

end FunctorSum

-- ── Contravariant ─────────────────────────────────────────────────────

/-- A **contravariant functor** $F : \mathsf{Type}^{\text{op}} \to \mathsf{Type}$.
Unlike a covariant functor which preserves morphism direction, a contravariant
functor reverses it: given $f : \alpha \to \beta$, we obtain

$$\text{contramap}\; f : F\;\beta \to F\;\alpha$$ -/
class Contravariant (F : Type u → Type v) where
  /-- Map contravariantly: given $f : \alpha \to \beta$, produce
  $\text{contramap}\;f : F\;\beta \to F\;\alpha$. -/
  contramap : (α → β) → F β → F α

/-- Laws for a lawful contravariant functor:

1. **Identity:** $\text{contramap}\;\text{id} = \text{id}$
2. **Composition:** $\text{contramap}\;(f \circ g) = \text{contramap}\;g \circ \text{contramap}\;f$

Note the reversal in the composition law — this is dual to the covariant functor law. -/
class LawfulContravariant (F : Type u → Type v) [Contravariant F] : Prop where
  /-- **Identity law:** $\text{contramap}\;\text{id}\;x = x$. -/
  contramap_id : ∀ (x : F α), Contravariant.contramap id x = x
  /-- **Composition law:**
  $\text{contramap}\;(f \circ g)\;x = \text{contramap}\;g\;(\text{contramap}\;f\;x)$. -/
  contramap_comp : ∀ (f : β → γ) (g : α → β) (x : F γ),
    Contravariant.contramap (f ∘ g) x = Contravariant.contramap g (Contravariant.contramap f x)

/-- A predicate $P : \alpha \to \text{Prop}$, wrapped as a contravariant functor.

Given $f : \alpha \to \beta$ and a predicate $P$ on $\beta$, the contramapped
predicate is $P \circ f$, i.e., $(\text{contramap}\;f\;P)(x) = P(f(x))$. -/
structure Predicate (α : Type u) where
  getPredicate : α → Prop

/-- `Contravariant` instance for `Predicate`:
$\text{contramap}\;f\;P = P \circ f$. -/
instance : Contravariant Predicate where
  contramap f p := ⟨p.getPredicate ∘ f⟩

/-- `Predicate` is a lawful contravariant functor — both laws hold definitionally. -/
instance : LawfulContravariant Predicate where
  contramap_id _ := rfl
  contramap_comp _ _ _ := rfl

/-- An equivalence relation $R : \alpha \to \alpha \to \text{Prop}$, wrapped as a
contravariant functor.

Given $f : \alpha \to \beta$ and an equivalence $R$ on $\beta$, the contramapped
equivalence is: $(\text{contramap}\;f\;R)(a, b) = R(f(a), f(b))$. -/
structure Equivalence (α : Type u) where
  getEquivalence : α → α → Prop

/-- `Contravariant` instance for `Equivalence`:
$(\text{contramap}\;f\;R)(a, b) = R(f(a),\, f(b))$. -/
instance : Contravariant Equivalence where
  contramap f e := ⟨fun a b => e.getEquivalence (f a) (f b)⟩

/-- `Equivalence` is a lawful contravariant functor — both laws hold definitionally. -/
instance : LawfulContravariant Equivalence where
  contramap_id _ := rfl
  contramap_comp _ _ _ := rfl

end Data.Functor
