/-
  Linen.Data.Bifunctor — Bifunctor typeclass

  `Bifunctor` maps over both components of a two-parameter type. Lean's core
  library has no `Bifunctor`, so this ports Haskell's `Data.Bifunctor`, with
  lawful instances for `Prod`, `Sum`, and `Except`.
-/

namespace Data

/-- A **bifunctor** is a type constructor $F : \mathsf{Type} \to \mathsf{Type} \to \mathsf{Type}$
that is functorial in both arguments. Given morphisms $f : \alpha \to \gamma$ and
$g : \beta \to \delta$, we obtain:

$$\text{bimap}\; f\; g : F\;\alpha\;\beta \to F\;\gamma\;\delta$$ -/
class Bifunctor (F : Type u → Type v → Type w) where
  /-- Map over both arguments simultaneously:
  $\text{bimap}\; f\; g : F\;\alpha\;\beta \to F\;\gamma\;\delta$. -/
  bimap : (α → γ) → (β → δ) → F α β → F γ δ
  /-- Map over the first argument only:
  $\text{mapFst}\; f = \text{bimap}\; f\; \text{id}$. -/
  mapFst : (α → γ) → F α β → F γ β := fun f => bimap f id
  /-- Map over the second argument only:
  $\text{mapSnd}\; g = \text{bimap}\; \text{id}\; g$. -/
  mapSnd : (β → δ) → F α β → F α δ := fun g => bimap id g

/-- Laws that a well-behaved `Bifunctor` must satisfy:

1. **Identity:** $\text{bimap}\;\text{id}\;\text{id} = \text{id}$
2. **Composition:** $\text{bimap}\;(f_1 \circ f_2)\;(g_1 \circ g_2)
   = \text{bimap}\;f_1\;g_1 \circ \text{bimap}\;f_2\;g_2$
-/
class LawfulBifunctor (F : Type u → Type v → Type w) [Bifunctor F] : Prop where
  /-- **Identity law:** $\text{bimap}\;\text{id}\;\text{id}\;x = x$. -/
  bimap_id : ∀ (x : F α β), Bifunctor.bimap id id x = x
  /-- **Composition law:**
  $\text{bimap}\;(f_1 \circ f_2)\;(g_1 \circ g_2)\;x
    = \text{bimap}\;f_1\;g_1\;(\text{bimap}\;f_2\;g_2\;x)$. -/
  bimap_comp : ∀ (f₁ : γ → δ) (f₂ : α → γ) (g₁ : ε → ζ) (g₂ : β → ε) (x : F α β),
    Bifunctor.bimap (f₁ ∘ f₂) (g₁ ∘ g₂) x = Bifunctor.bimap f₁ g₁ (Bifunctor.bimap f₂ g₂ x)

-- ── Instances ──────────────────────────────────

/-- `Bifunctor` instance for `Prod`: $\text{bimap}\;f\;g\;(a, b) = (f\,a,\; g\,b)$. -/
instance : Bifunctor Prod where
  bimap f g p := (f p.1, g p.2)

/-- `Prod` is a lawful bifunctor — both laws hold definitionally. -/
instance : LawfulBifunctor Prod where
  bimap_id _ := rfl
  bimap_comp _ _ _ _ _ := rfl

/-- `Bifunctor` instance for `Sum`:

$$\text{bimap}\;f\;g\;(\text{inl}\;a) = \text{inl}\;(f\,a)$$
$$\text{bimap}\;f\;g\;(\text{inr}\;b) = \text{inr}\;(g\,b)$$ -/
instance : Bifunctor Sum where
  bimap f g
    | .inl a => .inl (f a)
    | .inr b => .inr (g b)

/-- `Sum` is a lawful bifunctor — proved by case analysis. -/
instance : LawfulBifunctor Sum where
  bimap_id x := by cases x <;> rfl
  bimap_comp _ _ _ _ x := by cases x <;> rfl

/-- `Bifunctor` instance for `Except`:

$$\text{bimap}\;f\;g\;(\text{error}\;a) = \text{error}\;(f\,a)$$
$$\text{bimap}\;f\;g\;(\text{ok}\;b) = \text{ok}\;(g\,b)$$ -/
instance : Bifunctor Except where
  bimap f g
    | .error a => .error (f a)
    | .ok b => .ok (g b)

/-- `Except` is a lawful bifunctor — proved by case analysis. -/
instance : LawfulBifunctor Except where
  bimap_id x := by cases x <;> rfl
  bimap_comp _ _ _ _ x := by cases x <;> rfl

end Data
