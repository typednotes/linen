/-
  Category typeclass

  A category has identity morphisms and associative composition. Lean core has
  no general `Category` class (it lives in Mathlib's `CategoryTheory`), so it is
  provided here. Composition uses `≫` (diagrammatic, left-to-right) rather than
  `>>>`, which is already taken by `HShiftRight` in core.
-/

namespace Control

/-- A **category** $\mathcal{C}$ consists of:

    - **Objects:** types $\alpha, \beta, \gamma, \ldots$
    - **Morphisms:** $\text{Cat}\;\alpha\;\beta$ (the "arrows" from $\alpha$ to $\beta$)
    - **Identity:** $\text{id}_\alpha : \text{Cat}\;\alpha\;\alpha$
    - **Composition:** $(f \ggg g) : \text{Cat}\;\alpha\;\gamma$ for
      $f : \text{Cat}\;\alpha\;\beta$ and $g : \text{Cat}\;\beta\;\gamma$

    Composition is in **diagrammatic (left-to-right) order**: `comp f g` means
    "first $f$, then $g$". -/
class Category (Cat : Type u → Type u → Type v) where
  /-- The identity morphism: $\text{id}_\alpha : \text{Cat}\;\alpha\;\alpha$.

      $$\text{id} \ggg f = f = f \ggg \text{id}$$ -/
  id : Cat α α
  /-- Composition in diagrammatic order: $(f \ggg g)(x) = g(f(x))$.

      $$\text{comp}\;f\;g : \text{Cat}\;\alpha\;\gamma$$ -/
  comp : Cat α β → Cat β γ → Cat α γ

/-- Laws for a **lawful category**:

    1. **Left identity:** $\text{id} \ggg f = f$
    2. **Right identity:** $f \ggg \text{id} = f$
    3. **Associativity:** $(f \ggg g) \ggg h = f \ggg (g \ggg h)$ -/
class LawfulCategory (Cat : Type u → Type u → Type v) [Category Cat] : Prop where
  /-- **Left identity:** $\text{id} \ggg f = f$. -/
  id_comp : ∀ {α β : Type u} (f : Cat α β), Category.comp Category.id f = f
  /-- **Right identity:** $f \ggg \text{id} = f$. -/
  comp_id : ∀ {α β : Type u} (f : Cat α β), Category.comp f Category.id = f
  /-- **Associativity:** $(f \ggg g) \ggg h = f \ggg (g \ggg h)$. -/
  comp_assoc : ∀ {α β γ δ : Type u} (f : Cat α β) (g : Cat β γ) (h : Cat γ δ),
    Category.comp (Category.comp f g) h = Category.comp f (Category.comp g h)

-- ── Function instance ──────────────────────────

/-- A wrapper for functions as a two-parameter type suitable for `Category`.

    $$\text{Fun}\;\alpha\;\beta \;\cong\; (\alpha \to \beta)$$

    Needed because Lean's `→` is not a two-parameter type constructor
    in the required form `Type u → Type u → Type v`. -/
structure Fun (α β : Type u) where
  /-- Apply the wrapped function. -/
  apply : α → β

namespace Fun

/-- Functions form a category with:
    - $\text{id} = \lambda x.\, x$
    - $(f \ggg g)(x) = g(f(x))$ -/
instance : Category Fun where
  id := ⟨_root_.id⟩
  comp f g := ⟨g.apply ∘ f.apply⟩

/-- Functions form a **lawful** category — all three laws hold definitionally. -/
instance : LawfulCategory Fun where
  id_comp _ := rfl
  comp_id _ := rfl
  comp_assoc _ _ _ := rfl

end Fun

/-- Diagrammatic composition operator: `f ≫ g` means "first `f`, then `g`".

    $$f \ggg g = \text{Category.comp}\;f\;g$$ -/
scoped infixr:90 " ≫ " => fun f g => Category.comp f g

end Control
