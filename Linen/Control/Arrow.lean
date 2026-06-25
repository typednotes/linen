/-
  Linen.Control.Arrow — Arrow typeclass

  Arrows generalize functions with additional structure, extending `Category`.
  Provides `Arrow` and `ArrowChoice` typeclasses with `Fun` instances.

  Haskell's `Arrow` is phrased over `Either`; following the import rules we use
  Lean's `Sum` (`.inl` = `Left`, `.inr` = `Right`) instead of a bespoke `Either`.

  ## Hierarchy

  $$\text{Category} \to \text{Arrow} \to \text{ArrowChoice}$$
-/

import Linen.Control.Category

namespace Control

/-- An `Arrow` is a `Category` with the ability to lift pure functions
    and apply them in parallel.

    **Operations:**
    - `arr`: Lift a pure function $f : \alpha \to \beta$ into the arrow.
    - `first`: Apply an arrow to the first component of a pair, passing the second through.
    - `second`: Apply an arrow to the second component.
    - `split` (`&&&`): Apply two arrows in parallel.
-/
class Arrow (Cat : Type u → Type u → Type v) extends Category Cat where
  /-- Lift a pure function into the arrow. -/
  arr : (α → β) → Cat α β
  /-- Apply an arrow to the first component of a pair: `first f (a, c) = (f a, c)`. -/
  first : Cat α β → Cat (α × γ) (β × γ)
  /-- Apply an arrow to the second component of a pair: `second f (c, a) = (c, f a)`. -/
  second : Cat α β → Cat (γ × α) (γ × β) :=
    fun f => Category.comp (arr Prod.swap) (Category.comp (first f) (arr Prod.swap))
  /-- Apply two arrows in parallel: `split f g (a, c) = (f a, g c)`. -/
  split : Cat α β → Cat γ δ → Cat (α × γ) (β × δ) :=
    fun f g => Category.comp (first f) (second g)

/-- `ArrowChoice` extends `Arrow` with the ability to choose between branches,
    working with sum types (`Sum`).

    **Operations:**
    - `left`: Apply an arrow to the `.inl` case, passing `.inr` through.
    - `right`: Apply an arrow to the `.inr` case.
    - `fanin` (`|||`): Merge two arrows into one over a `Sum` input.
-/
class ArrowChoice (Cat : Type u → Type u → Type v) extends Arrow Cat where
  /-- Apply an arrow to the `.inl` branch of a `Sum`, passing `.inr` through.
      `left f (.inl a) = .inl (f a)`, `left f (.inr c) = .inr c`. -/
  left : Cat α β → Cat (α ⊕ γ) (β ⊕ γ)
  /-- Apply an arrow to the `.inr` branch of a `Sum`, passing `.inl` through.
      `right f (.inr a) = .inr (f a)`, `right f (.inl c) = .inl c`. -/
  right : Cat α β → Cat (γ ⊕ α) (γ ⊕ β) :=
    fun f =>
      let swap := arr (fun (e : γ ⊕ α) => match e with
        | .inl c => Sum.inr c
        | .inr a => Sum.inl a)
      let swapBack := arr (fun (e : β ⊕ γ) => match e with
        | .inl b => Sum.inr b
        | .inr c => Sum.inl c)
      Category.comp swap (Category.comp (left f) swapBack)
  /-- Merge two arrows from different branches into a single output.
      `fanin f g (.inl a) = f a`, `fanin f g (.inr c) = g c`. -/
  fanin : Cat α γ → Cat β γ → Cat (α ⊕ β) γ :=
    fun f g =>
      let merge := arr (fun (e : γ ⊕ γ) => match e with
        | .inl c => c
        | .inr c => c)
      Category.comp (Category.comp (left f) (right g)) merge

-- ── Fun instances ──────────────────────────────

instance : Arrow Fun where
  arr f := ⟨f⟩
  first f := ⟨fun (a, c) => (f.apply a, c)⟩
  second f := ⟨fun (c, a) => (c, f.apply a)⟩
  split f g := ⟨fun (a, c) => (f.apply a, g.apply c)⟩

instance : ArrowChoice Fun where
  left f := ⟨fun e => match e with
    | .inl a => .inl (f.apply a)
    | .inr c => .inr c⟩
  right f := ⟨fun e => match e with
    | .inl c => .inl c
    | .inr a => .inr (f.apply a)⟩
  fanin f g := ⟨fun e => match e with
    | .inl a => f.apply a
    | .inr b => g.apply b⟩

end Control
