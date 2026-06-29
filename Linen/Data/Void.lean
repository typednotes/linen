/-
  Linen.Data.Void — the uninhabited type

  Haskell's `Data.Void` is Lean core's `Empty`, and `absurd` is core
  `Empty.elim`. Core already supplies `DecidableEq`, `Repr`, and `Subsingleton`
  for `Empty` (the `Repr` instance exists precisely so `Empty` can be used as a
  type parameter without instance synthesis failing).

  This module adds the remaining vacuous instances — `BEq`, `Ord`, `Hashable`,
  `ToString`, and `Inhabited (Empty → α)` — by the same rationale, plus the
  singleton law for the function space `Empty → α`.
-/

namespace Empty

/-- `BEq` for `Empty` — vacuous; no two values exist to compare. -/
instance : BEq Empty where
  beq v _ := v.elim

/-- `Ord` for `Empty` — vacuous; no values exist to order. -/
instance : Ord Empty where
  compare v _ := v.elim

/-- `Hashable` for `Empty` — vacuous. -/
instance : Hashable Empty where
  hash v := v.elim

/-- `ToString` for `Empty` — vacuous; no value can be rendered. -/
instance : ToString Empty where
  toString v := v.elim

/-- The function space `Empty → α` is inhabited, witnessed by `Empty.elim`.
    Since $|\bot| = 0$, there is exactly one such function. -/
instance : Inhabited (Empty → α) where
  default := Empty.elim

/-- Any function out of `Empty` equals `Empty.elim`: the space `Empty → α` is a
    singleton (*ex falso quodlibet*).
    $$\forall\, f : \bot \to \alpha,\; f = \text{Empty.elim}$$ -/
theorem eq_absurd (f : Empty → α) : f = Empty.elim := by
  funext v; exact v.elim

end Empty
