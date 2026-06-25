/-
  Linen.Data.Function — missing function combinators

  The Haskell `Data.Function` pieces that have a function-level Lean spelling are
  not re-ported; only `on` and `applyTo` (which core lacks) are provided:

  | Haskell  | Lean                                            |
  |----------|-------------------------------------------------|
  | `flip`   | `flip` (core) — same signature                  |
  | `const`  | `Function.const _` (core; note: type-first)     |
  | `on`     | *(none in core)* → `on` below                   |
  | `(&)`    | `· |> ·` (syntax) → `applyTo` is its function form |
-/

namespace Data.Function

/-- The `on` combinator lifts a binary function through a unary projection:

$$(\texttt{on}\; f\; g)\; x\; y \;=\; f\,(g\,x)\,(g\,y)$$

Commonly used to compare or combine values by a derived key, e.g.
`on (· == ·) String.length` compares strings by length. -/
@[inline] def on (f : β → β → γ) (g : α → β) (x y : α) : γ := f (g x) (g y)

/-- Flip of function application — the function form of Haskell's `(&)` (and of
Lean's `· |> ·` pipe):

$$\texttt{applyTo}\; x\; f \;=\; f\,x$$

Useful where a *value* must be passed a function (e.g. `fs.map (applyTo a)`),
which the `|>` syntax can't express point-free. -/
@[inline] def applyTo (x : α) (f : α → β) : β := f x

/-- `on` unfolds to its definition. -/
theorem on_apply (f : β → β → γ) (g : α → β) (x y : α) : on f g x y = f (g x) (g y) := rfl

/-- `applyTo` unfolds to function application. -/
theorem applyTo_apply (x : α) (f : α → β) : applyTo x f = f x := rfl

end Data.Function
