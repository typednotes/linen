/-
  Linen.Data.Bool — Boolean utilities

  Haskell's `Data.Bool.bool` (case analysis as a function) is **already in Lean's
  core library** as `bool` (`Init.Control.Basic`), generalised over `[ToBool β]`:
  `bool ifFalse ifTrue b`. So it is not re-ported here; use the core `bool` (or
  `cond`/`if`).

  The one piece core lacks is `guard'`, the list-valued guard — `List` is not an
  `Alternative` in core, so there is no `guard`-into-`[]` for it.
-/

namespace Data.Bool

/-- Guard: returns `[x]` if the condition holds, `[]` otherwise.

    $$\text{guard'}(b, x) = \begin{cases} [x] & \text{if } b \\ [] & \text{otherwise} \end{cases}$$ -/
@[inline] def guard' (b : Bool) (x : α) : List α :=
  if b then [x] else []

/-- Guard on `true` returns a singleton list. -/
theorem guard'_true (x : α) : guard' true x = [x] := rfl

/-- Guard on `false` returns the empty list. -/
theorem guard'_false (x : α) : guard' false x = [] := rfl

end Data.Bool
