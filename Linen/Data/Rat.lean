/-
  Linen.Data.Rat — rational helpers

  Haskell's `Data.Ratio` is Lean core's `Rat`: a canonical `num : Int` / `den :
  Nat` rational that is a `Field` (so `+`/`-`/`*`/`/`/`⁻¹`), with `mkRat`,
  `Rat.floor`, `Rat.ceil`, `Rat.abs`, `LT`/`LE`, `ToString`, `Repr`. So the type
  and its arithmetic are **not** re-ported.

  The only function core's `Rat` lacks is `round`, added here (extending `Rat`).
-/

namespace Rat

/-- Round to the nearest integer, with halves rounding **away from zero**.
    $$\text{round}(r) = \frac{2\,\text{num} \pm \text{den}}{2\,\text{den}}$$
    (the sign of the `den` term follows the sign of `num`). -/
def round (r : Rat) : Int :=
  let doubled := 2 * r.num
  let shifted := doubled + (if r.num ≥ 0 then (r.den : Int) else -(r.den : Int))
  shifted / (2 * (r.den : Int))

end Rat
