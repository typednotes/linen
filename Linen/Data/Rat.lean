/-
  Linen.Data.Rat — rational helpers

  Haskell's `Data.Ratio` is Lean core's `Rat`: a canonical `num : Int` / `den :
  Nat` rational that is a `Field` (so `+`/`-`/`*`/`/`/`⁻¹`), with `mkRat`,
  `Rat.floor`, `Rat.ceil`, `Rat.abs`, `LT`/`LE`, `ToString`, `Repr`. So the type
  and its arithmetic are **not** re-ported.

  The only function core's `Rat` lacks is `round`, added here (extending `Rat`).
-/

namespace Rat

/-- Round to the nearest integer, with halves rounding **to even** (banker's
    rounding), matching GHC `base`'s `RealFrac.round`.
    $$\text{round}(r) = \begin{cases}
      \lfloor r \rfloor & r - \lfloor r \rfloor < 1/2 \\
      \lceil r \rceil & r - \lfloor r \rfloor > 1/2 \\
      \lfloor r \rfloor & r - \lfloor r \rfloor = 1/2 \text{ and } \lfloor r \rfloor \text{ even} \\
      \lceil r \rceil & r - \lfloor r \rfloor = 1/2 \text{ and } \lfloor r \rfloor \text{ odd}
    \end{cases}$$ -/
def round (r : Rat) : Int :=
  let fl := r.floor
  let diff := r - (fl : Rat)
  if diff < (1 / 2 : Rat) then fl
  else if diff > (1 / 2 : Rat) then fl + 1
  else if fl % 2 == 0 then fl else fl + 1

end Rat
