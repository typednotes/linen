/-
  `Data.Json.Bridge` — between linen's `Data.Json.Value` and Lean core's `Lean.Json`

  linen's `Data.Json` (an aeson port) and Lean core's `Lean.Json` are two
  independent JSON libraries. Both read and write standard JSON text, which
  is how this bridge converts: through each library's own total encoder and
  parser. So it is exact wherever both models can represent the value — and
  says so where they cannot:

  - **Numbers** are the one difference in what can be represented: linen's
    are IEEE doubles (`Float`), Lean core's exact decimals (`JsonNumber`).
    `ofLeanJson` rounds numbers a double cannot hold exactly (e.g. integers
    above $2^{53}$). `toLeanJson` is exact on integer-valued numbers, but
    **non-integer numbers lose precision**: linen's encoder
    (`Data.Json.Encode.renderNumber`) writes them with 6 significant digits
    (`0.123456789` ↦ `0.123457`) — a limitation of that encoder, which this
    bridge inherits rather than works around. A `NaN` or an infinity becomes
    `null`, as linen's encoder writes it.
  - **Objects**: Lean core keeps keys sorted and unique, linen keeps them in
    order and allows duplicates; `toLeanJson` keeps the last of duplicate
    keys, and `ofLeanJson` yields keys in sorted order.
-/
import Lean.Data.Json
import Linen.Data.Json

namespace Data.Json

/-- linen's JSON as Lean core's: exact, except that a non-finite number has no
    JSON form. -/
def Value.toLeanJson (v : Value) : Except String Lean.Json :=
  Lean.Json.parse (Encode.encode v)

/-- Lean core's JSON as linen's: exact, except that a number a double cannot
    hold is rounded to the nearest one. -/
def Value.ofLeanJson (j : Lean.Json) : Except String Value :=
  Decode.decode j.compress

end Data.Json
