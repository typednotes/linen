/-
  `Data.Json.Bridge` — between linen's `Data.Json.Value` and Lean core's `Lean.Json`

  linen's `Data.Json` (an aeson port) and Lean core's `Lean.Json` are two
  independent JSON libraries. The bridge builds Lean core's values directly,
  using JSON text for numbers and for the reverse conversion. It is exact
  wherever both models can represent the value, with these exceptions:

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

/-- Convert to Lean core's JSON, using the linen encoder's number precision
    and keeping the last occurrence of each object key. -/
def Value.toLeanJson : Value → Except String Lean.Json
  | .null => .ok .null
  | .bool b => .ok (.bool b)
  | .string s => .ok (.str s)
  | .number n => Lean.Json.parse (Encode.renderNumber n)
  | .array values => .arr <$> convertArray values.toList #[]
  | .object fields => .obj <$> convertFields fields {}
where
  convertArray : List Value → Array Lean.Json → Except String (Array Lean.Json)
    | [], acc => .ok acc
    | v :: rest, acc => do
      convertArray rest (acc.push (← v.toLeanJson))
  convertFields : List (String × Value) → Std.TreeMap.Raw String Lean.Json compare →
      Except String (Std.TreeMap.Raw String Lean.Json compare)
    | [], acc => .ok acc
    | (k, v) :: rest, acc => do
      convertFields rest (acc.insert k (← v.toLeanJson))

/-- Lean core's JSON as linen's: exact, except that a number a double cannot
    hold is rounded to the nearest one. -/
def Value.ofLeanJson (j : Lean.Json) : Except String Value :=
  Decode.decode j.compress

/-- linen's JSON through Lean core's classes, so it can be used wherever
    `ToJson`/`FromJson` is expected. A non-finite number, which has no JSON
    form, becomes `null` — as linen's own encoder writes it. -/
instance : Lean.ToJson Value := ⟨fun v => v.toLeanJson.toOption.getD .null⟩
instance : Lean.FromJson Value := ⟨Value.ofLeanJson⟩

end Data.Json
