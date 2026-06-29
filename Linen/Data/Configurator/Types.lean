/-
  Linen.Data.Configurator.Types — configuration value types

  A typed representation of configuration values and a configuration map,
  mirroring Haskell's `Data.Configurator.Types` (from `configurator-pg`).

  $$\text{Value} ::= \text{string}\ s \mid \text{number}\ n \mid \text{bool}\ b \mid \text{list}\ vs$$
  $$\text{Config} := \text{HashMap String Value}$$
-/

import Std.Data.HashMap

namespace Data.Configurator

/-- A typed configuration value.
    $$\text{Value} ::= \text{string}\ s \mid \text{number}\ n \mid \text{bool}\ b \mid \text{list}\ vs$$ -/
inductive Value where
  /-- A string configuration value. -/
  | string (s : String)
  /-- A numeric configuration value. -/
  | number (n : Float)
  /-- A boolean configuration value. -/
  | bool (b : Bool)
  /-- A list of configuration values. -/
  | list (vs : List Value)
  deriving BEq, Repr

/-- Render a `Value` as a human-readable string. Structural recursion over the
    nested `List Value` via the `where` helper (no `partial`). -/
def Value.toString : Value → String
  | .string s => s!"\"{s}\""
  | .number n => ToString.toString n
  | .bool b => ToString.toString b
  | .list vs => "[" ++ toStringList vs ++ "]"
where
  toStringList : List Value → String
    | [] => ""
    | [v] => Value.toString v
    | v :: vs => Value.toString v ++ ", " ++ toStringList vs

instance : ToString Value where
  toString := Value.toString

/-- A configuration is a map from dotted keys to values.
    $$\text{Config} := \text{HashMap String Value}$$ -/
abbrev Config := Std.HashMap String Value

instance : ToString Config where
  toString c := "\n".intercalate (c.toList.map (fun (k, v) => s!"{k} = {v}"))

end Data.Configurator
