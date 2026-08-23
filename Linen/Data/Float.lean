/-
  Linen.Data.Float — parsing floating-point numbers from text

  Lean core has `String.toNat?` and `String.toInt?` but no `String.toFloat?`,
  so every module that needed one grew its own. This is the shared
  implementation those now use.

  ## Provenance
  Consolidated from three existing copies inside the library:

  * `Linen.Data.Json.Decode.parseNumber` — a streaming parser over a character
    list, with the RFC 8259 no-leading-zeros rule. It keeps its own scanning
    (it must return the unconsumed input, and carries the termination proof
    that the JSON parser needs) but now shares `ofScientificParts`.
  * `Linen.Database.SQL.Decoders.parseFloat?` — a whole-string parser.
  * `Linen.Data.Yaml` — the same, for core-schema scalar resolution.

  The last two were identical and are now this module.

  ## Precision
  Assembly goes through `Float.ofScientific`, the same primitive the compiler
  uses for float literals, so a parsed value agrees with the corresponding
  literal. Digits are accumulated in `Nat`, which is unbounded, so a long
  mantissa loses precision only in the final conversion — not during scanning.
-/

namespace Data.Float

/-- Assemble a float from its sign, mantissa digits and net decimal exponent:
    $$(-1)^{s} \times m \times 10^{e}$$

    This is the step every float parser ends with, and the only part the JSON
    decoder shares — it does its own scanning. -/
def ofScientificParts (negative : Bool) (mantissa : Nat) (netExp : Int) : _root_.Float :=
  let magnitude :=
    if netExp ≥ 0 then _root_.Float.ofScientific mantissa false netExp.toNat
    else _root_.Float.ofScientific mantissa true (-netExp).toNat
  if negative then -magnitude else magnitude

private def digitVal (c : Char) : Nat := c.toNat - 48

private def isDigit (c : Char) : Bool := c ≥ '0' && c ≤ '9'

/-- Parse a decimal float: optional sign, integer part, optional fraction,
    optional exponent. Surrounding whitespace is ignored.

    Returns `none` unless the *whole* string is consumed, so `"1.5x"` is
    rejected rather than silently read as `1.5`. -/
def parseFloat? (s : String) : Option _root_.Float :=
  let chars := s.trimAscii.toString.toList
  if chars.isEmpty then none
  else
    let (negative, rest) := match chars with
      | '-' :: cs => (true, cs)
      | '+' :: cs => (false, cs)
      | cs        => (false, cs)
    let (intDigits, afterInt) := rest.span isDigit
    let (fracDigits, afterFrac) := match afterInt with
      | '.' :: cs => cs.span isDigit
      | cs        => ([], cs)
    if intDigits.isEmpty && fracDigits.isEmpty then none
    else
      let parsedExp := match afterFrac with
        | 'e' :: more | 'E' :: more =>
          let (expNeg, ds) := match more with
            | '-' :: d => (true, d)
            | '+' :: d => (false, d)
            | d        => (false, d)
          let (expDigits, trailing) := ds.span isDigit
          if !trailing.isEmpty || expDigits.isEmpty then none
          else some (expNeg, expDigits.foldl (fun a c => a * 10 + digitVal c) 0)
        | [] => some (false, 0)
        | _  => none      -- trailing junk: reject rather than truncate
      parsedExp.map fun (expNeg, ev) =>
        let mantissa := (intDigits ++ fracDigits).foldl (fun a c => a * 10 + digitVal c) 0
        let netExp : Int :=
          (if expNeg then -(ev : Int) else (ev : Int)) - (fracDigits.length : Int)
        ofScientificParts negative mantissa netExp

end Data.Float
