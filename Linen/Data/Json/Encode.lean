/-
  Linen.Data.Json.Encode — JSON encoding (Value → String)

  Renders a `Value` as a valid JSON string with proper escaping.

  $$\text{encode} : \text{Value} \to \text{String}$$

  ## Escaping
  Strings are escaped per RFC 8259 §7: `"`, `\\`, `/`, `\b`, `\f`,
  `\n`, `\r`, `\t`, and `\uXXXX` for control characters below U+0020.
  -/

import Linen.Data.Json.Types

namespace Data.Json.Encode

open Data.Json

-- ── String escaping ───────────────────────────────────────────────────

/-- Convert a nibble (0..15) to its hex character. -/
private def hexDigit (n : UInt32) : Char :=
  if n < 10 then Char.ofNat (48 + n.toNat)   -- '0'..'9'
  else Char.ofNat (87 + n.toNat)              -- 'a'..'f'

/-- Format a Unicode code point as `\uXXXX`. -/
private def unicodeEscape (c : Char) : String :=
  let n := c.toNat.toUInt32
  let d3 := hexDigit ((n >>> 12) &&& 0xF)
  let d2 := hexDigit ((n >>> 8) &&& 0xF)
  let d1 := hexDigit ((n >>> 4) &&& 0xF)
  let d0 := hexDigit (n &&& 0xF)
  "\\u" ++ String.ofList [d3, d2, d1, d0]

/-- Escape a single character for JSON string output. -/
private def escapeChar (c : Char) : String :=
  match c with
  | '"'  => "\\\""
  | '\\' => "\\\\"
  | '/'  => "\\/"
  | '\x08' => "\\b"   -- backspace
  | '\x0C' => "\\f"   -- form feed
  | '\n' => "\\n"
  | '\r' => "\\r"
  | '\t' => "\\t"
  | c =>
    if c.toNat < 0x20 then unicodeEscape c
    else c.toString

/-- Escape a string for JSON output, wrapping in double quotes.
    $$\text{escapeString} : \text{String} \to \text{String}$$ -/
def escapeString (s : String) : String := Id.run do
  let mut result := "\""
  for c in s.toList do
    result := result ++ escapeChar c
  return result ++ "\""

-- ── Number rendering ──────────────────────────────────────────────────

/-- Convert a `Float` to `Int` by truncating toward zero. -/
private def floatToInt (f : Float) : Int :=
  let s := toString f
  let intPart := match s.splitOn "." with
    | [whole] => whole
    | [whole, _] => whole
    | _ => s
  intPart.toInt!

/-- Render a `Float` as a JSON number.
    Integer-valued floats are rendered without a decimal point (e.g., `42`).
    $$\text{renderNumber} : \text{Float} \to \text{String}$$ -/
def renderNumber (n : Float) : String :=
  if n.isNaN then "null"        -- JSON has no NaN; encode as null
  else if n.isInf then "null"   -- JSON has no Infinity; encode as null
  else
    let i := floatToInt n
    if Float.ofInt i == n then
      toString i
    else
      toString n

-- ── Value encoding ────────────────────────────────────────────────────

/-- Encode a JSON `Value` as a `String`.
    $$\text{encode} : \text{Value} \to \text{String}$$ -/
partial def encode : Value → String
  | .null => "null"
  | .bool true => "true"
  | .bool false => "false"
  | .string s => escapeString s
  | .number n => renderNumber n
  | .array elems =>
    let inner := ",".intercalate (elems.toList.map encode)
    "[" ++ inner ++ "]"
  | .object fields =>
    let inner := ",".intercalate (fields.map fun (k, v) =>
      escapeString k ++ ":" ++ encode v)
    "{" ++ inner ++ "}"

/-- Pretty-print helper: render a value with indentation at a given nesting level. -/
private partial def encodePrettyGo (indent : Nat) (level : Nat) : Value → String
  | .null => "null"
  | .bool true => "true"
  | .bool false => "false"
  | .string s => escapeString s
  | .number n => renderNumber n
  | .array elems =>
    if elems.isEmpty then "[]"
    else
      let pad := String.ofList (List.replicate ((level + 1) * indent) ' ')
      let padClose := String.ofList (List.replicate (level * indent) ' ')
      let inner := (",\n").intercalate
        (elems.toList.map fun e => pad ++ encodePrettyGo indent (level + 1) e)
      "[\n" ++ inner ++ "\n" ++ padClose ++ "]"
  | .object fields =>
    if fields.isEmpty then "{}"
    else
      let pad := String.ofList (List.replicate ((level + 1) * indent) ' ')
      let padClose := String.ofList (List.replicate (level * indent) ' ')
      let inner := (",\n").intercalate
        (fields.map fun (k, v) =>
          pad ++ escapeString k ++ ": " ++ encodePrettyGo indent (level + 1) v)
      "{\n" ++ inner ++ "\n" ++ padClose ++ "}"

/-- Pretty-print a JSON `Value` with indentation.
    $$\text{encodePretty} : \text{Value} \to \text{String}$$ -/
def encodePretty (v : Value) (indent : Nat := 2) : String :=
  encodePrettyGo indent 0 v

end Data.Json.Encode
