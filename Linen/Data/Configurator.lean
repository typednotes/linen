/-
  Linen.Data.Configurator — configuration loading and querying

  Loads, parses, and queries configuration files in a simple `key = value`
  format (dotted keys, `#` comments, quoted strings, numbers, booleans).
  Mirrors Haskell's `Data.Configurator` (`configurator-pg`).

  The parsers are **structural recursions** over the character / line lists
  (no `partial`, no `while`/`Id.run`).

  ## Config file format
  ```
  # Comment
  db-uri = "postgres://..."
  server-port = 3000
  debug = true
  ```
-/

import Linen.Data.Configurator.Types

namespace Data.Configurator

/-- The empty configuration. -/
def empty : Config := ∅

/-- Look up a key. -/
def lookup (key : String) (config : Config) : Option Value :=
  config.get? key

/-- Look up a key with a default fallback. -/
def lookupDefault (default_ : Value) (key : String) (config : Config) : Value :=
  (lookup key config).getD default_

/-- Look up a key, erroring if absent. -/
def require (key : String) (config : Config) : Except String Value :=
  match lookup key config with
  | some v => .ok v
  | none => .error s!"Required configuration key not found: {key}"

/-! ── Parsing ── -/

/-- Trim leading/trailing ASCII whitespace. -/
private def trim (s : String) : String := s.trimAscii.toString

/-- Parse the body of a quoted string starting at `pos` (just past the opening
    quote), returning the unescaped contents and the position after the closing
    quote. Structural recursion over the remaining characters. -/
private def parseQuotedString (input : String) (pos : Nat) : Except String (String × Nat) :=
  go (input.toList.drop pos) pos []
where
  go : List Char → Nat → List Char → Except String (String × Nat)
    | [], _, _ => .error "Unterminated string literal"
    | '"' :: _, i, acc => .ok (String.ofList acc.reverse, i + 1)
    | '\\' :: c :: rest, i, acc =>
      let escaped := match c with
        | 'n' => '\n'
        | 't' => '\t'
        | 'r' => '\r'
        | '\\' => '\\'
        | '"' => '"'
        | other => other
      go rest (i + 2) (escaped :: acc)
    | c :: rest, i, acc => go rest (i + 1) (c :: acc)

/-- Parse a value: a quoted string, a boolean (`true`/`false`), or a number
    (integer or `int.frac` float). -/
private def parseValue (s : String) : Except String Value :=
  let s' := trim s
  if s'.isEmpty then
    .error "Empty value"
  else if s'.front == '"' then
    match parseQuotedString s' 1 with
    | .ok (str, _) => .ok (.string str)
    | .error e => .error e
  else if s'.toLower == "true" then .ok (.bool true)
  else if s'.toLower == "false" then .ok (.bool false)
  else
    match s'.toInt? with
    | some i => .ok (.number (Float.ofInt i))
    | none =>
      match s'.splitOn "." with
      | [intPart, fracPart] =>
        match intPart.toInt?, fracPart.toNat? with
        | some i, some f =>
          let fracVal := (Float.ofNat f) / (Float.ofNat (10 ^ fracPart.length))
          let result := if i < 0 then Float.ofInt i - fracVal else Float.ofInt i + fracVal
          .ok (.number result)
        | _, _ => .error s!"Cannot parse value: {s'}"
      | _ => .error s!"Cannot parse value: {s'}"

/-- Parse configuration-file content into a `Config`.

    - `#`-prefixed (after trimming) and empty lines are ignored.
    - `key = value` lines insert under the (dotted) key.
    Structural recursion over the lines. -/
def parseConfig (content : String) : Except String Config :=
  go (content.splitOn "\n") 1 empty
where
  go : List String → Nat → Config → Except String Config
    | [], _, config => .ok config
    | line :: rest, lineNum, config =>
      let trimmed := trim line
      if trimmed.isEmpty || trimmed.startsWith "#" then
        go rest (lineNum + 1) config
      else
        match trimmed.splitOn "=" with
        | [] => .error s!"Line {lineNum}: empty line after split"
        | [_] => .error s!"Line {lineNum}: missing '=' in '{trimmed}'"
        | key :: valueParts =>
          let keyStr := trim key
          if keyStr.isEmpty then
            .error s!"Line {lineNum}: empty key"
          else
            match parseValue (trim ("=".intercalate valueParts)) with
            | .ok v => go rest (lineNum + 1) (config.insert keyStr v)
            | .error e => .error s!"Line {lineNum}: {e}"

/-- Load and parse a configuration file from disk. -/
def load (path : String) : IO Config := do
  let content ← IO.FS.readFile path
  match parseConfig content with
  | .ok config => return config
  | .error e => throw (IO.userError s!"Failed to parse config file '{path}': {e}")

end Data.Configurator
