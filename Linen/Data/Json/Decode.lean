/-
  Linen.Data.Json.Decode — JSON decoding (String → Value)

  Recursive-descent parser for JSON (RFC 8259).

  $$\text{decode} : \text{String} \to \text{Except String Value}$$

  Handles:
  - Objects, arrays, strings, numbers, booleans, null
  - String escape sequences: `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`
  - Negative numbers, integer and floating-point literals
  - Whitespace skipping per RFC 8259 §2

-/
import Linen.Data.Json.Types

namespace Data.Json.Decode

open Data.Json

-- ── Parser state ──────────────────────────────────────────────────────

/-- Parser state: the input string and a raw byte position index. -/
structure PState where
  input : String
  pos : String.Pos.Raw
  deriving Repr

/-- Parser result: success with value and updated state, or error. -/
abbrev PResult (α : Type) := Except String (α × PState)

-- ── Helpers ───────────────────────────────────────────────────────────

/-- Get the current character, or `none` if at end. -/
private def peek (s : PState) : Option Char :=
  if s.pos < s.input.endPos.offset then some (s.pos.get s.input)
  else none

/-- Advance position by one character. -/
private def advance (s : PState) : PState :=
  { s with pos := s.pos.next s.input }

/-- Check if parser has reached end of input. -/
private def atEnd (s : PState) : Bool :=
  s.pos >= s.input.endPos.offset

/-- Skip JSON whitespace (space, tab, newline, carriage return). -/
private def skipWhitespace (s : PState) : PState := Id.run do
  let mut st := s
  while !atEnd st do
    match peek st with
    | some ' ' | some '\t' | some '\n' | some '\r' => st := advance st
    | _ => break
  return st

/-- Consume a specific character or fail. -/
private def expect (c : Char) (s : PState) : PResult Unit :=
  match peek s with
  | some c' =>
    if c' == c then .ok ((), advance s)
    else .error s!"expected '{c}', got '{c'}' at position {s.pos.byteIdx}"
  | none => .error s!"expected '{c}', got end of input"

/-- Consume a specific string literal or fail. -/
private def expectLit (lit : String) (s : PState) : PResult Unit := Id.run do
  let mut st := s
  for c in lit.toList do
    match peek st with
    | some c' =>
      if c' == c then st := advance st
      else return .error s!"expected '{lit}' at position {s.pos.byteIdx}"
    | none => return .error s!"unexpected end of input while parsing '{lit}'"
  return .ok ((), st)

-- ── String parsing ────────────────────────────────────────────────────

/-- Parse a hex digit character to its numeric value. -/
private def hexVal (c : Char) : Option UInt32 :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat.toUInt32 - 48)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat.toUInt32 - 87)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat.toUInt32 - 55)
  else none

/-- Parse exactly 4 hex digits into a UInt32. -/
private def parseHex4 (s : PState) : PResult UInt32 := Id.run do
  let mut st := s
  let mut n : UInt32 := 0
  for _ in List.range 4 do
    match peek st with
    | some c =>
      match hexVal c with
      | some d => n := n * 16 + d; st := advance st
      | none => return .error s!"invalid hex digit '{c}' at position {st.pos.byteIdx}"
    | none => return .error s!"unexpected end of input in unicode escape"
  return .ok (n, st)

/-- Parse a unicode escape (after `\u` has been consumed).
    Handles surrogate pairs. -/
private def parseUnicodeEscape (st : PState) : PResult Char :=
  match parseHex4 st with
  | .error e => .error e
  | .ok (code, st) =>
    if 0xD800 ≤ code && code ≤ 0xDBFF then
      match expect '\\' st with
      | .error e => .error e
      | .ok (_, st) =>
        match expect 'u' st with
        | .error e => .error e
        | .ok (_, st) =>
          match parseHex4 st with
          | .error e => .error e
          | .ok (low, st) =>
            if 0xDC00 ≤ low && low ≤ 0xDFFF then
              let cp := ((code - 0xD800) * 1024 + (low - 0xDC00) + 0x10000)
              .ok (Char.ofNat cp.toNat, st)
            else
              .error s!"invalid surrogate pair"
    else
      .ok (Char.ofNat code.toNat, st)

/-- Inner loop for string parsing: accumulate characters until closing `"`. -/
private partial def parseStringLoop (st : PState) (chars : List Char) : PResult String :=
  match peek st with
  | none => .error "unterminated string"
  | some '"' =>
    .ok (String.ofList chars.reverse, advance st)
  | some '\\' =>
    let st := advance st
    match peek st with
    | none => .error "unterminated escape sequence"
    | some '"'  => parseStringLoop (advance st) ('"' :: chars)
    | some '\\' => parseStringLoop (advance st) ('\\' :: chars)
    | some '/'  => parseStringLoop (advance st) ('/' :: chars)
    | some 'b'  => parseStringLoop (advance st) ('\x08' :: chars)
    | some 'f'  => parseStringLoop (advance st) ('\x0C' :: chars)
    | some 'n'  => parseStringLoop (advance st) ('\n' :: chars)
    | some 'r'  => parseStringLoop (advance st) ('\r' :: chars)
    | some 't'  => parseStringLoop (advance st) ('\t' :: chars)
    | some 'u'  =>
      match parseUnicodeEscape (advance st) with
      | .ok (ch, st') => parseStringLoop st' (ch :: chars)
      | .error e => .error e
    | some c => .error s!"unknown escape '\\{c}'"
  | some c =>
    parseStringLoop (advance st) (c :: chars)

/-- Parse a JSON string (opening `"` already expected at current position).
    Handles all escape sequences including `\uXXXX` and surrogate pairs. -/
private def parseString (s : PState) : PResult String :=
  match expect '"' s with
  | .error e => .error e
  | .ok (_, initSt) => parseStringLoop initSt []

-- ── Number parsing ────────────────────────────────────────────────────

/-- Convert a list of digit characters to a `Nat`. -/
private def digitsToNat (digits : List Char) : Nat :=
  digits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

/-- Parse a JSON number (integer or floating-point, possibly negative).
    Grammar: `-?` digits (`.` digits)? (`[eE]` `[+-]?` digits)? -/
private def parseNumber (s : PState) : PResult Float := Id.run do
  let mut st := s
  let mut neg := false
  -- optional minus
  if peek st == some '-' then
    neg := true; st := advance st
  -- integer part (at least one digit)
  let startPos := st.pos
  let mut intDigits : List Char := []
  while !atEnd st do
    match peek st with
    | some c =>
      if '0' ≤ c && c ≤ '9' then intDigits := intDigits ++ [c]; st := advance st
      else break
    | none => break
  if st.pos == startPos then
    return .error s!"expected digit at position {st.pos.byteIdx}"
  -- fractional part
  let mut fracDigits : List Char := []
  if peek st == some '.' then
    st := advance st
    let fracStart := st.pos
    while !atEnd st do
      match peek st with
      | some c =>
        if '0' ≤ c && c ≤ '9' then fracDigits := fracDigits ++ [c]; st := advance st
        else break
      | none => break
    if st.pos == fracStart then
      return .error s!"expected digit after '.' at position {st.pos.byteIdx}"
  -- exponent part
  let mut expNeg := false
  let mut expDigits : List Char := []
  let mut hasExp := false
  match peek st with
  | some 'e' | some 'E' =>
    hasExp := true; st := advance st
    match peek st with
    | some '+' => st := advance st
    | some '-' => expNeg := true; st := advance st
    | _ => pure ()
    let expStart := st.pos
    while !atEnd st do
      match peek st with
      | some c =>
        if '0' ≤ c && c ≤ '9' then expDigits := expDigits ++ [c]; st := advance st
        else break
      | none => break
    if st.pos == expStart then
      return .error s!"expected digit in exponent at position {st.pos.byteIdx}"
  | _ => pure ()
  -- Assemble the float
  -- Compute mantissa = intPart * 10^fracLen + fracPart
  let intPart := digitsToNat intDigits
  let fracLen := fracDigits.length
  let fracPart := digitsToNat fracDigits
  -- Build as: (intPart * 10^fracLen + fracPart) * 10^(exp - fracLen)
  let mantissa := intPart * (10 ^ fracLen) + fracPart
  let expVal := if hasExp then digitsToNat expDigits else 0
  -- Net exponent = expVal - fracLen (with sign)
  let netExpPos := if expNeg then 0 else expVal
  let netExpNeg := fracLen + (if expNeg then expVal else 0)
  -- Use Float.ofScientific: ofScientific m true e = m * 10^(-e), ofScientific m false e = m * 10^e
  let f := if netExpNeg > netExpPos then
    Float.ofScientific mantissa true (netExpNeg - netExpPos)
  else
    Float.ofScientific mantissa false (netExpPos - netExpNeg)
  let f := if neg then -f else f
  return .ok (f, st)

-- ── Main parser ───────────────────────────────────────────────────────

/-- Parse a JSON value from the current position.
    Uses a fuel parameter to guarantee termination. -/
private partial def parseValue : Nat → PState → PResult Value
  | 0, _ => .error "recursion limit exceeded"
  | fuel + 1, s =>
    let st := skipWhitespace s
    match peek st with
    | none => .error "unexpected end of input"
    | some '"' =>
      match parseString st with
      | .error e => .error e
      | .ok (str, st') => .ok (.string str, st')
    | some 't' =>
      match expectLit "true" st with
      | .error e => .error e
      | .ok (_, st') => .ok (.bool true, st')
    | some 'f' =>
      match expectLit "false" st with
      | .error e => .error e
      | .ok (_, st') => .ok (.bool false, st')
    | some 'n' =>
      match expectLit "null" st with
      | .error e => .error e
      | .ok (_, st') => .ok (.null, st')
    | some '[' => parseArray fuel st
    | some '{' => parseObject fuel st
    | some c =>
      if c == '-' || ('0' ≤ c && c ≤ '9') then
        match parseNumber st with
        | .error e => .error e
        | .ok (n, st') => .ok (.number n, st')
      else
        .error s!"unexpected character '{c}' at position {st.pos.byteIdx}"
where
  /-- Parse a JSON array `[v1, v2, ...]`. -/
  parseArray (fuel : Nat) (s : PState) : PResult Value :=
    match expect '[' s with
    | .error e => .error e
    | .ok (_, st) =>
      let st := skipWhitespace st
      if peek st == some ']' then
        .ok (.array #[], advance st)
      else
        arrayLoop fuel st #[]

  /-- Accumulate array elements until `]` is found. -/
  arrayLoop (fuel : Nat) (st : PState) (elems : Array Value) : PResult Value :=
    match parseValue fuel st with
    | .error e => .error e
    | .ok (v, st) =>
      let elems := elems.push v
      let st := skipWhitespace st
      match peek st with
      | some ',' =>
        let st := skipWhitespace (advance st)
        arrayLoop fuel st elems
      | some ']' => .ok (.array elems, advance st)
      | _ => .error s!"expected ',' or ']' at position {st.pos.byteIdx}"

  /-- Parse a JSON object `{key: value, ...}`. -/
  parseObject (fuel : Nat) (s : PState) : PResult Value :=
    match expect '{' s with
    | .error e => .error e
    | .ok (_, st) =>
      let st := skipWhitespace st
      if peek st == some '}' then
        .ok (.object [], advance st)
      else
        objectLoop fuel st []

  /-- Accumulate object fields until `}` is found. -/
  objectLoop (fuel : Nat) (st : PState) (fields : List (String × Value))
      : PResult Value :=
    match parseString st with
    | .error e => .error e
    | .ok (key, st) =>
      let st := skipWhitespace st
      match expect ':' st with
      | .error e => .error e
      | .ok (_, st) =>
        let st := skipWhitespace st
        match parseValue fuel st with
        | .error e => .error e
        | .ok (v, st) =>
          let fields := fields ++ [(key, v)]
          let st := skipWhitespace st
          match peek st with
          | some ',' =>
            let st := skipWhitespace (advance st)
            objectLoop fuel st fields
          | some '}' => .ok (.object fields, advance st)
          | _ => .error s!"expected ',' or '}}' at position {st.pos.byteIdx}"

-- ── Public API ────────────────────────────────────────────────────────

/-- Maximum parse recursion depth. -/
private def maxDepth : Nat := 512

/-- Decode a JSON string into a `Value`.
    $$\text{decode} : \text{String} \to \text{Except String Value}$$ -/
def decode (input : String) : Except String Value :=
  let s : PState := { input, pos := ⟨0⟩ }
  match parseValue maxDepth s with
  | .error e => .error e
  | .ok (v, st) =>
    let st := skipWhitespace st
    if atEnd st then .ok v
    else .error s!"trailing content at position {st.pos.byteIdx}"

/-- Decode a JSON string and parse it into a typed value via `FromJSON`.
    $$\text{decodeAs} : \text{String} \to \text{Except String}\ \alpha$$ -/
def decodeAs [FromJSON α] (input : String) : Except String α :=
  match decode input with
  | .error e => .error e
  | .ok v => FromJSON.parseJSON v

end Data.Json.Decode
