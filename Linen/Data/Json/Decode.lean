/-
  Linen.Data.Json.Decode — JSON decoding (String → Value)

  Recursive-descent parser for JSON (RFC 8259).

  $$\text{decode} : \text{String} \to \text{Except String Value}$$

  Handles:
  - Objects, arrays, strings, numbers, booleans, null
  - String escape sequences: `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`
  - Negative numbers, integer and floating-point literals
  - Whitespace skipping per RFC 8259 §2

  Termination is established without `partial` and without a fuel parameter.
  The input is carried as a `List Char` cursor and every recursive call is
  proven to shrink the remaining list (`List.length`). Each parser returns its
  remaining input bundled with a proof that it did not grow (`≤`) — and, where
  it always consumes at least one character, that it strictly shrank (`<`).
  The mutually recursive value/array/object parsers rely on that `<` invariant
  to discharge their `decreasing_by` obligations.
-/
import Linen.Data.Json.Types

namespace Data.Json.Decode

open Data.Json

-- ── Character classes ─────────────────────────────────────────────────

/-- ASCII decimal digit. -/
private def isDigit (c : Char) : Bool := '0' ≤ c && c ≤ '9'

/-- JSON whitespace (space, tab, newline, carriage return). -/
private def isWs (c : Char) : Bool := c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- ── Span helper ───────────────────────────────────────────────────────

/-- Split a list into the longest prefix satisfying `p` and the remainder. -/
private def spanList (p : Char → Bool) : List Char → List Char × List Char
  | [] => ([], [])
  | c :: cs =>
    if p c then
      let (a, b) := spanList p cs
      (c :: a, b)
    else ([], c :: cs)

/-- The remainder of `spanList` is no longer than the input. -/
private theorem spanList_le (p : Char → Bool) :
    ∀ l : List Char, (spanList p l).2.length ≤ l.length
  | [] => by simp [spanList]
  | c :: cs => by
    simp only [spanList]
    split
    · have ih := spanList_le p cs
      simpa using Nat.le_succ_of_le ih
    · simp

/-- If `spanList` consumed at least one character, the remainder is strictly
    shorter than the input. -/
private theorem spanList_nonempty_lt (p : Char → Bool) (l : List Char)
    (h : (spanList p l).1 ≠ []) : (spanList p l).2.length < l.length := by
  match l with
  | [] => simp [spanList] at h
  | c :: cs =>
    simp only [spanList] at h ⊢
    split
    · have ih := spanList_le p cs
      simpa using Nat.lt_succ_of_le ih
    · rename_i hpc; simp [hpc] at h

/-- Skip leading JSON whitespace, returning the remaining input. -/
private def skipWhitespace (inp : List Char) : List Char := (spanList isWs inp).2

private theorem skipWhitespace_le (inp : List Char) :
    (skipWhitespace inp).length ≤ inp.length := spanList_le isWs inp

-- ── Single-character expectation ──────────────────────────────────────

/-- Consume a specific character or fail. -/
private def expectC (c : Char) : List Char → Except String (Unit × List Char)
  | c' :: t => if c' == c then .ok ((), t) else .error s!"expected '{c}', got '{c'}'"
  | [] => .error s!"expected '{c}', got end of input"

/-- A successful `expectC` strictly shrinks the input. -/
private theorem expectC_lt {c : Char} {inp r : List Char} {u : Unit}
    (h : expectC c inp = .ok (u, r)) : r.length < inp.length := by
  match inp with
  | [] => simp [expectC] at h
  | c' :: t =>
    simp only [expectC] at h
    split at h
    · injection h with h; injection h with _ h; subst h; simp
    · simp at h

-- ── Hex / unicode escapes ─────────────────────────────────────────────

/-- Parse a hex digit character to its numeric value. -/
private def hexVal (c : Char) : Option UInt32 :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat.toUInt32 - 48)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat.toUInt32 - 87)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat.toUInt32 - 55)
  else none

/-- Parse exactly 4 hex digits into a UInt32. -/
private def parseHex4 : List Char → Except String (UInt32 × List Char)
  | c1 :: c2 :: c3 :: c4 :: rest =>
    match hexVal c1, hexVal c2, hexVal c3, hexVal c4 with
    | some d1, some d2, some d3, some d4 =>
      .ok (((d1 * 16 + d2) * 16 + d3) * 16 + d4, rest)
    | _, _, _, _ => .error "invalid hex digit in unicode escape"
  | _ => .error "unexpected end of input in unicode escape"

/-- A successful `parseHex4` strictly shrinks the input (it consumes 4 chars). -/
private theorem parseHex4_lt {inp r : List Char} {n : UInt32}
    (h : parseHex4 inp = .ok (n, r)) : r.length < inp.length := by
  match inp with
  | [] => simp [parseHex4] at h
  | _ :: [] => simp [parseHex4] at h
  | _ :: _ :: [] => simp [parseHex4] at h
  | _ :: _ :: _ :: [] => simp [parseHex4] at h
  | c1 :: c2 :: c3 :: c4 :: rest =>
    simp only [parseHex4] at h
    split at h
    · injection h with h; injection h with _ h; subst h; simp; omega
    · simp at h

/-- Parse a unicode escape (after `\u` has been consumed).
    Handles surrogate pairs. The remaining input is strictly shorter. -/
private def parseUnicodeEscape (inp : List Char) :
    Except String (Char × { r : List Char // r.length < inp.length }) :=
  match h1 : parseHex4 inp with
  | .error e => .error e
  | .ok (code, r1) =>
    if 0xD800 ≤ code && code ≤ 0xDBFF then
      match h2 : expectC '\\' r1 with
      | .error e => .error e
      | .ok (_, r2) =>
        match h3 : expectC 'u' r2 with
        | .error e => .error e
        | .ok (_, r3) =>
          match h4 : parseHex4 r3 with
          | .error e => .error e
          | .ok (low, r4) =>
            if 0xDC00 ≤ low && low ≤ 0xDFFF then
              let cp := (code - 0xD800) * 1024 + (low - 0xDC00) + 0x10000
              .ok (Char.ofNat cp.toNat,
                ⟨r4, by
                  have a := parseHex4_lt h1
                  have b := expectC_lt h2
                  have c := expectC_lt h3
                  have d := parseHex4_lt h4
                  omega⟩)
            else .error "invalid surrogate pair"
    else
      .ok (Char.ofNat code.toNat, ⟨r1, parseHex4_lt h1⟩)

-- ── String parsing ────────────────────────────────────────────────────

/-- Inner loop for string parsing: accumulate characters until closing `"`.
    Returns its remaining input proven no longer than the input. -/
private def parseStringLoop (acc : List Char) (inp : List Char) :
    Except String (String × { r : List Char // r.length ≤ inp.length }) :=
  match h0 : inp with
  | [] => .error "unterminated string"
  | '"' :: t => .ok (String.ofList acc.reverse, ⟨t, by subst_vars; simp⟩)
  | '\\' :: t =>
    match h1 : t with
    | [] => .error "unterminated escape sequence"
    | '"' :: u =>
      have hdec : u.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
      match parseStringLoop ('"' :: acc) u with
      | .error e => .error e
      | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | '\\' :: u =>
      have hdec : u.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
      match parseStringLoop ('\\' :: acc) u with
      | .error e => .error e
      | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | '/' :: u =>
      have hdec : u.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
      match parseStringLoop ('/' :: acc) u with
      | .error e => .error e
      | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | 'b' :: u =>
      have hdec : u.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
      match parseStringLoop ('\x08' :: acc) u with
      | .error e => .error e
      | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | 'f' :: u =>
      have hdec : u.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
      match parseStringLoop ('\x0C' :: acc) u with
      | .error e => .error e
      | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | 'n' :: u =>
      have hdec : u.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
      match parseStringLoop ('\n' :: acc) u with
      | .error e => .error e
      | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | 'r' :: u =>
      have hdec : u.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
      match parseStringLoop ('\r' :: acc) u with
      | .error e => .error e
      | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | 't' :: u =>
      have hdec : u.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
      match parseStringLoop ('\t' :: acc) u with
      | .error e => .error e
      | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | 'u' :: u =>
      match parseUnicodeEscape u with
      | .error e => .error e
      | .ok (ch, u') =>
        have hp : u'.val.length < u.length := u'.property
        have hdec : u'.val.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
        match parseStringLoop (ch :: acc) u'.val with
        | .error e => .error e
        | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
    | c :: _ => .error s!"unknown escape '\\{c}'"
  | c :: t =>
    have hdec : t.length < inp.length := by subst_vars; simp only [List.length_cons]; omega
    match parseStringLoop (c :: acc) t with
    | .error e => .error e
    | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by subst_vars; simp only [List.length_cons]; omega⟩)
termination_by inp.length
decreasing_by all_goals (simp_wf <;> omega)

/-- Parse a JSON string (opening `"` already expected at current position).
    Handles all escape sequences including `\uXXXX` and surrogate pairs. -/
private def parseString (inp : List Char) :
    Except String (String × { r : List Char // r.length < inp.length }) :=
  match h : expectC '"' inp with
  | .error e => .error e
  | .ok (_, t) =>
    match parseStringLoop [] t with
    | .error e => .error e
    | .ok (s, ⟨r, hr⟩) => .ok (s, ⟨r, by have := expectC_lt h; omega⟩)

-- ── Number parsing ────────────────────────────────────────────────────

/-- Convert a list of digit characters to a `Nat`. -/
private def digitsToNat (digits : List Char) : Nat :=
  digits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

/-- Strip an optional leading `-` sign. -/
private def stripSign (inp : List Char) : Bool × { r : List Char // r.length ≤ inp.length } :=
  match h : inp with
  | '-' :: t => (true, ⟨t, by subst_vars; simp only [List.length_cons]; omega⟩)
  | rest => (false, ⟨rest, Nat.le_refl _⟩)

/-- Strip an optional leading exponent sign (`+` or `-`). -/
private def stripExpSign (inp : List Char) :
    Bool × { r : List Char // r.length ≤ inp.length } :=
  match h : inp with
  | '+' :: u => (false, ⟨u, by subst_vars; simp only [List.length_cons]; omega⟩)
  | '-' :: u => (true, ⟨u, by subst_vars; simp only [List.length_cons]; omega⟩)
  | rest => (false, ⟨rest, Nat.le_refl _⟩)

/-- Parse the optional fractional part `.digits`. -/
private def parseFrac (inp : List Char) :
    Except String (List Char × { r : List Char // r.length ≤ inp.length }) :=
  match h : inp with
  | '.' :: t =>
    if hd : (spanList isDigit t).1 = [] then .error "expected digit after '.'"
    else .ok ((spanList isDigit t).1, ⟨(spanList isDigit t).2, by
      have h2 : (spanList isDigit t).2.length ≤ t.length := spanList_le isDigit t
      subst_vars; simp only [List.length_cons]; omega⟩)
  | rest => .ok ([], ⟨rest, Nat.le_refl _⟩)

/-- Parse the optional exponent part `[eE][+-]?digits`. -/
private def parseExp (inp : List Char) :
    Except String (Bool × List Char × { r : List Char // r.length ≤ inp.length }) :=
  match h : inp with
  | e :: t =>
    if e == 'e' || e == 'E' then
      let es := stripExpSign t
      let expNeg := es.1
      let t1 := es.2.val
      have ht1 : t1.length ≤ t.length := es.2.2
      if hd : (spanList isDigit t1).1 = [] then .error "expected digit in exponent"
      else .ok (expNeg, (spanList isDigit t1).1, ⟨(spanList isDigit t1).2, by
        have h2 : (spanList isDigit t1).2.length ≤ t1.length := spanList_le isDigit t1
        subst_vars; simp only [List.length_cons]; omega⟩)
    else .ok (false, [], ⟨e :: t, Nat.le_refl _⟩)
  | [] => .ok (false, [], ⟨[], Nat.le_refl _⟩)

/-- Parse a JSON number (integer or floating-point, possibly negative).
    Grammar: `-?` digits (`.` digits)? (`[eE]` `[+-]?` digits)?, where the
    integer part must be a single `0` or a non-zero digit followed by digits —
    no leading zeros (RFC 8259 §6). -/
private def parseNumber (inp : List Char) :
    Except String (Float × { r : List Char // r.length < inp.length }) :=
  let s := stripSign inp
  let neg := s.1
  let r0 := s.2.val
  have hr0 : r0.length ≤ inp.length := s.2.2
  if hd : (spanList isDigit r0).1 = [] then .error "expected digit"
  else if (spanList isDigit r0).1.length > 1 ∧ (spanList isDigit r0).1.head? = some '0' then
    .error "leading zeros are not allowed (RFC 8259 §6)"
  else
    match hf : parseFrac (spanList isDigit r0).2 with
    | .error e => .error e
    | .ok (fracDigits, frac) =>
      have hr2 : frac.val.length ≤ (spanList isDigit r0).2.length := frac.property
      match he : parseExp frac.val with
      | .error e => .error e
      | .ok (expNeg, expDigits, ex) =>
        have hr3 : ex.val.length ≤ frac.val.length := ex.property
        let intDigits := (spanList isDigit r0).1
        let intPart := digitsToNat intDigits
        let fracLen := fracDigits.length
        let fracPart := digitsToNat fracDigits
        let mantissa := intPart * (10 ^ fracLen) + fracPart
        let expVal := digitsToNat expDigits
        let netExpPos := if expNeg then 0 else expVal
        let netExpNeg := fracLen + (if expNeg then expVal else 0)
        let fAbs := if netExpNeg > netExpPos then
          Float.ofScientific mantissa true (netExpNeg - netExpPos)
        else
          Float.ofScientific mantissa false (netExpPos - netExpNeg)
        let f := if neg then -fAbs else fAbs
        .ok (f, ⟨ex.val, by
          have h1 : (spanList isDigit r0).2.length < r0.length :=
            spanList_nonempty_lt isDigit r0 hd
          omega⟩)

-- ── Main parser (mutual, terminating on remaining input length) ───────

mutual

/-- Parse a JSON value from the current position. -/
private def parseValue (inp : List Char) :
    Except String (Value × { r : List Char // r.length < inp.length }) :=
  have hr0le : (skipWhitespace inp).length ≤ inp.length := skipWhitespace_le inp
  match h0 : skipWhitespace inp with
  | [] => .error "unexpected end of input"
  | '"' :: _ =>
    match parseString (skipWhitespace inp) with
    | .error e => .error e
    | .ok (s, val) => .ok (.string s, ⟨val.val, by have hp := val.property; omega⟩)
  | 't' :: 'r' :: 'u' :: 'e' :: r =>
    .ok (.bool true, ⟨r, by
      have hle := skipWhitespace_le inp; rw [h0] at hle
      simp only [List.length_cons] at hle; omega⟩)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: r =>
    .ok (.bool false, ⟨r, by
      have hle := skipWhitespace_le inp; rw [h0] at hle
      simp only [List.length_cons] at hle; omega⟩)
  | 'n' :: 'u' :: 'l' :: 'l' :: r =>
    .ok (.null, ⟨r, by
      have hle := skipWhitespace_le inp; rw [h0] at hle
      simp only [List.length_cons] at hle; omega⟩)
  | '[' :: _ =>
    match parseArray (skipWhitespace inp) with
    | .error e => .error e
    | .ok (v, val) => .ok (v, ⟨val.val, by have hp := val.property; omega⟩)
  | '{' :: _ =>
    match parseObject (skipWhitespace inp) with
    | .error e => .error e
    | .ok (v, val) => .ok (v, ⟨val.val, by have hp := val.property; omega⟩)
  | c :: _ =>
    if c == '-' || isDigit c then
      match parseNumber (skipWhitespace inp) with
      | .error e => .error e
      | .ok (n, val) => .ok (.number n, ⟨val.val, by have hp := val.property; omega⟩)
    else .error s!"unexpected character '{c}'"
termination_by 3 * inp.length + 1
decreasing_by all_goals (simp_wf <;> omega)

/-- Parse a JSON array `[v1, v2, ...]`. -/
private def parseArray (inp : List Char) :
    Except String (Value × { r : List Char // r.length < inp.length }) :=
  match h1 : expectC '[' inp with
  | .error e => .error e
  | .ok (_, r1) =>
    have he : r1.length < inp.length := expectC_lt h1
    have hsw : (skipWhitespace r1).length ≤ r1.length := skipWhitespace_le r1
    match hm : skipWhitespace r1 with
    | ']' :: t =>
      .ok (.array #[], ⟨t, by
        have hle := skipWhitespace_le r1; rw [hm] at hle
        simp only [List.length_cons] at hle; omega⟩)
    | _ =>
      match parseArrayLoop (skipWhitespace r1) #[] with
      | .error e => .error e
      | .ok (v, val) => .ok (v, ⟨val.val, by have hp := val.property; omega⟩)
termination_by 3 * inp.length
decreasing_by all_goals (simp_wf <;> omega)

/-- Accumulate array elements until `]` is found. -/
private def parseArrayLoop (inp : List Char) (elems : Array Value) :
    Except String (Value × { r : List Char // r.length < inp.length }) :=
  match parseValue inp with
  | .error e => .error e
  | .ok (v, val) =>
    have h1 : val.val.length < inp.length := val.property
    have hsw : (skipWhitespace val.val).length ≤ val.val.length := skipWhitespace_le val.val
    match hm : skipWhitespace val.val with
    | ',' :: t =>
      have hkey : (skipWhitespace t).length < inp.length := by
        have a := skipWhitespace_le val.val; rw [hm] at a
        simp only [List.length_cons] at a
        have b := skipWhitespace_le t; omega
      match parseArrayLoop (skipWhitespace t) (elems.push v) with
      | .error e => .error e
      | .ok (w, val2) => .ok (w, ⟨val2.val, by have hp := val2.property; omega⟩)
    | ']' :: t =>
      .ok (.array (elems.push v), ⟨t, by
        have a := skipWhitespace_le val.val; rw [hm] at a
        simp only [List.length_cons] at a; omega⟩)
    | _ => .error "expected ',' or ']'"
termination_by 3 * inp.length + 2
decreasing_by all_goals (simp_wf <;> omega)

/-- Parse a JSON object `{key: value, ...}`. -/
private def parseObject (inp : List Char) :
    Except String (Value × { r : List Char // r.length < inp.length }) :=
  match h1 : expectC '{' inp with
  | .error e => .error e
  | .ok (_, r1) =>
    have he : r1.length < inp.length := expectC_lt h1
    have hsw : (skipWhitespace r1).length ≤ r1.length := skipWhitespace_le r1
    match hm : skipWhitespace r1 with
    | '}' :: t =>
      .ok (.object [], ⟨t, by
        have hle := skipWhitespace_le r1; rw [hm] at hle
        simp only [List.length_cons] at hle; omega⟩)
    | _ =>
      match parseObjectLoop (skipWhitespace r1) [] with
      | .error e => .error e
      | .ok (v, val) => .ok (v, ⟨val.val, by have hp := val.property; omega⟩)
termination_by 3 * inp.length
decreasing_by all_goals (simp_wf <;> omega)

/-- Accumulate object fields until `}` is found. -/
private def parseObjectLoop (inp : List Char) (fields : List (String × Value)) :
    Except String (Value × { r : List Char // r.length < inp.length }) :=
  match hk : parseString inp with
  | .error e => .error e
  | .ok (key, kval) =>
    have hk1 : kval.val.length < inp.length := kval.property
    have hsw1 : (skipWhitespace kval.val).length ≤ kval.val.length := skipWhitespace_le kval.val
    match hc : expectC ':' (skipWhitespace kval.val) with
    | .error e => .error e
    | .ok (_, r2) =>
      have hc2 : r2.length < (skipWhitespace kval.val).length := expectC_lt hc
      have hkey : (skipWhitespace r2).length < inp.length := by
        have b := skipWhitespace_le r2; omega
      match parseValue (skipWhitespace r2) with
      | .error e => .error e
      | .ok (v, vval) =>
        have h3 : vval.val.length < (skipWhitespace r2).length := vval.property
        have hsw2 : (skipWhitespace vval.val).length ≤ vval.val.length := skipWhitespace_le vval.val
        match hm : skipWhitespace vval.val with
        | ',' :: t =>
          have hkey2 : (skipWhitespace t).length < inp.length := by
            have a := skipWhitespace_le vval.val; rw [hm] at a
            simp only [List.length_cons] at a
            have b := skipWhitespace_le t; omega
          match parseObjectLoop (skipWhitespace t) (fields ++ [(key, v)]) with
          | .error e => .error e
          | .ok (w, val2) => .ok (w, ⟨val2.val, by have hp := val2.property; omega⟩)
        | '}' :: t =>
          .ok (.object (fields ++ [(key, v)]), ⟨t, by
            have a := skipWhitespace_le vval.val; rw [hm] at a
            simp only [List.length_cons] at a; omega⟩)
        | _ => .error "expected ',' or '}'"
termination_by 3 * inp.length + 2
decreasing_by all_goals (simp_wf <;> omega)

end

-- ── Public API ────────────────────────────────────────────────────────

/-- Decode a JSON string into a `Value`.
    $$\text{decode} : \text{String} \to \text{Except String Value}$$ -/
def decode (input : String) : Except String Value :=
  match parseValue input.toList with
  | .error e => .error e
  | .ok (v, ⟨rest, _⟩) =>
    if (skipWhitespace rest).isEmpty then .ok v
    else .error "trailing content"

/-- Decode a JSON string and parse it into a typed value via `FromJSON`.
    $$\text{decodeAs} : \text{String} \to \text{Except String}\ \alpha$$ -/
def decodeAs [FromJSON α] (input : String) : Except String α :=
  match decode input with
  | .error e => .error e
  | .ok v => FromJSON.parseJSON v

end Data.Json.Decode
