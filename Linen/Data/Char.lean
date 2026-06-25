/-
  Linen.Data.Char — Character classification and conversion

  Supplements Lean's built-in `Char` with the Haskell `Data.Char` predicates and
  conversions that core lacks. Lives in `Data.Char'` (primed) to avoid shadowing
  the core `Char` type.

  Already in core, so **not** re-ported (use these directly):

  | Haskell        | Lean core        |
  |----------------|------------------|
  | `isAlpha`/`isDigit`/`isLower`/`isUpper` | same names on `Char` |
  | `isSpace`      | `Char.isWhitespace` |
  | `isAlphaNum`   | `Char.isAlphanum`   |
  | `isHexDigit`   | `Char.isHexDigit`   |
  | `toLower`/`toUpper` | same names      |
  | `ord`          | `Char.toNat`        |
  | `chr`          | `Char.ofNat`        |
-/

namespace Data.Char'

-- ── Predicates ──────────────────────────────────

/-- Test if a character is in the ASCII range $[0, 128)$. -/
@[inline] def isAscii (c : Char) : Bool := c.toNat < 128

/-- Test if a character is in the Latin-1 range $[0, 256)$. -/
@[inline] def isLatin1 (c : Char) : Bool := c.toNat < 256

/-- Test if a character is a control character (codes 0--31 or 127). -/
@[inline] def isControl (c : Char) : Bool := c.toNat < 32 || c.toNat == 127

/-- Test if a character is printable (ASCII, non-control).
    Note: a simplified ASCII-only version. -/
@[inline] def isPrint (c : Char) : Bool := !isControl c && c.toNat < 128

/-- Test if a character is an octal digit: `0`--`7`. -/
@[inline] def isOctDigit (c : Char) : Bool := c.toNat >= 48 && c.toNat <= 55

/-- Test if a character is an ASCII uppercase letter: `A`--`Z`. -/
@[inline] def isAsciiUpper (c : Char) : Bool := c.toNat >= 65 && c.toNat <= 90

/-- Test if a character is an ASCII lowercase letter: `a`--`z`. -/
@[inline] def isAsciiLower (c : Char) : Bool := c.toNat >= 97 && c.toNat <= 122

/-- Test if a character is ASCII punctuation.
    Covers ASCII punctuation ranges: 33--47, 58--64, 91--96, 123--126. -/
@[inline] def isPunctuation (c : Char) : Bool :=
  let n := c.toNat
  (n >= 33 && n <= 47) || (n >= 58 && n <= 64) || (n >= 91 && n <= 96) || (n >= 123 && n <= 126)

-- ── Conversion ──────────────────────────────────

/-- Convert a hex digit character to its numeric value, bounded below 16.

    Returns `Option {n : Nat // n < 16}` — the proof that the digit is in $[0, 15]$
    is carried in the subtype and erased at runtime. -/
def digitToInt (c : Char) : Option {n : Nat // n < 16} :=
  let n := c.toNat
  if h1 : n >= 48 && n <= 57 then
    have h1a : n ≥ 48 := by simp [Bool.and_eq_true] at h1; exact h1.1
    have h1b : n ≤ 57 := by simp [Bool.and_eq_true] at h1; exact h1.2
    some ⟨n - 48, by omega⟩
  else if h2 : n >= 65 && n <= 70 then
    have h2a : n ≥ 65 := by simp [Bool.and_eq_true] at h2; exact h2.1
    have h2b : n ≤ 70 := by simp [Bool.and_eq_true] at h2; exact h2.2
    some ⟨n - 55, by omega⟩
  else if h3 : n >= 97 && n <= 102 then
    have h3a : n ≥ 97 := by simp [Bool.and_eq_true] at h3; exact h3.1
    have h3b : n ≤ 102 := by simp [Bool.and_eq_true] at h3; exact h3.2
    some ⟨n - 87, by omega⟩
  else none

/-- Convert a number in $[0, 15]$ to a hex digit character. Total — no `Option` needed.
    The proof obligation `n < 16` is required at the call site and erased at runtime. -/
def intToDigit (n : Nat) (_h : n < 16 := by omega) : Char :=
  if n <= 9 then Char.ofNat (48 + n)
  else Char.ofNat (87 + n)

-- ── Proofs ──────────────────────────────────────

/-- `isAscii c = true` implies `c.toNat < 128`. -/
theorem isAscii_bound (c : Char) (h : isAscii c = true) : c.toNat < 128 := by
  simp [isAscii] at h
  exact h

/-- `isAscii` is true iff the code point is below 128. -/
theorem isAscii_iff (c : Char) : isAscii c = true ↔ c.toNat < 128 := by
  simp [isAscii]

/-- Roundtrip: `digitToInt (intToDigit n) = some ⟨n, h⟩` for all `n < 16`. -/
theorem digitToInt_intToDigit (n : Nat) (h : n < 16) :
    digitToInt (intToDigit n h) = some ⟨n, h⟩ := by
  have : n = 0 ∨ n = 1 ∨ n = 2 ∨ n = 3 ∨ n = 4 ∨ n = 5 ∨ n = 6 ∨ n = 7 ∨
         n = 8 ∨ n = 9 ∨ n = 10 ∨ n = 11 ∨ n = 12 ∨ n = 13 ∨ n = 14 ∨ n = 15 := by omega
  rcases this with rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl |
    rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl <;> rfl

/-- `intToDigit` always produces an ASCII character. -/
theorem intToDigit_isAscii (n : Nat) (h : n < 16) : isAscii (intToDigit n h) = true := by
  have : n = 0 ∨ n = 1 ∨ n = 2 ∨ n = 3 ∨ n = 4 ∨ n = 5 ∨ n = 6 ∨ n = 7 ∨
         n = 8 ∨ n = 9 ∨ n = 10 ∨ n = 11 ∨ n = 12 ∨ n = 13 ∨ n = 14 ∨ n = 15 := by omega
  rcases this with rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl |
    rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl <;> rfl

end Data.Char'
