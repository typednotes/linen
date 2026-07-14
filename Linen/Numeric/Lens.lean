/-
  Linen.Numeric.Lens — `integral`, `base`, `binary`/`octal`/`decimal`/`hex`,
  `adding`/`subtracting`/`multiplying`/`dividing`/`exponentiating`/`negated`

  Port of Hackage's `lens-5.3.6`'s `Numeric.Lens` (fetched and read via the
  real source, not recalled from memory). Upstream's real exports:

  ```
  integral :: (Integral a, Integral b) => Prism Integer Integer a b
  base :: (HasCallStack, Integral a) => Int -> Prism' String a
  binary, octal, decimal, hex :: Integral a => Prism' String a
  adding, subtracting :: Num a => a -> Iso' a a
  multiplying, dividing, exponentiating :: (Fractional a, Eq a) => a -> Iso' a a
  negated :: Num a => Iso' a a
  pattern Integral :: Integral a => a -> Integer
  ```

  **Deviation (Haskell's `Integral`/`Num`/`Fractional` ad hoc polymorphism →
  concrete Lean types).** Upstream generalizes every combinator here over
  Haskell's numeric-class hierarchy (`Integral`, `Num`, `Fractional`), none
  of which `linen` has ported as standalone typeclasses (there is no
  bespoke `Class.Integral`/`Class.Num` anywhere in this codebase — see
  `AGENTS.md`'s "prefer Lean stdlib objects" rule, and Lean's own
  arithmetic already lives on concrete types like `Nat`/`Int`/`Float`
  instead of a `Num`-style class). Each combinator below is therefore
  specialized to the one concrete type upstream's own doc comments actually
  exercise: `integral`/`base`/`binary`/`octal`/`decimal`/`hex` to `Nat` (the
  natural "subset of `Int`" `linen` already has, matching this batch's
  sibling module `Linen.Numeric.Natural.Lens`'s own `Nat`-vs-`Int` focus),
  and `multiplying`/`dividing`/`exponentiating` (upstream's `Fractional`/
  `Floating` combinators) to `Float`. `adding`/`subtracting`/`negated`
  (upstream's plain `Num`) are kept generic over the minimal `Add`/`Sub`/
  `Neg` structural classes Lean's own arithmetic instances (`Nat`, `Int`,
  `Float`, …) already provide.

  **Deviation (no `error`/`panic!` guards).** Upstream's `base`
  (invalid-base) and `multiplying`/`dividing`/`exponentiating` (zero
  factor/divisor/exponent) all crash via `error` on a degenerate input.
  `linen`'s `Prism'`/`Iso'` are dependent Pi-types (`∀ {P} [...] {F} [...],
  …`) with no `Inhabited` instance in general, so `panic!` (which requires
  `[Inhabited α]`) is not usable here even if desired. `base` instead takes
  the base's validity (`2 ≤ b ∧ b ≤ 36`) as an explicit hypothesis argument
  the caller must discharge (trivially, via `by decide`, for the four fixed
  bases below). `multiplying`/`dividing`/`exponentiating` need no such
  guard at all: unlike `Prism`, `Iso` carries no round-trip proof
  obligation in this codebase (`Linen.Control.Lens.Iso`'s own module doc
  comment), so a `0` factor/divisor/exponent simply yields an `Iso` that
  is not a genuine bijection (dividing/multiplying by `0.0` on `Float`
  total-ly produces `inf`/`nan`, never raising) — exactly as unenforced as
  every other `Iso` law in this codebase, rather than a new special case. -/

import Linen.Control.Lens.Iso
import Linen.Control.Lens.Prism

open Control.Lens

namespace Numeric.Lens

-- ── integral ─────────────────────────────────────

/-- `integral :: (Integral a, Integral b) => Prism Integer Integer a b`:
    every `Nat` embeds into `Int` — `Nat`'s the one concrete "subset of
    `Integer`" type this port specializes upstream's ad hoc polymorphism to
    (see the module doc comment's deviation note). Embedding through this
    `Prism'` only succeeds (extracts an `a`) if the `Int` is non-negative,
    matching upstream's own round-trip-recognition test. -/
def integral : Prism' Int Nat :=
  prism' (fun n : Nat => (n : Int)) (fun i => if i ≥ 0 then some i.toNat else none)

-- ── digit helpers ────────────────────────────────

/-- Like Haskell's `Data.Char.intToDigit`, but handles up to base-36 (`0`–`9`
    then `a`–`z`) — upstream's `intToDigit'`. Digits `≥ 36` have no defined
    character upstream either (`error`s there); this port simply returns
    `'0'` rather than crash, since it is never called on such a digit by any
    combinator below (every caller already bounds its input digit `< b ≤
    36`). -/
def intToDigit' (i : Nat) : Char :=
  if i < 10 then Char.ofNat (i + '0'.toNat)
  else if i < 36 then Char.ofNat (i - 10 + 'a'.toNat)
  else '0'

/-- A safe variant of Haskell's `digitToInt`, handling up to base-36: `'0'`–
    `'9'` are digits `0`–`9`, `'a'`–`'z'`/`'A'`–`'Z'` are digits `10`–`35` —
    upstream's `digitToIntMay`. -/
def digitToIntMay (c : Char) : Option Nat :=
  if '0' ≤ c ∧ c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c ∧ c ≤ 'z' then some (c.toNat - 'a'.toNat + 10)
  else if 'A' ≤ c ∧ c ≤ 'Z' then some (c.toNat - 'A'.toNat + 10)
  else none

/-- Select digits that fall into the given base — upstream's `isDigit'`. -/
def isDigit' (b : Nat) (c : Char) : Bool :=
  match digitToIntMay c with
  | some i => i < b
  | none => false

-- ── natDigits / ofDigits ─────────────────────────

/-- Render `n` in base `b` as a most-significant-digit-first list of digit
    characters, given `2 ≤ b` (needed for the recursive call `n / b` to
    strictly decrease `n`, terminating the recursion). Structural helper
    behind `base`'s "build" direction, standing in for upstream's
    `showIntAtBase`. -/
def natDigits (b : Nat) (n : Nat) (hb : 2 ≤ b) : List Char :=
  if h : n < b then [intToDigit' n]
  else
    have : n / b < n :=
      Nat.div_lt_self (by omega) (by omega)
    natDigits b (n / b) hb ++ [intToDigit' (n % b)]
termination_by n

/-- Parse every character of `s` as a base-`b` digit, folding left-to-right
    into the accumulated value; fails (`none`) as soon as a character isn't
    a valid base-`b` digit. Structural helper behind `base`'s "match"
    direction. -/
def digitsToNatGo (b : Nat) : List Char → Nat → Option Nat
  | [], acc => some acc
  | c :: cs, acc =>
    match digitToIntMay c with
    | some d => if d < b then digitsToNatGo b cs (acc * b + d) else none
    | none => none

/-- Parse the whole string `s` as a base-`b` natural number, requiring at
    least one digit — upstream's `readInt (fromIntegral b) (isDigit' b)
    digitToInt'`, restricted to succeed only when the *entire* string
    parses (matching upstream's own `[(n,"")] -> Right n; _ -> Left s`
    all-or-nothing check). -/
def digitsToNat (b : Nat) (s : List Char) : Option Nat :=
  match s with
  | [] => none
  | _ :: _ => digitsToNatGo b s 0

-- ── base / binary / octal / decimal / hex ───────

/-- `base :: (HasCallStack, Integral a) => Int -> Prism' String a`: a prism
    that shows and reads natural numbers in base-2 through base-36 —
    `linen`'s `Nat` specialization of upstream's `Integral a` (see the
    module doc comment's deviation note); the validity check upstream
    performs at runtime (`error` on `b < 2 || b > 36`) is instead an
    explicit hypothesis `hb` here. Improper as a `Prism` in the same sense
    upstream documents: leading zeros are stripped when reading (`"007"`
    round-trips to `"7"`, not back to `"007"`), which `Iso`/`Prism`'s
    unenforced round-trip law already tolerates in this codebase. -/
def base (b : Nat) (hb : 2 ≤ b ∧ b ≤ 36) : Prism' String Nat :=
  prism' (fun n => String.mk (natDigits b n hb.1)) (fun s => digitsToNat b s.toList)

/-- `binary = base 2`. -/
def binary : Prism' String Nat := base 2 (by decide)

/-- `octal = base 8`. -/
def octal : Prism' String Nat := base 8 (by decide)

/-- `decimal = base 10`. -/
def decimal : Prism' String Nat := base 10 (by decide)

/-- `hex = base 16`. -/
def hex : Prism' String Nat := base 16 (by decide)

-- ── adding / subtracting / negated ──────────────

/-- `adding n = iso (+n) (subtract n)`. -/
def adding {A : Type u} [Add A] [Sub A] (n : A) : Iso' A A :=
  iso (· + n) (· - n)

/-- `subtracting n = iso (subtract n) (+n) = from (adding n)`. -/
def subtracting {A : Type u} [Add A] [Sub A] (n : A) : Iso' A A :=
  iso (· - n) (· + n)

/-- `negated = iso negate negate`. -/
def negated {A : Type u} [Neg A] : Iso' A A :=
  iso Neg.neg Neg.neg

-- ── multiplying / dividing / exponentiating ─────

/-- `multiplying n = iso (*n) (/n)`. See the module doc comment's deviation
    note for why `n = 0` needs no guard here (unlike upstream's `error`). -/
def multiplying (n : Float) : Iso' Float Float :=
  iso (· * n) (· / n)

/-- `dividing n = iso (/n) (*n) = from (multiplying n)`. -/
def dividing (n : Float) : Iso' Float Float :=
  iso (· / n) (· * n)

/-- `exponentiating n = iso (**n) (**recip n)`. -/
def exponentiating (n : Float) : Iso' Float Float :=
  iso (Float.pow · n) (Float.pow · (1 / n))

end Numeric.Lens
