/-
  Data.Scientific — Arbitrary-precision scientific notation

  A number represented as $c \times 10^{e}$ where $c$ (coefficient) and $e$
  (base-10 exponent) are arbitrary-precision integers.

  ## Design

  Mirrors Haskell's `Data.Scientific` from the `scientific` package.
  - No `Div` instance (like Haskell): division of scientific numbers cannot
    always be represented exactly in scientific notation.
  - `normalize` removes trailing zeros from the coefficient and adjusts the
    exponent accordingly, yielding a canonical form.
  - `ToString` formats as a plain decimal string (no exponential notation).

  ## Haskell source

  https://hackage.haskell.org/package/scientific-0.3.7.0/docs/Data-Scientific.html
-/

namespace Data

/-- A number in scientific notation: $c \times 10^{e}$.

    - `coefficient` — the significand $c$ (arbitrary-precision integer)
    - `base10Exponent` — the exponent $e$ (arbitrary-precision integer)

    Two `Scientific` values are logically equal when they represent the same
    real number; structural equality after `normalize` is canonical. -/
structure Scientific where
  /-- The significand $c$. -/
  coefficient : Int
  /-- The base-10 exponent $e$. -/
  base10Exponent : Int
deriving Repr

namespace Scientific

-- ── Construction ──────────────────────────────────

/-- Smart constructor: $\text{scientific}(c, e) = c \times 10^{e}$. -/
@[inline] def scientific (c : Int) (e : Int) : Scientific :=
  ⟨c, e⟩

/-- Create a `Scientific` from an integer: $n \mapsto n \times 10^{0}$. -/
@[inline] def fromInt (n : Int) : Scientific :=
  ⟨n, 0⟩

-- ── Helpers ───────────────────────────────────────

/-- Helper: multiply an integer by $10^{n}$. -/
private def mulPow10 (c : Int) : Nat → Int
  | 0 => c
  | n + 1 => mulPow10 (c * 10) n

/-- Helper: repeat a character `n` times. -/
private def repeatChar (c : Char) (n : Nat) : String :=
  String.ofList (List.replicate n c)

-- ── Normalization ─────────────────────────────────

/-- Count trailing zeros in the base-10 representation of a nonzero natural number. -/
private def countTrailingZerosNat (n : Nat) : Nat :=
  if n == 0 then 0
  else
    let rec go (n : Nat) (k : Nat) : Nat :=
      if h : n > 0 then
        if n % 10 == 0 then
          go (n / 10) (k + 1)
        else k
      else k
    termination_by n
    decreasing_by exact Nat.div_lt_self h (by omega)
    go n 0

/-- Remove trailing zeros from the coefficient, adjusting the exponent.

    $$\text{normalize}(c \times 10^{e}) = (c / 10^{k}) \times 10^{e+k}$$

    where $k$ is the number of trailing zeros in $c$.
    The result is canonical: the coefficient has no trailing zeros
    (or is zero). -/
def normalize (s : Scientific) : Scientific :=
  if s.coefficient == 0 then ⟨0, 0⟩
  else
    let k := countTrailingZerosNat s.coefficient.natAbs
    if k == 0 then s
    else
      let divisor := mulPow10 1 k
      ⟨s.coefficient / divisor, s.base10Exponent + k⟩

-- ── Predicates ────────────────────────────────────

/-- Is the number zero?
    $$\text{isZero}(s) \iff c = 0$$ -/
@[inline] def isZero (s : Scientific) : Bool :=
  s.coefficient == 0

/-- Is the number an integer?
    True when the exponent is non-negative (after normalization). -/
def isInteger (s : Scientific) : Bool :=
  let n := s.normalize
  n.base10Exponent ≥ 0

/-- Is the number negative zero?
    Always `false` since `Int` has no negative zero. -/
@[inline] def isNegativeZero (_ : Scientific) : Bool := false

-- ── Conversion to Float ───────────────────────────

/-- Helper: multiply a float by $10^{n}$. -/
private def floatMulPow10 (acc : Float) : Nat → Float
  | 0 => acc
  | n + 1 => floatMulPow10 (acc * 10.0) n

/-- Helper: divide a float by $10^{n}$. -/
private def floatDivPow10 (acc : Float) : Nat → Float
  | 0 => acc
  | n + 1 => floatDivPow10 (acc / 10.0) n

/-- Convert to `Float` (may lose precision for large or very precise values).

    $$\text{toRealFloat}(c \times 10^{e}) = \text{Float.ofInt}(c) \times 10^{e}$$ -/
def toRealFloat (s : Scientific) : Float :=
  let c := Float.ofInt s.coefficient
  let e := s.base10Exponent
  if e == 0 then c
  else if e > 0 then floatMulPow10 c e.toNat
  else floatDivPow10 c (-e).toNat

-- ── Conversion from Float ─────────────────────────

/-- Convert a `Float` to `Scientific` by decomposing it into decimal digits.

    Note: this performs a best-effort conversion through the string
    representation, similar to Haskell's `fromFloatDigits`. -/
def fromFloatDigits (f : Float) : Scientific :=
  if f == 0.0 then ⟨0, 0⟩
  else
    -- Use Float.toString and parse the decimal representation
    let str := toString f
    -- Check for scientific notation (e.g., "1.23e10")
    let parts := str.splitOn "e"
    match parts with
    | [mantissa, expStr] =>
      -- Scientific notation from Float.toString
      let expVal := expStr.toInt!
      let dotParts := mantissa.splitOn "."
      match dotParts with
      | [intPart, fracPart] =>
        let fracLen : Int := fracPart.length
        let combined := intPart ++ fracPart
        let coeff := combined.toInt!
        ⟨coeff, expVal - fracLen⟩
      | [intPart] =>
        ⟨intPart.toInt!, expVal⟩
      | _ => ⟨0, 0⟩
    | [decimal] =>
      -- Plain decimal (e.g., "3.14")
      let dotParts := decimal.splitOn "."
      match dotParts with
      | [intPart, fracPart] =>
        let fracLen : Int := fracPart.length
        let combined := intPart ++ fracPart
        let coeff := combined.toInt!
        ⟨coeff, -fracLen⟩
      | [intPart] =>
        ⟨intPart.toInt!, 0⟩
      | _ => ⟨0, 0⟩
    | _ => ⟨0, 0⟩

-- ── Bounded integer conversion ────────────────────

/-- Convert to a bounded integer, returning `none` if the number is
    fractional or outside the representable range of a 64-bit signed integer.

    Range: $[-2^{63}, 2^{63} - 1]$. -/
def toBoundedInteger (s : Scientific) : Option Int :=
  if !s.isInteger then none
  else
    let n := s.normalize
    -- Reconstruct the integer value: coefficient * 10^exponent
    let val := mulPow10 n.coefficient n.base10Exponent.toNat
    -- Check 64-bit signed range
    let minVal : Int := -9223372036854775808
    let maxVal : Int := 9223372036854775807
    if val ≥ minVal && val ≤ maxVal then some val
    else none

-- ── Decimal digits ────────────────────────────────

/-- Extract decimal digits from a natural number (most-significant first). -/
private def natToDigits (n : Nat) : List Nat :=
  if n == 0 then [0]
  else
    let rec go (n : Nat) (acc : List Nat) : List Nat :=
      if h : n > 0 then
        go (n / 10) ((n % 10) :: acc)
      else acc
    termination_by n
    decreasing_by exact Nat.div_lt_self h (by omega)
    go n []

/-- Remove trailing zeros from a digit list, preserving at least one digit. -/
private def dropTrailingZeros (ds : List Nat) : List Nat :=
  match ds.reverse.dropWhile (· == 0) with
  | [] => [0]
  | rev => rev.reverse

/-- Decompose into a list of mantissa digits and a base-10 exponent.

    Returns `(digits, exp)` such that:
    $$\text{value} = 0.\text{digits} \times 10^{\text{exp}}$$

    For example, $123.45$ yields `([1,2,3,4,5], 3)`.
    The digit list has no trailing zeros. For zero, returns `([0], 1)`. -/
def toDecimalDigits (s : Scientific) : List Nat × Int :=
  let n := s.normalize
  if n.coefficient == 0 then ([0], 1)
  else
    let absCoeff := n.coefficient.natAbs
    let digits := natToDigits absCoeff
    -- The number of digits gives the implicit decimal point position
    let numDigits : Int := digits.length
    -- exp is such that 0.digits * 10^exp = coefficient * 10^base10Exponent
    -- coefficient = 0.digits * 10^numDigits
    -- so value = 0.digits * 10^numDigits * 10^base10Exponent
    --          = 0.digits * 10^(numDigits + base10Exponent)
    let exp := numDigits + n.base10Exponent
    let cleanDigits := dropTrailingZeros digits
    (cleanDigits, exp)

-- ── Arithmetic ────────────────────────────────────

/-- Align two scientific numbers to the same exponent.
    Returns `(c1', c2', e)` where both coefficients are scaled to exponent `e`
    (the smaller of the two exponents). -/
private def align (a b : Scientific) : Int × Int × Int :=
  if a.base10Exponent ≤ b.base10Exponent then
    let diff := (b.base10Exponent - a.base10Exponent).toNat
    (a.coefficient, mulPow10 b.coefficient diff, a.base10Exponent)
  else
    let diff := (a.base10Exponent - b.base10Exponent).toNat
    (mulPow10 a.coefficient diff, b.coefficient, b.base10Exponent)

instance : Add Scientific where
  add a b :=
    let (c1, c2, e) := align a b
    ⟨c1 + c2, e⟩

instance : Sub Scientific where
  sub a b :=
    let (c1, c2, e) := align a b
    ⟨c1 - c2, e⟩

instance : Mul Scientific where
  mul a b :=
    ⟨a.coefficient * b.coefficient, a.base10Exponent + b.base10Exponent⟩

instance : Neg Scientific where
  neg s := ⟨-s.coefficient, s.base10Exponent⟩

-- ── Comparison ────────────────────────────────────

/-- Helper: sign of an integer as an `Ordering`. -/
private def intSign (n : Int) : Ordering :=
  if n < 0 then .lt
  else if n > 0 then .gt
  else .eq

instance : BEq Scientific where
  beq a b :=
    let an := a.normalize
    let bn := b.normalize
    an.coefficient == bn.coefficient && an.base10Exponent == bn.base10Exponent

instance : Ord Scientific where
  compare a b :=
    -- Handle sign comparison first for efficiency
    let aSign := intSign a.coefficient
    let bSign := intSign b.coefficient
    match aSign, bSign with
    | .lt, .gt => .lt
    | .gt, .lt => .gt
    | .eq, .eq => .eq
    | .eq, .gt => .lt
    | .eq, .lt => .gt
    | .gt, .eq => .gt
    | .lt, .eq => .lt
    | _, _ =>
      -- Same sign: align and compare coefficients
      let (c1, c2, _) := align a b
      compare c1 c2

instance : Inhabited Scientific where
  default := ⟨0, 0⟩

-- ── OfNat / OfScientific ──────────────────────────

instance : OfNat Scientific n where
  ofNat := ⟨n, 0⟩

instance : OfScientific Scientific where
  ofScientific m s e :=
    if s then ⟨m, -e⟩
    else ⟨m, e⟩

-- ── ToString ──────────────────────────────────────

/-- Format a `Scientific` as a plain decimal string.

    - $1.23 \times 10^{5}$ → `"123000.0"`
    - $1.23 \times 10^{-2}$ → `"0.0123"`
    - $-42 \times 10^{0}$ → `"-42.0"` -/
instance : ToString Scientific where
  toString s :=
    if s.coefficient == 0 then "0.0"
    else
      let isNeg := s.coefficient < 0
      let absCoeff := s.coefficient.natAbs
      let sign := if isNeg then "-" else ""
      let digits := toString absCoeff
      let numDigits : Int := digits.length
      let e := s.base10Exponent
      if e ≥ 0 then
        -- Positive exponent: append zeros
        sign ++ digits ++ repeatChar '0' e.toNat ++ ".0"
      else
        let negE := (-e).toNat
        if numDigits > negE then
          -- Decimal point falls within the digits
          let intPart := digits.take (digits.length - negE)
          let fracPart := digits.drop (digits.length - negE)
          sign ++ intPart ++ "." ++ fracPart
        else
          -- Need leading zeros after "0."
          let leadingZeros := negE - digits.length
          sign ++ "0." ++ repeatChar '0' leadingZeros ++ digits

-- ── Proofs ───────────────────────────────────────

/-- `isZero` is true if and only if the coefficient is zero.
    $$\text{isZero}(s) = \text{true} \iff s.\text{coefficient} = 0$$ -/
theorem isZero_iff (s : Scientific) : isZero s = true ↔ s.coefficient = 0 := by
  simp [isZero, beq_iff_eq]

/-- `fromInt 0` produces a zero scientific.
    $$\text{isZero}(\text{fromInt}(0)) = \text{true}$$ -/
theorem isZero_fromInt_zero : isZero (fromInt 0) = true := by
  simp [isZero, fromInt]

/-- Normalizing zero yields the canonical zero `⟨0, 0⟩`.
    $$\text{normalize}(0 \times 10^{e}) = 0 \times 10^{0}$$ -/
theorem normalize_zero (e : Int) : normalize ⟨0, e⟩ = ⟨0, 0⟩ := by
  simp [normalize]

-- NOTE: `normalize_idempotent` (normalize (normalize s) = normalize s) requires a lemma
-- showing that countTrailingZerosNat returns 0 after dividing out all trailing zeros.
-- This is true but the proof requires non-trivial number theory about Nat.div and modular
-- arithmetic. Skipped to avoid sorry.

/-- Negation is self-inverse.
    $$-(-s) = s$$ -/
theorem neg_neg (s : Scientific) : -(-s) = s := by
  cases s with
  | mk c e =>
    simp only [Neg.neg]
    show Scientific.mk c.neg.neg e = Scientific.mk c e
    congr 1
    exact Int.neg_neg c

/-- `fromInt` preserves zero under `BEq`.
    $$(\text{fromInt}(0) == 0) = \text{true}$$ -/
theorem fromInt_zero_beq : (fromInt 0 == (0 : Scientific)) = true := by
  native_decide

end Scientific
end Data
