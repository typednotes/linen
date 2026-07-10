/-
  Linen.Data.Word8 -- UInt8 classification predicates and byte constants

  Ports `Data.Word8`, itself a port of Haskell's `Data.Word8` from the
  `word8` package.
  https://hackage.haskell.org/package/word8-0.1.3/docs/Data-Word8.html

  ## Design
  All predicates are `@[inline]` for zero-overhead abstraction.
  Byte constants use simple definitions for readability.
  Proofs of idempotency and classification/conversion coherence
  are proved by exhaustive evaluation over all 256 UInt8 values via `native_decide`.
-/

namespace Data.Word8

-- ════════════════════════════════════════════════════════════════════
-- Classification predicates
-- ════════════════════════════════════════════════════════════════════

/-- Test if byte is an uppercase ASCII letter.
    $$\text{isUpper}(w) \iff w \in [65, 90]$$ -/
@[inline] def isUpper (w : UInt8) : Bool := 65 ≤ w && w ≤ 90

/-- Test if byte is a lowercase ASCII letter.
    $$\text{isLower}(w) \iff w \in [97, 122]$$ -/
@[inline] def isLower (w : UInt8) : Bool := 97 ≤ w && w ≤ 122

/-- Test if byte is an ASCII letter.
    $$\text{isAlpha}(w) \iff \text{isUpper}(w) \lor \text{isLower}(w)$$ -/
@[inline] def isAlpha (w : UInt8) : Bool := isUpper w || isLower w

/-- Test if byte is an ASCII decimal digit.
    $$\text{isDigit}(w) \iff w \in [48, 57]$$ -/
@[inline] def isDigit (w : UInt8) : Bool := 48 ≤ w && w ≤ 57

/-- Test if byte is an ASCII letter or digit.
    $$\text{isAlphaNum}(w) \iff \text{isAlpha}(w) \lor \text{isDigit}(w)$$ -/
@[inline] def isAlphaNum (w : UInt8) : Bool := isAlpha w || isDigit w

/-- Test if byte is ASCII whitespace (space, tab, newline, vertical tab, form feed, carriage return).
    $$\text{isSpace}(w) \iff w \in \{9,10,11,12,13,32\}$$ -/
@[inline] def isSpace (w : UInt8) : Bool :=
  w == 32 || (9 ≤ w && w ≤ 13)

/-- Test if byte is an ASCII control character.
    $$\text{isControl}(w) \iff w < 32 \lor w = 127$$ -/
@[inline] def isControl (w : UInt8) : Bool := w < 32 || w == 127

/-- Test if byte is a printable ASCII character.
    $$\text{isPrint}(w) \iff w \in [32, 126]$$ -/
@[inline] def isPrint (w : UInt8) : Bool := 32 ≤ w && w ≤ 126

/-- Test if byte is an ASCII hexadecimal digit.
    $$\text{isHexDigit}(w) \iff \text{isDigit}(w) \lor w \in [65,70] \lor w \in [97,102]$$ -/
@[inline] def isHexDigit (w : UInt8) : Bool :=
  isDigit w || (65 ≤ w && w ≤ 70) || (97 ≤ w && w ≤ 102)

/-- Test if byte is an ASCII octal digit.
    $$\text{isOctDigit}(w) \iff w \in [48, 55]$$ -/
@[inline] def isOctDigit (w : UInt8) : Bool := 48 ≤ w && w ≤ 55

/-- Test if byte is in the ASCII range.
    $$\text{isAscii}(w) \iff w \le 127$$ -/
@[inline] def isAscii (w : UInt8) : Bool := w ≤ 127

-- ════════════════════════════════════════════════════════════════════
-- Case conversion
-- ════════════════════════════════════════════════════════════════════

/-- Convert an uppercase ASCII letter to lowercase; other bytes unchanged.
    $$\text{toLower}(w) = \begin{cases} w + 32 & \text{if } w \in [65,90] \\ w & \text{otherwise} \end{cases}$$ -/
@[inline] def toLower (w : UInt8) : UInt8 :=
  if isUpper w then w + 32 else w

/-- Convert a lowercase ASCII letter to uppercase; other bytes unchanged.
    $$\text{toUpper}(w) = \begin{cases} w - 32 & \text{if } w \in [97,122] \\ w & \text{otherwise} \end{cases}$$ -/
@[inline] def toUpper (w : UInt8) : UInt8 :=
  if isLower w then w - 32 else w

-- ════════════════════════════════════════════════════════════════════
-- Proofs
-- ════════════════════════════════════════════════════════════════════

/-- Decidable universal quantification over `UInt8` (finite: 256 values).
    Reduces to `∀ i : Fin 256, P (UInt8.ofBitVec (BitVec.ofFin i))`. -/
@[reducible]
private def decidableForallUInt8 (P : UInt8 → Prop) [DecidablePred P] :
    Decidable (∀ w : UInt8, P w) :=
  have h : (∀ w : UInt8, P w) ↔ (∀ i : Fin (2^8), P (.ofBitVec (.ofFin i))) :=
    ⟨fun h i => h (.ofBitVec (.ofFin i)),
     fun h ⟨⟨v⟩⟩ => h v⟩
  decidable_of_iff _ h.symm

attribute [local instance] decidableForallUInt8

/-- `toLower` is idempotent: applying it twice yields the same result.
    $$\forall w.\; \text{toLower}(\text{toLower}(w)) = \text{toLower}(w)$$

    Proved by exhaustive evaluation over all 256 UInt8 values via `native_decide`. -/
theorem toLower_idempotent : ∀ (w : UInt8), toLower (toLower w) = toLower w := by
  native_decide

/-- `toUpper` is idempotent: applying it twice yields the same result.
    $$\forall w.\; \text{toUpper}(\text{toUpper}(w)) = \text{toUpper}(w)$$

    Proved by exhaustive evaluation over all 256 UInt8 values via `native_decide`. -/
theorem toUpper_idempotent : ∀ (w : UInt8), toUpper (toUpper w) = toUpper w := by
  native_decide

/-- `toLower` maps uppercase letters to lowercase letters.
    $$\forall w.\; \text{isUpper}(w) \to \text{isLower}(\text{toLower}(w))$$

    Proved by exhaustive evaluation over all 256 UInt8 values via `native_decide`. -/
theorem isUpper_toLower : ∀ (w : UInt8), isUpper w = true → isLower (toLower w) = true := by
  native_decide

/-- `toUpper` maps lowercase letters to uppercase letters.
    $$\forall w.\; \text{isLower}(w) \to \text{isUpper}(\text{toUpper}(w))$$

    Proved by exhaustive evaluation over all 256 UInt8 values via `native_decide`. -/
theorem isLower_toUpper : ∀ (w : UInt8), isLower w = true → isUpper (toUpper w) = true := by
  native_decide

-- ════════════════════════════════════════════════════════════════════
-- Byte constants — control characters
-- ════════════════════════════════════════════════════════════════════

/-- NUL byte (0x00). -/
@[inline] def _nul : UInt8 := 0
/-- Horizontal tab (0x09). -/
@[inline] def _tab : UInt8 := 9
/-- Line feed (0x0A). -/
@[inline] def _lf : UInt8 := 10
/-- Vertical tab (0x0B). -/
@[inline] def _vt : UInt8 := 11
/-- Form feed (0x0C). -/
@[inline] def _ff : UInt8 := 12
/-- Carriage return (0x0D). -/
@[inline] def _cr : UInt8 := 13
/-- Space (0x20). -/
@[inline] def _space : UInt8 := 32

-- ════════════════════════════════════════════════════════════════════
-- Byte constants — punctuation and symbols
-- ════════════════════════════════════════════════════════════════════

/-- Exclamation mark `!` (0x21). -/
@[inline] def _exclam : UInt8 := 33
/-- Double quote `"` (0x22). -/
@[inline] def _quotedbl : UInt8 := 34
/-- Number sign `#` (0x23). -/
@[inline] def _numbersign : UInt8 := 35
/-- Dollar sign `$` (0x24). -/
@[inline] def _dollar : UInt8 := 36
/-- Percent sign `%` (0x25). -/
@[inline] def _percent : UInt8 := 37
/-- Ampersand `&` (0x26). -/
@[inline] def _ampersand : UInt8 := 38
/-- Single quote `'` (0x27). -/
@[inline] def _quotesingle : UInt8 := 39
/-- Left parenthesis `(` (0x28). -/
@[inline] def _parenleft : UInt8 := 40
/-- Right parenthesis `)` (0x29). -/
@[inline] def _parenright : UInt8 := 41
/-- Asterisk `*` (0x2A). -/
@[inline] def _asterisk : UInt8 := 42
/-- Plus sign `+` (0x2B). -/
@[inline] def _plus : UInt8 := 43
/-- Comma `,` (0x2C). -/
@[inline] def _comma : UInt8 := 44
/-- Hyphen-minus `-` (0x2D). -/
@[inline] def _hyphen : UInt8 := 45
/-- Full stop `.` (0x2E). -/
@[inline] def _period : UInt8 := 46
/-- Solidus `/` (0x2F). -/
@[inline] def _slash : UInt8 := 47

-- ════════════════════════════════════════════════════════════════════
-- Byte constants — digits
-- ════════════════════════════════════════════════════════════════════

/-- Digit `0` (0x30). -/
@[inline] def _0 : UInt8 := 48
/-- Digit `1` (0x31). -/
@[inline] def _1 : UInt8 := 49
/-- Digit `2` (0x32). -/
@[inline] def _2 : UInt8 := 50
/-- Digit `3` (0x33). -/
@[inline] def _3 : UInt8 := 51
/-- Digit `4` (0x34). -/
@[inline] def _4 : UInt8 := 52
/-- Digit `5` (0x35). -/
@[inline] def _5 : UInt8 := 53
/-- Digit `6` (0x36). -/
@[inline] def _6 : UInt8 := 54
/-- Digit `7` (0x37). -/
@[inline] def _7 : UInt8 := 55
/-- Digit `8` (0x38). -/
@[inline] def _8 : UInt8 := 56
/-- Digit `9` (0x39). -/
@[inline] def _9 : UInt8 := 57

-- ════════════════════════════════════════════════════════════════════
-- Byte constants — more punctuation
-- ════════════════════════════════════════════════════════════════════

/-- Colon `:` (0x3A). -/
@[inline] def _colon : UInt8 := 58
/-- Semicolon `;` (0x3B). -/
@[inline] def _semicolon : UInt8 := 59
/-- Less-than sign `<` (0x3C). -/
@[inline] def _less : UInt8 := 60
/-- Equals sign `=` (0x3D). -/
@[inline] def _equal : UInt8 := 61
/-- Greater-than sign `>` (0x3E). -/
@[inline] def _greater : UInt8 := 62
/-- Question mark `?` (0x3F). -/
@[inline] def _question : UInt8 := 63
/-- At sign `@` (0x40). -/
@[inline] def _at : UInt8 := 64

-- ════════════════════════════════════════════════════════════════════
-- Byte constants — uppercase letters
-- ════════════════════════════════════════════════════════════════════

/-- Uppercase `A` (0x41). -/
@[inline] def _A : UInt8 := 65
/-- Uppercase `B` (0x42). -/
@[inline] def _B : UInt8 := 66
/-- Uppercase `C` (0x43). -/
@[inline] def _C : UInt8 := 67
/-- Uppercase `D` (0x44). -/
@[inline] def _D : UInt8 := 68
/-- Uppercase `E` (0x45). -/
@[inline] def _E : UInt8 := 69
/-- Uppercase `F` (0x46). -/
@[inline] def _F : UInt8 := 70
/-- Uppercase `G` (0x47). -/
@[inline] def _G : UInt8 := 71
/-- Uppercase `H` (0x48). -/
@[inline] def _H : UInt8 := 72
/-- Uppercase `I` (0x49). -/
@[inline] def _I : UInt8 := 73
/-- Uppercase `J` (0x4A). -/
@[inline] def _J : UInt8 := 74
/-- Uppercase `K` (0x4B). -/
@[inline] def _K : UInt8 := 75
/-- Uppercase `L` (0x4C). -/
@[inline] def _L : UInt8 := 76
/-- Uppercase `M` (0x4D). -/
@[inline] def _M : UInt8 := 77
/-- Uppercase `N` (0x4E). -/
@[inline] def _N : UInt8 := 78
/-- Uppercase `O` (0x4F). -/
@[inline] def _O : UInt8 := 79
/-- Uppercase `P` (0x50). -/
@[inline] def _P : UInt8 := 80
/-- Uppercase `Q` (0x51). -/
@[inline] def _Q : UInt8 := 81
/-- Uppercase `R` (0x52). -/
@[inline] def _R : UInt8 := 82
/-- Uppercase `S` (0x53). -/
@[inline] def _S : UInt8 := 83
/-- Uppercase `T` (0x54). -/
@[inline] def _T : UInt8 := 84
/-- Uppercase `U` (0x55). -/
@[inline] def _U : UInt8 := 85
/-- Uppercase `V` (0x56). -/
@[inline] def _V : UInt8 := 86
/-- Uppercase `W` (0x57). -/
@[inline] def _W : UInt8 := 87
/-- Uppercase `X` (0x58). -/
@[inline] def _X : UInt8 := 88
/-- Uppercase `Y` (0x59). -/
@[inline] def _Y : UInt8 := 89
/-- Uppercase `Z` (0x5A). -/
@[inline] def _Z : UInt8 := 90

-- ════════════════════════════════════════════════════════════════════
-- Byte constants — brackets and special
-- ════════════════════════════════════════════════════════════════════

/-- Left square bracket `[` (0x5B). -/
@[inline] def _bracketleft : UInt8 := 91
/-- Reverse solidus `\` (0x5C). -/
@[inline] def _backslash : UInt8 := 92
/-- Right square bracket `]` (0x5D). -/
@[inline] def _bracketright : UInt8 := 93
/-- Circumflex accent `^` (0x5E). -/
@[inline] def _circum : UInt8 := 94
/-- Low line `_` (0x5F). -/
@[inline] def _underscore : UInt8 := 95
/-- Grave accent `` ` `` (0x60). -/
@[inline] def _grave : UInt8 := 96

-- ════════════════════════════════════════════════════════════════════
-- Byte constants — lowercase letters
-- ════════════════════════════════════════════════════════════════════

/-- Lowercase `a` (0x61). -/
@[inline] def _a : UInt8 := 97
/-- Lowercase `b` (0x62). -/
@[inline] def _b : UInt8 := 98
/-- Lowercase `c` (0x63). -/
@[inline] def _c : UInt8 := 99
/-- Lowercase `d` (0x64). -/
@[inline] def _d : UInt8 := 100
/-- Lowercase `e` (0x65). -/
@[inline] def _e : UInt8 := 101
/-- Lowercase `f` (0x66). -/
@[inline] def _f : UInt8 := 102
/-- Lowercase `g` (0x67). -/
@[inline] def _g : UInt8 := 103
/-- Lowercase `h` (0x68). -/
@[inline] def _h : UInt8 := 104
/-- Lowercase `i` (0x69). -/
@[inline] def _i : UInt8 := 105
/-- Lowercase `j` (0x6A). -/
@[inline] def _j : UInt8 := 106
/-- Lowercase `k` (0x6B). -/
@[inline] def _k : UInt8 := 107
/-- Lowercase `l` (0x6C). -/
@[inline] def _l : UInt8 := 108
/-- Lowercase `m` (0x6D). -/
@[inline] def _m : UInt8 := 109
/-- Lowercase `n` (0x6E). -/
@[inline] def _n : UInt8 := 110
/-- Lowercase `o` (0x6F). -/
@[inline] def _o : UInt8 := 111
/-- Lowercase `p` (0x70). -/
@[inline] def _p : UInt8 := 112
/-- Lowercase `q` (0x71). -/
@[inline] def _q : UInt8 := 113
/-- Lowercase `r` (0x72). -/
@[inline] def _r : UInt8 := 114
/-- Lowercase `s` (0x73). -/
@[inline] def _s : UInt8 := 115
/-- Lowercase `t` (0x74). -/
@[inline] def _t : UInt8 := 116
/-- Lowercase `u` (0x75). -/
@[inline] def _u : UInt8 := 117
/-- Lowercase `v` (0x76). -/
@[inline] def _v : UInt8 := 118
/-- Lowercase `w` (0x77). -/
@[inline] def _w : UInt8 := 119
/-- Lowercase `x` (0x78). -/
@[inline] def _x : UInt8 := 120
/-- Lowercase `y` (0x79). -/
@[inline] def _y : UInt8 := 121
/-- Lowercase `z` (0x7A). -/
@[inline] def _z : UInt8 := 122

-- ════════════════════════════════════════════════════════════════════
-- Byte constants — braces and remaining
-- ════════════════════════════════════════════════════════════════════

/-- Left curly bracket `{` (0x7B). -/
@[inline] def _braceleft : UInt8 := 123
/-- Vertical line `|` (0x7C). -/
@[inline] def _bar : UInt8 := 124
/-- Right curly bracket `}` (0x7D). -/
@[inline] def _braceright : UInt8 := 125
/-- Tilde `~` (0x7E). -/
@[inline] def _tilde : UInt8 := 126
/-- DEL byte (0x7F). -/
@[inline] def _del : UInt8 := 127

end Data.Word8
