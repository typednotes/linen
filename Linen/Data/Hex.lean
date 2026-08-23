/-
  Linen.Data.Hex — Base16 (hexadecimal) encoding

  Byte-exact hex encoding and decoding, the Base16 counterpart to
  `Linen.Data.Base64`.

  ## Provenance
  Base16 is RFC 4648 §8, the same document `Linen.Data.Base64` implements.
  Added because nothing in the library exposed hex encoding: the only such
  code was a private `toHex` helper duplicated inside
  `Tests/Linen/Crypto/SHA256Test.lean`, and
  `Linen.Data.ByteString.Builder.wordHex` renders a *number* without
  per-byte zero padding (`10 ↦ "a"`, not `"0a"`), so it cannot encode a
  digest. Digest-to-hex is required by anything that prints or compares a
  hash — `Linen.Crypto.SigV4` among them.

  ## Case
  `encode` emits lowercase, which is what hash digests are conventionally
  written in and what AWS Signature Version 4 requires. `encodeUpper` emits
  uppercase for callers that need it; `decode` accepts either.
-/

namespace Data.Hex

-- ── Digits ──

/-- The lowercase hex digit for `n < 16`; `'?'` outside that range, which
    `encode` never produces since it only ever passes `n < 16`. -/
def digit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n)
  else if n < 16 then Char.ofNat (87 + n)
  else '?'

/-- The uppercase hex digit for `n < 16`. -/
def digitUpper (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n)
  else if n < 16 then Char.ofNat (55 + n)
  else '?'

/-- The numeric value of a hex digit, in either case.
    $$\text{digitVal} : \text{Char} \to \text{Option}\ \{0, \ldots, 15\}$$ -/
def digitVal (c : Char) : Option Nat :=
  if c ≥ '0' && c ≤ '9' then some (c.toNat - 48)
  else if c ≥ 'a' && c ≤ 'f' then some (c.toNat - 87)
  else if c ≥ 'A' && c ≤ 'F' then some (c.toNat - 55)
  else none

-- ── Encoding ──

/-- Encode `bytes` as lowercase hex, two digits per byte and no separator.
    $$|\text{encode}(b)| = 2\,|b|$$ -/
def encode (bytes : ByteArray) : String :=
  String.join (bytes.toList.map fun b =>
    String.ofList [digit (b.toNat / 16), digit (b.toNat % 16)])

/-- Encode `bytes` as uppercase hex. -/
def encodeUpper (bytes : ByteArray) : String :=
  String.join (bytes.toList.map fun b =>
    String.ofList [digitUpper (b.toNat / 16), digitUpper (b.toNat % 16)])

-- ── Decoding ──

/-- Decode a list of hex characters, most significant digit first, into
    accumulated bytes. Structurally recursive: each step consumes two
    characters and recurses on the strictly shorter tail. -/
private def decodeChars : List Char → List UInt8 → Option (List UInt8)
  | [],           acc => some acc.reverse
  | [_],          _   => none                    -- odd length: not a whole byte
  | h :: l :: rest, acc => do
    let hv ← digitVal h
    let lv ← digitVal l
    decodeChars rest (UInt8.ofNat (hv * 16 + lv) :: acc)

/-- Decode a hex string, accepting either case. Returns `none` on an odd
    length or any non-hex character — never a partial result. -/
def decode (s : String) : Option ByteArray :=
  (decodeChars s.toList []).map fun bs => ⟨bs.toArray⟩

-- ── Proofs ──

/-- Every digit round-trips through `digitVal`. Decidable over the finite
    range, so this is checked exhaustively rather than argued. -/
theorem digitVal_digit : ∀ n ∈ List.range 16, digitVal (digit n) = some n := by decide

/-- Uppercase digits round-trip too — `digitVal` is case-insensitive. -/
theorem digitVal_digitUpper : ∀ n ∈ List.range 16, digitVal (digitUpper n) = some n := by decide

/-- Both cases denote the same value, so `decode` is insensitive to the case
    `encode`/`encodeUpper` chose. -/
theorem digitVal_agree : ∀ n ∈ List.range 16, digitVal (digit n) = digitVal (digitUpper n) := by
  decide

end Data.Hex
