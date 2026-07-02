/-
  Linen.Data.Base64 — RFC 4648 Base64 codec

  Encodes/decodes core `ByteArray` ↔ `String`. (Haskell's
  `Data.ByteString.Base64`; the `ByteString` is Lean core's `ByteArray`, so no
  `ByteString` slice type is needed here.)

  The alphabet is computed arithmetically in both directions (no lookup table),
  and `encode`/`decode` are **structural recursions** over `List UInt8` /
  `List Char` in groups of three / four — no `partial`, no fuel, no `while`.

  Guarantees exercised in the tests: `decode (encode bs)` round-trips, and
  `encode` emits only `[A-Za-z0-9+/=]`.
-/

namespace Data.Base64

/-! ── Alphabet (computed, not tabulated) ── -/

/-- Map a 6-bit index (`0–63`) to its Base64 character. -/
private def encChar (n : Nat) : Char :=
  let m := n % 64
  if m < 26 then Char.ofNat (m + 'A'.toNat)
  else if m < 52 then Char.ofNat (m - 26 + 'a'.toNat)
  else if m < 62 then Char.ofNat (m - 52 + '0'.toNat)
  else if m == 62 then '+'
  else '/'

/-- Map a Base64 character back to its 6-bit value, or `none` if not in the
    alphabet (the `'='` pad is handled separately by `decodeBytes`). -/
private def decVal (c : Char) : Option Nat :=
  let n := c.toNat
  if 'A'.toNat ≤ n ∧ n ≤ 'Z'.toNat then some (n - 'A'.toNat)
  else if 'a'.toNat ≤ n ∧ n ≤ 'z'.toNat then some (n - 'a'.toNat + 26)
  else if '0'.toNat ≤ n ∧ n ≤ '9'.toNat then some (n - '0'.toNat + 52)
  else if c == '+' then some 62
  else if c == '/' then some 63
  else none

/-! ── Encoding ── -/

/-- Encode a list of bytes into Base64 characters, three input bytes at a time
    (with `=` padding for a 1- or 2-byte tail). Structural on the tail. -/
private def encodeChars : List UInt8 → List Char
  | [] => []
  | [a] =>
    let n := a.toNat <<< 16
    [encChar (n >>> 18 &&& 0x3F), encChar (n >>> 12 &&& 0x3F), '=', '=']
  | [a, b] =>
    let n := a.toNat <<< 16 ||| b.toNat <<< 8
    [encChar (n >>> 18 &&& 0x3F), encChar (n >>> 12 &&& 0x3F), encChar (n >>> 6 &&& 0x3F), '=']
  | a :: b :: c :: rest =>
    let n := a.toNat <<< 16 ||| b.toNat <<< 8 ||| c.toNat
    encChar (n >>> 18 &&& 0x3F) :: encChar (n >>> 12 &&& 0x3F)
      :: encChar (n >>> 6 &&& 0x3F) :: encChar (n &&& 0x3F) :: encodeChars rest

/-- Encode a `ByteArray` to a Base64 `String`.
    $$\text{encode} : \text{ByteArray} \to \text{String}$$ -/
def encode (input : ByteArray) : String :=
  String.ofList (encodeChars input.toList)

/-! ── Decoding ── -/

/-- Decode Base64 characters into bytes, four input characters at a time.
    `'='` padding is accepted only in the final group; any character outside
    the alphabet (or misplaced padding) yields `none`. Structural on the tail. -/
private def decodeBytes : List Char → Option (List UInt8)
  | [] => some []
  | a :: b :: c :: d :: rest => do
    let av ← decVal a
    let bv ← decVal b
    if c == '=' then
      if d == '=' && rest.isEmpty then
        let n := av <<< 18 ||| bv <<< 12
        some [(n >>> 16 &&& 0xFF).toUInt8]
      else none
    else
      let cv ← decVal c
      if d == '=' then
        if rest.isEmpty then
          let n := av <<< 18 ||| bv <<< 12 ||| cv <<< 6
          some [(n >>> 16 &&& 0xFF).toUInt8, (n >>> 8 &&& 0xFF).toUInt8]
        else none
      else
        let dv ← decVal d
        let n := av <<< 18 ||| bv <<< 12 ||| cv <<< 6 ||| dv
        let group := [(n >>> 16 &&& 0xFF).toUInt8, (n >>> 8 &&& 0xFF).toUInt8, (n &&& 0xFF).toUInt8]
        (group ++ ·) <$> decodeBytes rest
  | _ => none  -- a trailing 1–3 characters: length not a multiple of 4

/-- Decode a Base64 `String` back to a `ByteArray`, or `none` on invalid input.
    Whitespace is **not** stripped: any character outside the alphabet
    (including `'\n'`, `'\r'`, `' '`) makes the input invalid, matching
    Haskell's `Data.ByteString.Base64.decode` (as opposed to `decodeLenient`).
    $$\text{decode} : \text{String} \to \text{Option}\ \text{ByteArray}$$ -/
def decode (input : String) : Option ByteArray :=
  if input.length % 4 != 0 then none
  else (decodeBytes input.toList).map (fun bytes => bytes.foldl ByteArray.push ByteArray.empty)

end Data.Base64
