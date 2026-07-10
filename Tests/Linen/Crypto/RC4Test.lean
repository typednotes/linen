/-
  Tests for `Linen.Crypto.RC4` — the RC4 stream cipher (KSA + PRGA).

  Verified against the well-known RC4 test vectors (key/plaintext pairs whose
  ciphertext hex encoding is widely published, e.g. on Wikipedia's "RC4"
  article). `combine`'s output is compared via a small local hex-encoding
  helper rather than `BEq ByteArray` string-literal comparisons, so a
  mismatch shows up as a readable hex string.
-/
import Linen.Crypto.RC4

open Crypto.RC4

namespace Tests.Crypto.RC4

/-! ### Hex encoding of test outputs -/

/-- Render a nibble (`0–15`) as an uppercase hex digit. -/
private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + '0'.toNat) else Char.ofNat (n - 10 + 'A'.toNat)

/-- Render a `ByteArray` as an uppercase hex string, two digits per byte. -/
private def toHex (bs : ByteArray) : String :=
  String.ofList (bs.toList.flatMap fun b => [hexDigit (b.toNat / 16), hexDigit (b.toNat % 16)])

/-! ### Known-answer tests

    Key/plaintext/ciphertext triples with widely published ciphertexts. -/

#guard toHex (combine (initCtx "Key".toUTF8) "Plaintext".toUTF8).2 == "BBF316E8D940AF0AD3"
#guard toHex (combine (initCtx "Wiki".toUTF8) "pedia".toUTF8).2 == "1021BF0420"
#guard toHex (combine (initCtx "Secret".toUTF8) "Attack at dawn".toUTF8).2
        == "45A01F645FC35B383552544B9BF5"

/-! ### `combine` is its own inverse (XOR-based stream cipher)

    Decrypting a ciphertext (from a freshly re-initialized context with the
    same key) recovers the original plaintext. -/

#guard
  let plaintext := "The quick brown fox".toUTF8
  let ctext := (combine (initCtx "shared-secret".toUTF8) plaintext).2
  let dtext := (combine (initCtx "shared-secret".toUTF8) ctext).2
  dtext.toList == plaintext.toList

/-! ### Streaming `combine` over several chunks matches one shot

    Feeding the input through `combine` in pieces (each call advancing and
    reusing the returned context) yields the same keystream as a single
    call over the concatenated input. -/

#guard
  let key := initCtx "chunked-key".toUTF8
  let whole := (combine key "abcdefghij".toUTF8).2
  let (ctx1, part1) := combine key "abcde".toUTF8
  let (_, part2) := combine ctx1 "fghij".toUTF8
  whole.toList == part1.toList ++ part2.toList

/-! ### Empty input yields empty output, context otherwise unchanged -/

#guard (combine (initCtx "Key".toUTF8) ByteArray.empty).2.toList == ([] : List UInt8)

end Tests.Crypto.RC4
