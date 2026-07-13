/-
  Tests for `Linen.Crypto.SHA256`.

  The digest runs in `IO` (it calls the OpenSSL FFI), so behaviour is checked
  with `#eval` against known SHA-256 test vectors (a thrown error fails the
  build), which also confirms the OpenSSL FFI actually works end-to-end.
-/
import Linen.Crypto.SHA256

open Crypto.SHA256

namespace Tests.Crypto.SHA256

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

/-- Lowercase hex encoding of a `ByteArray`, for comparing against test vectors. -/
private def toHex (bytes : ByteArray) : String :=
  let hexDigit (n : Nat) : Char := if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
  String.join (bytes.toList.map fun b => String.ofList [hexDigit (b.toNat / 16), hexDigit (b.toNat % 16)])

-- SHA-256 of the empty string is a well-known test vector.
#eval show IO Unit from do
  let d ← digest ByteArray.empty
  check (d.size == 32) s!"digest size: {d.size}"
  check (toHex d == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    s!"sha256(\"\") = {toHex d}"

-- SHA-256 of "abc" is another well-known test vector (FIPS 180-2).
#eval show IO Unit from do
  let d ← digest "abc".toUTF8
  check (toHex d == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    s!"sha256(\"abc\") = {toHex d}"

/-! ### Signatures -/

example : ByteArray → IO ByteArray := digest

end Tests.Crypto.SHA256
