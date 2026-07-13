/-
  Tests for `Linen.Crypto.SecureRandom`.

  The generator runs in `IO` (it calls the OpenSSL FFI), so behaviour is
  checked with `#eval` (a thrown error fails the build): the returned length
  matches the request, `n = 0` yields the empty array, and two independent
  calls produce different bytes (as expected of a CSPRNG, with astronomically
  low false-failure probability).
-/
import Linen.Crypto.SecureRandom

open Crypto.SecureRandom

namespace Tests.Crypto.SecureRandom

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

#eval show IO Unit from do
  let bs ← randomBytes 32
  check (bs.size == 32) s!"randomBytes 32 size: {bs.size}"

#eval show IO Unit from do
  let bs ← randomBytes 0
  check (bs.size == 0) s!"randomBytes 0 size: {bs.size}"

#eval show IO Unit from do
  let a ← randomBytes 32
  let b ← randomBytes 32
  check (a != b) "two independent randomBytes calls should differ"

/-! ### Signatures -/

example : Nat → IO ByteArray := randomBytes

end Tests.Crypto.SecureRandom
