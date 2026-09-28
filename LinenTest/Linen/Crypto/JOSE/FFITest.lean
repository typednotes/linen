/-
  Tests for `Linen.Crypto.JOSE.FFI`.

  Every binding is a live OpenSSL operation in `IO`, so it cannot be exercised
  by `#guard`/`#eval` deterministically here.  These `example`s pin down the
  exact signature of each `@[extern]` binding — and, because the module is
  linked against OpenSSL, a successful build confirms every `linen_jose_*`
  symbol resolves.
-/
import Linen.Crypto.JOSE.FFI

open Crypto.JOSE.FFI

namespace Tests.Crypto.JOSE.FFI

example : ByteArray → ByteArray → UInt8 → IO ByteArray := hmac
example : ByteArray → ByteArray → ByteArray → UInt8 → UInt8 → IO UInt8 := rsaVerify
example : ByteArray → ByteArray → ByteArray → UInt8 → IO UInt8 := ecVerify
example : ByteArray → ByteArray → IO ByteArray := rsaPubkeyFromComponents
example : UInt8 → ByteArray → ByteArray → IO ByteArray := ecPubkeyFromComponents
example : String → IO ByteArray := base64urlDecode
example : ByteArray → IO String := base64urlEncode

end Tests.Crypto.JOSE.FFI
