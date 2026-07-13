/-
  Tests for `Linen.Network.OAuth2.Experiment.Pkce`.

  `genCodeVerifier`/`encodeCodeVerifier`/`mkPkceParam` run in `IO` (real
  OpenSSL FFI calls), so behaviour is checked with `#eval` (a thrown error
  fails the build).
-/
import Linen.Network.OAuth2.Experiment.Pkce

open Network.OAuth2.Experiment.Pkce

namespace Tests.Network.OAuth2.Experiment.Pkce

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

private def isUnreserved (c : Char) : Bool :=
  (c ≥ 'a' && c ≤ 'z') || (c ≥ 'A' && c ≤ 'Z') || c == '-' || c == '.' || c == '_' || c == '~'

-- `genCodeVerifier` produces a `cvMaxLen`-character string over the `unreserved` alphabet.
#eval show IO Unit from do
  let cv ← genCodeVerifier
  check (cv.length == cvMaxLen) s!"code verifier length: {cv.length}"
  check (cv.toList.all isUnreserved) "code verifier should be all `unreserved` characters"

-- `S256` code challenge = base64url(sha256(code_verifier)), well-known test vector.
#eval show IO Unit from do
  -- RFC 7636 Appendix B's example (S256 challenge for a fixed verifier).
  let challenge ← encodeCodeVerifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
  check (challenge == "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")
    s!"S256 challenge: {challenge}"

-- `mkPkceParam` bundles a fresh verifier/challenge pair, always `S256`.
#eval show IO Unit from do
  let p ← mkPkceParam
  check (p.codeVerifier.unCodeVerifier.length == cvMaxLen) "verifier length"
  check (p.codeChallengeMethod == .S256) "challenge method"
  let expected ← encodeCodeVerifier p.codeVerifier.unCodeVerifier
  check (p.codeChallenge.unCodeChallenge == expected) "challenge matches verifier"

/-! ### Signatures -/

example : IO String := genCodeVerifier
example : String → IO String := encodeCodeVerifier
example : IO PkceRequestParam := mkPkceParam

end Tests.Network.OAuth2.Experiment.Pkce
