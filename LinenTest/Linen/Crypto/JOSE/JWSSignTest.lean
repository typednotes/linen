/-
  Tests for `Linen.Crypto.JOSE.JWS` signature creation.

  The verification side is covered in `JWSTest`; this covers the signing
  primitives added alongside it, and the property that matters most about
  them: a signature this library produces is one it also accepts.

  ## Test key

  A throwaway 2048-bit RSA key generated for these tests only, together with
  the modulus and exponent of its public half so the round trip can be closed
  without a key-derivation primitive. It signs nothing outside this file and
  guards nothing; it is test data, like a fixed seed.
-/

import Linen.Crypto.JOSE.JWS

open Crypto.JOSE Crypto.JOSE.JWS

namespace Tests.Crypto.JOSE.JWSSign

-- ── Test key ──

/-- A throwaway RSA private key. Test data only. -/
def testKeyPem : String :=
  "-----BEGIN PRIVATE KEY-----\n\
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDpcDBZHjvbtp68\n\
ZJmXXFGQzgYVgOUZXMVs/mFiyelPoBrY2ZuQ+McU8m0xXzzGvEbe/isgChZ3g8+l\n\
JsL900iwLetXfnhdqFU7j+WPpB1Jx/tfqG3qOFPR0tCPndwKFtVyn1nJjHmZYT5N\n\
7RBSrVIfWhwa7y5tkpF19Rbrta0KH4d2+rXmYwFMT5Ft6wUpe6WifTdThfWNd47/\n\
c6kdqkWZtzTE9q7RkWCNrqk1C+W5lU3HdaxYEGmGk5tSWWiM3ZjqO3IYPXylc/Yi\n\
OZE2zCu6b59geQIuyp14kzDmHHp/qqNP0ykdKbyZTJgZr74Ug8mLDekupAl4Knd5\n\
ejWze0aXAgMBAAECggEABfU6ZqviNI4JUx7w2e9zQtI0pDaGol+/HN8JNppB/X7v\n\
8HpCrC/in42TF+ApuZtzO5xvwVZAkzWmsRJiMP7u1gBLXLpPnCRVuJAdoyLkfyOU\n\
LjGATKq6CPBBKR6K+n7xXQblexgTam9pmwI37m7vWk7UdM4x+H311HWNljUsK3FC\n\
ExQwifiT1eRcFLtGOYm5iNpJtLIbKZYguhb5rDoHYoqBflt5T9/R1cGMfcAR5V7J\n\
6uF8hwcc1vvv/knNHbkbA9qOAz+imFFr6LDzFltOU4rrqg9/4BMciQRQUg+FBWS0\n\
XUi8Z6X+UfQwyMkgsF6NQ+2F9Ii6M0haGh0WXgShkQKBgQD/IjVOr1j3enLrYKCi\n\
iVEy7L1aPxK9be4qx+W0n86e6jfaFNUx1u7eL+zKvNdWUWA4JPSr8LGYREcbJVns\n\
YzO6VLfuiOjIcUvz45DQKcADoc0HT1KE235QMr9+HlLMtDka46Alo2Y4zYfmX0jY\n\
R56CiHSMG1iSejwRMuYcq+fbAwKBgQDqOx7XoWP1cYNfydf2WMArgTrUW8y5PdjT\n\
tJcSgmSjb3ALRsl8Dvhc/DTSb1g5Rm6JL7TSnUx4yCYWvdBVxOxiQ3F5fR705k2F\n\
pUZTkvTJ5GhaSgIs/SIryVG1RiD0kVOspV+MI/7M9iLB52ltRlQkCqQYS+ulUC/l\n\
io3m92dn3QKBgQCMNZB2HYcW+gQNtpyQtkYZZmDpJ6B02eT5PcHO8cPrMWxgPPKs\n\
4SGEmXHYOM9ecHogYK7VjwEKXPt2v6AbeKkEzWoHfNXw0dKbxYPf4hHT7SdvzPfc\n\
a4OPL1RtStzWAnUfgdiQ1qtmrAzzXYn60eEae0MRfDXAycwY54/uUcqpYQKBgCcK\n\
670tnafP4AIbdvANIxsdU10KYDmQYZAIThY7veKwNJDsn7EaHbQCJhvdi2sgnlQn\n\
q5Bfv9tyIUcxJITnai+G5mdFv986dDmOrwZHPJ5agDpsk6hEGWoLCJ+arOuXPcdN\n\
WXvWlCY98NU5aY1ZZ7UKQQf7v6+yiglM6xJQst/RAoGAIWEVDnRDRTMupQ6yyPei\n\
qwtb2/Ew+5iwjMLHAhUdRrDJbl/kmI6DC2yLkFA5YjHoqx+9kOXzZzmLae9V384y\n\
pPJ+UAHBFApKyTS/x/dRWY6fVvnGrk3SXgyOizCg+rOlBxTYacPMXiJlEuIGANBH\n\
tgNxv+cGCEjoVJaNhTyrGYs=\n\
-----END PRIVATE KEY-----"

/-- The modulus of the matching public key, base64url, as a JWK carries it. -/
def testN : String := "6XAwWR4727aevGSZl1xRkM4GFYDlGVzFbP5hYsnpT6Aa2NmbkPjHFPJtMV88xrxG3v4rIAoWd4PPpSbC_dNIsC3rV354XahVO4_lj6QdScf7X6ht6jhT0dLQj53cChbVcp9ZyYx5mWE-Te0QUq1SH1ocGu8ubZKRdfUW67WtCh-Hdvq15mMBTE-RbesFKXulon03U4X1jXeO_3OpHapFmbc0xPau0ZFgja6pNQvluZVNx3WsWBBphpObUllojN2Y6jtyGD18pXP2IjmRNswrum-fYHkCLsqdeJMw5hx6f6qjT9MpHSm8mUyYGa--FIPJiw3pLqQJeCp3eXo1s3tGlw"

/-- The public exponent, base64url — 65537. -/
def testE : String := "AQAB"

/-- The public half, as a JWK, so a signature can be checked against it. -/
def testJwk : IO JWK := do
  let n ← FFI.base64urlDecode testN
  let e ← FFI.base64urlDecode testE
  -- `JWK` carries a proof that `kty` agrees with `material`; for a concrete
  -- pair the witnesses are the components themselves.
  return { kty := .RSA, material := .rsa n e none
           kty_material_coherent := by
             refine ⟨fun _ => ⟨n, e, none, rfl⟩, fun h => ?_, fun h => ?_⟩ <;> cases h }

-- ── Algorithm codes ──

#guard rsaCodes .RS256 == some (0, 0)
#guard rsaCodes .RS512 == some (2, 0)
#guard rsaCodes .PS256 == some (0, 1)
#guard rsaCodes .PS384 == some (1, 1)
-- HMAC needs no private key and EC signing is not implemented, so both are
-- `none` rather than a wrong-but-plausible code.
#guard rsaCodes .HS256 == none
#guard rsaCodes .ES256 == none

-- ── Round trip: what we sign, we verify ──

/-- Sign a payload and check the signature back against the public key.

    This is the whole point of the addition. A signing primitive that produced
    well-formed bytes which its own verifier rejected would pass every
    structural test and be useless. -/
def roundTrip (alg : JWSAlgorithm) (payload : String) : IO Bool := do
  let der ← FFI.privkeyPemToDer testKeyPem
  match ← signRsa alg der payload.toUTF8 with
  | none     => return false
  | some sig => verifySignature alg (← testJwk) payload.toUTF8 sig

/-- A signature over *different* data must not verify — otherwise the round
    trip above would pass for a function that ignored its input. -/
def rejectsTampered : IO Bool := do
  let der ← FFI.privkeyPemToDer testKeyPem
  match ← signRsa .RS256 der "the original".toUTF8 with
  | none     => return false
  | some sig => return !(← verifySignature .RS256 (← testJwk) "something else".toUTF8 sig)

/-- A compact serialization has three base64url segments and its middle one
    is the payload we asked for. -/
def compactShape : IO Bool := do
  let der ← FFI.privkeyPemToDer testKeyPem
  match ← signCompact .RS256 der "{\"alg\":\"RS256\"}" "{\"sub\":\"me\"}" with
  | none => return false
  | some tok =>
    match splitCompact tok with
    | none => return false
    | some (_, p, s) =>
      let decoded ← FFI.base64urlDecode p
      return !s.isEmpty && String.fromUTF8? decoded == some "{\"sub\":\"me\"}"

/-- Every RSA algorithm round-trips, not just the common one. -/
def allRsaRoundTrip : IO Bool := do
  let algs : List JWSAlgorithm := [.RS256, .RS384, .RS512, .PS256, .PS384, .PS512]
  algs.allM fun a => roundTrip a "payload to sign"

/-- `#guard` cannot run `IO`, so the checks above are driven from here and
    `lake build Tests` fails loudly if any of them is false. -/
def main : IO Unit := do
  let checks : List (String × IO Bool) :=
    [ ("RS256 round trip",        roundTrip .RS256 "hello")
    , ("every RSA algorithm",     allRsaRoundTrip)
    , ("tampered data rejected",  rejectsTampered)
    , ("compact serialization",   compactShape) ]
  for (name, act) in checks do
    unless ← act do throw (IO.userError s!"JWS signing: {name} failed")

#eval main

end Tests.Crypto.JOSE.JWSSign
