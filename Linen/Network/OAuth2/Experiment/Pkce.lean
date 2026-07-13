/-
  Linen.Network.OAuth2.Experiment.Pkce — RFC 7636 PKCE parameters

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Pkce` (see
  `docs/imports/hoauth2/dependencies.md`): the `code_verifier` /
  `code_challenge` / `code_challenge_method` triple used by the
  Authorization Code + PKCE flow (RFC 7636).

  ## Substitutions
  - `crypton`'s `Crypto.Hash`/`Crypto.Random` are `linen`'s own
    `Linen.Crypto.SHA256`/`Linen.Crypto.SecureRandom` — two small new
    OpenSSL-backed FFI primitives added by this very import (see
    `dependencies.md`), in place of pulling in all of `crypton` for two
    primitives.
  - `Data.ByteString.Base64.URL`'s unpadded base64url encoder is
    `Linen.Crypto.JOSE.FFI.base64urlEncode` (already strips `=` padding —
    see `ffi/jose.c`), the idiom this codebase already uses for base64url
    (over `Linen.Data.Base64`, which only offers standard/padded base64).

  ## `genCodeVerifier`: a from-uniform-bytes simplification
  Upstream's `getBytesInternal` draws random bytes and **rejects** any byte
  outside the `unreserved` alphabet (RFC 7636 §4.1's
  `code-verifier = 43*128unreserved`), looping until it has accumulated
  `cvMaxLen` accepted bytes. That loop's termination is only probabilistic
  (an adversarial or degenerate CSPRNG could reject forever) and Lean has no
  structural/well-founded measure for it — the same class of issue as a
  literal loop-until-success. RFC 7636 does not mandate rejection sampling;
  it only requires the *result* to be a random string of `unreserved`
  characters of the right length. So this port maps each of `cvMaxLen`
  drawn bytes into the 66-character `unreserved` alphabet with `% 66`
  instead of rejecting — every draw is used, the recursion is a plain
  structural map over a fixed-length byte array, and the required property
  (uniformly-driven, appropriate-length, `unreserved`-only string) still
  holds. This is the kind of implementation-detail simplification
  AGENTS.md's import convention reserves for cases upstream doesn't fully
  specify, not a substitute for proving a genuine termination obligation.
-/

import Linen.Crypto.SHA256
import Linen.Crypto.SecureRandom
import Linen.Crypto.JOSE.FFI

namespace Network.OAuth2.Experiment.Pkce

/-- A PKCE code challenge (RFC 7636 §4.2): the value sent in the
    authorization request. -/
structure CodeChallenge where
  unCodeChallenge : String
deriving Repr, BEq

/-- A PKCE code verifier (RFC 7636 §4.1): the secret kept by the client and
    sent in the token request. -/
structure CodeVerifier where
  unCodeVerifier : String
deriving Repr, BEq

/-- The `code_challenge_method` parameter. `hoauth2` only ever produces
    `S256` in practice (RFC 7636 §4.3 makes it optional, but `plain` is
    discouraged), so that is the only constructor ported. -/
inductive CodeChallengeMethod where
  | S256
deriving Repr, BEq

instance : ToString CodeChallengeMethod where
  toString | .S256 => "S256"

/-- The full PKCE parameter triple for one authorization attempt. -/
structure PkceRequestParam where
  codeVerifier : CodeVerifier
  codeChallenge : CodeChallenge
  /-- Spec says optional but in practice it is always `S256`, RFC 7636 §4.3. -/
  codeChallengeMethod : CodeChallengeMethod
deriving Repr, BEq

/-- RFC 7636 §4.1: `code-verifier` length in characters. -/
def cvMaxLen : Nat := 128

/-- RFC 7636 §4.1's `unreserved` alphabet: `ALPHA / DIGIT / "-" / "." / "_" / "~"`
    — 66 characters, indexed for the `% 66` mapping in `genCodeVerifier`. -/
def unreservedAlphabet : Array Char :=
  (Array.range 26).map (fun i => Char.ofNat (97 + i))          -- a-z
    ++ (Array.range 26).map (fun i => Char.ofNat (65 + i))     -- A-Z
    ++ #['-', '.', '_', '~']

/-- Map a random byte onto the `unreserved` alphabet (see the module
    doc-comment for why this replaces upstream's rejection-sampling loop). -/
private def toUnreservedChar (b : UInt8) : Char :=
  unreservedAlphabet[b.toNat % unreservedAlphabet.size]!

/-- Generate a `cvMaxLen`-character `code_verifier` drawn from `unreservedAlphabet`,
    using the CSPRNG.

    $$\text{genCodeVerifier} : \text{IO String}$$ -/
def genCodeVerifier : IO String := do
  let raw ← Crypto.SecureRandom.randomBytes cvMaxLen
  pure (String.ofList (raw.toList.map toUnreservedChar))

/-- Derive the `S256` `code_challenge` from a `code_verifier`:
    `base64url(sha256(code_verifier))`, unpadded.

    $$\text{encodeCodeVerifier} : \text{String} \to \text{IO String}$$ -/
def encodeCodeVerifier (codeVerifier : String) : IO String := do
  let digest ← Crypto.SHA256.digest codeVerifier.toUTF8
  Crypto.JOSE.FFI.base64urlEncode digest

/-- Generate a fresh PKCE parameter triple: a random `code_verifier` and its
    `S256` `code_challenge`.

    $$\text{mkPkceParam} : \text{IO PkceRequestParam}$$ -/
def mkPkceParam : IO PkceRequestParam := do
  let codeV ← genCodeVerifier
  let challenge ← encodeCodeVerifier codeV
  pure
    { codeVerifier := ⟨codeV⟩
      codeChallenge := ⟨challenge⟩
      codeChallengeMethod := .S256 }

end Network.OAuth2.Experiment.Pkce
