/-
  Linen.Crypto.SecureRandom — CSPRNG byte generation

  A minimal binding to OpenSSL's `RAND_bytes`, providing cryptographically
  secure random bytes.

  ## Provenance
  Added as part of the `hoauth2` Hackage import (see
  `docs/imports/hoauth2/dependencies.md`) to back PKCE's `code_verifier`
  generation (`Linen.Network.OAuth2.Experiment.Pkce`), in place of porting
  the whole `crypton` package (`Crypto.Random`) for this one primitive.

  ## FFI
  Implementation in `ffi/jose.c` (symbol `linen_crypto_random_bytes`),
  reusing the OpenSSL link already established for `Linen.Crypto.JOSE.FFI`;
  no `lakefile.lean` changes were needed.

  Because this is a live cryptographic operation backed by a C library, the
  tests only check the returned length and that two independent calls
  produce different output, not exact values.
-/

namespace Crypto.SecureRandom

/-- Generate `n` cryptographically secure random bytes.

    $$\text{randomBytes} : \mathbb{N} \to \text{IO ByteArray}, \quad
      |\text{randomBytes}(n)| = n$$ -/
@[extern "linen_crypto_random_bytes"]
opaque randomBytes (n : @& Nat) : IO ByteArray

end Crypto.SecureRandom
