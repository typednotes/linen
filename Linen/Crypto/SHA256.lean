/-
  Linen.Crypto.SHA256 — plain SHA-256 digest

  A minimal binding to OpenSSL's `EVP_sha256`/`EVP_Digest`, exposing a
  bare cryptographic hash (as opposed to `Linen.Crypto.JOSE.FFI.hmac`,
  which is keyed HMAC).

  ## Provenance
  Added as part of the `hoauth2` Hackage import (see
  `docs/imports/hoauth2/dependencies.md`) to back PKCE's `S256` code
  challenge method (`Linen.Network.OAuth2.Experiment.Pkce`), in place of
  porting the whole `crypton` package (`Crypto.Hash`) for this one
  primitive.

  ## FFI
  Implementation in `ffi/jose.c` (symbol `linen_crypto_sha256`), reusing
  the OpenSSL link already established for `Linen.Crypto.JOSE.FFI`; no
  `lakefile.lean` changes were needed.

  Because this is a live cryptographic operation backed by a C library,
  it is exercised with `#guard` against known SHA-256 test vectors rather
  than proved from first principles.
-/

namespace Crypto.SHA256

/-- Compute the SHA-256 digest of `data`, returning the 32-byte hash.

    $$\text{digest} : \text{ByteArray} \to \text{IO ByteArray}, \quad
      |\text{digest}(x)| = 32$$ -/
@[extern "linen_crypto_sha256"]
opaque digest (data : @& ByteArray) : IO ByteArray

end Crypto.SHA256
