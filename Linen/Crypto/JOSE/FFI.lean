/-
  Linen.Crypto.JOSE.FFI — FFI declarations for jose.c

  External function declarations for the JOSE crypto operations
  implemented in `ffi/jose.c` using OpenSSL's EVP API (HMAC, RSA/EC
  signature verification, JWK→DER key construction, base64url).

  ## FFI
  - Implementation in `ffi/jose.c` (symbols `linen_jose_*`).
  - Requires OpenSSL headers/lib, resolved via `pkg-config openssl` in the
    lakefile; OpenSSL is therefore a build-time dependency of the library.

  Because these are live cryptographic operations, they cannot be exercised by
  `#guard`/`#eval`; the test suite only typechecks the signatures.
-/

namespace Crypto.JOSE.FFI

/-- Compute HMAC using the specified SHA algorithm.
    `algorithm`: 0=SHA256, 1=SHA384, 2=SHA512. -/
@[extern "linen_jose_hmac"]
opaque hmac (key : @& ByteArray) (data : @& ByteArray)
    (algorithm : UInt8) : IO ByteArray

/-- Create an RSA signature over `data` with a DER-encoded PKCS#8 private key.

    The mirror of `rsaVerify`: same DER-in convention, same algorithm codes
    (`0=SHA256, 1=SHA384, 2=SHA512`) and the same `pss` flag
    (`0=PKCS1v15` for `RS*`, `1=PSS` for `PS*`). Returns the raw signature
    bytes, which is what JWS base64url-encodes as its third segment. -/
@[extern "linen_jose_rsa_sign"]
opaque rsaSign (privkeyDer : @& ByteArray) (data : @& ByteArray)
    (algorithm : UInt8) (pss : UInt8) : IO ByteArray

/-- Convert a PEM-encoded private key to DER.

    Private keys are distributed as PEM far more often than as DER, while
    `rsaSign` takes DER to stay symmetric with `rsaVerify`. This is the
    bridge, kept separate so neither function has to guess its input's
    encoding. -/
@[extern "linen_jose_privkey_pem_to_der"]
opaque privkeyPemToDer (pem : @& String) : IO ByteArray

/-- Verify an RSA signature.  Returns 1 if valid, 0 if invalid.
    `algorithm`: 0=SHA256, 1=SHA384, 2=SHA512.
    `usePss`: 1 for PSS padding, 0 for PKCS#1 v1.5. -/
@[extern "linen_jose_rsa_verify"]
opaque rsaVerify (pubkeyDer : @& ByteArray) (data : @& ByteArray)
    (signature : @& ByteArray) (algorithm : UInt8) (usePss : UInt8) : IO UInt8

/-- Verify an EC signature.  Returns 1 if valid, 0 if invalid.
    `algorithm`: 0=SHA256/P-256, 1=SHA384/P-384, 2=SHA512/P-521. -/
@[extern "linen_jose_ec_verify"]
opaque ecVerify (pubkeyDer : @& ByteArray) (data : @& ByteArray)
    (signature : @& ByteArray) (algorithm : UInt8) : IO UInt8

/-- Build an RSA public key from modulus (n) and exponent (e).
    Returns a DER-encoded public key. -/
@[extern "linen_jose_rsa_pubkey_from_components"]
opaque rsaPubkeyFromComponents (n : @& ByteArray) (e : @& ByteArray) : IO ByteArray

/-- Build an EC public key from curve and coordinates.
    Returns a DER-encoded public key.
    `crv`: 0=P-256, 1=P-384, 2=P-521. -/
@[extern "linen_jose_ec_pubkey_from_components"]
opaque ecPubkeyFromComponents (crv : UInt8) (x : @& ByteArray)
    (y : @& ByteArray) : IO ByteArray

/-- Decode base64url-encoded data. -/
@[extern "linen_jose_base64url_decode"]
opaque base64urlDecode (input : @& String) : IO ByteArray

/-- Encode data as base64url. -/
@[extern "linen_jose_base64url_encode"]
opaque base64urlEncode (input : @& ByteArray) : IO String

end Crypto.JOSE.FFI
