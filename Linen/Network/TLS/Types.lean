/-
  Network.TLS.Types — TLS type definitions

  Core types for the TLS FFI wrapper.

  ## Specifications

  `linen` implements none of these: `ffi/tls.c` binds OpenSSL, which does. They
  are what the versions in `TLSVersion` name, and what a peer is agreeing to.

  - **RFC 8446** — TLS 1.3 (`tls13`). A different handshake from its
    predecessors rather than an increment: one round trip, the legacy cipher
    suites removed, and renegotiation gone.
  - **RFC 5246** — TLS 1.2 (`tls12`), still the floor most servers accept.
  - **RFC 4346** / **RFC 2246** — TLS 1.1 (`tls11`) and 1.0 (`tls10`), both
    **deprecated by RFC 8996**. They are representable here because a client
    may have to report what a peer offered, not because they should be
    selected.
  - **RFC 6066** — the Server Name Indication extension, which is why a
    context carries a hostname separate from the address it connects to.
-/
namespace Network.TLS

/-- TLS protocol version. -/
inductive TLSVersion where
  | tls10 | tls11 | tls12 | tls13
deriving BEq, Repr

instance : ToString TLSVersion where
  toString
    | .tls10 => "TLSv1.0"
    | .tls11 => "TLSv1.1"
    | .tls12 => "TLSv1.2"
    | .tls13 => "TLSv1.3"

/-- TLS cipher ID. -/
abbrev CipherID := UInt16

/-- Outcome of a non-blocking TLS operation.
    OpenSSL returns `SSL_ERROR_WANT_READ` or `SSL_ERROR_WANT_WRITE` when the
    underlying socket needs readiness before the TLS operation can proceed.
    - `.ok` — operation completed successfully
    - `.wantRead` — need to wait for socket readability, then retry
    - `.wantWrite` — need to wait for socket writability, then retry
    - `.error` — TLS-level error -/
inductive TLSOutcome (α : Type) where
  | ok        : α → TLSOutcome α
  | wantRead  : TLSOutcome α
  | wantWrite : TLSOutcome α
  | error     : IO.Error → TLSOutcome α

end Network.TLS
