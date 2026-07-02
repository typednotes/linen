/-
  Network.TLS.Types — TLS type definitions

  Core types for the TLS FFI wrapper.
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
