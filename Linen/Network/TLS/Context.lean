/-
  Network.TLS.Context — TLS context and session management

  Opaque handles wrapping OpenSSL's SSL_CTX and SSL objects via FFI.
  Resources are automatically cleaned up by the GC finalizer.

  ## Design
  Uses the same `lean_alloc_external` / `lean_register_external_class` pattern
  as the socket FFI. SSL_CTX is created once per server (shared across connections),
  SSL sessions are per-connection.

  ## Guarantees
  - TLS context requires valid cert + key at creation time (checked by OpenSSL)
  - SSL_shutdown is called automatically on session finalization
  - Read/write on closed sessions return empty/error
-/
import Linen.Network.TLS.Types
import Linen.Network.Socket.FFI

namespace Network.TLS

/-- Opaque handle to an OpenSSL SSL_CTX (TLS server context).
    Created once, shared across all TLS connections. -/
opaque TLSContextHandle : NonemptyType
def TLSContext := TLSContextHandle.type
instance : Nonempty TLSContext := TLSContextHandle.property

/-- Opaque handle to an OpenSSL SSL session (one per TLS connection). -/
opaque TLSSessionHandle : NonemptyType
def TLSSession := TLSSessionHandle.type
instance : Nonempty TLSSession := TLSSessionHandle.property

/-- Create a TLS server context with the given certificate and key files.
    $$\text{createContext} : \text{String} \to \text{String} \to \text{IO TLSContext}$$ -/
@[extern "linen_tls_ctx_create"]
opaque createContext (certPath : @& String) (keyPath : @& String) : IO TLSContext

/-- Enable ALPN negotiation on the context (for HTTP/2 support). -/
@[extern "linen_tls_ctx_set_alpn"]
opaque setAlpn (ctx : @& TLSContext) : IO Unit

/-- Perform a TLS handshake on a connected socket.
    The socket handle is an opaque external object containing the fd.
    Returns the established TLS session.
    $$\text{accept} : \text{TLSContext} \to \text{RawSocket} \to \text{IO TLSSession}$$ -/
@[extern "linen_tls_accept_socket"]
opaque acceptSocket (ctx : @& TLSContext) (sock : @& Network.Socket.RawSocket) : IO TLSSession

/-- Read up to `maxLen` bytes from the TLS session.
    Returns empty ByteArray on EOF or error.
    $$\text{read} : \text{TLSSession} \to \text{USize} \to \text{IO ByteArray}$$ -/
@[extern "linen_tls_read"]
opaque read (session : @& TLSSession) (maxLen : USize) : IO ByteArray

/-- Write all bytes to the TLS session.
    $$\text{write} : \text{TLSSession} \to \text{ByteArray} \to \text{IO Unit}$$ -/
@[extern "linen_tls_write"]
opaque write (session : @& TLSSession) (data : @& ByteArray) : IO Unit

/-- Shut down the TLS session and free resources.
    $$\text{close} : \text{TLSSession} \to \text{IO Unit}$$ -/
@[extern "linen_tls_close"]
opaque close (session : @& TLSSession) : IO Unit

/-- Get the negotiated TLS protocol version string. -/
@[extern "linen_tls_get_version"]
opaque getVersion (session : @& TLSSession) : IO String

/-- Get the ALPN-negotiated protocol (e.g., "h2" or "http/1.1"). -/
@[extern "linen_tls_get_alpn"]
opaque getAlpn (session : @& TLSSession) : IO (Option String)

-- ── Non-blocking TLS operations ──

/-- Non-blocking TLS handshake. Returns `TLSOutcome TLSSession`.
    On `.wantRead`/`.wantWrite`, wait for socket readiness then retry.
    $$\text{acceptSocketNB} : \text{TLSContext} \to \text{RawSocket} \to \text{IO (TLSOutcome TLSSession)}$$ -/
@[extern "linen_tls_accept_socket_nb"]
opaque acceptSocketNB (ctx : @& TLSContext) (sock : @& Network.Socket.RawSocket)
    : IO (TLSOutcome TLSSession)

/-- Non-blocking TLS read. Returns `TLSOutcome ByteArray`.
    $$\text{readNB} : \text{TLSSession} \to \text{USize} \to \text{IO (TLSOutcome ByteArray)}$$ -/
@[extern "linen_tls_read_nb"]
opaque readNB (session : @& TLSSession) (maxLen : USize) : IO (TLSOutcome ByteArray)

/-- Non-blocking TLS write. Returns `TLSOutcome Unit`.
    $$\text{writeNB} : \text{TLSSession} \to \text{ByteArray} \to \text{IO (TLSOutcome Unit)}$$ -/
@[extern "linen_tls_write_nb"]
opaque writeNB (session : @& TLSSession) (data : @& ByteArray) : IO (TLSOutcome Unit)

-- ── Client-side TLS ──

/-- Create a TLS client context with system CA trust for server verification.
    No client certificate needed. Used for outgoing HTTPS connections.
    $$\text{createClientContext} : \text{IO TLSContext}$$ -/
@[extern "linen_tls_client_ctx_create"]
opaque createClientContext : IO TLSContext

/-- Create a TLS client context trusting only the CA certificate(s) at
    `caPath`, instead of the system default trust store. Useful for
    connecting to servers presenting a certificate signed by a private
    or self-signed CA (e.g. in tests).
    $$\text{createClientContextWithCA} : \text{String} \to \text{IO TLSContext}$$ -/
@[extern "linen_tls_client_ctx_create_with_ca"]
opaque createClientContextWithCA (caPath : @& String) : IO TLSContext

/-- Raw blocking TLS client handshake (C FFI). Does not handle
    WANT_READ/WANT_WRITE — use `connectSocket` for robustness. -/
@[extern "linen_tls_connect_socket"]
opaque connectSocketRaw (ctx : @& TLSContext) (sock : @& Network.Socket.RawSocket)
    (hostname : @& String) : IO TLSSession

/-- Non-blocking TLS client handshake. Returns `TLSOutcome TLSSession`.
    On `.wantRead`/`.wantWrite`, wait for socket readiness then retry.
    $$\text{connectSocketNB} : \text{TLSContext} \to \text{RawSocket} \to \text{String} \to \text{IO (TLSOutcome TLSSession)}$$ -/
@[extern "linen_tls_connect_socket_nb"]
opaque connectSocketNB (ctx : @& TLSContext) (sock : @& Network.Socket.RawSocket)
    (hostname : @& String) : IO (TLSOutcome TLSSession)

/-- Blocking TLS client handshake with select()-based retry.
    Uses the non-blocking FFI and polls for socket readiness on
    WANT_READ/WANT_WRITE, handling the case where the underlying
    TCP socket may not be fully ready yet.
    Sets SNI (Server Name Indication) from the hostname.
    The server's certificate is verified against system CA trust store.
    $$\text{connectSocket} : \text{TLSContext} \to \text{RawSocket} \to \text{String} \to \text{IO TLSSession}$$ -/
def connectSocket (ctx : @& TLSContext) (sock : @& Network.Socket.RawSocket)
    (hostname : @& String) : IO TLSSession := do
  while true do
    match ← connectSocketNB ctx sock hostname with
    | .ok session => return session
    | .error e => throw e
    | .wantRead =>
      match ← Network.Socket.FFI.socketPoll sock Network.Socket.PollMode.read.toUInt8 30000 with
      | .ready => pure ()
      | .timeout => throw (IO.userError "TLS handshake timed out (waiting for read)")
      | .error e => throw e
    | .wantWrite =>
      match ← Network.Socket.FFI.socketPoll sock Network.Socket.PollMode.write.toUInt8 30000 with
      | .ready => pure ()
      | .timeout => throw (IO.userError "TLS handshake timed out (waiting for write)")
      | .error e => throw e
  unreachable!

end Network.TLS
