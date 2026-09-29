/-
  Network.TLS.Context — TLS context and session management

  Opaque handles wrapping OpenSSL's SSL_CTX and SSL objects via FFI.
  Resources are automatically cleaned up by the GC finalizer.

  ## Design
  Uses the same `lean_alloc_external` / `lean_register_external_class` pattern
  as the socket FFI. SSL_CTX is created once per server (shared across connections),
  SSL sessions are per-connection.

  ## Non-blocking use

  A session is created once (`newServerSession`/`newClientSession`) and its
  handshake stepped with `handshakeNB` until done — the design of
  rust-openssl's `MidHandshakeSslStream`, HsOpenSSL's `sslBlock` and
  tokio-openssl. `readNB` is tried *before* waiting for readability, and
  either direction may be asked for by either call. `Network.TLS.Green`
  drives all three on the `EventDispatcher`.

  Through 1.8.0, `acceptSocketNB`/`connectSocketNB` created a fresh `SSL`
  per call and freed it on every would-block, so a handshake needing a
  second read could never finish; they are removed.

  ## Guarantees
  - TLS context requires valid cert + key at creation time (checked by OpenSSL)
  - `close` sends close_notify (once, and never after a fatal error); the GC
    finalizer only frees — it never writes, since by then the fd number may
    belong to another connection
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

-- ── Sessions and the resumable handshake ──

/-- A server-side session on a connected socket, before its handshake:
    `SSL_new` and the fd, in accept state. Drive it with `handshakeNB` (or
    `handshake`). Created once per connection — a handshake that has to
    wait is resumed on the same session, never restarted.
    $$\text{newServerSession} : \text{TLSContext} \to \text{RawSocket} \to \text{IO TLSSession}$$ -/
@[extern "linen_tls_session_new_server"]
opaque newServerSession (ctx : @& TLSContext) (sock : @& Network.Socket.RawSocket) : IO TLSSession

/-- A client-side session, in connect state, with SNI and the name the
    server's certificate is verified against set to `hostname`.
    $$\text{newClientSession} : \text{TLSContext} \to \text{RawSocket} \to \text{String} \to \text{IO TLSSession}$$ -/
@[extern "linen_tls_session_new_client"]
opaque newClientSession (ctx : @& TLSContext) (sock : @& Network.Socket.RawSocket)
    (hostname : @& String) : IO TLSSession

/-- One handshake step (`SSL_do_handshake`) on `session`: `.ok ()` once it
    is complete; `.wantRead`/`.wantWrite` to wait for the socket in that
    direction and step again — on the same session; `.error` otherwise.
    This is rust-openssl's `MidHandshakeSslStream::handshake`.
    $$\text{handshakeNB} : \text{TLSSession} \to \text{IO (TLSOutcome Unit)}$$ -/
@[extern "linen_tls_handshake_nb"]
opaque handshakeNB (session : @& TLSSession) : IO (TLSOutcome Unit)

/-- How long `handshake` waits for the socket at each step. -/
def handshakeTimeoutMillis : Nat := 30000

/-- Complete a session's handshake, waiting with `poll` whenever it has to —
    so it works on blocking and non-blocking sockets alike. Throws on a
    handshake error, or when one wait exceeds `timeoutMillis`.
    $$\text{handshake} : \text{TLSSession} \to \text{RawSocket} \to \text{IO Unit}$$ -/
def handshake (session : TLSSession) (sock : Network.Socket.RawSocket)
    (timeoutMillis : Nat := handshakeTimeoutMillis) : IO Unit := do
  let waitFor (mode : Network.Socket.PollMode) : IO Unit := do
    match ← Network.Socket.FFI.socketPoll sock mode.toUInt8 timeoutMillis.toUInt32 with
    | .ready => pure ()
    | .timeout => throw (IO.userError s!"TLS handshake timed out after {timeoutMillis}ms")
    | .error e => throw e
  repeat
    match ← handshakeNB session with
    | .ok () => return
    | .error e => throw e
    | .wantRead => waitFor .read
    | .wantWrite => waitFor .write

/-- Perform a server-side TLS handshake on a connected socket and return the
    established session (`newServerSession` then `handshake`).
    $$\text{acceptSocket} : \text{TLSContext} \to \text{RawSocket} \to \text{IO TLSSession}$$ -/
def acceptSocket (ctx : TLSContext) (sock : Network.Socket.RawSocket) : IO TLSSession := do
  let session ← newServerSession ctx sock
  handshake session sock
  return session

/-- Read up to `maxLen` bytes from the TLS session, on a **blocking**
    socket. Returns empty ByteArray on EOF or error.
    $$\text{read} : \text{TLSSession} \to \text{USize} \to \text{IO ByteArray}$$ -/
@[extern "linen_tls_read"]
opaque read (session : @& TLSSession) (maxLen : USize) : IO ByteArray

/-- Write all bytes to the TLS session, on a **blocking** socket.
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

/-- Non-blocking TLS read: `.ok` with data, `.ok` empty at end of input
    (close_notify, or the peer closing without one), or `.wantRead` /
    `.wantWrite` — a read can need to write — to retry after waiting.
    Call it *before* waiting for readability: decrypted bytes may already be
    buffered inside OpenSSL, which the socket will never signal.
    $$\text{readNB} : \text{TLSSession} \to \text{USize} \to \text{IO (TLSOutcome ByteArray)}$$ -/
@[extern "linen_tls_read_nb"]
opaque readNB (session : @& TLSSession) (maxLen : USize) : IO (TLSOutcome ByteArray)

/-- Non-blocking TLS write, all-or-nothing. On `.wantRead`/`.wantWrite`,
    wait in that direction and repeat the **same** write (same bytes), with
    no other write on the session in between (OpenSSL's retry contract).
    $$\text{writeNB} : \text{TLSSession} \to \text{ByteArray} \to \text{IO (TLSOutcome Unit)}$$ -/
@[extern "linen_tls_write_nb"]
opaque writeNB (session : @& TLSSession) (data : @& ByteArray) : IO (TLSOutcome Unit)

-- ── Reads and writes with timeouts ──

/-- Wait up to `timeoutMillis` for `sock` in `mode`; `false` on timeout. -/
private def pollFor (sock : Network.Socket.RawSocket) (mode : Network.Socket.PollMode)
    (timeoutMillis : Nat) : IO Bool := do
  match ← Network.Socket.FFI.socketPoll sock mode.toUInt8 timeoutMillis.toUInt32 with
  | .ready => return true
  | .timeout => return false
  | .error e => throw e

/-- Read up to `maxLen` decrypted bytes from a session over a **non-blocking**
    socket, waiting with `poll` in whichever direction OpenSSL asks (each wait
    at most `timeoutMillis`). `some` bytes, `some` empty at end of input,
    `none` when a wait timed out. The read is tried before any wait.
    $$\text{readWithin} : \text{TLSSession} \to \text{RawSocket} \to \mathbb{N} \to \mathbb{N} \to \text{IO (Option ByteArray)}$$ -/
def readWithin (session : TLSSession) (sock : Network.Socket.RawSocket) (timeoutMillis : Nat)
    (maxLen : Nat := 16384) : IO (Option ByteArray) := do
  repeat
    match ← readNB session maxLen.toUSize with
    | .ok bytes => return some bytes
    | .error e => throw e
    | .wantRead => unless ← pollFor sock .read timeoutMillis do return none
    | .wantWrite => unless ← pollFor sock .write timeoutMillis do return none
  return none

/-- Write all of `data` to a session over a **non-blocking** socket, repeating
    the same write after each wait; throws when a wait exceeds
    `timeoutMillis`.
    $$\text{writeWithin} : \text{TLSSession} \to \text{RawSocket} \to \mathbb{N} \to \text{ByteArray} \to \text{IO Unit}$$ -/
def writeWithin (session : TLSSession) (sock : Network.Socket.RawSocket) (timeoutMillis : Nat)
    (data : ByteArray) : IO Unit := do
  repeat
    let waited ← match ← writeNB session data with
      | .ok () => return
      | .error e => throw e
      | .wantRead => pollFor sock .read timeoutMillis
      | .wantWrite => pollFor sock .write timeoutMillis
    unless waited do throw (IO.userError s!"TLS write timed out after {timeoutMillis}ms")

-- ── Client-side TLS ──

/-- Create a TLS client context with system CA trust for server verification.
    No client certificate needed. Used for outgoing HTTPS connections.

    OpenSSL's defaults are loaded (`SSL_CERT_FILE` / `SSL_CERT_DIR` honoured),
    and, when the compiled-in default file does not exist and `SSL_CERT_FILE`
    is unset, the first readable well-known system bundle as well —
    `fallbackCaBundle` says which. That is the usual situation for the static
    OpenSSL a Lean toolchain links into an executable, whose compiled-in paths
    are the build machine's.
    $$\text{createClientContext} : \text{IO TLSContext}$$ -/
@[extern "linen_tls_client_ctx_create"]
opaque createClientContext : IO TLSContext

/-- The CA bundle `createClientContext` loads in addition to OpenSSL's
    defaults, or `""` when none is needed (`SSL_CERT_FILE` is set, or the
    compiled-in default file exists) or none of the known locations exists
    (`/etc/ssl/certs/ca-certificates.crt`, `/etc/pki/tls/certs/ca-bundle.crt`,
    `/etc/ssl/ca-bundle.pem`, `/etc/ssl/cert.pem`, in that order).
    $$\text{fallbackCaBundle} : \text{IO String}$$ -/
@[extern "linen_tls_fallback_ca_bundle"]
opaque fallbackCaBundle : IO String

/-- Create a TLS client context trusting only the CA certificate(s) at
    `caPath`, instead of the system default trust store. Useful for
    connecting to servers presenting a certificate signed by a private
    or self-signed CA (e.g. in tests).
    $$\text{createClientContextWithCA} : \text{String} \to \text{IO TLSContext}$$ -/
@[extern "linen_tls_client_ctx_create_with_ca"]
opaque createClientContextWithCA (caPath : @& String) : IO TLSContext

/-- Client TLS handshake over a connected socket, returning the session:
    `newClientSession` (SNI and certificate name `hostname`, verified against
    the context's trust store) then `handshake`, which waits with `poll` —
    so the socket may be blocking or not.
    $$\text{connectSocket} : \text{TLSContext} \to \text{RawSocket} \to \text{String} \to \text{IO TLSSession}$$ -/
def connectSocket (ctx : TLSContext) (sock : Network.Socket.RawSocket)
    (hostname : String) : IO TLSSession := do
  let session ← newClientSession ctx sock hostname
  handshake session sock
  return session

end Network.TLS
