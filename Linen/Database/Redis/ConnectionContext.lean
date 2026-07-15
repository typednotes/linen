/-
  Linen.Database.Redis.ConnectionContext — the raw connection handle

  Ported from `hedis`'s `Database.Redis.ConnectionContext`
  (https://hackage.haskell.org/package/hedis-0.16.1/src/src/Database/Redis/ConnectionContext.hs).

  Upstream wraps a GHC `Handle` (either a plain TCP/Unix-domain-socket handle,
  or one wrapped in a `tls` `TLS.Context`) and exposes `connect`/`disconnect`/
  `send`/`recv`/`flush`/`enableTLS`.

  ## Substitutions

  - GHC's `Network.Socket.socketToHandle`-based buffered `Handle` I/O is
    replaced by `Linen.Network.Socket.Blocking`'s blocking wrappers
    (`connect`, `sendAll`, `recv`) over the existing socket FFI.
  - The `tls` package's `TLS.Context` is replaced by `Linen.Network.TLS`'s
    `TLSContext`/`TLSSession` (OpenSSL-backed FFI), via `connectSocket`
    (a blocking, WANT_READ/WANT_WRITE-polling handshake).
  - `flush`: GHC's `Handle` is block-buffered and needs an explicit `hFlush`;
    both of our backends (`Network.Socket.Blocking.sendAll`,
    `Network.TLS.write`) write eagerly, so `flush` is a no-op that
    only exists to keep the public API shape faithful to upstream.

  ## Deviation: Unix-domain sockets

  Upstream's `ConnectAddrUnixSocket` connects via `Network.Socket`'s
  `AF_UNIX` support. The existing FFI shim (`ffi/network.c`'s
  `resolve_addr`) only implements `getaddrinfo`-based TCP/UDP resolution —
  there is no `sockaddr_un` construction anywhere in the FFI layer, despite
  `Network.Socket.Types.Family.unixDomain` existing as an enum value. Rather
  than adding new C FFI work (out of scope for this port) or silently
  misbehaving, `connect (.unixSocket _)` throws a clear, explicit
  "not yet supported" error.
-/
import Linen.Network.Socket
import Linen.Network.Socket.Blocking
import Linen.Network.TLS.Context

namespace Database.Redis.ConnectionContext

open Network.Socket (Socket SocketState)

/-- Where to connect: a resolvable host/port pair, or a Unix-domain socket
    path (see the module doc-comment for the latter's current limitation).
    Mirrors upstream's `ConnectAddr`. -/
inductive ConnectAddr where
  | hostPort (host : String) (port : UInt16)
  | unixSocket (path : String)
  deriving Repr

/-- Thrown when a connection attempt exceeds its timeout.
    Mirrors upstream's `ConnectTimeout`. -/
def connectTimeoutError : IO.Error :=
  IO.userError "Redis.ConnectionContext: connection attempt timed out"

/-- Thrown by `recv`/`send` when the underlying connection has already been
    disconnected. Mirrors upstream's `ConnectionLostException`. -/
def connectionLostError : IO.Error :=
  IO.userError "Redis.ConnectionContext: connection lost"

/-- Thrown when `connect (.unixSocket _)` is called (see the module
    doc-comment: no `AF_UNIX`/`sockaddr_un` support in the FFI layer yet). -/
def unixSocketUnsupportedError : IO.Error :=
  IO.userError
    "Redis.ConnectionContext: Unix-domain-socket connections are not yet \
     supported by the underlying socket FFI (no sockaddr_un construction \
     in ffi/network.c's resolve_addr); use ConnectAddr.hostPort instead"

/-- A raw connection handle: either a plain socket, or a socket wrapped in a
    TLS session. Mirrors upstream's `ConnectionContext = NormalHandle Handle
    | TLSContext TLS.Context Handle`.

    Unlike upstream, which keeps the `Handle` around alongside the `TLS.Context`
    only to close the underlying fd on disconnect, our TLS session retains the
    connected `Socket .connected` value directly (`Network.TLS.Context` reads
    the OS fd off of it via `RawSocket`, and we need it again at `close` time). -/
inductive ConnectionContext where
  | normal (sock : Socket .connected)
  | tls (session : Network.TLS.TLSSession) (sock : Socket .connected)

/-- Resolve a `ConnectAddr.hostPort` to a `SockAddr` and connect, blocking
    until the handshake completes or fails. -/
private def connectPlain (host : String) (port : UInt16) : IO (Socket .connected) := do
  let s ← Network.Socket.socket .inet .stream
  Network.Socket.Blocking.connect s { host, port }

/-- Connect a plain (non-TLS) connection to a Redis server at the given
    address. Mirrors the `Nothing`-TLS-settings case of upstream's `connect`.

    (Note: a `useTLS : Option Network.TLS.TLSContext` parameter was tried
    first, matching upstream's `Maybe TLSSettings` more literally, but
    `Network.TLS.TLSContext` is universe-polymorphic — its underlying
    `opaque ... : NonemptyType` was declared without pinning a universe — so
    wrapping it in `Option` behind a default argument leaves an unresolved
    universe metavariable at call sites using `#eval`/`IO.run`. Splitting into
    `connect`/`connectTLS`, both taking fully concrete arguments, sidesteps
    the issue entirely; this is also the pattern already used by
    `Linen.Network.HTTP.Client.Connection`'s `connectPlain`/`connectTLS`.) -/
def connect (addr : ConnectAddr) : IO ConnectionContext := do
  match addr with
  | .unixSocket _ => throw unixSocketUnsupportedError
  | .hostPort host port =>
    let sock ← connectPlain host port
    pure (.normal sock)

/-- Connect over TLS to a Redis server at the given address, performing a
    blocking handshake using `ctx` (typically
    `Network.TLS.createClientContext` or `createClientContextWithCA`), with
    the address's host used as the TLS server-name / hostname-verification
    target. Mirrors the `Just tlsSettings` case of upstream's `connect`. -/
def connectTLS (addr : ConnectAddr) (ctx : Network.TLS.TLSContext) : IO ConnectionContext := do
  match addr with
  | .unixSocket _ => throw unixSocketUnsupportedError
  | .hostPort host port =>
    let sock ← connectPlain host port
    let session ← Network.TLS.connectSocket ctx sock.raw host
    pure (.tls session sock)

/-- Upgrade an already-open plain connection to TLS in place, performing a
    blocking handshake over the existing socket. Mirrors upstream's
    `enableTLS`. Fails (via `Except.error`, not an exception) if the context
    is already a TLS session, since re-wrapping would leak the prior
    `TLSSession`. -/
def enableTLS (ctx : Network.TLS.TLSContext) (hostname : String) :
    ConnectionContext → IO (Except String ConnectionContext)
  | .tls _ _ => pure (Except.error "Redis.ConnectionContext: already using TLS")
  | .normal sock => do
    let session ← Network.TLS.connectSocket ctx sock.raw hostname
    pure (Except.ok (.tls session sock))

/-- Send bytes over the connection, blocking until fully written. Mirrors
    upstream's `send`. -/
def send : ConnectionContext → ByteArray → IO Unit
  | .normal sock, bytes => Network.Socket.Blocking.sendAll sock bytes
  | .tls session _, bytes => Network.TLS.write session bytes

/-- Receive up to `maxlen` bytes, blocking until some data (or EOF) arrives.
    An empty result means the peer closed the connection. Mirrors upstream's
    `recv`. -/
def recv (cc : ConnectionContext) (maxlen : Nat := 4096) : IO ByteArray :=
  match cc with
  | .normal sock => Network.Socket.Blocking.recv sock maxlen
  | .tls session _ => Network.TLS.read session maxlen.toUSize

/-- Flush any buffered output. Both backends write eagerly (see the module
    doc-comment), so this is a no-op kept only for API-shape fidelity with
    upstream's `flush`. -/
def flush (_cc : ConnectionContext) : IO Unit :=
  pure ()

/-- Close the underlying connection. Mirrors upstream's `disconnect`
    (upstream calls `hClose`; here we close the TLS session, if any, then
    the socket). -/
def disconnect : ConnectionContext → IO Unit
  | .normal sock => do let _ ← Network.Socket.close sock; pure ()
  | .tls session sock => do
    Network.TLS.close session
    let _ ← Network.Socket.close sock
    pure ()

end Database.Redis.ConnectionContext
