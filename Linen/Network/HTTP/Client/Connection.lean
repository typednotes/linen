/-
  Linen.Network.HTTP.Client.Connection — Transport connection establishment

  Builds `Connection` abstractions from either plain TCP or TLS transport,
  using Linen's existing socket and TLS infrastructure.

  ## Design
  - Uses `Data.Streaming.Network.getSocketTCP` for TCP connection
  - Uses `Network.TLS.createClientContext` / `connectSocket` for TLS
  - Returns a `Connection` record with uniform read/write/close interface
  - TLS client context is created once per `connectTLS` call; for connection
    pooling (future), the context should be shared.
-/

import Linen.Network.HTTP.Client.Types
import Linen.Data.Streaming.Network
import Linen.Network.TLS.Context

namespace Network.HTTP.Client

open Network.Socket

/-- Build a `Connection` from a connected TCP socket.
    Read/write use the blocking socket wrappers, bounded by `timeoutMillis`. -/
private def connectionFromSocket (sock : Socket .connected) (timeoutMillis : Nat) : Connection :=
  { connRead := fun n => Blocking.recv sock n timeoutMillis
  , connWrite := fun data => Blocking.sendAll sock data timeoutMillis
  , connClose := do let _ ← Network.Socket.close sock; pure ()
  , connIsSecure := false }

/-- Build a `Connection` from a TLS session over a connected socket.
    The socket is closed when the connection is closed. -/
private def connectionFromTLS (session : Network.TLS.TLSSession) (sock : Socket .connected) :
    Connection :=
  { connRead := fun n => Network.TLS.read session n.toUSize
  , connWrite := fun data => Network.TLS.write session data
  , connClose := do
      Network.TLS.close session
      let _ ← Network.Socket.close sock
      pure ()
  , connIsSecure := true }

/-- How long a single socket read or write may block, in milliseconds.

    `0` means wait indefinitely — the behaviour this client had before timeouts
    existed, and almost never what a caller wants: a peer that accepts a
    connection and then stalls would hang the process forever. The default
    matches `Req.HttpConfig.httpConfigTimeout`.

    This bounds each individual read, not the whole response. A peer dribbling
    one byte per interval can still hold a connection open; bounding total
    elapsed time is a separate concern, left to the caller. -/
def defaultTimeoutMillis : Nat := 30000

/-- Apply socket-level timeouts, unless asked for none.

    The plain-TCP path is already bounded by `Blocking.recv`/`sendAll`, which
    poll with this same deadline. These `setsockopt` calls matter for the TLS
    path: OpenSSL reads the descriptor itself, so a Lean-side poll cannot bound
    it and only `SO_RCVTIMEO`/`SO_SNDTIMEO` can. -/
private def applyTimeouts (sock : Socket .connected) (millis : Nat) : IO Unit := do
  if millis != 0 then
    Network.Socket.setRecvTimeout sock millis
    Network.Socket.setSendTimeout sock millis

/-- Connect to a host:port over plain TCP.
    $$\text{connectPlain} : \text{String} \to \text{UInt16} \to \text{IO Connection}$$ -/
def connectPlain (host : String) (port : UInt16)
    (timeoutMillis : Nat := defaultTimeoutMillis) : IO Connection := do
  let (sock, _addr) ← Data.Streaming.Network.getSocketTCP host port
  applyTimeouts sock timeoutMillis
  return connectionFromSocket sock timeoutMillis

/-- Connect to a host:port over TLS (HTTPS).
    Creates a fresh TLS client context, performs TCP connect, then TLS handshake.
    Server certificate is verified against system CA trust store with SNI.
    $$\text{connectTLS} : \text{String} \to \text{UInt16} \to \text{IO Connection}$$ -/
def connectTLS (host : String) (port : UInt16)
    (timeoutMillis : Nat := defaultTimeoutMillis) : IO Connection := do
  let (sock, _addr) ← Data.Streaming.Network.getSocketTCP host port
  -- Set before the handshake, so a peer that stalls mid-handshake is bounded
  -- too, not only one that stalls afterwards.
  applyTimeouts sock timeoutMillis
  let ctx ← Network.TLS.createClientContext
  let session ← Network.TLS.connectSocket ctx sock.raw host
  return connectionFromTLS session sock

/-- Connect to a host:port, choosing plain TCP or TLS based on the `secure` flag.
    $$\text{connect} : \text{String} \to \text{UInt16} \to \text{Bool} \to \text{IO Connection}$$ -/
def connect (host : String) (port : UInt16) (secure : Bool)
    (timeoutMillis : Nat := defaultTimeoutMillis) : IO Connection :=
  if secure then connectTLS host port timeoutMillis
  else connectPlain host port timeoutMillis

/-- Default port for the given security mode: 443 for HTTPS, 80 for HTTP. -/
def defaultPort (secure : Bool) : UInt16 :=
  if secure then 443 else 80

end Network.HTTP.Client
