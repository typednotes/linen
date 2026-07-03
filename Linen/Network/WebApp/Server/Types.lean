/-
  Linen.Network.WebApp.Server.Types — Core Server types

  Foundational types for the HTTP server. The key abstraction is `Connection`,
  which provides transport-agnostic I/O operations (TCP, TLS, QUIC all implement
  the same interface).

  Ports Hale's `Network.Wai.Handler.Warp.Types`, renamed from the
  Haskell-specific `Warp` to `Server` per this project's naming convention
  (matching `Network.HTTP2.Server`/`Network.HTTP3.Server`/`Network.QUIC.Server`).

  ## Design

  The `Connection` record decouples transport (TCP/TLS/QUIC) from request
  handling, enabling the TLS and QUIC variants to plug in by providing a
  different `Connection`.

  ## Guarantees

  - `Transport` encodes transport security as a provable property
  - `InvalidRequest` covers all protocol-level error cases
  - `Connection.connClose` is documented as single-use (axiom-dependent)
  - `Source` provides buffered reading with leftover support
-/
import Linen.Network.Socket.Types
import Linen.System.TimeManager

namespace Network.WebApp.Server

open Network.Socket (SockAddr)

-- ════════════════════════════════════════════════════════════════════
-- Basic type aliases
-- ════════════════════════════════════════════════════════════════════

/-- TCP port number. -/
abbrev Port := UInt16

/-- Header value type. -/
abbrev HeaderValue := String

-- ════════════════════════════════════════════════════════════════════
-- Error types
-- ════════════════════════════════════════════════════════════════════

/-- Error types for bad HTTP requests.
    Each constructor represents a specific protocol violation or resource limit.
    $$\text{InvalidRequest} = \text{NotEnoughLines} \mid \text{BadFirstLine} \mid \ldots$$ -/
inductive InvalidRequest where
  | notEnoughLines (received : List String)
  | badFirstLine (line : String)
  | nonHttp
  | incompleteHeaders
  | connectionClosedByPeer
  | overLargeHeader
  | badProxyHeader (details : String)
  | payloadTooLarge
  | requestHeaderFieldsTooLarge
deriving BEq, Repr

instance : ToString InvalidRequest where
  toString
    | .notEnoughLines xs =>
      s!"Server: Incomplete request headers, received: {xs}"
    | .badFirstLine s =>
      s!"Server: Invalid first line of request: {s}"
    | .nonHttp =>
      "Server: Request line specified a non-HTTP request"
    | .incompleteHeaders =>
      "Server: Request headers did not finish transmission"
    | .connectionClosedByPeer =>
      "Server: Client closed connection prematurely"
    | .overLargeHeader =>
      "Server: Request headers too large, possible memory attack detected. Closing connection."
    | .badProxyHeader s =>
      s!"Server: Invalid PROXY protocol header: {s}"
    | .payloadTooLarge =>
      "Payload too large"
    | .requestHeaderFieldsTooLarge =>
      "Request header fields too large"

-- ════════════════════════════════════════════════════════════════════
-- Write buffer
-- ════════════════════════════════════════════════════════════════════

/-- A write buffer of a specified size with a finalizer.
    $$\text{WriteBuffer} = \{ \text{buffer} : \text{ByteArray},\; \text{capacity} : \mathbb{N} \}$$ -/
structure WriteBuffer where
  /-- The buffer contents. -/
  buffer : ByteArray
  /-- The buffer capacity. -/
  capacity : Nat
  /-- Free the allocated buffer. The server guarantees single invocation. -/
  free : IO Unit

-- ════════════════════════════════════════════════════════════════════
-- Connection — the critical transport abstraction
-- ════════════════════════════════════════════════════════════════════

/-- Data type to manipulate IO actions for connections.
    This abstracts IO actions for plain HTTP, HTTP over TLS, and HTTP/3 over QUIC.
    Each transport (TCP, TLS, QUIC) provides its own `Connection` with
    transport-specific read/write implementations.

    $$\text{Connection} = \{ \text{sendAll}, \text{recv}, \text{close}, \ldots \}$$ -/
structure Connection where
  /-- Send multiple byte chunks. -/
  connSendMany : List ByteArray → IO Unit
  /-- Send a single byte chunk. The primary sending function. -/
  connSendAll : ByteArray → IO Unit
  /-- Send a file: path → offset → length → hook → headers → IO ().
      Uses sendfile(2) on supported platforms. -/
  connSendFile : String → Nat → Nat → IO Unit → List ByteArray → IO Unit
  /-- Close the connection. The server guarantees single invocation.
      Other functions may be called after close (they should return empty/error). -/
  connClose : IO Unit
  /-- Receive bytes. Returns empty ByteArray for EOF or errors. -/
  connRecv : IO ByteArray
  /-- Reference to the write buffer. May be replaced with a larger buffer
      during Builder response sending. -/
  connWriteBuffer : IO.Ref (Option WriteBuffer)
  /-- Is this connection using HTTP/2? -/
  connHTTP2 : IO.Ref Bool
  /-- The server's own socket address. -/
  connMySockAddr : SockAddr

/-- Check if the connection is using HTTP/2. -/
def Connection.getHTTP2 (conn : Connection) : IO Bool :=
  conn.connHTTP2.get

/-- Set the HTTP/2 flag on the connection. -/
def Connection.setHTTP2 (conn : Connection) (v : Bool) : IO Unit :=
  conn.connHTTP2.set v

-- ════════════════════════════════════════════════════════════════════
-- Internal server info
-- ════════════════════════════════════════════════════════════════════

/-- Internal server state passed to connection handlers.
    Contains cached resources shared across all connections.
    $$\text{InternalInfo} = \{ \text{timeoutManager}, \text{getDate}, \ldots \}$$ -/
structure InternalInfo where
  /-- The timeout manager for connection deadlines. -/
  timeoutManager : System.TimeManager.Manager
  /-- Get the current cached HTTP date string (updated once/second). -/
  getDate : IO String

-- ════════════════════════════════════════════════════════════════════
-- Source — input streaming with leftover support
-- ════════════════════════════════════════════════════════════════════

/-- Input source for streaming byte reads with leftover buffering.
    The leftover ref stores bytes read ahead (e.g., during header parsing)
    that belong to the request body.
    $$\text{Source} = \text{IO.Ref ByteArray} \times (\text{IO ByteArray})$$ -/
structure Source where
  /-- Leftover bytes from previous reads. -/
  leftoverRef : IO.Ref ByteArray
  /-- The underlying read function. -/
  readFunc : IO ByteArray

/-- Create a new Source from a read function. -/
def Source.mk' (readFunc : IO ByteArray) : IO Source := do
  let ref ← IO.mkRef ByteArray.empty
  return ⟨ref, readFunc⟩

/-- Read from the source. Returns leftover bytes first, then reads from
    the underlying function. Returns empty ByteArray on EOF. -/
def Source.read (src : Source) : IO ByteArray := do
  let leftover ← src.leftoverRef.get
  if leftover.isEmpty then
    src.readFunc
  else
    src.leftoverRef.set ByteArray.empty
    return leftover

/-- Push bytes back into the source as leftover.
    These will be returned by the next `read` call. -/
def Source.leftover (src : Source) (bs : ByteArray) : IO Unit :=
  src.leftoverRef.set bs

/-- Read leftover bytes without consuming them from the underlying source. -/
def Source.readLeftover (src : Source) : IO ByteArray :=
  src.leftoverRef.get

-- ════════════════════════════════════════════════════════════════════
-- Transport — what kind of connection is this?
-- ════════════════════════════════════════════════════════════════════

/-- What kind of transport is used for this connection?
    $$\text{Transport} = \text{TCP} \mid \text{TLS}(\ldots) \mid \text{QUIC}(\ldots)$$ -/
inductive Transport where
  /-- Plain TCP channel. -/
  | tcp
  /-- Encrypted TLS channel. -/
  | tls
      (majorVersion : Nat)
      (minorVersion : Nat)
      (negotiatedProtocol : Option String)
      (cipherID : UInt16)
  /-- QUIC transport (always encrypted). -/
  | quic
      (negotiatedProtocol : Option String)
      (cipherID : UInt16)
deriving BEq, Repr

/-- Is this transport secure (TLS or QUIC)?
    $$\text{isTransportSecure}(\text{TCP}) = \text{false}$$
    $$\text{isTransportSecure}(\text{TLS}\ldots) = \text{true}$$
    $$\text{isTransportSecure}(\text{QUIC}\ldots) = \text{true}$$ -/
def Transport.isSecure : Transport → Bool
  | .tcp => false
  | .tls .. => true
  | .quic .. => true

/-- Is this a QUIC transport? -/
def Transport.isQUIC : Transport → Bool
  | .quic .. => true
  | _ => false

-- Transport security proofs

/-- TCP connections are not secure by default.
    $$\forall\; t = \text{TCP},\; \neg\,\text{isSecure}(t)$$ -/
theorem tcp_not_secure : Transport.isSecure .tcp = false := rfl

/-- TLS connections are always secure.
    $$\forall\; v_1\, v_2\, p\, c,\; \text{isSecure}(\text{TLS}(v_1, v_2, p, c)) = \text{true}$$ -/
theorem tls_is_secure (v1 v2 : Nat) (p : Option String) (c : UInt16) :
    Transport.isSecure (.tls v1 v2 p c) = true := rfl

/-- QUIC connections are always secure.
    $$\forall\; p\, c,\; \text{isSecure}(\text{QUIC}(p, c)) = \text{true}$$ -/
theorem quic_is_secure (p : Option String) (c : UInt16) :
    Transport.isSecure (.quic p c) = true := rfl

end Network.WebApp.Server
