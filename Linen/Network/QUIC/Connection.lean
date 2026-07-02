/-
  Linen.Network.QUIC.Connection -- QUIC connection management

  Opaque handle for a QUIC connection with operations for stream management,
  data transfer, and connection lifecycle. Ports Haskell's `Network.QUIC`
  (connection) from the `quic` package.

  ## Design

  `Connection` is an opaque handle wrapping an internal ID. The actual QUIC
  state machine would be managed by a C FFI library (e.g., quiche or ngtcp2).
  All operations are currently stubbed with `IO.userError` since a real
  implementation requires TLS 1.3 FFI.

  ## Guarantees

  - `Connection` construction is private (only the server/client module creates them)
  - All operations are `IO`-based, reflecting the stateful, effectful nature of QUIC
-/

import Linen.Network.QUIC.Types

namespace Network.QUIC

/-- Opaque handle for a QUIC connection. Construction is private;
    connections are created by `Server.run`/`Client.connect`.
    $$\text{Connection} = \{ \text{id} : \mathbb{N} \}$$ -/
structure Connection where
  private mk ::
  /-- Internal handle identifier (opaque to callers). -/
  id : Nat
  deriving Repr, BEq

/-- Connection lifecycle state. -/
inductive ConnectionState where
  /-- TLS handshake in progress. -/
  | handshaking
  /-- Handshake complete, data transfer possible. -/
  | established
  /-- Connection is being gracefully shut down. -/
  | closing
  /-- Connection is fully closed. -/
  | closed
  deriving BEq, Repr

namespace Connection

-- NOTE: A real implementation would use FFI to quiche (https://github.com/cloudflare/quiche)
-- or ngtcp2 (https://github.com/ngtcp2/ngtcp2) for the QUIC protocol engine and TLS 1.3.
-- These stubs define the API surface that downstream packages (HTTP/3, warp-quic) program against.

/-- Send data on a specific QUIC stream.
    $$\text{sendStream} : \text{Connection} \to \text{StreamId} \to \text{ByteArray} \to \text{Bool} \to \text{IO}(\text{Unit})$$
    The `fin` flag indicates this is the last data on the stream. -/
def sendStream (_conn : Connection) (_streamId : StreamId) (_data : ByteArray) (_fin : Bool) : IO Unit :=
  throw (IO.userError "QUIC: sendStream not yet implemented (requires TLS 1.3 FFI)")

/-- Receive data from a specific QUIC stream.
    $$\text{recvStream} : \text{Connection} \to \text{StreamId} \to \mathbb{N} \to \text{IO}(\text{ByteArray} \times \text{Bool})$$
    Returns the received data and a flag indicating whether the stream has ended. -/
def recvStream (_conn : Connection) (_streamId : StreamId) (_maxLen : Nat) : IO (ByteArray × Bool) :=
  throw (IO.userError "QUIC: recvStream not yet implemented (requires TLS 1.3 FFI)")

/-- Open a new QUIC stream on the connection.
    $$\text{openStream} : \text{Connection} \to \text{Bool} \to \text{IO}(\text{StreamId})$$
    If `bidi` is true, opens a bidirectional stream; otherwise, opens a unidirectional stream. -/
def openStream (_conn : Connection) (_bidi : Bool) : IO StreamId :=
  throw (IO.userError "QUIC: openStream not yet implemented (requires TLS 1.3 FFI)")

/-- Close a specific QUIC stream with an optional application error code.
    $$\text{closeStream} : \text{Connection} \to \text{StreamId} \to \text{UInt64} \to \text{IO}(\text{Unit})$$ -/
def closeStream (_conn : Connection) (_streamId : StreamId) (_appError : UInt64 := 0) : IO Unit :=
  throw (IO.userError "QUIC: closeStream not yet implemented (requires TLS 1.3 FFI)")

/-- Get the current connection state.
    $$\text{getState} : \text{Connection} \to \text{IO}(\text{ConnectionState})$$ -/
def getState (_conn : Connection) : IO ConnectionState :=
  throw (IO.userError "QUIC: getState not yet implemented (requires TLS 1.3 FFI)")

/-- Close the QUIC connection with an optional transport error.
    $$\text{close} : \text{Connection} \to \text{TransportError} \to \text{IO}(\text{Unit})$$ -/
def close (_conn : Connection) (_error : TransportError := .noError) : IO Unit :=
  throw (IO.userError "QUIC: close not yet implemented (requires TLS 1.3 FFI)")

end Connection

end Network.QUIC
