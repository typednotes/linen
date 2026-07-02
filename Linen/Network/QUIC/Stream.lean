/-
  Linen.Network.QUIC.Stream -- QUIC stream abstraction

  A high-level wrapper around a QUIC connection + stream ID pair. Ports
  Haskell's `Network.QUIC` (streams) from the `quic` package.

  ## Design

  `QUICStream` bundles a `Connection` with a `StreamId` so that callers
  do not need to thread the stream ID through every send/recv call.
  All operations delegate to `Connection.sendStream` / `Connection.recvStream`.

  ## Guarantees

  - `QUICStream` is a pure data wrapper; no hidden state
  - All IO operations delegate to the underlying `Connection`
-/

import Linen.Network.QUIC.Connection

namespace Network.QUIC

/-- A QUIC stream, combining a connection handle and a stream identifier.
    $$\text{QUICStream} = \text{Connection} \times \text{StreamId}$$ -/
structure QUICStream where
  /-- The underlying QUIC connection. -/
  conn : Connection
  /-- The stream identifier within the connection. -/
  streamId : StreamId
  deriving Repr

namespace QUICStream

/-- Send data on this stream.
    $$\text{send} : \text{QUICStream} \to \text{ByteArray} \to \text{Bool} \to \text{IO}(\text{Unit})$$
    If `fin` is true, this is the last data on the stream. -/
def send (s : QUICStream) (data : ByteArray) (fin : Bool := false) : IO Unit :=
  s.conn.sendStream s.streamId data fin

/-- Receive data from this stream.
    $$\text{recv} : \text{QUICStream} \to \mathbb{N} \to \text{IO}(\text{ByteArray} \times \text{Bool})$$
    Returns the received data and whether the stream has ended (FIN received). -/
def recv (s : QUICStream) (maxLen : Nat := 65536) : IO (ByteArray × Bool) :=
  s.conn.recvStream s.streamId maxLen

/-- Close this stream with an optional application error code.
    $$\text{close} : \text{QUICStream} \to \text{UInt64} \to \text{IO}(\text{Unit})$$ -/
def close (s : QUICStream) (appError : UInt64 := 0) : IO Unit :=
  s.conn.closeStream s.streamId appError

end QUICStream

end Network.QUIC
