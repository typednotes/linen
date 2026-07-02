/-
  Tests for `Linen.Network.QUIC.Connection`.

  `Connection`'s constructor is private to its defining module — by design,
  only `Server`/`Client` (once real FFI backs them) can mint one — so no
  `Connection` value is constructible here, and the stub operations cannot be
  *run*. What every operation's stub body is is still a fact about a
  universally-quantified `conn`/argument tuple, so we pin each one with a
  compile-time `example ... := rfl`: if a stub's message or behaviour ever
  changes, these proofs stop compiling.

  `ConnectionState`, by contrast, is ordinary data and is checked with `#guard`.
-/
import Linen.Network.QUIC.Connection

open Network.QUIC

namespace Tests.Network.QUIC.Connection

/-! ### ConnectionState -/

#guard ConnectionState.handshaking == ConnectionState.handshaking
#guard (ConnectionState.handshaking == ConnectionState.established) == false
#guard (ConnectionState.closing == ConnectionState.closed) == false

/-! ### Stub laws (compile-time) -/

example (conn : Connection) (streamId : StreamId) (data : ByteArray) (fin : Bool) :
    conn.sendStream streamId data fin =
      throw (IO.userError "QUIC: sendStream not yet implemented (requires TLS 1.3 FFI)") :=
  rfl

example (conn : Connection) (streamId : StreamId) (maxLen : Nat) :
    conn.recvStream streamId maxLen =
      throw (IO.userError "QUIC: recvStream not yet implemented (requires TLS 1.3 FFI)") :=
  rfl

example (conn : Connection) (bidi : Bool) :
    conn.openStream bidi =
      throw (IO.userError "QUIC: openStream not yet implemented (requires TLS 1.3 FFI)") :=
  rfl

example (conn : Connection) (streamId : StreamId) (appError : UInt64) :
    conn.closeStream streamId appError =
      throw (IO.userError "QUIC: closeStream not yet implemented (requires TLS 1.3 FFI)") :=
  rfl

example (conn : Connection) : conn.getState =
    throw (IO.userError "QUIC: getState not yet implemented (requires TLS 1.3 FFI)") :=
  rfl

example (conn : Connection) (err : TransportError) :
    conn.close err = throw (IO.userError "QUIC: close not yet implemented (requires TLS 1.3 FFI)") :=
  rfl

end Tests.Network.QUIC.Connection
