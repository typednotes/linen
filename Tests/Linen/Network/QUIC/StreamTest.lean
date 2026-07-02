/-
  Tests for `Linen.Network.QUIC.Stream`.

  `QUICStream` embeds a `Connection`, whose constructor is private to
  `Connection.lean` — so, as in `ConnectionTest.lean`, no `QUICStream` value
  is constructible here. Each operation's delegation to the underlying
  `Connection` method is still a fact about a universally-quantified
  `QUICStream`, pinned with a compile-time `example ... := rfl`.
-/
import Linen.Network.QUIC.Stream

open Network.QUIC

namespace Tests.Network.QUIC.Stream

example (s : QUICStream) (data : ByteArray) (fin : Bool) :
    s.send data fin = s.conn.sendStream s.streamId data fin :=
  rfl

example (s : QUICStream) (data : ByteArray) :
    s.send data = s.conn.sendStream s.streamId data false :=
  rfl

example (s : QUICStream) (maxLen : Nat) :
    s.recv maxLen = s.conn.recvStream s.streamId maxLen :=
  rfl

example (s : QUICStream) (appError : UInt64) :
    s.close appError = s.conn.closeStream s.streamId appError :=
  rfl

end Tests.Network.QUIC.Stream
