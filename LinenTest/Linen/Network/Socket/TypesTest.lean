/-
  Tests for `Linen.Network.Socket.Types`.

  These are pure type/encoding definitions, so behaviour is asserted with
  `#guard` (a failing check fails the build). The phantom-typed `Socket state`
  protocol is checked at compile time by the module's own theorems; here we
  also illustrate a couple of the state-distinctness facts with `example`.
-/
import Linen.Network.Socket.Types

open Network.Socket

namespace Tests.Network.Socket.Types

/-! ### Family ↔ UInt8 (FFI tag encoding, with roundtrip) -/

#guard Family.inet.toUInt8 == 0
#guard Family.inet6.toUInt8 == 1
#guard Family.unixDomain.toUInt8 == 2
#guard Family.ofUInt8 0 == .inet
#guard Family.ofUInt8 1 == .inet6
#guard Family.ofUInt8 2 == .unixDomain
#guard Family.ofUInt8 99 == .inet                                  -- out-of-range → inet
#guard Family.ofUInt8 Family.inet.toUInt8 == .inet
#guard Family.ofUInt8 Family.inet6.toUInt8 == .inet6
#guard Family.ofUInt8 Family.unixDomain.toUInt8 == .unixDomain

/-! ### SocketType / ShutdownHow / PollMode encodings -/

#guard SocketType.stream.toUInt8 == 0
#guard SocketType.datagram.toUInt8 == 1
#guard SocketType.raw.toUInt8 == 2
#guard ShutdownHow.read.toUInt8 == 0
#guard ShutdownHow.write.toUInt8 == 1
#guard ShutdownHow.both.toUInt8 == 2
#guard PollMode.read.toUInt8 == 0
#guard PollMode.write.toUInt8 == 1
#guard PollMode.both.toUInt8 == 2

/-! ### EventType bitmask -/

#guard EventType.readable.flags == 1
#guard EventType.writable.flags == 2
#guard EventType.error.flags == 4
#guard EventType.readable.hasReadable
#guard !EventType.readable.hasWritable
#guard !EventType.readable.hasError
#guard (EventType.readable ||| EventType.writable).hasReadable
#guard (EventType.readable ||| EventType.writable).hasWritable
#guard !(EventType.readable ||| EventType.writable).hasError
#guard (EventType.merge EventType.readable EventType.error).hasError
#guard (EventType.readable ||| EventType.writable).flags == 3      -- 1 ||| 2

/-! ### SockAddr rendering -/

#guard toString (SockAddr.mk "localhost" 8080) == "localhost:8080"
#guard toString (SockAddr.mk "127.0.0.1" 443) == "127.0.0.1:443"

/-! ### SocketState equality / distinctness -/

#guard (SocketState.fresh == SocketState.fresh)
#guard !(SocketState.fresh == SocketState.bound)
#guard (SocketState.connected == SocketState.connected)

-- The compile-time protocol facts are theorems in the module; illustrate two:
example : SocketState.fresh ≠ SocketState.closed := SocketState.fresh_ne_closed
example : (SocketState.listening == SocketState.listening) = true := SocketState.beq_refl _

end Tests.Network.Socket.Types
