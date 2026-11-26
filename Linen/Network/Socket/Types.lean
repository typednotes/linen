/-
  Linen.Network.Socket.Types — Socket type definitions

  Core types for the network socket abstraction. Ports Haskell's
  `Network.Socket.Types` (from the `network` package), reshaped to Lean's
  module hierarchy and built on the standard library (`IO.Error`, `ByteArray`,
  `USize`, …).

  ## Socket state indices

  `Socket (state : SocketState)` tags a socket value with the state recorded
  when it was returned. Signatures use this index to reject a mismatched
  tag, such as passing `Socket .fresh` to `send`. The index is erased at
  runtime. Earlier aliases share the underlying socket and keep their
  original tags when a transition returns a new value.

  ## Non-blocking I/O and External State Changes

  The non-blocking I/O operations return **outcome
  sum types** (`AcceptOutcome`, `RecvOutcome`, `SendOutcome`, `ConnectOutcome`)
  that represent success, `wouldBlock` (EAGAIN), EOF (peer closed), and
  errors. This models the fact that OS-level
  state can change asynchronously (peer disconnect, non-blocking connect in
  progress, send buffer full).

  The `.connecting` state represents a non-blocking `connect()` that returned
  `EINPROGRESS` -- the TCP handshake is in flight but not yet resolved.

  ## Types

  - `Family`: AF_INET, AF_INET6, AF_UNIX
  - `SocketType`: SOCK_STREAM, SOCK_DGRAM, SOCK_RAW
  - `ShutdownHow`: SHUT_RD, SHUT_WR, SHUT_RDWR
  - `EventType`: readable, writable, error flags
  - `EventLoop`: opaque event multiplexing handle (kqueue/epoll)
  - `SockAddr`: host + port
  - `Socket`: opaque socket handle parameterised by `SocketState`
  - `AcceptOutcome`, `ConnectOutcome`, `RecvOutcome`, `SendOutcome`: outcome sum types

  ## Design

  Socket and EventLoop are opaque types backed by POSIX file descriptors
  managed via `lean_alloc_external` with automatic cleanup on GC.
  This follows the same pattern as Lean's `IO.FS.Handle`.
-/

namespace Network.Socket

/-- Address family.
    $$\text{Family} = \text{inet} \mid \text{inet6} \mid \text{unixDomain}$$ -/
inductive Family where
  | inet : Family        -- AF_INET (IPv4)
  | inet6 : Family       -- AF_INET6 (IPv6)
  | unixDomain : Family  -- AF_UNIX
deriving BEq, Repr

/-- Encode a Family to the UInt8 tag expected by the C FFI.
    $$\text{Family.toUInt8} : \text{Family} \to \text{UInt8}$$
    - 0 = AF_INET, 1 = AF_INET6, 2 = AF_UNIX -/
def Family.toUInt8 : Family → UInt8
  | .inet => 0
  | .inet6 => 1
  | .unixDomain => 2

/-- Decode a UInt8 tag from the C FFI to a Family. -/
def Family.ofUInt8 : UInt8 → Family
  | 0 => .inet
  | 1 => .inet6
  | 2 => .unixDomain
  | _ => .inet

/-- Socket type.
    $$\text{SocketType} = \text{stream} \mid \text{datagram} \mid \text{raw}$$ -/
inductive SocketType where
  | stream : SocketType    -- SOCK_STREAM (TCP)
  | datagram : SocketType  -- SOCK_DGRAM (UDP)
  | raw : SocketType       -- SOCK_RAW
deriving BEq, Repr

/-- Encode a SocketType to the UInt8 tag expected by the C FFI.
    $$\text{SocketType.toUInt8} : \text{SocketType} \to \text{UInt8}$$
    - 0 = SOCK_STREAM, 1 = SOCK_DGRAM, 2 = SOCK_RAW -/
def SocketType.toUInt8 : SocketType → UInt8
  | .stream => 0
  | .datagram => 1
  | .raw => 2

/-- How to shut down a socket.
    $$\text{ShutdownHow} = \text{read} \mid \text{write} \mid \text{both}$$ -/
inductive ShutdownHow where
  | read : ShutdownHow   -- SHUT_RD
  | write : ShutdownHow  -- SHUT_WR
  | both : ShutdownHow   -- SHUT_RDWR
deriving BEq, Repr

/-- Encode ShutdownHow to the UInt8 expected by the C FFI.
    - 0 = SHUT_RD, 1 = SHUT_WR, 2 = SHUT_RDWR -/
def ShutdownHow.toUInt8 : ShutdownHow → UInt8
  | .read => 0
  | .write => 1
  | .both => 2

/-- Event type flags for event multiplexing.
    $$\text{EventType} = \{ \text{flags} : \text{USize} \}$$

    Bitmask:
    - bit 0 (1) = readable
    - bit 1 (2) = writable
    - bit 2 (4) = error / hangup -/
structure EventType where
  flags : USize
deriving BEq, Repr

namespace EventType

/-- Readable event flag (bit 0). -/
def readable : EventType := ⟨1⟩

/-- Writable event flag (bit 1). -/
def writable : EventType := ⟨2⟩

/-- Error/hangup event flag (bit 2). -/
def error : EventType := ⟨4⟩

/-- Registration flag (bit 3), not an event: arm the registration for **one**
    notification (`EV_ONESHOT` / `EPOLLONESHOT`), after which it must be
    re-added. Without it registrations are level-triggered and stay armed, so
    a socket that stays writable (almost always) or readable reports itself
    on every wait. With `epoll`, one-shot re-arming replaces the fd's whole
    mask, so pass every direction still wanted. -/
def oneshot : EventType := ⟨8⟩

/-- Combine event flags. -/
def merge (a b : EventType) : EventType := ⟨a.flags ||| b.flags⟩

instance : OrOp EventType where
  or := merge

/-- Test if a specific flag is set. -/
def hasReadable (e : EventType) : Bool := (e.flags &&& 1) != 0
def hasWritable (e : EventType) : Bool := (e.flags &&& 2) != 0
def hasError (e : EventType) : Bool := (e.flags &&& 4) != 0
def hasOneshot (e : EventType) : Bool := (e.flags &&& 8) != 0

end EventType

/-- Opaque event loop handle (kqueue on macOS, epoll on Linux).
    Backed by a POSIX file descriptor managed via `lean_alloc_external`
    with automatic cleanup on GC.

    Following the same pattern as Lean's `IO.FS.Handle`. -/
opaque EventLoopHandle : NonemptyType
def EventLoop : Type := EventLoopHandle.type
instance : Nonempty EventLoop := EventLoopHandle.property

/-- A ready event: which socket fd became ready, and what events fired.
    Uses `Nat` (boxed) for the fd to avoid compiled-mode ABI issues with
    `USize` in polymorphic positions (Prod fields, List elements). -/
structure ReadyEvent where
  socketFd : Nat
  events : EventType
deriving Repr

/-- A socket address: host string + port.
    $$\text{SockAddr} = \{ \text{host} : \text{String},\; \text{port} : \text{UInt16} \}$$ -/
structure SockAddr where
  host : String
  port : UInt16
deriving BEq, Repr

instance : ToString SockAddr where
  toString sa := s!"{sa.host}:{sa.port}"

/-- Address info returned by getAddrInfo.
    $$\text{AddrInfo} = \{ \text{family} : \text{Family},\; \text{host} : \text{String},\; \text{port} : \mathbb{N} \}$$ -/
structure AddrInfo where
  family : Family
  host : String
  port : Nat
deriving Repr

/-- Opaque socket handle. Backed by a POSIX file descriptor managed via
    `lean_alloc_external` with automatic cleanup on GC.

    Following the same pattern as Lean's `IO.FS.Handle`. -/
opaque SocketHandle : NonemptyType

/-- Raw socket handle from FFI. Internal — use `Socket state` for the typed API. -/
abbrev RawSocket : Type := SocketHandle.type
instance : Nonempty RawSocket := SocketHandle.property

/-- State tags used in the signatures of the high-level socket operations. -/
inductive SocketState where
  | fresh       -- Created via socket(), not yet bound or connected
  | bound       -- bind() succeeded
  | listening   -- listen() succeeded
  | connecting  -- Non-blocking connect() returned EINPROGRESS
  | connected   -- connect() or accept() produced this socket
  | closed      -- A successful close returned this tag
deriving BEq, DecidableEq, Repr

/-- A socket handle tagged with the state recorded when this value was returned.

    `state` is erased at runtime. The high-level API checks it at each call.
    Earlier aliases can carry stale tags, and the low-level API exposes the
    raw handle. Use the handle returned by each high-level transition. -/
structure Socket (state : SocketState) where
  protected mk ::
  raw : RawSocket

instance : Nonempty (Socket s) :=
  let ⟨raw⟩ := SocketHandle.property
  ⟨Socket.mk raw⟩

/-- State distinctness: all six POSIX socket states are pairwise distinct (15 theorems). -/
theorem SocketState.fresh_ne_bound : SocketState.fresh ≠ SocketState.bound := by decide
theorem SocketState.fresh_ne_listening : SocketState.fresh ≠ SocketState.listening := by decide
theorem SocketState.fresh_ne_connecting : SocketState.fresh ≠ SocketState.connecting := by decide
theorem SocketState.fresh_ne_connected : SocketState.fresh ≠ SocketState.connected := by decide
theorem SocketState.fresh_ne_closed : SocketState.fresh ≠ SocketState.closed := by decide
theorem SocketState.bound_ne_listening : SocketState.bound ≠ SocketState.listening := by decide
theorem SocketState.bound_ne_connecting : SocketState.bound ≠ SocketState.connecting := by decide
theorem SocketState.bound_ne_connected : SocketState.bound ≠ SocketState.connected := by decide
theorem SocketState.bound_ne_closed : SocketState.bound ≠ SocketState.closed := by decide
theorem SocketState.listening_ne_connecting : SocketState.listening ≠ SocketState.connecting := by decide
theorem SocketState.listening_ne_connected : SocketState.listening ≠ SocketState.connected := by decide
theorem SocketState.listening_ne_closed : SocketState.listening ≠ SocketState.closed := by decide
theorem SocketState.connecting_ne_connected : SocketState.connecting ≠ SocketState.connected := by decide
theorem SocketState.connecting_ne_closed : SocketState.connecting ≠ SocketState.closed := by decide
theorem SocketState.connected_ne_closed : SocketState.connected ≠ SocketState.closed := by decide

/-- SocketState BEq is reflexive — each state equals itself. -/
theorem SocketState.beq_refl (s : SocketState) : (s == s) = true := by
  cases s <;> decide

/-! ### Outcome Sum Types

    Outcome types represent the results of non-blocking socket calls.
    An outcome can report a peer disconnect, a connection still in progress,
    or a full send buffer. -/

/-- Outcome of a non-blocking `accept` on a listening socket.
    - `.accepted` — a new connected socket and the peer address
    - `.wouldBlock` — no pending connection (EAGAIN)
    - `.error` — OS-level error -/
inductive AcceptOutcome where
  | accepted   : Socket .connected → SockAddr → AcceptOutcome
  | wouldBlock : AcceptOutcome
  | error      : IO.Error → AcceptOutcome

/-- Outcome of a non-blocking `connect` or `connectFinish`.
    - `.connected` — TCP handshake completed
    - `.inProgress` — EINPROGRESS, handshake in flight (poll for writability)
    - `.refused` — connect failed (ECONNREFUSED, ETIMEDOUT, etc.) -/
inductive ConnectOutcome where
  | connected  : Socket .connected → ConnectOutcome
  | inProgress : Socket .connecting → ConnectOutcome
  | refused    : IO.Error → ConnectOutcome

/-- Outcome of a non-blocking `recv` on a connected socket.
    - `.data` — received bytes (size > 0)
    - `.wouldBlock` — no data available yet (EAGAIN)
    - `.eof` — peer closed the connection (recv returned 0)
    - `.error` — OS-level error (ECONNRESET, etc.) -/
inductive RecvOutcome where
  | data       : ByteArray → RecvOutcome
  | wouldBlock : RecvOutcome
  | eof        : RecvOutcome
  | error      : IO.Error → RecvOutcome

/-- Outcome of a non-blocking `send` on a connected socket.
    - `.sent` — n bytes written (may be partial)
    - `.wouldBlock` — send buffer full (EAGAIN)
    - `.error` — OS-level error (EPIPE, ECONNRESET, etc.) -/
inductive SendOutcome where
  | sent       : Nat → SendOutcome
  | wouldBlock : SendOutcome
  | error      : IO.Error → SendOutcome

/-- Outcome of a `poll` (select/poll) call on a socket.
    - `.ready` — the socket is ready for the requested operation
    - `.timeout` — the timeout expired before readiness
    - `.error` — OS-level error from select() -/
inductive PollOutcome where
  | ready   : PollOutcome
  | timeout : PollOutcome
  | error   : IO.Error → PollOutcome

/-- Direction to poll for: read, write, or both. -/
inductive PollMode where
  | read  : PollMode
  | write : PollMode
  | both  : PollMode

/-- Convert PollMode to the uint8 encoding expected by the C FFI. -/
def PollMode.toUInt8 : PollMode → UInt8
  | .read  => 0
  | .write => 1
  | .both  => 2

end Network.Socket
