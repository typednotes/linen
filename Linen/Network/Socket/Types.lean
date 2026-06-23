/-
  Linen.Network.Socket.Types — Socket type definitions

  Core types for the network socket abstraction. Ports Haskell's
  `Network.Socket.Types` (from the `network` package), reshaped to Lean's
  module hierarchy and built on the standard library (`IO.Error`, `ByteArray`,
  `USize`, …).

  ## Dependent-Type Encoding of POSIX Socket Lifecycle

  The POSIX socket API is a state machine: a socket must be created, then
  bound, then set to listen, before it can accept connections. Calling
  operations in the wrong order returns an error code that nothing forces
  you to check.

  Lean 4's dependent types let us encode this state machine directly in the
  type system via a **phantom type parameter** on `Socket`:

  ```
  Socket (state : SocketState)    -- state is erased at runtime (zero cost)
  ```

  Every API function declares which state it requires and which state it
  produces. The Lean kernel rejects any program that violates the protocol
  -- no runtime check, no assert, no exception. Just a type error.

  The `.closed` state and the proof obligation `state ≠ .closed` on `close`
  additionally prevent **double-close** at compile time: once a socket enters
  the `.closed` state, no operation (including `close` itself) will accept it.

  ## Non-blocking I/O and External State Changes

  All socket operations are non-blocking by default and return **outcome
  sum types** (`AcceptOutcome`, `RecvOutcome`, `SendOutcome`, `ConnectOutcome`)
  that force callers to handle every possibility: success, `wouldBlock`
  (EAGAIN), EOF (peer closed), and errors. This models the fact that OS-level
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

/-- Combine event flags. -/
def merge (a b : EventType) : EventType := ⟨a.flags ||| b.flags⟩

instance : OrOp EventType where
  or := merge

/-- Test if a specific flag is set. -/
def hasReadable (e : EventType) : Bool := (e.flags &&& 1) != 0
def hasWritable (e : EventType) : Bool := (e.flags &&& 2) != 0
def hasError (e : EventType) : Bool := (e.flags &&& 4) != 0

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

/-- POSIX socket lifecycle states.

    Lean 4's dependent types encode the full POSIX state machine as a phantom
    parameter on `Socket`. Each API function constrains which state it accepts
    and which state it returns, so the compiler enforces the protocol:

    - `bind`          requires `.fresh`,      produces `.bound`
    - `listen`        requires `.bound`,      produces `.listening`
    - `accept`        requires `.listening`,  returns `AcceptOutcome`
    - `connect`       requires `.fresh`,      returns `ConnectOutcome`
    - `connectFinish` requires `.connecting`, returns `ConnectOutcome`
    - `send`          requires `.connected`,  returns `SendOutcome`
    - `recv`          requires `.connected`,  returns `RecvOutcome`
    - `close`         requires `state ≠ .closed` (proof obligation), produces `.closed`

    The parameter is fully erased at runtime (zero cost). Protocol violations
    are compile-time errors -- not exceptions, not asserts, not runtime checks. -/
inductive SocketState where
  | fresh       -- Created via socket(), not yet bound or connected
  | bound       -- bind() succeeded
  | listening   -- listen() succeeded
  | connecting  -- Non-blocking connect() returned EINPROGRESS
  | connected   -- connect() or accept() produced this socket
  | closed      -- close() succeeded — no further operations are valid
deriving BEq, DecidableEq, Repr

/-- A socket tagged with its POSIX lifecycle state.

    This is the central example of Lean 4's phantom-type-parameter technique:
    the `state` parameter exists only at the type level and is **completely
    erased** at runtime. A `Socket .connected` and a `Socket .fresh` have
    identical runtime representations (both are opaque FFI handles), yet the
    compiler statically distinguishes them and rejects invalid transitions.

    ```
    Fresh ──bind──→ Bound ──listen──→ Listening ──accept──→ Connected
      │                                                      (send/recv)
      ├──connect──→ Connected (immediate)
      └──connect──→ Connecting ──connectFinish──→ Connected
                                                                 │
    Any state ──close(proof: state ≠ .closed)──→ Closed
    ```

    **Double-close prevention:** `close` requires a proof `state ≠ .closed`.
    For concrete states (`.fresh`, `.connected`, ...) the proof is discharged
    automatically by `decide`. For a `Socket .closed` value the proof is
    **impossible** -- the compiler rejects the call at type-checking time.

    The constructor is `protected` to prevent casual state fabrication.
    Use the high-level API in `Network.Socket` for state transitions. -/
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

    Non-blocking socket operations return **outcome types** that force callers
    to handle every possibility. The type system models the programmer's
    *knowledge* of the socket state, updated at each synchronization point.

    These encode the fact that OS-level state can change asynchronously:
    peer disconnect, non-blocking connect in progress, send buffer full, etc. -/

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
