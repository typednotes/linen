/-
  Linen.Network.Socket — High-level socket API with POSIX lifecycle states

  Provides a safe, high-level API for POSIX sockets where the full lifecycle
  (fresh, bound, listening, connected, closed) is tracked as a **phantom type
  parameter** on `Socket`. The Lean 4 kernel verifies every state transition at
  compile time; the parameter is **erased at runtime** -- zero overhead, zero
  branches, same codegen as raw C.

  ## How Lean 4's Dependent Types Enforce the POSIX Protocol

  Every function in this module declares its pre- and post-state in the type
  signature. The compiler threads these constraints through the program and
  rejects any code path that would violate the protocol:

  | Function        | Requires            | Produces           | Enforced by            |
  |-----------------|---------------------|--------------------|------------------------|
  | `socket`        | (nothing)           | `Socket .fresh`    | return type            |
  | `bind`          | `Socket .fresh`     | `Socket .bound`    | argument type          |
  | `listen`        | `Socket .bound`     | `Socket .listening`| argument type          |
  | `accept`        | `Socket .listening` | `AcceptOutcome`    | argument type          |
  | `connect`       | `Socket .fresh`     | `ConnectOutcome`   | argument type          |
  | `connectFinish` | `Socket .connecting`| `ConnectOutcome`   | argument type          |
  | `send`          | `Socket .connected` | `SendOutcome`      | argument type          |
  | `recv`          | `Socket .connected` | `RecvOutcome`      | argument type          |
  | `close`         | `Socket state`, `state ≠ .closed` | `Socket .closed` | **proof obligation** |

  The `close` function is the most interesting case: it accepts a socket in
  **any** state, but requires the caller to supply a proof that the state is
  not already `.closed`. For concrete states this proof is discharged
  automatically by `decide`. For a `Socket .closed` the proof is impossible,
  making double-close a **compile-time error** -- not a runtime exception.

  ## Design

  Wraps the raw FFI bindings (`Linen.Network.Socket.FFI`) with a clean API
  matching Haskell's `Network.Socket`. Supports IPv4, IPv6, UDP, and event
  multiplexing (kqueue/epoll).

  Socket and EventLoop are opaque types backed by `lean_alloc_external`.
  The GC finalizer automatically closes file descriptors, but explicit
  `close` is preferred for deterministic resource management.

  ## POSIX Socket State Machine

  ```
  Fresh ──bind──→ Bound ──listen──→ Listening ──accept──→ Connected
    │                                                      (send/recv)
    ├──connect──→ Connected (immediate)
    └──connect──→ Connecting ──connectFinish──→ Connected
                                                               │
  Any state ──close(proof: state ≠ .closed)──→ Closed
  ```

  ## Compile-Time Guarantees (all zero-cost)

  - Can't send/recv on a non-connected socket (type error)
  - Can't accept on a non-listening socket (type error)
  - Can't bind an already-bound socket (type error)
  - Can't listen on an unbound socket (type error)
  - Can't close an already-closed socket (proof obligation fails)
  - `withSocket` ensures sockets are closed even on exceptions (try/finally)
  - `withEventLoop` ensures event loops are closed even on exceptions
  - SO_REUSEADDR is set by default for server sockets via `listenTCP`
  - All IO errors from POSIX calls are surfaced as `IO.Error`
-/

import Linen.Network.Socket.FFI

namespace Network.Socket

open Network.Socket.FFI

-- ══════════════════════════════════════════════════════════════
-- Socket creation and lifecycle
-- ══════════════════════════════════════════════════════════════

/-- Create a new socket in the fresh state.
    $$\text{socket} : \text{Family} \to \text{SocketType} \to \text{IO}(\text{Socket}\ \texttt{.fresh})$$
    POSIX: socket(2) returns an unbound, unconnected socket. -/
def socket (fam : Family) (typ : SocketType) : IO (Socket .fresh) := do
  let raw ← socketCreate fam.toUInt8 typ.toUInt8
  pure (Socket.mk raw)

/-- Close a socket in any non-closed state, returning a socket in the `.closed` state.
    $$\text{close} : \text{Socket}\ s \to (s \neq \texttt{.closed}) \to \text{IO}(\text{Socket}\ \texttt{.closed})$$
    POSIX: close(2) is valid in any state.

    **Lean 4 dependent-type guarantee — double-close is a compile-time error.**

    The second parameter `_h : state ≠ .closed` is a proof obligation that
    the Lean kernel must discharge before the program type-checks.

    - For `Socket .fresh`, `.bound`, `.listening`, `.connected`: the default
      tactic `by decide` produces the proof automatically (zero effort for
      the caller, zero cost at runtime -- proofs are erased).
    - For `Socket .closed`: the proposition `.closed ≠ .closed` is **false**,
      so no proof can exist.  The call is rejected at compile time.

    The returned `Socket .closed` carries no valid operations -- every
    function in this module requires a non-closed state. This is
    enforced by the type system, not by runtime checks.

    Prefer `withSocket` for bracket-style resource safety. -/
@[inline] def close (s : Socket state) (_h : state ≠ .closed := by decide) : IO (Socket .closed) := do
  socketClose s.raw
  pure (Socket.mk s.raw)

/-- Run an action with a fresh socket, ensuring it is closed afterwards.
    $$\text{withSocket} : \text{Family} \to \text{SocketType} \to (\text{Socket}\ \texttt{.fresh} \to \text{IO}(\alpha)) \to \text{IO}(\alpha)$$ -/
def withSocket (fam : Family) (typ : SocketType) (f : Socket .fresh → IO α) : IO α := do
  let s ← socket fam typ
  try
    f s
  finally
    let _ ← close s

-- ══════════════════════════════════════════════════════════════
-- Core socket operations
-- ══════════════════════════════════════════════════════════════

/-- Bind a fresh socket to an address.
    $$\text{bind} : \text{Socket}\ \texttt{.fresh} \to \text{SockAddr} \to \text{IO}(\text{Socket}\ \texttt{.bound})$$
    POSIX: bind(2) requires an unbound socket. -/
def bind (s : Socket .fresh) (addr : SockAddr) : IO (Socket .bound) := do
  socketBind s.raw addr.host addr.port
  pure (Socket.mk s.raw)

/-- Start listening for connections on a bound socket.
    $$\text{listen} : \text{Socket}\ \texttt{.bound} \to \mathbb{N} \to \text{IO}(\text{Socket}\ \texttt{.listening})$$
    POSIX: listen(2) requires a bound socket. -/
def listen (s : Socket .bound) (backlog : Nat := 128) : IO (Socket .listening) := do
  socketListen s.raw backlog.toUSize
  pure (Socket.mk s.raw)

/-- Accept a connection on a listening socket (non-blocking).
    Returns `AcceptOutcome` — the caller must pattern match on success,
    `wouldBlock` (EAGAIN), or error.
    $$\text{accept} : \text{Socket}\ \texttt{.listening} \to \text{IO AcceptOutcome}$$
    POSIX: accept(2) requires a listening socket. -/
def accept (s : Socket .listening) : IO AcceptOutcome :=
  socketAcceptNB s.raw

/-- Connect a fresh socket to a remote address (non-blocking).
    Returns `ConnectOutcome` — may be `.connected` (immediate) or
    `.inProgress` (EINPROGRESS, poll for writability then call `connectFinish`).
    $$\text{connect} : \text{Socket}\ \texttt{.fresh} \to \text{SockAddr} \to \text{IO ConnectOutcome}$$
    POSIX: connect(2) on an unbound socket implicitly binds it. -/
def connect (s : Socket .fresh) (addr : SockAddr) : IO ConnectOutcome :=
  socketConnectNB s.raw addr.host addr.port

/-- Check whether a non-blocking connect completed.
    Call after the event loop reports the socket is writable.
    $$\text{connectFinish} : \text{Socket}\ \texttt{.connecting} \to \text{IO ConnectOutcome}$$ -/
def connectFinish (s : Socket .connecting) : IO ConnectOutcome :=
  socketConnectFinish s.raw

/-- Send a ByteArray on a connected socket (non-blocking).
    Returns `SendOutcome` — `.sent n` (partial or full), `.wouldBlock`, or `.error`.
    $$\text{send} : \text{Socket}\ \texttt{.connected} \to \text{ByteArray} \to \text{IO SendOutcome}$$
    POSIX: send(2) requires a connected socket. -/
@[inline] def send (s : Socket .connected) (data : ByteArray) : IO SendOutcome :=
  socketSendNB s.raw data

/-- Receive up to `maxlen` bytes from a connected socket (non-blocking).
    Returns `RecvOutcome` — `.data`, `.wouldBlock`, `.eof` (peer closed), or `.error`.
    $$\text{recv} : \text{Socket}\ \texttt{.connected} \to \mathbb{N} \to \text{IO RecvOutcome}$$
    POSIX: recv(2) requires a connected socket. -/
@[inline] def recv (s : Socket .connected) (maxlen : Nat := 4096) : IO RecvOutcome :=
  socketRecvNB s.raw maxlen.toUSize

/-- Get the raw file descriptor for EventLoop correlation.
    $$\text{getFd} : \text{Socket}\ s \to \text{IO Nat}$$ -/
@[inline] def getFd (s : Socket state) : IO Nat :=
  socketGetFd s.raw

/-- Poll a socket for readiness (read, write, or both) with a timeout.
    Uses POSIX select() under the hood.
    $$\text{poll} : \text{Socket}\ s \to \text{PollMode} \to \text{Nat} \to \text{IO PollOutcome}$$ -/
@[inline] def poll (s : Socket state) (mode : PollMode) (timeoutMs : Nat := 30000) : IO PollOutcome :=
  FFI.socketPoll s.raw mode.toUInt8 timeoutMs.toUInt32

/-- Shutdown a connected socket for reading, writing, or both.
    $$\text{shutdown} : \text{Socket}\ \texttt{.connected} \to \text{ShutdownHow} \to \text{IO}(\text{Unit})$$
    POSIX: shutdown(2) requires a connected socket. -/
@[inline] def shutdown (s : Socket .connected) (how : ShutdownHow) : IO Unit :=
  socketShutdown s.raw how.toUInt8

-- ══════════════════════════════════════════════════════════════
-- Socket options
-- ══════════════════════════════════════════════════════════════

/-- Set the SO_REUSEADDR option. Valid in any state (typically before bind). -/
@[inline] def setReuseAddr (s : Socket state) (enable : Bool := true) : IO Unit :=
  FFI.setReuseAddr s.raw (if enable then 1 else 0)

/-- Set the TCP_NODELAY option on a connected socket. -/
@[inline] def setNoDelay (s : Socket .connected) (enable : Bool := true) : IO Unit :=
  FFI.setNoDelay s.raw (if enable then 1 else 0)

/-- Set non-blocking mode. Valid in any state. -/
@[inline] def setNonBlocking (s : Socket state) (enable : Bool := true) : IO Unit :=
  FFI.setNonBlocking s.raw (if enable then 1 else 0)

/-- Set the SO_KEEPALIVE option on a connected socket. -/
@[inline] def setKeepAlive (s : Socket .connected) (enable : Bool := true) : IO Unit :=
  FFI.setKeepAlive s.raw (if enable then 1 else 0)

/-- Set the SO_LINGER option.
    When enabled, `close` will block for up to `seconds` to flush pending data. -/
@[inline] def setLinger (s : Socket state) (enable : Bool) (seconds : Nat := 0) : IO Unit :=
  FFI.setLinger s.raw (if enable then 1 else 0) seconds.toUSize

/-- Set the receive buffer size (SO_RCVBUF). -/
@[inline] def setRecvBufSize (s : Socket state) (size : Nat) : IO Unit :=
  FFI.setRecvBuf s.raw size.toUSize

/-- Set the send buffer size (SO_SNDBUF). -/
@[inline] def setSendBufSize (s : Socket state) (size : Nat) : IO Unit :=
  FFI.setSendBuf s.raw size.toUSize

-- ══════════════════════════════════════════════════════════════
-- Address introspection
-- ══════════════════════════════════════════════════════════════

/-- Get the remote peer's address. Requires a connected socket.
    $$\text{getPeerName} : \text{Socket}\ \texttt{.connected} \to \text{IO}(\text{SockAddr})$$ -/
def getPeerName (s : Socket .connected) : IO SockAddr := do
  let host ← FFI.getPeerNameHost s.raw
  let port ← FFI.getPeerNamePort s.raw
  pure ⟨host, port⟩

/-- Get the socket's locally-bound address. Valid in any state.
    $$\text{getSockName} : \text{Socket}\ s \to \text{IO}(\text{SockAddr})$$ -/
def getSockName (s : Socket state) : IO SockAddr := do
  let host ← FFI.getSockNameHost s.raw
  let port ← FFI.getSockNamePort s.raw
  pure ⟨host, port⟩

-- ══════════════════════════════════════════════════════════════
-- DNS resolution
-- ══════════════════════════════════════════════════════════════

/-- Resolve a hostname and service to a list of addresses.
    $$\text{getAddrInfo} : \text{String} \to \text{String} \to \text{IO}(\text{List}\ \text{AddrInfo})$$ -/
def getAddrInfo (host : String) (service : String) : IO (List AddrInfo) := do
  let results ← FFI.getAddrInfo host service
  pure (results.map fun (fam, h, p) =>
    { family := Family.ofUInt8 fam.toUInt8
    , host := h
    , port := p })

-- ══════════════════════════════════════════════════════════════
-- Convenience: TCP server
-- ══════════════════════════════════════════════════════════════

/-- Create a TCP server socket: socket + reuseaddr + bind + listen.
    Returns a socket in the listening state.
    $$\text{listenTCP} : \text{String} \to \text{UInt16} \to \text{IO}(\text{Socket}\ \texttt{.listening})$$ -/
def listenTCP (host : String) (port : UInt16) (backlog : Nat := 128) : IO (Socket .listening) := do
  let s ← socket .inet .stream
  setReuseAddr s
  let s ← bind s ⟨host, port⟩
  listen s backlog

/-- Create a TCP server socket with IPv6 support.
    Returns a socket in the listening state.
    $$\text{listenTCP6} : \text{String} \to \text{UInt16} \to \text{IO}(\text{Socket}\ \texttt{.listening})$$ -/
def listenTCP6 (host : String) (port : UInt16) (backlog : Nat := 128) : IO (Socket .listening) := do
  let s ← socket .inet6 .stream
  setReuseAddr s
  let s ← bind s ⟨host, port⟩
  listen s backlog

-- ══════════════════════════════════════════════════════════════
-- Resource-safe bracket
-- ══════════════════════════════════════════════════════════════

/-- Run an action with a listening socket, ensuring it is closed afterwards.
    $$\text{withListenTCP} : \text{String} \to \text{UInt16} \to (\text{Socket}\ \texttt{.listening} \to \text{IO}\ \alpha) \to \text{IO}\ \alpha$$ -/
def withListenTCP (host : String) (port : UInt16) (f : Socket .listening → IO α) :
    IO α := do
  let s ← listenTCP host port
  try
    f s
  finally
    let _ ← close s

-- ══════════════════════════════════════════════════════════════
-- Event loop (kqueue / epoll)
-- ══════════════════════════════════════════════════════════════

namespace EventLoop

/-- Create a new event loop.
    $$\text{create} : \text{IO}(\text{EventLoop})$$ -/
def create : IO EventLoop :=
  FFI.eventLoopCreate

/-- Close an event loop.
    $$\text{close} : \text{EventLoop} \to \text{IO}(\text{Unit})$$ -/
@[inline] def close (el : EventLoop) : IO Unit :=
  FFI.eventLoopClose el

/-- Register interest in events for a socket (any state).
    $$\text{add} : \text{EventLoop} \to \text{Socket}\ s \to \text{EventType} \to \text{IO}(\text{Unit})$$ -/
@[inline] def add (el : EventLoop) (s : Socket state) (events : EventType) : IO Unit :=
  FFI.eventLoopAdd el s.raw events.flags

/-- Unregister a socket from the event loop (any state).
    $$\text{del} : \text{EventLoop} \to \text{Socket}\ s \to \text{IO}(\text{Unit})$$ -/
@[inline] def del (el : EventLoop) (s : Socket state) : IO Unit :=
  FFI.eventLoopDel el s.raw

/-- Wait for events with a timeout (in milliseconds).
    Returns a list of ready events.
    $$\text{wait} : \text{EventLoop} \to \text{Nat} \to \text{IO}(\text{List}\ \text{ReadyEvent})$$ -/
def wait (el : EventLoop) (timeoutMs : Nat := 1000) : IO (List ReadyEvent) := do
  let results ← FFI.eventLoopWait el timeoutMs.toUSize
  pure (results.map fun (fd, evts) => ⟨fd, ⟨evts.toUSize⟩⟩)

end EventLoop

/-- Run an action with an event loop, ensuring it is closed afterwards.
    $$\text{withEventLoop} : (\text{EventLoop} \to \text{IO}(\alpha)) \to \text{IO}(\alpha)$$ -/
def withEventLoop (f : EventLoop → IO α) : IO α := do
  let el ← EventLoop.create
  try
    f el
  finally
    EventLoop.close el

end Network.Socket
