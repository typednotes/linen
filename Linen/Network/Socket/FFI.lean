/-
  Linen.Network.Socket.FFI — C FFI bindings for POSIX sockets

  Low-level `@[extern]` declarations mapping to the C shim in `ffi/network.c`
  (built and linked by the package's `lakefile.lean` via `extern_lib linenffi`).
  These are not intended for direct use; see `Network.Socket` for the safe,
  phantom-typed API.

  ## Design

  Socket and EventLoop are opaque external objects (`lean_alloc_external`).
  All FFI functions receive them as borrowed references (`@& RawSocket` /
  `@& EventLoop`). The external classes are registered lazily on first use in
  the C layer (`linen_ensure_classes_initialized`). File descriptors are closed
  automatically by the GC finalizer; explicit `socketClose` is preferred for
  deterministic release.

  ## Encoding conventions

  - Pairs are nested: `(A × B × C)` = `(A × (B × C))` = `ctor(0,2,0)[a, ctor(0,2,0)[b, c]]`
  - Lists use `ctor(1,2,0)` for cons, `box(0)` for nil
  - `USize` values are boxed/unboxed with `lean_box`/`lean_unbox`
  - All functions return `IO` (`lean_io_result_mk_ok` / `lean_io_result_mk_error`)
  - Outcome inductives (`AcceptOutcome`, …) are built C-side with tags matching
    the Lean constructor order in `Linen.Network.Socket.Types`.
-/

import Linen.Network.Socket.Types

namespace Network.Socket.FFI

open Network.Socket

-- ── RecvBuffer: buffered reader for HTTP request parsing ──
-- Reads socket data in 4KB chunks, scans for CRLF entirely in C.
-- The RecvBuffer borrows the socket fd — the Socket must outlive it.

/-- Opaque buffered reader handle. -/
opaque RecvBufferHandle : NonemptyType
/-- A buffered reader over a socket. Reads in 4KB chunks, scans for CRLF in C.
    **Invariant (axiom-dependent):** the Socket must outlive the RecvBuffer. -/
def RecvBuffer : Type := RecvBufferHandle.type
instance : Nonempty RecvBuffer := RecvBufferHandle.property

/-- Create a buffered reader for a socket.
    $$\text{recvBufCreate} : \text{Socket} \to \text{IO}(\text{RecvBuffer})$$ -/
@[extern "linen_recvbuf_create"]
opaque recvBufCreate (sock : @& RawSocket) : IO RecvBuffer

/-- Read a CRLF-terminated line. Returns the line without the CRLF.
    Returns empty string on EOF. The scan loop runs entirely in C.
    $$\text{recvBufReadLine} : \text{RecvBuffer} \to \text{IO}(\text{String})$$ -/
@[extern "linen_recvbuf_readline"]
opaque recvBufReadLine (buf : @& RecvBuffer) : IO String

/-- Read exactly n bytes. For reading request bodies with known Content-Length.
    $$\text{recvBufReadN} : \text{RecvBuffer} \to \text{USize} \to \text{IO}(\text{ByteArray})$$ -/
@[extern "linen_recvbuf_readn"]
opaque recvBufReadN (buf : @& RecvBuffer) (n : USize) : IO ByteArray

-- ── Socket creation and management ──
-- Note: External classes for Socket and EventLoop are lazily initialized
-- in the C FFI (linen_ensure_classes_initialized) on first use.

/-- Create a socket. Returns an opaque Socket handle.
    $$\text{socketCreate} : \text{UInt8} \to \text{UInt8} \to \text{IO}(\text{Socket})$$ -/
@[extern "linen_socket_create"]
opaque socketCreate (domain : UInt8) (socktype : UInt8) : IO RawSocket

/-- Close a socket. The fd is also closed by the GC finalizer, but
    explicit close is preferred for deterministic resource release.
    $$\text{socketClose} : \text{Socket} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_close"]
opaque socketClose (sock : @& RawSocket) : IO Unit

/-- Bind a socket to an address (IPv4/IPv6 via getaddrinfo).
    $$\text{socketBind} : \text{Socket} \to \text{String} \to \text{UInt16} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_bind"]
opaque socketBind (sock : @& RawSocket) (host : @& String) (port : UInt16) : IO Unit

/-- Listen for connections.
    $$\text{socketListen} : \text{Socket} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_listen"]
opaque socketListen (sock : @& RawSocket) (backlog : USize) : IO Unit

/-- Accept a connection. Returns the client socket.
    $$\text{socketAccept} : \text{Socket} \to \text{IO}\ \text{Socket}$$ -/
@[extern "linen_socket_accept"]
opaque socketAccept (sock : @& RawSocket) : IO RawSocket

/-- Connect to a remote address (IPv4/IPv6 via getaddrinfo).
    $$\text{socketConnect} : \text{Socket} \to \text{String} \to \text{UInt16} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_connect"]
opaque socketConnect (sock : @& RawSocket) (host : @& String) (port : UInt16) : IO Unit

-- ── Send / Recv (TCP) ──

/-- Send data. Returns bytes sent.
    $$\text{socketSend} : \text{Socket} \to \text{ByteArray} \to \text{IO}(\text{USize})$$ -/
@[extern "linen_socket_send"]
opaque socketSend (sock : @& RawSocket) (data : @& ByteArray) : IO USize

/-- Receive data. Returns received bytes.
    $$\text{socketRecv} : \text{Socket} \to \text{USize} \to \text{IO}(\text{ByteArray})$$ -/
@[extern "linen_socket_recv"]
opaque socketRecv (sock : @& RawSocket) (maxlen : USize) : IO ByteArray

/-- Send all data, looping until complete. Implemented in C to avoid
    Lean compiler issues with Prod containing scalar loop state.
    $$\text{socketSendAll} : \text{Socket} \to \text{ByteArray} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_sendall"]
opaque socketSendAll (sock : @& RawSocket) (data : @& ByteArray) : IO Unit

-- ── UDP: sendto / recvfrom ──

/-- Send data to a specific address (UDP).
    $$\text{socketSendTo} : \text{Socket} \to \text{ByteArray} \to \text{String} \to \text{UInt16} \to \text{IO}(\text{USize})$$ -/
@[extern "linen_socket_sendto"]
opaque socketSendTo (sock : @& RawSocket) (data : @& ByteArray) (host : @& String) (port : UInt16) : IO USize

/-- Receive data with sender address (UDP).
    Returns `(data, (host, port))`.
    $$\text{socketRecvFrom} : \text{Socket} \to \text{USize} \to \text{IO}(\text{ByteArray} \times (\text{String} \times \text{USize}))$$ -/
@[extern "linen_socket_recvfrom"]
opaque socketRecvFrom (sock : @& RawSocket) (maxlen : USize) : IO (ByteArray × String × USize)

-- ── Socket options ──

/-- Set SO_REUSEADDR option.
    $$\text{setReuseAddr} : \text{Socket} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_set_reuseaddr"]
opaque setReuseAddr (sock : @& RawSocket) (enable : UInt8) : IO Unit

/-- Set TCP_NODELAY option.
    $$\text{setNoDelay} : \text{Socket} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_set_nodelay"]
opaque setNoDelay (sock : @& RawSocket) (enable : UInt8) : IO Unit

/-- Set non-blocking mode.
    $$\text{setNonBlocking} : \text{Socket} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_set_nonblocking"]
opaque setNonBlocking (sock : @& RawSocket) (enable : UInt8) : IO Unit

/-- Set SO_KEEPALIVE option.
    $$\text{setKeepAlive} : \text{Socket} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_set_keepalive"]
opaque setKeepAlive (sock : @& RawSocket) (enable : UInt8) : IO Unit

/-- Set SO_LINGER option.
    $$\text{setLinger} : \text{Socket} \to \text{UInt8} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_set_linger"]
opaque setLinger (sock : @& RawSocket) (enable : UInt8) (seconds : USize) : IO Unit

/-- Set SO_RCVBUF size.
    $$\text{setRecvBuf} : \text{Socket} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_set_recvbuf"]
opaque setRecvBuf (sock : @& RawSocket) (size : USize) : IO Unit

/-- Set SO_SNDBUF size.
    $$\text{setSendBuf} : \text{Socket} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_set_sendbuf"]
opaque setSendBuf (sock : @& RawSocket) (size : USize) : IO Unit

/-- Shutdown a socket (read, write, or both).
    $$\text{socketShutdown} : \text{Socket} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_socket_shutdown"]
opaque socketShutdown (sock : @& RawSocket) (how : UInt8) : IO Unit

/-- Get peer address host string.
    $$\text{getPeerNameHost} : \text{Socket} \to \text{IO}(\text{String})$$ -/
@[extern "linen_socket_getpeername_host"]
opaque getPeerNameHost (sock : @& RawSocket) : IO String

/-- Get peer address port.
    $$\text{getPeerNamePort} : \text{Socket} \to \text{IO}(\text{UInt16})$$ -/
@[extern "linen_socket_getpeername_port"]
opaque getPeerNamePort (sock : @& RawSocket) : IO UInt16

/-- Get local address host string.
    $$\text{getSockNameHost} : \text{Socket} \to \text{IO}(\text{String})$$ -/
@[extern "linen_socket_getsockname_host"]
opaque getSockNameHost (sock : @& RawSocket) : IO String

/-- Get local address port.
    $$\text{getSockNamePort} : \text{Socket} \to \text{IO}(\text{UInt16})$$ -/
@[extern "linen_socket_getsockname_port"]
opaque getSockNamePort (sock : @& RawSocket) : IO UInt16

-- ── DNS resolution ──

/-- Resolve a hostname. Returns list of `(family, (host, port))`.
    Uses `Nat` instead of `USize` in product fields for compiled-mode ABI safety.
    $$\text{getAddrInfo} : \text{String} \to \text{String} \to \text{IO}(\text{List}(\text{Nat} \times (\text{String} \times \text{Nat})))$$ -/
@[extern "linen_getaddrinfo"]
opaque getAddrInfo (node : @& String) (service : @& String) : IO (List (Nat × String × Nat))

-- ── Non-blocking socket operations ──
-- Return outcome sum types instead of throwing on EAGAIN/EWOULDBLOCK.

/-- Non-blocking accept. Returns `AcceptOutcome` (tagged union from C).
    $$\text{socketAcceptNB} : \text{Socket} \to \text{IO AcceptOutcome}$$ -/
@[extern "linen_socket_accept_nb"]
opaque socketAcceptNB (sock : @& RawSocket) : IO AcceptOutcome

/-- Non-blocking connect. Sets O_NONBLOCK, returns `ConnectOutcome`.
    $$\text{socketConnectNB} : \text{Socket} \to \text{String} \to \text{UInt16} \to \text{IO ConnectOutcome}$$ -/
@[extern "linen_socket_connect_nb"]
opaque socketConnectNB (sock : @& RawSocket) (host : @& String) (port : UInt16) : IO ConnectOutcome

/-- Check non-blocking connect result (after writable event).
    $$\text{socketConnectFinish} : \text{Socket} \to \text{IO ConnectOutcome}$$ -/
@[extern "linen_socket_connect_finish"]
opaque socketConnectFinish (sock : @& RawSocket) : IO ConnectOutcome

/-- Non-blocking send. Returns `SendOutcome`.
    $$\text{socketSendNB} : \text{Socket} \to \text{ByteArray} \to \text{IO SendOutcome}$$ -/
@[extern "linen_socket_send_nb"]
opaque socketSendNB (sock : @& RawSocket) (data : @& ByteArray) : IO SendOutcome

/-- Non-blocking recv. Returns `RecvOutcome`.
    $$\text{socketRecvNB} : \text{Socket} \to \text{USize} \to \text{IO RecvOutcome}$$ -/
@[extern "linen_socket_recv_nb"]
opaque socketRecvNB (sock : @& RawSocket) (maxlen : USize) : IO RecvOutcome

/-- Extract the raw file descriptor from a socket handle. For EventLoop correlation.
    Returns a Nat (boxed) to avoid compiled-mode ABI issues with `IO USize`.
    $$\text{socketGetFd} : \text{Socket} \to \text{IO Nat}$$ -/
@[extern "linen_socket_get_fd"]
opaque socketGetFd (sock : @& RawSocket) : IO Nat

-- ── Non-blocking RecvBuffer operations ──

/-- Non-blocking readline. Returns `none` on EAGAIN, `some line` when complete.
    Partial line state is preserved in the buffer between calls.
    $$\text{recvBufReadLineNB} : \text{RecvBuffer} \to \text{IO (Option String)}$$ -/
@[extern "linen_recvbuf_readline_nb"]
opaque recvBufReadLineNB (buf : @& RecvBuffer) : IO (Option String)

/-- Non-blocking readn. Returns `(data, complete)` where complete indicates
    all n bytes were read.
    $$\text{recvBufReadNNB} : \text{RecvBuffer} \to \text{USize} \to \text{IO (ByteArray × Bool)}$$ -/
@[extern "linen_recvbuf_readn_nb"]
opaque recvBufReadNNB (buf : @& RecvBuffer) (n : USize) : IO (ByteArray × Bool)

-- ── Socket readiness polling (select) ──

/-- Poll a socket for readiness using select().
    Thin wrapper around the POSIX select() syscall.
    $$\text{socketPoll} : \text{Socket} \to \text{UInt8} \to \text{UInt32} \to \text{IO PollOutcome}$$ -/
@[extern "linen_socket_poll"]
opaque socketPoll (sock : @& RawSocket) (mode : UInt8) (timeoutMs : UInt32) : IO PollOutcome

-- ── Event multiplexing (kqueue/epoll) ──

/-- Create an event loop (kqueue on macOS, epoll on Linux).
    $$\text{eventLoopCreate} : \text{IO}(\text{EventLoop})$$ -/
@[extern "linen_event_loop_create"]
opaque eventLoopCreate : IO EventLoop

/-- Register interest in events for a socket.
    $$\text{eventLoopAdd} : \text{EventLoop} \to \text{Socket} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_event_loop_add"]
opaque eventLoopAdd (loop : @& EventLoop) (sock : @& RawSocket) (events : USize) : IO Unit

/-- Unregister a socket from the event loop.
    $$\text{eventLoopDel} : \text{EventLoop} \to \text{Socket} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_event_loop_del"]
opaque eventLoopDel (loop : @& EventLoop) (sock : @& RawSocket) : IO Unit

/-- Wait for events. Returns `List (fd × events)` where both are `Nat` (boxed).
    Uses `Nat` instead of `USize` to avoid compiled-mode ABI issues with scalar
    types in polymorphic ctor fields.
    timeout is in milliseconds; pass a very large value for indefinite blocking.
    $$\text{eventLoopWait} : \text{EventLoop} \to \text{USize} \to \text{IO}(\text{List}(\text{Nat} \times \text{Nat}))$$ -/
@[extern "linen_event_loop_wait"]
opaque eventLoopWait (loop : @& EventLoop) (timeoutMs : USize) : IO (List (Nat × Nat))

/-- Close the event loop. The fd is also closed by the GC finalizer, but
    explicit close is preferred for deterministic resource release.
    $$\text{eventLoopClose} : \text{EventLoop} \to \text{IO}(\text{Unit})$$ -/
@[extern "linen_event_loop_close"]
opaque eventLoopClose (loop : @& EventLoop) : IO Unit

end Network.Socket.FFI
