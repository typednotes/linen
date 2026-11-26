import Linen.Control.Resource
import Linen.Network.Socket.Blocking
import Linen.Network.TLS.Scoped

/-! # Scoped sockets
The runner owns the socket. Compose operations with `Program.ibind` for a
fixed transition and `Program.bind` when the result selects the next state.
-/

namespace Network.Socket
open Control.Resource

private opaque socketAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- A socket action with a fixed postcondition. -/
abbrev SocketM := Action socketAccess.val Socket

/-- A socket action whose result selects its postcondition. -/
abbrev SocketProgram := Program socketAccess.val Socket

namespace SocketM

private def use (f : Socket state → IO α) : SocketM state state α :=
  Program.step socketAccess.property fun socket => do return ⟨← f socket, socket⟩

private def move (f : Socket pre → IO (Socket post)) : SocketM pre post Unit :=
  Program.step socketAccess.property fun socket => do return ⟨(), ← f socket⟩

private def eval (s : Socket state) (action : SocketM state post α) : IO α :=
  (·.1) <$> Program.run socketAccess.property action s

/-- Run a program that closes its newly acquired socket on successful return.
The bracket also releases the descriptor if an operation throws. -/
def run (family : Family) (kind : SocketType) (action : SocketM .fresh .closed α) : IO α :=
  withSocket family kind fun s => eval s action

/-- Bind a fresh socket to a local address. -/
def bindAddress (addr : SockAddr) : SocketM .fresh .bound Unit :=
  move (Network.Socket.bind · addr)

/-- Begin listening for incoming connections. -/
def listen (backlog : Nat := 128) : SocketM .bound .listening Unit :=
  move (Network.Socket.listen · backlog)

/-- Connect and wait for completion, restoring blocking mode on success. -/
def connect (addr : SockAddr) : SocketM .fresh .connected Unit :=
  move (Blocking.connect · addr)

/-- State information returned by a non-blocking connect operation. -/
inductive ConnectStatus where
  | connected
  | inProgress

/-- A successful observation selects the next socket state. -/
def ConnectStatus.state : ConnectStatus → SocketState
  | .connected => .connected
  | .inProgress => .connecting

private def connectStep (f : Socket pre → IO ConnectOutcome) :
    SocketProgram pre ConnectStatus ConnectStatus.state :=
  Program.step socketAccess.property fun s => do
    match ← f s with
    | .connected next => return ⟨.connected, next⟩
    | .inProgress next => return ⟨.inProgress, next⟩
    | .refused error => throw error

/-- Start a non-blocking connection. Refusal aborts the program and triggers
cleanup. The continuation sees either a connected or a connecting socket. -/
def connectNB (addr : SockAddr) : SocketProgram .fresh ConnectStatus ConnectStatus.state :=
  connectStep (Network.Socket.connect · addr)

/-- Observe completion after polling the connecting socket for writability.
The socket can still be in progress. -/
def connectFinish : SocketProgram .connecting ConnectStatus ConnectStatus.state :=
  connectStep Network.Socket.connectFinish

/-- Poll a live socket for readiness. -/
def poll (mode : PollMode) (timeoutMillis : Nat := 30000)
    (_ : state ≠ .closed := by decide) : SocketM state state PollOutcome :=
  use (Network.Socket.poll · mode timeoutMillis)

/-- Enable or disable address reuse before binding. -/
def setReuseAddr (enable : Bool := true) : SocketM .fresh .fresh Unit :=
  use (Network.Socket.setReuseAddr · enable)

/-- Enable or disable non-blocking mode on a live socket. -/
def setNonBlocking (enable : Bool := true) (_ : state ≠ .closed := by decide) :
    SocketM state state Unit :=
  use (Network.Socket.setNonBlocking · enable)

/-- Inspect the address associated with a live socket. -/
def localAddress (_ : state ≠ .closed := by decide) : SocketM state state SockAddr :=
  use getSockName

/-- Inspect the peer address of a connected socket. -/
def peerAddress : SocketM .connected .connected SockAddr :=
  use getPeerName

/-- Send bytes once, reporting partial progress, backpressure or an error. -/
def send (data : ByteArray) : SocketM .connected .connected SendOutcome :=
  use (Network.Socket.send · data)

/-- Receive bytes once, reporting EOF, backpressure or an error. -/
def recv (maxLen : Nat := 4096) : SocketM .connected .connected RecvOutcome :=
  use (Network.Socket.recv · maxLen)

/-- A receive leaves the socket available on data or backpressure. An
    end-of-input or error result from `recvOrClose` means it was closed. -/
def recvOrCloseState : RecvOutcome → SocketState
  | .data _ | .wouldBlock => .connected
  | .eof | .error _ => .closed

/-- Attempt a receive, then close on EOF or an error before running the
    continuation. For stream protocols that treat either result as terminal.
    Use `recv` to retry errors or write after the peer finishes sending.
    A positive buffer size prevents an empty read from being treated as EOF. -/
def recvOrClose (maxLen : Nat := 4096) (_ : 0 < maxLen := by decide) :
    SocketProgram .connected RecvOutcome recvOrCloseState :=
  Program.step socketAccess.property fun s => do
    match ← Network.Socket.recv s maxLen with
    | .data bytes => return ⟨.data bytes, s⟩
    | .wouldBlock => return ⟨.wouldBlock, s⟩
    | .eof => return ⟨.eof, ← Network.Socket.close s⟩
    | .error error => return ⟨.error error, ← Network.Socket.close s⟩

/-- Send all bytes, waiting for backpressure to clear. -/
def sendAll (data : ByteArray) (timeoutMillis : Nat := Blocking.defaultTimeoutMillis) :
    SocketM .connected .connected Unit :=
  use (Blocking.sendAll · data timeoutMillis)

/-- Receive bytes, waiting for readability if needed. Empty bytes indicate EOF. -/
def recvBlocking (maxLen : Nat := 4096) (timeoutMillis : Nat := Blocking.defaultTimeoutMillis) :
    SocketM .connected .connected ByteArray :=
  use (Blocking.recv · maxLen timeoutMillis)

/-- Wait for a connection, then run a nested program over the accepted socket.
The listener remains available. The child must close on successful return
and is closed by the bracket on an exception. -/
def withAccepted (action : SockAddr → SocketM .connected .closed α)
    (timeoutMillis : Nat := Blocking.defaultTimeoutMillis) : SocketM .listening .listening α :=
  use fun listener => do
    let (child, peer) ← Blocking.accept listener timeoutMillis
    try eval child (action peer) finally discard (Network.Socket.close child)

/-- Handle one non-blocking accept attempt. `none` means no connection was
ready. The accepted socket has its own scope. -/
def tryWithAccepted (action : SockAddr → SocketM .connected .closed α) :
    SocketM .listening .listening (Option α) :=
  use fun listener => do
    match ← Network.Socket.accept listener with
    | .wouldBlock => return none
    | .error error => throw error
    | .accepted child peer =>
      try some <$> eval child (action peer) finally discard (Network.Socket.close child)

/-- Run client TLS inside the socket's scope. TLS cleanup precedes socket cleanup. -/
def withClientTLS (ctx : TLS.TLSContext) (hostname : String) (action : TLS.TLSM .open_ post α) :
    SocketM .connected .connected α :=
  use fun s => TLS.TLSM.runClient ctx s.raw hostname action

/-- Run server TLS inside the socket's scope. TLS cleanup precedes socket cleanup. -/
def withServerTLS (ctx : TLS.TLSContext) (action : TLS.TLSM .open_ post α) :
    SocketM .connected .connected α :=
  use fun s => TLS.TLSM.runServer ctx s.raw action

/-- Close a live socket. No scoped operation uses the closed socket again. -/
def close (h : state ≠ .closed := by decide) : SocketM state .closed Unit :=
  move (Network.Socket.close · h)

end SocketM
end Network.Socket
