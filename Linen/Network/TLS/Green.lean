/-
  Linen.Network.TLS.Green — TLS on green threads

  Drives the non-blocking TLS primitives of `Network.TLS.Context`
  (`handshakeNB`, `readNB`, `writeNB`) on the `EventDispatcher`, so a TLS
  connection waits by suspending its green thread instead of blocking a
  pool thread.

  ## Design, and where it comes from

  The shape is that of rust-openssl + tokio-openssl and of HsOpenSSL on
  GHC's IO manager:

  - **One session, stepped.** The session is created once and the handshake
    stepped on it (`MidHandshakeSslStream::handshake`, HsOpenSSL's
    `sslBlock tryAccept`); a would-block result waits and retries the *same*
    session.
  - **Try, then wait.** Every operation is attempted *before* waiting for
    the socket. For reads this is not an optimisation: OpenSSL may already
    hold decrypted bytes (`SSL_pending`) that the socket will never signal
    as readable (tokio-openssl's `poll_read` calls `SSL_read` first for the
    same reason).
  - **Wait in the direction asked.** Any call can want either direction —
    a read can need to write (e.g. a TLS 1.3 key update), a write to read —
    so the wait follows the outcome, not the operation.
  - **Repeat the same write.** A write that would block is retried with the
    same bytes; nothing else writes in between, since each call runs to
    completion on its green thread (the caller must not write concurrently
    on one session, as with any `SSL*`).

  ## No `partial`

  Each loop is a `repeat` whose body `return`s on every terminating outcome.
-/
import Linen.Network.TLS.Context
import Linen.Network.Socket.EventDispatcher

namespace Network.TLS.Green

open Network.Socket
open Control.Concurrent.Green (Green)

/-- Wait on the dispatcher in the direction an outcome asks for. -/
private def waitFor (disp : EventDispatcher) (sock : Socket state) :
    TLSOutcome α → Green Unit
  | .wantWrite => disp.waitWritable sock
  | _ => disp.waitReadable sock

/-- Complete `session`'s handshake, suspending the green thread whenever it
    has to wait.
    $$\text{handshake} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \text{Green Unit}$$ -/
def handshake (disp : EventDispatcher) (sock : Socket state) (session : TLSSession) :
    Green Unit := do
  repeat
    let outcome ← (handshakeNB session : IO _)
    match outcome with
    | .ok () => return
    | .error e => throw e
    | _ => waitFor disp sock outcome

/-- Accept a TLS connection on a connected socket: a server session and its
    handshake, on the dispatcher.
    $$\text{accept} : \text{EventDispatcher} \to \text{TLSContext} \to \text{Socket} \to \text{Green TLSSession}$$ -/
def accept (disp : EventDispatcher) (ctx : TLSContext) (sock : Socket .connected) :
    Green TLSSession := do
  let session ← (newServerSession ctx sock.raw : IO _)
  handshake disp sock session
  return session

/-- Open a TLS connection as a client (SNI and certificate name `hostname`).
    $$\text{connect} : \text{EventDispatcher} \to \text{TLSContext} \to \text{Socket} \to \text{String} \to \text{Green TLSSession}$$ -/
def connect (disp : EventDispatcher) (ctx : TLSContext) (sock : Socket .connected)
    (hostname : String) : Green TLSSession := do
  let session ← (newClientSession ctx sock.raw hostname : IO _)
  handshake disp sock session
  return session

/-- Read up to `maxLen` decrypted bytes: tried first, waiting only when
    OpenSSL has nothing and needs the socket. Empty at end of input.
    $$\text{read} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \mathbb{N} \to \text{Green ByteArray}$$ -/
def read (disp : EventDispatcher) (sock : Socket state) (session : TLSSession)
    (maxLen : Nat := 16384) : Green ByteArray := do
  repeat
    let outcome ← (readNB session maxLen.toUSize : IO _)
    match outcome with
    | .ok bytes => return bytes
    | .error e => throw e
    | _ => waitFor disp sock outcome
  return ByteArray.empty

/-- Write all of `data`, repeating the same write after each wait.
    $$\text{write} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \text{ByteArray} \to \text{Green Unit}$$ -/
def write (disp : EventDispatcher) (sock : Socket state) (session : TLSSession)
    (data : ByteArray) : Green Unit := do
  repeat
    let outcome ← (writeNB session data : IO _)
    match outcome with
    | .ok () => return
    | .error e => throw e
    | _ => waitFor disp sock outcome

end Network.TLS.Green
