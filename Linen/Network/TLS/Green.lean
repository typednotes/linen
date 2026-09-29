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

/-- Wait on the dispatcher in the direction an outcome asks for, for at most
    `timeoutMillis` when given; `false` when that time ran out. -/
private def waitFor (disp : EventDispatcher) (sock : Socket state) (timeoutMillis : Option Nat) :
    TLSOutcome α → Green Bool
  | .wantWrite => match timeoutMillis with
    | some ms => disp.waitWritableFor sock ms
    | none => do disp.waitWritable sock; pure true
  | _ => match timeoutMillis with
    | some ms => disp.waitReadableFor sock ms
    | none => do disp.waitReadable sock; pure true

private def timedOut (what : String) (ms : Option Nat) : IO.Error :=
  IO.userError s!"TLS {what} timed out after {ms.getD 0}ms"

/-- Complete `session`'s handshake, suspending the green thread whenever it
    has to wait — each wait at most `timeoutMillis`, when given.
    $$\text{handshake} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \text{Green Unit}$$ -/
def handshake (disp : EventDispatcher) (sock : Socket state) (session : TLSSession)
    (timeoutMillis : Option Nat := none) : Green Unit := do
  repeat
    let outcome ← (handshakeNB session : IO _)
    match outcome with
    | .ok () => return
    | .error e => throw e
    | _ => unless ← waitFor disp sock timeoutMillis outcome do
        throw (timedOut "handshake" timeoutMillis)

/-- Accept a TLS connection on a connected socket: a server session and its
    handshake, on the dispatcher.
    $$\text{accept} : \text{EventDispatcher} \to \text{TLSContext} \to \text{Socket} \to \text{Green TLSSession}$$ -/
def accept (disp : EventDispatcher) (ctx : TLSContext) (sock : Socket .connected)
    (timeoutMillis : Option Nat := none) : Green TLSSession := do
  let session ← (newServerSession ctx sock.raw : IO _)
  handshake disp sock session timeoutMillis
  return session

/-- Open a TLS connection as a client (SNI and certificate name `hostname`).
    $$\text{connect} : \text{EventDispatcher} \to \text{TLSContext} \to \text{Socket} \to \text{String} \to \text{Green TLSSession}$$ -/
def connect (disp : EventDispatcher) (ctx : TLSContext) (sock : Socket .connected)
    (hostname : String) (timeoutMillis : Option Nat := none) : Green TLSSession := do
  let session ← (newClientSession ctx sock.raw hostname : IO _)
  handshake disp sock session timeoutMillis
  return session

/-- Read up to `maxLen` decrypted bytes: tried first, waiting only when
    OpenSSL has nothing and needs the socket. `some` bytes, `some` empty at
    end of input, `none` when a wait exceeded `timeoutMillis`.
    $$\text{readFor} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \text{Option}\ \mathbb{N} \to \text{Green (Option ByteArray)}$$ -/
def readFor (disp : EventDispatcher) (sock : Socket state) (session : TLSSession)
    (timeoutMillis : Option Nat) (maxLen : Nat := 16384) : Green (Option ByteArray) := do
  repeat
    let outcome ← (readNB session maxLen.toUSize : IO _)
    match outcome with
    | .ok bytes => return some bytes
    | .error e => throw e
    | _ => unless ← waitFor disp sock timeoutMillis outcome do return none
  return none

/-- `readFor` without a deadline. Empty at end of input.
    $$\text{read} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \mathbb{N} \to \text{Green ByteArray}$$ -/
def read (disp : EventDispatcher) (sock : Socket state) (session : TLSSession)
    (maxLen : Nat := 16384) : Green ByteArray := do
  return (← readFor disp sock session none maxLen).getD ByteArray.empty

/-- Write all of `data`, repeating the same write after each wait; throws
    when a wait exceeds `timeoutMillis`.
    $$\text{write} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \text{ByteArray} \to \text{Green Unit}$$ -/
def write (disp : EventDispatcher) (sock : Socket state) (session : TLSSession)
    (data : ByteArray) (timeoutMillis : Option Nat := none) : Green Unit := do
  repeat
    let outcome ← (writeNB session data : IO _)
    match outcome with
    | .ok () => return
    | .error e => throw e
    | _ => unless ← waitFor disp sock timeoutMillis outcome do
        throw (timedOut "write" timeoutMillis)

-- ── From `IO`, waiting on the dispatcher ──

/-- Wait from `IO` in the direction an outcome asks for (`awaitReadableFor`:
    `IO.wait` on the dispatcher's promise, which the task manager compensates
    for). -/
private def awaitFor (disp : EventDispatcher) (sock : Socket state) (timeoutMillis : Nat) :
    TLSOutcome α → IO Bool
  | .wantWrite => disp.awaitWritableFor sock timeoutMillis
  | _ => disp.awaitReadableFor sock timeoutMillis

/-- `readFor` for code that runs in `IO` — a request body read by an
    application — waiting through the dispatcher without holding a pool
    thread hostage. Throws when a wait exceeds `timeoutMillis`; empty at end
    of input.
    $$\text{readIO} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \mathbb{N} \to \text{IO ByteArray}$$ -/
def readIO (disp : EventDispatcher) (sock : Socket state) (session : TLSSession)
    (timeoutMillis : Nat) (maxLen : Nat := 16384) : IO ByteArray := do
  repeat
    let outcome ← readNB session maxLen.toUSize
    match outcome with
    | .ok bytes => return bytes
    | .error e => throw e
    | _ => unless ← awaitFor disp sock timeoutMillis outcome do
        throw (timedOut "read" (some timeoutMillis))
  return ByteArray.empty

/-- `write` for code that runs in `IO` (see `readIO`).
    $$\text{writeIO} : \text{EventDispatcher} \to \text{Socket} \to \text{TLSSession} \to \text{ByteArray} \to \mathbb{N} \to \text{IO Unit}$$ -/
def writeIO (disp : EventDispatcher) (sock : Socket state) (session : TLSSession)
    (data : ByteArray) (timeoutMillis : Nat) : IO Unit := do
  repeat
    let outcome ← writeNB session data
    match outcome with
    | .ok () => return
    | .error e => throw e
    | _ => unless ← awaitFor disp sock timeoutMillis outcome do
        throw (timedOut "write" (some timeoutMillis))

end Network.TLS.Green
