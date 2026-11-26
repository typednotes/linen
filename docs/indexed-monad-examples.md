# Indexed monads by example

An indexed program declares its input and output state. The compiler checks
that the output of one action matches the input of the next. An interpreter
runs the program and owns its resources. When an I/O result changes the state,
a dependent bind gives each branch the state selected by that result.

The examples below use that pattern at three levels: define a small
interpreter, use an existing scope, and nest one resource inside another.
Each Lean block is a standalone module. To check one, copy it to
`Example.lean` and run `lake env lean Example.lean` after building Linen.
The functions that use native resources can run from a normally linked Lake
executable. See the [native linking guide](linking.md) for using those same
functions in `#eval`.

The [resource lifecycle guide](resource-lifecycles.md) describes the ownership
boundary and lists the module contracts. These examples follow the approach
of the [Idris socket tutorial](https://docs.idris-lang.org/en/latest/st/examples.html):
declare the legal transitions, write the program, then interpret it.

## Build an indexed program

A one-slot mailbox starts empty. Putting a value makes it full. Taking the
value makes it empty. There is no input value for an empty mailbox beyond
`Unit`, and a full one contains a `Nat`. This toy interpreter uses `Id` and
grants access with `True`. A resource library would keep its access proof and
interpreter state private.

~~~lean
import Linen.Control.Resource

open Control.Resource

namespace MailboxExample

inductive State where
  | empty
  | full

def World : State → Type
  | .empty => Unit
  | .full => Nat

def put (n : Nat) : ActionT Id True World .empty .full Unit :=
  ProgramT.step trivial fun () => ⟨(), n⟩

def take : ActionT Id True World .full .empty Nat :=
  ProgramT.step trivial fun n => ⟨n, ()⟩

def roundTrip (n : Nat) : Nat :=
  (ProgramT.run trivial (ProgramT.ibind (put n) fun () => take) ()).1

#guard roundTrip 42 == 42

end MailboxExample
~~~

Putting a second value immediately after `put` fails because it requires an
empty slot. `ActionT` is the fixed-output form of `ProgramT`. `Action`
and `Program` specialize them to `IO`. The `Owned` adapter below underlies
the open/closed APIs. Its runner can finish with the resource still open
and release it on exit. An explicit close consumes the open state earlier.

## Serve one byte with SocketM

The listener goes from fresh to bound to listening. Accept creates a child
scope that starts connected, while the listener stays listening. The child
tries one receive after polling. Data and backpressure leave it connected.
EOF and receive errors make `recvOrClose` close the socket before calling
the continuation. That continuation can return or report the error.

~~~lean
import Linen.Network.Socket.Scoped

open Control.Resource Network.Socket

namespace SocketExample

def echoOnce : SocketM .connected .closed Unit :=
  Program.ibind (SocketM.poll .read 2000) fun readiness =>
  match readiness with
  | .timeout => SocketM.close
  | .error error =>
    Program.ibind (SocketM.close (state := .connected)) fun () =>
    Program.lift (throw error)
  | .ready =>
    Program.bind (SocketM.recvOrClose 1) fun outcome =>
    match outcome with
    | .data bytes =>
      Program.ibind (SocketM.sendAll bytes 2000) fun () => SocketM.close
    | .wouldBlock => SocketM.close
    | .eof => Program.pure ()
    | .error error => Program.lift (throw error)

def client (address : SockAddr) : IO ByteArray :=
  SocketM.run .inet .stream <|
    Program.ibind (SocketM.connect address) fun () =>
    Program.ibind (SocketM.sendAll "x".toUTF8 2000) fun () =>
    Program.ibind (SocketM.recvBlocking 1 2000) fun bytes =>
    Program.map (fun () => bytes) SocketM.close

def roundTrip : IO ByteArray :=
  SocketM.run .inet .stream <|
    Program.ibind (SocketM.bindAddress ⟨"127.0.0.1", 0⟩) fun () =>
    Program.ibind SocketM.listen fun () =>
    Program.ibind (SocketM.setNonBlocking (state := .listening)) fun () =>
    Program.ibind (SocketM.localAddress (state := .listening)) fun address =>
    Program.ibind (Program.lift (IO.asTask (prio := .dedicated) (client address))) fun task =>
    Program.ibind (SocketM.withAccepted (fun _ => echoOnce) 2000) fun () =>
    Program.ibind (Program.lift (IO.ofExcept task.get)) fun bytes =>
    Program.map (fun () => bytes) SocketM.close

end SocketExample
~~~

`SocketExample.roundTrip` returns the byte `x` over loopback. The type
behind `recvOrClose` is `SocketProgram`, whose result chooses the next
state. `connectNB` uses it too. Its `.inProgress` branch can poll and
call `connectFinish`. That branch cannot send yet. If a protocol needs to
write after the peer ends its send side, use `recv` and choose the shutdown
policy in the application.

To run the loopback example in this repo, save the block as
`LinenTest/SocketTutorial.lean` and add
`#eval String.fromUTF8! <$> SocketExample.roundTrip` at the end. Run
`lake build --wfail +LinenTest.SocketTutorial` to see `"x"`. Lake links
the native libraries and evaluates the example as part of the build.

## Nest a TLS session with TLSM

A TCP socket is connected before the TLS handshake starts. The TLS scope begins
in `.open_` after a successful handshake. This example writes a payload,
reads one chunk, and closes TLS before closing TCP. The supplied context
determines which certificates are trusted, and the hostname is passed to
the client handshake.

~~~lean
import Linen.Network.Socket.Scoped

open Control.Resource Network.Socket Network.TLS

namespace TLSExample

def exchange (ctx : TLSContext) (host : String) (addr : SockAddr)
    (message : ByteArray) : IO ByteArray :=
  SocketM.run .inet .stream <|
    Program.ibind (SocketM.connect addr) fun () =>
    Program.ibind (SocketM.withClientTLS ctx host <|
      Program.ibind (do
        TLSM.write message
        TLSM.read 4096) fun bytes =>
      Program.map (fun () => bytes) TLSM.close) fun bytes =>
    Program.map (fun () => bytes) SocketM.close

end TLSExample
~~~

`TLSM.write` and `TLSM.read` preserve `.open_`, so they fit in an ordinary
`do` block. After `TLSM.close`, Lean rejects either action. The example
reads one chunk from a service that speaks the caller's protocol. A protocol
with its own framing needs to keep reading until its message is complete.

## Send before receiving with ExchangeM

An HTTP/1.1 exchange starts ready to send its request. Sending moves it to
`.awaitingResponse`. Receiving consumes that phase and finishes the exchange.
The runner closes the transport when the program returns or throws.

~~~lean
import Linen.Network.HTTP.Client.Scoped

open Control.Resource Network.HTTP.Client

namespace HTTPExample

def requestWithTrace (request : Request) (onSent : IO Unit) : IO Response :=
  ExchangeM.run request <|
    Program.ibind ExchangeM.send fun () =>
    Program.ibind (Program.lift onSent) fun () =>
    ExchangeM.receive

end HTTPExample
~~~

The callback records that the request was sent. `Program.lift` preserves the
current exchange state, so the next action still receives a response. Lean
rejects swapping `send` and `receive` or sending a second time.

## Observe a peer close with WebSocketM

A WebSocket can send data while open. Sending a close frame moves it to
`.closing`, where it can still receive the peer's reply. `WebSocketProgram`
expresses both possible results here. Data leaves the program closing.
An observed close frame moves it to `.closed`.

~~~lean
import Linen.Network.WebSockets.Scoped

open Control.Resource Network.WebSockets

namespace WebSocketExample

def sayGoodbye : WebSocketProgram .open_ Incoming (Incoming.state .closing) :=
  Program.bind (WebSocketM.sendText "goodbye") fun () =>
  Program.bind WebSocketM.sendClose fun () =>
  WebSocketM.receive (state := .closing)

def observeClose : IO Bool := do
  let frame : Frame := ⟨true, .close, none, ByteArray.empty⟩
  let result ← WebSocketM.runTransport (fun _ => pure ()) (pure frame.encode) sayGoodbye
  return match result with
    | .closed => true
    | .data _ => false

end WebSocketExample
~~~

`observeClose` uses in-memory transport callbacks and returns `true`.
A real client can pass `sayGoodbye` to `WebSocketM.runClient`, which owns
the TCP transport. A server using `runTransport` supplies its own cleanup.
Receiving one frame demonstrates the state change. A complete close
handshake may require reading more than one frame.

## Send one web response with AppM

`Application` expects an `AppM .pending .sent ResponseReceived`. A handler can
make a decision in IO before it responds, provided every successfully
returning branch goes from pending to sent.

~~~lean
import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Status

open Network.WebApp Network.HTTP.Types

namespace WebAppExample

def health (isReady : IO Bool) : Application :=
  fun _req respond =>
    AppM.ioThen isReady fun ready =>
      if ready then
        AppM.respond respond (responseLBS status200 [] "ready")
      else
        AppM.respond respond (responseLBS status503 [] "starting")

end WebAppExample
~~~

Replacing either branch with `AppM.ipure` leaves the state pending. Sequencing
two calls to `AppM.respond` tries to start the second from sent. Lean rejects
both through the indexed API. The server supplies `respond` and handles
transport failure. Middleware that uses the explicit framework escape hatch
takes responsibility for the response protocol.

## Step a SQLite cursor with ConnectionM and StatementM

The connection stays open while a child statement works through its own
states. Bind parameters before stepping. `StatementProgram` gives the
`.row` branch access to column data and the `.done` branch no current row.
Both branches finalize the statement before returning to the connection.

~~~lean
import Linen.Database.SQLite.Scoped

open Control.Resource Database.SQLite.Simple

namespace SQLiteExample

def readFirst : StatementM .ready .closed (Array Database.SQLite3.SQLData) :=
  Program.ibind (StatementM.bindParams #[.integer 42]) fun () =>
  Program.bind StatementM.step fun outcome =>
    match outcome with
    | .row =>
      Program.ibind StatementM.columns fun row =>
      Program.map (fun () => row) StatementM.finalize
    | .done => Program.map (fun () => #[]) StatementM.finalize

def run : IO (Array Database.SQLite3.SQLData) :=
  ConnectionM.run ":memory:" <|
    Program.ibind (ConnectionM.withStatement "SELECT ?" readFirst) fun row =>
    Program.map (fun () => row) ConnectionM.close

end SQLiteExample
~~~

The result is `#[.integer 42]`. Reading columns in the done branch fails to
type-check. If an operation throws, the statement scope still finalizes
before the connection closes.

## Own DuckDB appenders and files

An open DuckDB connection can own a child appender. The example creates a
table, appends a complete row inside a transaction, closes the appender, and
then queries the table. The explicit close flushes rows before the query.

~~~lean
import Linen.Database.DuckDB.Scoped

open Control.Resource Database.DuckDB.Simple

namespace DuckDBExample

def appendOne : IO (Array Int32) :=
  ConnectionM.run none do
    ConnectionM.execute_ "CREATE TABLE samples (n INTEGER)"
    ConnectionM.transaction <|
      ConnectionM.withAppender "samples" <|
        Program.ibind (AppenderM.appendRow #[.int32 42]) fun () =>
        AppenderM.close
    let rows : Array (Only Int32) ← ConnectionM.query_ "SELECT n FROM samples"
    return rows.map (·.fromOnly)

def readPrefix (path : String) : IO ByteArray :=
  ConnectionM.run none <|
    ConnectionM.withFile path #[.read] do
      FileM.seek 0
      FileM.read 4

end DuckDBExample
~~~

`appendOne` returns `#[42]`. `AppenderM.appendRow` requires an open
appender, so calling it after close is a type error. `FileM` has one usable
phase. The file opens for the child action and closes before returning
control to the connection. `readPrefix` reads up to four bytes from an
existing file.

## Run a typed PostgreSQL statement

A scoped PostgreSQL connection can execute a typed statement without
exposing the underlying connection. The runner returns acquisition errors
separately from statement errors. A pool offers the same statement through
a borrowed connection and returns that connection before the next action.

~~~lean
import Linen.Database.SQL.Scoped

open Control.Resource Database.SQL

namespace PostgreSQLExample

def answer : Statement.Statement Unit Int :=
  { sql := "SELECT 42"
    encode := Encoders.Params.none
    decode := Decoders.Result.singleRow (Decoders.Row.column Decoders.Value.int) }

def onConnection (settings : Connection.Settings) :
    IO (Except Connection.ConnectionError (Except Session.SessionError Int)) :=
  ConnectionM.run settings <|
    Program.ibind (ConnectionM.statement answer ()) fun result =>
    Program.map (fun () => result) ConnectionM.close

def throughPool (settings : Pool.PoolSettings) : IO (Except Pool.PoolError Int) :=
  PoolM.run settings <|
    Program.ibind (PoolM.statement answer ()) fun result =>
    Program.map (fun () => result) PoolM.close

end PostgreSQLExample
~~~

With a reachable PostgreSQL server and valid settings, the query decodes
one integer. `ConnectionM.close` consumes the open state of a connection.
`PoolM.close` does the same for the pool. Each result can leave its scope
without retaining a usable database handle.

## Send a Redis command in an owned pool

A scoped Redis command takes arguments and returns an existing Redis
decoder's result. The Redis environment and pool stay private. A second
command after explicit close fails to type-check.

~~~lean
import Linen.Database.Redis.Scoped

open Control.Resource Database.Redis

namespace RedisExample

def ping (settings : Connection.ConnectInfo) :
    IO (Except Protocol.Reply ByteArray) :=
  ConnectionM.run settings <|
    Program.ibind (ConnectionM.command ["PING".toUTF8]) fun reply =>
    Program.map (fun () => reply) ConnectionM.close

end RedisExample
~~~

Pass settings for a running Redis server to get `.ok "PONG".toUTF8`.
The return type keeps Redis reply errors, and connection I/O can still
throw. Use `ConnectionM.runTLS` with an appropriate TLS context for
a server that requires it.

## Exchange a line with WorkerM

The interpreter spawns the process, then runs requests while the worker
is open. Validate the line before spawning. The example uses `cat` as a
worker that echoes each line and explicitly stops it after one reply.

~~~lean
import Linen.System.Worker

open Control.Resource System.Worker

namespace WorkerExample

def echo (text : String) : IO String := do
  let line ← IO.ofExcept ((Line.check text).mapError IO.userError)
  WorkerM.run "cat" <|
    Program.ibind (WorkerM.call line 2000) fun reply =>
    Program.map (fun () => reply) WorkerM.stop

end WorkerExample
~~~

`WorkerExample.echo "hello"` returns `"hello"` on systems with `cat`
on `PATH`. Lean rejects a call in the closed state. A timeout or protocol
failure can abort an open worker at runtime, in which case the interpreter
reaps it.
