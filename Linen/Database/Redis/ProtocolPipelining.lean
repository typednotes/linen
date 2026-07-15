/-
  Linen.Database.Redis.ProtocolPipelining — a request/response pipeline queue
  over a `ConnectionContext`

  Ported from `hedis`'s `Database.Redis.ProtocolPipelining`
  (https://raw.githubusercontent.com/informatikr/hedis/master/src/Database/Redis/ProtocolPipelining.hs).

  Upstream's own doc-comment describes its purpose precisely: "a module for
  automatic, optimal protocol pipelining" — writing many requests to the
  socket without waiting for each response, so that a `request`'s network
  round-trip cost is only paid once per *batch*, not once per request.

  ## Substitution: strict IO instead of `unsafeInterleaveIO`

  Upstream achieves *automatic* pipelining through Haskell's laziness:
  `connGetReplies` builds an infinite lazy list of `Reply` thunks via
  `unsafeInterleaveIO`, so `send` never blocks (it only appends to an
  in-memory output buffer) and the actual network read is deferred until a
  caller actually forces a `Reply` value by pattern-matching on it.

  Lean's `IO` is strict, so there is no way to build a "thunk that reads from
  the network when forced" without `unsafeInterleaveIO`'s escape hatch (which
  has no faithful, safe Lean equivalent, and using it would violate the
  spirit of a language with no unchecked laziness). Two of upstream's design
  choices are consequences purely of that laziness trick, and have no
  counterpart here:

  - `beginReceiving` (which seeds `connReplies`/`connPending` with the lazy
    list) becomes a no-op: there is no lazy list to seed.
  - the "limit pipeline length to 1000 to avoid thunk build-up" safeguard in
    upstream's `send` doesn't apply: there are no thunks to build up in
    strict IO, so nothing needs forcing.

  What remains faithful: `send` still writes to the socket without reading a
  reply, and `recv` still parses one reply off of a byte buffer that is
  filled on demand — genuine pipelining (`request₁; request₂; recv₁; recv₂`
  batches two round-trips into one) is preserved; only the *automatic*,
  laziness-driven scheduling of exactly when the network read happens is
  necessarily different (it happens eagerly inside `recv`, not the first time
  some later code forces a thunk).

  `connPendingCnt`'s "don't count negative" clamp (`max 0 (n-1)`) is upstream
  itself just simulating `Nat.sub`'s built-in saturation — ported here as
  plain `Nat` subtraction, which already saturates at `0`.
-/
import Linen.Database.Redis.ConnectionContext
import Linen.Database.Redis.Hooks
import Linen.Database.Redis.Protocol

namespace Database.Redis.ProtocolPipelining

open Database.Redis.Protocol (Reply)
open Database.Redis.Hooks (Hooks defaultHooks)

/-- A pipelined connection: a raw `ConnectionContext`, an input buffer of
    not-yet-parsed bytes received from the socket, a count of replies still
    owed by the server, and the instrumentation `Hooks` to apply around
    sends/receives. Mirrors upstream's `Connection` (minus the
    laziness-only `connReplies`/`connPending` fields; see the module
    doc-comment). -/
structure Connection where
  connCtx : Database.Redis.ConnectionContext.ConnectionContext
  connBuf : IO.Ref ByteArray
  connPendingCnt : IO.Ref Nat
  hooks : Hooks

/-- Wrap an already-connected `ConnectionContext`, using default hooks.
    Mirrors upstream's `fromCtx`. -/
def fromCtx (ctx : Database.Redis.ConnectionContext.ConnectionContext) : IO Connection := do
  let buf ← IO.mkRef ByteArray.empty
  let cnt ← IO.mkRef 0
  pure { connCtx := ctx, connBuf := buf, connPendingCnt := cnt, hooks := defaultHooks }

/-- Wrap an already-connected `ConnectionContext`, with custom hooks.
    Mirrors upstream's `fromCtxWithHooks`. -/
def fromCtxWithHooks (ctx : Database.Redis.ConnectionContext.ConnectionContext) (hooks : Hooks) :
    IO Connection := do
  let buf ← IO.mkRef ByteArray.empty
  let cnt ← IO.mkRef 0
  pure { connCtx := ctx, connBuf := buf, connPendingCnt := cnt, hooks := hooks }

/-- Connect (plain, non-TLS) and wrap the result, using default hooks.
    Mirrors the `Nothing`-TLS-params case of upstream's `connect`.

    (As in `Database.Redis.ConnectionContext`, TLS is a separate function
    rather than an `Option`-wrapped parameter, to avoid `TLSContext`'s
    universe-polymorphism from leaking an unresolved universe metavariable
    into call sites — see that module's doc-comment for the full
    explanation.) -/
def connect (addr : Database.Redis.ConnectionContext.ConnectAddr) : IO Connection := do
  let ctx ← Database.Redis.ConnectionContext.connect addr
  fromCtx ctx

/-- Connect (plain, non-TLS) and wrap the result, with custom hooks. Mirrors
    the `Nothing`-TLS-params case of upstream's `connectWithHooks`. -/
def connectWithHooks (addr : Database.Redis.ConnectionContext.ConnectAddr) (hooks : Hooks) :
    IO Connection := do
  let ctx ← Database.Redis.ConnectionContext.connect addr
  fromCtxWithHooks ctx hooks

/-- Connect over TLS and wrap the result, using default hooks. Mirrors the
    `Just tlsParams` case of upstream's `connect`. -/
def connectTLS (addr : Database.Redis.ConnectionContext.ConnectAddr)
    (ctx : Network.TLS.TLSContext) : IO Connection := do
  let cc ← Database.Redis.ConnectionContext.connectTLS addr ctx
  fromCtx cc

/-- Connect over TLS and wrap the result, with custom hooks. Mirrors the
    `Just tlsParams` case of upstream's `connectWithHooks`. -/
def connectTLSWithHooks (addr : Database.Redis.ConnectionContext.ConnectAddr)
    (ctx : Network.TLS.TLSContext) (hooks : Hooks) : IO Connection := do
  let cc ← Database.Redis.ConnectionContext.connectTLS addr ctx
  fromCtxWithHooks cc hooks

/-- A no-op: there is no lazy reply list to seed (see the module
    doc-comment). Kept only so callers porting upstream call sites of
    `beginReceiving` still have something to call. -/
def beginReceiving (_conn : Connection) : IO Unit :=
  pure ()

/-- Close the underlying connection. Mirrors upstream's `disconnect`. -/
def disconnect (conn : Connection) : IO Unit :=
  Database.Redis.ConnectionContext.disconnect conn.connCtx

/-- Flush the socket. Both of `ConnectionContext`'s backends write eagerly
    (see that module's doc-comment), so this simply forwards to its own
    no-op `flush`; kept for API-shape fidelity with upstream's `flush`. -/
def flush (conn : Connection) : IO Unit :=
  Database.Redis.ConnectionContext.flush conn.connCtx

/-- Write a request to the socket (through the `sendHook`), without waiting
    for a reply, and record that one more reply is now owed. Mirrors
    upstream's `send` (minus the thunk-build-up safeguard, which doesn't
    apply in strict IO — see the module doc-comment). -/
def send (conn : Connection) (bytes : ByteArray) : IO Unit := do
  conn.hooks.sendHook (Database.Redis.ConnectionContext.send conn.connCtx) bytes
  conn.connPendingCnt.modify (· + 1)

/-- Try to parse exactly one `Reply` off of `conn.connBuf`, reading more
    bytes from the socket on demand until either a full reply is parsed or
    the connection is closed.

    Termination: `while true do ...; unreachable!` — every branch either
    `return`s a parsed reply, `throw`s (protocol error or closed
    connection), or grows the buffer and loops again to retry the parse.
    There is no recursive call and thus nothing for the termination checker
    to prove; `unreachable!` is reached only along a path the type checker,
    not the termination checker, needs satisfied (mirroring the established
    pattern in `Linen.Network.Socket.Blocking`). This is not a fuel
    parameter dressed up as a loop: nothing bounds the number of iterations
    by construction, because none is needed — each iteration either
    terminates the loop or makes genuine progress by reading more bytes off
    a live socket. -/
private def parseOneReply (conn : Connection) : IO Reply := do
  while true do
    let buf ← conn.connBuf.get
    match Database.Redis.Protocol.reply buf.iter with
    | .success it r =>
      conn.connBuf.set (it.array.extract it.idx it.array.size)
      return r
    | .error _ .eof =>
      let chunk ← Database.Redis.ConnectionContext.recv conn.connCtx
      if chunk.isEmpty then
        throw (IO.userError "Redis.ProtocolPipelining: connection closed by peer")
      else
        conn.connBuf.set (buf ++ chunk)
    | .error _ (.other msg) =>
      throw (IO.userError s!"Redis.ProtocolPipelining: protocol error: {msg}")
  unreachable!

/-- Take the next reply owed by the server, blocking on the network as
    needed (through the `receiveHook`), and record that one fewer reply is
    now owed. Mirrors upstream's `recv`. -/
def recv (conn : Connection) : IO Reply :=
  conn.hooks.receiveHook do
    let r ← parseOneReply conn
    conn.connPendingCnt.modify (· - 1)
    pure r

/-- Send a request and receive its corresponding reply. Mirrors upstream's
    `request`. -/
def request (conn : Connection) (bytes : ByteArray) : IO Reply := do
  send conn bytes
  recv conn

end Database.Redis.ProtocolPipelining
