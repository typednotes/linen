/-
  Tests for `Linen.Database.Redis.Transactions`.

  The `Queued` `Functor`/`Applicative`/`Monad` instances are pure and tested
  with `#guard` (they need no connection — just a reply vector fed to
  `runQueued`). `multiExec` needs a live `Redis` connection, so it is run via
  `runRedisInternal` against a loopback TCP "server" that replies with a
  scripted sequence of RESP frames (one reply per request), following the
  `#eval` loopback pattern of `CoreTest`/`CommandsTest`. The three
  `TxResult` shapes are covered: `TxSuccess` (two `get`s combined via `<*>`),
  `TxAborted` (a `WATCH`-aborted transaction, surfaced by `EXEC` returning a
  null multi-bulk `*-1\r\n`), and `TxError` (a command returning an error
  reply inside the `EXEC` multi-bulk).
-/
import Linen.Database.Redis.Transactions
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.Transactions
open Database.Redis.Core (Redis runRedisInternal)
open Database.Redis.Protocol (Reply)
open Database.Redis.Types (Status)

namespace Tests.Database.Redis.Transactions

/-! ### Compile-time: public API shape -/

example : List ByteArray → Redis (Except Reply Status) := watch
example : Redis (Except Reply Status) := unwatch
example {α : Type} : RedisTx (Queued α) → Redis (TxResult α) := multiExec

/-! ### Pure: `Queued`'s `Functor`/`Applicative`/`Monad` instances -/

-- `Functor`: `fmap` over a pure `Queued`.
#guard match (((· + 1) <$> (pure 3 : Queued Nat)).runQueued #[]) with
  | .ok 4 => true | _ => false

-- `Applicative`: `<*>` runs both decoders against the same reply vector and
-- applies one result to the other — exactly how a caller combines two queued
-- command results into a tuple.
#guard match ((Prod.mk <$> (pure 1 : Queued Nat) <*> pure 2).runQueued #[]) with
  | .ok (1, 2) => true | _ => false

-- `Monad`: `>>=` threads the reply vector through both steps.
#guard match (((pure 3 : Queued Nat) >>= fun x => pure (x * 2)).runQueued #[]) with
  | .ok 6 => true | _ => false

-- A short `EXEC` result vector fails safely (out-of-range index → error),
-- rather than crashing as upstream's partial `(! i)` would.
#guard match (Queued.mk (fun rs =>
    match rs[5]? with
    | some r => (Except.ok r : Except Reply Reply)
    | none => Except.error (Reply.error "short".toUTF8))).runQueued #[] with
  | .error (.error _) => true | _ => false

/-! ### Runtime helper: script a loopback server's replies -/

/-- Run `action` against a loopback server that answers each request it reads
    with the next reply from `replies`, in order. -/
def runTx (replies : List ByteArray) (action : Redis α) : IO α := do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    for rep in replies do
      let _ ← Network.Socket.Blocking.recv accepted 4096
      Network.Socket.sendAll accepted rep
    Network.Socket.close accepted
  let conn ← Database.Redis.ProtocolPipelining.connect (.hostPort addr.host addr.port)
  let result ← runRedisInternal conn action
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  Database.Redis.ProtocolPipelining.disconnect conn
  let _ ← Network.Socket.close server
  unless done do throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure result

/-! ### `multiExec`: the three `TxResult` outcomes -/

-- `TxSuccess`: two queued `get`s combined via `Queued`'s `<*>`, decoding a
-- well-typed tuple from the `EXEC` multi-bulk (`MULTI` → +OK, each `GET` →
-- +QUEUED, `EXEC` → the two results).
#eval show IO Unit from do
  let result ← runTx
    ["+OK\r\n".toUTF8, "+QUEUED\r\n".toUTF8, "+QUEUED\r\n".toUTF8,
     "*2\r\n$2\r\nv1\r\n$2\r\nv2\r\n".toUTF8]
    (multiExec do
      let a ← Database.Redis.Commands.get "k1".toUTF8
      let b ← Database.Redis.Commands.get "k2".toUTF8
      pure (Prod.mk <$> a <*> b))
  match result with
  | .success (some v1, some v2) =>
    unless v1 == "v1".toUTF8 ∧ v2 == "v2".toUTF8 do
      throw (IO.userError "TxSuccess decoded the wrong values")
  | _ => throw (IO.userError "expected TxSuccess with two decoded values")

-- `TxAborted`: a `WATCH`-aborted transaction — `EXEC` replies with a null
-- multi-bulk (`*-1\r\n`).
#eval show IO Unit from do
  let result ← runTx
    ["+OK\r\n".toUTF8, "+QUEUED\r\n".toUTF8, "*-1\r\n".toUTF8]
    (multiExec (do
      let a ← Database.Redis.Commands.get "k".toUTF8
      pure a))
  match result with
  | .aborted => pure ()
  | _ => throw (IO.userError "expected TxAborted for a null-multi-bulk EXEC reply")

-- `TxError`: a command inside the transaction returns an error reply, so the
-- collected result fails to decode.
#eval show IO Unit from do
  let result ← runTx
    ["+OK\r\n".toUTF8, "+QUEUED\r\n".toUTF8, "*1\r\n-ERR boom\r\n".toUTF8]
    (multiExec (do
      let a ← Database.Redis.Commands.get "k".toUTF8
      pure a))
  match result with
  | .error msg =>
    unless (msg.splitOn "boom").length > 1 do
      throw (IO.userError s!"TxError message did not mention the failing reply: {msg}")
  | _ => throw (IO.userError "expected TxError for an error reply inside EXEC")

end Tests.Database.Redis.Transactions
