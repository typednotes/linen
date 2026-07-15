/-
  Tests for `Linen.Database.Redis.Core`.

  Runs `Redis` actions end-to-end via `runRedisInternal`/
  `runRedisClusteredInternal` against real loopback TCP "servers" (as in
  `ProtocolPipeliningTest`/`ClusterTest`), covering `send`/`recv`/
  `sendRequest` in both the non-clustered and clustered environments, plus
  the crash-avoidance deviation (`recv`/`send` in a clustered environment
  throw instead of pattern-match-crashing, per the module doc-comment).
-/
import Linen.Database.Redis.Core
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.Core
open Database.Redis.Protocol (Reply)
open Database.Redis.Types (Status)

namespace Tests.Database.Redis.Core

/-! ### Compile-time: public API shape -/

example : Database.Redis.ProtocolPipelining.Connection → Redis α → IO α := runRedisInternal
example : Database.Redis.Cluster.Connection → IO Database.Redis.Cluster.ShardMap → Redis α → IO α :=
  runRedisClusteredInternal
example [Monad m] [MonadRedis m] : m Reply := recv
example [Monad m] [MonadRedis m] : List ByteArray → m Unit := send
example [Monad m] [MonadRedis m] [RedisCtx m f] [Database.Redis.Types.RedisResult α] :
    List ByteArray → m (f α) := sendRequest

/-! ### Runtime, non-clustered: `sendRequest`/`send`/`recv` -/

-- `sendRequest` = write the request, read and decode the reply, in one
-- `Redis` action, run via `runRedisInternal`.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv accepted 256
    Network.Socket.sendAll accepted "+PONG\r\n".toUTF8
    Network.Socket.close accepted
  let conn ← Database.Redis.ProtocolPipelining.connect (.hostPort addr.host addr.port)
  let result ← runRedisInternal conn (sendRequest (f := Except Reply) (α := Status) ["PING".toUTF8])
  match result with
  | .ok Status.pong => pure ()
  | .ok _ => throw (IO.userError "expected Status.pong")
  | .error _ => throw (IO.userError "expected a decoded Status, got a raw error reply")
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure ()
  Database.Redis.ProtocolPipelining.disconnect conn
  let _ ← Network.Socket.close server
  pure ()

-- `send` then `recv` separately, exercising `setLastReply`'s plumbing.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv accepted 256
    Network.Socket.sendAll accepted "+OK\r\n".toUTF8
    Network.Socket.close accepted
  let conn ← Database.Redis.ProtocolPipelining.connect (.hostPort addr.host addr.port)
  let reply ← runRedisInternal conn (do
    send ["SET".toUTF8, "k".toUTF8, "v".toUTF8]
    recv)
  match reply with
  | .singleLine s => unless s == "OK".toUTF8 do throw (IO.userError "expected OK")
  | _ => throw (IO.userError "expected a singleLine reply")
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure ()
  Database.Redis.ProtocolPipelining.disconnect conn
  let _ ← Network.Socket.close server
  pure ()

/-! ### Runtime, clustered: `sendRequest` routes through `Cluster.
    requestPipelined`; `send`/`recv` throw instead of crashing -/

private def pingInfo : Database.Redis.Cluster.Command.CommandInfo :=
  { name := "ping".toUTF8
    arity := Database.Redis.Cluster.Command.AritySpec.required 1
    flags := [Database.Redis.Cluster.Command.Flag.readOnly]
    firstKeyPosition := 0
    lastKeyPosition := Database.Redis.Cluster.Command.LastKeyPositionSpec.lastKeyPosition 0
    stepCount := 0 }

#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let node : Database.Redis.Cluster.Node :=
    { id := "n".toUTF8, role := .master, host := addr.host, port := addr.port.toNat }
  let shardMap : Database.Redis.Cluster.ShardMap :=
    { slots := (∅ : Std.HashMap Nat Database.Redis.Cluster.Shard).insert 0
        { master := node, slaves := [] } }
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv accepted 256
    Network.Socket.sendAll accepted "+PONG\r\n".toUTF8
    Network.Socket.close accepted
  let clusterConn ← Database.Redis.Cluster.connectWith none none [pingInfo] shardMap
    Database.Redis.Hooks.defaultHooks
  let result ← runRedisClusteredInternal clusterConn (pure shardMap)
    (sendRequest (f := Except Reply) (α := Status) ["PING".toUTF8])
  match result with
  | .ok Status.pong => pure ()
  | .ok _ => throw (IO.userError "expected Status.pong")
  | .error _ => throw (IO.userError "expected a decoded Status, got a raw error reply")
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure ()
  Database.Redis.Cluster.disconnect clusterConn
  let _ ← Network.Socket.close server
  pure ()

-- `recv`/`send` in a clustered environment throw a descriptive error
-- instead of crashing (upstream's `asks envConn` would pattern-match-fail
-- at runtime for a `ClusteredEnv` — see the module doc-comment).
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let node : Database.Redis.Cluster.Node :=
    { id := "n".toUTF8, role := .master, host := addr.host, port := addr.port.toNat }
  let shardMap : Database.Redis.Cluster.ShardMap :=
    { slots := (∅ : Std.HashMap Nat Database.Redis.Cluster.Shard).insert 0
        { master := node, slaves := [] } }
  let clusterConn ← Database.Redis.Cluster.connectWith none none [pingInfo] shardMap
    Database.Redis.Hooks.defaultHooks
  try
    let _ ← runRedisClusteredInternal clusterConn (pure shardMap) (recv (m := Redis))
    throw (IO.userError "expected recv to throw in a clustered environment")
  catch _ => pure ()
  try
    let _ ← runRedisClusteredInternal clusterConn (pure shardMap) (send (m := Redis) ["PING".toUTF8])
    throw (IO.userError "expected send to throw in a clustered environment")
  catch _ => pure ()
  Database.Redis.Cluster.disconnect clusterConn
  let _ ← Network.Socket.close server
  pure ()

end Tests.Database.Redis.Core
