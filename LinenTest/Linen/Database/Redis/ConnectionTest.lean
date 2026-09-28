/-
  Tests for `Linen.Database.Redis.Connection`.

  `ConnectInfo`/`defaultConnectInfo` and `shardMapFromClusterSlotsResponse`
  are pure and checked with `#guard`. The generic connection `Pool` is
  exercised with a trivial `Nat` resource (create = bump a counter), so its
  acquire/reuse/cap/release logic is tested without a network. `connect`/
  `runRedis` are then run end-to-end against a canned RESP2 loopback server,
  mirroring `ProtocolPipeliningTest`.
-/
import Linen.Database.Redis.Connection
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.Connection
open Database.Redis.Core (Redis)
open Database.Redis.Protocol (Reply)

namespace Tests.Database.Redis.Connection

/-! ### Compile-time: public API shape -/

example : ConnectInfo → IO Connection := connect
example : ConnectInfo → IO Connection := checkedConnect
example : ConnectInfo → IO Connection := connectCluster
example : ConnectInfo → IO Connection := checkedConnectCluster
example : Connection → IO Unit := disconnect
example {α} : Connection → Redis α → IO α := runRedis
example {α} : Connection → Redis α → IO (Option α) := runRedisNonBlocking
example : Database.Redis.ManualCommands.ClusterSlotsResponse → Database.Redis.Cluster.ShardMap :=
  shardMapFromClusterSlotsResponse
example : Database.Redis.Cluster.Connection → IO Database.Redis.Cluster.ShardMap := refreshShardMap

/-! ### defaultConnectInfo -/

example : defaultConnectInfo.connectAddr = .hostPort "localhost" 6379 := rfl
#guard defaultConnectInfo.connectDatabase == 0
#guard defaultConnectInfo.connectMaxConnections == 50
#guard defaultConnectInfo.connectNumStripes == some 1
#guard defaultConnectInfo.connectMaxIdleTime == 30
#guard defaultConnectInfo.connectTimeout == none
#guard defaultConnectInfo.connectAuth == none
#guard defaultConnectInfo.connectUsername == none
#guard defaultConnectInfo.connectPoolLabel == ""

/-! ### shardMapFromClusterSlotsResponse -/

private def sampleSlots : Database.Redis.ManualCommands.ClusterSlotsResponse :=
  { entries :=
      [ { startSlot := 0, endSlot := 2
        , master := { nodeIP := "127.0.0.1".toUTF8, nodePort := 7000, nodeID := "master1".toUTF8 }
        , replicas := [ { nodeIP := "127.0.0.2".toUTF8, nodePort := 7001, nodeID := "slave1".toUTF8 } ] } ] }

-- Slots 0..2 all map to the shard.
#guard (shardMapFromClusterSlotsResponse sampleSlots).slots.size == 3
#guard (shardMapFromClusterSlotsResponse sampleSlots).slots.contains 0
#guard (shardMapFromClusterSlotsResponse sampleSlots).slots.contains 2
#guard !(shardMapFromClusterSlotsResponse sampleSlots).slots.contains 3
-- The master node decodes correctly (IP, port, role).
#guard ((shardMapFromClusterSlotsResponse sampleSlots).slots.get? 0).map (·.master.host) == some "127.0.0.1"
#guard ((shardMapFromClusterSlotsResponse sampleSlots).slots.get? 0).map (·.master.port) == some 7000
#guard ((shardMapFromClusterSlotsResponse sampleSlots).slots.get? 0).map (·.slaves.length) == some 1

/-! ### Generic connection pool -/

-- Acquire creates a resource, `withResource` returns it, stats reflect it.
#eval show IO Unit from do
  let counter ← IO.mkRef 0
  let cfg : PoolConfig Nat :=
    defaultPoolConfig (do counter.modify (· + 1); counter.get) (fun _ => pure ()) 30 2
  let pool ← newPool cfg
  let n1 ← pool.withResource (fun n => pure n)
  unless n1 == 1 do throw (IO.userError s!"expected first resource 1, got {n1}")
  let (idle, inUse, total) ← pool.stats
  unless idle == 1 && inUse == 0 && total == 1 do
    throw (IO.userError s!"expected (1,0,1), got ({idle},{inUse},{total})")
  -- Second use reuses the idle resource — no new creation.
  let n2 ← pool.withResource (fun n => pure n)
  unless n2 == 1 do throw (IO.userError s!"expected reused resource 1, got {n2}")
  let (_, _, total2) ← pool.stats
  unless total2 == 1 do throw (IO.userError s!"expected total still 1, got {total2}")

-- `tryWithResource` returns `none` when the pool is at capacity.
#eval show IO Unit from do
  let counter ← IO.mkRef 0
  let cfg : PoolConfig Nat :=
    defaultPoolConfig (do counter.modify (· + 1); counter.get) (fun _ => pure ()) 30 1
  let pool ← newPool cfg
  -- Hold the single slot, then a nested tryWithResource must fail (capacity 1).
  let inner ← pool.withResource fun _ => pool.tryWithResource (fun _ => pure 99)
  unless inner == none do throw (IO.userError "expected tryWithResource to fail at capacity")

-- `destroyAllResources` frees idle resources (destroy bumps a counter).
#eval show IO Unit from do
  let created ← IO.mkRef 0
  let destroyed ← IO.mkRef 0
  let cfg : PoolConfig Nat :=
    defaultPoolConfig (do created.modify (· + 1); created.get)
      (fun _ => destroyed.modify (· + 1)) 30 3
  let pool ← newPool cfg
  let _ ← pool.withResource (fun n => pure n)
  pool.destroyAllResources
  let d ← destroyed.get
  unless d == 1 do throw (IO.userError s!"expected 1 destroyed, got {d}")

/-! ### connect / runRedis against a canned RESP2 loopback server -/

-- `connect` is lazy; the socket is opened on the first `runRedis`, which
-- sends `PING` and reads back `+PONG`. Afterwards the pool holds one idle
-- connection.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv accepted 256
    Network.Socket.sendAll accepted "+PONG\r\n".toUTF8
    Network.Socket.close accepted
  let cInfo := { defaultConnectInfo with connectAddr := .hostPort addr.host addr.port }
  let conn ← connect cInfo
  let reply ← runRedis conn Database.Redis.ManualCommands.ping
  match reply with
  | .ok _ => pure ()
  | .error _ => throw (IO.userError "expected PING to succeed, got an error reply")
  match conn with
  | .nonClustered pool =>
    let (idle, inUse, total) ← pool.stats
    unless idle == 1 && inUse == 0 && total == 1 do
      throw (IO.userError s!"expected pool (1,0,1), got ({idle},{inUse},{total})")
  | .clustered _ _ => throw (IO.userError "expected a non-clustered connection")
  disconnect conn
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure ()
  let _ ← Network.Socket.close server
  pure ()

-- `checkedConnect` pings during setup; against a `+PONG` server it succeeds.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv accepted 256
    Network.Socket.sendAll accepted "+PONG\r\n".toUTF8
    Network.Socket.close accepted
  let cInfo := { defaultConnectInfo with connectAddr := .hostPort addr.host addr.port }
  let conn ← checkedConnect cInfo
  disconnect conn
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure ()
  let _ ← Network.Socket.close server
  pure ()

end Tests.Database.Redis.Connection
