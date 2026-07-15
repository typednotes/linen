/-
  Linen.Database.Redis.Connection — `ConnectInfo`, `connect`/`connectCluster`,
  and the connection pool

  Ported from `hedis`'s `Database.Redis.Connection`
  (https://hackage.haskell.org/package/hedis-0.16.1/src/src/Database/Redis/Connection.hs),
  module #13 of the `hedis` import (see `docs/imports/hedis/dependencies.md`).

  Exposes `ConnectInfo`/`defaultConnectInfo`, the `connect`/`checkedConnect`/
  `connectCluster`/`checkedConnectCluster` family (plus TLS variants),
  `disconnect`, the `withConnect`/`withCheckedConnect` brackets, and
  `runRedis`/`runRedisNonBlocking` (which take a connection from the pool and
  hand it to `Core.runRedisInternal`/`runRedisClusteredInternal` — those live
  in `Database.Redis.Core`, module #10, and are *not* redefined here).

  ## Substitution: `resource-pool` → an `IO.Ref`-guarded pool

  Upstream pools live connections with the `resource-pool` package
  (`Data.Pool`: `defaultPoolConfig`/`setNumStripes`/`setPoolLabel`/`newPool`/
  `withResource`/`tryWithResource`/`destroyAllResources`). Per
  `dependencies.md`'s substitution note, this port does not import a fresh
  package: `linen` already has this exact shape of thing in
  `Linen.Database.SQL.Pool` (an `IO.Ref`-guarding an `Array` of idle resources
  plus `inUse`/`totalCreated` counts, resources created on demand and recycled
  after use). That module is written concretely against
  `Database.SQL.Connection`, so it cannot be imported as-is; instead the pool
  below follows its *design* exactly, generalised over the pooled resource
  type `α` (upstream's `Pool a` is likewise generic) so the one pool serves
  both `ProtocolPipelining.Connection` (non-clustered) and
  `Cluster.Connection` (clustered).

  The mapping onto `resource-pool`'s API is faithful:
  `defaultPoolConfig`/`setNumStripes`/`setPoolLabel` build a `PoolConfig`;
  `newPool` allocates the `IO.Ref`; `withResource`/`tryWithResource` acquire
  (reusing an idle resource or creating a new one), run the action, and return
  the resource in a `finally` (exactly `SQL.Pool.use`'s release-on-all-paths
  behaviour); `destroyAllResources` closes every idle resource. Two design
  points inherited from the `SQL.Pool` shape / the strict-`IO` setting:

  - There is no background reaper for `maxIdleTime`, and `numStripes` is a
    single logical stripe: both fields are carried for API fidelity but do not
    change this single-`Ref` pool's behaviour (idle resources are freed by
    `destroyAllResources`/`disconnect`, exactly as in `SQL.Pool`, which also
    stores an `idleTimeout` it never reaps by). This matches `resource-pool`'s
    *observable* contract for a workload that disconnects explicitly.
  - `withResource` never blocks waiting for a slot to free: when no idle
    resource is available it creates a new one (mirroring `SQL.Pool.use`,
    which also creates rather than blocking). `tryWithResource` honours the
    `maxResources` cap — it returns `none` once `inUse` reaches the maximum
    and no idle resource is available, matching `resource-pool`'s
    "return immediately rather than block" contract. (Unlike `SQL.Pool`,
    every acquire path increments `inUse`, so `releaseConn`'s decrement stays
    balanced even under the cap — a small correctness tightening over the
    sibling module, not a behavioural divergence from upstream.)

  ## Other substitutions / deviations

  - **`MVar ShardMap` → `IO.Ref ShardMap`.** Exactly as in
    `Database.Redis.Cluster` (see that module's doc-comment): this batch does
    not port a concurrency-safety story, and `IO.Ref` preserves the same
    reference-cell shape.
  - **Distinct exception types → descriptive `IO.userError`s.** Upstream's
    `ConnectError` (`ConnectAuthError`/`ConnectSelectError`),
    `ClusterConnectError`, and `ClusterDownError` are `Exception` instances
    distinguished by type. As throughout this import (see `Core`/`Cluster`),
    Lean's `IO.Error` is not an open catch-by-type hierarchy and no caller
    needs to distinguish these by type, so each becomes an `IO.userError`
    carrying the offending reply's text.
  - **TLS params are a separate explicit argument, not an `Option` field on
    `ConnectInfo`.** Upstream's `connectTLSParams :: Maybe ClientParams` is a
    record field. Here — for the exact universe-polymorphism reason
    documented in `Database.Redis.ConnectionContext` (wrapping the
    universe-polymorphic `Network.TLS.TLSContext` in an `Option` behind a
    structure/default leaks an unresolved universe metavariable at `#eval`
    call sites) — TLS is offered through parallel `connectTLS`/
    `checkedConnectTLS`/`connectClusterTLS`/`checkedConnectClusterTLS`
    functions that take a fully concrete `TLSContext`, mirroring the
    `connect`/`connectTLS` split already used in `ConnectionContext`,
    `ProtocolPipelining`, and `Cluster`.
  - **`connectTimeout` is carried but not enforced.** Upstream converts it to
    a microsecond socket-connect timeout. The underlying transport in this
    port (`Database.Redis.ConnectionContext`, over
    `Linen.Network.Socket.Blocking`) exposes no connect-timeout hook (see that
    module's doc-comment), so the field is retained for API fidelity and
    future use but does not currently bound the connect. This is a limitation
    of the already-ported lower layers, not a simplification of this module.
-/
import Linen.Database.Redis.Cluster
import Linen.Database.Redis.ConnectionContext
import Linen.Database.Redis.Core
import Linen.Database.Redis.Hooks
import Linen.Database.Redis.ManualCommands
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.ProtocolPipelining
import Std.Data.HashMap

namespace Database.Redis.Connection

open Database.Redis.Protocol (Reply)
open Database.Redis.Hooks (Hooks defaultHooks)
open Database.Redis.ProtocolPipelining (beginReceiving connectWithHooks connectTLSWithHooks fromCtx)

-- ────────────────────────────────────────────────────────────────────
-- Connection pool (the `resource-pool` substitution — see the module
-- doc-comment)
-- ────────────────────────────────────────────────────────────────────

/-- Configuration for a resource pool. Mirrors `resource-pool`'s
    `PoolConfig`, built by `defaultPoolConfig` and refined with
    `setNumStripes`/`setPoolLabel`. `create`/`destroy` are the resource's
    acquire/release actions; `maxResources` caps concurrent resources;
    `maxIdleTime`/`numStripes`/`poolLabel` are carried for fidelity (see the
    module doc-comment for why they do not affect this pool's behaviour). -/
structure PoolConfig (α : Type) where
  /-- Create a fresh resource. -/
  create : IO α
  /-- Release a resource. -/
  destroy : α → IO Unit
  /-- Seconds an idle resource may live (carried for fidelity; not reaped). -/
  maxIdleTime : Nat
  /-- Maximum number of concurrent resources. -/
  maxResources : Nat
  /-- Number of stripes (carried for fidelity; single logical stripe here). -/
  numStripes : Option Nat := none
  /-- Instrumentation label for the pool. -/
  poolLabel : String := ""

/-- The default pool configuration. Mirrors `resource-pool`'s
    `defaultPoolConfig create destroy idleTime maxResources`. -/
def defaultPoolConfig (create : IO α) (destroy : α → IO Unit)
    (maxIdleTime : Nat) (maxResources : Nat) : PoolConfig α :=
  { create, destroy, maxIdleTime, maxResources }

/-- Set the number of stripes. Mirrors `resource-pool`'s `setNumStripes`. -/
def PoolConfig.setNumStripes (n : Option Nat) (c : PoolConfig α) : PoolConfig α :=
  { c with numStripes := n }

/-- Set the pool label. Mirrors `resource-pool`'s `setPoolLabel`. -/
def PoolConfig.setPoolLabel (l : String) (c : PoolConfig α) : PoolConfig α :=
  { c with poolLabel := l }

/-- Internal pool state protected by an `IO.Ref`. Mirrors
    `Database.SQL.Pool.PoolState`. -/
private structure PoolState (α : Type) where
  /-- Available (idle) resources. -/
  available : Array α
  /-- Number of resources currently checked out. -/
  inUse : Nat
  /-- Total resources ever created (for accounting). -/
  totalCreated : Nat

/-- A pool of resources of type `α`. Mirrors upstream's `Pool a`
    (`resource-pool`), built on the `Database.SQL.Pool` `IO.Ref` design. -/
structure Pool (α : Type) where
  /-- The `IO.Ref` guarding the mutable pool state. -/
  state : IO.Ref (PoolState α)
  /-- The static configuration. -/
  config : PoolConfig α

/-- The outcome of inspecting the pool state for an acquire. -/
private inductive Acquire (α : Type) where
  /-- An idle resource was available. -/
  | got (a : α)
  /-- No idle resource; a new one should be created (a slot was reserved). -/
  | make
  /-- The pool is at capacity and nothing is idle. -/
  | exhausted

/-- Allocate a fresh, empty pool from a configuration. Mirrors
    `resource-pool`'s `newPool`. -/
def newPool (config : PoolConfig α) : IO (Pool α) := do
  let ref ← IO.mkRef { available := #[], inUse := 0, totalCreated := 0 }
  pure { state := ref, config }

/-- Acquire a resource, blocking-free: reuse an idle one if available,
    otherwise create a new one (this pool never blocks — see the module
    doc-comment). Every path increments `inUse` so `releaseConn` stays
    balanced. -/
private def acquire (pool : Pool α) : IO α := do
  let r ← pool.state.modifyGet fun st =>
    if h : 0 < st.available.size then
      (Acquire.got st.available[st.available.size - 1],
        { st with available := st.available.pop, inUse := st.inUse + 1 })
    else
      (Acquire.make,
        { st with inUse := st.inUse + 1, totalCreated := st.totalCreated + 1 })
  match r with
  | .got a => pure a
  | .make => pool.config.create
  | .exhausted => pool.config.create  -- unreachable: `acquire` never reports exhaustion

/-- Try to acquire a resource without exceeding `maxResources`: reuse an idle
    one, create a new one if under the cap, or report exhaustion. -/
private def tryAcquire (pool : Pool α) : IO (Option α) := do
  let r ← pool.state.modifyGet fun st =>
    if h : 0 < st.available.size then
      (Acquire.got st.available[st.available.size - 1],
        { st with available := st.available.pop, inUse := st.inUse + 1 })
    else if st.inUse < pool.config.maxResources then
      (Acquire.make,
        { st with inUse := st.inUse + 1, totalCreated := st.totalCreated + 1 })
    else
      (Acquire.exhausted, st)
  match r with
  | .got a => pure (some a)
  | .make => some <$> pool.config.create
  | .exhausted => pure none

/-- Return a resource to the pool. -/
private def releaseConn (pool : Pool α) (a : α) : IO Unit :=
  pool.state.modify fun st =>
    { st with available := st.available.push a, inUse := st.inUse - 1 }

/-- Run an action with a resource borrowed from the pool, returning the
    resource on every path (success or exception). Mirrors upstream's
    `withResource` / `Database.SQL.Pool.use`. -/
def Pool.withResource (pool : Pool α) (act : α → IO β) : IO β := do
  let a ← acquire pool
  try
    act a
  finally
    releaseConn pool a

/-- Like `withResource`, but returns `none` immediately (rather than blocking)
    if the pool is at capacity with nothing idle. Mirrors upstream's
    `tryWithResource`. -/
def Pool.tryWithResource (pool : Pool α) (act : α → IO β) : IO (Option β) := do
  match ← tryAcquire pool with
  | none => pure none
  | some a =>
    try
      some <$> act a
    finally
      releaseConn pool a

/-- Destroy every idle resource in the pool. Mirrors upstream's
    `destroyAllResources`. -/
def Pool.destroyAllResources (pool : Pool α) : IO Unit := do
  let idle ← pool.state.modifyGet fun st => (st.available, { st with available := #[] })
  idle.forM pool.config.destroy

/-- Current pool statistics (idle, in-use, total created). Mirrors
    `Database.SQL.Pool.stats`. -/
def Pool.stats (pool : Pool α) : IO (Nat × Nat × Nat) := do
  let st ← pool.state.get
  pure (st.available.size, st.inUse, st.totalCreated)

-- ────────────────────────────────────────────────────────────────────
-- Connection & ConnectInfo
-- ────────────────────────────────────────────────────────────────────

/-- A threadsafe pool of network connections to a Redis server (single-node
    or clustered). Use `connect`/`connectCluster` to create one. Mirrors
    upstream's `Connection` (the `MVar ShardMap` is an `IO.Ref` here — see the
    module doc-comment). -/
inductive Connection where
  /-- A pool of single-node pipelined connections. -/
  | nonClustered (pool : Pool Database.Redis.ProtocolPipelining.Connection)
  /-- A pool of clustered connections, plus the shared shard map. -/
  | clustered (shardMap : IO.Ref Database.Redis.Cluster.ShardMap)
      (pool : Pool Database.Redis.Cluster.Connection)

/-- Information for connecting to a Redis server. Prefer building from
    `defaultConnectInfo` with record-update syntax. Mirrors upstream's
    `ConnectInfo` (minus `connectTLSParams`, which is a separate explicit
    argument here — see the module doc-comment). -/
structure ConnectInfo where
  /-- Where to connect. -/
  connectAddr : Database.Redis.ConnectionContext.ConnectAddr
  /-- Password, if the server requires `AUTH`. -/
  connectAuth : Option ByteArray := none
  /-- Username, if ACLs are in use. -/
  connectUsername : Option ByteArray := none
  /-- Database index each connection will `SELECT`. -/
  connectDatabase : Int := 0
  /-- Maximum number of connections to keep open (smallest sensible: 1). -/
  connectMaxConnections : Nat := 50
  /-- Number of stripes in the connection pool. -/
  connectNumStripes : Option Nat := some 1
  /-- Seconds an unused connection is kept open. -/
  connectMaxIdleTime : Nat := 30
  /-- Optional connect timeout in microseconds (carried but not enforced —
      see the module doc-comment). -/
  connectTimeout : Option Nat := none
  /-- Connection hooks. -/
  connectHooks : Hooks := defaultHooks
  /-- Label of the connection pool for instrumentation. -/
  connectPoolLabel : String := ""

/-- Default connection information: `localhost:6379`, no auth, database 0, up
    to 50 connections, one stripe, 30s idle time, no timeout, default hooks,
    no label. Mirrors upstream's `defaultConnectInfo`. -/
def defaultConnectInfo : ConnectInfo where
  connectAddr := .hostPort "localhost" 6379

-- ────────────────────────────────────────────────────────────────────
-- Errors (see the module doc-comment: distinct upstream exception types
-- become descriptive `IO.userError`s)
-- ────────────────────────────────────────────────────────────────────

/-- Render a reply's payload as text for an error message. -/
private def replyText (r : Reply) : String :=
  match r with
  | .error s => (String.fromUTF8? s).getD "<non-utf8 error>"
  | .singleLine s => (String.fromUTF8? s).getD "<non-utf8>"
  | .integer i => toString i
  | .bulk (some s) => (String.fromUTF8? s).getD "<bulk>"
  | .bulk none => "<nil>"
  | .multiBulk _ => "<array>"

/-- `AUTH` failed while establishing a connection (upstream's
    `ConnectAuthError`). -/
def connectAuthError (r : Reply) : IO.Error :=
  IO.userError s!"Redis.Connection: AUTH failed: {replyText r}"

/-- `SELECT` failed while establishing a connection (upstream's
    `ConnectSelectError`). -/
def connectSelectError (r : Reply) : IO.Error :=
  IO.userError s!"Redis.Connection: SELECT failed: {replyText r}"

/-- A cluster bootstrap command (`CLUSTER SLOTS`/`COMMAND`/`CLUSTER INFO`)
    returned an error reply (upstream's `ClusterConnectError`). -/
def clusterConnectError (r : Reply) : IO.Error :=
  IO.userError s!"Redis.Connection: cluster connect error: {replyText r}"

/-- `CLUSTER INFO` reported the cluster is down (upstream's
    `ClusterDownError`). -/
def clusterDownError : IO.Error :=
  IO.userError "Redis.Connection: cluster is down (cluster_state:fail)"

-- ────────────────────────────────────────────────────────────────────
-- Establishing a single connection
-- ────────────────────────────────────────────────────────────────────

/-- Run the per-connection `AUTH`/`SELECT` handshake. Mirrors the
    `runRedisInternal` block inside upstream's `createConnection`. -/
private def setupConnection (cInfo : ConnectInfo) : Database.Redis.Core.Redis Unit := do
  -- AUTH
  match cInfo.connectAuth with
  | none => pure ()
  | some pass =>
    match ← Database.Redis.ManualCommands.authOpts pass
        { authOptsUsername := cInfo.connectUsername } with
    | .error r => (throw (connectAuthError r) : IO Unit)
    | .ok _ => pure ()
  -- SELECT
  if cInfo.connectDatabase != 0 then
    match ← Database.Redis.ManualCommands.select cInfo.connectDatabase with
    | .error r => (throw (connectSelectError r) : IO Unit)
    | .ok _ => pure ()
  else pure ()

/-- Create a single (non-TLS) pipelined connection, running its `AUTH`/
    `SELECT` handshake. Mirrors the non-TLS path of upstream's
    `createConnection`. -/
def createConnection (cInfo : ConnectInfo) : IO Database.Redis.ProtocolPipelining.Connection := do
  let conn ← connectWithHooks cInfo.connectAddr cInfo.connectHooks
  beginReceiving conn
  Database.Redis.Core.runRedisInternal conn (setupConnection cInfo)
  pure conn

/-- Create a single TLS pipelined connection, running its `AUTH`/`SELECT`
    handshake. Mirrors the TLS path of upstream's `createConnection`. -/
def createConnectionTLS (cInfo : ConnectInfo) (ctx : Network.TLS.TLSContext) :
    IO Database.Redis.ProtocolPipelining.Connection := do
  let conn ← connectTLSWithHooks cInfo.connectAddr ctx cInfo.connectHooks
  beginReceiving conn
  Database.Redis.Core.runRedisInternal conn (setupConnection cInfo)
  pure conn

-- ────────────────────────────────────────────────────────────────────
-- Shard-map construction (from a `CLUSTER SLOTS` reply)
-- ────────────────────────────────────────────────────────────────────

/-- Convert one `CLUSTER SLOTS` node into a `Cluster.Node`. Mirrors upstream's
    `nodeFromClusterSlotNode`. -/
private def nodeFromClusterSlotNode (isMaster : Bool)
    (n : Database.Redis.ManualCommands.ClusterSlotsNode) : Database.Redis.Cluster.Node :=
  { id := n.nodeID
    role := if isMaster then .master else .slave
    host := (String.fromUTF8? n.nodeIP).getD ""
    port := n.nodePort.toNat }

/-- Build a `ShardMap` from a parsed `CLUSTER SLOTS` reply. Mirrors upstream's
    `shardMapFromClusterSlotsResponse` (pure here: upstream's `IO` was only
    incidental to folding over `pure IntMap.empty`). For each slot-range
    entry, every slot in `[startSlot, endSlot]` maps to that entry's shard;
    on an (unexpected) overlap, the entry processed later left-to-right wins,
    matching upstream's left-biased `IntMap.union slotMap accumulated`. -/
def shardMapFromClusterSlotsResponse
    (resp : Database.Redis.ManualCommands.ClusterSlotsResponse) :
    Database.Redis.Cluster.ShardMap :=
  let slots := resp.entries.foldr (init := (∅ : Std.HashMap Nat Database.Redis.Cluster.Shard))
    fun entry acc =>
      let master := nodeFromClusterSlotNode true entry.master
      let replicas := entry.replicas.map (nodeFromClusterSlotNode false)
      let shard : Database.Redis.Cluster.Shard := { master, slaves := replicas }
      let lo := entry.startSlot.toNat
      let hi := entry.endSlot.toNat
      (List.range' lo (hi + 1 - lo)).foldl (fun m s => m.insert s shard) acc
  { slots }

/-- Refresh the shard map for a clustered connection by re-issuing
    `CLUSTER SLOTS` over one of its live node connections. Mirrors upstream's
    `refreshShardMap`. -/
def refreshShardMap (conn : Database.Redis.Cluster.Connection) :
    IO Database.Redis.Cluster.ShardMap := do
  match conn.nodes.toList.head? with
  | none =>
    throw (IO.userError "Redis.Connection: cannot refresh shard map — no node connections")
  | some (_, nodeConn) =>
    let ctx := nodeConn.conn.connCtx
    let pipelineConn ← fromCtx ctx
    beginReceiving pipelineConn
    match ← Database.Redis.Core.runRedisInternal pipelineConn
        Database.Redis.ManualCommands.clusterSlots with
    | .error e => throw (clusterConnectError e)
    | .ok slots => pure (shardMapFromClusterSlotsResponse slots)

-- ────────────────────────────────────────────────────────────────────
-- connect / connectCluster
-- ────────────────────────────────────────────────────────────────────

/-- Construct a `Connection` pool to a single Redis server. The first
    connection is not established until the first `runRedis`. Mirrors
    upstream's `connect`. -/
def connect (cInfo : ConnectInfo) : IO Connection := do
  let pool ← newPool <| PoolConfig.setPoolLabel cInfo.connectPoolLabel
    <| PoolConfig.setNumStripes cInfo.connectNumStripes
    <| defaultPoolConfig (createConnection cInfo) Database.Redis.ProtocolPipelining.disconnect
        cInfo.connectMaxIdleTime cInfo.connectMaxConnections
  pure (.nonClustered pool)

/-- Construct a TLS `Connection` pool to a single Redis server. TLS variant of
    `connect` (see the module doc-comment on why TLS is a separate argument). -/
def connectTLS (cInfo : ConnectInfo) (ctx : Network.TLS.TLSContext) : IO Connection := do
  let pool ← newPool <| PoolConfig.setPoolLabel cInfo.connectPoolLabel
    <| PoolConfig.setNumStripes cInfo.connectNumStripes
    <| defaultPoolConfig (createConnectionTLS cInfo ctx) Database.Redis.ProtocolPipelining.disconnect
        cInfo.connectMaxIdleTime cInfo.connectMaxConnections
  pure (.nonClustered pool)

-- ────────────────────────────────────────────────────────────────────
-- runRedis
-- ────────────────────────────────────────────────────────────────────

/-- Run a `Redis` action using a connection borrowed from the pool. May block
    (conceptually) while all connections are in use. Mirrors upstream's
    `runRedis`. -/
def runRedis (conn : Connection) (redis : Database.Redis.Core.Redis α) : IO α :=
  match conn with
  | .nonClustered pool =>
    pool.withResource fun c => Database.Redis.Core.runRedisInternal c redis
  | .clustered _ pool =>
    pool.withResource fun c =>
      Database.Redis.Core.runRedisClusteredInternal c (refreshShardMap c) redis

/-- Like `runRedis`, but returns `none` immediately if all pooled connections
    are in use. Mirrors upstream's `runRedisNonBlocking`. -/
def runRedisNonBlocking (conn : Connection) (redis : Database.Redis.Core.Redis α) :
    IO (Option α) :=
  match conn with
  | .nonClustered pool =>
    pool.tryWithResource fun c => Database.Redis.Core.runRedisInternal c redis
  | .clustered _ pool =>
    pool.tryWithResource fun c =>
      Database.Redis.Core.runRedisClusteredInternal c (refreshShardMap c) redis

-- ────────────────────────────────────────────────────────────────────
-- checkedConnect / connectCluster / checked cluster
-- ────────────────────────────────────────────────────────────────────

/-- `connect`, then ping the server to confirm it is reachable (throwing if
    not). Mirrors upstream's `checkedConnect`. -/
def checkedConnect (cInfo : ConnectInfo) : IO Connection := do
  let conn ← connect cInfo
  let _ ← runRedis conn (do let _ ← Database.Redis.ManualCommands.ping; pure ())
  pure conn

/-- TLS variant of `checkedConnect`. -/
def checkedConnectTLS (cInfo : ConnectInfo) (ctx : Network.TLS.TLSContext) : IO Connection := do
  let conn ← connectTLS cInfo ctx
  let _ ← runRedis conn (do let _ ← Database.Redis.ManualCommands.ping; pure ())
  pure conn

/-- Build a clustered `Connection` from a bootstrap `ConnectInfo`, using a
    temporary connection to fetch the shard map (`CLUSTER SLOTS`) and the
    command routing table (`COMMAND`) before opening the pool. Mirrors
    upstream's `connectCluster` (the bootstrap connection is disconnected via
    a `finally`, matching upstream's `bracket … Database.Redis.ProtocolPipelining.disconnect`). -/
def connectCluster (bootstrap : ConnectInfo) : IO Connection := do
  let conn ← createConnection bootstrap
  try
    let shardMap ← match ← Database.Redis.Core.runRedisInternal conn
        Database.Redis.ManualCommands.clusterSlots with
      | .error e => throw (clusterConnectError e)
      | .ok slots => pure (shardMapFromClusterSlotsResponse slots)
    match ← Database.Redis.Core.runRedisInternal conn
        Database.Redis.ManualCommands.command with
    | .error e => throw (clusterConnectError e)
    | .ok infos =>
      let shardMapRef ← IO.mkRef shardMap
      let pool ← newPool <| PoolConfig.setPoolLabel bootstrap.connectPoolLabel
        <| PoolConfig.setNumStripes bootstrap.connectNumStripes
        <| defaultPoolConfig
            (do
              let sm ← shardMapRef.get
              Database.Redis.Cluster.connectWith bootstrap.connectUsername
                bootstrap.connectAuth infos sm bootstrap.connectHooks)
            Database.Redis.Cluster.disconnect
            bootstrap.connectMaxIdleTime bootstrap.connectMaxConnections
      pure (.clustered shardMapRef pool)
  finally
    Database.Redis.ProtocolPipelining.disconnect conn

/-- TLS variant of `connectCluster`. -/
def connectClusterTLS (bootstrap : ConnectInfo) (ctx : Network.TLS.TLSContext) :
    IO Connection := do
  let conn ← createConnectionTLS bootstrap ctx
  try
    let shardMap ← match ← Database.Redis.Core.runRedisInternal conn
        Database.Redis.ManualCommands.clusterSlots with
      | .error e => throw (clusterConnectError e)
      | .ok slots => pure (shardMapFromClusterSlotsResponse slots)
    match ← Database.Redis.Core.runRedisInternal conn
        Database.Redis.ManualCommands.command with
    | .error e => throw (clusterConnectError e)
    | .ok infos =>
      let shardMapRef ← IO.mkRef shardMap
      let pool ← newPool <| PoolConfig.setPoolLabel bootstrap.connectPoolLabel
        <| PoolConfig.setNumStripes bootstrap.connectNumStripes
        <| defaultPoolConfig
            (do
              let sm ← shardMapRef.get
              Database.Redis.Cluster.connectWithTLS bootstrap.connectUsername
                bootstrap.connectAuth infos sm bootstrap.connectHooks ctx)
            Database.Redis.Cluster.disconnect
            bootstrap.connectMaxIdleTime bootstrap.connectMaxConnections
      pure (.clustered shardMapRef pool)
  finally
    Database.Redis.ProtocolPipelining.disconnect conn

/-- `connectCluster`, then confirm the cluster is up via `CLUSTER INFO`.
    Mirrors upstream's `checkedConnectCluster`. -/
def checkedConnectCluster (cInfo : ConnectInfo) : IO Connection := do
  let conn ← connectCluster cInfo
  match ← runRedis conn Database.Redis.ManualCommands.clusterInfo with
  | .ok r =>
    match r.state with
    | .ok => pure conn
    | .down => throw clusterDownError
  | .error e => throw (clusterConnectError e)

/-- TLS variant of `checkedConnectCluster`. -/
def checkedConnectClusterTLS (cInfo : ConnectInfo) (ctx : Network.TLS.TLSContext) :
    IO Connection := do
  let conn ← connectClusterTLS cInfo ctx
  match ← runRedis conn Database.Redis.ManualCommands.clusterInfo with
  | .ok r =>
    match r.state with
    | .ok => pure conn
    | .down => throw clusterDownError
  | .error e => throw (clusterConnectError e)

-- ────────────────────────────────────────────────────────────────────
-- disconnect / brackets
-- ────────────────────────────────────────────────────────────────────

/-- Destroy all idle resources in the pool. Mirrors upstream's `disconnect`. -/
def disconnect : Connection → IO Unit
  | .nonClustered pool => pool.destroyAllResources
  | .clustered _ pool => pool.destroyAllResources

/-- Bracket around `connect` and `disconnect`. Mirrors upstream's
    `withConnect`. -/
def withConnect (cInfo : ConnectInfo) (action : Connection → IO β) : IO β := do
  let conn ← connect cInfo
  try action conn finally disconnect conn

/-- Bracket around `checkedConnect` and `disconnect`. Mirrors upstream's
    `withCheckedConnect`. -/
def withCheckedConnect (cInfo : ConnectInfo) (action : Connection → IO β) : IO β := do
  let conn ← checkedConnect cInfo
  try action conn finally disconnect conn

end Database.Redis.Connection
