/-
  Linen.Database.Redis.Cluster — cluster topology and MOVED/ASK redirects

  Ported from `hedis`'s `Database.Redis.Cluster`
  (https://raw.githubusercontent.com/informatikr/hedis/master/src/Database/Redis/Cluster.hs).

  Upstream implements a clustered connection: a map from node IDs to
  per-node connections, a shard map (hash-slot → shard, i.e. a master plus
  its replicas), request routing by key hash slot, and retry-on-redirect
  (`MOVED`/`ASK`) handling — plus, in the same module, an implicit-pipelining
  scheduler (`MVar Pipeline` + `unsafeInterleaveIO`) and a `MULTI`/`EXEC`
  transaction-buffering state machine layered on top of it.

  ## Scope reductions (both genuinely out of scope for this batch, not
  simplifications of in-scope behaviour)

  - **No `MULTI`/`EXEC` transaction state machine.** Upstream's
    `requestPipelined` doubles as the entry point for `Database.Redis.
    Transactions`' `MULTI`/`EXEC` support (`TransactionPending`,
    `evaluateTransactionPipeline`): it watches for the literal command names
    `"MULTI"`/`"EXEC"` and buffers an entire transaction until it sees `EXEC`,
    so that every command in a transaction is routed to the same node.
    `Database.Redis.Transactions` is module #15 of the `hedis` import plan
    (see `docs/imports/hedis/dependencies.md`), explicitly out of scope for
    this batch (#1–10) — so this port only implements the non-transactional
    per-command routing path (`evaluatePipeline`'s logic, ported as
    `requestPipelined`/`evaluateBatch`), and does not recognise `MULTI`/`EXEC`
    at all.
  - **No implicit, deferred pipelining across separate `requestPipelined`
    calls.** As in `Linen.Database.Redis.ProtocolPipelining` (see that
    module's doc-comment for the full rationale), Lean's strict `IO` has no
    faithful, safe equivalent of `unsafeInterleaveIO`. `requestPipelined`
    here evaluates and returns its result eagerly rather than an
    unevaluated thunk that transparently batches with whatever
    `requestPipelined` calls come after it — genuine pipelining of several
    commands *known up front* is still available via `evaluateBatch`, which
    takes the whole list of pending commands, groups them by target node,
    and pipelines each node's sub-batch (mirroring upstream's
    `evaluatePipeline` faithfully); only the automatic, laziness-driven
    batching of calls the caller hasn't made yet is dropped.

  ## Other substitutions

  - `Control.Concurrent.MVar` → `IO.Ref`. Upstream's `MVar`s
    (`connectionPipeline`, `connectionShardMap`, `nodeConnectionLastRecvRef`)
    exist to make the `Connection` safe to share across concurrent Haskell
    threads. This batch doesn't port a concurrency-safety story for
    `Database.Redis.Cluster` (no other already-ported `linen` module
    provides an `MVar`-equivalent write-serializing primitive suitable for
    reuse here without pulling in `Control.Concurrent.MVar` as a fresh
    dependency of its own) — `IO.Ref` preserves the same reference-cell
    *shape*, just without the mutual-exclusion guarantee.
  - `Data.HashMap.Strict`/`Data.IntMap.Strict` → `Std.HashMap`, exactly as in
    `Database.Redis.Cluster.Command` (module #6 of this batch).
  - Per-node reply buffering/parsing (`nodeConnectionLastRecvRef` +
    `Scanner.scanWith`) is not reimplemented here: `NodeConnection` simply
    wraps a `Database.Redis.ProtocolPipelining.Connection`, which already
    provides exactly this (its own `connBuf`/`parseOneReply`), avoiding a
    redundant second copy of the same buffering logic.
  - Distinct upstream exception types (`MissingNodeException`,
    `CrossSlotException`, `UnsupportedClusterCommandException`,
    `ClusterAuthError`) become descriptive `IO.userError`s: Lean's `IO.Error`
    isn't an open, `catch`-by-type hierarchy the way GHC's `Exception` class
    is, and no caller in this batch (or the modules that would consume this
    one — #9/#10 — per the dependency plan) needs to distinguish them by
    type rather than by message.
-/
import Linen.Database.Redis.Cluster.Command
import Linen.Database.Redis.Cluster.HashSlot
import Linen.Database.Redis.Hooks
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.ProtocolPipelining
import Std.Data.HashMap

namespace Database.Redis.Cluster

open Database.Redis.Protocol (Reply renderRequest)
open Database.Redis.Hooks (Hooks)
open Database.Redis.Cluster.HashSlot (HashSlot keyToSlot)

-- ── Byte-string helpers ──

/-- Does `bytes` start with `prefix`? -/
private def hasBytePrefix (bytes prefixBytes : ByteArray) : Bool :=
  bytes.size ≥ prefixBytes.size ∧ bytes.extract 0 prefixBytes.size == prefixBytes

/-- Split `bytes` on every occurrence of `sep`. Structural recursion on the
    byte list. -/
private def splitOnByte (sep : UInt8) (bytes : ByteArray) : List ByteArray :=
  let rec go : List UInt8 → List UInt8 → List (List UInt8)
    | [], acc => [acc.reverse]
    | b :: rest, acc =>
      if b == sep then acc.reverse :: go rest [] else go rest (b :: acc)
  (go bytes.toList []).map (fun l => ByteArray.mk l.toArray)

/-- Parse a `ByteArray` as an unsigned decimal `Nat`, requiring every byte to
    be a digit (no sign, no trailing garbage). Structural recursion on the
    byte list. -/
private def readNatDecimalExact (bytes : ByteArray) : Option Nat :=
  if bytes.isEmpty then none
  else
    let rec go : List UInt8 → Nat → Option Nat
      | [], acc => some acc
      | b :: rest, acc =>
        if '0'.toUInt8 ≤ b ∧ b ≤ '9'.toUInt8 then
          go rest (acc * 10 + (b - '0'.toUInt8).toNat)
        else none
    go bytes.toList 0

-- ── Cluster topology ──

abbrev Host := String
abbrev Port := Nat
abbrev NodeID := ByteArray

/-- A node's role within its shard. Mirrors upstream's `NodeRole`. -/
inductive NodeRole where
  | master | slave
  deriving BEq, Repr, Inhabited

/-- A single cluster node (address + identity), without a live connection —
    the connection, if any, lives separately in `Connection.nodes`. Mirrors
    upstream's `Node`. -/
structure Node where
  id : NodeID
  role : NodeRole
  host : Host
  port : Port
  deriving BEq, Inhabited

/-- A shard: one master and zero or more replicas. Mirrors upstream's
    `Shard`. -/
structure Shard where
  master : Node
  slaves : List Node
  deriving BEq, Inhabited

/-- A map from hash slot to the shard that owns it. Mirrors upstream's
    `ShardMap` (an `IntMap`, here a `HashMap` over the slot's `UInt16`
    reinterpreted as a `Nat`). -/
structure ShardMap where
  slots : Std.HashMap Nat Shard

/-- Deduplicate a list of `Node`s by ID, keeping the first occurrence of
    each. Structural recursion on the list. Shared by `ShardMap.nodes`,
    `allMasterNodes`, and `masterNodes` (all of which upstream implements
    via `Data.List.nub` on `Node`'s derived `Eq` instance). -/
private def dedupNodes (seen : Std.HashMap NodeID Unit) : List Node → List Node
  | [] => []
  | n :: rest =>
    if seen.contains n.id then dedupNodes seen rest
    else n :: dedupNodes (seen.insert n.id ()) rest

/-- Every node that appears in `shardMap` (master and replicas of every
    shard), deduplicated by node ID. Mirrors upstream's `nodes`. -/
def ShardMap.nodes (shardMap : ShardMap) : List Node :=
  dedupNodes {} (shardMap.slots.toList.flatMap (fun (_, s) => s.master :: s.slaves))

/-- Find the node at the given host/port, if any. Mirrors upstream's
    `nodeWithHostAndPort`. -/
def ShardMap.nodeWithHostAndPort (shardMap : ShardMap) (host : Host) (port : Port) : Option Node :=
  (ShardMap.nodes shardMap).find? (fun n => n.host == host ∧ n.port == port)

-- ── Per-node and cluster-wide connections ──

/-- A connection to a single node in the cluster. Mirrors upstream's
    `NodeConnection`, reusing `ProtocolPipelining.Connection` for the
    buffered-reply-parsing machinery (see the module doc-comment). -/
structure NodeConnection where
  conn : Database.Redis.ProtocolPipelining.Connection
  nodeId : NodeID

/-- Two `NodeConnection`s are the "same" node iff their node IDs match.
    Mirrors upstream's `Eq NodeConnection` instance (upstream also derives
    `Ord`, used only to put `NodeConnection`s into a `Data.Map`/dedup via
    `nub`; this port uses `NodeID`-keyed `Std.HashMap`s instead, so no `Ord`
    instance is needed). -/
def NodeConnection.sameNode (a b : NodeConnection) : Bool :=
  a.nodeId == b.nodeId

/-- A connection to a redis cluster: a map from node ID to live
    `NodeConnection`, the current `ShardMap`, the `COMMAND`-derived
    `InfoMap` used for key extraction, and instrumentation `Hooks`. Mirrors
    upstream's `Connection` (minus `connectionPipeline`, which existed only
    to support the implicit-pipelining/transaction state machine this port
    doesn't implement — see the module doc-comment). -/
structure Connection where
  nodes : Std.HashMap NodeID NodeConnection
  shardMap : IO.Ref ShardMap
  infoMap : Database.Redis.Cluster.Command.InfoMap
  hooks : Hooks

/-- The `Hooks` a `Connection` was built with. Mirrors upstream's `hooks`. -/
def Connection.hooksOf (conn : Connection) : Hooks := conn.hooks

/-- Connect (and, if `mPassword` is given, `AUTH`) to a single node. Mirrors
    the `connectNode` inner-helper of upstream's `connectWith`.

    (As in `Database.Redis.ConnectionContext`/`ProtocolPipelining`, TLS is a
    separate function rather than an `Option`-wrapped parameter, to avoid
    `TLSContext`'s universe-polymorphism issue — see
    `Database.Redis.ConnectionContext`'s doc-comment.) -/
def connectNode (node : Node) (mUsername : Option ByteArray) (mPassword : Option ByteArray) :
    IO NodeConnection := do
  let conn ← Database.Redis.ProtocolPipelining.connect
    (.hostPort node.host node.port.toUInt16)
  let nodeConn : NodeConnection := { conn, nodeId := node.id }
  match mPassword with
  | none => pure ()
  | some password =>
    let reqOpts := match mUsername with
      | some u => [u, password]
      | none => [password]
    let authReply ← Database.Redis.ProtocolPipelining.request nodeConn.conn
      (renderRequest (["AUTH".toUTF8] ++ reqOpts))
    match authReply with
    | .singleLine s =>
      unless s == "OK".toUTF8 do
        throw (IO.userError
          s!"Redis.Cluster: AUTH failed for node {node.host}:{node.port}")
    | _ =>
      throw (IO.userError
        s!"Redis.Cluster: AUTH failed for node {node.host}:{node.port}")
  pure nodeConn

/-- Connect over TLS (and, if `mPassword` is given, `AUTH`) to a single
    node. Mirrors the TLS-enabled path of upstream's `connectNode`. -/
def connectNodeTLS (node : Node) (mUsername : Option ByteArray) (mPassword : Option ByteArray)
    (ctx : Network.TLS.TLSContext) : IO NodeConnection := do
  let conn ← Database.Redis.ProtocolPipelining.connectTLS
    (.hostPort node.host node.port.toUInt16) ctx
  let nodeConn : NodeConnection := { conn, nodeId := node.id }
  match mPassword with
  | none => pure ()
  | some password =>
    let reqOpts := match mUsername with
      | some u => [u, password]
      | none => [password]
    let authReply ← Database.Redis.ProtocolPipelining.request nodeConn.conn
      (renderRequest (["AUTH".toUTF8] ++ reqOpts))
    match authReply with
    | .singleLine s =>
      unless s == "OK".toUTF8 do
        throw (IO.userError
          s!"Redis.Cluster: AUTH failed for node {node.host}:{node.port}")
    | _ =>
      throw (IO.userError
        s!"Redis.Cluster: AUTH failed for node {node.host}:{node.port}")
  pure nodeConn

/-- Connect to every unique node in `shardMap` (best-effort cleanup: on any
    connection failure, already-opened connections are closed before the
    error is re-thrown, approximating upstream's `bracketOnError`). Mirrors
    the non-TLS path of upstream's `connectWith`. -/
def connectWith (mUsername mPassword : Option ByteArray) (commandInfos : List Database.Redis.Cluster.Command.CommandInfo)
    (shardMap : ShardMap) (hooks : Hooks) : IO Connection := do
  let opened ← IO.mkRef (∅ : Std.HashMap NodeID NodeConnection)
  try
    for node in shardMap.nodes do
      let nc ← connectNode node mUsername mPassword
      opened.modify (·.insert node.id nc)
    let nodeMap ← opened.get
    let shardMapRef ← IO.mkRef shardMap
    pure { nodes := nodeMap, shardMap := shardMapRef
         , infoMap := Database.Redis.Cluster.Command.newInfoMap commandInfos, hooks := hooks }
  catch e =>
    let nodeMap ← opened.get
    for (_, nc) in nodeMap.toList do
      Database.Redis.ProtocolPipelining.disconnect nc.conn
    throw e

/-- Connect over TLS to every unique node in `shardMap`. Mirrors the
    TLS-enabled path of upstream's `connectWith`. -/
def connectWithTLS (mUsername mPassword : Option ByteArray)
    (commandInfos : List Database.Redis.Cluster.Command.CommandInfo)
    (shardMap : ShardMap) (hooks : Hooks) (ctx : Network.TLS.TLSContext) : IO Connection := do
  let opened ← IO.mkRef (∅ : Std.HashMap NodeID NodeConnection)
  try
    for node in shardMap.nodes do
      let nc ← connectNodeTLS node mUsername mPassword ctx
      opened.modify (·.insert node.id nc)
    let nodeMap ← opened.get
    let shardMapRef ← IO.mkRef shardMap
    pure { nodes := nodeMap, shardMap := shardMapRef
         , infoMap := Database.Redis.Cluster.Command.newInfoMap commandInfos, hooks := hooks }
  catch e =>
    let nodeMap ← opened.get
    for (_, nc) in nodeMap.toList do
      Database.Redis.ProtocolPipelining.disconnect nc.conn
    throw e

/-- Disconnect every node connection. Mirrors upstream's `disconnect`. -/
def disconnect (conn : Connection) : IO Unit := do
  for (_, nc) in conn.nodes.toList do
    Database.Redis.ProtocolPipelining.disconnect nc.conn

-- ── Sending requests to a node ──

/-- Send a batch of requests to one node, pipelined (all requests written
    before any reply is read), and return their replies in order. Mirrors
    upstream's `requestNode`. -/
def requestNode (nodeConn : NodeConnection) (requests : List (List ByteArray)) : IO (List Reply) := do
  for request in requests do
    Database.Redis.ProtocolPipelining.send nodeConn.conn (renderRequest request)
  Database.Redis.ProtocolPipelining.flush nodeConn.conn
  requests.mapM (fun _ => Database.Redis.ProtocolPipelining.recv nodeConn.conn)

/-- Send a single request to one node and return its reply. Mirrors
    upstream's `requestNode1`. -/
def requestNode1 (nodeConn : NodeConnection) (request : List ByteArray) : IO Reply :=
  Database.Redis.ProtocolPipelining.request nodeConn.conn (renderRequest request)

-- ── Routing ──

/-- Every currently-connected master node, deduplicated by shard. Mirrors
    upstream's `allMasterNodes`. `none` iff some shard's master has no live
    connection in `conn.nodes` (a `Connection` invariant violation, matching
    upstream's own `Maybe`-returning `mapM`/lookup chain). -/
def allMasterNodes (conn : Connection) (shardMap : ShardMap) : Option (List NodeConnection) :=
  let masters := shardMap.slots.toList.map (fun (_, s) => s.master)
  (dedupNodes {} masters).mapM (fun n => conn.nodes.get? n.id)

/-- The keys touched by a request, per the `COMMAND`-derived `InfoMap`.
    Mirrors upstream's `requestKeys`. -/
def requestKeys (infoMap : Database.Redis.Cluster.Command.InfoMap) (request : List ByteArray) :
    IO (List ByteArray) :=
  match Database.Redis.Cluster.Command.keysForRequest infoMap request with
  | none => throw (IO.userError s!"Redis.Cluster: unsupported cluster command")
  | some ks => pure ks

/-- The single hash slot that every key in `keys` maps to, or slot `0` if
    there are no keys (any node will do). Throws if the keys span more than
    one slot. Mirrors upstream's `hashSlotForKeys`. -/
def hashSlotForKeys (requests : List (List ByteArray)) (keys : List ByteArray) : IO HashSlot :=
  let slots := (keys.map keyToSlot).foldl
    (fun acc s => if acc.contains s then acc else acc ++ [s]) []
  match slots with
  | [] => pure ⟨0⟩
  | [slot] => pure slot
  | _ => throw (IO.userError s!"Redis.Cluster: keys span multiple hash slots in {requests.length} request(s)")

/-- The node connection responsible for a given hash slot's master. Mirrors
    upstream's `nodeConnForHashSlot`. -/
def nodeConnForHashSlot (conn : Connection) (hashSlot : HashSlot) : IO NodeConnection := do
  let shardMap ← conn.shardMap.get
  match shardMap.slots.get? hashSlot.toUInt16.toNat with
  | none => throw (IO.userError s!"Redis.Cluster: no node owns hash slot {hashSlot.toUInt16}")
  | some shard =>
    match conn.nodes.get? shard.master.id with
    | none => throw (IO.userError s!"Redis.Cluster: no connection for node owning hash slot {hashSlot.toUInt16}")
    | some nodeConn => pure nodeConn

/-- The node connection(s) that should receive a given request: every
    master (for cluster-wide commands like `FLUSHALL`), or the single
    master owning the request's key(s)' hash slot. Mirrors upstream's
    `nodeConnectionForCommand`. -/
def nodeConnectionForCommand (conn : Connection) (shardMap : ShardMap) (request : List ByteArray) :
    IO (List NodeConnection) := do
  match request with
  | cmd :: _ =>
    if cmd == "FLUSHALL".toUTF8 ∨ cmd == "FLUSHDB".toUTF8 ∨ cmd == "QUIT".toUTF8
        ∨ cmd == "UNWATCH".toUTF8 then
      match allMasterNodes conn shardMap with
      | none => throw (IO.userError "Redis.Cluster: missing node connection for a master node")
      | some ns => pure ns
    else do
      let keys ← requestKeys conn.infoMap request
      let hashSlot ← hashSlotForKeys [request] keys
      match shardMap.slots.get? hashSlot.toUInt16.toNat with
      | none => throw (IO.userError s!"Redis.Cluster: no node owns hash slot {hashSlot.toUInt16}")
      | some shard =>
        match conn.nodes.get? shard.master.id with
        | none => throw (IO.userError "Redis.Cluster: missing node connection for the request's master node")
        | some nodeConn => pure [nodeConn]
  | [] => throw (IO.userError "Redis.Cluster: empty request")

-- ── MOVED / ASK redirect handling ──

/-- Is this reply a `MOVED` redirect error? Mirrors upstream's `moved`. -/
def moved (r : Reply) : Bool :=
  match r with
  | .error s => hasBytePrefix s "MOVED".toUTF8
  | _ => false

/-- If this reply is an `ASK` redirect error, the target host/port. Mirrors
    upstream's `askingRedirection`. -/
def askingRedirection (r : Reply) : Option (Host × Port) :=
  match r with
  | .error s =>
    match splitOnByte ' '.toUInt8 s with
    | [tag, _slot, hostport] =>
      if tag == "ASK".toUTF8 then
        match splitOnByte ':'.toUInt8 hostport with
        | [hostBytes, portBytes] =>
          match String.fromUTF8? hostBytes, readNatDecimalExact portBytes with
          | some host, some port => some (host, port)
          | _, _ => none
        | _ => none
      else none
    | _ => none
  | _ => none

/-- The node connection at a given host/port, if connected. Mirrors
    upstream's `nodeConnWithHostAndPort`. -/
def nodeConnWithHostAndPort (conn : Connection) (shardMap : ShardMap) (host : Host) (port : Port) :
    Option NodeConnection :=
  match shardMap.nodeWithHostAndPort host port with
  | none => none
  | some n => conn.nodes.get? n.id

/-- Retry a batch of requests (all belonging to a single logical request, or
    to a `MULTI`/`EXEC` transaction in upstream — see the module
    doc-comment for why this port only ever calls it with a singleton list)
    if the *last* reply is a `MOVED`/`ASK` redirect. Mirrors upstream's
    `retryBatch`, minus the one-retry-then-give-up loop upstream uses for a
    still-unknown `ASK` target: ported as unconditional refresh-then-retry
    exactly once (upstream's `retryCount` parameter distinguishes "haven't
    refreshed yet" from "already refreshed and still missing" — represented
    here directly by whether `refreshShardMap` succeeds in finding the
    node). No recursion (structural or otherwise): unlike upstream's
    self-recursive `retryBatch`, which calls itself with `retryCount + 1`
    exactly once before giving up, this port inlines both attempts as a
    single straight-line sequence (try, refresh, try again, else throw) —
    still exactly bounded to one refresh-and-retry, just without needing a
    recursive call (or a termination proof for one) to express that bound. -/
def retryBatch (conn : Connection) (refreshAction : IO ShardMap) (requests : List (List ByteArray))
    (replies : List Reply) : IO (List Reply) := do
  match replies.getLast? with
  | none => pure replies
  | some lastReply =>
    if moved lastReply then
      let keys := (← requests.mapM (requestKeys conn.infoMap)).flatten
      let hashSlot ← hashSlotForKeys requests keys
      let nodeConn ← nodeConnForHashSlot conn hashSlot
      requestNode nodeConn requests
    else
      match askingRedirection lastReply with
      | none => pure replies
      | some (host, port) => do
        let shardMap ← conn.shardMap.get
        match nodeConnWithHostAndPort conn shardMap host port with
        | some askNode =>
          match ← requestNode askNode (["ASKING".toUTF8] :: requests) with
          | _ :: rest => pure rest
          | [] => throw (IO.userError "Redis.Cluster: impossible: requestNode returned no replies")
        | none =>
          let newMap ← refreshAction
          conn.shardMap.set newMap
          match nodeConnWithHostAndPort conn newMap host port with
          | some askNode =>
            match ← requestNode askNode (["ASKING".toUTF8] :: requests) with
            | _ :: rest => pure rest
            | [] => throw (IO.userError "Redis.Cluster: impossible: requestNode returned no replies")
          | none =>
            throw (IO.userError
              s!"Redis.Cluster: missing node connection for ASK redirect to {host}:{port}")

/-- Group a batch of pending requests by the node connection(s) that should
    receive them (see `nodeConnectionForCommand`: most requests go to
    exactly one node, but a handful of cluster-wide commands broadcast to
    every master). Mirrors the `getRequestsByNode` inner-helper of
    upstream's `evaluatePipeline`. -/
private def groupRequestsByNode (shardMap : ShardMap) (conn : Connection)
    (requests : List (List ByteArray)) :
    IO (List (NodeConnection × List (Nat × List ByteArray))) := do
  let indexed := requests.zipIdx
  let pairs ← indexed.mapM (fun (request, idx) => do
    let nodeConns ← nodeConnectionForCommand conn shardMap request
    pure (nodeConns.map (fun nc => (nc, (idx, request)))))
  let grouped := pairs.flatten.foldl
    (fun (m : Std.HashMap NodeID (NodeConnection × List (Nat × List ByteArray))) (nc, pr) =>
      match m.get? nc.nodeId with
      | some (nc', prs) => m.insert nc.nodeId (nc', prs ++ [pr])
      | none => m.insert nc.nodeId (nc, [pr]))
    {}
  pure (grouped.toList.map (·.2))

/-- Run a batch of requests, grouping them by target node so that each
    node's sub-batch is pipelined (all of that node's requests written
    before any of its replies are read), then individually retry any
    `MOVED`/`ASK`-redirected replies, refreshing the shard map first if any
    reply was a `MOVED`. Mirrors upstream's `evaluatePipeline`.

    Note: for a broadcast command (see `nodeConnectionForCommand`) the
    output can contain more replies than `requests` had entries — one per
    master node it was sent to — exactly mirroring upstream's own behaviour
    (each master's reply keeps the *same* original index, and the final
    `sortBy`-on-index is stable, so they appear consecutively). -/
def evaluateBatch (conn : Connection) (refreshAction : IO ShardMap)
    (requests : List (List ByteArray)) : IO (List Reply) := do
  let shardMap ← conn.shardMap.get
  let grouped ← groupRequestsByNode shardMap conn requests
  let resultsByNode ← grouped.mapM (fun (nc, prs) => do
    let replies ← requestNode nc (prs.map (·.2))
    pure (List.zip (prs.map (·.1)) replies))
  let flat := resultsByNode.flatten
  if flat.any (fun (_, r) => moved r) then
    let newMap ← refreshAction
    conn.shardMap.set newMap
  let retried ← flat.mapM (fun (idx, r) => do
    let request := requests.getD idx []
    let rs ← retryBatch conn refreshAction [request] [r]
    pure (idx, rs.headD r))
  pure ((retried.mergeSort (fun a b => Nat.ble a.1 b.1)).map (·.2))

/-- Send a single request, routed by key hash slot (or broadcast, for the
    handful of cluster-wide commands), retrying on `MOVED`/`ASK` redirects.
    Mirrors upstream's `requestPipelined` — minus the implicit-pipelining
    and `MULTI`/`EXEC` machinery it also implements; see the module
    doc-comment. -/
def requestPipelined (refreshAction : IO ShardMap) (conn : Connection) (request : List ByteArray) :
    IO Reply := do
  match ← evaluateBatch conn refreshAction [request] with
  | r :: _ => pure r
  | [] => throw (IO.userError "Redis.Cluster: no reply received for request")

/-- Every currently-connected master node's connection. Mirrors upstream's
    `masterNodes`. -/
def masterNodes (conn : Connection) : IO (List NodeConnection) := do
  let shardMap ← conn.shardMap.get
  let masters := shardMap.slots.toList.map (fun (_, s) => s.master)
  pure ((dedupNodes {} masters).filterMap (fun n => conn.nodes.get? n.id))

/-- Send a request to every master node in the cluster (e.g. for
    `FLUSHALL`/`CONFIG SET`). Mirrors upstream's `requestMasterNodes`. -/
def requestMasterNodes (conn : Connection) (request : List ByteArray) : IO (List Reply) := do
  let masterConns ← masterNodes conn
  let repliesLists ← masterConns.mapM (fun nc => requestNode nc [request])
  pure repliesLists.flatten

/-- A connection to some node other than `nc`, if the cluster has more than
    one node; otherwise (or if `conn` has no connections at all) an
    arbitrary one. Mirrors upstream's `getRandomConnection` — except where
    upstream crashes (`head conns`) on a `Connection` with no node
    connections at all, this port returns `none` instead. (As elsewhere in
    this import, e.g. `RedisResult RedisType`'s decode fallback in
    `Database.Redis.Types`, AGENTS.md's ban on introducing crashes takes
    precedence over matching an upstream partial function litearlly; this
    is the same treatment, applied here for the same reason.) -/
def getRandomConnection (nc : NodeConnection) (conn : Connection) : Option NodeConnection :=
  let conns := conn.nodes.toList.map (·.2)
  match conns.find? (fun c => !(NodeConnection.sameNode nc c)) with
  | some c => some c
  | none => conns.head?

end Database.Redis.Cluster
