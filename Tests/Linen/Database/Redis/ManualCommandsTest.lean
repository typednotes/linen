/-
  Tests for `Linen.Database.Redis.ManualCommands`.

  Two kinds of checks:

  * **Request-shape checks** — as in `CommandsTest`, each command is run via
    `runRedisInternal` against a loopback TCP server that captures the bytes it
    receives, then compared to `renderRequest` of the expected argument list.
    This exercises the option-record/flag encoding of a representative command
    per category (the interesting, irregular part of this module). The small
    `captureRequest`/`checkRequest` loopback helpers are duplicated from
    `CommandsTest` (they are file-local there).
  * **Pure decode checks** — several types here (`Slowlog`, `StreamsRecord`,
    `GeoLocation`, the `CLUSTER INFO`/`CLUSTER NODES` textual parsers) decode a
    `Reply` into a structured value; those are checked with `#guard` on a
    hand-built `Reply`, needing no connection.
-/
import Linen.Database.Redis.ManualCommands
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Data.List (NonEmpty)
open Database.Redis.ManualCommands
open Database.Redis.Core (Redis runRedisInternal)
open Database.Redis.Protocol (renderRequest Reply)
open Database.Redis.Types (encode decode)

namespace Tests.Database.Redis.ManualCommands

/-! ### Runtime helper: capture the request bytes a command sends -/

/-- Run `action` against a loopback server, returning the raw request bytes
    the server received. -/
def captureRequest (action : Redis α) : IO ByteArray := do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let capTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let bytes ← Network.Socket.Blocking.recv accepted 4096
    Network.Socket.sendAll accepted "$-1\r\n".toUTF8
    let _ ← Network.Socket.close accepted
    pure bytes
  let conn ← Database.Redis.ProtocolPipelining.connect (.hostPort addr.host addr.port)
  let _ ← runRedisInternal conn action
  let mut result : Option ByteArray := none
  for _ in [0:200] do
    if ← IO.hasFinished capTask then
      match capTask.get with
      | .error e => throw e
      | .ok bytes => result := some bytes
      break
    IO.sleep 10
  Database.Redis.ProtocolPipelining.disconnect conn
  let _ ← Network.Socket.close server
  match result with
  | some bytes => pure bytes
  | none => throw (IO.userError "server task did not capture a request within ~2s")

/-- Assert that running `action` sends exactly `renderRequest expected`. -/
def checkRequest (action : Redis (Except Reply α)) (expected : List ByteArray) : IO Unit := do
  let got ← captureRequest action
  unless got == renderRequest expected do
    throw (IO.userError s!"request mismatch: got {got.toList}, expected {(renderRequest expected).toList}")

private def b (s : String) : ByteArray := s.toUTF8

/-! ### Compile-time API-shape checks -/

example [Monad m] [Database.Redis.Core.MonadRedis m] [Database.Redis.Core.RedisCtx m f] :
    ByteArray → ByteArray → SetOpts → m (f Database.Redis.Types.Status) := setOpts
example [Monad m] [Database.Redis.Core.MonadRedis m] [Database.Redis.Core.RedisCtx m f] :
    Cursor → m (f (Cursor × List ByteArray)) := scan
example [Monad m] [Database.Redis.Core.MonadRedis m] [Database.Redis.Core.RedisCtx m f] :
    m (f ClusterInfoResponse) := clusterInfo
example [Monad m] [Database.Redis.Core.MonadRedis m] [Database.Redis.Core.RedisCtx m f] :
    ByteArray → m (f XInfoStreamResponse) := xinfoStream

/-! ### Request-shape checks, one representative per category -/

-- Objects / LINSERT / TYPE / SLOWLOG
#eval checkRequest (objectRefcount (b "k")) [b "OBJECT", b "refcount", b "k"]
#eval checkRequest (getType (b "k")) [b "TYPE", b "k"]
#eval checkRequest (linsertBefore (b "l") (b "p") (b "v")) [b "LINSERT", b "l", b "BEFORE", b "p", b "v"]
#eval checkRequest (slowlogGet 10) [b "SLOWLOG", b "GET", b "10"]

-- Sorted-set ranges
#eval checkRequest (zrangeWithscores (b "z") 0 (-1)) [b "ZRANGE", b "z", b "0", b "-1", b "WITHSCORES"]
#eval checkRequest (zrangebyscoreLimit (b "z") 1.0 5.0 0 10)
  [b "ZRANGEBYSCORE", b "z", encode (1.0 : Float), encode (5.0 : Float), b "LIMIT", b "0", b "10"]
#eval checkRequest (zrangebylex (b "z") .minr (.incl (b "c"))) [b "ZRANGEBYLEX", b "z", b "-", b "[c"]

-- SORT / ZSTORE
#eval checkRequest (sort (b "k") { sortDefault with sortBy := some (b "w_*"), sortAlpha := true })
  [b "SORT", b "k", b "BY", b "w_*", b "LIMIT", b "0", b "-1", b "ASC", b "ALPHA"]
#eval checkRequest (sortStore (b "k") (b "dst") sortDefault)
  [b "SORT", b "k", b "LIMIT", b "0", b "-1", b "ASC", b "STORE", b "dst"]
#eval checkRequest (zunionstore (b "d") [b "z1", b "z2"] .sum)
  [b "ZUNIONSTORE", b "d", b "2", b "z1", b "z2", b "AGGREGATE", b "SUM"]
#eval checkRequest (zinterstoreWeights (b "d") ⟨(b "z1", 1.0), [(b "z2", 2.0)]⟩ .max)
  [b "ZINTERSTORE", b "d", b "2", b "z1", b "z2", b "WEIGHTS", encode (1.0 : Float),
   encode (2.0 : Float), b "AGGREGATE", b "MAX"]

-- Scripting
#eval checkRequest (eval (α := ByteArray) (b "return 1") [b "k1"] [b "a1"])
  [b "EVAL", b "return 1", b "1", b "k1", b "a1"]
#eval checkRequest (scriptDebug .yes) [b "SCRIPT DEBUG", b "YES"]

-- Bit
#eval checkRequest (bitcountRange (b "k") 0 10) [b "BITCOUNT", b "k", b "0", b "10"]
#eval checkRequest (bitopAnd (b "d") [b "s1", b "s2"]) [b "BITOP", b "AND", b "d", b "s1", b "s2"]
#eval checkRequest (bitposOpts (b "k") 1 (.startEnd 0 10 (some .bit)))
  [b "BITPOS", b "k", b "1", b "0", b "10", b "BIT"]

-- MIGRATE / RESTORE
#eval checkRequest (migrateMultiple (b "h") (b "p") 0 5000 ⟨true, false, some (.auth (b "pw"))⟩ [b "k1"])
  [b "MIGRATE", b "h", b "p", b "", b "0", b "5000", b "AUTH", b "pw", b "COPY", b "k1"]
#eval checkRequest (restoreOpts (b "k") 0 (b "val") ⟨true, false, some 100, none⟩)
  [b "RESTORE", b "k", b "0", b "val", b "REPLACE", b "IDLE", b "100"]

-- SET / CLIENT REPLY / random members
#eval checkRequest (setOpts (b "k") (b "v") { setDefault with setSeconds := some 100, setCondition := some .nx })
  [b "SET", b "k", b "v", b "EX", b "100", b "NX"]
#eval checkRequest (setGet (b "k") (b "v")) [b "SET", b "k", b "v", b "GET"]
#eval checkRequest (clientReply .skip) [b "CLIENT REPLY", b "SKIP"]
#eval checkRequest (srandmemberN (b "s") 3) [b "SRANDMEMBER", b "s", b "3"]
#eval checkRequest (spopN (b "s") 2) [b "SPOP", b "s", b "2"]
#eval checkRequest («exists» (b "k")) [b "EXISTS", b "k"]

-- SCAN / ZADD
#eval checkRequest (scanOpts cursor0 ⟨some (b "pat"), some 10⟩ (some (b "string")))
  [b "SCAN", b "0", b "MATCH", b "pat", b "COUNT", b "10", b "TYPE", b "string"]
#eval checkRequest (hscan (b "h") cursor0) [b "HSCAN", b "h", b "0"]
#eval checkRequest (zaddOpts (b "z") [(1.0, b "m")] ⟨some .nx, none, true, false⟩)
  [b "ZADD", b "z", b "NX", b "CH", encode (1.0 : Float), b "m"]
#eval checkRequest (zadd (b "z") [(2.5, b "m")]) [b "ZADD", b "z", encode (2.5 : Float), b "m"]

-- Streams
#eval checkRequest (xaddOpts (b "s") (b "*") [(b "f", b "v")] ⟨some (trimOpts (.maxlen 100) .exact), false⟩)
  [b "XADD", b "s", b "MAXLEN", b "=", b "100", b "*", b "f", b "v"]
#eval checkRequest (xreadOpts [(b "s", b "0")] ⟨some 1000, some 5⟩)
  [b "XREAD", b "BLOCK", b "1000", b "COUNT", b "5", b "STREAMS", b "s", b "0"]
#eval checkRequest (xreadGroupOpts (b "g") (b "c") [(b "s", b ">")] ⟨none, some 5, true⟩)
  [b "XREADGROUP", b "GROUP", b "g", b "c", b "COUNT", b "5", b "NOACK", b "STREAMS", b "s", b ">"]
#eval checkRequest (xgroupCreateOpts (b "s") (b "g") (b "$") ⟨true, none⟩)
  [b "XGROUP", b "CREATE", b "s", b "g", b "$", b "MKSTREAM"]
#eval checkRequest (xack (b "s") (b "g") [b "id1", b "id2"]) [b "XACK", b "s", b "g", b "id1", b "id2"]
#eval checkRequest (xrange (b "s") (b "-") (b "+") (some 10)) [b "XRANGE", b "s", b "-", b "+", b "COUNT", b "10"]
#eval checkRequest (xpendingDetail (b "s") (b "g") (b "-") (b "+") 10 ⟨some (b "cons"), some 5000⟩)
  [b "XPENDING", b "s", b "g", b "IDLE", b "5000", b "-", b "+", b "10", b "cons"]
#eval checkRequest (xclaim (b "s") (b "g") (b "c") 5000 ⟨some 1000, none, none, true⟩ [b "id1"])
  [b "XCLAIM", b "s", b "g", b "c", b "5000", b "id1", b "IDLE", b "1000", b "FORCE"]

-- Geo
#eval checkRequest (geoaddOpts (b "k") [(13.0, 52.0, b "m")] ⟨some .nx, true⟩)
  [b "GEOADD", b "k", b "NX", b "CH", encode (13.0 : Float), encode (52.0 : Float), b "m"]
#eval checkRequest (geodist (b "k") (b "m1") (b "m2") (some .kilometers))
  [b "GEODIST", b "k", b "m1", b "m2", b "km"]
#eval checkRequest (geoSearch (b "k") (.fromLonLat 15.0 37.0) (.byRadius 200.0 .kilometers)
    { defaultGeoSearchOpts with geoSearchWithCoord := true })
  [b "GEOSEARCH", b "k", b "FROMLONLAT", encode (15.0 : Float), encode (37.0 : Float),
   b "BYRADIUS", encode (200.0 : Float), b "km", b "WITHCOORD"]

-- AUTH / SELECT / PING / EXPIRE / FLUSH
#eval checkRequest (authOpts (b "pw") ⟨some (b "user")⟩) [b "AUTH", b "user", b "pw"]
#eval checkRequest (select 2) [b "SELECT", b "2"]
#eval checkRequest ping [b "PING"]
#eval checkRequest (expireOpts (b "k") 100 (.value .gt)) [b "EXPIRE", b "k", b "100", b "GT"]
#eval checkRequest (flushdbOpts .async) [b "FLUSHDB", b "ASYNC"]

-- Cluster
#eval checkRequest (clusterSetSlotStable 42) [b "CLUSTER", b "SETSLOT", b "STABLE", b "42"]
#eval checkRequest (clusterGetKeysInSlot 42 3) [b "CLUSTER", b "GETKEYSINSLOT", b "42", b "3"]
#eval checkRequest clusterInfo [b "CLUSTER", b "INFO"]
#eval checkRequest command [b "COMMAND"]

/-! ### Pure decode checks -/

-- Slowlog: a 4-element entry decodes with no client info.
#guard (match (decode (α := Slowlog)
    (.multiBulk (some [.integer 1, .integer 100, .integer 50,
      .multiBulk (some [.bulk (some (b "get")), .bulk (some (b "x"))])]))) with
  | .ok s => s == ⟨1, 100, 50, [b "get", b "x"], none, none⟩
  | _ => false)

-- StreamsRecord: id plus flat field/value pairs.
#guard (match (decode (α := StreamsRecord)
    (.multiBulk (some [.bulk (some (b "1-1")),
      .multiBulk (some [.bulk (some (b "f")), .bulk (some (b "v"))])]))) with
  | .ok r => r == ⟨b "1-1", [(b "f", b "v")]⟩
  | _ => false)

-- GeoLocation: a bare bulk decodes to just the member.
#guard (match (decode (α := GeoLocation) (.bulk (some (b "Palermo")))) with
  | .ok loc => loc == ⟨b "Palermo", none, none, none⟩
  | _ => false)

-- CLUSTER INFO: textual reply parses into the structured response.
#guard (match (decode (α := ClusterInfoResponse)
    (.bulk (some (b "cluster_state:ok\r\ncluster_slots_assigned:16384\r\ncluster_known_nodes:6\r\n")))) with
  | .ok resp => resp.state == .ok && resp.slotsAssigned == 16384 && resp.knownNodes == 6
  | _ => false)

-- CLUSTER NODES: one master line with a slot range.
#guard (match (decode (α := ClusterNodesResponse)
    (.bulk (some (b "abc 127.0.0.1:7000@17000 master - 0 0 5 connected 0-16383\n")))) with
  | .ok resp =>
    match resp.entries with
    | [e] => e.nodePort == 7000 && e.masterId == none && e.slots == [.slotRange 0 16383]
    | _ => false
  | _ => false)

end Tests.Database.Redis.ManualCommands
