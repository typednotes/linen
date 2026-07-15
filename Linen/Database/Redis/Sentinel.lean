/-
  Linen.Database.Redis.Sentinel — a `Database.Redis`-like interface that
  connects through Redis Sentinel.

  Ported from `hedis`'s `Database.Redis.Sentinel`
  (https://hackage.haskell.org/package/hedis-0.16.1/src/src/Database/Redis/Sentinel.hs),
  module #18 (the last) of the `hedis` import (see
  `docs/imports/hedis/dependencies.md`). More on Redis Sentinel:
  <https://redis.io/topics/sentinel>. This implementation follows a Gist by
  Emanuel Borsboom, <https://gist.github.com/borsboom/681d37d273d5c4168723>.

  When a `SentinelConnection` is opened, the configured Sentinels are queried
  (`SENTINEL get-master-addr-by-name`) to discover the current master;
  subsequent `runRedis` calls talk to that master. If a `runRedis` call fails
  (or the connection turns into a replica, reported by a `READONLY` error),
  the next call re-queries the Sentinels and reconnects to the new master.

  Built **on top of the public facade** (`Linen.Database.Redis`), exactly as
  upstream builds on `import Database.Redis hiding (Connection, connect,
  runRedis)` plus a qualified `import qualified Database.Redis as Redis`:
  this module defines its *own* `SentinelConnection`, `connect`, and `runRedis`
  wrapping the underlying `Database.Redis.Connection.{Connection,connect,
  runRedis}` with sentinel-discovery/failover logic. The underlying ones are
  referred to by their fully-qualified `Database.Redis.Connection.*` names
  (mirroring upstream's `Redis.` qualifier), so the two `connect`/`runRedis`
  pairs coexist without clashing.

  ## Substitutions / deviations

  - **`exceptions` (`Control.Monad.Catch`) → direct `IO`/`Except` handling.**
    Upstream's `catchRedis`/`catchRedisRethrow` use `Control.Monad.Catch`'s
    `Handler`/`catches`/`throwM` to catch two distinct exception *types*
    (`IOException` and `ConnectionLostException`) by GHC generics. Per
    `dependencies.md`'s `exceptions` substitution note — and exactly as
    `Database.Redis.Connection` handles the same situation — Lean's `IO.Error`
    is not an open catch-by-type hierarchy, and both of those upstream
    exception types are already modelled here as `IO.userError` values
    (`Database.Redis.ConnectionContext.connectionLostError` and plain
    `IO` errors). So the two helpers become a single `try … catch` over
    `IO.Error`, passing the error's rendered text to the handler — a faithful
    rendering of "catch either of the two failure shapes". `throwM ex` in
    `catchRedisRethrow` becomes a re-`throw` of the caught `IO.Error`.

  - **`MVar SentinelConnection'` → `IO.Ref SentinelConnection'`.** As
    throughout this import (see `Database.Redis.Connection`/`Cluster`), this
    batch does not port a concurrency-safety story; `IO.Ref` preserves the
    same mutable reference-cell shape. Upstream's `modifyMVar`/`modifyMVar_`
    (which hold the lock across the enclosed `IO` action) become a
    read-then-`IO`-then-write on the `Ref`. The `Data.Unique` token that
    upstream uses to detect concurrent failovers is carried faithfully (via
    `Linen.Data.Unique`), so the `setCheckSentinel` "only flip the flag if the
    token is unchanged" guard is preserved verbatim even though this port has
    no true concurrent writer.

  - **`Data.List.NonEmpty` → `Linen.Data.List.NonEmpty`.** Same substitution
    already used by `Commands`/`ManualCommands`. `(host, port) :| rest`
    becomes `⟨(host, port), rest⟩`; `delete` becomes `List.erase`.

  - **`RedisSentinelException (NoSentinels …)` → `noSentinelsError`.** Upstream
    derives an `Exception` instance for `RedisSentinelException`; as with every
    other exception type in this import, it becomes a descriptive
    `IO.userError` naming the sentinels that could not be reached.

  - **`Show SentinelConnectInfo` not derived.** Upstream derives `Show`.
    `connectBaseInfo : ConnectInfo` carries a function-valued `connectHooks`
    field (see `Database.Redis.Connection`), which is not `Repr`/`Show`-able,
    so no `Repr`/`ToString` is derived here. No functionality depends on it.

  - **`evaluate` is dropped.** Upstream wraps `runRedis` in `evaluate` to force
    a lazily-thrown exception before the `catch` scope ends. Lean `IO` is
    strict and effect-ordered, so the exception is already raised inside the
    `try`; no forcing combinator is needed.
-/
import Linen.Database.Redis
import Linen.Data.List.NonEmpty
import Linen.Data.Unique

namespace Database.Redis.Sentinel

open Database.Redis.Core (Redis)
open Database.Redis.Protocol (Reply)
open Data.List (NonEmpty)
open Data (Unique)

-- ────────────────────────────────────────────────────────────────────
-- Configuration & connection types
-- ────────────────────────────────────────────────────────────────────

/-- Configuration of the Sentinel hosts. Mirrors upstream's
    `SentinelConnectInfo` (`Show` is not derived — see the module
    doc-comment). -/
structure SentinelConnectInfo where
  /-- The list of sentinels as `(host, port)` pairs (non-empty). -/
  connectSentinels : NonEmpty (String × UInt16)
  /-- Name of the master to connect to. -/
  connectMasterName : ByteArray
  /-- Used to configure auth and other parameters for the Redis connection;
      its `connectAddr` is ignored (overwritten with the discovered master). -/
  connectBaseInfo : Database.Redis.Connection.ConnectInfo

/-- The internal, mutable sentinel-connection state. Mirrors upstream's
    `SentinelConnection'` record. -/
structure SentinelConnection' where
  /-- Whether the next `runRedis` must re-query the sentinels first. -/
  rcCheckFailover : Bool
  /-- Token identifying the current topology generation (see the module
      doc-comment on the `MVar` → `IO.Ref` substitution). -/
  rcToken : Unique
  /-- The (possibly reordered) sentinel configuration. -/
  rcSentinelConnectInfo : SentinelConnectInfo
  /-- The connect info of the current master. -/
  rcMasterConnectInfo : Database.Redis.Connection.ConnectInfo
  /-- The live connection to the current master. -/
  rcBaseConnection : Database.Redis.Connection.Connection

/-- A Sentinel-aware connection. Mirrors upstream's `SentinelConnection`
    newtype (an `MVar` upstream; an `IO.Ref` here). -/
structure SentinelConnection where
  /-- The mutable cell guarding the connection state. -/
  ref : IO.Ref SentinelConnection'

-- ────────────────────────────────────────────────────────────────────
-- Errors (see the module doc-comment: upstream's `RedisSentinelException`
-- becomes a descriptive `IO.userError`)
-- ────────────────────────────────────────────────────────────────────

/-- No sentinel could be reached (upstream's `NoSentinels`). -/
def noSentinelsError (sentinels : NonEmpty (String × UInt16)) : IO.Error :=
  let rendered := ", ".intercalate <| sentinels.toList.map fun (h, p) => s!"{h}:{p}"
  IO.userError s!"Redis.Sentinel: no sentinel could be reached ({rendered})"

-- ────────────────────────────────────────────────────────────────────
-- Exception helpers (the `exceptions` substitution — see the module
-- doc-comment)
-- ────────────────────────────────────────────────────────────────────

/-- Run `action`; on any `IO.Error`, invoke `handler` with the error's text
    and then re-raise. Mirrors upstream's `catchRedisRethrow`. -/
def catchRedisRethrow (action : IO α) (handler : String → IO Unit) : IO α := do
  try
    action
  catch e =>
    handler (toString e)
    throw e

/-- Run `action`; on any `IO.Error`, recover via `handler` (given the error's
    text). Mirrors upstream's `catchRedis`. -/
def catchRedis (action : IO α) (handler : String → IO α) : IO α := do
  try
    action
  catch e =>
    handler (toString e)

-- ────────────────────────────────────────────────────────────────────
-- Small pure helpers
-- ────────────────────────────────────────────────────────────────────

/-- Two connect infos target the same host. Mirrors upstream's local
    `sameHost` (`connectAddr l == connectAddr r`); written out here because
    `ConnectAddr` carries no `BEq` instance. -/
private def sameHost (l r : Database.Redis.Connection.ConnectInfo) : Bool :=
  match l.connectAddr, r.connectAddr with
  | .hostPort h1 p1, .hostPort h2 p2 => h1 == h2 && p1 == p2
  | .unixSocket a, .unixSocket b => a == b
  | _, _ => false

/-- Parse a port number from a reply payload, defaulting to 26379. Mirrors
    upstream's `maybe 26379 (fromIntegral . fst) $ BS8.readInt port`
    (`readInt` reads the leading decimal digits). -/
private def readPort (bs : ByteArray) : UInt16 :=
  match String.fromUTF8? bs with
  | none => 26379
  | some s =>
    match (s.takeWhile Char.isDigit).toNat? with
    | some n => n.toUInt16
    | none => 26379

-- ────────────────────────────────────────────────────────────────────
-- Master discovery (updateMaster)
-- ────────────────────────────────────────────────────────────────────

/-- Ask one sentinel for the current master's address. Returns the master
    `ConnectInfo` (built from `connectBaseInfo` with its `connectAddr`
    overwritten by the discovered host/port) paired with this working
    sentinel, or `none` if the sentinel is unreachable or gives no answer.
    Mirrors the body of upstream's `trySentinel` (the `bracket` opening a
    single-connection client to the sentinel, the `SENTINEL
    get-master-addr-by-name` request, and the `[host, port]` decode), with the
    whole thing wrapped in `catchRedis` so a failure yields `none`. -/
private def trySentinel (sci : SentinelConnectInfo) (host : String) (port : UInt16) :
    IO (Option (Database.Redis.Connection.ConnectInfo × (String × UInt16))) :=
  catchRedis
    (do
      let replyE ← do
        let sentinelConn ← Database.Redis.Connection.connect
          { Database.Redis.Connection.defaultConnectInfo with
            connectAddr := .hostPort host port
            connectMaxConnections := 1 }
        try
          (Database.Redis.Connection.runRedis sentinelConn
            (Database.Redis.Core.sendRequest
              ["SENTINEL".toUTF8, "get-master-addr-by-name".toUTF8, sci.connectMasterName]) :
            IO (Except Reply (List ByteArray)))
        finally
          Database.Redis.Connection.disconnect sentinelConn
      match replyE with
      | .ok [mHost, mPort] =>
        let masterInfo : Database.Redis.Connection.ConnectInfo :=
          { sci.connectBaseInfo with
            connectAddr := .hostPort ((String.fromUTF8? mHost).getD "") (readPort mPort) }
        pure (some (masterInfo, (host, port)))
      | _ => pure none)
    (fun _ => pure none)

/-- Walk the sentinel list, returning the first working sentinel's answer.
    Structural recursion over the list — the "`Either` used backwards"
    (`Left` = stop, `Right` = try again) `ExceptT`/`forM_` loop of upstream's
    `updateMaster` becomes an explicit first-success search. -/
private def findMaster (sci : SentinelConnectInfo) :
    List (String × UInt16) →
    IO (Option (Database.Redis.Connection.ConnectInfo × (String × UInt16)))
  | [] => pure none
  | (host, port) :: rest => do
    match ← trySentinel sci host port with
    | some result => pure (some result)
    | none => findMaster sci rest

/-- Query the configured sentinels for the current master. On success returns
    the sentinel config with the working sentinel moved to the front, together
    with the master's `ConnectInfo`; if no sentinel answers, throws
    `noSentinelsError`. Mirrors upstream's `updateMaster`. -/
def updateMaster (sci : SentinelConnectInfo) :
    IO (SentinelConnectInfo × Database.Redis.Connection.ConnectInfo) := do
  match ← findMaster sci sci.connectSentinels.toList with
  | some (masterInfo, workingPair) =>
    let reordered : NonEmpty (String × UInt16) :=
      ⟨workingPair, sci.connectSentinels.toList.erase workingPair⟩
    pure ({ sci with connectSentinels := reordered }, masterInfo)
  | none => throw (noSentinelsError sci.connectSentinels)

-- ────────────────────────────────────────────────────────────────────
-- connect / runRedis
-- ────────────────────────────────────────────────────────────────────

/-- Open a Sentinel-aware connection: discover the current master through the
    sentinels, connect to it, and store the state behind an `IO.Ref`. Mirrors
    upstream's `connect`. -/
def connect (origConnectInfo : SentinelConnectInfo) : IO SentinelConnection := do
  let (connectInfo, masterConnectInfo) ← updateMaster origConnectInfo
  let conn ← Database.Redis.Connection.connect masterConnectInfo
  let token ← Data.newUnique
  let ref ← IO.mkRef
    { rcCheckFailover := false
      rcToken := token
      rcSentinelConnectInfo := connectInfo
      rcMasterConnectInfo := masterConnectInfo
      rcBaseConnection := conn }
  pure ⟨ref⟩

/-- Flip the failover flag so the next `runRedis` re-queries the sentinels —
    but only if the topology generation (`rcToken`) has not changed since the
    request began, so a concurrent failover is not clobbered. Mirrors
    upstream's local `setCheckSentinel` (`modifyMVar_` → read/write on the
    `Ref`). -/
private def setCheckSentinel (ref : IO.Ref SentinelConnection') (preToken : Unique) :
    IO Unit := do
  let conn ← ref.get
  if preToken == conn.rcToken then
    let newToken ← Data.newUnique
    ref.set { conn with rcToken := newToken, rcCheckFailover := true }
  else pure ()

/-- Run a `Redis` action against the current master, re-discovering the master
    first if a previous call flagged a failover. On an exception, or on a
    `READONLY` error reply (the connection has become a replica), flag the next
    call to re-check the sentinels. Mirrors upstream's `runRedis`. -/
def runRedis (sc : SentinelConnection) (action : Redis (Except Reply α)) :
    IO (Except Reply α) := do
  let old ← sc.ref.get
  let (baseConn, preToken) ←
    if old.rcCheckFailover then do
      let (newConnectInfo, newMasterConnectInfo) ← updateMaster old.rcSentinelConnectInfo
      let newToken ← Data.newUnique
      let (connInfo, conn) ←
        if sameHost newMasterConnectInfo old.rcMasterConnectInfo then
          pure (old.rcMasterConnectInfo, old.rcBaseConnection)
        else do
          let newConn ← Database.Redis.Connection.connect newMasterConnectInfo
          Database.Redis.Connection.disconnect old.rcBaseConnection
          pure (newMasterConnectInfo, newConn)
      sc.ref.set
        { rcCheckFailover := false
          rcToken := newToken
          rcSentinelConnectInfo := newConnectInfo
          rcMasterConnectInfo := connInfo
          rcBaseConnection := conn }
      pure (conn, newToken)
    else
      pure (old.rcBaseConnection, old.rcToken)
  let reply ← catchRedisRethrow
    (Database.Redis.Connection.runRedis baseConn action)
    (fun _ => setCheckSentinel sc.ref preToken)
  match reply with
  | .error (.error e) =>
    -- The connection turned into a replica.
    if (String.fromUTF8? e).elim false (·.startsWith "READONLY ") then
      setCheckSentinel sc.ref preToken
    else pure ()
  | _ => pure ()
  pure reply

end Database.Redis.Sentinel
