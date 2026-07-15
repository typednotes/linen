/-
  Linen.Database.Redis.Core — the public `Redis` monad and request dispatch

  ## Haskell source
  `Database.Redis.Core` from https://hackage.haskell.org/package/hedis
  (module 10 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/Core.hs`. Exposes `RedisCtx`, `MonadRedis`, `send`,
  `recv`, `sendRequest`, `runRedisInternal`, `runRedisClusteredInternal`.

  ## Class hierarchy
  Upstream declares `RedisCtx m f` with a superclass constraint
  `(MonadRedis m) => RedisCtx m f`, so any context with a `RedisCtx`
  instance automatically has a `MonadRedis` one too. This port keeps the two
  classes separate (`RedisCtx` does not `extend` `MonadRedis`) and instead
  requires `[MonadRedis m]` explicitly alongside `[RedisCtx m f]` wherever
  upstream would rely on the superclass — a purely notational difference
  (nothing upstream's superclass constraint provides is unavailable here;
  it is simply spelled out at each use site instead of threaded implicitly
  through class dictionaries), chosen to keep instance resolution
  predictable without relying on Lean's `extends`-triggered auto-synthesis
  of omitted parent fields.

  Upstream's `{-# OVERLAPPABLE #-} instance (MonadTrans t, MonadRedis m,
  Monad (t m)) => MonadRedis (t m)` — letting any monad transformer stack
  built on top of a `MonadRedis` base automatically get one too — is not
  ported: Lean's typeclass resolution has no overlapping-instance
  mechanism (see `Database.Redis.Types`' doc-comment for the same
  limitation, there affecting `RedisResult (List (k, v))` vs.
  `RedisResult (List a)`), and no module in this batch (#1–10) stacks a
  transformer on top of `Redis` — that is for `Database.Redis.Transactions`/
  `PubSub` (modules #14/#15, out of scope) to add if and when they need it.

  ## Deviations
  Upstream's `recv`/`send` read the connection out of `RedisEnv` via
  `asks envConn` — a GHC record-selector for a field that exists on
  *only* the `NonClusteredEnv` constructor, so calling either function
  against a `ClusteredEnv` crashes with a partial-pattern-match exception
  at runtime. AGENTS.md forbids introducing crashes; here, that case
  throws a descriptive `IO.userError` instead (the same "upstream crash →
  safe failure" treatment already used for `RedisResult RedisType`'s decode
  fallback in `Database.Redis.Types` and `getRandomConnection` in
  `Database.Redis.Cluster`).
-/
import Linen.Control.Monad.Reader
import Linen.Database.Redis.Cluster
import Linen.Database.Redis.Core.Internal
import Linen.Database.Redis.Hooks
import Linen.Database.Redis.ProtocolPipelining
import Linen.Database.Redis.Types

namespace Database.Redis.Core

open Control.Monad.Reader (ask)
open Database.Redis.Core.Internal (Redis RedisEnv envLastReply)
open Database.Redis.Protocol (Reply renderRequest)
open Database.Redis.Types (RedisResult decode)

-- `SendPubSubHook`/`CallbackHook` are not exported here: they are typed in
-- terms of `Database.Redis.PubSub`'s `Message`/`PubSub`, out of scope for
-- this batch (see `Database.Redis.Hooks`'s module doc-comment).
export Database.Redis.Hooks (Hooks SendRequestHook SendHook ReceiveHook defaultHooks)
export Database.Redis.Core.Internal (RedisEnv Redis unRedis reRedis)

-- ── The `Redis` monad's dispatch classes ──

/-- A context in which `Redis` actions can be run. Mirrors upstream's
    `MonadRedis` (see the module doc-comment for why this is not declared
    as `RedisCtx`'s superclass here). -/
class MonadRedis (m : Type → Type) [Monad m] where
  /-- Run a `Redis` action in `m`. -/
  liftRedis {α : Type} : Redis α → m α

/-- A context `m` in which a command's result comes back wrapped in a
    "container" `f` (e.g. `Except Reply` for plain `Redis`, or `Identity`
    for a `MULTI`/`EXEC` transaction context in the modules this batch
    doesn't port). Mirrors upstream's `RedisCtx`. -/
class RedisCtx (m : Type → Type) (f : outParam (Type → Type)) [Monad m] [MonadRedis m] where
  /-- Decode a `Reply` (via its `RedisResult` instance), wrapped in `f`. -/
  returnDecode {α : Type} [RedisResult α] : Reply → m (f α)

instance : MonadRedis Redis where
  liftRedis := id

instance : RedisCtx Redis (Except Reply) where
  returnDecode r := pure (decode r)

-- ── Running `Redis` actions ──

/-- Internal version of `runRedis` that does not depend on the `Connection`
    abstraction. Used to run the `AUTH` command when connecting. Mirrors
    upstream's `runRedisInternal`. -/
def runRedisInternal (conn : Database.Redis.ProtocolPipelining.Connection) (redis : Redis α) :
    IO α := do
  -- Dummy reply in case no request is sent.
  let ref ← IO.mkRef (Reply.singleLine "nobody will ever see this".toUTF8)
  redis.run (RedisEnv.nonClustered conn ref)

/-- Mirrors upstream's `runRedisClusteredInternal`. -/
def runRedisClusteredInternal (connection : Database.Redis.Cluster.Connection)
    (refreshShardmapAction : IO Database.Redis.Cluster.ShardMap) (redis : Redis α) : IO α := do
  let ref ← IO.mkRef (Reply.singleLine "no reply yet".toUTF8)
  redis.run (RedisEnv.clustered refreshShardmapAction connection ref)

/-- Record `r` as the environment's most recently received reply. Mirrors
    upstream's `setLastReply`. -/
private def setLastReply (r : Reply) : Redis Unit := do
  (envLastReply (← ask)).set r

-- ── Sending/receiving on the environment's connection ──

/-- Receive the next reply on the environment's (non-clustered) connection.
    Mirrors upstream's `recv` — see the module doc-comment for the
    `ClusteredEnv` deviation. -/
def recv [Monad m] [MonadRedis m] : m Reply :=
  MonadRedis.liftRedis (show Redis Reply from do
    match ← ask with
    | .nonClustered conn _ =>
      let r ← (Database.Redis.ProtocolPipelining.recv conn : IO Reply)
      setLastReply r
      pure r
    | .clustered .. =>
      (throw (IO.userError "Redis.Core: recv is only valid for a non-clustered connection") :
        IO Reply))

/-- Send a request on the environment's (non-clustered) connection, without
    reading its reply. Mirrors upstream's `send` — see the module
    doc-comment for the `ClusteredEnv` deviation. -/
def send [Monad m] [MonadRedis m] (req : List ByteArray) : m Unit :=
  MonadRedis.liftRedis (show Redis Unit from do
    match ← ask with
    | .nonClustered conn _ =>
      (Database.Redis.ProtocolPipelining.send conn (renderRequest req) : IO Unit)
    | .clustered .. =>
      (throw (IO.userError "Redis.Core: send is only valid for a non-clustered connection") :
        IO Unit))

/-- Send a request and decode its reply. Dispatches through the
    environment's `Hooks.sendRequestHook`, and to either a plain request
    (non-clustered) or `Database.Redis.Cluster.requestPipelined`
    (clustered). Mirrors upstream's `sendRequest`; can be used to implement
    commands from experimental Redis versions not otherwise covered by this
    library. -/
def sendRequest [Monad m] [MonadRedis m] [RedisCtx m f] [RedisResult α] (req : List ByteArray) :
    m (f α) := do
  let r ← MonadRedis.liftRedis (show Redis Reply from do
    match ← ask with
    | .nonClustered conn _ =>
      let r ← (conn.hooks.sendRequestHook
        (fun args => Database.Redis.ProtocolPipelining.request conn (renderRequest args)) req :
        IO Reply)
      setLastReply r
      pure r
    | .clustered refreshAction connection _ =>
      let r ← (connection.hooks.sendRequestHook
        (fun args => Database.Redis.Cluster.requestPipelined refreshAction connection args) req :
        IO Reply)
      setLastReply r
      pure r)
  RedisCtx.returnDecode r

end Database.Redis.Core
