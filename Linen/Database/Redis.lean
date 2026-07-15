/-
  Linen.Database.Redis — the top-level public facade for the Redis client.

  ## Haskell source
  `Database.Redis` from https://hackage.haskell.org/package/hedis
  (module 16 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis.hs`. Upstream this module contains no logic of its own:
  it is a curated re-export list that assembles the package's public surface
  under a single `Database.Redis` namespace. This port does the same, using
  Lean's `export` command inside `namespace Database.Redis` so that every
  re-exported name is reachable directly as `Database.Redis.<name>` without
  opening the individual submodules.

  ## What is re-exported (mirroring upstream's export list)
  - **The Redis monad** (`Database.Redis.Core`): `Redis`, `unRedis`,
    `reRedis`, `RedisCtx`, `MonadRedis`, plus the low-level `sendRequest`.
  - **Connection** (`Database.Redis.Connection`): `runRedis`,
    `runRedisNonBlocking`, `connect`, `checkedConnect`, `connectCluster`,
    `checkedConnectCluster`, `disconnect`, `withConnect`, `withCheckedConnect`,
    `Connection`, `ConnectInfo`, `defaultConnectInfo`.
  - **Connection-string parsing** (`Database.Redis.URL`): `parseConnectInfo`.
  - **The raw connection handle** (`Database.Redis.ConnectionContext`):
    `ConnectAddr`.
  - **Low-level reply / status / decoding** (`Database.Redis.Protocol`,
    `Database.Redis.Types`): `Reply`, `Status`, `RedisArg`, `RedisResult`.
  - **Hooks** (`Database.Redis.Hooks`): `Hooks`, `SendRequestHook`,
    `SendPubSubHook`, `CallbackHook`, `SendHook`, `ReceiveHook`,
    `defaultHooks`.
  - **Cluster hash slots** (`Database.Redis.Cluster.HashSlot`): `HashSlot`,
    `keyToSlot`.
  - **The whole command surface** (`Database.Redis.Commands` and
    `Database.Redis.ManualCommands`, upstream's `module Database.Redis.Commands`
    re-export, which itself re-exports the manual commands).
  - **Transactions** (`Database.Redis.Transactions`) and **Pub/Sub**
    (`Database.Redis.PubSub`), upstream's two whole-module re-exports.

  ## Deviations
  Upstream re-exports several *exception types* — `ConnectError`
  (`ConnectAuthError`/`ConnectSelectError`), `ClusterConnectError`,
  `ConnectionLostException`, and `ConnectTimeout`. As documented in
  `Database.Redis.Connection` and `Database.Redis.ConnectionContext`, this
  port does not model them as distinct catch-by-type exceptions (Lean's
  `IO.Error` is not an open type hierarchy); each is instead a descriptive
  `IO.Error`-valued helper (`connectAuthError`, `connectSelectError`,
  `clusterConnectError`, `connectionLostError`, `connectTimeoutError`). Those
  helpers are re-exported here in their place, so the facade still surfaces
  the same conceptual error values under `Database.Redis.*`.

  The intro documentation-only sections of upstream's export list (the "How
  To Use This Module", command-type-signature, Lua-scripting, pipelining, and
  error-behavior prose, plus the exercise solution) carry no exported names;
  their content is preserved as the usage discussion above rather than as
  Haddock anchors.
-/
import Linen.Database.Redis.Cluster.HashSlot
import Linen.Database.Redis.Commands
import Linen.Database.Redis.Connection
import Linen.Database.Redis.ConnectionContext
import Linen.Database.Redis.Core
import Linen.Database.Redis.Hooks
import Linen.Database.Redis.ManualCommands
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.PubSub
import Linen.Database.Redis.Transactions
import Linen.Database.Redis.Types
import Linen.Database.Redis.URL

namespace Database.Redis

-- ── The Redis monad (module 10) ──
export Database.Redis.Core (Redis unRedis reRedis RedisCtx MonadRedis sendRequest)

-- ── Connection (module 13) ──
-- `connectAuthError`/`connectSelectError`/`clusterConnectError` stand in for
-- upstream's `ConnectError`/`ClusterConnectError` exception types (see the
-- module doc-comment's deviation note).
export Database.Redis.Connection (
  runRedis runRedisNonBlocking connect checkedConnect connectCluster
  checkedConnectCluster disconnect withConnect withCheckedConnect Connection
  ConnectInfo defaultConnectInfo connectAuthError connectSelectError
  clusterConnectError)

-- ── Connection-string parsing (module in the URL layer) ──
export Database.Redis.URL (parseConnectInfo)

-- ── The raw connection handle & lost-connection / timeout errors (module 2) ──
-- `connectionLostError`/`connectTimeoutError` stand in for upstream's
-- `ConnectionLostException`/`ConnectTimeout` exception types.
export Database.Redis.ConnectionContext (ConnectAddr connectionLostError connectTimeoutError)

-- ── Low-level reply / status / decoding (modules 3 and the Types layer) ──
export Database.Redis.Protocol (Reply)
export Database.Redis.Types (Status RedisArg RedisResult)

-- ── Hooks ──
export Database.Redis.Hooks (
  Hooks SendRequestHook SendPubSubHook CallbackHook SendHook ReceiveHook
  defaultHooks)

-- ── Cluster hash slots ──
export Database.Redis.Cluster.HashSlot (HashSlot keyToSlot)

-- ── Commands (module 11): the ~125 uniform commands ──
export Database.Redis.Commands (
  KeyValueReply append bgrewriteaof bgsave bgsaveSchedule bitpos blpop
  blpopFloat brpop brpopFloat brpoplpush clientGetname clientId clientList
  clientPause clientSetname commandCount commandInfo configGet
  configResetstat configRewrite configSet dbsize debugObject decr decrby
  del dump echo expire expireat flushall flushdb get getbit getrange getset
  hdel hexists hget hgetall hincrby hincrbyfloat hkeys hlen hmget hmset
  hset hsetnx hstrlen hvals incr incrby incrbyfloat keys lastsave lindex
  llen lpop lpopCount lpush lpushx lrange lrem lset ltrim mget move mset
  msetnx persist pexpire pexpireat pfadd pfcount pfmerge psetex pttl quit
  randomkey rename renamenx rpop rpopCount rpoplpush rpush rpushx sadd save
  scard scriptExists scriptFlush scriptKill scriptLoad sdiff sdiffstore
  setbit setex setnx setrange sinter sinterstore sismember slaveof smembers
  smove srem strlen sunion sunionstore time ttl wait zcard zcount zincrby
  zlexcount zrank zrankWithScore zrem zremrangebylex zremrangebyrank
  zremrangebyscore zrevrank zrevrankWithScore zscore)

-- ── Manual / irregular commands (module 12) ──
export Database.Redis.ManualCommands (
  Aggregate AuthOpts BitposOpts BitposType ClusterInfoResponse
  ClusterInfoResponseState ClusterNodesResponse ClusterNodesResponseEntry
  ClusterNodesResponseSlotSpec ClusterSlotsNode ClusterSlotsResponse
  ClusterSlotsResponseEntry Condition Cursor DebugMode ExpireOpts FlushOpts
  GeoAddOpts GeoCoordinates GeoLocation GeoOrder GeoSearchBy GeoSearchFrom
  GeoSearchOpts GeoSearchStoreOpts GeoUnit MigrateAuth MigrateOpts RangeLex
  ReplyMode RestoreOpts ScanOpts SetOpts SizeCondition Slowlog SortOpts
  SortOrder StreamsRecord TrimOpts TrimStrategy TrimType XAddOpts
  XAutoclaimJustIdsResult XAutoclaimOpts XAutoclaimResult
  XAutoclaimStreamsResult XClaimOpts XGroupCreateOpts XGroupSetIdOpts
  XInfoConsumersResponse XInfoGroupsResponse XInfoStreamResponse
  XPendingDetailOpts XPendingDetailRecord XPendingSummaryResponse
  XReadGroupOpts XReadOpts XReadResponse ZaddOpts auth authOpts bitcount
  bitcountRange bitopAnd bitopNot bitopOr bitopXor bitposOpts clientReply
  clusterGetKeysInSlot clusterInfo clusterNodes clusterSetSlotImporting
  clusterSetSlotMigrating clusterSetSlotNode clusterSetSlotStable
  clusterSlots command cursor0 defClusterInfoResponse defaultAuthOpts
  defaultGeoAddOpts defaultGeoSearchOpts defaultGeoSearchStoreOpts
  defaultXAddOpts defaultXAutoclaimOpts defaultXClaimOpts
  defaultXGroupCreateOpts defaultXGroupSetIdOpts defaultXPendingDetailOpts
  defaultXReadGroupOpts defaultXreadOpts eval evalsha expireOpts
  expireatOpts flushallOpts flushdbOpts geoSearch geoSearchStore geoadd
  geoaddOpts geodist geopos getType hscan hscanOpts inf info infoSection
  linsertAfter linsertBefore migrate migrateDefault migrateMultiple
  objectEncoding objectIdletime objectRefcount pexpireatOpts ping restore
  restoreOpts restoreOptsDefault restoreReplace scan scanOpts
  scanOptsDefault scriptDebug select set setDefault setGet setGetOpts
  setOpts slowlogGet slowlogLen slowlogReset sort sortDefault sortStore
  spop spopN srandmember srandmemberN sscan sscanOpts trimOpts xack xadd
  xaddOpts xautoclaim xautoclaimJustIds xautoclaimJustIdsOpts
  xautoclaimOpts xclaim xclaimJustIds xdel xgroupCreate
  xgroupCreateConsumer xgroupCreateOpts xgroupDelConsumer xgroupDestroy
  xgroupSetId xgroupSetIdOpts xinfoConsumers xinfoGroups xinfoStream xlen
  xpendingDetail xpendingSummary xrange xread xreadGroup xreadGroupOpts
  xreadOpts xrevRange xtrim zadd zaddDefault zaddOpts zinterstore
  zinterstoreWeights zrange zrangeWithscores zrangebylex zrangebylexLimit
  zrangebyscore zrangebyscoreLimit zrangebyscoreWithscores
  zrangebyscoreWithscoresLimit zrevrange zrevrangeWithscores
  zrevrangebyscore zrevrangebyscoreLimit zrevrangebyscoreWithscores
  zrevrangebyscoreWithscoresLimit zscan zscanOpts zunionstore
  zunionstoreWeights «exists»)

-- ── Transactions (module 14) ──
export Database.Redis.Transactions (
  Queued RedisTx TxResult multiExec runRedisTx unwatch watch)

-- ── Pub/Sub (module 15) ──
export Database.Redis.PubSub (
  ChannelData MessageCallback PMessageCallback PubSubController PubSubReply
  RedisChannel RedisPChannel UnregisterCallbacksAction UnregisterHandle
  addChannels addChannelsAndWait addChannelsOfType currentChannels
  currentPChannels decodeMsg decodeMsgIO listenThread newChannelData
  newPubSubController pendingChannels pendingPatternChannels pubSub
  pubSubForever pubSubForeverOnConn publish rawSendCmd rawSendPubSub
  removeChannels removeChannels' removeChannelsAndWait sendThread
  waitUntilAbsent withPubSub withPubSubOnConn)

end Database.Redis
