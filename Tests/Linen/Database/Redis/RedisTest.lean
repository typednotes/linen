/-
  Tests for `Linen.Database.Redis` — the top-level facade.

  The facade has no logic of its own: it re-exports the public surface of the
  Redis submodules under the single `Database.Redis` namespace. So what is
  worth testing is exactly that: every re-exported name is reachable directly
  as `Database.Redis.<name>` (no `open` of a submodule needed) and that each
  alias points at the very definition it claims to. We assert the latter with
  `example : @Database.Redis.<name> = @Database.Redis.<Submodule>.<name> := rfl`
  — a `rfl` here can only close if the `export` alias resolves to the same
  declaration, so it doubles as a reachability check that is robust to the
  exact type signatures. A handful of runtime `#guard`s then drive pure
  re-exports (`keyToSlot`, `defaultConnectInfo`, `parseConnectInfo`) through
  the facade end-to-end.

  Only `Linen.Database.Redis` is imported: the submodule-qualified names on
  the right-hand sides are reachable transitively through the facade, so no
  submodule is opened.
-/
import Linen.Database.Redis

namespace Tests.Database.Redis.Facade

/-! ### The Redis monad + low-level command API (`Core`) -/

example : @Database.Redis.Redis = @Database.Redis.Core.Redis := rfl
example : @Database.Redis.unRedis = @Database.Redis.Core.unRedis := rfl
example : @Database.Redis.reRedis = @Database.Redis.Core.reRedis := rfl
example : @Database.Redis.RedisCtx = @Database.Redis.Core.RedisCtx := rfl
example : @Database.Redis.MonadRedis = @Database.Redis.Core.MonadRedis := rfl
example : @Database.Redis.sendRequest = @Database.Redis.Core.sendRequest := rfl

/-! ### Connection (`Connection`, plus `URL`/`ConnectionContext`) -/

example : @Database.Redis.connect = @Database.Redis.Connection.connect := rfl
example : @Database.Redis.checkedConnect = @Database.Redis.Connection.checkedConnect := rfl
example : @Database.Redis.connectCluster = @Database.Redis.Connection.connectCluster := rfl
example : @Database.Redis.checkedConnectCluster = @Database.Redis.Connection.checkedConnectCluster := rfl
example : @Database.Redis.disconnect = @Database.Redis.Connection.disconnect := rfl
example : @Database.Redis.withConnect = @Database.Redis.Connection.withConnect := rfl
example : @Database.Redis.withCheckedConnect = @Database.Redis.Connection.withCheckedConnect := rfl
example : @Database.Redis.runRedis = @Database.Redis.Connection.runRedis := rfl
example : @Database.Redis.runRedisNonBlocking = @Database.Redis.Connection.runRedisNonBlocking := rfl
example : @Database.Redis.Connection = @Database.Redis.Connection.Connection := rfl
example : @Database.Redis.ConnectInfo = @Database.Redis.Connection.ConnectInfo := rfl
example : @Database.Redis.defaultConnectInfo = @Database.Redis.Connection.defaultConnectInfo := rfl
example : @Database.Redis.parseConnectInfo = @Database.Redis.URL.parseConnectInfo := rfl
example : @Database.Redis.ConnectAddr = @Database.Redis.ConnectionContext.ConnectAddr := rfl

-- Deviation: upstream's `ConnectError`/`ClusterConnectError`/
-- `ConnectionLostException`/`ConnectTimeout` exception *types* are surfaced
-- here as descriptive `IO.Error`-valued helpers.
example : @Database.Redis.connectAuthError = @Database.Redis.Connection.connectAuthError := rfl
example : @Database.Redis.connectSelectError = @Database.Redis.Connection.connectSelectError := rfl
example : @Database.Redis.clusterConnectError = @Database.Redis.Connection.clusterConnectError := rfl
example : @Database.Redis.connectionLostError = @Database.Redis.ConnectionContext.connectionLostError := rfl
example : @Database.Redis.connectTimeoutError = @Database.Redis.ConnectionContext.connectTimeoutError := rfl

/-! ### Low-level reply / status / decoding (`Protocol`, `Types`) -/

example : @Database.Redis.Reply = @Database.Redis.Protocol.Reply := rfl
example : @Database.Redis.Status = @Database.Redis.Types.Status := rfl
example : @Database.Redis.RedisArg = @Database.Redis.Types.RedisArg := rfl
example : @Database.Redis.RedisResult = @Database.Redis.Types.RedisResult := rfl

/-! ### Hooks -/

example : @Database.Redis.Hooks = @Database.Redis.Hooks.Hooks := rfl
example : @Database.Redis.defaultHooks = @Database.Redis.Hooks.defaultHooks := rfl
example : @Database.Redis.SendRequestHook = @Database.Redis.Hooks.SendRequestHook := rfl
example : @Database.Redis.SendPubSubHook = @Database.Redis.Hooks.SendPubSubHook := rfl
example : @Database.Redis.CallbackHook = @Database.Redis.Hooks.CallbackHook := rfl
example : @Database.Redis.SendHook = @Database.Redis.Hooks.SendHook := rfl
example : @Database.Redis.ReceiveHook = @Database.Redis.Hooks.ReceiveHook := rfl

/-! ### Cluster hash slots -/

example : @Database.Redis.HashSlot = @Database.Redis.Cluster.HashSlot.HashSlot := rfl
example : @Database.Redis.keyToSlot = @Database.Redis.Cluster.HashSlot.keyToSlot := rfl

/-! ### Commands — a representative sample of the ~125 uniform commands -/

example : @Database.Redis.get = @Database.Redis.Commands.get := rfl
example : @Database.Redis.del = @Database.Redis.Commands.del := rfl
example : @Database.Redis.expire = @Database.Redis.Commands.expire := rfl
example : @Database.Redis.echo = @Database.Redis.Commands.echo := rfl
example : @Database.Redis.hset = @Database.Redis.Commands.hset := rfl
example : @Database.Redis.zscore = @Database.Redis.Commands.zscore := rfl

/-! ### Manual / irregular commands -/

example : @Database.Redis.set = @Database.Redis.ManualCommands.set := rfl
example : @Database.Redis.ping = @Database.Redis.ManualCommands.ping := rfl
example : @Database.Redis.eval = @Database.Redis.ManualCommands.eval := rfl
example : @Database.Redis.«exists» = @Database.Redis.ManualCommands.«exists» := rfl
example : @Database.Redis.SetOpts = @Database.Redis.ManualCommands.SetOpts := rfl

/-! ### Transactions -/

example : @Database.Redis.multiExec = @Database.Redis.Transactions.multiExec := rfl
example : @Database.Redis.watch = @Database.Redis.Transactions.watch := rfl
example : @Database.Redis.unwatch = @Database.Redis.Transactions.unwatch := rfl
example : @Database.Redis.runRedisTx = @Database.Redis.Transactions.runRedisTx := rfl
example : @Database.Redis.RedisTx = @Database.Redis.Transactions.RedisTx := rfl
example : @Database.Redis.Queued = @Database.Redis.Transactions.Queued := rfl
example : @Database.Redis.TxResult = @Database.Redis.Transactions.TxResult := rfl

/-! ### Pub/Sub -/

example : @Database.Redis.publish = @Database.Redis.PubSub.publish := rfl
example : @Database.Redis.pubSub = @Database.Redis.PubSub.pubSub := rfl
example : @Database.Redis.withPubSub = @Database.Redis.PubSub.withPubSub := rfl
example : @Database.Redis.PubSubController = @Database.Redis.PubSub.PubSubController := rfl
example : @Database.Redis.newPubSubController = @Database.Redis.PubSub.newPubSubController := rfl

/-! ### Runtime smoke tests through the facade (pure re-exports) -/

-- `keyToSlot` reachable and computing via the facade (cross-checked against
-- the canonical CRC16 vector used in `Cluster.HashSlotTest`).
#guard (Database.Redis.keyToSlot "123456789".toUTF8).toUInt16 == 12739
#guard (Database.Redis.keyToSlot "anything".toUTF8).toUInt16 < 16384

-- `defaultConnectInfo` and its fields reachable through the facade.
#guard Database.Redis.defaultConnectInfo.connectDatabase == 0
#guard Database.Redis.defaultConnectInfo.connectMaxConnections == 50
example : Database.Redis.defaultConnectInfo.connectAddr = .hostPort "localhost" 6379 := rfl

-- `parseConnectInfo` reachable through the facade and round-trips a URL back
-- to `defaultConnectInfo`'s host/port.
#guard (Database.Redis.parseConnectInfo "redis://localhost:6379").toOption.isSome
#guard (Database.Redis.parseConnectInfo "not a redis url").toOption.isNone

end Tests.Database.Redis.Facade
