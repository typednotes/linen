/-
  Tests for `Linen.Database.Redis.Core.Internal`.
-/
import Linen.Database.Redis.Core.Internal
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.Core.Internal
open Database.Redis.Protocol (Reply)

namespace Tests.Database.Redis.Core.Internal

/-! ### Compile-time: `Redis` is definitionally `ReaderT RedisEnv IO` -/

example : Redis α = ReaderT RedisEnv IO α := rfl
example (r : Redis α) : ReaderT RedisEnv IO α := unRedis r
example (r : ReaderT RedisEnv IO α) : Redis α := reRedis r

/-- `unRedis`/`reRedis` are the identity function (see the module
    doc-comment): round-tripping through either direction is a no-op. -/
example (r : Redis α) : reRedis (unRedis r) = r := rfl
example (r : ReaderT RedisEnv IO α) : unRedis (reRedis r) = r := rfl

/-! ### Runtime: `envLastReply` reads the right `IO.Ref` for each `RedisEnv`
    constructor -/

-- A `NonClustered` environment's `envLastReply` is the ref it was built
-- with. (A real loopback connection stands in for a `ProtocolPipelining.
-- Connection` value; nothing is ever sent or received on it, since only
-- the environment plumbing is under test here.)
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let conn ← Database.Redis.ProtocolPipelining.connect (.hostPort addr.host addr.port)
  let ref ← IO.mkRef (Reply.singleLine "a".toUTF8)
  let env := RedisEnv.nonClustered conn ref
  unless (← (envLastReply env).get) == Reply.singleLine "a".toUTF8 do
    throw (IO.userError "expected envLastReply to read back the nonClustered ref's initial value")
  (envLastReply env).set (Reply.integer 7)
  unless (← ref.get) == Reply.integer 7 do
    throw (IO.userError "expected envLastReply to alias the very same ref, not a copy")
  Database.Redis.ProtocolPipelining.disconnect conn
  let _ ← Network.Socket.close server
  pure ()

-- A `Clustered` environment's `envLastReply` is *its* ref, distinct from
-- `refreshAction`/`connection`.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let node : Database.Redis.Cluster.Node :=
    { id := "n".toUTF8, role := .master, host := addr.host, port := addr.port.toNat }
  let shardMap : Database.Redis.Cluster.ShardMap :=
    { slots := (∅ : Std.HashMap Nat Database.Redis.Cluster.Shard).insert 0
        { master := node, slaves := [] } }
  let clusterConn ← Database.Redis.Cluster.connectWith none none [] shardMap
    Database.Redis.Hooks.defaultHooks
  let ref ← IO.mkRef (Reply.singleLine "b".toUTF8)
  let env := RedisEnv.clustered (pure shardMap) clusterConn ref
  unless (← (envLastReply env).get) == Reply.singleLine "b".toUTF8 do
    throw (IO.userError "expected envLastReply to read back the clustered ref's initial value")
  (envLastReply env).set (Reply.error "x".toUTF8)
  unless (← ref.get) == Reply.error "x".toUTF8 do
    throw (IO.userError "expected envLastReply to alias the very same ref, not a copy")
  Database.Redis.Cluster.disconnect clusterConn
  let _ ← Network.Socket.close server
  pure ()

end Tests.Database.Redis.Core.Internal
