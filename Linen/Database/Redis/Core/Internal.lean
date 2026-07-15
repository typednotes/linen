/-
  Linen.Database.Redis.Core.Internal — the `Redis` monad's environment

  ## Haskell source
  `Database.Redis.Core.Internal` from https://hackage.haskell.org/package/hedis
  (module 9 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/Core/Internal.hs`. Exposes `Redis`, `RedisEnv`,
  `envLastReply`.

  ## `mtl`/`exceptions`/`unliftio` substitution
  Upstream's `Redis` is a `newtype` over `ReaderT RedisEnv IO`, with
  `GeneralizedNewtypeDeriving` deriving `Monad`/`MonadIO`/`Functor`/
  `Applicative`/`MonadUnliftIO`/`MonadThrow`/`MonadCatch`/`MonadMask`/
  `MonadFail` straight through from the underlying `ReaderT`/`IO` instances.
  Lean's `ReaderT` (used here directly, no wrapper `newtype`) already
  carries every one of those instances generically for any `[Monad m]`/
  `[MonadExceptOf ε m]`/... base monad, so `Redis` is defined as a plain
  `abbrev` for `ReaderT RedisEnv IO` rather than a fresh single-constructor
  structure: this is not a behavioural simplification (every derived
  instance upstream lists is still available, transparently, via the
  underlying `ReaderT`/`IO` instances Lean already provides — see
  `Linen.Control.Monad.Reader`'s doc-comment for the general "Lean core
  already generalizes `mtl`'s per-transformer instances" point), just
  dropping the `newtype`-plus-`deriving` boilerplate that only exists in
  Haskell to re-derive what Lean's `abbrev` gets for free by being
  definitionally the same type. `unRedis`/`reRedis` are kept (as the
  identity function) purely so a module built on top of this one (out of
  scope for this batch) that wants those exact names for `deriving via`-style
  instance definitions still has them.
-/
import Linen.Database.Redis.Cluster
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.ProtocolPipelining

namespace Database.Redis.Core.Internal

open Database.Redis.Protocol (Reply)

/-- The `Redis` monad's environment: either a single, non-clustered
    connection, or a clustered connection plus the action used to refresh
    its shard map on a `MOVED` redirect. Mirrors upstream's `RedisEnv`. -/
inductive RedisEnv where
  /-- A single-node connection. -/
  | nonClustered
      (envConn : Database.Redis.ProtocolPipelining.Connection)
      (nonClusteredLastReply : IO.Ref Reply)
  /-- A clustered connection. -/
  | clustered
      (refreshAction : IO Database.Redis.Cluster.ShardMap)
      (connection : Database.Redis.Cluster.Connection)
      (clusteredLastReply : IO.Ref Reply)

/-- Context for normal command execution, outside of transactions.
    `ReaderT RedisEnv IO` directly (see the module doc-comment for why no
    `newtype` wrapper is needed). -/
abbrev Redis (α : Type) : Type := ReaderT RedisEnv IO α

/-- The `IO.Ref` holding the most recently received `Reply`, regardless of
    whether the environment is clustered. Mirrors upstream's
    `envLastReply`. -/
def envLastReply : RedisEnv → IO.Ref Reply
  | .nonClustered _ r => r
  | .clustered _ _ r => r

/-- Deconstruct the `Redis` constructor. Identity, since `Redis` is
    definitionally `ReaderT RedisEnv IO` already (see the module
    doc-comment). Mirrors upstream's `unRedis`. -/
@[inline] def unRedis (r : Redis α) : ReaderT RedisEnv IO α := r

/-- Reconstruct the `Redis` constructor. Identity, for the same reason as
    `unRedis`. Mirrors upstream's `reRedis`. -/
@[inline] def reRedis (r : ReaderT RedisEnv IO α) : Redis α := r

end Database.Redis.Core.Internal
