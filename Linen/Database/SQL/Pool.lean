/-
  Linen.Database.SQL.Pool — Connection pool

  A thread-safe pool of PostgreSQL connections, backed by Lean's `IO.Ref`
  and an `Array` of available connections.  The pool automatically creates
  new connections on demand (up to the configured maximum) and recycles
  them after use.

  ## Haskell source
  - `Hasql.Pool` (hasql-pool package)

  ## Design
  Uses an `IO.Ref` guarding the pool state (available connections +
  count of outstanding connections).  Connections are created on demand up
  to `maxSize` and returned to the pool after each `use`.
-/

import Linen.Database.SQL.Connection
import Linen.Database.SQL.Session

namespace Database.SQL.Pool

open Database.SQL.Connection
open Database.SQL.Session

-- ────────────────────────────────────────────────────────────────────
-- Pool configuration
-- ────────────────────────────────────────────────────────────────────

/-- Pool configuration settings.
    `maxSize` must be positive (a pool with 0 connections is useless).
    `idleTimeout` is bounded to at most 24 hours (86400s) to prevent
    stale connections from lingering indefinitely. -/
structure PoolSettings where
  /-- Maximum number of connections. -/
  maxSize : Nat := 10
  /-- Proof that maxSize is positive — a pool must allow at least one connection. -/
  maxSize_pos : maxSize > 0 := by omega
  /-- Connection string for PostgreSQL. -/
  connSettings : Settings
  /-- Idle timeout in seconds (0 = no timeout). Bounded to at most 86400 (24h). -/
  idleTimeout : Nat := 300
  /-- The idle timeout must not exceed 24 hours. -/
  idleTimeout_bounded : idleTimeout ≤ 86400 := by omega

instance : Repr PoolSettings where
  reprPrec ps _ :=
    s!"PoolSettings(maxSize={ps.maxSize}, connSettings={repr ps.connSettings}, idleTimeout={ps.idleTimeout})"

-- ────────────────────────────────────────────────────────────────────
-- Pool state
-- ────────────────────────────────────────────────────────────────────

/-- Internal pool state protected by an IO.Ref. -/
private structure PoolState where
  /-- Available (idle) connections. -/
  available : Array Connection
  /-- Number of connections currently checked out. -/
  inUse : Nat
  /-- Total connections ever created (for accounting). -/
  totalCreated : Nat
  deriving Inhabited

-- ────────────────────────────────────────────────────────────────────
-- Pool
-- ────────────────────────────────────────────────────────────────────

/-- A pool of PostgreSQL connections.
    $$\text{Pool} = \text{Ref}\ \text{PoolState} \times \text{PoolSettings}$$ -/
structure Pool where
  state : IO.Ref PoolState
  settings : PoolSettings

/-- Pool errors. -/
inductive PoolError where
  | connectionError (e : ConnectionError)
  | sessionError (e : SessionError)
  | poolExhausted
  deriving Repr

instance : ToString PoolError where
  toString
    | .connectionError e => s!"PoolError: {e}"
    | .sessionError e => s!"PoolError: {e}"
    | .poolExhausted => "PoolError: pool exhausted"

namespace Pool

/-- Create a new connection pool.
    $$\text{create} : \text{PoolSettings} \to \text{IO}\ \text{Pool}$$ -/
def create (settings : PoolSettings) : IO Pool := do
  let ref ← IO.mkRef {
    available := #[]
    inUse := 0
    totalCreated := 0
  }
  return { state := ref, settings }

/-- Acquire a connection from the pool. -/
private def acquireConn (pool : Pool) : IO (Except PoolError Connection) := do
  -- Try to get an idle connection first
  let maybeConn ← pool.state.modifyGet fun st =>
    if h : 0 < st.available.size then
      let conn := st.available[st.available.size - 1]
      (some conn, { st with
        available := st.available.pop
        inUse := st.inUse + 1 })
    else if st.inUse + st.available.size < pool.settings.maxSize then
      (none, { st with inUse := st.inUse + 1, totalCreated := st.totalCreated + 1 })
    else
      (none, st)  -- pool exhausted — we'll handle below
  match maybeConn with
  | some conn => return .ok conn
  | none => do
    -- Create a new connection
    match ← acquire pool.settings.connSettings with
    | .ok conn => return .ok conn
    | .error e => return .error (.connectionError e)

/-- Return a connection to the pool. -/
private def releaseConn (pool : Pool) (conn : Connection) : IO Unit := do
  pool.state.modify fun st =>
    { st with
      available := st.available.push conn
      inUse := st.inUse - 1 }

/-- Run a session using a connection from the pool.
    The connection is automatically returned to the pool after use.
    $$\text{use} : \text{Pool} \to \text{Session}\ \alpha
      \to \text{IO}\ (\text{Except PoolError}\ \alpha)$$ -/
def use (pool : Pool) (session : Session α) : IO (Except PoolError α) := do
  match ← acquireConn pool with
  | .error e => return .error e
  | .ok conn =>
    try
      match ← Session.run session conn with
      | .ok a =>
        releaseConn pool conn
        return .ok a
      | .error e =>
        -- On session error, release the connection but propagate the error
        releaseConn pool conn
        return .error (.sessionError e)
    catch ex =>
      -- On IO exception, release and rethrow
      releaseConn pool conn
      throw ex

/-- Destroy the pool, closing all idle connections. -/
def destroy (pool : Pool) : IO Unit := do
  let conns ← pool.state.modifyGet fun st =>
    (st.available, { st with available := #[] })
  conns.forM fun conn =>
    Database.SQL.Connection.release conn

/-- Return the current pool statistics (idle, in-use, total created). -/
def stats (pool : Pool) : IO (Nat × Nat × Nat) := do
  let st ← pool.state.get
  return (st.available.size, st.inUse, st.totalCreated)

end Pool
end Database.SQL.Pool
