/-
  Linen.Database.SQL.Pool — Connection pool

  A thread-safe pool of PostgreSQL connections. It reserves capacity
  before opening a connection and recycles connections after use.

  ## Haskell source
  - `Hasql.Pool` (hasql-pool package)

  ## Design
  One mutex protects idle connections and outstanding reservations.
  Connection establishment and session execution run outside the mutex.
-/

import Linen.Database.SQL.Connection
import Linen.Database.SQL.Session
import Std.Sync.Mutex

namespace Database.SQL.Pool

open Database.SQL.Connection
open Database.SQL.Session

-- ────────────────────────────────────────────────────────────────────
-- Pool configuration
-- ────────────────────────────────────────────────────────────────────

/-- Pool configuration. The maximum connection count must be positive. -/
structure PoolSettings where
  /-- Maximum number of connections. -/
  maxSize : Nat := 10
  /-- Proof that maxSize is positive — a pool must allow at least one connection. -/
  maxSize_pos : maxSize > 0 := by omega
  /-- Connection string for PostgreSQL. -/
  connSettings : Settings
  /-- Reserved idle timeout in seconds. The pool currently retains idle
  connections until they are reused or drained with `Pool.destroy`. -/
  idleTimeout : Nat := 300
  /-- The idle timeout must not exceed 24 hours. -/
  idleTimeout_bounded : idleTimeout ≤ 86400 := by omega

instance : Repr PoolSettings where
  reprPrec ps _ :=
    s!"PoolSettings(maxSize={ps.maxSize}, connSettings={repr ps.connSettings}, idleTimeout={ps.idleTimeout})"

-- ────────────────────────────────────────────────────────────────────
-- Pool state
-- ────────────────────────────────────────────────────────────────────

/-- Internal pool state protected by the pool's mutex. -/
private structure PoolState where
  /-- Available (idle) connections. -/
  available : Array Connection
  /-- Number of connections checked out or being opened. -/
  inUse : Nat
  /-- Total connections ever created (for accounting). -/
  totalCreated : Nat

private inductive Checkout where
  | reuse (conn : Connection)
  | openNew
  | exhausted

-- ────────────────────────────────────────────────────────────────────
-- Pool
-- ────────────────────────────────────────────────────────────────────

/-- A pool of PostgreSQL connections. Create it through `Pool.create`. -/
structure Pool where
  private mk ::
  private state : Std.Mutex PoolState
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
  let state ← Std.Mutex.new {
    available := #[]
    inUse := 0
    totalCreated := 0
  }
  return { state, settings }

/-- Acquire a connection from the pool. -/
private def acquireConn (pool : Pool) : IO (Except PoolError Connection) := do
  let checkout ← pool.state.atomically (m := IO) do
    let st ← get
    if let some conn := st.available.back? then
      set { st with available := st.available.pop, inUse := st.inUse + 1 }
      return Checkout.reuse conn
    else if st.inUse < pool.settings.maxSize then
      set { st with inUse := st.inUse + 1 }
      return .openNew
    else
      return .exhausted
  match checkout with
  | .reuse conn => return .ok conn
  | .exhausted => return .error .poolExhausted
  | .openNew =>
    let result ← (acquire pool.settings.connSettings).toBaseIO
    pool.state.atomically (m := IO) do
      modify fun st =>
        match result with
        | .ok (.ok _) => { st with totalCreated := st.totalCreated + 1 }
        | _ => { st with inUse := st.inUse - 1 }
    return (← IO.ofExcept result).mapError PoolError.connectionError

/-- Return a connection to the pool. -/
private def releaseConn (pool : Pool) (conn : Connection) : IO Unit := do
  pool.state.atomically (m := IO) do
    modify fun st => { st with available := st.available.push conn, inUse := st.inUse - 1 }

/-- Run a session using a connection from the pool.
    The connection is returned after the session, including on exception.
    If every slot is occupied or reserved, return `.poolExhausted`.
    $$\text{use} : \text{Pool} \to \text{Session}\ \alpha
      \to \text{IO}\ (\text{Except PoolError}\ \alpha)$$ -/
def use (pool : Pool) (session : Session α) : IO (Except PoolError α) := do
  match ← acquireConn pool with
  | .error e => return .error e
  | .ok conn =>
    try
      return (← Session.run session conn).mapError PoolError.sessionError
    finally
      releaseConn pool conn

/-- Drain the idle connections present at the time of the call.
Active sessions can still return their connections and the pool can be reused. -/
def destroy (pool : Pool) : IO Unit := do
  pool.state.atomically (m := IO) do
    let st ← get
    set { st with available := #[] }
    for conn in st.available do
      Database.SQL.Connection.release conn

/-- Return idle connections, checked-out or opening connections, and the
total number of successful openings. -/
def stats (pool : Pool) : IO (Nat × Nat × Nat) := pool.state.atomically (m := IO) do
  let st ← get
  return (st.available.size, st.inUse, st.totalCreated)

end Pool
end Database.SQL.Pool
