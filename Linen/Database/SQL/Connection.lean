/-
  Linen.Database.SQL.Connection — PostgreSQL connection management

  High-level connection acquisition and release, wrapping the low-level
  libpq bindings.  Connections are acquired via `acquire` and released
  via `release` (or via GC finalization).

  ## Haskell source
  - `Hasql.Connection` (hasql package)

  ## Design
  Connections wrap a `PgConn` with an `IO.Ref Bool` tracking whether
  the connection has been explicitly released.  This prevents double-close
  without requiring phantom state parameters (the connection may be shared
  across threads via a pool).
-/

import Linen.Database.PostgreSQL.LibPQ

namespace Database.SQL.Connection

open Database.PostgreSQL.LibPQ

-- ────────────────────────────────────────────────────────────────────
-- Connection settings
-- ────────────────────────────────────────────────────────────────────

/-- Connection settings for PostgreSQL.
    A libpq connection string or URI.
    The proof field ensures the connection string is non-empty,
    preventing silent failures when connecting with a blank string. -/
structure Settings where
  /-- The libpq connection string. -/
  connString : String
  /-- The connection string must be non-empty. -/
  nonEmpty : connString.length > 0

instance : Repr Settings where
  reprPrec s _ := repr s.connString

instance : BEq Settings where
  beq a b := a.connString == b.connString

instance : Inhabited Settings where
  default := { connString := "host=localhost", nonEmpty := by decide }

/-- Create settings from a connection URI.
    Example: `"postgresql://user:pass@localhost:5432/mydb"` -/
def Settings.uri (uri : String) (h : uri.length > 0 := by decide) : Settings :=
  { connString := uri, nonEmpty := h }

/-- Coerce Settings to String for backward compatibility with libpq. -/
instance : Coe Settings String where
  coe s := s.connString

/-- Create settings from individual components.
    Always produces a non-empty string because `port=N` is always present. -/
def Settings.components (host : String := "localhost") (port : Nat := 5432)
    (user : String := "") (password : String := "") (database : String := "")
    : Settings :=
  let parts := #[
    if host.isEmpty then "" else s!"host={host}",
    s!"port={port}",
    if user.isEmpty then "" else s!"user={user}",
    if password.isEmpty then "" else s!"password={password}",
    if database.isEmpty then "" else s!"dbname={database}"
  ]
  let s := " ".intercalate (parts.filter (· != "") |>.toList)
  -- The port part always produces a non-empty string (e.g. "port=5432"),
  -- so the result is guaranteed non-empty.
  if h : s.length > 0 then
    { connString := s, nonEmpty := h }
  else
    -- Fallback: impossible in practice since port=N is always non-empty,
    -- but we provide a safe default rather than using sorry.
    let fallback := s!"port={port}"
    if h2 : fallback.length > 0 then
      { connString := fallback, nonEmpty := h2 }
    else
      -- "host=localhost" is a static string with known length
      { connString := "host=localhost", nonEmpty := by decide }

-- ────────────────────────────────────────────────────────────────────
-- Connection type
-- ────────────────────────────────────────────────────────────────────

/-- A managed PostgreSQL connection.
    $$\text{Connection} = \text{PgConn} \times \text{IORef}\ \text{Bool}$$ -/
structure Connection where
  raw : PgConn
  released : IO.Ref Bool

-- ────────────────────────────────────────────────────────────────────
-- Connection errors
-- ────────────────────────────────────────────────────────────────────

/-- Error returned when acquiring a connection fails. -/
inductive ConnectionError where
  | cantConnect (message : String)
  deriving BEq, Repr

instance : ToString ConnectionError where
  toString
    | .cantConnect msg => s!"ConnectionError: {msg}"

-- ────────────────────────────────────────────────────────────────────
-- Acquire / release
-- ────────────────────────────────────────────────────────────────────

/-- Acquire a new PostgreSQL connection.
    $$\text{acquire} : \text{Settings} \to \text{IO}\ (\text{Except ConnectionError Connection})$$
    Returns `Except.error` if the connection cannot be established. -/
def acquire (settings : Settings) : IO (Except ConnectionError Connection) := do
  let conn ← connect settings.connString
  let st ← status conn
  match st with
  | .ok => do
    let ref ← IO.mkRef false
    return .ok { raw := conn, released := ref }
  | _ => do
    let msg ← errorMessage conn
    return .error (.cantConnect msg)

/-- Release a connection.  Idempotent: calling release twice is safe. -/
def release (conn : Connection) : IO Unit := do
  let alreadyReleased ← conn.released.get
  unless alreadyReleased do
    conn.released.set true
    close conn.raw

/-- Run an action with a connection, releasing it afterwards (even on exception).
    $$\text{withConnection} : \text{Settings} \to (\text{Connection} \to \text{IO}\ \alpha)
      \to \text{IO}\ (\text{Except ConnectionError}\ \alpha)$$ -/
def withConnection (settings : Settings) (action : Connection → IO α)
    : IO (Except ConnectionError α) := do
  let result ← acquire settings
  match result with
  | .error e => return .error e
  | .ok conn =>
    try
      let a ← action conn
      return .ok a
    finally
      release conn

end Database.SQL.Connection
