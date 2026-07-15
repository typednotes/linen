/-
  Tests for `Linen.Database.Redis.URL` — `parseConnectInfo` over example
  connection strings, mirroring the upstream `Database.Redis.URL` doctests plus
  a few extra edge cases.
-/
import Linen.Database.Redis.URL

namespace Database.Redis.URL.Test

open Database.Redis.URL
open Database.Redis.Connection (ConnectInfo)
open Database.Redis.ConnectionContext (ConnectAddr)

-- ── small projection helpers for asserting on the parse result ──

/-- The host/port of a `.hostPort` address, else `none`. -/
private def hostPortOf (ci : ConnectInfo) : Option (String × UInt16) :=
  match ci.connectAddr with
  | .hostPort h p => some (h, p)
  | .unixSocket _ => none

/-- The Unix-socket path of a `.unixSocket` address, else `none`. -/
private def socketOf (ci : ConnectInfo) : Option String :=
  match ci.connectAddr with
  | .unixSocket p => some p
  | .hostPort _ _ => none

/-- Decode an optional `ByteArray` credential back to a `String`. -/
private def credOf (b : Option ByteArray) : Option String :=
  b.bind (fun ba => String.fromUTF8? ba)

-- ── standalone `redis://` ──

-- Full standalone URL: user, password, host, port, and database.
#guard (match parseConnectInfo "redis://username:password@host:42/2" with
  | .ok ci => hostPortOf ci == some ("host", 42)
      && credOf ci.connectAuth == some "password"
      && credOf ci.connectUsername == some "username"
      && ci.connectDatabase == 2
  | .error _ => false)

-- A single credential (no `:`) is treated as the password, username stays unset.
#guard (match parseConnectInfo "redis://password@host:42/2" with
  | .ok ci => hostPortOf ci == some ("host", 42)
      && credOf ci.connectAuth == some "password"
      && ci.connectUsername == none
      && ci.connectDatabase == 2
  | .error _ => false)

-- Bare `redis://` falls back entirely to `defaultConnectInfo`.
#guard (match parseConnectInfo "redis://" with
  | .ok ci => hostPortOf ci == some ("localhost", 6379)
      && ci.connectAuth == none
      && ci.connectUsername == none
      && ci.connectDatabase == 0
  | .error _ => false)

-- ── TLS `rediss://` ──

-- `rediss://` parses (the TLS flag is not recorded — see the module doc-comment); it otherwise behaves like `redis://`.
#guard (match parseConnectInfo "rediss://" with
  | .ok ci => hostPortOf ci == some ("localhost", 6379)
      && ci.connectDatabase == 0
  | .error _ => false)

-- `rediss://` with full authority.
#guard (match parseConnectInfo "rediss://username:password@host:6380/3" with
  | .ok ci => hostPortOf ci == some ("host", 6380)
      && credOf ci.connectAuth == some "password"
      && credOf ci.connectUsername == some "username"
      && ci.connectDatabase == 3
  | .error _ => false)

-- ── Unix socket `redis-socket://` ──

-- Unix socket with a `password@`, a socket path, and a `?database=` query.
#guard (match parseConnectInfo "redis-socket://password@/tmp/redis.sock?database=2" with
  | .ok ci => socketOf ci == some "/tmp/redis.sock"
      && credOf ci.connectAuth == some "password"
      && ci.connectUsername == none
      && ci.connectDatabase == 2
  | .error _ => false)

-- Unix socket without a `database` query defaults the database to 0.
#guard (match parseConnectInfo "redis-socket:///var/run/redis.sock" with
  | .ok ci => socketOf ci == some "/var/run/redis.sock"
      && ci.connectDatabase == 0
  | .error _ => false)

-- ── error cases ──

-- A non-numeric database segment fails (with upstream's quirky wording).
#guard (match parseConnectInfo "redis://username:password@host:42/db" with
  | .error e => e == "Invalid port: db"
  | .ok _ => false)

-- An unrecognised scheme is rejected.
#guard (match parseConnectInfo "postgres://" with
  | .error e => e == "Wrong scheme postgres:"
  | .ok _ => false)

-- A string that is not a URI at all is rejected.
#guard (match parseConnectInfo "not a uri" with
  | .error e => e == "Invalid URI"
  | .ok _ => false)

-- A non-numeric `database` query value on a socket URL fails.
#guard (match parseConnectInfo "redis-socket:///tmp/r.sock?database=xyz" with
  | .error e => e == "Invalid database"
  | .ok _ => false)

-- ── authority-splitter unit checks ──

#guard (parseURIAuthority "user:pass@host:42"
  == some { user := some "user", password := some "pass", host := "host", port := some 42 })

#guard (parseURIAuthority "password@host"
  == some { user := some "password", password := none, host := "host", port := none })

#guard (parseURIAuthority ""
  == some { user := none, password := none, host := "", port := none })

end Database.Redis.URL.Test
