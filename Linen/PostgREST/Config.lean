/-
  PostgREST.Config -- Application configuration

  The `AppConfig` structure holds all PostgREST configuration options,
  corresponding to the Haskell `PostgREST.Config.AppConfig` type.
  Configuration is loaded from environment variables, a config file,
  or the database (via `app.settings`).

  ## Haskell source
  - `PostgREST.Config` (postgrest package)

  ## Design
  - `AppConfig` is a flat record with all configuration fields, matching
    the Haskell record field-for-field
  - Default values match PostgREST's documented defaults
  - `LogLevel` and `OpenAPIMode` are separate inductive types to prevent
    invalid configuration at the type level
  - $$\text{AppConfig} = \{ \text{dbUri},\; \text{dbSchemas},\;
      \text{anonRole},\; \text{jwtSecret}?,\; \text{serverPort},\; \ldots \}$$
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.Config

open PostgREST.SchemaCache.Identifiers

-- ────────────────────────────────────────────────────────────────────
-- Log level
-- ────────────────────────────────────────────────────────────────────

/-- Logging verbosity level, ordered from most critical to most verbose.
    $$\text{LogLevel} \in \{ \text{crit}, \text{error}, \text{warn},
      \text{info}, \text{debug} \}$$ -/
inductive LogLevel where
  | crit
  | error
  | warn
  | info
  | debug
  deriving BEq, Repr, Inhabited, DecidableEq

instance : ToString LogLevel where
  toString
    | .crit  => "crit"
    | .error => "error"
    | .warn  => "warn"
    | .info  => "info"
    | .debug => "debug"

/-- Parse a log level from a string (case-insensitive). -/
def LogLevel.parse (s : String) : Option LogLevel :=
  match s.toLower with
  | "crit"  => some .crit
  | "error" => some .error
  | "warn"  => some .warn
  | "info"  => some .info
  | "debug" => some .debug
  | _       => none

-- ────────────────────────────────────────────────────────────────────
-- OpenAPI mode
-- ────────────────────────────────────────────────────────────────────

/-- OpenAPI specification generation mode.
    $$\text{OpenAPIMode} \in \{ \text{followPriv}, \text{ignorePriv},
      \text{disabled}, \text{securityNone} \}$$
    - `followPriv`: respect database privileges in the spec
    - `ignorePriv`: expose all endpoints regardless of privileges
    - `disabled`: do not serve the OpenAPI spec
    - `securityNone`: serve spec with no security definitions -/
inductive OpenAPIMode where
  | followPriv
  | ignorePriv
  | disabled
  | securityNone
  deriving BEq, Repr, DecidableEq

instance : ToString OpenAPIMode where
  toString
    | .followPriv   => "follow-privileges"
    | .ignorePriv   => "ignore-privileges"
    | .disabled     => "disabled"
    | .securityNone => "security-none"

/-- Parse an OpenAPI mode from a string. -/
def OpenAPIMode.parse (s : String) : Option OpenAPIMode :=
  match s.toLower with
  | "follow-privileges" => some .followPriv
  | "ignore-privileges" => some .ignorePriv
  | "disabled"          => some .disabled
  | "security-none"     => some .securityNone
  | _                   => none

-- ────────────────────────────────────────────────────────────────────
-- Refined numeric types
-- ────────────────────────────────────────────────────────────────────

/-- A valid TCP/UDP port number: 1 through 65535.
    $$\text{Port} = \{ n : \mathbb{N} \mid 0 < n \land n \leq 65535 \}$$
    The proof field is erased at runtime (zero cost). -/
structure Port where
  /-- The numeric port value. -/
  val : Nat
  /-- Proof that the port is in the valid range. -/
  port_valid : 0 < val ∧ val ≤ 65535 := by omega
deriving Repr

instance : BEq Port where
  beq a b := a.val == b.val

instance : ToString Port where
  toString p := toString p.val

/-- Create a port from a literal.
    $$\text{mkPort} : \{ n : \mathbb{N} \mid 0 < n \leq 65535 \} \to \text{Port}$$ -/
@[inline] def mkPort (n : Nat) (h : 0 < n ∧ n ≤ 65535 := by omega) : Port := ⟨n, h⟩

-- ────────────────────────────────────────────────────────────────────
-- Application configuration
-- ────────────────────────────────────────────────────────────────────

/-- The main PostgREST application configuration.
    $$\text{AppConfig} = \{ \text{dbUri} : \text{String},\;
      \text{dbSchemas} : [\text{Schema}],\; \text{anonRole} : \text{String},\;
      \ldots \}$$

    Each field corresponds to a PostgREST configuration option
    (e.g., `PGRST_DB_URI`, `PGRST_DB_SCHEMAS`).

    ## Typing guarantees
    - `configServerPort` is a valid TCP port (1-65535) by construction
    - `configDbPoolSize` is positive by proof field
    - `configDbSchemas` is non-empty by proof field -/
structure AppConfig where
  /-- PostgreSQL connection URI (e.g., `postgresql://user:pass@host/db`). -/
  configDbUri : String
  /-- Schemas to expose via the API (e.g., `["public"]`). -/
  configDbSchemas : List Schema
  /-- Proof that at least one schema is configured. A PostgREST instance
      with no schemas cannot serve any requests. -/
  configDbSchemas_nonempty : configDbSchemas.length > 0 := by omega
  /-- Additional schemas to add to the search path. -/
  configDbExtraSearchPath : List Schema := []
  /-- The PostgreSQL role for unauthenticated requests. -/
  configDbAnonRole : String := "anon"
  /-- Whether to use prepared statements for queries. -/
  configDbPreparedStatements : Bool := true
  /-- Maximum number of rows returned by any request (none = unlimited). -/
  configDbMaxRows : Option Nat := none
  /-- A function to call before every request (for row-level security setup). -/
  configDbPreRequest : Option QualifiedIdentifier := none
  /-- Root spec: a function that serves as the API root (`/`). -/
  configDbRootSpec : Option QualifiedIdentifier := none
  /-- Whether clients can override the transaction mode via headers. -/
  configDbTxAllowOverride : Bool := true
  /-- Whether to rollback all transactions (useful for testing). -/
  configDbTxRollbackAll : Bool := false
  /-- Connection pool size. -/
  configDbPoolSize : Nat := 10
  /-- Proof that the pool size is positive (a zero-size pool is useless). -/
  configDbPoolSize_pos : configDbPoolSize > 0 := by omega
  /-- Maximum idle time (seconds) before a pooled connection is closed. -/
  configDbPoolMaxIdleTime : Nat := 30
  /-- Maximum lifetime (seconds) for a pooled connection. -/
  configDbPoolMaxLifetime : Nat := 1800
  /-- JWT secret for token verification. -/
  configJwtSecret : Option String := none
  /-- Whether the JWT secret is base64-encoded. -/
  configJwtSecretIsBase64 : Bool := false
  /-- Expected JWT audience claim. -/
  configJwtAudience : Option String := none
  /-- JSON path to the role claim in the JWT (e.g., `.role`). -/
  configJwtRoleClaimKey : String := ".role"
  /-- OpenAPI specification generation mode. -/
  configOpenApiMode : OpenAPIMode := .followPriv
  /-- Proxy URI for the OpenAPI server URL. -/
  configOpenApiServerProxyUri : Option String := none
  /-- Server bind address (`!4` = all IPv4, `!6` = all IPv6). -/
  configServerHost : String := "!4"
  /-- Server listen port (valid TCP port: 1-65535). -/
  configServerPort : Port := mkPort 3000
  /-- Unix domain socket path (alternative to TCP). -/
  configServerUnixSocket : Option String := none
  /-- File mode for the Unix domain socket. -/
  configServerUnixSocketMode : Nat := 0o660
  /-- Admin server port (for health checks and metrics). -/
  configAdminServerPort : Option Port := none
  /-- Logging verbosity level. -/
  configLogLevel : LogLevel := .error
  /-- Allowed CORS origins (none = allow all). -/
  configCorsAllowedOrigins : Option (List String) := none
  /-- Additional raw media types to accept. -/
  configRawMediaTypes : List String := []
  deriving Repr

-- ────────────────────────────────────────────────────────────────────
-- Default configuration
-- ────────────────────────────────────────────────────────────────────

/-- A default configuration suitable for local development and testing.
    Uses `postgresql://localhost/postgres` with the `public` schema. -/
def AppConfig.default : AppConfig where
  configDbUri := "postgresql://localhost/postgres"
  configDbSchemas := ["public"]
  configDbSchemas_nonempty := by decide

-- ────────────────────────────────────────────────────────────────────
-- Configuration queries
-- ────────────────────────────────────────────────────────────────────

/-- Whether JWT authentication is configured. -/
def AppConfig.hasJwtSecret (c : AppConfig) : Bool :=
  c.configJwtSecret.isSome

/-- Whether the admin server is enabled. -/
def AppConfig.hasAdminServer (c : AppConfig) : Bool :=
  c.configAdminServerPort.isSome

/-- Whether the API has a root spec function. -/
def AppConfig.hasRootSpec (c : AppConfig) : Bool :=
  c.configDbRootSpec.isSome

/-- Whether the API has a pre-request function. -/
def AppConfig.hasPreRequest (c : AppConfig) : Bool :=
  c.configDbPreRequest.isSome

/-- The first schema in the exposed schemas list (used as default).
    Guaranteed to succeed because `configDbSchemas` is proven non-empty. -/
def AppConfig.mainSchema (c : AppConfig) : Schema :=
  match h : c.configDbSchemas with
  | s :: _ => s
  | [] => absurd c.configDbSchemas_nonempty (by simp [h])

-- ────────────────────────────────────────────────────────────────────
-- Roundtrip theorems
-- ────────────────────────────────────────────────────────────────────

/-- `LogLevel.parse` roundtrips `toString` for every log level.
    $$\forall l,\; \text{LogLevel.parse}(\text{toString}(l)) = \text{some}\ l$$ -/
theorem LogLevel.parse_toString_roundtrip (l : LogLevel) :
    LogLevel.parse (toString l) = some l := by
  cases l <;> native_decide

/-- `OpenAPIMode.parse` roundtrips `toString` for every mode.
    $$\forall m,\; \text{OpenAPIMode.parse}(\text{toString}(m)) = \text{some}\ m$$ -/
theorem OpenAPIMode.parse_toString_roundtrip (m : OpenAPIMode) :
    OpenAPIMode.parse (toString m) = some m := by
  cases m <;> native_decide

end PostgREST.Config
