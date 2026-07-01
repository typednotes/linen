/-
  PostgREST.AppState — Application shared state

  Mutable state shared across all request handlers: configuration,
  schema cache, connection pool, JWT cache, logger, and metrics.

  ## Haskell source
  - `PostgREST.AppState` (postgrest package)
-/

import Linen.PostgREST.SchemaCache

namespace PostgREST.AppState

open PostgREST.SchemaCache

-- ────────────────────────────────────────────────────────────────────
-- Observation events (for logging/metrics)
-- ────────────────────────────────────────────────────────────────────

/-- Events observed by the application for logging and metrics. -/
inductive Observation where
  | schemaCacheLoaded (tableCount : Nat) (relationCount : Nat)
  | schemaCacheLoadFailed (msg : String)
  | connectionPoolExhausted
  | jwtValidationFailed (msg : String)
  | requestCompleted (method : String) (path : String) (status : Nat) (durationMs : Nat)
  | serverStarted (host : String) (port : Nat)
  | configReloaded
  | listenerNotification (channel : String) (payload : String)
  deriving Repr

-- ────────────────────────────────────────────────────────────────────
-- Metrics
-- ────────────────────────────────────────────────────────────────────

/-- Simple metrics counters. -/
structure Metrics where
  requestCount : Nat := 0
  errorCount : Nat := 0
  schemaCacheReloads : Nat := 0
  poolConnectionsCreated : Nat := 0
  deriving Repr, Inhabited

-- ────────────────────────────────────────────────────────────────────
-- Application state
-- ────────────────────────────────────────────────────────────────────

/-- The shared mutable state of a running PostgREST instance.
    $$\text{AppState} = \{ \text{config}, \text{schemaCache}, \text{pool},
      \text{observer}, \text{metrics} \}$$

    All mutable fields are behind `IO.Ref` for thread-safe access. -/
structure AppState where
  /-- The current configuration (may be reloaded). -/
  stateSchemaCache : IO.Ref SchemaCache
  /-- Observation callback (logging + metrics). -/
  stateObserver : Observation → IO Unit
  /-- Metrics counters. -/
  stateMetrics : IO.Ref Metrics

namespace AppState

/-- Create a new application state with an empty schema cache. -/
def create (observer : Observation → IO Unit) : IO AppState := do
  let scRef ← IO.mkRef SchemaCache.empty
  let metricsRef ← IO.mkRef {}
  return {
    stateSchemaCache := scRef
    stateObserver := observer
    stateMetrics := metricsRef
  }

/-- Get the current schema cache. -/
def getSchemaCache (st : AppState) : IO SchemaCache :=
  st.stateSchemaCache.get

/-- Replace the schema cache with a new one. -/
def putSchemaCache (st : AppState) (sc : SchemaCache) : IO Unit :=
  st.stateSchemaCache.set sc

/-- Record an observation. -/
def observe (st : AppState) (obs : Observation) : IO Unit :=
  st.stateObserver obs

/-- Increment request count. -/
def incRequestCount (st : AppState) : IO Unit :=
  st.stateMetrics.modify fun m => { m with requestCount := m.requestCount + 1 }

/-- Increment error count. -/
def incErrorCount (st : AppState) : IO Unit :=
  st.stateMetrics.modify fun m => { m with errorCount := m.errorCount + 1 }

end AppState
end PostgREST.AppState
