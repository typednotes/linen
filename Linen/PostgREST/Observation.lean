/-
  PostgREST.Observation — Observability events

  ## Haskell source
  - `PostgREST.Observation` (postgrest package)
-/

import Linen.PostgREST.AppState

namespace PostgREST.Observation

open PostgREST.AppState

/-- Default observer that logs to stderr. -/
def defaultObserver : Observation → IO Unit
  | .schemaCacheLoaded tc rc =>
    IO.eprintln s!"Schema cache loaded: {tc} tables, {rc} relationships"
  | .schemaCacheLoadFailed msg =>
    IO.eprintln s!"Schema cache load failed: {msg}"
  | .connectionPoolExhausted =>
    IO.eprintln "Connection pool exhausted"
  | .jwtValidationFailed msg =>
    IO.eprintln s!"JWT validation failed: {msg}"
  | .requestCompleted method path status dur =>
    IO.eprintln s!"{method} {path} {status} ({dur}ms)"
  | .serverStarted host port =>
    IO.eprintln s!"PostgREST started on {host}:{port}"
  | .configReloaded =>
    IO.eprintln "Configuration reloaded"
  | .listenerNotification ch payload =>
    IO.eprintln s!"NOTIFY {ch}: {payload}"

end PostgREST.Observation
