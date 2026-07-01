/-
  PostgREST.Admin — Admin server

  Optional admin HTTP server for health checks and metrics.

  ## Haskell source
  - `PostgREST.Admin` (postgrest package)
-/

import Linen.PostgREST.AppState
import Linen.PostgREST.Metrics

namespace PostgREST.Admin

open PostgREST.AppState

/-- Handle an admin server request. -/
def handleAdminRequest (appState : AppState) (path : String)
    : IO (Nat × String × String) := do  -- (status, contentType, body)
  match path with
  | "/live" =>
    return (200, "text/plain", "OK")
  | "/ready" =>
    let sc ← appState.getSchemaCache
    if !sc.dbTables.isEmpty then
      return (200, "text/plain", "OK")
    else
      return (503, "text/plain", "Schema cache not loaded")
  | "/metrics" =>
    let metrics ← appState.stateMetrics.get
    return (200, "text/plain; version=0.0.4; charset=utf-8",
      Metrics.renderMetrics metrics)
  | _ =>
    return (404, "text/plain", "Not Found")

end PostgREST.Admin
