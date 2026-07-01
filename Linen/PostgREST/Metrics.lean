/-
  PostgREST.Metrics — Prometheus-compatible metrics

  ## Haskell source
  - `PostgREST.Metrics` (postgrest package)
-/

import Linen.PostgREST.AppState

namespace PostgREST.Metrics

open PostgREST.AppState

/-- Render metrics in Prometheus text exposition format. -/
def renderMetrics (metrics : Metrics) : String :=
  let lines := [
    "# HELP postgrest_requests_total Total number of HTTP requests.",
    "# TYPE postgrest_requests_total counter",
    s!"postgrest_requests_total {metrics.requestCount}",
    "",
    "# HELP postgrest_errors_total Total number of errors.",
    "# TYPE postgrest_errors_total counter",
    s!"postgrest_errors_total {metrics.errorCount}",
    "",
    "# HELP postgrest_schema_cache_reloads_total Total schema cache reloads.",
    "# TYPE postgrest_schema_cache_reloads_total counter",
    s!"postgrest_schema_cache_reloads_total {metrics.schemaCacheReloads}",
    "",
    "# HELP postgrest_pool_connections_created_total Total pool connections created.",
    "# TYPE postgrest_pool_connections_created_total counter",
    s!"postgrest_pool_connections_created_total {metrics.poolConnectionsCreated}"
  ]
  "\n".intercalate lines ++ "\n"

end PostgREST.Metrics
