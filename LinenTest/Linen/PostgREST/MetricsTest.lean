/-
  Tests for `Linen.PostgREST.Metrics`.
-/
import Linen.PostgREST.Metrics

open PostgREST.Metrics
open PostgREST.AppState

namespace Tests.PostgREST.Metrics

#guard renderMetrics {} ==
  "# HELP postgrest_requests_total Total number of HTTP requests.\n\
   # TYPE postgrest_requests_total counter\n\
   postgrest_requests_total 0\n\
   \n\
   # HELP postgrest_errors_total Total number of errors.\n\
   # TYPE postgrest_errors_total counter\n\
   postgrest_errors_total 0\n\
   \n\
   # HELP postgrest_schema_cache_reloads_total Total schema cache reloads.\n\
   # TYPE postgrest_schema_cache_reloads_total counter\n\
   postgrest_schema_cache_reloads_total 0\n\
   \n\
   # HELP postgrest_pool_connections_created_total Total pool connections created.\n\
   # TYPE postgrest_pool_connections_created_total counter\n\
   postgrest_pool_connections_created_total 0\n"

#guard (renderMetrics { requestCount := 42, errorCount := 3, schemaCacheReloads := 1, poolConnectionsCreated := 5 }).splitOn "\n" ==
  [ "# HELP postgrest_requests_total Total number of HTTP requests."
  , "# TYPE postgrest_requests_total counter"
  , "postgrest_requests_total 42"
  , ""
  , "# HELP postgrest_errors_total Total number of errors."
  , "# TYPE postgrest_errors_total counter"
  , "postgrest_errors_total 3"
  , ""
  , "# HELP postgrest_schema_cache_reloads_total Total schema cache reloads."
  , "# TYPE postgrest_schema_cache_reloads_total counter"
  , "postgrest_schema_cache_reloads_total 1"
  , ""
  , "# HELP postgrest_pool_connections_created_total Total pool connections created."
  , "# TYPE postgrest_pool_connections_created_total counter"
  , "postgrest_pool_connections_created_total 5"
  , "" ]

end Tests.PostgREST.Metrics
