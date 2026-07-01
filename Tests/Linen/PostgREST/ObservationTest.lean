/-
  Tests for `Linen.PostgREST.Observation`.

  `defaultObserver` is IO-effectful (it writes to stderr), so it is
  exercised with `#eval show IO Unit from do ...` — a thrown error fails
  the build. Since the output goes to stderr rather than a checkable
  value, these calls only confirm that every variant runs without error.
-/
import Linen.PostgREST.Observation

open PostgREST.Observation
open PostgREST.AppState

namespace Tests.PostgREST.Observation

#eval show IO Unit from do
  defaultObserver (.schemaCacheLoaded 10 5)
  defaultObserver (.schemaCacheLoadFailed "connection refused")
  defaultObserver .connectionPoolExhausted
  defaultObserver (.jwtValidationFailed "expired")
  defaultObserver (.requestCompleted "GET" "/users" 200 12)
  defaultObserver (.serverStarted "0.0.0.0" 3000)
  defaultObserver .configReloaded
  defaultObserver (.listenerNotification "pgrst" "reload schema")

end Tests.PostgREST.Observation
