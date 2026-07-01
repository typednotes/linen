/-
  Tests for `Linen.PostgREST.AppState`.

  `AppState` is built from `IO.Ref`s, so it is exercised with
  `#eval show IO Unit from do ...` — a thrown error fails the build.
-/
import Linen.PostgREST.AppState

open PostgREST.AppState
open PostgREST.SchemaCache

namespace Tests.PostgREST.AppState

#eval show IO Unit from do
  let events ← IO.mkRef (#[] : Array Observation)
  let st ← AppState.create (fun obs => events.modify (·.push obs))

  let sc0 ← st.getSchemaCache
  unless sc0.dbTables.isEmpty do throw (IO.userError "expected a fresh AppState to start with an empty schema cache")

  let sc' : SchemaCache := { SchemaCache.empty with dbTimezones := ["UTC"] }
  st.putSchemaCache sc'
  let sc1 ← st.getSchemaCache
  unless sc1.dbTimezones == ["UTC"] do
    throw (IO.userError s!"expected putSchemaCache to update the cache, got {repr sc1.dbTimezones}")

  st.observe .configReloaded
  let obs ← events.get
  unless obs.size == 1 do throw (IO.userError s!"expected one recorded observation, got {obs.size}")

  st.incRequestCount
  st.incRequestCount
  st.incErrorCount
  let m ← st.stateMetrics.get
  unless m.requestCount == 2 do throw (IO.userError s!"expected requestCount = 2, got {m.requestCount}")
  unless m.errorCount == 1 do throw (IO.userError s!"expected errorCount = 1, got {m.errorCount}")
  unless m.schemaCacheReloads == 0 do throw (IO.userError s!"expected schemaCacheReloads to stay at its default of 0, got {m.schemaCacheReloads}")

end Tests.PostgREST.AppState
