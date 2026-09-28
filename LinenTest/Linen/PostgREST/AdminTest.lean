/-
  Tests for `Linen.PostgREST.Admin`.

  `handleAdminRequest` is IO-effectful (it reads from the `AppState`'s
  `IO.Ref`s), so it is exercised with `#eval show IO Unit from do ...` —
  a thrown error fails the build.
-/
import Linen.PostgREST.Admin

open PostgREST.Admin
open PostgREST.AppState
open PostgREST.SchemaCache

namespace Tests.PostgREST.Admin

#eval show IO Unit from do
  let st ← AppState.create (fun _ => pure ())

  let (liveStatus, liveType, liveBody) ← handleAdminRequest st "/live"
  unless liveStatus == 200 && liveType == "text/plain" && liveBody == "OK" do
    throw (IO.userError s!"expected /live to report 200 OK, got ({liveStatus}, {liveType}, {liveBody})")

  let (readyStatus1, _, readyBody1) ← handleAdminRequest st "/ready"
  unless readyStatus1 == 503 do
    throw (IO.userError s!"expected /ready with an empty schema cache to report 503, got ({readyStatus1}, {readyBody1})")

  st.putSchemaCache { SchemaCache.empty with dbTables := [({ qiSchema := "public", qiName := "users" }, { tableSchema := "public", tableName := "users" })] }
  let (readyStatus2, _, readyBody2) ← handleAdminRequest st "/ready"
  unless readyStatus2 == 200 && readyBody2 == "OK" do
    throw (IO.userError s!"expected /ready with a loaded schema cache to report 200 OK, got ({readyStatus2}, {readyBody2})")

  st.incRequestCount
  let (metricsStatus, metricsType, metricsBody) ← handleAdminRequest st "/metrics"
  unless metricsStatus == 200 do
    throw (IO.userError s!"expected /metrics to report 200, got {metricsStatus}")
  unless metricsType == "text/plain; version=0.0.4; charset=utf-8" do
    throw (IO.userError s!"unexpected /metrics content type: {metricsType}")
  unless (metricsBody.splitOn "\n").contains "postgrest_requests_total 1" do
    throw (IO.userError s!"expected /metrics body to report 1 request, got:\n{metricsBody}")

  let (notFoundStatus, _, notFoundBody) ← handleAdminRequest st "/bogus"
  unless notFoundStatus == 404 && notFoundBody == "Not Found" do
    throw (IO.userError s!"expected an unknown path to report 404, got ({notFoundStatus}, {notFoundBody})")

end Tests.PostgREST.Admin
