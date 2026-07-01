/-
  PostgREST.App — Core application

  The main PostgREST application: a WAI `Application` that processes
  HTTP requests by parsing them into API requests, planning SQL queries,
  executing them against PostgreSQL, and formatting responses.

  ## Haskell source
  - `PostgREST.App` (postgrest package)

  ## Request processing pipeline
  1. HTTP request arrives (via Warp)
  2. CORS headers added
  3. Auth middleware extracts/validates JWT
  4. `ApiRequest` parsed from HTTP request
  5. Query plan computed from `ApiRequest` + `SchemaCache`
  6. SQL executed via `Hasql.Session`
  7. Response formatted and returned

  ## Design
  The application state is shared via `IO.Ref` values in `AppState`.
  Each request handler gets a snapshot of the schema cache and config.
-/

import Linen.PostgREST.AppState
import Linen.PostgREST.Auth
import Linen.PostgREST.Cors
import Linen.PostgREST.SchemaCache
import Linen.PostgREST.Logger
import Linen.PostgREST.MediaType
import Linen.PostgREST.Network
import Linen.PostgREST.Version
import Linen.PostgREST.TimeIt

namespace PostgREST.App

open PostgREST.AppState
open PostgREST.Auth
open PostgREST.Cors
open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers
open PostgREST.Logger
open PostgREST.MediaType
open PostgREST.TimeIt
open PostgREST.Version

-- ────────────────────────────────────────────────────────────────────
-- Request / Response types (simplified WAI-compatible)
-- ────────────────────────────────────────────────────────────────────

/-- A simplified HTTP request (mirrors WAI Request fields). -/
structure SimpleRequest where
  method : String
  path : String
  queryString : String := ""
  headers : List (String × String) := []
  body : Option String := none
  deriving Repr

/-- A simplified HTTP response. -/
structure SimpleResponse where
  status : Nat
  headers : List (String × String) := []
  body : String := ""
  deriving Repr

-- ────────────────────────────────────────────────────────────────────
-- Response builders
-- ────────────────────────────────────────────────────────────────────

private def jsonResponse (status : Nat) (body : String)
    (extraHeaders : List (String × String) := []) : SimpleResponse :=
  { status
    headers := [("Content-Type", "application/json; charset=utf-8")] ++ extraHeaders
    body }

private def errorResponse (status : Nat) (code : String) (message : String)
    (detail : String := "") (hint : String := "") : SimpleResponse :=
  let obj := String.intercalate ", " (
    [s!"\"code\":\"{code}\"", s!"\"message\":{escapeJsonString message}"] ++
    (if detail.isEmpty then [] else [s!"\"details\":{escapeJsonString detail}"]) ++
    (if hint.isEmpty then [] else [s!"\"hint\":{escapeJsonString hint}"])
  )
  jsonResponse status s!"\{{obj}}"
where
  escapeJsonString (s : String) : String :=
    "\"" ++ (s.replace "\\" "\\\\" |>.replace "\"" "\\\"" |>.replace "\n" "\\n") ++ "\""

-- ────────────────────────────────────────────────────────────────────
-- Core request handler
-- ────────────────────────────────────────────────────────────────────

/-- Handle a single HTTP request.
    This is the core of PostgREST: parse, plan, execute, respond. -/
def handleRequest (appState : AppState) (req : SimpleRequest)
    : IO SimpleResponse := do
  let (response, elapsed) ← timeIt do
    -- 1. Get current schema cache
    let sc ← appState.getSchemaCache

    -- 2. Handle special paths
    if req.path == "/" && req.method == "GET" then
      -- Root spec: return OpenAPI definition or table listing
      let tables := sc.tablesInSchemas (sc.dbTables.map (·.1.qiSchema) |>.eraseDups)
      let tableNames := tables.map (fun t => s!"\"{t.tableName}\"")
      return jsonResponse 200 ("[" ++ String.intercalate "," tableNames ++ "]")

    if req.path == "/" && req.method == "OPTIONS" then
      let origin := req.headers.find? (·.1.toLower == "origin") |>.map (·.2)
      let corsHdrs := match origin with
        | some orig => preflightHeaders orig
            (req.headers.find? (·.1.toLower == "access-control-request-method") |>.map (·.2))
            (req.headers.find? (·.1.toLower == "access-control-request-headers") |>.map (·.2))
            none
        | none => []
      return { status := 204, headers := corsHdrs, body := "" }

    -- 3. Parse path to determine target
    let pathSegments := (req.path.splitOn "/").filter (· != "")
    match pathSegments with
    | [] =>
      return errorResponse 404 "PGRST000" "No route found"
    | ["rpc", funcName] =>
      -- RPC call
      let _qi : QualifiedIdentifier := { qiSchema := "public", qiName := funcName }
      -- In full implementation: plan and execute the RPC call
      return errorResponse 501 "PGRST000" s!"RPC endpoint /rpc/{funcName} not yet implemented"
    | [tableName] =>
      -- Table/view access
      let qi : QualifiedIdentifier := { qiSchema := "public", qiName := tableName }
      match sc.findTable qi with
      | none =>
        return errorResponse 404 "PGRST200"
          s!"Could not find relation '{tableName}' in schema 'public'"
      | some _table =>
        -- In full implementation: parse query params, plan, execute SQL
        match req.method with
        | "GET" | "HEAD" =>
          -- Would execute: SELECT ... FROM "public"."tableName" WHERE ... ORDER BY ... LIMIT ...
          return jsonResponse 200 "[]"
            [("Content-Range", "*/0")]
        | "POST" =>
          return jsonResponse 201 ""
        | "PATCH" =>
          return jsonResponse 200 ""
        | "DELETE" =>
          return jsonResponse 200 ""
        | "OPTIONS" =>
          return { status := 200, headers := [], body := "" }
        | _ =>
          return errorResponse 405 "PGRST103"
            s!"Unsupported method {req.method} on /{tableName}"
    | _ =>
      return errorResponse 404 "PGRST000"
        s!"No route for path: {req.path}"

  -- Record metrics
  appState.incRequestCount
  appState.observe (.requestCompleted req.method req.path response.status elapsed)

  -- Add CORS headers
  let origin := req.headers.find? (·.1.toLower == "origin") |>.map (·.2)
  let corsHdrs := corsHeaders origin none
  return { response with headers := response.headers ++ corsHdrs }

-- ────────────────────────────────────────────────────────────────────
-- Startup
-- ────────────────────────────────────────────────────────────────────

/-- Print the startup banner. -/
def printBanner (host : String) (port : Nat) : IO Unit := do
  IO.eprintln s!"Starting {prettyVersion}"
  IO.eprintln s!"Listening on {Network.resolveHost host}:{port}"

end PostgREST.App
