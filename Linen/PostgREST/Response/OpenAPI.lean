/-
  PostgREST.Response.OpenAPI — OpenAPI 3.0 specification generation

  Generates an OpenAPI 3.0 JSON specification from the SchemaCache,
  describing all available tables, views, and RPC endpoints.

  ## Haskell source
  - `PostgREST.Response.OpenAPI` (postgrest package)
-/

import Linen.PostgREST.SchemaCache
import Linen.PostgREST.Version

namespace PostgREST.Response.OpenAPI

open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

/-- Map a PostgreSQL type to an OpenAPI type/format pair. -/
def pgTypeToOpenAPI (pgType : String) : String × Option String :=
  match pgType with
  | "integer" | "int4" => ("integer", some "int32")
  | "bigint" | "int8" => ("integer", some "int64")
  | "smallint" | "int2" => ("integer", some "int16")
  | "numeric" | "decimal" => ("number", some "double")
  | "real" | "float4" => ("number", some "float")
  | "double precision" | "float8" => ("number", some "double")
  | "boolean" | "bool" => ("boolean", none)
  | "uuid" => ("string", some "uuid")
  | "date" => ("string", some "date")
  | "timestamp without time zone" | "timestamp" => ("string", some "date-time")
  | "timestamp with time zone" | "timestamptz" => ("string", some "date-time")
  | "json" | "jsonb" => ("object", none)
  | "bytea" => ("string", some "byte")
  | _ => ("string", none)

/-- Generate a column schema as a JSON string. -/
def columnSchema (col : Column) : String :=
  let (typ, fmt) := pgTypeToOpenAPI col.colType
  let typePart := s!"\"type\":\"{typ}\""
  let fmtPart := match fmt with
    | some f => s!",\"format\":\"{f}\""
    | none => ""
  let descPart := match col.colDescription with
    | some d => s!",\"description\":\"{d}\""
    | none => ""
  let defaultPart := match col.colDefault with
    | some d => s!",\"default\":\"{d}\""
    | none => ""
  s!"\{{typePart}{fmtPart}{descPart}{defaultPart}}"

/-- Generate the OpenAPI JSON spec from a schema cache. -/
def generateOpenAPISpec (sc : SchemaCache) (title : String := "PostgREST API")
    (serverUrl : Option String := none) : String :=
  let info := s!"\"info\":\{\"title\":\"{title}\",\"version\":\"{Version.version}\"}"
  let server := match serverUrl with
    | some url => s!",\"servers\":[\{\"url\":\"{url}\"}]"
    | none => ""
  let paths := sc.dbTables.map fun (qi, table) =>
    let tablePath := s!"\"/{qi.qiName}\""
    let properties := table.tableColumns.toList.map fun col =>
      s!"\"{col.colName}\":{columnSchema col}"
    let schema := s!"\{\"type\":\"object\",\"properties\":\{{String.intercalate "," properties}}}"
    let getOp := s!"\"get\":\{\"summary\":\"Read {qi.qiName}\",\"responses\":\{\"200\":\{\"description\":\"OK\",\"content\":\{\"application/json\":\{\"schema\":\{\"type\":\"array\",\"items\":{schema}}}}}}}"
    s!"{tablePath}:\{{getOp}}"
  let pathsJson := s!"\"paths\":\{{String.intercalate "," paths}}"
  s!"\{\"openapi\":\"3.0.0\",{info}{server},{pathsJson}}"

end PostgREST.Response.OpenAPI
