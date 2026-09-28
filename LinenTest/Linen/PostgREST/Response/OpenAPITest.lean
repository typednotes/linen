/-
  Tests for `Linen.PostgREST.Response.OpenAPI`.
-/
import Linen.PostgREST.Response.OpenAPI

open PostgREST.Response.OpenAPI
open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.Response.OpenAPI

/-! ### `pgTypeToOpenAPI` -/

#guard pgTypeToOpenAPI "int4" == ("integer", some "int32")
#guard pgTypeToOpenAPI "bigint" == ("integer", some "int64")
#guard pgTypeToOpenAPI "bool" == ("boolean", none)
#guard pgTypeToOpenAPI "uuid" == ("string", some "uuid")
#guard pgTypeToOpenAPI "timestamptz" == ("string", some "date-time")
#guard pgTypeToOpenAPI "jsonb" == ("object", none)
#guard pgTypeToOpenAPI "some_enum" == ("string", none)

/-! ### `columnSchema` -/

def idCol : Column :=
  { colTable := { qiSchema := "public", qiName := "users" }
    colName := "id", colNullable := false, colType := "int4" }

#guard columnSchema idCol == "{\"type\":\"integer\",\"format\":\"int32\"}"

def describedCol : Column :=
  { colTable := { qiSchema := "public", qiName := "users" }
    colName := "name", colNullable := true, colType := "text"
    colDescription := some "Full name", colDefault := some "''" }

#guard columnSchema describedCol ==
  "{\"type\":\"string\",\"description\":\"Full name\",\"default\":\"''\"}"

/-! ### `generateOpenAPISpec` -/

def usersQi : QualifiedIdentifier := { qiSchema := "public", qiName := "users" }

def usersTable : Table :=
  { tableSchema := "public", tableName := "users", tableColumns := #[idCol] }

def sc : SchemaCache :=
  { SchemaCache.empty with dbTables := [(usersQi, usersTable)] }

def spec := generateOpenAPISpec sc

#guard spec.startsWith "{\"openapi\":\"3.0.0\","
#guard (spec.splitOn "\"title\":\"PostgREST API\"").length == 2
#guard (spec.splitOn s!"\"version\":\"{PostgREST.Version.version}\"").length == 2
#guard (spec.splitOn "\"/users\"").length == 2
#guard (spec.splitOn "\"id\":{\"type\":\"integer\",\"format\":\"int32\"}").length == 2

def specWithServer := generateOpenAPISpec sc "My API" (some "https://api.example.com")

#guard (specWithServer.splitOn "\"title\":\"My API\"").length == 2
#guard (specWithServer.splitOn "\"servers\":[{\"url\":\"https://api.example.com\"}]").length == 2

def emptySpec := generateOpenAPISpec SchemaCache.empty

#guard emptySpec == s!"\{\"openapi\":\"3.0.0\",\"info\":\{\"title\":\"PostgREST API\",\"version\":\"{PostgREST.Version.version}\"},\"paths\":\{}}"

end Tests.PostgREST.Response.OpenAPI
