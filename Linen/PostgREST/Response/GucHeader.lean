/-
  PostgREST.Response.GucHeader — GUC variable to HTTP header mapping

  PostgREST allows PostgreSQL functions to set HTTP response headers and
  status codes via `SET LOCAL` on specially-named GUC variables.

  ## Haskell source
  - `PostgREST.Response.GucHeader` (postgrest package)
-/

namespace PostgREST.Response.GucHeader

/-- The GUC variable prefix for response headers. -/
def gucHeaderPrefix : String := "response.headers"

/-- The GUC variable for response status. -/
def gucStatusVar : String := "response.status"

/-- Parse a GUC header JSON array into HTTP headers.
    Format: `[{"header_name": "value"}, ...]` -/
def parseGucHeaders (json : String) : List (String × String) :=
  -- Simplified parser for `[{"key":"value"}, ...]`
  -- In full implementation, would use proper JSON parsing
  let trimmed := json.trimAscii.toString
  if trimmed.startsWith "[" && trimmed.endsWith "]" then
    -- Strip brackets, split by }, parse each pair
    -- For now, return empty (proper impl needs JSON parser)
    []
  else
    []

/-- Parse a GUC status value into an HTTP status code. -/
def parseGucStatus (value : String) : Option Nat :=
  value.trimAscii.toString.toNat?

end PostgREST.Response.GucHeader
