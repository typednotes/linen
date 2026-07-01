/-
  PostgREST.Cors — CORS middleware

  Cross-Origin Resource Sharing handling for PostgREST.

  ## Haskell source
  - `PostgREST.Cors` (postgrest package)
-/

namespace PostgREST.Cors

/-- Default CORS headers that PostgREST exposes. -/
def defaultExposedHeaders : List String :=
  [ "Content-Encoding"
  , "Content-Location"
  , "Content-Range"
  , "Content-Type"
  , "Date"
  , "Location"
  , "Server"
  , "Transfer-Encoding"
  , "Range-Unit" ]

/-- Default CORS allowed headers. -/
def defaultAllowedHeaders : List String :=
  [ "Authorization"
  , "Content-Type"
  , "Accept"
  , "Accept-Language"
  , "Content-Language"
  , "Range"
  , "Range-Unit"
  , "Prefer"
  , "Accept-Profile"
  , "Content-Profile" ]

/-- Generate CORS response headers for a given origin. -/
def corsHeaders (origin : Option String) (allowedOrigins : Option (List String))
    : List (String × String) :=
  match origin with
  | none => []
  | some orig =>
    let allowed := match allowedOrigins with
      | none => true  -- allow all origins if not configured
      | some origins => origins.elem orig || origins.elem "*"
    if allowed then
      [ ("Access-Control-Allow-Origin", orig)
      , ("Access-Control-Allow-Credentials", "true")
      , ("Access-Control-Expose-Headers", ", ".intercalate defaultExposedHeaders)
      , ("Vary", "Origin") ]
    else
      []

/-- Generate CORS preflight response headers. -/
def preflightHeaders (origin : String) (requestMethod : Option String)
    (requestHeaders : Option String) (allowedOrigins : Option (List String))
    : List (String × String) :=
  let base := corsHeaders (some origin) allowedOrigins
  if base.isEmpty then []
  else
    base ++
    [ ("Access-Control-Allow-Methods", match requestMethod with
        | some m => m
        | none => "GET, POST, PATCH, PUT, DELETE, OPTIONS, HEAD")
    , ("Access-Control-Allow-Headers", match requestHeaders with
        | some h => h
        | none => ", ".intercalate defaultAllowedHeaders)
    , ("Access-Control-Max-Age", "86400") ]

end PostgREST.Cors
