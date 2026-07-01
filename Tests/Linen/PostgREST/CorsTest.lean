/-
  Tests for `Linen.PostgREST.Cors`.
-/
import Linen.PostgREST.Cors

open PostgREST.Cors

namespace Tests.PostgREST.Cors

/-! ### Defaults -/

#guard defaultExposedHeaders.elem "Content-Type"
#guard defaultExposedHeaders.elem "Range-Unit"
#guard defaultAllowedHeaders.elem "Authorization"
#guard defaultAllowedHeaders.elem "Prefer"

/-! ### `corsHeaders` -/

#guard corsHeaders none none == []
#guard corsHeaders (some "https://example.com") none ==
  [ ("Access-Control-Allow-Origin", "https://example.com")
  , ("Access-Control-Allow-Credentials", "true")
  , ("Access-Control-Expose-Headers", ", ".intercalate defaultExposedHeaders)
  , ("Vary", "Origin") ]
#guard corsHeaders (some "https://example.com") (some ["https://example.com"]) ==
  [ ("Access-Control-Allow-Origin", "https://example.com")
  , ("Access-Control-Allow-Credentials", "true")
  , ("Access-Control-Expose-Headers", ", ".intercalate defaultExposedHeaders)
  , ("Vary", "Origin") ]
#guard corsHeaders (some "https://example.com") (some ["*"]) ==
  [ ("Access-Control-Allow-Origin", "https://example.com")
  , ("Access-Control-Allow-Credentials", "true")
  , ("Access-Control-Expose-Headers", ", ".intercalate defaultExposedHeaders)
  , ("Vary", "Origin") ]
#guard corsHeaders (some "https://evil.com") (some ["https://example.com"]) == []

/-! ### `preflightHeaders` -/

#guard preflightHeaders "https://evil.com" none none (some ["https://example.com"]) == []
#guard preflightHeaders "https://example.com" none none none ==
  [ ("Access-Control-Allow-Origin", "https://example.com")
  , ("Access-Control-Allow-Credentials", "true")
  , ("Access-Control-Expose-Headers", ", ".intercalate defaultExposedHeaders)
  , ("Vary", "Origin")
  , ("Access-Control-Allow-Methods", "GET, POST, PATCH, PUT, DELETE, OPTIONS, HEAD")
  , ("Access-Control-Allow-Headers", ", ".intercalate defaultAllowedHeaders)
  , ("Access-Control-Max-Age", "86400") ]
#guard preflightHeaders "https://example.com" (some "POST") (some "Content-Type") none ==
  [ ("Access-Control-Allow-Origin", "https://example.com")
  , ("Access-Control-Allow-Credentials", "true")
  , ("Access-Control-Expose-Headers", ", ".intercalate defaultExposedHeaders)
  , ("Vary", "Origin")
  , ("Access-Control-Allow-Methods", "POST")
  , ("Access-Control-Allow-Headers", "Content-Type")
  , ("Access-Control-Max-Age", "86400") ]

end Tests.PostgREST.Cors
