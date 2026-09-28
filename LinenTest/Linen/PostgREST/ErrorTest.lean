/-
  Tests for `Linen.PostgREST.Error`.
-/
import Linen.PostgREST.Error

open PostgREST.Error

namespace Tests.PostgREST.Error

/-! ### `errorPayload` -/

#guard errorPayload (.apiRequestError .invalidFilters) == "{\"message\":\"Invalid filters\",\"code\":\"PGRST100\"}"
#guard errorPayload (.apiRequestError (.notFound "users")) == "{\"message\":\"Resource not found: users\",\"code\":\"PGRST200\"}"
#guard errorPayload (.apiRequestError (.queryParamError (.invalidFilter "bad filter"))) == "{\"message\":\"Invalid filter: bad filter\",\"code\":\"PGRST100\"}"
#guard errorPayload (.apiRequestError (.contentTypeError ["application/json"] "text/xml")) == "{\"message\":\"Content type 'text/xml' not acceptable, expected one of: application/json\",\"code\":\"PGRST104\"}"

#guard errorPayload (.jwtError .tokenExpired) == "{\"message\":\"JWT expired\",\"code\":\"PGRST301\"}"
#guard errorPayload (.jwtError (.tokenInvalid "bad \"sig\"")) == "{\"message\":\"JWT invalid: bad \\\"sig\\\"\",\"code\":\"PGRST301\"}"

#guard errorPayload (.pgError { pgCode := "42501", pgMessage := "permission denied" } true) == "{\"message\":\"permission denied\",\"code\":\"42501\"}"
#guard errorPayload (.pgError { pgCode := "23505", pgMessage := "duplicate key", pgDetail := some "already exists", pgHint := some "use upsert" } false) == "{\"message\":\"duplicate key\",\"code\":\"23505\",\"details\":\"already exists\",\"hint\":\"use upsert\"}"

#guard errorPayload (.schemaCacheError (.connectionLost "timeout")) == "{\"message\":\"Connection to database lost: timeout\",\"code\":\"PGRST400\"}"

#guard errorPayload (.singularViolation 3) == "{\"message\":\"JSON object requested, 3 rows returned\",\"code\":\"PGRST505\"}"
#guard errorPayload .notFound == "{\"message\":\"Not Found\",\"code\":\"PGRST000\"}"
#guard errorPayload (.gucHeadersError "bad guc") == "{\"message\":\"bad guc\",\"code\":\"PGRST500\"}"
#guard errorPayload (.gucStatusError "bad status") == "{\"message\":\"bad status\",\"code\":\"PGRST500\"}"
#guard errorPayload (.offLimitsChangesError 200 100) == "{\"message\":\"Payload Too Large: 200 rows affected, max 100\",\"code\":\"PGRST504\"}"

/-! ### `errorHeaders` -/

#guard errorHeaders (.jwtError .tokenMissing) == [("Content-Type", "application/json; charset=utf-8"), ("WWW-Authenticate", "Bearer")]
#guard errorHeaders (.singularViolation 1) == [("Content-Type", "application/json; charset=utf-8")]
#guard errorHeaders (.pgError { pgCode := "42501", pgMessage := "m" } false) == [("Content-Type", "application/json; charset=utf-8"), ("WWW-Authenticate", "Bearer")]
#guard errorHeaders (.pgError { pgCode := "42501", pgMessage := "m" } true) == [("Content-Type", "application/json; charset=utf-8")]
#guard errorHeaders (.pgError { pgCode := "23505", pgMessage := "m" } false) == [("Content-Type", "application/json; charset=utf-8")]
#guard errorHeaders .notFound == [("Content-Type", "application/json; charset=utf-8")]

end Tests.PostgREST.Error
