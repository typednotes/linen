/-
  Tests for `Linen.PostgREST.Error.Types`.
-/
import Linen.PostgREST.Error.Types

open PostgREST.Error

namespace Tests.PostgREST.Error.Types

/-! ### `RangeError` -/

#guard toString (RangeError.outOfBounds 0 10 5) == "Range out of bounds: 0-10 (total 5)"
#guard toString RangeError.invalidLimit == "Invalid range limit"
#guard toString RangeError.invalidOffset == "Invalid range offset"

/-! ### `QPError` -/

#guard toString (QPError.badOperator "eqq" "unknown operator") == "Bad operator 'eqq': unknown operator"
#guard toString (QPError.invalidFilter "bad filter") == "Invalid filter: bad filter"

/-! ### `ApiRequestError` -/

#guard toString (ApiRequestError.actionMismatch "GET vs POST") == "Action mismatch: GET vs POST"
#guard toString ApiRequestError.invalidFilters == "Invalid filters"
#guard toString (ApiRequestError.invalidRange .invalidLimit) == "Invalid range: Invalid range limit"
#guard toString (ApiRequestError.contentTypeError ["application/json", "text/csv"] "text/xml") ==
  "Content type 'text/xml' not acceptable, expected one of: application/json, text/csv"
#guard toString (ApiRequestError.notFound "users") == "Resource not found: users"
#guard ApiRequestError.invalidFilters == ApiRequestError.invalidFilters
#guard ApiRequestError.invalidFilters != ApiRequestError.notFound "users"

/-! ### `SchemaCacheError` -/

#guard toString (SchemaCacheError.connectionLost "timeout") == "Schema cache connection lost: timeout"
#guard toString (SchemaCacheError.pgVersionUnsupported "9.4") == "PostgreSQL version '9.4' is not supported"

/-! ### `JwtError` -/

#guard toString (JwtError.tokenInvalid "bad signature") == "JWT invalid: bad signature"
#guard toString JwtError.tokenExpired == "JWT expired"
#guard toString JwtError.tokenMissing == "JWT missing"
#guard toString JwtError.secretNotConfigured == "JWT secret not configured"

/-! ### `PgError` -/

#guard toString ({ pgCode := "42501", pgMessage := "permission denied" } : PgError) ==
  "PG error 42501: permission denied"
#guard toString ({ pgCode := "23505", pgMessage := "duplicate key", pgDetail := some "Key already exists.", pgHint := some "Use upsert instead." } : PgError) == "PG error 23505: duplicate key\n  Detail: Key already exists.\n  Hint: Use upsert instead."
#guard ({ pgCode := "42501", pgMessage := "m" } : PgError) == ({ pgCode := "42501", pgMessage := "m" } : PgError)
#guard ({ pgCode := "42501", pgMessage := "m" } : PgError) != ({ pgCode := "23505", pgMessage := "m" } : PgError)

/-! ### `Error` -/

#guard toString (Error.jwtError .tokenMissing) == "JWT missing"
#guard toString (Error.singularViolation 3) == "Singular violation: query returned 3 rows instead of 1"
#guard toString Error.notFound == "Not Found"
#guard toString (Error.offLimitsChangesError 200 100) ==
  "Off-limits changes: 200 rows affected, max allowed is 100"

/-! ### HTTP status mapping -/

#guard ApiRequestError.invalidFilters.toHttpStatus == 400
#guard (ApiRequestError.invalidRange .invalidLimit).toHttpStatus == 416
#guard JwtError.tokenExpired.toHttpStatus == 401
#guard JwtError.secretNotConfigured.toHttpStatus == 500
#guard (SchemaCacheError.connectionLost "x").toHttpStatus == 503
#guard PgError.toHttpStatus { pgCode := "42501", pgMessage := "m" } true == 403
#guard PgError.toHttpStatus { pgCode := "42501", pgMessage := "m" } false == 401
#guard PgError.toHttpStatus { pgCode := "23505", pgMessage := "m" } false == 409
#guard PgError.toHttpStatus { pgCode := "99999", pgMessage := "m" } false == 400

#guard Error.toHttpStatus (.apiRequestError .invalidFilters) == 400
#guard Error.toHttpStatus (.jwtError .tokenMissing) == 401
#guard Error.toHttpStatus (.pgError { pgCode := "23505", pgMessage := "m" } false) == 409
#guard Error.toHttpStatus (.schemaCacheError (.loadError "x")) == 503
#guard Error.toHttpStatus (.singularViolation 2) == 406
#guard Error.toHttpStatus .notFound == 404
#guard Error.toHttpStatus (.gucHeadersError "x") == 500
#guard Error.toHttpStatus (.gucStatusError "x") == 500
#guard Error.toHttpStatus (.offLimitsChangesError 5 1) == 400

/-! ### Status validity theorems -/

example : ∀ e : ApiRequestError, 100 ≤ e.toHttpStatus ∧ e.toHttpStatus ≤ 599 :=
  ApiRequestError.toHttpStatus_valid

example : ∀ e : JwtError, 100 ≤ e.toHttpStatus ∧ e.toHttpStatus ≤ 599 :=
  JwtError.toHttpStatus_valid

example : ∀ e : SchemaCacheError, 100 ≤ e.toHttpStatus ∧ e.toHttpStatus ≤ 599 :=
  SchemaCacheError.toHttpStatus_valid

end Tests.PostgREST.Error.Types
