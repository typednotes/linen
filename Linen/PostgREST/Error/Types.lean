/-
  PostgREST.Error.Types — PostgREST error hierarchy

  Defines all error types that PostgREST can produce, from HTTP parsing
  through JWT validation, SQL execution, and schema cache failures.
  Each error variant carries enough context to generate an appropriate
  HTTP status code and JSON error body.

  ## Haskell source
  - `PostgREST.Error` (postgrest package)

  ## Design
  - Errors form a layered hierarchy:
    $$\text{Error} = \text{ApiRequestError} + \text{JwtError} + \text{PgError}
      + \text{SchemaCacheError} + \ldots$$
  - `Error.toHttpStatus` maps every error to an HTTP status code
  - `BEq`, `Repr`, `ToString` instances on all types for diagnostics
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.Error

-- ────────────────────────────────────────────────────────────────────
-- Range errors
-- ────────────────────────────────────────────────────────────────────

/-- Errors arising from `Range` header parsing or validation.
    $$\text{RangeError} \in \{\text{outOfBounds}, \text{invalidLimit}, \text{invalidOffset}\}$$ -/
inductive RangeError where
  | outOfBounds (lowBound highBound totalLength : Int)
  | invalidLimit
  | invalidOffset
  deriving BEq, Repr

instance : ToString RangeError where
  toString
    | .outOfBounds lo hi total =>
      s!"Range out of bounds: {lo}-{hi} (total {total})"
    | .invalidLimit => "Invalid range limit"
    | .invalidOffset => "Invalid range offset"

-- ────────────────────────────────────────────────────────────────────
-- Query parameter errors
-- ────────────────────────────────────────────────────────────────────

/-- Errors from parsing query string parameters (`select`, `order`,
    `limit`, filter operators, etc.).
    $$\text{QPError} \in \{\text{badOperator}, \text{badLogicTree}, \ldots\}$$ -/
inductive QPError where
  | badOperator (op : String) (detail : String)
  | badLogicTree (msg : String)
  | invalidEmbedResource (msg : String)
  | invalidFilter (msg : String)
  | invalidOrderTerm (msg : String)
  | invalidSelectTerm (msg : String)
  | invalidRpcParam (msg : String)
  deriving BEq, Repr

instance : ToString QPError where
  toString
    | .badOperator op detail => s!"Bad operator '{op}': {detail}"
    | .badLogicTree msg => s!"Bad logic tree: {msg}"
    | .invalidEmbedResource msg => s!"Invalid embed resource: {msg}"
    | .invalidFilter msg => s!"Invalid filter: {msg}"
    | .invalidOrderTerm msg => s!"Invalid order term: {msg}"
    | .invalidSelectTerm msg => s!"Invalid select term: {msg}"
    | .invalidRpcParam msg => s!"Invalid RPC param: {msg}"

-- ────────────────────────────────────────────────────────────────────
-- API request errors
-- ────────────────────────────────────────────────────────────────────

/-- Errors from the HTTP request parsing phase, before any SQL is executed.
    $$\text{ApiRequestError} \in \{\text{actionMismatch}, \text{invalidBody}, \ldots\}$$ -/
inductive ApiRequestError where
  | actionMismatch (detail : String)
  | invalidBody (msg : String)
  | invalidFilters
  | invalidRange (err : RangeError)
  | invalidRpcMethod (method : String)
  | parseRequestError (msg : String)
  | queryParamError (err : QPError)
  | unsupportedMethod (method : String)
  | contentTypeError (accepted : List String) (got : String)
  | notFound (resource : String)
  | ambiguousRelationship (detail : String)
  | noRelationship (detail : String)
  deriving Repr

instance : ToString ApiRequestError where
  toString
    | .actionMismatch detail => s!"Action mismatch: {detail}"
    | .invalidBody msg => s!"Invalid request body: {msg}"
    | .invalidFilters => "Invalid filters"
    | .invalidRange err => s!"Invalid range: {err}"
    | .invalidRpcMethod method => s!"Invalid RPC method: {method}"
    | .parseRequestError msg => s!"Parse request error: {msg}"
    | .queryParamError err => s!"Query parameter error: {err}"
    | .unsupportedMethod method => s!"Unsupported method: {method}"
    | .contentTypeError accepted got =>
      let acceptedStr := ", ".intercalate accepted
      s!"Content type '{got}' not acceptable, expected one of: {acceptedStr}"
    | .notFound resource => s!"Resource not found: {resource}"
    | .ambiguousRelationship detail => s!"Ambiguous relationship: {detail}"
    | .noRelationship detail => s!"No relationship found: {detail}"

instance : BEq ApiRequestError where
  beq a b := toString a == toString b

-- ────────────────────────────────────────────────────────────────────
-- Schema cache errors
-- ────────────────────────────────────────────────────────────────────

/-- Errors from schema cache loading or refresh.
    $$\text{SchemaCacheError} \in \{\text{connectionLost}, \text{loadError},
      \text{pgVersionUnsupported}\}$$ -/
inductive SchemaCacheError where
  | connectionLost (msg : String)
  | loadError (msg : String)
  | pgVersionUnsupported (version : String)
  deriving BEq, Repr

instance : ToString SchemaCacheError where
  toString
    | .connectionLost msg => s!"Schema cache connection lost: {msg}"
    | .loadError msg => s!"Schema cache load error: {msg}"
    | .pgVersionUnsupported version =>
      s!"PostgreSQL version '{version}' is not supported"

-- ────────────────────────────────────────────────────────────────────
-- JWT errors
-- ────────────────────────────────────────────────────────────────────

/-- Errors from JWT validation.
    $$\text{JwtError} \in \{\text{tokenInvalid}, \text{tokenExpired},
      \text{tokenMissing}, \text{secretNotConfigured}\}$$ -/
inductive JwtError where
  | tokenInvalid (msg : String)
  | tokenExpired
  | tokenMissing
  | secretNotConfigured
  deriving BEq, Repr

instance : ToString JwtError where
  toString
    | .tokenInvalid msg => s!"JWT invalid: {msg}"
    | .tokenExpired => "JWT expired"
    | .tokenMissing => "JWT missing"
    | .secretNotConfigured => "JWT secret not configured"

-- ────────────────────────────────────────────────────────────────────
-- PostgreSQL execution errors
-- ────────────────────────────────────────────────────────────────────

/-- A PostgreSQL error returned from query execution.
    $$\text{PgError} = \langle \text{pgCode} : \{s : \text{String} \mid |s| = 5\},\;
      \text{pgMessage}, \text{pgDetail}?, \text{pgHint}? \rangle$$
    PostgreSQL SQLSTATE codes are always exactly 5 characters (e.g., `"42P01"`).
    The proof field ensures this invariant. -/
structure PgError where
  /-- 5-character SQLSTATE code (e.g., `"42501"`, `"23505"`). -/
  pgCode : String
  /-- Proof that the SQLSTATE code is exactly 5 characters. -/
  pgCode_len : pgCode.length = 5 := by decide
  pgMessage : String
  pgDetail : Option String := none
  pgHint : Option String := none
  deriving Repr

instance : BEq PgError where
  beq a b := a.pgCode == b.pgCode && a.pgMessage == b.pgMessage
    && a.pgDetail == b.pgDetail && a.pgHint == b.pgHint

instance : ToString PgError where
  toString e :=
    let base := s!"PG error {e.pgCode}: {e.pgMessage}"
    let withDetail := match e.pgDetail with
      | some d => s!"{base}\n  Detail: {d}"
      | none => base
    match e.pgHint with
    | some h => s!"{withDetail}\n  Hint: {h}"
    | none => withDetail

-- ────────────────────────────────────────────────────────────────────
-- Top-level error union
-- ────────────────────────────────────────────────────────────────────

/-- The top-level error union for all PostgREST errors.
    $$\text{Error} = \text{ApiRequestError} + \text{JwtError} + \text{PgError}
      + \text{SchemaCacheError} + \text{singularViolation} + \text{notFound}
      + \text{gucHeadersError} + \text{gucStatusError}
      + \text{offLimitsChangesError}$$ -/
inductive Error where
  | apiRequestError (e : ApiRequestError)
  | jwtError (e : JwtError)
  | pgError (e : PgError) (authenticated : Bool)
  | schemaCacheError (e : SchemaCacheError)
  | singularViolation (count : Nat)
  | notFound
  | gucHeadersError (msg : String)
  | gucStatusError (msg : String)
  | offLimitsChangesError (count : Nat) (maxSize : Nat)
  deriving Repr

instance : ToString Error where
  toString
    | .apiRequestError e => toString e
    | .jwtError e => toString e
    | .pgError e _ => toString e
    | .schemaCacheError e => toString e
    | .singularViolation count =>
      s!"Singular violation: query returned {count} rows instead of 1"
    | .notFound => "Not Found"
    | .gucHeadersError msg => s!"GUC headers error: {msg}"
    | .gucStatusError msg => s!"GUC status error: {msg}"
    | .offLimitsChangesError count maxSize =>
      s!"Off-limits changes: {count} rows affected, max allowed is {maxSize}"

instance : BEq Error where
  beq a b := toString a == toString b

-- ────────────────────────────────────────────────────────────────────
-- HTTP status mapping
-- ────────────────────────────────────────────────────────────────────

/-- Map an `ApiRequestError` to an HTTP status code.
    $$\text{apiRequestErrorStatus} : \text{ApiRequestError} \to \mathbb{N}$$ -/
def ApiRequestError.toHttpStatus : ApiRequestError → Nat
  | .actionMismatch _ => 405
  | .invalidBody _ => 400
  | .invalidFilters => 400
  | .invalidRange _ => 416
  | .invalidRpcMethod _ => 405
  | .parseRequestError _ => 400
  | .queryParamError _ => 400
  | .unsupportedMethod _ => 405
  | .contentTypeError _ _ => 415
  | .notFound _ => 404
  | .ambiguousRelationship _ => 300
  | .noRelationship _ => 400

/-- Map a `JwtError` to an HTTP status code.
    $$\text{jwtErrorStatus} : \text{JwtError} \to \mathbb{N}$$ -/
def JwtError.toHttpStatus : JwtError → Nat
  | .tokenInvalid _ => 401
  | .tokenExpired => 401
  | .tokenMissing => 401
  | .secretNotConfigured => 500

/-- Map a `SchemaCacheError` to an HTTP status code.
    $$\text{schemaCacheErrorStatus} : \text{SchemaCacheError} \to \mathbb{N}$$ -/
def SchemaCacheError.toHttpStatus : SchemaCacheError → Nat
  | .connectionLost _ => 503
  | .loadError _ => 503
  | .pgVersionUnsupported _ => 500

/-- Map a `PgError` to an HTTP status code based on the PostgreSQL error code.
    Uses the SQLSTATE code prefix to determine the HTTP status:
    - `42501` (insufficient_privilege) $\to$ 401 or 403 depending on auth
    - `42P01` (undefined_table) $\to$ 404
    - `23503` (foreign_key_violation) $\to$ 409
    - `23505` (unique_violation) $\to$ 409
    - Otherwise $\to$ 400 for class 42/class 23, 500 for the rest -/
def PgError.toHttpStatus (e : PgError) (authenticated : Bool) : Nat :=
  match e.pgCode with
  | "42501" => if authenticated then 403 else 401
  | "42P01" => 404
  | "42P17" => 404
  | "23503" => 409
  | "23505" => 409
  | "23514" => 400
  | "25006" => 405
  | "25001" => 405
  | "08P01" => 400
  | code =>
    if code.startsWith "08" then 503
    else if code.startsWith "09" then 500
    else if code.startsWith "0L" then 403
    else if code.startsWith "0P" then 403
    else if code.startsWith "28" then 403
    else if code.startsWith "2D" then 500
    else if code.startsWith "38" then 500
    else if code.startsWith "39" then 500
    else if code.startsWith "3B" then 500
    else if code.startsWith "40" then 500
    else if code.startsWith "53" then 503
    else if code.startsWith "54" then 500
    else if code.startsWith "55" then 500
    else if code.startsWith "57" then 500
    else if code.startsWith "58" then 500
    else if code.startsWith "F0" then 500
    else if code.startsWith "HV" then 500
    else if code.startsWith "P0" then 500
    else if code.startsWith "XX" then 500
    else 400

/-- Map any `Error` to an HTTP status code.
    $$\text{toHttpStatus} : \text{Error} \to \mathbb{N}$$ -/
def Error.toHttpStatus : Error → Nat
  | .apiRequestError e => e.toHttpStatus
  | .jwtError e => e.toHttpStatus
  | .pgError e authenticated => e.toHttpStatus authenticated
  | .schemaCacheError e => e.toHttpStatus
  | .singularViolation _ => 406
  | .notFound => 404
  | .gucHeadersError _ => 500
  | .gucStatusError _ => 500
  | .offLimitsChangesError _ _ => 400

-- ────────────────────────────────────────────────────────────────────
-- Status validity theorems
-- ────────────────────────────────────────────────────────────────────

/-- `ApiRequestError.toHttpStatus` always returns a valid HTTP status (100-599).
    $$\forall e,\; 100 \leq \text{toHttpStatus}(e) \leq 599$$ -/
theorem ApiRequestError.toHttpStatus_valid (e : ApiRequestError) :
    100 ≤ e.toHttpStatus ∧ e.toHttpStatus ≤ 599 := by
  cases e <;> simp [ApiRequestError.toHttpStatus] <;> omega

/-- `JwtError.toHttpStatus` always returns a valid HTTP status (100-599).
    $$\forall e,\; 100 \leq \text{toHttpStatus}(e) \leq 599$$ -/
theorem JwtError.toHttpStatus_valid (e : JwtError) :
    100 ≤ e.toHttpStatus ∧ e.toHttpStatus ≤ 599 := by
  cases e <;> simp [JwtError.toHttpStatus] <;> omega

/-- `SchemaCacheError.toHttpStatus` always returns a valid HTTP status (100-599).
    $$\forall e,\; 100 \leq \text{toHttpStatus}(e) \leq 599$$ -/
theorem SchemaCacheError.toHttpStatus_valid (e : SchemaCacheError) :
    100 ≤ e.toHttpStatus ∧ e.toHttpStatus ≤ 599 := by
  cases e <;> simp [SchemaCacheError.toHttpStatus] <;> omega

end PostgREST.Error
