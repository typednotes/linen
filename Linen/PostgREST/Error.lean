/-
  PostgREST.Error — PostgREST error formatting and response generation

  Re-exports `PostgREST.Error.Types` and provides functions to render errors
  as JSON payloads and HTTP response headers suitable for sending to clients.

  ## Haskell source
  - `PostgREST.Error` (postgrest package)

  ## Design
  - `errorPayload` produces a JSON error body with `message`, `details`,
    `hint`, and `code` fields
  - `errorHeaders` produces HTTP headers including `Content-Type` and,
    for auth errors, `WWW-Authenticate`
-/

import Linen.PostgREST.Error.Types

namespace PostgREST.Error

-- ────────────────────────────────────────────────────────────────────
-- JSON helpers (minimal, no dependency on a JSON library)
-- ────────────────────────────────────────────────────────────────────

/-- Escape a string for inclusion in a JSON string literal.
    Handles backslash, double-quote, and common control characters.
    $$\text{jsonEscape}(s) = s[\texttt{\\} \mapsto \texttt{\\\\},
      \texttt{"} \mapsto \texttt{\\"}, \ldots]$$ -/
private def jsonEscape (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
    | '\\' => "\\\\"
    | '"'  => "\\\""
    | '\n' => "\\n"
    | '\r' => "\\r"
    | '\t' => "\\t"
    | c    => c.toString
  ) ""

/-- Produce a JSON string value `"..."` with escaping. -/
private def jsonString (s : String) : String :=
  "\"" ++ jsonEscape s ++ "\""

/-- Produce a JSON field for an optional value, or omit it. -/
private def jsonOptionalField (key : String) (val : Option String) : String :=
  match val with
  | some v => s!",{jsonString key}:{jsonString v}"
  | none => ""

-- ────────────────────────────────────────────────────────────────────
-- Error payload
-- ────────────────────────────────────────────────────────────────────

/-- Produce a JSON error body from a `QPError`.
    $$\text{qpErrorPayload} : \text{QPError} \to \text{String}$$ -/
private def qpErrorPayload (e : QPError) : String :=
  let (code, msg) := match e with
    | .badOperator op detail => ("PGRST100", s!"Bad operator '{op}': {detail}")
    | .badLogicTree msg => ("PGRST100", s!"Bad logic tree: {msg}")
    | .invalidEmbedResource msg => ("PGRST106", s!"Invalid embed resource: {msg}")
    | .invalidFilter msg => ("PGRST100", s!"Invalid filter: {msg}")
    | .invalidOrderTerm msg => ("PGRST102", s!"Invalid order term: {msg}")
    | .invalidSelectTerm msg => ("PGRST101", s!"Invalid select term: {msg}")
    | .invalidRpcParam msg => ("PGRST103", s!"Invalid RPC param: {msg}")
  s!"\{\"message\":{jsonString msg},\"code\":{jsonString code}}"

/-- Produce a JSON error body from an `ApiRequestError`.
    $$\text{apiRequestErrorPayload} : \text{ApiRequestError} \to \text{String}$$ -/
private def apiRequestErrorPayload (e : ApiRequestError) : String :=
  match e with
  | .queryParamError qpe => qpErrorPayload qpe
  | .actionMismatch detail =>
    s!"\{\"message\":{jsonString detail},\"code\":\"PGRST105\"}"
  | .invalidBody msg =>
    s!"\{\"message\":{jsonString msg},\"code\":\"PGRST104\"}"
  | .invalidFilters =>
    s!"\{\"message\":\"Invalid filters\",\"code\":\"PGRST100\"}"
  | .invalidRange err =>
    s!"\{\"message\":{jsonString (toString err)},\"code\":\"PGRST103\"}"
  | .invalidRpcMethod method =>
    s!"\{\"message\":\"Invalid RPC method: {jsonEscape method}\",\"code\":\"PGRST105\"}"
  | .parseRequestError msg =>
    s!"\{\"message\":{jsonString msg},\"code\":\"PGRST104\"}"
  | .unsupportedMethod method =>
    s!"\{\"message\":\"Unsupported method: {jsonEscape method}\",\"code\":\"PGRST105\"}"
  | .contentTypeError accepted got =>
    let acceptedStr := ", ".intercalate accepted
    s!"\{\"message\":\"Content type '{jsonEscape got}' not acceptable, expected one of: {jsonEscape acceptedStr}\",\"code\":\"PGRST104\"}"
  | .notFound resource =>
    s!"\{\"message\":\"Resource not found: {jsonEscape resource}\",\"code\":\"PGRST200\"}"
  | .ambiguousRelationship detail =>
    s!"\{\"message\":{jsonString detail},\"code\":\"PGRST201\"}"
  | .noRelationship detail =>
    s!"\{\"message\":{jsonString detail},\"code\":\"PGRST200\"}"

/-- Produce a JSON error body from a `PgError`.
    Includes `message`, `details`, `hint`, and `code` fields.
    $$\text{pgErrorPayload} : \text{PgError} \to \text{String}$$ -/
private def pgErrorPayload (e : PgError) : String :=
  let base := s!"\"message\":{jsonString e.pgMessage},\"code\":{jsonString e.pgCode}"
  let withDetail := base ++ jsonOptionalField "details" e.pgDetail
  let withHint := withDetail ++ jsonOptionalField "hint" e.pgHint
  s!"\{{withHint}}"

/-- Produce a JSON error body for any PostgREST `Error`.
    $$\text{errorPayload} : \text{Error} \to \text{String}$$ -/
def errorPayload : Error → String
  | .apiRequestError e => apiRequestErrorPayload e
  | .jwtError e =>
    let msg := match e with
      | .tokenInvalid msg => s!"JWT invalid: {msg}"
      | .tokenExpired => "JWT expired"
      | .tokenMissing => "JWT missing"
      | .secretNotConfigured => "JWT secret not configured"
    s!"\{\"message\":{jsonString msg},\"code\":\"PGRST301\"}"
  | .pgError e _ => pgErrorPayload e
  | .schemaCacheError e =>
    let msg := match e with
      | .connectionLost msg => s!"Connection to database lost: {msg}"
      | .loadError msg => s!"Could not load schema cache: {msg}"
      | .pgVersionUnsupported version =>
        s!"PostgreSQL version '{version}' is not supported"
    s!"\{\"message\":{jsonString msg},\"code\":\"PGRST400\"}"
  | .singularViolation count =>
    s!"\{\"message\":\"JSON object requested, {count} rows returned\",\"code\":\"PGRST505\"}"
  | .notFound =>
    s!"\{\"message\":\"Not Found\",\"code\":\"PGRST000\"}"
  | .gucHeadersError msg =>
    s!"\{\"message\":{jsonString msg},\"code\":\"PGRST500\"}"
  | .gucStatusError msg =>
    s!"\{\"message\":{jsonString msg},\"code\":\"PGRST500\"}"
  | .offLimitsChangesError count maxSize =>
    s!"\{\"message\":\"Payload Too Large: {count} rows affected, max {maxSize}\",\"code\":\"PGRST504\"}"

-- ────────────────────────────────────────────────────────────────────
-- Error headers
-- ────────────────────────────────────────────────────────────────────

/-- Produce HTTP response headers for a PostgREST error.
    Always includes `Content-Type: application/json`.
    JWT errors additionally include `WWW-Authenticate: Bearer`.
    $$\text{errorHeaders} : \text{Error} \to \text{List}\ (\text{String} \times \text{String})$$ -/
def errorHeaders : Error → List (String × String)
  | .jwtError _ =>
    [ ("Content-Type", "application/json; charset=utf-8"),
      ("WWW-Authenticate", "Bearer") ]
  | .singularViolation _ =>
    [ ("Content-Type", "application/json; charset=utf-8") ]
  | .pgError e authenticated =>
    let base := [("Content-Type", "application/json; charset=utf-8")]
    if e.pgCode == "42501" && !authenticated then
      base ++ [("WWW-Authenticate", "Bearer")]
    else
      base
  | _ =>
    [ ("Content-Type", "application/json; charset=utf-8") ]

end PostgREST.Error
