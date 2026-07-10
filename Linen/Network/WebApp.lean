/-
  Linen.Network.WebApp — Web Application Interface

  Public API. Re-exports the core types from `Network.WebApp.Internal` and
  provides convenience functions.

  Ports `Network.Wai`, renamed from the Haskell-specific acronym
  `WAI` to `WebApp` per this project's naming convention.

  ## Design

  The overriding design principles are performance and generality. Uses a
  streaming interface for request and response bodies paired with
  `ByteArray`.

  ## Lean 4 Dependent-Type Guarantees (compile-time, zero-cost)

  - **Exactly-once response:** The `AppM .pending .sent` indexed monad
    enforces at the type level that every `Application` invokes `respond`
    exactly once.  Double-respond and no-respond are both type errors.
  - **Middleware algebra:** `Middleware = Application → Application`.
    Proven: `id ∘ m = m`, `m ∘ id = m`, `modifyRequest id = id`,
    `modifyResponse id = id`, `ifRequest (fun _ => false) m = id`.
  - **CPS bracket safety:** The continuation-passing style lets the server
    bracket resources (buffers, connections) around the response.
  - **Streaming body:** Request body reading is non-idempotent (each chunk
    consumed once) -- encoded by the `IO ByteArray` action type.
-/

import Linen.Network.WebApp.Internal

namespace Network.WebApp

open Network.HTTP.Types

-- ── Response constructors ──

/-- Create a simple response from a status, headers, and body ByteArray.
    $$\text{responseLBS} : \text{Status} \to \text{ResponseHeaders} \to \text{String} \to \text{Response}$$ -/
def responseLBS (status : Status)
    (headers : ResponseHeaders) (body : String) : Response :=
  .responseBuilder status headers body.toUTF8

/-- Create a file response. -/
def responseFile' (status : Status)
    (headers : ResponseHeaders)
    (path : String) (part : Option Network.Sendfile.FilePart := none) : Response :=
  .responseFile status headers path part

/-- Create a streaming response. -/
def responseStream' (status : Status)
    (headers : ResponseHeaders)
    (body : StreamingBody) : Response :=
  .responseStream status headers body

-- ── Request accessors ──

/-- Get the next chunk of the request body. Returns empty ByteArray when
    the body is fully consumed. Preferred over direct `requestBody` access.
    $$\text{getRequestBodyChunk} : \text{Request} \to \text{IO}(\text{ByteArray})$$ -/
@[inline] def getRequestBodyChunk (req : Request) : IO ByteArray :=
  req.requestBody

/-- Set the request body chunks IO action on a request.
    The supplied IO action should return the next chunk each time it is called
    and empty ByteArray when fully consumed.
    $$\text{setRequestBodyChunks} : \text{IO}(\text{ByteArray}) \to \text{Request} \to \text{Request}$$ -/
def setRequestBodyChunks (body : IO ByteArray) (req : Request) : Request :=
  { req with requestBody := body }

/-- Get a header value from a request by name. -/
def requestHeader (name : HeaderName)
    (req : Request) : Option String :=
  req.requestHeaders.find? (fun (n, _) => n == name) |>.map (·.2)

/-- Read the entire request body strictly into memory.
    Returns all chunks concatenated as a single ByteArray.

    **Warning:** This consumes the request body. Future calls return empty.
    Consider using `getRequestBodyChunk` for streaming when possible.

    The upstream original is a `partial def` (an unbounded chunk-reading loop with
    no decreasing measure to prove termination against — the `IO ByteArray`
    action can in principle keep returning nonempty chunks forever). Here the
    same unbounded reading is expressed as a `while` loop over local mutable
    state instead of an explicit recursive call, matching this project's
    established idiom (e.g. `Network.HTTP.Client.Response`'s body readers) —
    which needs no termination proof and keeps `strictRequestBody` a plain,
    total `def`.
    $$\text{strictRequestBody} : \text{Request} \to \text{IO}(\text{ByteArray})$$ -/
def strictRequestBody (req : Request) : IO ByteArray := do
  let mut result := ByteArray.empty
  let mut done := false
  while !done do
    let chunk ← getRequestBodyChunk req
    if chunk.isEmpty then
      done := true
    else
      result := result ++ chunk
  return result

/-- Synonym for `strictRequestBody`.
    Name signals the non-idempotent (consuming) nature. -/
abbrev consumeRequestBodyStrict := @strictRequestBody

/-- A default, blank request. -/
def defaultRequest : Request where
  requestMethod := .standard .GET
  httpVersion := http10
  rawPathInfo := ""
  rawQueryString := ""
  requestHeaders := []
  isSecure := false
  remoteHost := ⟨"0.0.0.0", 0⟩
  pathInfo := []
  queryString := []
  requestBody := pure ByteArray.empty
  vault := Data.Vault.empty
  requestBodyLength := .knownLength 0
  requestHeaderHost := none
  requestHeaderRange := none
  requestHeaderReferer := none
  requestHeaderUserAgent := none

-- ── Request modifiers ──

/-- Apply the provided function to the request header list.
    $$\text{mapRequestHeaders} : (H \to H) \to \text{Request} \to \text{Request}$$ -/
def mapRequestHeaders (f : RequestHeaders → RequestHeaders)
    (req : Request) : Request :=
  { req with requestHeaders := f req.requestHeaders }

-- ── Middleware ──

/-- The identity middleware (does nothing). -/
def idMiddleware : Middleware := id

/-- Compose two middlewares.
    $$\text{composeMiddleware}(f, g) = f \circ g$$ -/
@[inline] def composeMiddleware (f g : Middleware) : Middleware := f ∘ g

/-- Add a header to the response. -/
def addHeader (name : HeaderName) (val : String)
    (resp : Response) : Response :=
  resp.mapResponseHeaders ((name, val) :: ·)

/-- Apply a function that modifies a request as a Middleware.
    $$\text{modifyRequest} : (\text{Request} \to \text{Request}) \to \text{Middleware}$$ -/
def modifyRequest (f : Request → Request) : Middleware :=
  fun app req respond => app (f req) respond

/-- Apply a function that modifies a response as a Middleware.
    $$\text{modifyResponse} : (\text{Response} \to \text{Response}) \to \text{Middleware}$$ -/
def modifyResponse (f : Response → Response) : Middleware :=
  fun app req respond => app req (respond ∘ f)

/-- Conditionally apply a Middleware based on a request predicate.
    $$\text{ifRequest}(p, m) = \begin{cases} m & \text{if } p(\text{req}) \\ \text{id} & \text{otherwise} \end{cases}$$ -/
def ifRequest (pred : Request → Bool) (middle : Middleware) : Middleware :=
  fun app req respond =>
    if pred req then middle app req respond
    else app req respond

-- ── Middleware algebraic properties ──

/-- Identity middleware is left identity for composition.
    $$\text{id} \circ m = m$$ -/
theorem idMiddleware_comp_left (m : Middleware) : composeMiddleware idMiddleware m = m := rfl

/-- Identity middleware is right identity for composition.
    $$m \circ \text{id} = m$$ -/
theorem idMiddleware_comp_right (m : Middleware) : composeMiddleware m idMiddleware = m := rfl

/-- `modifyRequest id` is the identity middleware. -/
theorem modifyRequest_id : modifyRequest id = (idMiddleware : Middleware) := rfl

/-- `modifyResponse id` is the identity middleware. -/
theorem modifyResponse_id : modifyResponse id = (idMiddleware : Middleware) := rfl

/-- `ifRequest (fun _ => false)` always passes through.
    $$\text{ifRequest}(\bot, m) = \text{id}$$ -/
theorem ifRequest_false (middle : Middleware) :
    ifRequest (fun _ => false) middle = (idMiddleware : Middleware) := rfl

end Network.WebApp
