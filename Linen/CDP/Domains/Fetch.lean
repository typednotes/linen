/-
  Linen.CDP.Domains.Fetch — the `Fetch` CDP domain

  A domain for letting clients substitute the browser's network layer with
  client code. Ports `CDP.Domains.Fetch` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.Memory`'s docstring. Cross-domain
  references to the `IO` domain follow `CDP.Domains.IO`'s docstring: `open
  CDP.Domains` and refer to `IO.StreamHandle` unambiguously; likewise
  `DOMPageNetworkEmulationSecurity.Network.…`/`DOMPageNetworkEmulationSecurity.Page.…`
  for the request/network types this domain's events and commands embed.

  None of this module's own types are self- or mutually-recursive. `RequestPaused`
  and `AuthRequired` embed `Network.Request`, which (per
  `CDP.Domains.DOMPageNetworkEmulationSecurity`) derives full `DecidableEq`, so
  every type here derives `DecidableEq` too.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.IO

namespace CDP.Domains.Fetch

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)
open CDP.Domains

-- ── Types ──

/-- Unique request identifier. -/
abbrev RequestId := String

/-- Stages of the request to handle. `request` will intercept before the
    request is sent. `response` will intercept after the response is received
    (but before the response body is received). -/
inductive RequestStage where
  | request | response
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestStage where
  parseJSON
    | .string "Request" => .ok .request
    | .string "Response" => .ok .response
    | v => .error s!"failed to parse RequestStage: {repr v}"

instance : ToJSON RequestStage where
  toJSON
    | .request => .string "Request"
    | .response => .string "Response"

/-- A pattern for requests to intercept. -/
structure RequestPattern where
  /-- Wildcards (`'*'` -> zero or more, `'?'` -> exactly one) are allowed.
      Escape character is backslash. Omitting is equivalent to `"*"`. -/
  urlPattern : Option String := none
  /-- If set, only requests for matching resource types will be intercepted. -/
  resourceType : Option DOMPageNetworkEmulationSecurity.Network.ResourceType := none
  /-- Stage at which to begin intercepting requests. Default is `request`. -/
  requestStage : Option RequestStage := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestPattern where
  parseJSON v := do
    .ok
      { urlPattern := ← (← Value.getFieldOpt v "urlPattern").mapM FromJSON.parseJSON
        resourceType := ← (← Value.getFieldOpt v "resourceType").mapM FromJSON.parseJSON
        requestStage := ← (← Value.getFieldOpt v "requestStage").mapM FromJSON.parseJSON }

instance : ToJSON RequestPattern where
  toJSON p := Data.Json.object <|
       (p.urlPattern.map fun x => ("urlPattern", ToJSON.toJSON x)).toList
    ++ (p.resourceType.map fun x => ("resourceType", ToJSON.toJSON x)).toList
    ++ (p.requestStage.map fun x => ("requestStage", ToJSON.toJSON x)).toList

/-- A response HTTP header entry. -/
structure HeaderEntry where
  name : String
  value : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON HeaderEntry where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON HeaderEntry where
  toJSON p := Data.Json.object [("name", ToJSON.toJSON p.name), ("value", ToJSON.toJSON p.value)]

/-- Source of an authorization challenge. -/
inductive AuthChallengeSource where
  | server | proxy
  deriving Repr, BEq, DecidableEq

instance : FromJSON AuthChallengeSource where
  parseJSON
    | .string "Server" => .ok .server
    | .string "Proxy" => .ok .proxy
    | v => .error s!"failed to parse AuthChallengeSource: {repr v}"

instance : ToJSON AuthChallengeSource where
  toJSON
    | .server => .string "Server"
    | .proxy => .string "Proxy"

/-- Authorization challenge for HTTP status code 401 or 407. -/
structure AuthChallenge where
  /-- Source of the authentication challenge. -/
  source : Option AuthChallengeSource := none
  /-- Origin of the challenger. -/
  origin : String
  /-- The authentication scheme used, such as basic or digest. -/
  scheme : String
  /-- The realm of the challenge. May be empty. -/
  realm : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON AuthChallenge where
  parseJSON v := do
    .ok
      { source := ← (← Value.getFieldOpt v "source").mapM FromJSON.parseJSON
        origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        scheme := ← Value.getField v "scheme" >>= FromJSON.parseJSON
        realm := ← Value.getField v "realm" >>= FromJSON.parseJSON }

instance : ToJSON AuthChallenge where
  toJSON p := Data.Json.object <|
       (p.source.map fun x => ("source", ToJSON.toJSON x)).toList
    ++ [("origin", ToJSON.toJSON p.origin)]
    ++ [("scheme", ToJSON.toJSON p.scheme)]
    ++ [("realm", ToJSON.toJSON p.realm)]

/-- The decision on what to do in response to an `AuthChallenge`. `default`
    means deferring to the default behavior of the net stack, which will
    likely either cancel authentication or display a popup dialog box. -/
inductive AuthChallengeResponseResponse where
  | default | cancelAuth | provideCredentials
  deriving Repr, BEq, DecidableEq

instance : FromJSON AuthChallengeResponseResponse where
  parseJSON
    | .string "Default" => .ok .default
    | .string "CancelAuth" => .ok .cancelAuth
    | .string "ProvideCredentials" => .ok .provideCredentials
    | v => .error s!"failed to parse AuthChallengeResponseResponse: {repr v}"

instance : ToJSON AuthChallengeResponseResponse where
  toJSON
    | .default => .string "Default"
    | .cancelAuth => .string "CancelAuth"
    | .provideCredentials => .string "ProvideCredentials"

/-- Response to an `AuthChallenge`. -/
structure AuthChallengeResponse where
  /-- The decision on what to do in response to the authorization challenge. -/
  response : AuthChallengeResponseResponse
  /-- The username to provide, possibly empty. Should only be set if
      `response` is `provideCredentials`. -/
  username : Option String := none
  /-- The password to provide, possibly empty. Should only be set if
      `response` is `provideCredentials`. -/
  password : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON AuthChallengeResponse where
  parseJSON v := do
    .ok
      { response := ← Value.getField v "response" >>= FromJSON.parseJSON
        username := ← (← Value.getFieldOpt v "username").mapM FromJSON.parseJSON
        password := ← (← Value.getFieldOpt v "password").mapM FromJSON.parseJSON }

instance : ToJSON AuthChallengeResponse where
  toJSON p := Data.Json.object <|
       [("response", ToJSON.toJSON p.response)]
    ++ (p.username.map fun x => ("username", ToJSON.toJSON x)).toList
    ++ (p.password.map fun x => ("password", ToJSON.toJSON x)).toList

-- ── Events ──

/-- The `Fetch.requestPaused` event: fired when a request is about to be sent
    or a response is about to be received, and matches a pattern registered
    with `enable`. -/
structure RequestPaused where
  /-- Each request the page makes will have a unique id. -/
  requestId : RequestId
  /-- The details of the request. -/
  request : DOMPageNetworkEmulationSecurity.Network.Request
  /-- The id of the frame that initiated the request. -/
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  /-- How the requested resource will be used. -/
  resourceType : DOMPageNetworkEmulationSecurity.Network.ResourceType
  /-- Response error if intercepted at response stage. -/
  responseErrorReason : Option DOMPageNetworkEmulationSecurity.Network.ErrorReason := none
  /-- Response code if intercepted at response stage. -/
  responseStatusCode : Option Int := none
  /-- Response status text if intercepted at response stage. -/
  responseStatusText : Option String := none
  /-- Response headers if intercepted at the response stage. -/
  responseHeaders : Option (List HeaderEntry) := none
  /-- If the intercepted request had a corresponding `Network.requestWillBeSent`
      event fired for it, then this `networkId` will be the same as the
      `requestId` present in the `requestWillBeSent` event. -/
  networkId : Option DOMPageNetworkEmulationSecurity.Network.RequestId := none
  /-- If the request is due to a redirect response from the server, the id of
      the request that has caused the redirect. -/
  redirectedRequestId : Option RequestId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestPaused where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        request := ← Value.getField v "request" >>= FromJSON.parseJSON
        frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        resourceType := ← Value.getField v "resourceType" >>= FromJSON.parseJSON
        responseErrorReason := ← (← Value.getFieldOpt v "responseErrorReason").mapM FromJSON.parseJSON
        responseStatusCode := ← (← Value.getFieldOpt v "responseStatusCode").mapM FromJSON.parseJSON
        responseStatusText := ← (← Value.getFieldOpt v "responseStatusText").mapM FromJSON.parseJSON
        responseHeaders := ← (← Value.getFieldOpt v "responseHeaders").mapM FromJSON.parseJSON
        networkId := ← (← Value.getFieldOpt v "networkId").mapM FromJSON.parseJSON
        redirectedRequestId := ← (← Value.getFieldOpt v "redirectedRequestId").mapM FromJSON.parseJSON }

instance : Event RequestPaused where
  eventName := "Fetch.requestPaused"

/-- The `Fetch.authRequired` event: fired when the resource identified by
    `requestId` obtained an authentication challenge which cannot be handled
    transparently. -/
structure AuthRequired where
  /-- Each request the page makes will have a unique id. -/
  requestId : RequestId
  /-- The details of the request. -/
  request : DOMPageNetworkEmulationSecurity.Network.Request
  /-- The id of the frame that initiated the request. -/
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  /-- How the requested resource will be used. -/
  resourceType : DOMPageNetworkEmulationSecurity.Network.ResourceType
  /-- Details of the authorization challenge encountered. If this is set, the
      client should respond with `continueRequest` that contains an
      `AuthChallengeResponse`. -/
  authChallenge : AuthChallenge
  deriving Repr, BEq, DecidableEq

instance : FromJSON AuthRequired where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        request := ← Value.getField v "request" >>= FromJSON.parseJSON
        frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        resourceType := ← Value.getField v "resourceType" >>= FromJSON.parseJSON
        authChallenge := ← Value.getField v "authChallenge" >>= FromJSON.parseJSON }

instance : Event AuthRequired where
  eventName := "Fetch.authRequired"

-- ── Commands ──

/-- Parameters of the `Fetch.disable` command: disables the fetch domain. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Fetch.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Fetch.enable` command: enables issuing of
    `requestPaused` events. A request will be paused until the client calls
    one of `failRequest`, `fulfillRequest` or `continueRequest`/`continueWithAuth`. -/
structure PEnable where
  /-- If specified, only requests matching any of these patterns will produce
      a `requestPaused` event and will be paused until the client responds.
      If not set, all requests will be affected. -/
  patterns : Option (List RequestPattern) := none
  /-- If true, `authRequired` events will be issued and requests will be
      paused expecting a call to `continueWithAuth`. -/
  handleAuthRequests : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where
  toJSON p := Data.Json.object <|
       (p.patterns.map fun x => ("patterns", ToJSON.toJSON x)).toList
    ++ (p.handleAuthRequests.map fun x => ("handleAuthRequests", ToJSON.toJSON x)).toList

instance : Command PEnable where
  Response := Unit
  commandName _ := "Fetch.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Fetch.failRequest` command: causes the request to fail
    with the specified reason. -/
structure PFailRequest where
  /-- An id the client received in a `requestPaused` event. -/
  requestId : RequestId
  /-- Causes the request to fail with the given reason. -/
  errorReason : DOMPageNetworkEmulationSecurity.Network.ErrorReason
  deriving Repr, BEq, DecidableEq

instance : ToJSON PFailRequest where
  toJSON p := Data.Json.object
    [("requestId", ToJSON.toJSON p.requestId), ("errorReason", ToJSON.toJSON p.errorReason)]

instance : Command PFailRequest where
  Response := Unit
  commandName _ := "Fetch.failRequest"
  decodeResponse _ := .ok ()

/-- Parameters of the `Fetch.fulfillRequest` command: provides a response to
    the request. -/
structure PFulfillRequest where
  /-- An id the client received in a `requestPaused` event. -/
  requestId : RequestId
  /-- An HTTP response code. -/
  responseCode : Int
  /-- Response headers. -/
  responseHeaders : Option (List HeaderEntry) := none
  /-- Alternative way of specifying response headers as a `\0`-separated
      series of `name: value` pairs. Prefer the above method unless you need
      to represent some non-UTF8 values that can't be transmitted over the
      protocol as text. (Encoded as a base64 string when passed over JSON.) -/
  binaryResponseHeaders : Option String := none
  /-- A response body. If absent, the original response body will be used if
      the request is intercepted at the response stage, and an empty body
      will be used if the request is intercepted at the request stage.
      (Encoded as a base64 string when passed over JSON.) -/
  body : Option String := none
  /-- A textual representation of `responseCode`. If absent, a standard
      phrase matching `responseCode` is used. -/
  responsePhrase : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PFulfillRequest where
  toJSON p := Data.Json.object <|
       [("requestId", ToJSON.toJSON p.requestId)]
    ++ [("responseCode", ToJSON.toJSON p.responseCode)]
    ++ (p.responseHeaders.map fun x => ("responseHeaders", ToJSON.toJSON x)).toList
    ++ (p.binaryResponseHeaders.map fun x => ("binaryResponseHeaders", ToJSON.toJSON x)).toList
    ++ (p.body.map fun x => ("body", ToJSON.toJSON x)).toList
    ++ (p.responsePhrase.map fun x => ("responsePhrase", ToJSON.toJSON x)).toList

instance : Command PFulfillRequest where
  Response := Unit
  commandName _ := "Fetch.fulfillRequest"
  decodeResponse _ := .ok ()

/-- Parameters of the `Fetch.continueRequest` command: continues the request,
    optionally modifying some of its parameters. -/
structure PContinueRequest where
  /-- An id the client received in a `requestPaused` event. -/
  requestId : RequestId
  /-- If set, the request url will be modified in a way that's not observable
      by the page. -/
  url : Option String := none
  /-- If set, the request method is overridden. -/
  method : Option String := none
  /-- If set, overrides the post data in the request. (Encoded as a base64
      string when passed over JSON.) -/
  postData : Option String := none
  /-- If set, overrides the request headers. Note that the overrides do not
      extend to subsequent redirect hops, if a redirect happens. Another
      override may be applied to a different request produced by a redirect. -/
  headers : Option (List HeaderEntry) := none
  /-- If set, overrides response interception behavior for this request. -/
  interceptResponse : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PContinueRequest where
  toJSON p := Data.Json.object <|
       [("requestId", ToJSON.toJSON p.requestId)]
    ++ (p.url.map fun x => ("url", ToJSON.toJSON x)).toList
    ++ (p.method.map fun x => ("method", ToJSON.toJSON x)).toList
    ++ (p.postData.map fun x => ("postData", ToJSON.toJSON x)).toList
    ++ (p.headers.map fun x => ("headers", ToJSON.toJSON x)).toList
    ++ (p.interceptResponse.map fun x => ("interceptResponse", ToJSON.toJSON x)).toList

instance : Command PContinueRequest where
  Response := Unit
  commandName _ := "Fetch.continueRequest"
  decodeResponse _ := .ok ()

/-- Parameters of the `Fetch.continueWithAuth` command: continues a request
    supplying an `authChallengeResponse` following an `authRequired` event. -/
structure PContinueWithAuth where
  /-- An id the client received in an `authRequired` event. -/
  requestId : RequestId
  /-- Response to the auth challenge. -/
  authChallengeResponse : AuthChallengeResponse
  deriving Repr, BEq, DecidableEq

instance : ToJSON PContinueWithAuth where
  toJSON p := Data.Json.object
    [ ("requestId", ToJSON.toJSON p.requestId)
    , ("authChallengeResponse", ToJSON.toJSON p.authChallengeResponse) ]

instance : Command PContinueWithAuth where
  Response := Unit
  commandName _ := "Fetch.continueWithAuth"
  decodeResponse _ := .ok ()

/-- Parameters of the `Fetch.continueResponse` command: continues loading of
    the paused response, optionally modifying the response headers. If either
    `responseCode` or `headers` are modified, all of them must be present. -/
structure PContinueResponse where
  /-- An id the client received in a `requestPaused` event. -/
  requestId : RequestId
  /-- An HTTP response code. If absent, the original response code will be
      used. -/
  responseCode : Option Int := none
  /-- A textual representation of `responseCode`. If absent, a standard
      phrase matching `responseCode` is used. -/
  responsePhrase : Option String := none
  /-- Response headers. If absent, the original response headers will be
      used. -/
  responseHeaders : Option (List HeaderEntry) := none
  /-- Alternative way of specifying response headers as a `\0`-separated
      series of `name: value` pairs. Prefer the above method unless you need
      to represent some non-UTF8 values that can't be transmitted over the
      protocol as text. (Encoded as a base64 string when passed over JSON.) -/
  binaryResponseHeaders : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PContinueResponse where
  toJSON p := Data.Json.object <|
       [("requestId", ToJSON.toJSON p.requestId)]
    ++ (p.responseCode.map fun x => ("responseCode", ToJSON.toJSON x)).toList
    ++ (p.responsePhrase.map fun x => ("responsePhrase", ToJSON.toJSON x)).toList
    ++ (p.responseHeaders.map fun x => ("responseHeaders", ToJSON.toJSON x)).toList
    ++ (p.binaryResponseHeaders.map fun x => ("binaryResponseHeaders", ToJSON.toJSON x)).toList

instance : Command PContinueResponse where
  Response := Unit
  commandName _ := "Fetch.continueResponse"
  decodeResponse _ := .ok ()

/-- Parameters of the `Fetch.getResponseBody` command: causes the body of the
    response to be received from the server and returned as a single string.
    May only be issued for a request that is paused in the response stage,
    and is mutually exclusive with `takeResponseBodyAsStream`. Calling other
    methods that affect the request, or disabling the fetch domain before the
    body is received, results in undefined behavior. -/
structure PGetResponseBody where
  /-- Identifier for the intercepted request to get the body for. -/
  requestId : RequestId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetResponseBody where
  toJSON p := Data.Json.object [("requestId", ToJSON.toJSON p.requestId)]

/-- Response of the `Fetch.getResponseBody` command. -/
structure GetResponseBody where
  /-- Response body. -/
  body : String
  /-- `true` if content was sent as base64. -/
  base64Encoded : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetResponseBody where
  parseJSON v := do
    .ok
      { body := ← Value.getField v "body" >>= FromJSON.parseJSON
        base64Encoded := ← Value.getField v "base64Encoded" >>= FromJSON.parseJSON }

instance : Command PGetResponseBody where
  Response := GetResponseBody
  commandName _ := "Fetch.getResponseBody"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Fetch.takeResponseBodyAsStream` command: returns a
    handle to the stream representing the response body. The request must be
    paused in the `HeadersReceived` stage. Note that after this command the
    request can't be continued as is — the client either needs to cancel it
    or provide the response body. The stream only supports sequential reads;
    `IO.read` will fail if a position is specified. This method is mutually
    exclusive with `getResponseBody`. Calling other methods that affect the
    request, or disabling the fetch domain before the body is received,
    results in undefined behavior. -/
structure PTakeResponseBodyAsStream where
  requestId : RequestId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTakeResponseBodyAsStream where
  toJSON p := Data.Json.object [("requestId", ToJSON.toJSON p.requestId)]

/-- Response of the `Fetch.takeResponseBodyAsStream` command. -/
structure TakeResponseBodyAsStream where
  stream : IO.StreamHandle
  deriving Repr, BEq, DecidableEq

instance : FromJSON TakeResponseBodyAsStream where
  parseJSON v := do .ok { stream := ← Value.getField v "stream" >>= FromJSON.parseJSON }

instance : Command PTakeResponseBodyAsStream where
  Response := TakeResponseBodyAsStream
  commandName _ := "Fetch.takeResponseBodyAsStream"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.Fetch
