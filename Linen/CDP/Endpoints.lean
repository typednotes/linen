/-
  Linen.CDP.Endpoints — HTTP endpoints exposed by a browser's remote-debugging port

  Ports `CDP.Endpoints` (see `docs/imports/cdp/dependencies.md`): the
  `/json/*` HTTP endpoints a Chrome-family browser exposes alongside its
  WebSocket debugger, used to discover/open/activate/close tabs and to fetch
  the live protocol schema before establishing the WebSocket connection
  proper (`CDP.Runtime`).
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Definition
import Linen.Network.HTTP.Simple
import Linen.Network.URI

namespace CDP.Endpoints

open CDP.Internal.Utils
open Data.Json (Value FromJSON)
open Network.HTTP.Types

-- ── Marker types ──

abbrev URL := String
abbrev TargetId := String

/-- Fetch the browser's version info (`/json/version`). -/
structure EPBrowserVersion where
  deriving Repr, BEq

/-- List every open target/tab (`/json/list`). -/
structure EPAllTargets where
  deriving Repr, BEq

/-- Fetch the live protocol schema served by the browser (`/json/protocol`). -/
structure EPCurrentProtocol where
  deriving Repr, BEq

/-- Open a new tab at `url` (`/json/new`). -/
structure EPOpenNewTab where
  url : URL
  deriving Repr, BEq

/-- Bring a target to the foreground (`/json/activate/<id>`). -/
structure EPActivateTarget where
  targetId : TargetId
  deriving Repr, BEq

/-- Close a target (`/json/close/<id>`). -/
structure EPCloseTarget where
  targetId : TargetId
  deriving Repr, BEq

/-- Fetch the DevTools frontend page (`/devtools/inspector.html`). -/
structure EPFrontend where
  deriving Repr, BEq

-- ── Responses ──

/-- The response to `EPBrowserVersion` (`/json/version`). -/
structure BrowserVersion where
  browser : String
  protocolVersion : String
  userAgent : String
  v8Version : String
  webKitVersion : String
  webSocketDebuggerUrl : String
  deriving Repr, BEq

instance : FromJSON BrowserVersion where
  parseJSON v := do
    .ok
      { browser := ← Value.getField v "Browser" >>= FromJSON.parseJSON
        protocolVersion := ← Value.getField v "Protocol-Version" >>= FromJSON.parseJSON
        userAgent := ← Value.getField v "User-Agent" >>= FromJSON.parseJSON
        v8Version := ← Value.getField v "V8-Version" >>= FromJSON.parseJSON
        webKitVersion := ← Value.getField v "WebKit-Version" >>= FromJSON.parseJSON
        webSocketDebuggerUrl := ← Value.getField v "webSocketDebuggerUrl" >>= FromJSON.parseJSON }

/-- The response to `EPOpenNewTab`/an element of `EPAllTargets`'s response. -/
structure TargetInfo where
  description : String
  devtoolsFrontendUrl : String
  id : String
  title : String
  type : String
  url : String
  webSocketDebuggerUrl : String
  deriving Repr, BEq

instance : FromJSON TargetInfo where
  parseJSON v := do
    .ok
      { description := ← Value.getField v "description" >>= FromJSON.parseJSON
        devtoolsFrontendUrl := ← Value.getField v "devtoolsFrontendUrl" >>= FromJSON.parseJSON
        id := ← Value.getField v "id" >>= FromJSON.parseJSON
        title := ← Value.getField v "title" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        webSocketDebuggerUrl := ← Value.getField v "webSocketDebuggerUrl" >>= FromJSON.parseJSON }

-- ── The `Endpoint` class ──

/-- An HTTP endpoint exposed by a browser's remote-debugging port. -/
class Endpoint (ep : Type) where
  /-- The type of a successful response. -/
  Response : Type
  /-- Perform the request against `host:port` and decode the response. -/
  getEndpoint : String × Nat → ep → IO Response
  /-- Decode a raw response body. -/
  epDecode : ByteArray → Except String Response

/-- Strip a `http://`/`https://` scheme prefix, reporting whether it was secure. -/
private def stripScheme (s : String) : Bool × String :=
  if s.startsWith "https://" then (true, (s.drop 8).toString)
  else if s.startsWith "http://" then (false, (s.drop 7).toString)
  else (false, s)

/-- Build a `GET` request for `path` (joined with `/`) with an optional raw
    query string (`param`, without the leading `?`), against `hostPort`
    (Config's `(scheme://host, port)` pair). -/
def getRequest (hostPort : String × Nat) (path : List String) (param : Option String)
    : Network.HTTP.Client.Request :=
  let (isSecure, host) := stripScheme hostPort.1
  { method := Method.standard .GET
    host, isSecure
    port := hostPort.2.toUInt16
    path := "/" ++ String.intercalate "/" path
    queryString := match param with
      | some p => s!"?{p}"
      | none => "" }

/-- Decode a JSON response body via its `FromJSON` instance, as `A.eitherDecode` does. -/
private def decodeJSON [FromJSON α] (body : ByteArray) : Except String α :=
  Data.Json.Decode.decodeAs (String.fromUTF8! body)

/-- Perform `req` and decode its body with `decode`. -/
private def performRequestWith (decode : ByteArray → Except String α)
    (req : Network.HTTP.Client.Request) : IO α := do
  let resp ← Network.HTTP.Simple.httpBS req
  match decode resp.body with
  | .ok v => pure v
  | .error e => throw (IO.userError e)

/-- Perform `req` and decode its body via `Endpoint.epDecode`. -/
def performRequest (ep : Type) [Endpoint ep] (req : Network.HTTP.Client.Request)
    : IO (Endpoint.Response ep) :=
  performRequestWith (Endpoint.epDecode (ep := ep)) req

instance : Endpoint EPBrowserVersion where
  Response := BrowserVersion
  getEndpoint hostPort _ := performRequestWith decodeJSON (getRequest hostPort ["json", "version"] none)
  epDecode := decodeJSON

instance : Endpoint EPAllTargets where
  Response := List TargetInfo
  getEndpoint hostPort _ := performRequestWith decodeJSON (getRequest hostPort ["json", "list"] none)
  epDecode := decodeJSON

instance : Endpoint EPCurrentProtocol where
  Response := CDP.Definition.TopLevel
  getEndpoint hostPort _ := performRequestWith decodeJSON (getRequest hostPort ["json", "protocol"] none)
  epDecode := decodeJSON

instance : Endpoint EPOpenNewTab where
  Response := TargetInfo
  getEndpoint hostPort ep := performRequestWith decodeJSON (getRequest hostPort ["json", "new"] (some ep.url))
  epDecode := decodeJSON

instance : Endpoint EPActivateTarget where
  Response := Unit
  getEndpoint hostPort ep :=
    performRequestWith (fun _ => .ok ()) (getRequest hostPort ["json", "activate", ep.targetId] none)
  epDecode := fun _ => .ok ()

instance : Endpoint EPCloseTarget where
  Response := Unit
  getEndpoint hostPort ep :=
    performRequestWith (fun _ => .ok ()) (getRequest hostPort ["json", "close", ep.targetId] none)
  epDecode := fun _ => .ok ()

instance : Endpoint EPFrontend where
  Response := ByteArray
  getEndpoint hostPort _ := performRequestWith .ok (getRequest hostPort ["devtools", "inspector.html"] none)
  epDecode := .ok

-- ── Existential wrapper ──

/-- An endpoint whose type has been erased, retaining only its `Endpoint`
    instance. -/
structure SomeEndpoint where
  {ep : Type}
  [inst : Endpoint ep]
  val : ep

/-- Eliminate a `SomeEndpoint` with a function polymorphic over every
    `Endpoint` instance. -/
def fromSomeEndpoint (f : {ep : Type} → [Endpoint ep] → ep → α) (se : SomeEndpoint) : α :=
  @f se.ep se.inst se.val

/-- Send a request to the corresponding endpoint. -/
def endpoint (ep : Type) [Endpoint ep] (config : Config) (val : ep) : IO (Endpoint.Response ep) :=
  Endpoint.getEndpoint config.hostPort val

/-- Open a new tab at `url` and switch to it, returning its `TargetInfo`. -/
def connectToTab (config : Config) (url : URL) : IO TargetInfo := do
  let targetInfo ← endpoint EPOpenNewTab config { url }
  let _ ← endpoint EPActivateTarget config { targetId := targetInfo.id }
  pure targetInfo

-- ── Address resolution ──

/-- Parse `(host, port, path)` out of an absolute URI, as returned in
    `webSocketDebuggerUrl` fields. -/
def parseUri (uri : String) : Option (String × Nat × String) := do
  let u ← Network.URI.parseURI uri
  let auth ← u.uriAuthority
  let port ← match auth.uriPort.toList with
    | ':' :: rest => (String.ofList rest).toNat?
    | _ => some 80
  some (auth.uriRegName, port, u.uriPath)

/-- Resolve `(host, port, path)` for connecting directly to the browser's own
    debugger WebSocket. -/
def browserAddress (hostPort : String × Nat) : IO (String × Nat × String) := do
  let bv ← Endpoint.getEndpoint (ep := EPBrowserVersion) hostPort {}
  match parseUri bv.webSocketDebuggerUrl with
  | some addr => pure addr
  | none => throw (IO.userError "invalid URI when connecting to browser")

/-- Resolve `(host, port, path)` for connecting to the first open page/tab. -/
def pageAddress (hostPort : String × Nat) : IO (String × Nat × String) := do
  let targets ← Endpoint.getEndpoint (ep := EPAllTargets) hostPort {}
  match targets.head? with
  | none => throw (IO.userError "invalid URI when connecting to page")
  | some t =>
    match parseUri t.webSocketDebuggerUrl with
    | some addr => pure addr
    | none => throw (IO.userError "invalid URI when connecting to page")

end CDP.Endpoints
