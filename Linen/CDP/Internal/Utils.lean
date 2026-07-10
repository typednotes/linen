/-
  Linen.CDP.Internal.Utils — CDP protocol runtime primitives

  Ports `CDP.Internal.Utils` (see `docs/imports/cdp/dependencies.md`). Two pieces
  of the upstream module are dead code in the source library itself — never read
  anywhere in `src/`, verified by grep — and are dropped here: the `Handle`
  field `responseBuffer` (written on connect, never read) and the top-level
  `uncapitalizeFirst` helper (never called).
-/
import Linen.Data.Json
import Linen.Data.Default
import Linen.Control.Concurrent.MVar
import Linen.Network.WebSockets.Connection
import Std.Data.HashMap

namespace CDP.Internal.Utils

open Data.Json (Value ToJSON FromJSON)

-- ── Identifiers ──

/-- A JSON-RPC command's id, unique per connection. -/
structure CommandId where
  val : Nat
  deriving BEq, Hashable, Repr, Inhabited, Ord

/-- The session a command/event is scoped to (for multi-target/session
    debugging), when present. -/
abbrev SessionId := String

-- ── Errors ──

/-- An error reported by the browser in a JSON-RPC response
    (`{"error": {"code": ..., "message": ...}}`), classified by its JSON-RPC
    error code. -/
inductive ProtocolError where
  /-- Invalid JSON was received by the server. -/
  | parseError (message : String)
  /-- The JSON sent is not a valid Request object. -/
  | invalidRequest (message : String)
  /-- The method does not exist / is not available. -/
  | methodNotFound (message : String)
  /-- Invalid method parameter(s). -/
  | invalidParams (message : String)
  /-- Internal JSON-RPC error. -/
  | internalError (message : String)
  /-- Server error. -/
  | serverError (message : String)
  /-- An uncategorized error. -/
  | other (message : String)
  deriving Repr, BEq, DecidableEq

/-- Render a `ProtocolError` the way upstream's `Show` instance does. -/
def ProtocolError.toString : ProtocolError → String
  | .parseError m => s!"Server parsing protocol error:\n{m}"
  | .invalidRequest m => s!"Invalid request protocol error:\n{m}"
  | .methodNotFound m => s!"Method not found protocol error:\n{m}"
  | .invalidParams m => s!"Invalid params protocol error:\n{m}"
  | .internalError m => s!"Internal protocol error:\n{m}"
  | .serverError m => s!"Server protocol error:\n{m}"
  | .other m => s!"Other protocol error:\n{m}"

instance : ToString ProtocolError := ⟨ProtocolError.toString⟩

/-- Classify a JSON-RPC error code (JSON-RPC 2.0 §5.1) into a `ProtocolError`. -/
def ProtocolError.ofCode (code : Int) (message : String) : ProtocolError :=
  if code == -32700 then .parseError message
  else if code == -32600 then .invalidRequest message
  else if code == -32601 then .methodNotFound message
  else if code == -32602 then .invalidParams message
  else if code == -32603 then .internalError message
  else if code > -32099 && code < -32000 then .serverError message
  else .other message

instance : FromJSON ProtocolError where
  parseJSON v := do
    let codeV ← Value.getField v "code"
    let code ← FromJSON.parseJSON (α := Int) codeV
    let message ← Value.getField v "message" >>= FromJSON.parseJSON (α := String)
    .ok (ProtocolError.ofCode code message)

/-- Errors this client can raise. -/
inductive Error where
  /-- No response was received from the browser (e.g. the configured
      `commandTimeout` elapsed). -/
  | noResponse
  /-- A message from the browser failed to parse. -/
  | parseError (message : String)
  /-- The browser reported a protocol-level error. -/
  | protocol (err : ProtocolError)
  deriving Repr, BEq

/-- Render an `Error` the way upstream's `Show` instance does. -/
def Error.toString : Error → String
  | .noResponse => "no response received from the browser"
  | .parseError m => s!"error in parsing a message received from the browser:\n{m}"
  | .protocol pe => s!"error encountered by the browser:\n{ProtocolError.toString pe}"

instance : ToString Error := ⟨Error.toString⟩

-- ── Connection configuration and state ──

/-- Handlers subscribed to CDP events, keyed by `(event name, session)`. -/
structure Subscriptions where
  handlers : Std.HashMap (String × Option SessionId) (Std.HashMap Nat (Value → IO Unit)) := {}
  nextId : Nat := 0

/-- Connection settings. -/
structure Config where
  /-- Host and port of the browser's remote-debugging endpoint. -/
  hostPort : String × Nat := ("http://127.0.0.1", 9222)
  /-- Target of the initial connection. If `false` (the default), it targets a
      page rather than the browser itself. -/
  connectToBrowser : Bool := false
  doLogResponses : Bool := false
  /-- How long to wait for a command response; `none` waits forever. -/
  commandTimeout : Option Nat := none
  deriving Repr, Inhabited

instance : Data.Default Config := ⟨{}⟩

/-- A live connection to the browser. -/
structure Handle where
  config : Config
  commandNextId : Control.Concurrent.MVar CommandId
  subscriptions : IO.Ref Subscriptions
  commandBuffer : IO.Ref (Std.HashMap CommandId (Control.Concurrent.MVar (Except ProtocolError Value)))
  conn : Network.WebSockets.Connection
  /-- The background task dispatching incoming messages; see `CDP.Runtime.runClient`. -/
  listenTask : Task (Except IO.Error Unit)

-- ── Commands and events ──

/-- An event a client can subscribe to via `CDP.Runtime.subscribe`. -/
class Event (α : Type) [FromJSON α] where
  /-- The event's CDP method name, e.g. `"Page.loadEventFired"`. -/
  eventName : String

/-- A command that can be sent to the browser via `CDP.Runtime.sendCommand`. -/
class Command (α : Type) [ToJSON α] where
  /-- The type of a successful response's `result` value. -/
  Response : Type
  /-- The command's CDP method name, e.g. `"Page.navigate"`. -/
  commandName : α → String
  /-- Decode a successful response's `result` value. Commands with no
      meaningful response (`result: {}`) override this to ignore the value and
      always succeed, matching upstream's `fromJSON = const . A.Success . const
      ()` commands. -/
  decodeResponse : Value → Except String Response

end CDP.Internal.Utils
