/-
  Linen.CDP.Runtime — CDP client runtime: connect, send commands, subscribe to events

  Ports `CDP.Runtime` (see `docs/imports/cdp/dependencies.md`): the WebSocket
  message loop, command/response correlation, and event-subscription
  machinery built on top of `CDP.Internal.Utils`'s `Handle`/`Config` and
  `CDP.Endpoints`'s address resolution.

  Upstream's `listenThread :: ThreadId` (obtained from `forkIO`, then killed
  via `bracket` once the client app returns) has no direct analogue here:
  `linen`'s `Task` has no cooperative-cancellation hook wired through a
  blocking socket `recv`. The listen loop is instead left to end on its own
  once the connection closes (or the process exits) — the same outcome
  `killThread` guarantees promptly, just not on the same tight schedule.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Endpoints
import Linen.Network.WebSockets.Client

namespace CDP.Runtime

open CDP.Internal.Utils
open CDP.Endpoints
open Data.Json (Value ToJSON FromJSON)

/-- A client application: given a live `Handle`, does whatever it likes with
    it and returns a result once done. -/
abbrev ClientApp (β : Type) := Handle → IO β

instance : ToJSON CommandId where
  toJSON cid := ToJSON.toJSON cid.val

instance : FromJSON CommandId where
  parseJSON v := return { val := ← FromJSON.parseJSON v }

/-- Decode an optional field: absent is `none`, present-but-malformed is an error. -/
private def optField [FromJSON α] (v : Value) (key : String) : Except String (Option α) := do
  match ← Value.getFieldOpt v key with
  | some fv => some <$> FromJSON.parseJSON fv
  | none => pure none

-- ── Incoming messages ──

/-- A message from the browser. We don't know yet if this is a command
    response or an event. -/
structure IncomingMessage where
  method : Option String
  params : Option Value
  sessionId : Option SessionId
  id : Option CommandId
  error : Option ProtocolError
  result : Option Value

instance : FromJSON IncomingMessage where
  parseJSON v := do
    .ok
      { method := ← optField v "method"
        params := ← optField v "params"
        sessionId := ← optField v "sessionId"
        id := ← optField v "id"
        error := ← optField v "error"
        result := ← optField v "result" }

/-- Deliver a command's response (or error) to whoever is waiting on it. -/
def dispatchCommandResponse (handle : Handle) (commandId : CommandId)
    (mbErr : Option ProtocolError) (mbVal : Option Value) : IO Unit := do
  let mbMVar ← handle.commandBuffer.modifyGet fun buffer =>
    match buffer.get? commandId with
    | none => (none, buffer)
    | some mv => (some mv, buffer.erase commandId)
  match mbMVar with
  | none => pure ()
  | some mv =>
    let _ ← Control.Concurrent.MVar.put mv
      (match mbErr with
        | some err => .error err
        | none => .ok (mbVal.getD .null))

/-- Deliver an event to every handler subscribed to it. -/
def dispatchEvent (handle : Handle) (mbSessionId : Option SessionId) (method : String)
    (mbParams : Option Value) : IO Unit := do
  let subs ← handle.subscriptions.get
  match subs.handlers.get? (method, mbSessionId) with
  | none => IO.eprintln s!"No handler for {method}"
  | some byId =>
    match mbParams with
    | none => IO.eprintln s!"No params for {method}"
    | some params => for (_, h) in byId do h params

-- ── Subscriptions ──

/-- A live subscription to an event, returned by `subscribe`/`subscribeForSession`. -/
structure Subscription where
  eventName : String
  sessionId : Option SessionId
  id : Nat

private def subscribe_ (α : Type) [FromJSON α] [Event α] (handle : Handle)
    (mbSessionId : Option SessionId) (handler : α → IO Unit) : IO Subscription := do
  let ename := Event.eventName (α := α)
  let handler2 : Value → IO Unit := fun val =>
    match FromJSON.parseJSON (α := α) val with
    | .error err => do IO.eprintln s!"Error parsing JSON: {err}"; IO.eprintln s!"Value: {repr val}"
    | .ok x => handler x
  let id ← handle.subscriptions.modifyGet fun s =>
    let id := s.nextId
    let byId := (s.handlers.getD (ename, mbSessionId) {}).insert id handler2
    (id, { s with nextId := id + 1, handlers := s.handlers.insert (ename, mbSessionId) byId })
  pure { eventName := ename, sessionId := mbSessionId, id }

/-- Subscribes to an event. -/
def subscribe (α : Type) [FromJSON α] [Event α] (handle : Handle) (handler : α → IO Unit)
    : IO Subscription :=
  subscribe_ α handle none handler

/-- Subscribes to an event for a given session. -/
def subscribeForSession (α : Type) [FromJSON α] [Event α] (handle : Handle)
    (sessionId : SessionId) (handler : α → IO Unit) : IO Subscription :=
  subscribe_ α handle (some sessionId) handler

/-- Unsubscribes from an event. -/
def unsubscribe (handle : Handle) (sub : Subscription) : IO Unit :=
  handle.subscriptions.modify fun s =>
    match s.handlers.get? (sub.eventName, sub.sessionId) with
    | none => s
    | some byId =>
      { s with handlers := s.handlers.insert (sub.eventName, sub.sessionId) (byId.erase sub.id) }

-- ── Commands ──

/-- A command response still in flight. -/
structure Promise (α : Type) where
  mvar : Control.Concurrent.MVar (Except ProtocolError Value)
  decode : Value → Except String α

/-- Resolves a promise to its value, blocking until the response arrives. -/
def readPromise (p : Promise α) : IO α := do
  match ← Control.Concurrent.MVar.readSync p.mvar with
  | .error err => throw (IO.userError (Error.toString (.protocol err)))
  | .ok v =>
    match p.decode v with
    | .ok x => pure x
    | .error err => throw (IO.userError (Error.toString (.parseError err)))

/-- Allocates the next command id, unique per connection. -/
def nextCommandId (handle : Handle) : IO CommandId := do
  let cur ← Control.Concurrent.MVar.takeSync handle.commandNextId
  Control.Concurrent.MVar.putSync handle.commandNextId { val := cur.val + 1 }
  pure cur

/-- Serialize a command envelope: `{sessionId?, id, method, params?}`. -/
private def commandEnvelope (mbSessionId : Option SessionId) (cid : CommandId) (method : String)
    (paramsJSON : Value) : Value :=
  .object <|
    (mbSessionId.map (fun sid => ("sessionId", ToJSON.toJSON sid))).toList ++
    [ ("id", ToJSON.toJSON cid), ("method", ToJSON.toJSON method) ] ++
    (match paramsJSON with
      | .null => []
      | pv => [("params", pv)])

private def sendCommand_ (α : Type) [ToJSON α] [Command α] (handle : Handle)
    (mbSessionId : Option SessionId) (params : α) : IO (Promise (Command.Response (α := α))) := do
  let cid ← nextCommandId handle
  let envelope := commandEnvelope mbSessionId cid (Command.commandName params) (ToJSON.toJSON params)
  let mv ← Control.Concurrent.MVar.newEmpty (Except ProtocolError Value)
  handle.commandBuffer.modify fun buffer => buffer.insert cid mv
  handle.conn.sendText (Data.Json.Encode.encode envelope)
  pure { mvar := mv, decode := Command.decodeResponse (α := α) }

/-- Sends a command to the browser. -/
def sendCommand (α : Type) [ToJSON α] [Command α] (handle : Handle) (params : α)
    : IO (Promise (Command.Response (α := α))) :=
  sendCommand_ α handle none params

/-- Sends a command to the browser for a given session. -/
def sendCommandForSession (α : Type) [ToJSON α] [Command α] (handle : Handle)
    (sessionId : SessionId) (params : α) : IO (Promise (Command.Response (α := α))) :=
  sendCommand_ α handle (some sessionId) params

private def sendCommandWait_ (α : Type) [ToJSON α] [Command α] (handle : Handle)
    (mbSessionId : Option SessionId) (params : α) : IO (Command.Response (α := α)) := do
  let promise ← sendCommand_ α handle mbSessionId params
  match handle.config.commandTimeout with
  | none => readPromise promise
  | some ms =>
    let resultRef ← IO.mkRef (none : Option (Except IO.Error (Command.Response (α := α))))
    let _task ← IO.asTask (prio := .dedicated) do
      try
        let r ← readPromise promise
        resultRef.set (some (.ok r))
      catch e =>
        resultRef.set (some (.error e))
    IO.sleep ms.toUInt32
    match ← resultRef.get with
    | some (.ok r) => pure r
    | some (.error e) => throw e
    | none => throw (IO.userError (Error.toString .noResponse))

/-- Sends a command to the browser and waits until a response is received,
    for the timeout duration configured. -/
def sendCommandWait (α : Type) [ToJSON α] [Command α] (handle : Handle) (params : α)
    : IO (Command.Response (α := α)) :=
  sendCommandWait_ α handle none params

/-- Sends a command to the browser for a given session and waits until a
    response is received, for the timeout duration configured. -/
def sendCommandForSessionWait (α : Type) [ToJSON α] [Command α] (handle : Handle)
    (sessionId : SessionId) (params : α) : IO (Command.Response (α := α)) :=
  sendCommandWait_ α handle (some sessionId) params

/-- A command whose type has been erased, retaining only its `Command`
    instance. -/
structure SomeCommand where
  {cmd : Type}
  [tinst : ToJSON cmd]
  [cinst : Command cmd]
  val : cmd

/-- Eliminate a `SomeCommand` with a function polymorphic over every
    `Command` instance. -/
def fromSomeCommand (f : {cmd : Type} → [ToJSON cmd] → [Command cmd] → cmd → r) (sc : SomeCommand) : r :=
  @f sc.cmd sc.tinst sc.cinst sc.val

-- ── Connecting ──

/-- Runs a client application against the browser's remote-debugging port.
    By default, the connection targets a page (see `Config.connectToBrowser`
    to target the browser itself). The connection stays open for the
    duration of `app`. -/
def runClient (α : Type) (config : Config) (app : ClientApp α) : IO α := do
  let commandNextId ← Control.Concurrent.MVar.new (⟨0⟩ : CommandId)
  let subscriptions ← IO.mkRef ({} : Subscriptions)
  let commandBuffer ←
    IO.mkRef (({} : Std.HashMap CommandId (Control.Concurrent.MVar (Except ProtocolError Value))))
  let (host, port, path) ←
    if config.connectToBrowser then CDP.Endpoints.browserAddress config.hostPort
    else CDP.Endpoints.pageAddress config.hostPort
  Network.WebSockets.Client.runClient host port.toUInt16 path fun conn => do
    let handleRef ← IO.mkRef (none : Option Handle)
    let listenTask ← IO.asTask (prio := .dedicated) do
      while true do
        match ← conn.getState with
        | .closed => return
        | _ =>
          let bs ← conn.receiveText
          match (Data.Json.Decode.decodeAs bs : Except String IncomingMessage) with
          | .error err => IO.eprintln s!"Could not parse message: {err}"
          | .ok im =>
            match ← handleRef.get with
            | none => pure ()
            | some handle =>
              match im.method with
              | some method => dispatchEvent handle im.sessionId method im.params
              | none =>
                match im.id with
                | some cid => dispatchCommandResponse handle cid im.error im.result
                | none => pure ()
    let handle : Handle := { config, commandNextId, subscriptions, commandBuffer, conn, listenTask }
    handleRef.set (some handle)
    app handle

end CDP.Runtime
