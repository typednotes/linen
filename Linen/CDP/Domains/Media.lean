/-
  Linen.CDP.Domains.Media — the `Media` CDP domain

  Ports `CDP.Domains.Media` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Media

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

/-- Players get an id that is unique within the agent context. -/
abbrev PlayerId := String

/-- A media timestamp. -/
abbrev Timestamp := Float

/-- Corresponds to `kMessage` — one level per entry in `MediaLogRecord::Type`. -/
inductive PlayerMessageLevel where
  | error | warning | info | debug
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayerMessageLevel where
  parseJSON
    | .string "error" => .ok .error
    | .string "warning" => .ok .warning
    | .string "info" => .ok .info
    | .string "debug" => .ok .debug
    | v => .error s!"failed to parse PlayerMessageLevel: {repr v}"

instance : ToJSON PlayerMessageLevel where
  toJSON | .error => .string "error" | .warning => .string "warning"
         | .info => .string "info" | .debug => .string "debug"

/-- Keep in sync with `MediaLogMessageLevel`. The `error` level is kept separate
    from `PlayerError` because right now they represent different things: this
    one is a `DVLOG(ERROR)`-style log message printed based on the UI's
    selected log level, while `PlayerError` represents a `media::PipelineStatus`
    object. -/
structure PlayerMessage where
  level : PlayerMessageLevel
  message : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayerMessage where
  parseJSON v := do
    .ok
      { level := ← Value.getField v "level" >>= FromJSON.parseJSON
        message := ← Value.getField v "message" >>= FromJSON.parseJSON }

instance : ToJSON PlayerMessage where
  toJSON p := Data.Json.object [("level", ToJSON.toJSON p.level), ("message", ToJSON.toJSON p.message)]

/-- Corresponds to `kMediaPropertyChange`. -/
structure PlayerProperty where
  name : String
  value : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayerProperty where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON PlayerProperty where
  toJSON p := Data.Json.object [("name", ToJSON.toJSON p.name), ("value", ToJSON.toJSON p.value)]

/-- Corresponds to `kMediaEventTriggered`. -/
structure PlayerEvent where
  timestamp : Timestamp
  value : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayerEvent where
  parseJSON v := do
    .ok
      { timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON PlayerEvent where
  toJSON p := Data.Json.object [("timestamp", ToJSON.toJSON p.timestamp), ("value", ToJSON.toJSON p.value)]

/-- A logged source line number reported in an error. Note: `file`/`line` are
    from the Chromium C++ implementation code, not JS. -/
structure PlayerErrorSourceLocation where
  file : String
  line : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayerErrorSourceLocation where
  parseJSON v := do
    .ok
      { file := ← Value.getField v "file" >>= FromJSON.parseJSON
        line := ← Value.getField v "line" >>= FromJSON.parseJSON }

instance : ToJSON PlayerErrorSourceLocation where
  toJSON p := Data.Json.object [("file", ToJSON.toJSON p.file), ("line", ToJSON.toJSON p.line)]

/-- Corresponds to `kMediaError`. Self-referential via `cause` (an error's
    possible root-cause errors) — Lean accepts the *type* directly (recursion
    through `List` is positive), but `FromJSON`/`ToJSON` need a real
    termination proof for the same reason `Data.Json.Decode`'s own
    mutually-recursive parser does; see `parsePlayerError`/`encodePlayerError`
    below and `Data.Json.Value.getField_sizeOf_lt`. -/
structure PlayerError where
  errorType : String
  /-- The numeric enum entry for a specific set of error codes, such as
      `PipelineStatusCodes` in `media/base/pipeline_status.h`. -/
  code : Int
  /-- A trace of where this error was caused / where it passed through. -/
  stack : List PlayerErrorSourceLocation
  /-- Errors potentially have a root cause error, e.g. a `DecoderError` might be
      caused by a `WindowsError`. -/
  cause : List PlayerError
  /-- Extra data attached to an error, such as an HRESULT, video codec, etc. -/
  data : List (String × String)
  deriving Repr, BEq

set_option linter.unusedVariables false in
mutual

/-- Decode a `PlayerError`. A plain recursive `def` — rather than `cause`
    going through the generic `FromJSON (List α)` instance — to sidestep the
    circular instance dependency a self-referential `instance : FromJSON
    PlayerError` would otherwise have on itself. Terminates on `sizeOf`, via
    `Value.getField_sizeOf_lt`. -/
def parsePlayerError (v : Value) : Except String PlayerError :=
  match h : Value.getField v "cause" with
  | .error e => .error e
  | .ok causeV =>
    match parsePlayerErrorList causeV with
    | .error e => .error e
    | .ok cause =>
      (do
        let errorType ← Value.getField v "errorType" >>= FromJSON.parseJSON
        let code ← Value.getField v "code" >>= FromJSON.parseJSON
        let stack ← Value.getField v "stack" >>= FromJSON.parseJSON
        let data ← Value.getField v "data" >>= FromJSON.parseJSON
        pure { errorType, code, stack, cause, data })
termination_by sizeOf v
decreasing_by exact Value.getField_sizeOf_lt h

private def parsePlayerErrorList (v : Value) : Except String (List PlayerError) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => parsePlayerError p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega

end

instance : FromJSON PlayerError where parseJSON := parsePlayerError

mutual

/-- Encode a `PlayerError`. A plain recursive `def`, for the same reason
    `parsePlayerError` is: sidesteps the circular instance dependency a
    self-referential `instance : ToJSON PlayerError` would have on itself
    through the generic `ToJSON (List α)` instance. Terminates structurally on
    `PlayerError.cause`'s own `sizeOf` (an ordinary Lean value, not JSON to be
    decoded, so no `Value.getField`-style lemma is needed here). -/
def encodePlayerError (p : PlayerError) : Value :=
  Data.Json.object
    [ ("errorType", ToJSON.toJSON p.errorType), ("code", ToJSON.toJSON p.code)
    , ("stack", ToJSON.toJSON p.stack), ("cause", encodePlayerErrorList p.cause)
    , ("data", ToJSON.toJSON p.data) ]
termination_by sizeOf p
decreasing_by
  cases p with
  | mk errorType code stack cause data =>
    simp only [PlayerError.mk.sizeOf_spec]
    omega

private def encodePlayerErrorList (l : List PlayerError) : Value :=
  Value.array (l.map encodePlayerError).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega

end

instance : ToJSON PlayerError where toJSON := encodePlayerError

/-- The `Media.playerPropertiesChanged` event. -/
structure PlayerPropertiesChanged where
  playerId : PlayerId
  properties : List PlayerProperty
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayerPropertiesChanged where
  parseJSON v := do
    .ok
      { playerId := ← Value.getField v "playerId" >>= FromJSON.parseJSON
        properties := ← Value.getField v "properties" >>= FromJSON.parseJSON }

instance : Event PlayerPropertiesChanged where
  eventName := "Media.playerPropertiesChanged"

/-- The `Media.playerEventsAdded` event. -/
structure PlayerEventsAdded where
  playerId : PlayerId
  events : List PlayerEvent
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayerEventsAdded where
  parseJSON v := do
    .ok
      { playerId := ← Value.getField v "playerId" >>= FromJSON.parseJSON
        events := ← Value.getField v "events" >>= FromJSON.parseJSON }

instance : Event PlayerEventsAdded where
  eventName := "Media.playerEventsAdded"

/-- The `Media.playerMessagesLogged` event. -/
structure PlayerMessagesLogged where
  playerId : PlayerId
  messages : List PlayerMessage
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayerMessagesLogged where
  parseJSON v := do
    .ok
      { playerId := ← Value.getField v "playerId" >>= FromJSON.parseJSON
        messages := ← Value.getField v "messages" >>= FromJSON.parseJSON }

instance : Event PlayerMessagesLogged where
  eventName := "Media.playerMessagesLogged"

/-- The `Media.playerErrorsRaised` event. -/
structure PlayerErrorsRaised where
  playerId : PlayerId
  errors : List PlayerError
  deriving Repr, BEq

instance : FromJSON PlayerErrorsRaised where
  parseJSON v := do
    .ok
      { playerId := ← Value.getField v "playerId" >>= FromJSON.parseJSON
        errors := ← Value.getField v "errors" >>= FromJSON.parseJSON }

instance : Event PlayerErrorsRaised where
  eventName := "Media.playerErrorsRaised"

/-- The `Media.playersCreated` event. -/
structure PlayersCreated where
  players : List PlayerId
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlayersCreated where
  parseJSON v := do .ok { players := ← Value.getField v "players" >>= FromJSON.parseJSON }

instance : Event PlayersCreated where
  eventName := "Media.playersCreated"

/-- Parameters of the `Media.enable` command: enables the Media domain. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Media.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Media.disable` command: disables the Media domain. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Media.disable"
  decodeResponse _ := .ok ()

end CDP.Domains.Media
