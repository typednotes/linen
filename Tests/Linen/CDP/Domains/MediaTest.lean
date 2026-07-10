/-
  Tests for `Linen.CDP.Domains.Media`.
-/
import Linen.CDP.Domains.Media

open CDP.Domains.Media
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON Value)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Media

/-! ### PlayerMessageLevel / PlayerMessage -/

#guard decodeAs "\"warning\"" (α := PlayerMessageLevel) = .ok .warning
#guard encode (ToJSON.toJSON PlayerMessageLevel.debug) = "\"debug\""
#guard decodeAs "{\"level\": \"error\", \"message\": \"boom\"}" (α := PlayerMessage)
  = .ok { level := .error, message := "boom" }

/-! ### PlayerProperty / PlayerEvent -/

#guard decodeAs "{\"name\": \"n\", \"value\": \"v\"}" (α := PlayerProperty) = .ok { name := "n", value := "v" }
#guard decodeAs "{\"timestamp\": 1.5, \"value\": \"v\"}" (α := PlayerEvent) = .ok { timestamp := 1.5, value := "v" }

/-! ### PlayerErrorSourceLocation -/

#guard decodeAs "{\"file\": \"f.cc\", \"line\": 10}" (α := PlayerErrorSourceLocation)
  = .ok { file := "f.cc", line := 10 }

/-! ### PlayerError — `cause` is genuinely self-referential (`List PlayerError`);
    `PlayerError` has no `DecidableEq` (only `BEq`, since the auto-deriving
    handler can't see through the hand-proven recursive `FromJSON`/`ToJSON`),
    so equality checks here pattern-match or use `==` rather than `=`. -/

#guard match decodeAs "{\"errorType\": \"t\", \"code\": 1, \"stack\": [], \"cause\": [], \"data\": []}"
    (α := PlayerError) with
  | .ok e => e == ({ errorType := "t", code := 1, stack := [], cause := [], data := [] } : PlayerError)
  | .error _ => false

-- A nested `cause` chain decodes recursively, not just its outermost layer.
#guard match decodeAs
    ("{\"errorType\": \"outer\", \"code\": 1, \"stack\": [], \"data\": [], \"cause\": [" ++
     "{\"errorType\": \"inner\", \"code\": 2, \"stack\": [], \"cause\": [], \"data\": []}]}")
    (α := PlayerError) with
  | .ok e =>
    e.errorType == "outer" &&
    match e.cause with
    | [inner] => inner.errorType == "inner" && inner.code == 2 && inner.cause == []
    | _ => false
  | .error _ => false

#guard (decodeAs
    "{\"errorType\": \"t\", \"code\": 1, \"stack\": [], \"cause\": [], \"data\": [[\"k\", \"v\"]]}"
    (α := PlayerError)).map (fun e => e.data)
  = .ok [("k", "v")]

#guard encode (ToJSON.toJSON ({ errorType := "t", code := 1, stack := [], cause := [], data := [] } :
    PlayerError))
  = "{\"errorType\":\"t\",\"code\":1,\"stack\":[],\"cause\":[],\"data\":[]}"

-- Encode/decode round-trips through a nested `cause`.
def innerErr : PlayerError := { errorType := "inner", code := 2, stack := [], cause := [], data := [] }
def outerErr : PlayerError :=
  { errorType := "outer", code := 1, stack := [], cause := [innerErr], data := [] }

#guard match decodeAs (encode (ToJSON.toJSON outerErr)) (α := PlayerError) with
  | .ok e => e.errorType == "outer" && e.cause.length == 1
  | .error _ => false

/-! ### Events -/

#guard Event.eventName (α := PlayerPropertiesChanged) = "Media.playerPropertiesChanged"
#guard Event.eventName (α := PlayerEventsAdded) = "Media.playerEventsAdded"
#guard Event.eventName (α := PlayerMessagesLogged) = "Media.playerMessagesLogged"
#guard Event.eventName (α := PlayerErrorsRaised) = "Media.playerErrorsRaised"
#guard Event.eventName (α := PlayersCreated) = "Media.playersCreated"

#guard decodeAs "{\"playerId\": \"1\", \"properties\": []}" (α := PlayerPropertiesChanged)
  = .ok { playerId := "1", properties := [] }
#guard decodeAs "{\"players\": [\"1\", \"2\"]}" (α := PlayersCreated) = .ok { players := ["1", "2"] }

/-! ### Commands -/

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PEnable) = "Media.enable"
#guard Command.commandName ({} : PDisable) = "Media.disable"

end Tests.CDP.Domains.Media
