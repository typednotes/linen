/-
  Tests for `Linen.CDP.Domains.EventBreakpoints`.
-/
import Linen.CDP.Domains.EventBreakpoints

open CDP.Domains.EventBreakpoints
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.EventBreakpoints

#guard Command.commandName ({ eventName := "setTimeout" } : PSetInstrumentationBreakpoint)
  = "EventBreakpoints.setInstrumentationBreakpoint"
#guard encode (ToJSON.toJSON ({ eventName := "setTimeout" } : PSetInstrumentationBreakpoint))
  = "{\"eventName\":\"setTimeout\"}"
#guard Command.commandName ({ eventName := "setTimeout" } : PRemoveInstrumentationBreakpoint)
  = "EventBreakpoints.removeInstrumentationBreakpoint"

end Tests.CDP.Domains.EventBreakpoints
