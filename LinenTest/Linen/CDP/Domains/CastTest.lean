/-
  Tests for `Linen.CDP.Domains.Cast`.
-/
import Linen.CDP.Domains.Cast

open CDP.Domains.Cast
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Cast

/-! ### Sink -/

#guard decodeAs "{\"name\": \"n\", \"id\": \"1\"}" (α := Sink) = .ok { name := "n", id := "1", session := none }
#guard decodeAs "{\"name\": \"n\", \"id\": \"1\", \"session\": \"s\"}" (α := Sink)
  = .ok { name := "n", id := "1", session := some "s" }
#guard encode (ToJSON.toJSON ({ name := "n", id := "1" } : Sink)) = "{\"name\":\"n\",\"id\":\"1\"}"

/-! ### Events -/

#guard Event.eventName (α := SinksUpdated) = "Cast.sinksUpdated"
#guard Event.eventName (α := IssueUpdated) = "Cast.issueUpdated"
#guard decodeAs "{\"sinks\": []}" (α := SinksUpdated) = .ok { sinks := [] }
#guard decodeAs "{\"issueMessage\": \"oops\"}" (α := IssueUpdated) = .ok { issueMessage := "oops" }

/-! ### PEnable — optional field, present vs. absent -/

#guard encode (ToJSON.toJSON ({} : PEnable)) = "{}"
#guard encode (ToJSON.toJSON ({ presentationUrl := some "http://x" } : PEnable))
  = "{\"presentationUrl\":\"http:\\/\\/x\"}"
#guard Command.commandName ({} : PEnable) = "Cast.enable"

/-! ### PDisable — the whole params value is JSON `null`, not `{}`, matching
    upstream's `toJSON _ = A.Null` (so `CDP.Runtime`'s command envelope omits the
    `params` key entirely for it). -/

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Cast.disable"

/-! ### Sink-name commands -/

#guard Command.commandName ({ sinkName := "s" } : PSetSinkToUse) = "Cast.setSinkToUse"
#guard Command.commandName ({ sinkName := "s" } : PStartDesktopMirroring) = "Cast.startDesktopMirroring"
#guard Command.commandName ({ sinkName := "s" } : PStartTabMirroring) = "Cast.startTabMirroring"
#guard Command.commandName ({ sinkName := "s" } : PStopCasting) = "Cast.stopCasting"
#guard encode (ToJSON.toJSON ({ sinkName := "s" } : PStopCasting)) = "{\"sinkName\":\"s\"}"

end Tests.CDP.Domains.Cast
