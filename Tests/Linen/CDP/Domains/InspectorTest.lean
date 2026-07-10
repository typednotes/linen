/-
  Tests for `Linen.CDP.Domains.Inspector`.
-/
import Linen.CDP.Domains.Inspector

open CDP.Domains.Inspector
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Inspector

#guard Event.eventName (α := Detached) = "Inspector.detached"
#guard decodeAs "{\"reason\": \"replaced\"}" (α := Detached) = .ok { reason := "replaced" }

#guard Event.eventName (α := TargetCrashed) = "Inspector.targetCrashed"
#guard decodeAs "{}" (α := TargetCrashed) = .ok {}
#guard decodeAs "null" (α := TargetCrashed) = .ok {}  -- ignores its (nonexistent) payload entirely

#guard Event.eventName (α := TargetReloadedAfterCrash) = "Inspector.targetReloadedAfterCrash"
#guard decodeAs "{}" (α := TargetReloadedAfterCrash) = .ok {}

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PDisable) = "Inspector.disable"
#guard Command.commandName ({} : PEnable) = "Inspector.enable"

end Tests.CDP.Domains.Inspector
