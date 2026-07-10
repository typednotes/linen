/-
  Tests for `Linen.CDP.Domains.Tethering`.
-/
import Linen.CDP.Domains.Tethering

open CDP.Domains.Tethering
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Tethering

#guard decodeAs "{\"port\": 1234, \"connectionId\": \"abc\"}" (α := Accepted)
  = .ok { port := 1234, connectionId := "abc" }
#guard Event.eventName (α := Accepted) = "Tethering.accepted"

#guard encode (ToJSON.toJSON ({ port := 1234 } : PBind)) = "{\"port\":1234}"
#guard Command.commandName ({ port := 1234 } : PBind) = "Tethering.bind"

#guard encode (ToJSON.toJSON ({ port := 1234 } : PUnbind)) = "{\"port\":1234}"
#guard Command.commandName ({ port := 1234 } : PUnbind) = "Tethering.unbind"

end Tests.CDP.Domains.Tethering
