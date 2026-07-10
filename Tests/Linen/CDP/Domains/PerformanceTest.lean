/-
  Tests for `Linen.CDP.Domains.Performance`.
-/
import Linen.CDP.Domains.Performance

open CDP.Domains.Performance
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Performance

#guard decodeAs "{\"name\": \"n\", \"value\": 1}" (α := Metric) = .ok { name := "n", value := 1 }
#guard Event.eventName (α := Metrics) = "Performance.metrics"
#guard decodeAs "{\"metrics\": [], \"title\": \"t\"}" (α := Metrics) = .ok { metrics := [], title := "t" }

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Performance.disable"

#guard decodeAs "\"timeTicks\"" (α := PEnable.TimeDomain) = .ok .timeTicks
#guard encode (ToJSON.toJSON PEnable.TimeDomain.threadTicks) = "\"threadTicks\""
#guard encode (ToJSON.toJSON ({} : PEnable)) = "{}"
#guard encode (ToJSON.toJSON ({ timeDomain := some .timeTicks } : PEnable)) = "{\"timeDomain\":\"timeTicks\"}"
#guard Command.commandName ({} : PEnable) = "Performance.enable"

#guard encode (ToJSON.toJSON ({} : PGetMetrics)) = "null"
#guard Command.commandName ({} : PGetMetrics) = "Performance.getMetrics"
#guard decodeAs "{\"metrics\": [{\"name\": \"n\", \"value\": 1}]}" (α := GetMetrics)
  = .ok { metrics := [{ name := "n", value := 1 }] }

end Tests.CDP.Domains.Performance
