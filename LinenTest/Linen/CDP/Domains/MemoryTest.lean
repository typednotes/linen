/-
  Tests for `Linen.CDP.Domains.Memory`.
-/
import Linen.CDP.Domains.Memory

open CDP.Domains.Memory
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Memory

#guard decodeAs "\"critical\"" (α := PressureLevel) = .ok .critical
#guard encode (ToJSON.toJSON PressureLevel.moderate) = "\"moderate\""

#guard decodeAs "{\"size\": 1, \"total\": 2, \"stack\": [\"a\"]}" (α := SamplingProfileNode)
  = .ok { size := 1, total := 2, stack := ["a"] }
#guard decodeAs "{\"name\": \"n\", \"uuid\": \"u\", \"baseAddress\": \"0x1\", \"size\": 10}" (α := Module)
  = .ok { name := "n", uuid := "u", baseAddress := "0x1", size := 10 }
#guard decodeAs "{\"samples\": [], \"modules\": []}" (α := SamplingProfile) = .ok { samples := [], modules := [] }

#guard encode (ToJSON.toJSON ({} : PGetDOMCounters)) = "null"
#guard decodeAs "{\"documents\": 1, \"nodes\": 2, \"jsEventListeners\": 3}" (α := GetDOMCounters)
  = .ok { documents := 1, nodes := 2, jsEventListeners := 3 }
#guard Command.commandName ({} : PGetDOMCounters) = "Memory.getDOMCounters"

#guard encode (ToJSON.toJSON ({} : PPrepareForLeakDetection)) = "null"
#guard encode (ToJSON.toJSON ({} : PForciblyPurgeJavaScriptMemory)) = "null"
#guard Command.commandName ({} : PPrepareForLeakDetection) = "Memory.prepareForLeakDetection"
#guard Command.commandName ({} : PForciblyPurgeJavaScriptMemory) = "Memory.forciblyPurgeJavaScriptMemory"

#guard encode (ToJSON.toJSON ({ suppressed := true } : PSetPressureNotificationsSuppressed))
  = "{\"suppressed\":true}"
#guard Command.commandName ({ suppressed := true } : PSetPressureNotificationsSuppressed)
  = "Memory.setPressureNotificationsSuppressed"

#guard encode (ToJSON.toJSON ({ level := .critical } : PSimulatePressureNotification))
  = "{\"level\":\"critical\"}"
#guard Command.commandName ({ level := .critical } : PSimulatePressureNotification)
  = "Memory.simulatePressureNotification"

#guard encode (ToJSON.toJSON ({} : PStartSampling)) = "{}"
#guard encode (ToJSON.toJSON ({ samplingInterval := some 1024 } : PStartSampling))
  = "{\"samplingInterval\":1024}"
#guard Command.commandName ({} : PStartSampling) = "Memory.startSampling"
#guard encode (ToJSON.toJSON ({} : PStopSampling)) = "null"
#guard Command.commandName ({} : PStopSampling) = "Memory.stopSampling"

#guard Command.commandName ({} : PGetAllTimeSamplingProfile) = "Memory.getAllTimeSamplingProfile"
#guard Command.commandName ({} : PGetBrowserSamplingProfile) = "Memory.getBrowserSamplingProfile"
#guard Command.commandName ({} : PGetSamplingProfile) = "Memory.getSamplingProfile"
#guard decodeAs "{\"profile\": {\"samples\": [], \"modules\": []}}" (α := GetSamplingProfile)
  = .ok { profile := { samples := [], modules := [] } }

end Tests.CDP.Domains.Memory
