/-
  Tests for `Linen.CDP.Domains.Log`.
-/
import Linen.CDP.Domains.Log

open CDP.Domains.Log
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Log

#guard decodeAs "\"javascript\"" (α := LogEntrySource) = .ok .javascript
#guard encode (ToJSON.toJSON LogEntrySource.network) = "\"network\""

#guard decodeAs "\"warning\"" (α := LogEntryLevel) = .ok .warning
#guard encode (ToJSON.toJSON LogEntryLevel.error) = "\"error\""

#guard decodeAs "\"cors\"" (α := LogEntryCategory) = .ok .cors
#guard encode (ToJSON.toJSON LogEntryCategory.cors) = "\"cors\""

#guard match decodeAs
    "{\"source\": \"network\", \"level\": \"error\", \"text\": \"oops\", \"timestamp\": 1.0}"
    (α := LogEntry) with
  | .ok v => v == { source := .network, level := .error, text := "oops", timestamp := 1.0 }
  | .error _ => false
#guard encode
    (ToJSON.toJSON ({ source := .network, level := .error, text := "oops", timestamp := 1.0 } : LogEntry))
  = "{\"source\":\"network\",\"level\":\"error\",\"text\":\"oops\",\"timestamp\":1}"
#guard match decodeAs
    "{\"source\": \"other\", \"level\": \"info\", \"text\": \"t\", \"category\": \"cors\", \"timestamp\": 2.0, \"url\": \"http://x\", \"lineNumber\": 3, \"workerId\": \"w\"}"
    (α := LogEntry) with
  | .ok v =>
    v == { source := .other, level := .info, text := "t", category := some .cors, timestamp := 2.0, url := some "http://x", lineNumber := some 3, workerId := some "w" }
  | .error _ => false

#guard decodeAs "\"longTask\"" (α := ViolationSettingName) = .ok .longTask
#guard encode (ToJSON.toJSON ViolationSettingName.recurringHandler) = "\"recurringHandler\""

#guard decodeAs "{\"name\": \"handler\", \"threshold\": 100}" (α := ViolationSetting)
  = .ok { name := .handler, threshold := 100 }
#guard encode (ToJSON.toJSON ({ name := .handler, threshold := 100 } : ViolationSetting))
  = "{\"name\":\"handler\",\"threshold\":100}"

#guard match decodeAs
    "{\"entry\": {\"source\": \"network\", \"level\": \"error\", \"text\": \"oops\", \"timestamp\": 1.0}}"
    (α := EntryAdded) with
  | .ok v => v == { entry := { source := .network, level := .error, text := "oops", timestamp := 1.0 } }
  | .error _ => false
#guard Event.eventName (α := EntryAdded) = "Log.entryAdded"

#guard encode (ToJSON.toJSON ({} : PClear)) = "null"
#guard Command.commandName ({} : PClear) = "Log.clear"

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Log.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "Log.enable"

#guard encode
    (ToJSON.toJSON ({ config := [{ name := .longTask, threshold := 50 }] } : PStartViolationsReport))
  = "{\"config\":[{\"name\":\"longTask\",\"threshold\":50}]}"
#guard Command.commandName ({ config := [] } : PStartViolationsReport) = "Log.startViolationsReport"

#guard encode (ToJSON.toJSON ({} : PStopViolationsReport)) = "null"
#guard Command.commandName ({} : PStopViolationsReport) = "Log.stopViolationsReport"

end Tests.CDP.Domains.Log
