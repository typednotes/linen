/-
  Tests for `Linen.CDP.Domains.Profiler`.
-/
import Linen.CDP.Domains.Profiler

open CDP.Domains.Profiler
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Profiler

#guard decodeAs "{\"line\": 1, \"ticks\": 2}" (α := PositionTickInfo) = .ok { line := 1, ticks := 2 }
#guard encode (ToJSON.toJSON ({ line := 1, ticks := 2 } : PositionTickInfo)) = "{\"line\":1,\"ticks\":2}"

#guard (decodeAs
    "{\"id\": 1, \"callFrame\": {\"functionName\": \"f\", \"scriptId\": \"1\", \"url\": \"u\", \"lineNumber\": 0, \"columnNumber\": 0}}"
    (α := ProfileNode)
  |>.map fun p => p ==
      ({ id := 1
         callFrame := { functionName := "f", scriptId := "1", url := "u", lineNumber := 0, columnNumber := 0 } }
        : ProfileNode))
  = .ok true
#guard encode
    (ToJSON.toJSON
      ({ id := 1
         callFrame :=
           { functionName := "f", scriptId := "1", url := "u", lineNumber := 0, columnNumber := 0 } }
        : ProfileNode))
  = "{\"id\":1,\"callFrame\":{\"functionName\":\"f\",\"scriptId\":\"1\",\"url\":\"u\",\"lineNumber\":0,\"columnNumber\":0}}"

#guard (decodeAs "{\"nodes\": [], \"startTime\": 0, \"endTime\": 1}" (α := Profile)
  |>.map fun p => p == { nodes := [], startTime := 0, endTime := 1 }) = .ok true
#guard encode (ToJSON.toJSON ({ nodes := [], startTime := 0, endTime := 1 } : Profile))
  = "{\"nodes\":[],\"startTime\":0,\"endTime\":1}"

#guard decodeAs "{\"startOffset\": 0, \"endOffset\": 1, \"count\": 2}" (α := CoverageRange)
  = .ok { startOffset := 0, endOffset := 1, count := 2 }
#guard encode (ToJSON.toJSON ({ startOffset := 0, endOffset := 1, count := 2 } : CoverageRange))
  = "{\"startOffset\":0,\"endOffset\":1,\"count\":2}"

#guard decodeAs "{\"functionName\": \"f\", \"ranges\": [], \"isBlockCoverage\": true}" (α := FunctionCoverage)
  = .ok { functionName := "f", ranges := [], isBlockCoverage := true }
#guard encode
    (ToJSON.toJSON ({ functionName := "f", ranges := [], isBlockCoverage := true } : FunctionCoverage))
  = "{\"functionName\":\"f\",\"ranges\":[],\"isBlockCoverage\":true}"

#guard decodeAs "{\"scriptId\": \"1\", \"url\": \"u\", \"functions\": []}" (α := ScriptCoverage)
  = .ok { scriptId := "1", url := "u", functions := [] }
#guard encode (ToJSON.toJSON ({ scriptId := "1", url := "u", functions := [] } : ScriptCoverage))
  = "{\"scriptId\":\"1\",\"url\":\"u\",\"functions\":[]}"

#guard (decodeAs
    "{\"id\": \"1\", \"location\": {\"scriptId\": \"1\", \"lineNumber\": 0}, \"profile\": {\"nodes\": [], \"startTime\": 0, \"endTime\": 1}}"
    (α := ConsoleProfileFinished)
  |>.map fun p => p ==
      ({ id := "1"
         location := { scriptId := "1", lineNumber := 0 }
         profile := { nodes := [], startTime := 0, endTime := 1 } }
        : ConsoleProfileFinished)) = .ok true
#guard Event.eventName (α := ConsoleProfileFinished) = "Profiler.consoleProfileFinished"

#guard decodeAs "{\"id\": \"1\", \"location\": {\"scriptId\": \"1\", \"lineNumber\": 0}}" (α := ConsoleProfileStarted)
  = .ok { id := "1", location := { scriptId := "1", lineNumber := 0 } }
#guard Event.eventName (α := ConsoleProfileStarted) = "Profiler.consoleProfileStarted"

#guard (decodeAs "{\"timestamp\": 1, \"occasion\": \"o\", \"result\": []}" (α := PreciseCoverageDeltaUpdate)
  |>.map fun p => p == { timestamp := 1, occasion := "o", result := [] }) = .ok true
#guard Event.eventName (α := PreciseCoverageDeltaUpdate) = "Profiler.preciseCoverageDeltaUpdate"

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Profiler.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "Profiler.enable"

#guard encode (ToJSON.toJSON ({} : PGetBestEffortCoverage)) = "null"
#guard Command.commandName ({} : PGetBestEffortCoverage) = "Profiler.getBestEffortCoverage"
#guard decodeAs "{\"result\": []}" (α := GetBestEffortCoverage) = .ok { result := [] }

#guard encode (ToJSON.toJSON ({ interval := 100 } : PSetSamplingInterval)) = "{\"interval\":100}"
#guard Command.commandName ({ interval := 100 } : PSetSamplingInterval) = "Profiler.setSamplingInterval"

#guard encode (ToJSON.toJSON ({} : PStart)) = "null"
#guard Command.commandName ({} : PStart) = "Profiler.start"

#guard encode (ToJSON.toJSON ({} : PStartPreciseCoverage)) = "{}"
#guard encode (ToJSON.toJSON ({ callCount := some true } : PStartPreciseCoverage))
  = "{\"callCount\":true}"
#guard Command.commandName ({} : PStartPreciseCoverage) = "Profiler.startPreciseCoverage"
#guard decodeAs "{\"timestamp\": 1}" (α := StartPreciseCoverage) = .ok { timestamp := 1 }

#guard encode (ToJSON.toJSON ({} : PStop)) = "null"
#guard Command.commandName ({} : PStop) = "Profiler.stop"
#guard (decodeAs "{\"profile\": {\"nodes\": [], \"startTime\": 0, \"endTime\": 1}}" (α := Stop)
  |>.map fun p => p == { profile := { nodes := [], startTime := 0, endTime := 1 } }) = .ok true

#guard encode (ToJSON.toJSON ({} : PStopPreciseCoverage)) = "null"
#guard Command.commandName ({} : PStopPreciseCoverage) = "Profiler.stopPreciseCoverage"

#guard encode (ToJSON.toJSON ({} : PTakePreciseCoverage)) = "null"
#guard Command.commandName ({} : PTakePreciseCoverage) = "Profiler.takePreciseCoverage"
#guard (decodeAs "{\"result\": [], \"timestamp\": 1}" (α := TakePreciseCoverage)
  |>.map fun p => p == { result := [], timestamp := 1 }) = .ok true

end Tests.CDP.Domains.Profiler
