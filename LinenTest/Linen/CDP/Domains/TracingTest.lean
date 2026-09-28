/-
  Tests for `Linen.CDP.Domains.Tracing`.
-/
import Linen.CDP.Domains.Tracing

open CDP.Domains.Tracing
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Tracing

-- ── Types ──

#guard decodeAs "[[\"k\", \"v\"]]" (α := MemoryDumpConfig) = .ok [("k", "v")]

#guard decodeAs "\"recordContinuously\"" (α := TraceConfigRecordMode) = .ok .recordContinuously
#guard encode (ToJSON.toJSON TraceConfigRecordMode.echoToConsole) = "\"echoToConsole\""

#guard decodeAs "{}" (α := TraceConfig) = .ok {}
#guard decodeAs
    "{\"recordMode\": \"recordUntilFull\", \"traceBufferSizeInKb\": 1024, \"enableSampling\": true, \
    \"enableSystrace\": false, \"enableArgumentFilter\": true, \"includedCategories\": [\"a\"], \
    \"excludedCategories\": [\"b\"], \"syntheticDelays\": [\"c\"], \"memoryDumpConfig\": [[\"k\", \"v\"]]}"
    (α := TraceConfig)
  = .ok
    { recordMode := some .recordUntilFull, traceBufferSizeInKb := some 1024, enableSampling := some true
      enableSystrace := some false, enableArgumentFilter := some true, includedCategories := some ["a"]
      excludedCategories := some ["b"], syntheticDelays := some ["c"], memoryDumpConfig := some [("k", "v")] }
#guard encode (ToJSON.toJSON ({} : TraceConfig)) = "{}"
#guard encode (ToJSON.toJSON ({ recordMode := some .echoToConsole } : TraceConfig))
  = "{\"recordMode\":\"echoToConsole\"}"

#guard decodeAs "\"json\"" (α := StreamFormat) = .ok .json
#guard encode (ToJSON.toJSON StreamFormat.proto) = "\"proto\""

#guard decodeAs "\"gzip\"" (α := StreamCompression) = .ok .gzip
#guard encode (ToJSON.toJSON StreamCompression.none) = "\"none\""

#guard decodeAs "\"light\"" (α := MemoryDumpLevelOfDetail) = .ok .light
#guard encode (ToJSON.toJSON MemoryDumpLevelOfDetail.detailed) = "\"detailed\""

#guard decodeAs "\"system\"" (α := TracingBackend) = .ok .system
#guard encode (ToJSON.toJSON TracingBackend.auto) = "\"auto\""

-- ── Events ──

#guard decodeAs "{}" (α := BufferUsage) = .ok {}
#guard decodeAs "{\"percentFull\": 0.5, \"eventCount\": 10, \"value\": 0.5}" (α := BufferUsage)
  = .ok { percentFull := some 0.5, eventCount := some 10, value := some 0.5 }
#guard Event.eventName (α := BufferUsage) = "Tracing.bufferUsage"

#guard decodeAs "{\"value\": [[[\"k\", \"v\"]]]}" (α := DataCollected) = .ok { value := [[("k", "v")]] }
#guard Event.eventName (α := DataCollected) = "Tracing.dataCollected"

#guard decodeAs "{\"dataLossOccurred\": true}" (α := TracingComplete)
  = .ok { dataLossOccurred := true, stream := none, traceFormat := none, streamCompression := none }
#guard decodeAs
    "{\"dataLossOccurred\": false, \"stream\": \"blob:abc\", \"traceFormat\": \"json\", \
    \"streamCompression\": \"gzip\"}"
    (α := TracingComplete)
  = .ok
    { dataLossOccurred := false, stream := some "blob:abc", traceFormat := some .json
      streamCompression := some .gzip }
#guard Event.eventName (α := TracingComplete) = "Tracing.tracingComplete"

-- ── Commands ──

#guard encode (ToJSON.toJSON ({} : PEnd)) = "null"
#guard Command.commandName ({} : PEnd) = "Tracing.end"

#guard encode (ToJSON.toJSON ({} : PGetCategories)) = "null"
#guard Command.commandName ({} : PGetCategories) = "Tracing.getCategories"
#guard decodeAs "{\"categories\": [\"a\", \"b\"]}" (α := GetCategories) = .ok { categories := ["a", "b"] }

#guard encode (ToJSON.toJSON ({ syncId := "sync-1" } : PRecordClockSyncMarker)) = "{\"syncId\":\"sync-1\"}"
#guard Command.commandName ({ syncId := "sync-1" } : PRecordClockSyncMarker) = "Tracing.recordClockSyncMarker"

#guard encode (ToJSON.toJSON ({} : PRequestMemoryDump)) = "{}"
#guard encode (ToJSON.toJSON ({ deterministic := some true, levelOfDetail := some .light } : PRequestMemoryDump))
  = "{\"deterministic\":true,\"levelOfDetail\":\"light\"}"
#guard Command.commandName ({} : PRequestMemoryDump) = "Tracing.requestMemoryDump"
#guard decodeAs "{\"dumpGuid\": \"g-1\", \"success\": true}" (α := RequestMemoryDump)
  = .ok { dumpGuid := "g-1", success := true }

#guard decodeAs "\"ReportEvents\"" (α := StartTransferMode) = .ok .reportEvents
#guard encode (ToJSON.toJSON StartTransferMode.returnAsStream) = "\"ReturnAsStream\""

#guard encode (ToJSON.toJSON ({} : PStart)) = "{}"
#guard encode (ToJSON.toJSON
    ({ bufferUsageReportingInterval := some 1000, transferMode := some .returnAsStream
       streamFormat := some .proto, streamCompression := some .gzip } : PStart))
  = "{\"bufferUsageReportingInterval\":1000,\"transferMode\":\"ReturnAsStream\",\"streamFormat\":\"proto\"," ++
    "\"streamCompression\":\"gzip\"}"
#guard Command.commandName ({} : PStart) = "Tracing.start"

end Tests.CDP.Domains.Tracing
