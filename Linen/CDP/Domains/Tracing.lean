/-
  Linen.CDP.Domains.Tracing — the `Tracing` CDP domain

  Ports `CDP.Domains.Tracing` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.Memory`'s docstring. Cross-domain
  references to the `IO` domain follow `CDP.Domains.IO`'s docstring: `open
  CDP.Domains` and refer to `IO.StreamHandle` unambiguously.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.IO

namespace CDP.Domains.Tracing

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)
open CDP.Domains

-- ── Types ──

/-- Configuration for a memory dump. Used only when the `memory-infra`
    category is enabled. -/
abbrev MemoryDumpConfig := List (String × String)

/-- Controls how the trace buffer stores data. -/
inductive TraceConfigRecordMode where
  | recordUntilFull | recordContinuously | recordAsMuchAsPossible | echoToConsole
  deriving Repr, BEq, DecidableEq

instance : FromJSON TraceConfigRecordMode where
  parseJSON
    | .string "recordUntilFull" => .ok .recordUntilFull
    | .string "recordContinuously" => .ok .recordContinuously
    | .string "recordAsMuchAsPossible" => .ok .recordAsMuchAsPossible
    | .string "echoToConsole" => .ok .echoToConsole
    | v => .error s!"failed to parse TraceConfigRecordMode: {repr v}"

instance : ToJSON TraceConfigRecordMode where
  toJSON
    | .recordUntilFull => .string "recordUntilFull"
    | .recordContinuously => .string "recordContinuously"
    | .recordAsMuchAsPossible => .string "recordAsMuchAsPossible"
    | .echoToConsole => .string "echoToConsole"

/-- Trace configuration. -/
structure TraceConfig where
  /-- Controls how the trace buffer stores data. -/
  recordMode : Option TraceConfigRecordMode := none
  /-- Size of the trace buffer in kilobytes. If not specified or zero is
      passed, a default value of 200 MB is used. -/
  traceBufferSizeInKb : Option Float := none
  /-- Turns on JavaScript stack sampling. -/
  enableSampling : Option Bool := none
  /-- Turns on system tracing. -/
  enableSystrace : Option Bool := none
  /-- Turns on argument filter. -/
  enableArgumentFilter : Option Bool := none
  /-- Included category filters. -/
  includedCategories : Option (List String) := none
  /-- Excluded category filters. -/
  excludedCategories : Option (List String) := none
  /-- Configuration to synthesize the delays in tracing. -/
  syntheticDelays : Option (List String) := none
  /-- Configuration for memory dump triggers. Used only when the
      `memory-infra` category is enabled. -/
  memoryDumpConfig : Option MemoryDumpConfig := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON TraceConfig where
  parseJSON v := do
    .ok
      { recordMode := ← (← Value.getFieldOpt v "recordMode").mapM FromJSON.parseJSON
        traceBufferSizeInKb := ← (← Value.getFieldOpt v "traceBufferSizeInKb").mapM FromJSON.parseJSON
        enableSampling := ← (← Value.getFieldOpt v "enableSampling").mapM FromJSON.parseJSON
        enableSystrace := ← (← Value.getFieldOpt v "enableSystrace").mapM FromJSON.parseJSON
        enableArgumentFilter := ← (← Value.getFieldOpt v "enableArgumentFilter").mapM FromJSON.parseJSON
        includedCategories := ← (← Value.getFieldOpt v "includedCategories").mapM FromJSON.parseJSON
        excludedCategories := ← (← Value.getFieldOpt v "excludedCategories").mapM FromJSON.parseJSON
        syntheticDelays := ← (← Value.getFieldOpt v "syntheticDelays").mapM FromJSON.parseJSON
        memoryDumpConfig := ← (← Value.getFieldOpt v "memoryDumpConfig").mapM FromJSON.parseJSON }

instance : ToJSON TraceConfig where
  toJSON p := Data.Json.object <|
    (p.recordMode.map fun v => ("recordMode", ToJSON.toJSON v)).toList
    ++ (p.traceBufferSizeInKb.map fun v => ("traceBufferSizeInKb", ToJSON.toJSON v)).toList
    ++ (p.enableSampling.map fun v => ("enableSampling", ToJSON.toJSON v)).toList
    ++ (p.enableSystrace.map fun v => ("enableSystrace", ToJSON.toJSON v)).toList
    ++ (p.enableArgumentFilter.map fun v => ("enableArgumentFilter", ToJSON.toJSON v)).toList
    ++ (p.includedCategories.map fun v => ("includedCategories", ToJSON.toJSON v)).toList
    ++ (p.excludedCategories.map fun v => ("excludedCategories", ToJSON.toJSON v)).toList
    ++ (p.syntheticDelays.map fun v => ("syntheticDelays", ToJSON.toJSON v)).toList
    ++ (p.memoryDumpConfig.map fun v => ("memoryDumpConfig", ToJSON.toJSON v)).toList

/-- Data format of a trace. Can be either the legacy JSON format or the
    protocol buffer format. The JSON format is expected to be deprecated. -/
inductive StreamFormat where
  | json | proto
  deriving Repr, BEq, DecidableEq

instance : FromJSON StreamFormat where
  parseJSON
    | .string "json" => .ok .json
    | .string "proto" => .ok .proto
    | v => .error s!"failed to parse StreamFormat: {repr v}"

instance : ToJSON StreamFormat where
  toJSON | .json => .string "json" | .proto => .string "proto"

/-- Compression type to use for traces returned via streams. -/
inductive StreamCompression where
  | none | gzip
  deriving Repr, BEq, DecidableEq

instance : FromJSON StreamCompression where
  parseJSON
    | .string "none" => .ok .none
    | .string "gzip" => .ok .gzip
    | v => .error s!"failed to parse StreamCompression: {repr v}"

instance : ToJSON StreamCompression where
  toJSON | .none => .string "none" | .gzip => .string "gzip"

/-- Details exposed when a memory request is explicitly declared. Keep
    consistent with `memory_dump_request_args.h` and
    `memory_instrumentation.mojom`. -/
inductive MemoryDumpLevelOfDetail where
  | background | light | detailed
  deriving Repr, BEq, DecidableEq

instance : FromJSON MemoryDumpLevelOfDetail where
  parseJSON
    | .string "background" => .ok .background
    | .string "light" => .ok .light
    | .string "detailed" => .ok .detailed
    | v => .error s!"failed to parse MemoryDumpLevelOfDetail: {repr v}"

instance : ToJSON MemoryDumpLevelOfDetail where
  toJSON
    | .background => .string "background"
    | .light => .string "light"
    | .detailed => .string "detailed"

/-- Backend type to use for tracing. `chrome` uses the Chrome-integrated
    tracing service and is supported on all platforms. `system` is only
    supported on Chrome OS and uses the Perfetto system tracing service.
    `auto` chooses `system` when the `perfettoConfig` provided to
    `Tracing.start` specifies at least one non-Chrome data source; otherwise
    it uses `chrome`. -/
inductive TracingBackend where
  | auto | chrome | system
  deriving Repr, BEq, DecidableEq

instance : FromJSON TracingBackend where
  parseJSON
    | .string "auto" => .ok .auto
    | .string "chrome" => .ok .chrome
    | .string "system" => .ok .system
    | v => .error s!"failed to parse TracingBackend: {repr v}"

instance : ToJSON TracingBackend where
  toJSON | .auto => .string "auto" | .chrome => .string "chrome" | .system => .string "system"

-- ── Events ──

/-- The `Tracing.bufferUsage` event. -/
structure BufferUsage where
  /-- A number in range `[0..1]` that indicates the used size of the event
      buffer as a fraction of its total size. -/
  percentFull : Option Float := none
  /-- An approximate number of events in the trace log. -/
  eventCount : Option Float := none
  /-- A number in range `[0..1]` that indicates the used size of the event
      buffer as a fraction of its total size. -/
  value : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON BufferUsage where
  parseJSON v := do
    .ok
      { percentFull := ← (← Value.getFieldOpt v "percentFull").mapM FromJSON.parseJSON
        eventCount := ← (← Value.getFieldOpt v "eventCount").mapM FromJSON.parseJSON
        value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON }

instance : Event BufferUsage where
  eventName := "Tracing.bufferUsage"

/-- The `Tracing.dataCollected` event. Contains a bucket of collected trace
    events. When tracing has finished, a last event is sent with `value`
    empty. -/
structure DataCollected where
  value : List MemoryDumpConfig
  deriving Repr, BEq, DecidableEq

instance : FromJSON DataCollected where
  parseJSON v := do .ok { value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : Event DataCollected where
  eventName := "Tracing.dataCollected"

/-- The `Tracing.tracingComplete` event. Signals that tracing is stopped and
    there is no trace buffers pending flush, all data were delivered via
    `dataCollected` events. -/
structure TracingComplete where
  /-- Indicates whether some trace data is known to have been lost, e.g.
      because the trace ring buffer wrapped around. -/
  dataLossOccurred : Bool
  /-- A handle of the stream that holds the resulting trace data. -/
  stream : Option IO.StreamHandle := none
  /-- Trace data format of the returned stream. -/
  traceFormat : Option StreamFormat := none
  /-- Compression format of the returned stream. -/
  streamCompression : Option StreamCompression := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON TracingComplete where
  parseJSON v := do
    .ok
      { dataLossOccurred := ← Value.getField v "dataLossOccurred" >>= FromJSON.parseJSON
        stream := ← (← Value.getFieldOpt v "stream").mapM FromJSON.parseJSON
        traceFormat := ← (← Value.getFieldOpt v "traceFormat").mapM FromJSON.parseJSON
        streamCompression := ← (← Value.getFieldOpt v "streamCompression").mapM FromJSON.parseJSON }

instance : Event TracingComplete where
  eventName := "Tracing.tracingComplete"

-- ── Commands ──

/-- Parameters of the `Tracing.end` command: stops trace events collection. -/
structure PEnd where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnd where toJSON _ := .null

instance : Command PEnd where
  Response := Unit
  commandName _ := "Tracing.end"
  decodeResponse _ := .ok ()

/-- Parameters of the `Tracing.getCategories` command: gets the supported
    tracing categories. -/
structure PGetCategories where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetCategories where toJSON _ := .null

/-- Response of the `Tracing.getCategories` command. -/
structure GetCategories where
  /-- A list of supported tracing categories. -/
  categories : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetCategories where
  parseJSON v := do .ok { categories := ← Value.getField v "categories" >>= FromJSON.parseJSON }

instance : Command PGetCategories where
  Response := GetCategories
  commandName _ := "Tracing.getCategories"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Tracing.recordClockSyncMarker` command: records a
    clock sync marker in the trace. -/
structure PRecordClockSyncMarker where
  /-- The ID of this clock sync marker. -/
  syncId : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRecordClockSyncMarker where
  toJSON p := Data.Json.object [("syncId", ToJSON.toJSON p.syncId)]

instance : Command PRecordClockSyncMarker where
  Response := Unit
  commandName _ := "Tracing.recordClockSyncMarker"
  decodeResponse _ := .ok ()

/-- Parameters of the `Tracing.requestMemoryDump` command: requests a global
    memory dump. -/
structure PRequestMemoryDump where
  /-- Enables more deterministic results by forcing garbage collection. -/
  deterministic : Option Bool := none
  /-- Specifies the level of detail in the memory dump. Defaults to
      `detailed`. -/
  levelOfDetail : Option MemoryDumpLevelOfDetail := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRequestMemoryDump where
  toJSON p := Data.Json.object <|
    (p.deterministic.map fun v => ("deterministic", ToJSON.toJSON v)).toList
    ++ (p.levelOfDetail.map fun v => ("levelOfDetail", ToJSON.toJSON v)).toList

/-- Response of the `Tracing.requestMemoryDump` command. -/
structure RequestMemoryDump where
  /-- GUID of the resulting global memory dump. -/
  dumpGuid : String
  /-- `true` iff the global memory dump succeeded. -/
  success : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestMemoryDump where
  parseJSON v := do
    .ok
      { dumpGuid := ← Value.getField v "dumpGuid" >>= FromJSON.parseJSON
        success := ← Value.getField v "success" >>= FromJSON.parseJSON }

instance : Command PRequestMemoryDump where
  Response := RequestMemoryDump
  commandName _ := "Tracing.requestMemoryDump"
  decodeResponse := FromJSON.parseJSON

/-- Whether to report trace events as a series of `dataCollected` events or
    to save the trace to a stream, for the `Tracing.start` command. -/
inductive StartTransferMode where
  | reportEvents | returnAsStream
  deriving Repr, BEq, DecidableEq

instance : FromJSON StartTransferMode where
  parseJSON
    | .string "ReportEvents" => .ok .reportEvents
    | .string "ReturnAsStream" => .ok .returnAsStream
    | v => .error s!"failed to parse StartTransferMode: {repr v}"

instance : ToJSON StartTransferMode where
  toJSON | .reportEvents => .string "ReportEvents" | .returnAsStream => .string "ReturnAsStream"

/-- Parameters of the `Tracing.start` command: starts trace events
    collection. -/
structure PStart where
  /-- If set, the agent will issue `bufferUsage` events at this interval,
      specified in milliseconds. -/
  bufferUsageReportingInterval : Option Float := none
  /-- Whether to report trace events as a series of `dataCollected` events or
      to save the trace to a stream (defaults to `ReportEvents`). -/
  transferMode : Option StartTransferMode := none
  /-- Trace data format to use. This only applies when using the
      `ReturnAsStream` transfer mode (defaults to `json`). -/
  streamFormat : Option StreamFormat := none
  /-- Compression format to use. This only applies when using the
      `ReturnAsStream` transfer mode (defaults to `none`). -/
  streamCompression : Option StreamCompression := none
  traceConfig : Option TraceConfig := none
  /-- Base64-encoded serialized `perfetto.protos.TraceConfig` protobuf
      message. When specified, the parameters `categories`, `options`,
      `traceConfig` are ignored. -/
  perfettoConfig : Option String := none
  /-- Backend type (defaults to `auto`). -/
  tracingBackend : Option TracingBackend := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStart where
  toJSON p := Data.Json.object <|
    (p.bufferUsageReportingInterval.map fun v => ("bufferUsageReportingInterval", ToJSON.toJSON v)).toList
    ++ (p.transferMode.map fun v => ("transferMode", ToJSON.toJSON v)).toList
    ++ (p.streamFormat.map fun v => ("streamFormat", ToJSON.toJSON v)).toList
    ++ (p.streamCompression.map fun v => ("streamCompression", ToJSON.toJSON v)).toList
    ++ (p.traceConfig.map fun v => ("traceConfig", ToJSON.toJSON v)).toList
    ++ (p.perfettoConfig.map fun v => ("perfettoConfig", ToJSON.toJSON v)).toList
    ++ (p.tracingBackend.map fun v => ("tracingBackend", ToJSON.toJSON v)).toList

instance : Command PStart where
  Response := Unit
  commandName _ := "Tracing.start"
  decodeResponse _ := .ok ()

end CDP.Domains.Tracing
