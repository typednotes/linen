/-
  Linen.CDP.Domains.Profiler — the `Profiler` CDP domain

  Ports `CDP.Domains.Profiler` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring.

  `ProfileNode.children` refers to sibling nodes only by their `id : Int`
  (looked up in the flat `Profile.nodes` list), not by nesting the child
  structures inline, so this module has no self- or mutually-recursive types
  and needs no termination proofs.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.Debugger
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.Profiler

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Types ──

/-- Specifies a number of samples attributed to a certain source position. -/
structure PositionTickInfo where
  /-- Source line number (1-based). -/
  line : Int
  /-- Number of samples attributed to the source line. -/
  ticks : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON PositionTickInfo where
  parseJSON v := do
    .ok
      { line := ← Value.getField v "line" >>= FromJSON.parseJSON
        ticks := ← Value.getField v "ticks" >>= FromJSON.parseJSON }

instance : ToJSON PositionTickInfo where
  toJSON p := Data.Json.object [("line", ToJSON.toJSON p.line), ("ticks", ToJSON.toJSON p.ticks)]

/-- Profile node. Holds callsite information, execution statistics and child
    node ids. -/
structure ProfileNode where
  /-- Unique id of the node. -/
  id : Int
  /-- Function location. -/
  callFrame : Runtime.CallFrame
  /-- Number of samples where this node was on top of the call stack. -/
  hitCount : Option Int := none
  /-- Child node ids. -/
  children : Option (List Int) := none
  /-- The reason of being not optimized. The function may be deoptimized or
      marked as don't optimize. -/
  deoptReason : Option String := none
  /-- An array of source position ticks. -/
  positionTicks : Option (List PositionTickInfo) := none
  deriving Repr, BEq

instance : FromJSON ProfileNode where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        callFrame := ← Value.getField v "callFrame" >>= FromJSON.parseJSON
        hitCount := ← (← Value.getFieldOpt v "hitCount").mapM FromJSON.parseJSON
        children := ← (← Value.getFieldOpt v "children").mapM FromJSON.parseJSON
        deoptReason := ← (← Value.getFieldOpt v "deoptReason").mapM FromJSON.parseJSON
        positionTicks := ← (← Value.getFieldOpt v "positionTicks").mapM FromJSON.parseJSON }

instance : ToJSON ProfileNode where
  toJSON p := Data.Json.object <|
    [("id", ToJSON.toJSON p.id), ("callFrame", ToJSON.toJSON p.callFrame)]
    ++ (p.hitCount.map fun v => ("hitCount", ToJSON.toJSON v)).toList
    ++ (p.children.map fun v => ("children", ToJSON.toJSON v)).toList
    ++ (p.deoptReason.map fun v => ("deoptReason", ToJSON.toJSON v)).toList
    ++ (p.positionTicks.map fun v => ("positionTicks", ToJSON.toJSON v)).toList

/-- Profile. -/
structure Profile where
  /-- The list of profile nodes. First item is the root node. -/
  nodes : List ProfileNode
  /-- Profiling start timestamp in microseconds. -/
  startTime : Float
  /-- Profiling end timestamp in microseconds. -/
  endTime : Float
  /-- Ids of samples top nodes. -/
  samples : Option (List Int) := none
  /-- Time intervals between adjacent samples in microseconds. The first
      delta is relative to the profile `startTime`. -/
  timeDeltas : Option (List Int) := none
  deriving Repr, BEq

instance : FromJSON Profile where
  parseJSON v := do
    .ok
      { nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON
        startTime := ← Value.getField v "startTime" >>= FromJSON.parseJSON
        endTime := ← Value.getField v "endTime" >>= FromJSON.parseJSON
        samples := ← (← Value.getFieldOpt v "samples").mapM FromJSON.parseJSON
        timeDeltas := ← (← Value.getFieldOpt v "timeDeltas").mapM FromJSON.parseJSON }

instance : ToJSON Profile where
  toJSON p := Data.Json.object <|
    [ ("nodes", ToJSON.toJSON p.nodes), ("startTime", ToJSON.toJSON p.startTime)
    , ("endTime", ToJSON.toJSON p.endTime) ]
    ++ (p.samples.map fun v => ("samples", ToJSON.toJSON v)).toList
    ++ (p.timeDeltas.map fun v => ("timeDeltas", ToJSON.toJSON v)).toList

/-- Coverage data for a source range. -/
structure CoverageRange where
  /-- JavaScript script source offset for the range start. -/
  startOffset : Int
  /-- JavaScript script source offset for the range end. -/
  endOffset : Int
  /-- Collected execution count of the source range. -/
  count : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON CoverageRange where
  parseJSON v := do
    .ok
      { startOffset := ← Value.getField v "startOffset" >>= FromJSON.parseJSON
        endOffset := ← Value.getField v "endOffset" >>= FromJSON.parseJSON
        count := ← Value.getField v "count" >>= FromJSON.parseJSON }

instance : ToJSON CoverageRange where
  toJSON p := Data.Json.object
    [ ("startOffset", ToJSON.toJSON p.startOffset), ("endOffset", ToJSON.toJSON p.endOffset)
    , ("count", ToJSON.toJSON p.count) ]

/-- Coverage data for a JavaScript function. -/
structure FunctionCoverage where
  /-- JavaScript function name. -/
  functionName : String
  /-- Source ranges inside the function with coverage data. -/
  ranges : List CoverageRange
  /-- Whether coverage data for this function has block granularity. -/
  isBlockCoverage : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON FunctionCoverage where
  parseJSON v := do
    .ok
      { functionName := ← Value.getField v "functionName" >>= FromJSON.parseJSON
        ranges := ← Value.getField v "ranges" >>= FromJSON.parseJSON
        isBlockCoverage := ← Value.getField v "isBlockCoverage" >>= FromJSON.parseJSON }

instance : ToJSON FunctionCoverage where
  toJSON p := Data.Json.object
    [ ("functionName", ToJSON.toJSON p.functionName), ("ranges", ToJSON.toJSON p.ranges)
    , ("isBlockCoverage", ToJSON.toJSON p.isBlockCoverage) ]

/-- Coverage data for a JavaScript script. -/
structure ScriptCoverage where
  /-- JavaScript script id. -/
  scriptId : Runtime.ScriptId
  /-- JavaScript script name or url. -/
  url : String
  /-- Functions contained in the script that has coverage data. -/
  functions : List FunctionCoverage
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScriptCoverage where
  parseJSON v := do
    .ok
      { scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        functions := ← Value.getField v "functions" >>= FromJSON.parseJSON }

instance : ToJSON ScriptCoverage where
  toJSON p := Data.Json.object
    [ ("scriptId", ToJSON.toJSON p.scriptId), ("url", ToJSON.toJSON p.url)
    , ("functions", ToJSON.toJSON p.functions) ]

-- ── Events ──

/-- The `Profiler.consoleProfileFinished` event. -/
structure ConsoleProfileFinished where
  id : String
  /-- Location of `console.profileEnd()`. -/
  location : Debugger.Location
  profile : Profile
  /-- Profile title passed as an argument to `console.profile()`. -/
  title : Option String := none
  deriving Repr, BEq

instance : FromJSON ConsoleProfileFinished where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        location := ← Value.getField v "location" >>= FromJSON.parseJSON
        profile := ← Value.getField v "profile" >>= FromJSON.parseJSON
        title := ← (← Value.getFieldOpt v "title").mapM FromJSON.parseJSON }

instance : Event ConsoleProfileFinished where
  eventName := "Profiler.consoleProfileFinished"

/-- The `Profiler.consoleProfileStarted` event. -/
structure ConsoleProfileStarted where
  id : String
  /-- Location of `console.profile()`. -/
  location : Debugger.Location
  /-- Profile title passed as an argument to `console.profile()`. -/
  title : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ConsoleProfileStarted where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        location := ← Value.getField v "location" >>= FromJSON.parseJSON
        title := ← (← Value.getFieldOpt v "title").mapM FromJSON.parseJSON }

instance : Event ConsoleProfileStarted where
  eventName := "Profiler.consoleProfileStarted"

/-- The `Profiler.preciseCoverageDeltaUpdate` event. -/
structure PreciseCoverageDeltaUpdate where
  /-- Monotonically increasing time (in seconds) when the coverage update
      was taken in the backend. -/
  timestamp : Float
  /-- Identifier for distinguishing coverage events. -/
  occasion : String
  /-- Coverage data for the current isolate. -/
  result : List ScriptCoverage
  deriving Repr, BEq

instance : FromJSON PreciseCoverageDeltaUpdate where
  parseJSON v := do
    .ok
      { timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        occasion := ← Value.getField v "occasion" >>= FromJSON.parseJSON
        result := ← Value.getField v "result" >>= FromJSON.parseJSON }

instance : Event PreciseCoverageDeltaUpdate where
  eventName := "Profiler.preciseCoverageDeltaUpdate"

-- ── Commands ──

/-- Parameters of the `Profiler.disable` command. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Profiler.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Profiler.enable` command. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Profiler.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Profiler.getBestEffortCoverage` command: collects
    coverage data for the current isolate. The coverage data may be
    incomplete due to garbage collection. -/
structure PGetBestEffortCoverage where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetBestEffortCoverage where toJSON _ := .null

/-- Response of the `Profiler.getBestEffortCoverage` command. -/
structure GetBestEffortCoverage where
  /-- Coverage data for the current isolate. -/
  result : List ScriptCoverage
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetBestEffortCoverage where
  parseJSON v := do .ok { result := ← Value.getField v "result" >>= FromJSON.parseJSON }

instance : Command PGetBestEffortCoverage where
  Response := GetBestEffortCoverage
  commandName _ := "Profiler.getBestEffortCoverage"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Profiler.setSamplingInterval` command: changes CPU
    profiler sampling interval. Must be called before CPU profiles recording
    started. -/
structure PSetSamplingInterval where
  /-- New sampling interval in microseconds. -/
  interval : Int
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetSamplingInterval where
  toJSON p := Data.Json.object [("interval", ToJSON.toJSON p.interval)]

instance : Command PSetSamplingInterval where
  Response := Unit
  commandName _ := "Profiler.setSamplingInterval"
  decodeResponse _ := .ok ()

/-- Parameters of the `Profiler.start` command. -/
structure PStart where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStart where toJSON _ := .null

instance : Command PStart where
  Response := Unit
  commandName _ := "Profiler.start"
  decodeResponse _ := .ok ()

/-- Parameters of the `Profiler.startPreciseCoverage` command: enables
    precise code coverage. Coverage data for JavaScript executed before
    enabling precise code coverage may be incomplete. Enabling prevents
    running optimized code and resets execution counters. -/
structure PStartPreciseCoverage where
  /-- Collect accurate call counts beyond simple 'covered' or 'not
      covered'. -/
  callCount : Option Bool := none
  /-- Collect block-based coverage. -/
  detailed : Option Bool := none
  /-- Allow the backend to send updates on its own initiative. -/
  allowTriggeredUpdates : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartPreciseCoverage where
  toJSON p := Data.Json.object <|
    (p.callCount.map fun v => ("callCount", ToJSON.toJSON v)).toList
    ++ (p.detailed.map fun v => ("detailed", ToJSON.toJSON v)).toList
    ++ (p.allowTriggeredUpdates.map fun v => ("allowTriggeredUpdates", ToJSON.toJSON v)).toList

/-- Response of the `Profiler.startPreciseCoverage` command. -/
structure StartPreciseCoverage where
  /-- Monotonically increasing time (in seconds) when the coverage update
      was taken in the backend. -/
  timestamp : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON StartPreciseCoverage where
  parseJSON v := do .ok { timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }

instance : Command PStartPreciseCoverage where
  Response := StartPreciseCoverage
  commandName _ := "Profiler.startPreciseCoverage"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Profiler.stop` command. -/
structure PStop where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStop where toJSON _ := .null

/-- Response of the `Profiler.stop` command. -/
structure Stop where
  /-- Recorded profile. -/
  profile : Profile
  deriving Repr, BEq

instance : FromJSON Stop where
  parseJSON v := do .ok { profile := ← Value.getField v "profile" >>= FromJSON.parseJSON }

instance : Command PStop where
  Response := Stop
  commandName _ := "Profiler.stop"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Profiler.stopPreciseCoverage` command: disables
    precise code coverage. Disabling releases unnecessary execution count
    records and allows executing optimized code. -/
structure PStopPreciseCoverage where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopPreciseCoverage where toJSON _ := .null

instance : Command PStopPreciseCoverage where
  Response := Unit
  commandName _ := "Profiler.stopPreciseCoverage"
  decodeResponse _ := .ok ()

/-- Parameters of the `Profiler.takePreciseCoverage` command: collects
    coverage data for the current isolate, and resets execution counters.
    Precise code coverage needs to have started. -/
structure PTakePreciseCoverage where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTakePreciseCoverage where toJSON _ := .null

/-- Response of the `Profiler.takePreciseCoverage` command. -/
structure TakePreciseCoverage where
  /-- Coverage data for the current isolate. -/
  result : List ScriptCoverage
  /-- Monotonically increasing time (in seconds) when the coverage update
      was taken in the backend. -/
  timestamp : Float
  deriving Repr, BEq

instance : FromJSON TakePreciseCoverage where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }

instance : Command PTakePreciseCoverage where
  Response := TakePreciseCoverage
  commandName _ := "Profiler.takePreciseCoverage"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.Profiler
