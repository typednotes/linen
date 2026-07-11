/-
  Linen.CDP.Domains.HeapProfiler — the `HeapProfiler` CDP domain

  Ports `CDP.Domains.HeapProfiler` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring.

  `SamplingHeapProfileNode` is self-referential via `children` — Lean accepts
  the *type* directly (recursion through `List` is positive), but
  `FromJSON`/`ToJSON` need a real termination proof for the same reason
  `Data.Json.Decode`'s own mutually-recursive parser does; see
  `parseSamplingHeapProfileNode`/`encodeSamplingHeapProfileNode` below and
  `CDP.Domains.Media.PlayerError` for the same technique applied to that
  module's singly-recursive case.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.HeapProfiler

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

/-- Heap snapshot object id. -/
abbrev HeapSnapshotObjectId := String

-- ── Sampling heap profile ──

/-- Sampling Heap Profile node. Holds callsite information, allocation
    statistics and child nodes. -/
structure SamplingHeapProfileNode where
  /-- Function location. -/
  callFrame : Runtime.CallFrame
  /-- Allocations size in bytes for the node excluding children. -/
  selfSize : Float
  /-- Node id. Ids are unique across all profiles collected between
      `startSampling` and `stopSampling`. -/
  id : Int
  /-- Child nodes. -/
  children : List SamplingHeapProfileNode
  deriving Repr, BEq

set_option linter.unusedVariables false in
mutual

/-- Decode a `SamplingHeapProfileNode`. A plain recursive `def` — rather than
    `children` going through the generic `FromJSON (List α)` instance — to
    sidestep the circular instance dependency a self-referential `instance :
    FromJSON SamplingHeapProfileNode` would otherwise have on itself.
    Terminates on `sizeOf`, via `Value.getField_sizeOf_lt`. -/
def parseSamplingHeapProfileNode (v : Value) : Except String SamplingHeapProfileNode :=
  match h : Value.getField v "children" with
  | .error e => .error e
  | .ok childrenV =>
    match parseSamplingHeapProfileNodeList childrenV with
    | .error e => .error e
    | .ok children =>
      (do
        let callFrame ← Value.getField v "callFrame" >>= FromJSON.parseJSON
        let selfSize ← Value.getField v "selfSize" >>= FromJSON.parseJSON
        let id ← Value.getField v "id" >>= FromJSON.parseJSON
        pure { callFrame, selfSize, id, children })
termination_by sizeOf v
decreasing_by exact Value.getField_sizeOf_lt h

private def parseSamplingHeapProfileNodeList (v : Value) : Except String (List SamplingHeapProfileNode) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => parseSamplingHeapProfileNode p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega

end

instance : FromJSON SamplingHeapProfileNode where parseJSON := parseSamplingHeapProfileNode

mutual

/-- Encode a `SamplingHeapProfileNode`. A plain recursive `def`, for the same
    reason `parseSamplingHeapProfileNode` is: sidesteps the circular instance
    dependency a self-referential `instance : ToJSON
    SamplingHeapProfileNode` would have on itself through the generic
    `ToJSON (List α)` instance. Terminates structurally on
    `SamplingHeapProfileNode.children`'s own `sizeOf` (an ordinary Lean value,
    not JSON to be decoded, so no `Value.getField`-style lemma is needed
    here). -/
def encodeSamplingHeapProfileNode (p : SamplingHeapProfileNode) : Value :=
  Data.Json.object
    [ ("callFrame", ToJSON.toJSON p.callFrame), ("selfSize", ToJSON.toJSON p.selfSize)
    , ("id", ToJSON.toJSON p.id), ("children", encodeSamplingHeapProfileNodeList p.children) ]
termination_by sizeOf p
decreasing_by
  cases p with
  | mk callFrame selfSize id children =>
    simp only [SamplingHeapProfileNode.mk.sizeOf_spec]
    omega

private def encodeSamplingHeapProfileNodeList (l : List SamplingHeapProfileNode) : Value :=
  Value.array (l.map encodeSamplingHeapProfileNode).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega

end

instance : ToJSON SamplingHeapProfileNode where toJSON := encodeSamplingHeapProfileNode

/-- A single sample from a sampling profile. -/
structure SamplingHeapProfileSample where
  /-- Allocation size in bytes attributed to the sample. -/
  size : Float
  /-- Id of the corresponding profile tree node. -/
  nodeId : Int
  /-- Time-ordered sample ordinal number. It is unique across all profiles
      retrieved between `startSampling` and `stopSampling`. -/
  ordinal : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON SamplingHeapProfileSample where
  parseJSON v := do
    .ok
      { size := ← Value.getField v "size" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        ordinal := ← Value.getField v "ordinal" >>= FromJSON.parseJSON }

instance : ToJSON SamplingHeapProfileSample where
  toJSON p := Data.Json.object
    [("size", ToJSON.toJSON p.size), ("nodeId", ToJSON.toJSON p.nodeId), ("ordinal", ToJSON.toJSON p.ordinal)]

/-- Sampling profile. -/
structure SamplingHeapProfile where
  head : SamplingHeapProfileNode
  samples : List SamplingHeapProfileSample
  deriving Repr, BEq

instance : FromJSON SamplingHeapProfile where
  parseJSON v := do
    .ok
      { head := ← Value.getField v "head" >>= FromJSON.parseJSON
        samples := ← Value.getField v "samples" >>= FromJSON.parseJSON }

instance : ToJSON SamplingHeapProfile where
  toJSON p := Data.Json.object [("head", ToJSON.toJSON p.head), ("samples", ToJSON.toJSON p.samples)]

-- ── Events ──

/-- The `HeapProfiler.addHeapSnapshotChunk` event. -/
structure AddHeapSnapshotChunk where
  chunk : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON AddHeapSnapshotChunk where
  parseJSON v := do .ok { chunk := ← Value.getField v "chunk" >>= FromJSON.parseJSON }

instance : Event AddHeapSnapshotChunk where
  eventName := "HeapProfiler.addHeapSnapshotChunk"

/-- The `HeapProfiler.heapStatsUpdate` event. An array of triplets. Each
    triplet describes a fragment: the first integer is the fragment index,
    the second is a total count of objects for the fragment, the third is a
    total size of the objects for the fragment. -/
structure HeapStatsUpdate where
  statsUpdate : List Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON HeapStatsUpdate where
  parseJSON v := do .ok { statsUpdate := ← Value.getField v "statsUpdate" >>= FromJSON.parseJSON }

instance : Event HeapStatsUpdate where
  eventName := "HeapProfiler.heapStatsUpdate"

/-- The `HeapProfiler.lastSeenObjectId` event. -/
structure LastSeenObjectId where
  lastSeenObjectId : Int
  timestamp : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON LastSeenObjectId where
  parseJSON v := do
    .ok
      { lastSeenObjectId := ← Value.getField v "lastSeenObjectId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }

instance : Event LastSeenObjectId where
  eventName := "HeapProfiler.lastSeenObjectId"

/-- The `HeapProfiler.reportHeapSnapshotProgress` event. -/
structure ReportHeapSnapshotProgress where
  done : Int
  total : Int
  finished : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ReportHeapSnapshotProgress where
  parseJSON v := do
    .ok
      { done := ← Value.getField v "done" >>= FromJSON.parseJSON
        total := ← Value.getField v "total" >>= FromJSON.parseJSON
        finished := ← (← Value.getFieldOpt v "finished").mapM FromJSON.parseJSON }

instance : Event ReportHeapSnapshotProgress where
  eventName := "HeapProfiler.reportHeapSnapshotProgress"

/-- The `HeapProfiler.resetProfiles` event. -/
structure ResetProfiles where
  deriving Repr, BEq, DecidableEq

instance : FromJSON ResetProfiles where parseJSON _ := .ok {}

instance : Event ResetProfiles where
  eventName := "HeapProfiler.resetProfiles"

-- ── Commands ──

/-- Parameters of the `HeapProfiler.addInspectedHeapObject` command: enables
    console to refer to the node with given id via `$x` (see Command Line
    API for more details `$x` functions). -/
structure PAddInspectedHeapObject where
  /-- Heap snapshot object id to be accessible by means of `$x` command line
      API. -/
  heapObjectId : HeapSnapshotObjectId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PAddInspectedHeapObject where
  toJSON p := Data.Json.object [("heapObjectId", ToJSON.toJSON p.heapObjectId)]

instance : Command PAddInspectedHeapObject where
  Response := Unit
  commandName _ := "HeapProfiler.addInspectedHeapObject"
  decodeResponse _ := .ok ()

/-- Parameters of the `HeapProfiler.collectGarbage` command. -/
structure PCollectGarbage where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PCollectGarbage where toJSON _ := .null

instance : Command PCollectGarbage where
  Response := Unit
  commandName _ := "HeapProfiler.collectGarbage"
  decodeResponse _ := .ok ()

/-- Parameters of the `HeapProfiler.disable` command. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "HeapProfiler.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `HeapProfiler.enable` command. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "HeapProfiler.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `HeapProfiler.getHeapObjectId` command. -/
structure PGetHeapObjectId where
  /-- Identifier of the object to get heap object id for. -/
  objectId : Runtime.RemoteObjectId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetHeapObjectId where
  toJSON p := Data.Json.object [("objectId", ToJSON.toJSON p.objectId)]

/-- Response of the `HeapProfiler.getHeapObjectId` command. -/
structure GetHeapObjectId where
  /-- Id of the heap snapshot object corresponding to the passed remote
      object id. -/
  heapSnapshotObjectId : HeapSnapshotObjectId
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetHeapObjectId where
  parseJSON v := do
    .ok { heapSnapshotObjectId := ← Value.getField v "heapSnapshotObjectId" >>= FromJSON.parseJSON }

instance : Command PGetHeapObjectId where
  Response := GetHeapObjectId
  commandName _ := "HeapProfiler.getHeapObjectId"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `HeapProfiler.getObjectByHeapObjectId` command. -/
structure PGetObjectByHeapObjectId where
  objectId : HeapSnapshotObjectId
  /-- Symbolic group name that can be used to release multiple objects. -/
  objectGroup : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetObjectByHeapObjectId where
  toJSON p := Data.Json.object <|
    [("objectId", ToJSON.toJSON p.objectId)]
    ++ (p.objectGroup.map fun v => ("objectGroup", ToJSON.toJSON v)).toList

/-- Response of the `HeapProfiler.getObjectByHeapObjectId` command. -/
structure GetObjectByHeapObjectId where
  /-- Evaluation result. -/
  result : Runtime.RemoteObject
  deriving Repr, BEq

instance : FromJSON GetObjectByHeapObjectId where
  parseJSON v := do .ok { result := ← Value.getField v "result" >>= FromJSON.parseJSON }

instance : Command PGetObjectByHeapObjectId where
  Response := GetObjectByHeapObjectId
  commandName _ := "HeapProfiler.getObjectByHeapObjectId"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `HeapProfiler.getSamplingProfile` command. -/
structure PGetSamplingProfile where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetSamplingProfile where toJSON _ := .null

/-- Response of the `HeapProfiler.getSamplingProfile` command. -/
structure GetSamplingProfile where
  /-- Return the sampling profile being collected. -/
  profile : SamplingHeapProfile
  deriving Repr, BEq

instance : FromJSON GetSamplingProfile where
  parseJSON v := do .ok { profile := ← Value.getField v "profile" >>= FromJSON.parseJSON }

instance : Command PGetSamplingProfile where
  Response := GetSamplingProfile
  commandName _ := "HeapProfiler.getSamplingProfile"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `HeapProfiler.startSampling` command. -/
structure PStartSampling where
  /-- Average sample interval in bytes. Poisson distribution is used for the
      intervals. The default value is 32768 bytes. -/
  samplingInterval : Option Float := none
  /-- By default, the sampling heap profiler reports only objects which are
      still alive when the profile is returned via `getSamplingProfile` or
      `stopSampling`, which is useful for determining what functions
      contribute the most to steady-state memory usage. This flag instructs
      the sampling heap profiler to also include information about objects
      discarded by major GC, which will show which functions cause large
      temporary memory usage or long GC pauses. -/
  includeObjectsCollectedByMajorGC : Option Bool := none
  /-- By default, the sampling heap profiler reports only objects which are
      still alive when the profile is returned via `getSamplingProfile` or
      `stopSampling`, which is useful for determining what functions
      contribute the most to steady-state memory usage. This flag instructs
      the sampling heap profiler to also include information about objects
      discarded by minor GC, which is useful when tuning a latency-sensitive
      application for minimal GC activity. -/
  includeObjectsCollectedByMinorGC : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartSampling where
  toJSON p := Data.Json.object <|
    (p.samplingInterval.map fun v => ("samplingInterval", ToJSON.toJSON v)).toList
    ++ (p.includeObjectsCollectedByMajorGC.map fun v =>
          ("includeObjectsCollectedByMajorGC", ToJSON.toJSON v)).toList
    ++ (p.includeObjectsCollectedByMinorGC.map fun v =>
          ("includeObjectsCollectedByMinorGC", ToJSON.toJSON v)).toList

instance : Command PStartSampling where
  Response := Unit
  commandName _ := "HeapProfiler.startSampling"
  decodeResponse _ := .ok ()

/-- Parameters of the `HeapProfiler.startTrackingHeapObjects` command. -/
structure PStartTrackingHeapObjects where
  trackAllocations : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartTrackingHeapObjects where
  toJSON p := Data.Json.object <|
    (p.trackAllocations.map fun v => ("trackAllocations", ToJSON.toJSON v)).toList

instance : Command PStartTrackingHeapObjects where
  Response := Unit
  commandName _ := "HeapProfiler.startTrackingHeapObjects"
  decodeResponse _ := .ok ()

/-- Parameters of the `HeapProfiler.stopSampling` command. -/
structure PStopSampling where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopSampling where toJSON _ := .null

/-- Response of the `HeapProfiler.stopSampling` command. -/
structure StopSampling where
  /-- Recorded sampling heap profile. -/
  profile : SamplingHeapProfile
  deriving Repr, BEq

instance : FromJSON StopSampling where
  parseJSON v := do .ok { profile := ← Value.getField v "profile" >>= FromJSON.parseJSON }

instance : Command PStopSampling where
  Response := StopSampling
  commandName _ := "HeapProfiler.stopSampling"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `HeapProfiler.stopTrackingHeapObjects` command. -/
structure PStopTrackingHeapObjects where
  /-- If `true` `reportHeapSnapshotProgress` events will be generated while
      snapshot is being taken when the tracking is stopped. -/
  reportProgress : Option Bool := none
  /-- If `true`, numerical values are included in the snapshot. -/
  captureNumericValue : Option Bool := none
  /-- If `true`, exposes internals of the snapshot. -/
  exposeInternals : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopTrackingHeapObjects where
  toJSON p := Data.Json.object <|
    (p.reportProgress.map fun v => ("reportProgress", ToJSON.toJSON v)).toList
    ++ (p.captureNumericValue.map fun v => ("captureNumericValue", ToJSON.toJSON v)).toList
    ++ (p.exposeInternals.map fun v => ("exposeInternals", ToJSON.toJSON v)).toList

instance : Command PStopTrackingHeapObjects where
  Response := Unit
  commandName _ := "HeapProfiler.stopTrackingHeapObjects"
  decodeResponse _ := .ok ()

/-- Parameters of the `HeapProfiler.takeHeapSnapshot` command. -/
structure PTakeHeapSnapshot where
  /-- If `true` `reportHeapSnapshotProgress` events will be generated while
      snapshot is being taken. -/
  reportProgress : Option Bool := none
  /-- If `true`, numerical values are included in the snapshot. -/
  captureNumericValue : Option Bool := none
  /-- If `true`, exposes internals of the snapshot. -/
  exposeInternals : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTakeHeapSnapshot where
  toJSON p := Data.Json.object <|
    (p.reportProgress.map fun v => ("reportProgress", ToJSON.toJSON v)).toList
    ++ (p.captureNumericValue.map fun v => ("captureNumericValue", ToJSON.toJSON v)).toList
    ++ (p.exposeInternals.map fun v => ("exposeInternals", ToJSON.toJSON v)).toList

instance : Command PTakeHeapSnapshot where
  Response := Unit
  commandName _ := "HeapProfiler.takeHeapSnapshot"
  decodeResponse _ := .ok ()

end CDP.Domains.HeapProfiler
