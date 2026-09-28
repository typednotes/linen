/-
  Tests for `Linen.CDP.Domains.HeapProfiler`.
-/
import Linen.CDP.Domains.HeapProfiler

open CDP.Domains.HeapProfiler
open CDP.Domains.Runtime (CallFrame ObjType RemoteObject)
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.HeapProfiler

/-- A minimal leaf call frame used across these tests. -/
def sampleCallFrame : CallFrame :=
  { functionName := "f", scriptId := "1", url := "a.js", lineNumber := 0, columnNumber := 0 }

def sampleCallFrameJson : String :=
  "{\"functionName\":\"f\",\"scriptId\":\"1\",\"url\":\"a.js\",\"lineNumber\":0,\"columnNumber\":0}"

/-! `SamplingHeapProfileNode` is genuinely self-referential (`children :
    List SamplingHeapProfileNode`); like `CDP.Domains.Media.PlayerError`, it
    has no `DecidableEq` (only `BEq`, since the auto-deriving handler can't
    see through the hand-proven recursive `FromJSON`/`ToJSON`), so equality
    checks here pattern-match or use `==` rather than `=`. -/

-- SamplingHeapProfileNode: leaf (no children).
#guard match decodeAs
    ("{\"callFrame\":" ++ sampleCallFrameJson ++ ",\"selfSize\":10,\"id\":1,\"children\":[]}")
    (α := SamplingHeapProfileNode) with
  | .ok n => n == ({ callFrame := sampleCallFrame, selfSize := 10, id := 1, children := [] } :
      SamplingHeapProfileNode)
  | .error _ => false

-- SamplingHeapProfileNode: one level of recursion via `children`.
#guard match decodeAs
    ("{\"callFrame\":" ++ sampleCallFrameJson ++ ",\"selfSize\":10,\"id\":1,\"children\":["
      ++ "{\"callFrame\":" ++ sampleCallFrameJson ++ ",\"selfSize\":5,\"id\":2,\"children\":[]}]}")
    (α := SamplingHeapProfileNode) with
  | .ok n =>
    n == ({ callFrame := sampleCallFrame, selfSize := 10, id := 1
            children := [{ callFrame := sampleCallFrame, selfSize := 5, id := 2, children := [] }] } :
      SamplingHeapProfileNode)
  | .error _ => false

#guard encode (ToJSON.toJSON
    ({ callFrame := sampleCallFrame, selfSize := 10, id := 1, children := [] } : SamplingHeapProfileNode))
  = "{\"callFrame\":" ++ sampleCallFrameJson ++ ",\"selfSize\":10,\"id\":1,\"children\":[]}"

-- Encode/decode round-trips through a nested `children`.
def innerNode : SamplingHeapProfileNode :=
  { callFrame := sampleCallFrame, selfSize := 5, id := 2, children := [] }
def outerNode : SamplingHeapProfileNode :=
  { callFrame := sampleCallFrame, selfSize := 10, id := 1, children := [innerNode] }

#guard match decodeAs (encode (ToJSON.toJSON outerNode)) (α := SamplingHeapProfileNode) with
  | .ok n => n.id == 1 && n.children.length == 1
  | .error _ => false

#guard decodeAs "{\"size\": 1, \"nodeId\": 2, \"ordinal\": 3}" (α := SamplingHeapProfileSample)
  = .ok { size := 1, nodeId := 2, ordinal := 3 }
#guard encode (ToJSON.toJSON ({ size := 1, nodeId := 2, ordinal := 3 } : SamplingHeapProfileSample))
  = "{\"size\":1,\"nodeId\":2,\"ordinal\":3}"

#guard match decodeAs
    ("{\"head\":{\"callFrame\":" ++ sampleCallFrameJson ++ ",\"selfSize\":0,\"id\":1,\"children\":[]},"
      ++ "\"samples\":[]}")
    (α := SamplingHeapProfile) with
  | .ok p => p.head.id == 1 && p.samples == []
  | .error _ => false

-- Events.
#guard decodeAs "{\"chunk\": \"abc\"}" (α := AddHeapSnapshotChunk) = .ok { chunk := "abc" }
#guard Event.eventName (α := AddHeapSnapshotChunk) = "HeapProfiler.addHeapSnapshotChunk"

#guard decodeAs "{\"statsUpdate\": [1, 2, 3]}" (α := HeapStatsUpdate) = .ok { statsUpdate := [1, 2, 3] }
#guard Event.eventName (α := HeapStatsUpdate) = "HeapProfiler.heapStatsUpdate"

#guard decodeAs "{\"lastSeenObjectId\": 5, \"timestamp\": 1.5}" (α := LastSeenObjectId)
  = .ok { lastSeenObjectId := 5, timestamp := 1.5 }
#guard Event.eventName (α := LastSeenObjectId) = "HeapProfiler.lastSeenObjectId"

#guard decodeAs "{\"done\": 1, \"total\": 2}" (α := ReportHeapSnapshotProgress)
  = .ok { done := 1, total := 2, finished := none }
#guard decodeAs "{\"done\": 1, \"total\": 2, \"finished\": true}" (α := ReportHeapSnapshotProgress)
  = .ok { done := 1, total := 2, finished := some true }
#guard Event.eventName (α := ReportHeapSnapshotProgress) = "HeapProfiler.reportHeapSnapshotProgress"

#guard decodeAs "{}" (α := ResetProfiles) = .ok {}
#guard Event.eventName (α := ResetProfiles) = "HeapProfiler.resetProfiles"

-- Commands.
#guard encode (ToJSON.toJSON ({ heapObjectId := "1" } : PAddInspectedHeapObject))
  = "{\"heapObjectId\":\"1\"}"
#guard Command.commandName ({ heapObjectId := "1" } : PAddInspectedHeapObject)
  = "HeapProfiler.addInspectedHeapObject"

#guard encode (ToJSON.toJSON ({} : PCollectGarbage)) = "null"
#guard Command.commandName ({} : PCollectGarbage) = "HeapProfiler.collectGarbage"

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "HeapProfiler.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "HeapProfiler.enable"

#guard encode (ToJSON.toJSON ({ objectId := "42" } : PGetHeapObjectId)) = "{\"objectId\":\"42\"}"
#guard Command.commandName ({ objectId := "42" } : PGetHeapObjectId) = "HeapProfiler.getHeapObjectId"
#guard decodeAs "{\"heapSnapshotObjectId\": \"7\"}" (α := GetHeapObjectId)
  = .ok { heapSnapshotObjectId := "7" }

#guard encode (ToJSON.toJSON ({ objectId := "1", objectGroup := none } : PGetObjectByHeapObjectId))
  = "{\"objectId\":\"1\"}"
#guard encode (ToJSON.toJSON ({ objectId := "1", objectGroup := some "g" } : PGetObjectByHeapObjectId))
  = "{\"objectId\":\"1\",\"objectGroup\":\"g\"}"
#guard Command.commandName ({ objectId := "1" } : PGetObjectByHeapObjectId)
  = "HeapProfiler.getObjectByHeapObjectId"
#guard match decodeAs "{\"result\": {\"type\": \"undefined\"}}" (α := GetObjectByHeapObjectId) with
  | .ok r => r.result == ({ type := ObjType.undefined } : RemoteObject)
  | .error _ => false

#guard encode (ToJSON.toJSON ({} : PGetSamplingProfile)) = "null"
#guard Command.commandName ({} : PGetSamplingProfile) = "HeapProfiler.getSamplingProfile"
#guard match decodeAs
    ("{\"profile\": {\"head\":{\"callFrame\":" ++ sampleCallFrameJson
      ++ ",\"selfSize\":0,\"id\":1,\"children\":[]}, \"samples\": []}}")
    (α := GetSamplingProfile) with
  | .ok r => r.profile.head.id == 1 && r.profile.samples == []
  | .error _ => false

#guard encode (ToJSON.toJSON ({} : PStartSampling)) = "{}"
#guard encode (ToJSON.toJSON ({ samplingInterval := some 1024 } : PStartSampling))
  = "{\"samplingInterval\":1024}"
#guard Command.commandName ({} : PStartSampling) = "HeapProfiler.startSampling"

#guard encode (ToJSON.toJSON ({} : PStartTrackingHeapObjects)) = "{}"
#guard encode (ToJSON.toJSON ({ trackAllocations := some true } : PStartTrackingHeapObjects))
  = "{\"trackAllocations\":true}"
#guard Command.commandName ({} : PStartTrackingHeapObjects) = "HeapProfiler.startTrackingHeapObjects"

#guard encode (ToJSON.toJSON ({} : PStopSampling)) = "null"
#guard Command.commandName ({} : PStopSampling) = "HeapProfiler.stopSampling"
#guard match decodeAs
    ("{\"profile\": {\"head\":{\"callFrame\":" ++ sampleCallFrameJson
      ++ ",\"selfSize\":0,\"id\":1,\"children\":[]}, \"samples\": []}}")
    (α := StopSampling) with
  | .ok r => r.profile.head.id == 1 && r.profile.samples == []
  | .error _ => false

#guard encode (ToJSON.toJSON ({} : PStopTrackingHeapObjects)) = "{}"
#guard encode (ToJSON.toJSON ({ reportProgress := some true } : PStopTrackingHeapObjects))
  = "{\"reportProgress\":true}"
#guard Command.commandName ({} : PStopTrackingHeapObjects) = "HeapProfiler.stopTrackingHeapObjects"

#guard encode (ToJSON.toJSON ({} : PTakeHeapSnapshot)) = "{}"
#guard encode (ToJSON.toJSON ({ exposeInternals := some true } : PTakeHeapSnapshot))
  = "{\"exposeInternals\":true}"
#guard Command.commandName ({} : PTakeHeapSnapshot) = "HeapProfiler.takeHeapSnapshot"

end Tests.CDP.Domains.HeapProfiler
