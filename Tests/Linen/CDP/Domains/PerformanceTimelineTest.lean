/-
  Tests for `Linen.CDP.Domains.PerformanceTimeline`.
-/
import Linen.CDP.Domains.PerformanceTimeline

open CDP.Domains.PerformanceTimeline
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.PerformanceTimeline

#guard decodeAs
    "{\"renderTime\": 1, \"loadTime\": 2, \"size\": 3, \"elementId\": \"e\", \"url\": \"u\", \"nodeId\": 5}"
    (α := LargestContentfulPaint)
  = .ok { renderTime := 1, loadTime := 2, size := 3, elementId := some "e", url := some "u", nodeId := some 5 }
#guard decodeAs "{\"renderTime\": 1, \"loadTime\": 2, \"size\": 3}" (α := LargestContentfulPaint)
  = .ok { renderTime := 1, loadTime := 2, size := 3 }
#guard encode (ToJSON.toJSON ({ renderTime := 1, loadTime := 2, size := 3 } : LargestContentfulPaint))
  = "{\"renderTime\":1,\"loadTime\":2,\"size\":3}"

#guard decodeAs
    "{\"previousRect\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 1}, \
     \"currentRect\": {\"x\": 1, \"y\": 1, \"width\": 2, \"height\": 2}, \"nodeId\": 3}"
    (α := LayoutShiftAttribution)
  = .ok
      { previousRect := { x := 0, y := 0, width := 1, height := 1 }
        currentRect := { x := 1, y := 1, width := 2, height := 2 }
        nodeId := some 3 }
#guard encode
    (ToJSON.toJSON
      ({ previousRect := { x := 0, y := 0, width := 1, height := 1 }
         currentRect := { x := 1, y := 1, width := 2, height := 2 } } : LayoutShiftAttribution))
  = "{\"previousRect\":{\"x\":0,\"y\":0,\"width\":1,\"height\":1}," ++
    "\"currentRect\":{\"x\":1,\"y\":1,\"width\":2,\"height\":2}}"

#guard decodeAs "{\"value\": 1, \"hadRecentInput\": false, \"lastInputTime\": 2, \"sources\": []}"
    (α := LayoutShift)
  = .ok { value := 1, hadRecentInput := false, lastInputTime := 2, sources := [] }
#guard encode
    (ToJSON.toJSON ({ value := 1, hadRecentInput := false, lastInputTime := 2, sources := [] } : LayoutShift))
  = "{\"value\":1,\"hadRecentInput\":false,\"lastInputTime\":2,\"sources\":[]}"

#guard decodeAs "{\"frameId\": \"f\", \"type\": \"layout-shift\", \"name\": \"\", \"time\": 1}"
    (α := TimelineEvent)
  = .ok { frameId := "f", type := "layout-shift", name := "", time := 1 }
#guard decodeAs
    "{\"frameId\": \"f\", \"type\": \"largest-contentful-paint\", \"name\": \"\", \"time\": 1, \"duration\": 2, \
     \"lcpDetails\": {\"renderTime\": 1, \"loadTime\": 2, \"size\": 3}}"
    (α := TimelineEvent)
  = .ok
      { frameId := "f", type := "largest-contentful-paint", name := "", time := 1, duration := some 2
        lcpDetails := some { renderTime := 1, loadTime := 2, size := 3 } }
#guard encode (ToJSON.toJSON ({ frameId := "f", type := "t", name := "n", time := 1 } : TimelineEvent))
  = "{\"frameId\":\"f\",\"type\":\"t\",\"name\":\"n\",\"time\":1}"

#guard decodeAs "{\"event\": {\"frameId\": \"f\", \"type\": \"t\", \"name\": \"\", \"time\": 1}}"
    (α := TimelineEventAdded)
  = .ok { event := { frameId := "f", type := "t", name := "", time := 1 } }
#guard Event.eventName (α := TimelineEventAdded) = "PerformanceTimeline.timelineEventAdded"

#guard encode (ToJSON.toJSON ({ eventTypes := ["largest-contentful-paint"] } : PEnable))
  = "{\"eventTypes\":[\"largest-contentful-paint\"]}"
#guard encode (ToJSON.toJSON ({ eventTypes := [] } : PEnable)) = "{\"eventTypes\":[]}"
#guard Command.commandName ({ eventTypes := [] } : PEnable) = "PerformanceTimeline.enable"

end Tests.CDP.Domains.PerformanceTimeline
