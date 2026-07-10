/-
  Linen.CDP.Domains.PerformanceTimeline — the `PerformanceTimeline` CDP domain

  Reporting of performance timeline events, as specified in
  https://w3c.github.io/performance-timeline/#dom-performanceobserver. Ports
  `CDP.Domains.PerformanceTimeline` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring.

  None of this module's own types are self- or mutually-recursive, so no
  termination proofs are needed here.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity

namespace CDP.Domains.PerformanceTimeline

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Types ──

/-- `PerformanceTimeline.LargestContentfulPaint`. See
    https://github.com/WICG/LargestContentfulPaint and
    `largest_contentful_paint.idl`. -/
structure LargestContentfulPaint where
  renderTime : DOMPageNetworkEmulationSecurity.Network.TimeSinceEpoch
  loadTime : DOMPageNetworkEmulationSecurity.Network.TimeSinceEpoch
  /-- The number of pixels being painted. -/
  size : Float
  /-- The id attribute of the element, if available. -/
  elementId : Option String := none
  /-- The URL of the image (may be trimmed). -/
  url : Option String := none
  nodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON LargestContentfulPaint where
  parseJSON v := do
    .ok
      { renderTime := ← Value.getField v "renderTime" >>= FromJSON.parseJSON
        loadTime := ← Value.getField v "loadTime" >>= FromJSON.parseJSON
        size := ← Value.getField v "size" >>= FromJSON.parseJSON
        elementId := ← (← Value.getFieldOpt v "elementId").mapM FromJSON.parseJSON
        url := ← (← Value.getFieldOpt v "url").mapM FromJSON.parseJSON
        nodeId := ← (← Value.getFieldOpt v "nodeId").mapM FromJSON.parseJSON }

instance : ToJSON LargestContentfulPaint where
  toJSON p := Data.Json.object <|
       [("renderTime", ToJSON.toJSON p.renderTime)]
    ++ [("loadTime", ToJSON.toJSON p.loadTime)]
    ++ [("size", ToJSON.toJSON p.size)]
    ++ (p.elementId.map fun v => ("elementId", ToJSON.toJSON v)).toList
    ++ (p.url.map fun v => ("url", ToJSON.toJSON v)).toList
    ++ (p.nodeId.map fun v => ("nodeId", ToJSON.toJSON v)).toList

/-- `PerformanceTimeline.LayoutShiftAttribution`. -/
structure LayoutShiftAttribution where
  previousRect : DOMPageNetworkEmulationSecurity.DOM.Rect
  currentRect : DOMPageNetworkEmulationSecurity.DOM.Rect
  nodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON LayoutShiftAttribution where
  parseJSON v := do
    .ok
      { previousRect := ← Value.getField v "previousRect" >>= FromJSON.parseJSON
        currentRect := ← Value.getField v "currentRect" >>= FromJSON.parseJSON
        nodeId := ← (← Value.getFieldOpt v "nodeId").mapM FromJSON.parseJSON }

instance : ToJSON LayoutShiftAttribution where
  toJSON p := Data.Json.object <|
       [("previousRect", ToJSON.toJSON p.previousRect)]
    ++ [("currentRect", ToJSON.toJSON p.currentRect)]
    ++ (p.nodeId.map fun v => ("nodeId", ToJSON.toJSON v)).toList

/-- `PerformanceTimeline.LayoutShift`. See
    https://wicg.github.io/layout-instability/#sec-layout-shift and
    `layout_shift.idl`. -/
structure LayoutShift where
  /-- Score increment produced by this event. -/
  value : Float
  hadRecentInput : Bool
  lastInputTime : DOMPageNetworkEmulationSecurity.Network.TimeSinceEpoch
  sources : List LayoutShiftAttribution
  deriving Repr, BEq, DecidableEq

instance : FromJSON LayoutShift where
  parseJSON v := do
    .ok
      { value := ← Value.getField v "value" >>= FromJSON.parseJSON
        hadRecentInput := ← Value.getField v "hadRecentInput" >>= FromJSON.parseJSON
        lastInputTime := ← Value.getField v "lastInputTime" >>= FromJSON.parseJSON
        sources := ← Value.getField v "sources" >>= FromJSON.parseJSON }

instance : ToJSON LayoutShift where
  toJSON p := Data.Json.object <|
       [("value", ToJSON.toJSON p.value)]
    ++ [("hadRecentInput", ToJSON.toJSON p.hadRecentInput)]
    ++ [("lastInputTime", ToJSON.toJSON p.lastInputTime)]
    ++ [("sources", ToJSON.toJSON p.sources)]

/-- `PerformanceTimeline.TimelineEvent`. -/
structure TimelineEvent where
  /-- Identifies the frame that this event is related to. Empty for
      non-frame targets. -/
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  /-- The event type, as specified in
      https://w3c.github.io/performance-timeline/#dom-performanceentry-entrytype.
      This determines which of the optional "details" fields is present. -/
  type : String
  /-- Name may be empty depending on the type. -/
  name : String
  /-- Time in seconds since Epoch, monotonically increasing within document
      lifetime. -/
  time : DOMPageNetworkEmulationSecurity.Network.TimeSinceEpoch
  /-- Event duration, if applicable. -/
  duration : Option Float := none
  lcpDetails : Option LargestContentfulPaint := none
  layoutShiftDetails : Option LayoutShift := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON TimelineEvent where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        time := ← Value.getField v "time" >>= FromJSON.parseJSON
        duration := ← (← Value.getFieldOpt v "duration").mapM FromJSON.parseJSON
        lcpDetails := ← (← Value.getFieldOpt v "lcpDetails").mapM FromJSON.parseJSON
        layoutShiftDetails := ← (← Value.getFieldOpt v "layoutShiftDetails").mapM FromJSON.parseJSON }

instance : ToJSON TimelineEvent where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
    ++ [("type", ToJSON.toJSON p.type)]
    ++ [("name", ToJSON.toJSON p.name)]
    ++ [("time", ToJSON.toJSON p.time)]
    ++ (p.duration.map fun v => ("duration", ToJSON.toJSON v)).toList
    ++ (p.lcpDetails.map fun v => ("lcpDetails", ToJSON.toJSON v)).toList
    ++ (p.layoutShiftDetails.map fun v => ("layoutShiftDetails", ToJSON.toJSON v)).toList

-- ── Events ──

/-- The `PerformanceTimeline.timelineEventAdded` event: previously buffered
    events would be reported before `PerformanceTimeline.enable` returns. -/
structure TimelineEventAdded where
  event : TimelineEvent
  deriving Repr, BEq, DecidableEq

instance : FromJSON TimelineEventAdded where
  parseJSON v := do .ok { event := ← Value.getField v "event" >>= FromJSON.parseJSON }

instance : Event TimelineEventAdded where
  eventName := "PerformanceTimeline.timelineEventAdded"

-- ── Commands ──

/-- Parameters of the `PerformanceTimeline.enable` command. -/
structure PEnable where
  /-- The types of event to report, as specified in
      https://w3c.github.io/performance-timeline/#dom-performanceentry-entrytype.
      The specified filter overrides any previous filters, passing an empty
      filter disables recording. Note that not all types exposed to the web
      platform are currently supported. -/
  eventTypes : List String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where
  toJSON p := Data.Json.object [("eventTypes", ToJSON.toJSON p.eventTypes)]

instance : Command PEnable where
  Response := Unit
  commandName _ := "PerformanceTimeline.enable"
  decodeResponse _ := .ok ()

end CDP.Domains.PerformanceTimeline
