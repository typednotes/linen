/-
  Tests for `Linen.CDP.Domains.Overlay`.
-/
import Linen.CDP.Domains.Overlay

open CDP.Domains.Overlay
open CDP.Domains.DOMPageNetworkEmulationSecurity
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Overlay

-- ── Configuration types ──

#guard decodeAs "{\"parentOutlineColor\": {\"r\": 1, \"g\": 2, \"b\": 3}, \"childOutlineColor\": {\"r\": 4, \"g\": 5, \"b\": 6}}"
    (α := SourceOrderConfig)
  = .ok
    { parentOutlineColor := { r := 1, g := 2, b := 3 }
      childOutlineColor := { r := 4, g := 5, b := 6 } }
#guard encode (ToJSON.toJSON
    ({ parentOutlineColor := { r := 1, g := 2, b := 3 }
       childOutlineColor := { r := 4, g := 5, b := 6 } } : SourceOrderConfig))
  = "{\"parentOutlineColor\":{\"r\":1,\"g\":2,\"b\":3},\"childOutlineColor\":{\"r\":4,\"g\":5,\"b\":6}}"

#guard encode (ToJSON.toJSON ({} : GridHighlightConfig)) = "{}"
#guard decodeAs "{\"showGridExtensionLines\": true}" (α := GridHighlightConfig)
  = .ok { showGridExtensionLines := some true }
#guard encode (ToJSON.toJSON ({ showGridExtensionLines := some true } : GridHighlightConfig))
  = "{\"showGridExtensionLines\":true}"

#guard decodeAs "\"dashed\"" (α := LineStylePattern) = .ok .dashed
#guard encode (ToJSON.toJSON LineStylePattern.dotted) = "\"dotted\""

#guard encode (ToJSON.toJSON ({} : LineStyle)) = "{}"
#guard decodeAs "{\"pattern\": \"dashed\"}" (α := LineStyle) = .ok { pattern := some .dashed }

#guard encode (ToJSON.toJSON ({} : BoxStyle)) = "{}"
#guard decodeAs "{\"fillColor\": {\"r\": 0, \"g\": 0, \"b\": 0}}" (α := BoxStyle)
  = .ok { fillColor := some { r := 0, g := 0, b := 0 } }

#guard encode (ToJSON.toJSON ({} : FlexContainerHighlightConfig)) = "{}"
#guard decodeAs "{}" (α := FlexContainerHighlightConfig) = .ok {}

#guard encode (ToJSON.toJSON ({} : FlexItemHighlightConfig)) = "{}"
#guard decodeAs "{}" (α := FlexItemHighlightConfig) = .ok {}

#guard decodeAs "\"aa\"" (α := ContrastAlgorithm) = .ok .aa
#guard encode (ToJSON.toJSON ContrastAlgorithm.apca) = "\"apca\""

#guard decodeAs "\"hex\"" (α := ColorFormat) = .ok .hex
#guard encode (ToJSON.toJSON ColorFormat.rgb) = "\"rgb\""

#guard encode (ToJSON.toJSON ({} : ContainerQueryContainerHighlightConfig)) = "{}"
#guard decodeAs "{}" (α := ContainerQueryContainerHighlightConfig) = .ok {}

#guard encode (ToJSON.toJSON ({} : HighlightConfig)) = "{}"
#guard decodeAs "{\"showInfo\": true, \"colorFormat\": \"rgb\"}" (α := HighlightConfig)
  = .ok { showInfo := some true, colorFormat := some .rgb }

#guard decodeAs "{\"gridHighlightConfig\": {}, \"nodeId\": 1}" (α := GridNodeHighlightConfig)
  = .ok { gridHighlightConfig := {}, nodeId := 1 }
#guard encode (ToJSON.toJSON ({ gridHighlightConfig := {}, nodeId := (1 : DOM.NodeId) } : GridNodeHighlightConfig))
  = "{\"gridHighlightConfig\":{},\"nodeId\":1}"

#guard decodeAs "{\"flexContainerHighlightConfig\": {}, \"nodeId\": 1}" (α := FlexNodeHighlightConfig)
  = .ok { flexContainerHighlightConfig := {}, nodeId := 1 }

#guard encode (ToJSON.toJSON ({} : ScrollSnapContainerHighlightConfig)) = "{}"
#guard decodeAs "{}" (α := ScrollSnapContainerHighlightConfig) = .ok {}

#guard decodeAs "{\"scrollSnapContainerHighlightConfig\": {}, \"nodeId\": 2}" (α := ScrollSnapHighlightConfig)
  = .ok { scrollSnapContainerHighlightConfig := {}, nodeId := 2 }

#guard decodeAs "{\"rect\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 1}}" (α := HingeConfig)
  = .ok { rect := { x := 0, y := 0, width := 1, height := 1 } }
#guard encode (ToJSON.toJSON ({ rect := ({ x := 0, y := 0, width := 1, height := 1 } : DOM.Rect) } : HingeConfig))
  = "{\"rect\":{\"x\":0,\"y\":0,\"width\":1,\"height\":1}}"

#guard decodeAs "{\"containerQueryContainerHighlightConfig\": {}, \"nodeId\": 3}" (α := ContainerQueryHighlightConfig)
  = .ok { containerQueryContainerHighlightConfig := {}, nodeId := 3 }

#guard encode (ToJSON.toJSON ({} : IsolationModeHighlightConfig)) = "{}"
#guard decodeAs "{}" (α := IsolationModeHighlightConfig) = .ok {}

#guard decodeAs "{\"isolationModeHighlightConfig\": {}, \"nodeId\": 4}" (α := IsolatedElementHighlightConfig)
  = .ok { isolationModeHighlightConfig := {}, nodeId := 4 }

#guard decodeAs "\"searchForNode\"" (α := InspectMode) = .ok .searchForNode
#guard encode (ToJSON.toJSON InspectMode.none) = "\"none\""

-- ── Events ──

#guard decodeAs "{\"backendNodeId\": 5}" (α := InspectNodeRequested) = .ok { backendNodeId := 5 }
#guard Event.eventName (α := InspectNodeRequested) = "Overlay.inspectNodeRequested"

#guard decodeAs "{\"nodeId\": 6}" (α := NodeHighlightRequested) = .ok { nodeId := 6 }
#guard Event.eventName (α := NodeHighlightRequested) = "Overlay.nodeHighlightRequested"

#guard decodeAs "{\"viewport\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 1, \"scale\": 1}}"
    (α := ScreenshotRequested)
  = .ok { viewport := { x := 0, y := 0, width := 1, height := 1, scale := 1 } }
#guard Event.eventName (α := ScreenshotRequested) = "Overlay.screenshotRequested"

#guard decodeAs "null" (α := InspectModeCanceled) = .ok {}
#guard Event.eventName (α := InspectModeCanceled) = "Overlay.inspectModeCanceled"

-- ── Commands ──

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Overlay.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "Overlay.enable"

#guard encode (ToJSON.toJSON ({ nodeId := (1 : DOM.NodeId) } : PGetHighlightObjectForTest))
  = "{\"nodeId\":1}"
#guard Command.commandName ({ nodeId := (1 : DOM.NodeId) } : PGetHighlightObjectForTest)
  = "Overlay.getHighlightObjectForTest"
#guard decodeAs "{\"highlight\": [[\"k\", \"v\"]]}" (α := GetHighlightObjectForTest)
  = .ok { highlight := [("k", "v")] }

#guard encode (ToJSON.toJSON ({ nodeIds := [1, 2] } : PGetGridHighlightObjectsForTest))
  = "{\"nodeIds\":[1,2]}"
#guard Command.commandName ({ nodeIds := [1, 2] } : PGetGridHighlightObjectsForTest)
  = "Overlay.getGridHighlightObjectsForTest"
#guard decodeAs "{\"highlights\": [[\"k\", \"v\"]]}" (α := GetGridHighlightObjectsForTest)
  = .ok { highlights := [("k", "v")] }

#guard encode (ToJSON.toJSON ({ nodeId := (1 : DOM.NodeId) } : PGetSourceOrderHighlightObjectForTest))
  = "{\"nodeId\":1}"
#guard Command.commandName ({ nodeId := (1 : DOM.NodeId) } : PGetSourceOrderHighlightObjectForTest)
  = "Overlay.getSourceOrderHighlightObjectForTest"
#guard decodeAs "{\"highlight\": []}" (α := GetSourceOrderHighlightObjectForTest)
  = .ok { highlight := [] }

#guard encode (ToJSON.toJSON ({} : PHideHighlight)) = "null"
#guard Command.commandName ({} : PHideHighlight) = "Overlay.hideHighlight"

#guard encode (ToJSON.toJSON ({ highlightConfig := {} } : PHighlightNode))
  = "{\"highlightConfig\":{}}"
#guard encode (ToJSON.toJSON
    ({ highlightConfig := {}, nodeId := some 1 } : PHighlightNode))
  = "{\"highlightConfig\":{},\"nodeId\":1}"
#guard Command.commandName ({ highlightConfig := {} } : PHighlightNode) = "Overlay.highlightNode"

#guard encode (ToJSON.toJSON ({ quad := [0, 0, 1, 0, 1, 1, 0, 1] } : PHighlightQuad))
  = "{\"quad\":[0,0,1,0,1,1,0,1]}"
#guard Command.commandName ({ quad := [0, 0, 1, 0, 1, 1, 0, 1] } : PHighlightQuad) = "Overlay.highlightQuad"

#guard encode (ToJSON.toJSON ({ x := 0, y := 0, width := 1, height := 1 } : PHighlightRect))
  = "{\"x\":0,\"y\":0,\"width\":1,\"height\":1}"
#guard Command.commandName ({ x := 0, y := 0, width := 1, height := 1 } : PHighlightRect)
  = "Overlay.highlightRect"

#guard encode (ToJSON.toJSON
    ({ sourceOrderConfig := { parentOutlineColor := { r := 0, g := 0, b := 0 }
                              childOutlineColor := { r := 0, g := 0, b := 0 } } } : PHighlightSourceOrder))
  = "{\"sourceOrderConfig\":{\"parentOutlineColor\":{\"r\":0,\"g\":0,\"b\":0},\"childOutlineColor\":{\"r\":0,\"g\":0,\"b\":0}}}"
#guard Command.commandName
    ({ sourceOrderConfig := { parentOutlineColor := { r := 0, g := 0, b := 0 }
                              childOutlineColor := { r := 0, g := 0, b := 0 } } } : PHighlightSourceOrder)
  = "Overlay.highlightSourceOrder"

#guard encode (ToJSON.toJSON ({ mode := .searchForNode } : PSetInspectMode))
  = "{\"mode\":\"searchForNode\"}"
#guard Command.commandName ({ mode := .searchForNode } : PSetInspectMode) = "Overlay.setInspectMode"

#guard encode (ToJSON.toJSON ({ «show» := true } : PSetShowAdHighlights)) = "{\"show\":true}"
#guard Command.commandName ({ «show» := true } : PSetShowAdHighlights) = "Overlay.setShowAdHighlights"

#guard encode (ToJSON.toJSON ({} : PSetPausedInDebuggerMessage)) = "{}"
#guard encode (ToJSON.toJSON ({ message := some "paused" } : PSetPausedInDebuggerMessage))
  = "{\"message\":\"paused\"}"
#guard Command.commandName ({} : PSetPausedInDebuggerMessage) = "Overlay.setPausedInDebuggerMessage"

#guard encode (ToJSON.toJSON ({ «show» := true } : PSetShowDebugBorders)) = "{\"show\":true}"
#guard Command.commandName ({ «show» := true } : PSetShowDebugBorders) = "Overlay.setShowDebugBorders"

#guard encode (ToJSON.toJSON ({ «show» := true } : PSetShowFPSCounter)) = "{\"show\":true}"
#guard Command.commandName ({ «show» := true } : PSetShowFPSCounter) = "Overlay.setShowFPSCounter"

#guard encode (ToJSON.toJSON ({ gridNodeHighlightConfigs := [] } : PSetShowGridOverlays))
  = "{\"gridNodeHighlightConfigs\":[]}"
#guard Command.commandName ({ gridNodeHighlightConfigs := [] } : PSetShowGridOverlays)
  = "Overlay.setShowGridOverlays"

#guard encode (ToJSON.toJSON ({ flexNodeHighlightConfigs := [] } : PSetShowFlexOverlays))
  = "{\"flexNodeHighlightConfigs\":[]}"
#guard Command.commandName ({ flexNodeHighlightConfigs := [] } : PSetShowFlexOverlays)
  = "Overlay.setShowFlexOverlays"

#guard encode (ToJSON.toJSON ({ scrollSnapHighlightConfigs := [] } : PSetShowScrollSnapOverlays))
  = "{\"scrollSnapHighlightConfigs\":[]}"
#guard Command.commandName ({ scrollSnapHighlightConfigs := [] } : PSetShowScrollSnapOverlays)
  = "Overlay.setShowScrollSnapOverlays"

#guard encode (ToJSON.toJSON ({ containerQueryHighlightConfigs := [] } : PSetShowContainerQueryOverlays))
  = "{\"containerQueryHighlightConfigs\":[]}"
#guard Command.commandName ({ containerQueryHighlightConfigs := [] } : PSetShowContainerQueryOverlays)
  = "Overlay.setShowContainerQueryOverlays"

#guard encode (ToJSON.toJSON ({ result := true } : PSetShowPaintRects)) = "{\"result\":true}"
#guard Command.commandName ({ result := true } : PSetShowPaintRects) = "Overlay.setShowPaintRects"

#guard encode (ToJSON.toJSON ({ result := true } : PSetShowLayoutShiftRegions)) = "{\"result\":true}"
#guard Command.commandName ({ result := true } : PSetShowLayoutShiftRegions)
  = "Overlay.setShowLayoutShiftRegions"

#guard encode (ToJSON.toJSON ({ «show» := true } : PSetShowScrollBottleneckRects)) = "{\"show\":true}"
#guard Command.commandName ({ «show» := true } : PSetShowScrollBottleneckRects)
  = "Overlay.setShowScrollBottleneckRects"

#guard encode (ToJSON.toJSON ({ «show» := true } : PSetShowWebVitals)) = "{\"show\":true}"
#guard Command.commandName ({ «show» := true } : PSetShowWebVitals) = "Overlay.setShowWebVitals"

#guard encode (ToJSON.toJSON ({ «show» := true } : PSetShowViewportSizeOnResize)) = "{\"show\":true}"
#guard Command.commandName ({ «show» := true } : PSetShowViewportSizeOnResize)
  = "Overlay.setShowViewportSizeOnResize"

#guard encode (ToJSON.toJSON ({} : PSetShowHinge)) = "{}"
#guard encode (ToJSON.toJSON
    ({ hingeConfig := some { rect := { x := 0, y := 0, width := 1, height := 1 } } } : PSetShowHinge))
  = "{\"hingeConfig\":{\"rect\":{\"x\":0,\"y\":0,\"width\":1,\"height\":1}}}"
#guard Command.commandName ({} : PSetShowHinge) = "Overlay.setShowHinge"

#guard encode (ToJSON.toJSON ({ isolatedElementHighlightConfigs := [] } : PSetShowIsolatedElements))
  = "{\"isolatedElementHighlightConfigs\":[]}"
#guard Command.commandName ({ isolatedElementHighlightConfigs := [] } : PSetShowIsolatedElements)
  = "Overlay.setShowIsolatedElements"

end Tests.CDP.Domains.Overlay
