/-
  Tests for `Linen.CDP.Domains.LayerTree`.
-/
import Linen.CDP.Domains.LayerTree

open CDP.Domains.LayerTree
open CDP.Domains
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.LayerTree

-- ── Types ──

#guard decodeAs "\"RepaintsOnScroll\"" (α := ScrollRectType) = .ok .repaintsOnScroll
#guard encode (ToJSON.toJSON ScrollRectType.wheelEventHandler) = "\"WheelEventHandler\""

#guard decodeAs "{\"rect\": {\"x\": 1, \"y\": 2, \"width\": 3, \"height\": 4}, \"type\": \"TouchEventHandler\"}"
    (α := ScrollRect)
  = .ok { rect := { x := 1, y := 2, width := 3, height := 4 }, type := .touchEventHandler }
#guard encode (ToJSON.toJSON
    ({ rect := { x := 1, y := 2, width := 3, height := 4 }, type := .touchEventHandler } : ScrollRect))
  = "{\"rect\":{\"x\":1,\"y\":2,\"width\":3,\"height\":4},\"type\":\"TouchEventHandler\"}"

#guard decodeAs
    "{\"stickyBoxRect\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 1}, \"containingBlockRect\": {\"x\": 0, \"y\": 0, \"width\": 2, \"height\": 2}}"
    (α := StickyPositionConstraint)
  = .ok
    { stickyBoxRect := { x := 0, y := 0, width := 1, height := 1 }
      containingBlockRect := { x := 0, y := 0, width := 2, height := 2 } }
#guard encode (ToJSON.toJSON
    ({ stickyBoxRect := { x := 0, y := 0, width := 1, height := 1 }
       containingBlockRect := { x := 0, y := 0, width := 2, height := 2 }
       nearestLayerShiftingStickyBox := some "L1" } : StickyPositionConstraint))
  = "{\"stickyBoxRect\":{\"x\":0,\"y\":0,\"width\":1,\"height\":1},\"containingBlockRect\":{\"x\":0,\"y\":0,\"width\":2,\"height\":2},\"nearestLayerShiftingStickyBox\":\"L1\"}"

#guard decodeAs "{\"x\": 1, \"y\": 2, \"picture\": \"abc\"}" (α := PictureTile)
  = .ok { x := 1, y := 2, picture := "abc" }
#guard encode (ToJSON.toJSON ({ x := 1, y := 2, picture := "abc" } : PictureTile))
  = "{\"x\":1,\"y\":2,\"picture\":\"abc\"}"

#guard decodeAs
    "{\"layerId\": \"1\", \"offsetX\": 0, \"offsetY\": 0, \"width\": 10, \"height\": 10, \"paintCount\": 3, \"drawsContent\": true}"
    (α := Layer)
  = .ok
    { layerId := "1", offsetX := 0, offsetY := 0, width := 10, height := 10
      paintCount := 3, drawsContent := true }
#guard decodeAs
    "{\"layerId\": \"2\", \"parentLayerId\": \"1\", \"offsetX\": 1, \"offsetY\": 1, \"width\": 5, \"height\": 5, \"paintCount\": 0, \"drawsContent\": false}"
    (α := Layer)
  = .ok
    { layerId := "2", parentLayerId := some "1", offsetX := 1, offsetY := 1, width := 5, height := 5
      paintCount := 0, drawsContent := false }
#guard encode (ToJSON.toJSON
    ({ layerId := "1", offsetX := 0, offsetY := 0, width := 10, height := 10
       paintCount := 3, drawsContent := true } : Layer))
  = "{\"layerId\":\"1\",\"offsetX\":0,\"offsetY\":0,\"width\":10,\"height\":10,\"paintCount\":3,\"drawsContent\":true}"

-- ── Events ──

#guard decodeAs "{\"layerId\": \"1\", \"clip\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 1}}"
    (α := LayerPainted)
  = .ok { layerId := "1", clip := { x := 0, y := 0, width := 1, height := 1 } }

#guard decodeAs "{}" (α := LayerTreeDidChange) = .ok { layers := none }
#guard decodeAs
    "{\"layers\": [{\"layerId\": \"1\", \"offsetX\": 0, \"offsetY\": 0, \"width\": 10, \"height\": 10, \"paintCount\": 0, \"drawsContent\": true}]}"
    (α := LayerTreeDidChange)
  = .ok
    { layers := some
        [ { layerId := "1", offsetX := 0, offsetY := 0, width := 10, height := 10
            paintCount := 0, drawsContent := true } ] }

-- ── Commands ──

#guard encode (ToJSON.toJSON ({ layerId := "1" } : PCompositingReasons)) = "{\"layerId\":\"1\"}"
#guard Command.commandName ({ layerId := "1" } : PCompositingReasons) = "LayerTree.compositingReasons"
#guard decodeAs "{\"compositingReasonIds\": [\"a\", \"b\"]}" (α := CompositingReasons)
  = .ok { compositingReasonIds := ["a", "b"] }

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "LayerTree.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "LayerTree.enable"

#guard encode (ToJSON.toJSON ({ tiles := [{ x := 0, y := 0, picture := "p" }] } : PLoadSnapshot))
  = "{\"tiles\":[{\"x\":0,\"y\":0,\"picture\":\"p\"}]}"
#guard Command.commandName ({ tiles := [] } : PLoadSnapshot) = "LayerTree.loadSnapshot"
#guard decodeAs "{\"snapshotId\": \"s1\"}" (α := LoadSnapshot) = .ok { snapshotId := "s1" }

#guard encode (ToJSON.toJSON ({ layerId := "1" } : PMakeSnapshot)) = "{\"layerId\":\"1\"}"
#guard Command.commandName ({ layerId := "1" } : PMakeSnapshot) = "LayerTree.makeSnapshot"
#guard decodeAs "{\"snapshotId\": \"s1\"}" (α := MakeSnapshot) = .ok { snapshotId := "s1" }

#guard encode (ToJSON.toJSON ({ snapshotId := "s1" } : PProfileSnapshot)) = "{\"snapshotId\":\"s1\"}"
#guard encode (ToJSON.toJSON ({ snapshotId := "s1", minRepeatCount := some 5 } : PProfileSnapshot))
  = "{\"snapshotId\":\"s1\",\"minRepeatCount\":5}"
#guard Command.commandName ({ snapshotId := "s1" } : PProfileSnapshot) = "LayerTree.profileSnapshot"
#guard decodeAs "{\"timings\": [[1, 2], [3]]}" (α := ProfileSnapshot)
  = .ok { timings := [[1, 2], [3]] }

#guard encode (ToJSON.toJSON ({ snapshotId := "s1" } : PReleaseSnapshot)) = "{\"snapshotId\":\"s1\"}"
#guard Command.commandName ({ snapshotId := "s1" } : PReleaseSnapshot) = "LayerTree.releaseSnapshot"

#guard encode (ToJSON.toJSON ({ snapshotId := "s1" } : PReplaySnapshot)) = "{\"snapshotId\":\"s1\"}"
#guard encode (ToJSON.toJSON ({ snapshotId := "s1", scale := some 2.5 } : PReplaySnapshot))
  = "{\"snapshotId\":\"s1\",\"scale\":2.500000}"
#guard Command.commandName ({ snapshotId := "s1" } : PReplaySnapshot) = "LayerTree.replaySnapshot"
#guard decodeAs "{\"dataURL\": \"data:...\"}" (α := ReplaySnapshot) = .ok { dataURL := "data:..." }

#guard encode (ToJSON.toJSON ({ snapshotId := "s1" } : PSnapshotCommandLog)) = "{\"snapshotId\":\"s1\"}"
#guard Command.commandName ({ snapshotId := "s1" } : PSnapshotCommandLog) = "LayerTree.snapshotCommandLog"
#guard decodeAs "{\"commandLog\": [[[\"a\", \"b\"]]]}" (α := SnapshotCommandLog)
  = .ok { commandLog := [[("a", "b")]] }

end Tests.CDP.Domains.LayerTree
