/-
  Linen.CDP.Domains.LayerTree — the `LayerTree` CDP domain

  Ports `CDP.Domains.LayerTree` from cdp-hs, which describes the tree of
  compositor layers. Naming conventions as in `CDP.Domains.Memory`'s
  docstring. Cross-domain references to the `DOM`/`Page` domains follow
  `CDP.Domains.Debugger`'s docstring: types are qualified as
  `DOMPageNetworkEmulationSecurity.DOM.Rect`,
  `DOMPageNetworkEmulationSecurity.DOM.BackendNodeId`, …

  Upstream's `LayerTreeLayer` ("`LayerTree.Layer`") looks tree-shaped by name,
  but is genuinely flat: each layer merely carries an *optional
  `parentLayerId`* (`Maybe LayerTreeLayerId`) pointing at its parent, rather
  than nesting its children inline. The `LayerTree.layerTreeDidChange` event
  likewise ships the whole layer set as a flat `[LayerTreeLayer]`. So no
  self-reference exists in this module and no termination proof is needed —
  reconstructing the tree from parent pointers is left to the client, exactly
  as upstream leaves it.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity

namespace CDP.Domains.LayerTree

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)
open CDP.Domains

-- ── Identifiers ──

/-- Unique layer identifier. -/
abbrev LayerId := String

/-- Unique snapshot identifier. -/
abbrev SnapshotId := String

/-- Array of timings, one per paint step. -/
abbrev PaintProfile := List Float

-- ── Types ──

/-- Reason for a `ScrollRect` to force scrolling on the main thread. -/
inductive ScrollRectType where
  | repaintsOnScroll | touchEventHandler | wheelEventHandler
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScrollRectType where
  parseJSON
    | .string "RepaintsOnScroll" => .ok .repaintsOnScroll
    | .string "TouchEventHandler" => .ok .touchEventHandler
    | .string "WheelEventHandler" => .ok .wheelEventHandler
    | v => .error s!"failed to parse ScrollRectType: {repr v}"

instance : ToJSON ScrollRectType where
  toJSON
    | .repaintsOnScroll => .string "RepaintsOnScroll"
    | .touchEventHandler => .string "TouchEventHandler"
    | .wheelEventHandler => .string "WheelEventHandler"

/-- Rectangle where scrolling happens on the main thread. -/
structure ScrollRect where
  /-- Rectangle itself. -/
  rect : DOMPageNetworkEmulationSecurity.DOM.Rect
  /-- Reason for rectangle to force scrolling on the main thread. -/
  type : ScrollRectType
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScrollRect where
  parseJSON v := do
    .ok
      { rect := ← Value.getField v "rect" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON }

instance : ToJSON ScrollRect where
  toJSON p := Data.Json.object [("rect", ToJSON.toJSON p.rect), ("type", ToJSON.toJSON p.type)]

/-- Sticky position constraints. -/
structure StickyPositionConstraint where
  /-- Layout rectangle of the sticky element before being shifted. -/
  stickyBoxRect : DOMPageNetworkEmulationSecurity.DOM.Rect
  /-- Layout rectangle of the containing block of the sticky element. -/
  containingBlockRect : DOMPageNetworkEmulationSecurity.DOM.Rect
  /-- The nearest sticky layer that shifts the sticky box. -/
  nearestLayerShiftingStickyBox : Option LayerId := none
  /-- The nearest sticky layer that shifts the containing block. -/
  nearestLayerShiftingContainingBlock : Option LayerId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON StickyPositionConstraint where
  parseJSON v := do
    .ok
      { stickyBoxRect := ← Value.getField v "stickyBoxRect" >>= FromJSON.parseJSON
        containingBlockRect := ← Value.getField v "containingBlockRect" >>= FromJSON.parseJSON
        nearestLayerShiftingStickyBox :=
          ← (← Value.getFieldOpt v "nearestLayerShiftingStickyBox").mapM FromJSON.parseJSON
        nearestLayerShiftingContainingBlock :=
          ← (← Value.getFieldOpt v "nearestLayerShiftingContainingBlock").mapM FromJSON.parseJSON }

instance : ToJSON StickyPositionConstraint where
  toJSON p := Data.Json.object <|
       [("stickyBoxRect", ToJSON.toJSON p.stickyBoxRect)]
    ++ [("containingBlockRect", ToJSON.toJSON p.containingBlockRect)]
    ++ (p.nearestLayerShiftingStickyBox.map fun v => ("nearestLayerShiftingStickyBox", ToJSON.toJSON v)).toList
    ++ (p.nearestLayerShiftingContainingBlock.map
          fun v => ("nearestLayerShiftingContainingBlock", ToJSON.toJSON v)).toList

/-- Serialized fragment of layer picture along with its offset within the
    layer. -/
structure PictureTile where
  /-- Offset from owning layer left boundary. -/
  x : Float
  /-- Offset from owning layer top boundary. -/
  y : Float
  /-- Base64-encoded snapshot data. -/
  picture : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON PictureTile where
  parseJSON v := do
    .ok
      { x := ← Value.getField v "x" >>= FromJSON.parseJSON
        y := ← Value.getField v "y" >>= FromJSON.parseJSON
        picture := ← Value.getField v "picture" >>= FromJSON.parseJSON }

instance : ToJSON PictureTile where
  toJSON p := Data.Json.object
    [("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y), ("picture", ToJSON.toJSON p.picture)]

/-- Information about a compositing layer. Flat, not self-referential: a
    layer merely carries its parent's id (absent for the root), rather than
    nesting children inline — see the module docstring. -/
structure Layer where
  /-- The unique id for this layer. -/
  layerId : LayerId
  /-- The id of parent (not present for root). -/
  parentLayerId : Option LayerId := none
  /-- The backend id for the node associated with this layer. -/
  backendNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- Offset from parent layer, X coordinate. -/
  offsetX : Float
  /-- Offset from parent layer, Y coordinate. -/
  offsetY : Float
  /-- Layer width. -/
  width : Float
  /-- Layer height. -/
  height : Float
  /-- Transformation matrix for layer, default is identity matrix. -/
  transform : Option (List Float) := none
  /-- Transform anchor point X, absent if no transform specified. -/
  anchorX : Option Float := none
  /-- Transform anchor point Y, absent if no transform specified. -/
  anchorY : Option Float := none
  /-- Transform anchor point Z, absent if no transform specified. -/
  anchorZ : Option Float := none
  /-- Indicates how many time this layer has painted. -/
  paintCount : Int
  /-- Indicates whether this layer hosts any content, rather than being used
      for transform/scrolling purposes only. -/
  drawsContent : Bool
  /-- Set if layer is not visible. -/
  invisible : Option Bool := none
  /-- Rectangles scrolling on main thread only. -/
  scrollRects : Option (List ScrollRect) := none
  /-- Sticky position constraint information. -/
  stickyPositionConstraint : Option StickyPositionConstraint := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Layer where
  parseJSON v := do
    .ok
      { layerId := ← Value.getField v "layerId" >>= FromJSON.parseJSON
        parentLayerId := ← (← Value.getFieldOpt v "parentLayerId").mapM FromJSON.parseJSON
        backendNodeId := ← (← Value.getFieldOpt v "backendNodeId").mapM FromJSON.parseJSON
        offsetX := ← Value.getField v "offsetX" >>= FromJSON.parseJSON
        offsetY := ← Value.getField v "offsetY" >>= FromJSON.parseJSON
        width := ← Value.getField v "width" >>= FromJSON.parseJSON
        height := ← Value.getField v "height" >>= FromJSON.parseJSON
        transform := ← (← Value.getFieldOpt v "transform").mapM FromJSON.parseJSON
        anchorX := ← (← Value.getFieldOpt v "anchorX").mapM FromJSON.parseJSON
        anchorY := ← (← Value.getFieldOpt v "anchorY").mapM FromJSON.parseJSON
        anchorZ := ← (← Value.getFieldOpt v "anchorZ").mapM FromJSON.parseJSON
        paintCount := ← Value.getField v "paintCount" >>= FromJSON.parseJSON
        drawsContent := ← Value.getField v "drawsContent" >>= FromJSON.parseJSON
        invisible := ← (← Value.getFieldOpt v "invisible").mapM FromJSON.parseJSON
        scrollRects := ← (← Value.getFieldOpt v "scrollRects").mapM FromJSON.parseJSON
        stickyPositionConstraint :=
          ← (← Value.getFieldOpt v "stickyPositionConstraint").mapM FromJSON.parseJSON }

instance : ToJSON Layer where
  toJSON p := Data.Json.object <|
       [("layerId", ToJSON.toJSON p.layerId)]
    ++ (p.parentLayerId.map fun v => ("parentLayerId", ToJSON.toJSON v)).toList
    ++ (p.backendNodeId.map fun v => ("backendNodeId", ToJSON.toJSON v)).toList
    ++ [("offsetX", ToJSON.toJSON p.offsetX)]
    ++ [("offsetY", ToJSON.toJSON p.offsetY)]
    ++ [("width", ToJSON.toJSON p.width)]
    ++ [("height", ToJSON.toJSON p.height)]
    ++ (p.transform.map fun v => ("transform", ToJSON.toJSON v)).toList
    ++ (p.anchorX.map fun v => ("anchorX", ToJSON.toJSON v)).toList
    ++ (p.anchorY.map fun v => ("anchorY", ToJSON.toJSON v)).toList
    ++ (p.anchorZ.map fun v => ("anchorZ", ToJSON.toJSON v)).toList
    ++ [("paintCount", ToJSON.toJSON p.paintCount)]
    ++ [("drawsContent", ToJSON.toJSON p.drawsContent)]
    ++ (p.invisible.map fun v => ("invisible", ToJSON.toJSON v)).toList
    ++ (p.scrollRects.map fun v => ("scrollRects", ToJSON.toJSON v)).toList
    ++ (p.stickyPositionConstraint.map fun v => ("stickyPositionConstraint", ToJSON.toJSON v)).toList

-- ── Events ──

/-- The `LayerTree.layerPainted` event. -/
structure LayerPainted where
  /-- The id of the painted layer. -/
  layerId : LayerId
  /-- Clip rectangle. -/
  clip : DOMPageNetworkEmulationSecurity.DOM.Rect
  deriving Repr, BEq, DecidableEq

instance : FromJSON LayerPainted where
  parseJSON v := do
    .ok
      { layerId := ← Value.getField v "layerId" >>= FromJSON.parseJSON
        clip := ← Value.getField v "clip" >>= FromJSON.parseJSON }

instance : Event LayerPainted where
  eventName := "LayerTree.layerPainted"

/-- The `LayerTree.layerTreeDidChange` event. -/
structure LayerTreeDidChange where
  /-- Layer tree, absent if not in the compositing mode. -/
  layers : Option (List Layer) := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON LayerTreeDidChange where
  parseJSON v := do
    .ok { layers := ← (← Value.getFieldOpt v "layers").mapM FromJSON.parseJSON }

instance : Event LayerTreeDidChange where
  eventName := "LayerTree.layerTreeDidChange"

-- ── Commands ──

/-- Parameters of the `LayerTree.compositingReasons` command: provides the
    reasons why the given layer was composited. -/
structure PCompositingReasons where
  /-- The id of the layer for which we want to get the reasons it was
      composited. -/
  layerId : LayerId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PCompositingReasons where
  toJSON p := Data.Json.object [("layerId", ToJSON.toJSON p.layerId)]

/-- Response of the `LayerTree.compositingReasons` command. -/
structure CompositingReasons where
  /-- A list of strings specifying reason IDs for the given layer to become
      composited. -/
  compositingReasonIds : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON CompositingReasons where
  parseJSON v := do
    .ok { compositingReasonIds := ← Value.getField v "compositingReasonIds" >>= FromJSON.parseJSON }

instance : Command PCompositingReasons where
  Response := CompositingReasons
  commandName _ := "LayerTree.compositingReasons"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `LayerTree.disable` command: disables compositing tree
    inspection. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "LayerTree.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `LayerTree.enable` command: enables compositing tree
    inspection. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "LayerTree.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `LayerTree.loadSnapshot` command: returns the snapshot
    identifier. -/
structure PLoadSnapshot where
  /-- An array of tiles composing the snapshot. -/
  tiles : List PictureTile
  deriving Repr, BEq, DecidableEq

instance : ToJSON PLoadSnapshot where
  toJSON p := Data.Json.object [("tiles", ToJSON.toJSON p.tiles)]

/-- Response of the `LayerTree.loadSnapshot` command. -/
structure LoadSnapshot where
  /-- The id of the snapshot. -/
  snapshotId : SnapshotId
  deriving Repr, BEq, DecidableEq

instance : FromJSON LoadSnapshot where
  parseJSON v := do .ok { snapshotId := ← Value.getField v "snapshotId" >>= FromJSON.parseJSON }

instance : Command PLoadSnapshot where
  Response := LoadSnapshot
  commandName _ := "LayerTree.loadSnapshot"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `LayerTree.makeSnapshot` command: returns the layer
    snapshot identifier. -/
structure PMakeSnapshot where
  /-- The id of the layer. -/
  layerId : LayerId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PMakeSnapshot where
  toJSON p := Data.Json.object [("layerId", ToJSON.toJSON p.layerId)]

/-- Response of the `LayerTree.makeSnapshot` command. -/
structure MakeSnapshot where
  /-- The id of the layer snapshot. -/
  snapshotId : SnapshotId
  deriving Repr, BEq, DecidableEq

instance : FromJSON MakeSnapshot where
  parseJSON v := do .ok { snapshotId := ← Value.getField v "snapshotId" >>= FromJSON.parseJSON }

instance : Command PMakeSnapshot where
  Response := MakeSnapshot
  commandName _ := "LayerTree.makeSnapshot"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `LayerTree.profileSnapshot` command. -/
structure PProfileSnapshot where
  /-- The id of the layer snapshot. -/
  snapshotId : SnapshotId
  /-- The maximum number of times to replay the snapshot (1, if not
      specified). -/
  minRepeatCount : Option Int := none
  /-- The minimum duration (in seconds) to replay the snapshot. -/
  minDuration : Option Float := none
  /-- The clip rectangle to apply when replaying the snapshot. -/
  clipRect : Option DOMPageNetworkEmulationSecurity.DOM.Rect := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PProfileSnapshot where
  toJSON p := Data.Json.object <|
       [("snapshotId", ToJSON.toJSON p.snapshotId)]
    ++ (p.minRepeatCount.map fun v => ("minRepeatCount", ToJSON.toJSON v)).toList
    ++ (p.minDuration.map fun v => ("minDuration", ToJSON.toJSON v)).toList
    ++ (p.clipRect.map fun v => ("clipRect", ToJSON.toJSON v)).toList

/-- Response of the `LayerTree.profileSnapshot` command. -/
structure ProfileSnapshot where
  /-- The array of paint profiles, one per run. -/
  timings : List PaintProfile
  deriving Repr, BEq, DecidableEq

instance : FromJSON ProfileSnapshot where
  parseJSON v := do .ok { timings := ← Value.getField v "timings" >>= FromJSON.parseJSON }

instance : Command PProfileSnapshot where
  Response := ProfileSnapshot
  commandName _ := "LayerTree.profileSnapshot"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `LayerTree.releaseSnapshot` command: releases layer
    snapshot captured by the back-end. -/
structure PReleaseSnapshot where
  /-- The id of the layer snapshot. -/
  snapshotId : SnapshotId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PReleaseSnapshot where
  toJSON p := Data.Json.object [("snapshotId", ToJSON.toJSON p.snapshotId)]

instance : Command PReleaseSnapshot where
  Response := Unit
  commandName _ := "LayerTree.releaseSnapshot"
  decodeResponse _ := .ok ()

/-- Parameters of the `LayerTree.replaySnapshot` command: replays the layer
    snapshot and returns the resulting bitmap. -/
structure PReplaySnapshot where
  /-- The id of the layer snapshot. -/
  snapshotId : SnapshotId
  /-- The first step to replay from (replay from the very start if not
      specified). -/
  fromStep : Option Int := none
  /-- The last step to replay to (replay till the end if not specified). -/
  toStep : Option Int := none
  /-- The scale to apply while replaying (defaults to 1). -/
  scale : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PReplaySnapshot where
  toJSON p := Data.Json.object <|
       [("snapshotId", ToJSON.toJSON p.snapshotId)]
    ++ (p.fromStep.map fun v => ("fromStep", ToJSON.toJSON v)).toList
    ++ (p.toStep.map fun v => ("toStep", ToJSON.toJSON v)).toList
    ++ (p.scale.map fun v => ("scale", ToJSON.toJSON v)).toList

/-- Response of the `LayerTree.replaySnapshot` command. -/
structure ReplaySnapshot where
  /-- A data: URL for resulting image. -/
  dataURL : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON ReplaySnapshot where
  parseJSON v := do .ok { dataURL := ← Value.getField v "dataURL" >>= FromJSON.parseJSON }

instance : Command PReplaySnapshot where
  Response := ReplaySnapshot
  commandName _ := "LayerTree.replaySnapshot"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `LayerTree.snapshotCommandLog` command: replays the
    layer snapshot and returns canvas log. -/
structure PSnapshotCommandLog where
  /-- The id of the layer snapshot. -/
  snapshotId : SnapshotId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSnapshotCommandLog where
  toJSON p := Data.Json.object [("snapshotId", ToJSON.toJSON p.snapshotId)]

/-- Response of the `LayerTree.snapshotCommandLog` command. -/
structure SnapshotCommandLog where
  /-- The array of canvas function calls. -/
  commandLog : List (List (String × String))
  deriving Repr, BEq, DecidableEq

instance : FromJSON SnapshotCommandLog where
  parseJSON v := do .ok { commandLog := ← Value.getField v "commandLog" >>= FromJSON.parseJSON }

instance : Command PSnapshotCommandLog where
  Response := SnapshotCommandLog
  commandName _ := "LayerTree.snapshotCommandLog"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.LayerTree
