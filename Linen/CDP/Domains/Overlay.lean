/-
  Linen.CDP.Domains.Overlay — the `Overlay` CDP domain

  Ports `CDP.Domains.Overlay` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.Memory`'s docstring. Types are emitted in
  topological order (leaf configuration records before the `HighlightConfig`
  / node-highlight-config records that embed them). References to the
  `DOM`/`Page` domains use `CDP.Domains.DOMPageNetworkEmulationSecurity`'s
  nested namespaces (`DOM.NodeId`, `DOM.RGBA`, `Page.Viewport`, …); references
  to `Runtime` use `CDP.Domains.Runtime`. The `highlight`/`highlights` fields
  of the `*ForTest` command responses are upstream's opaque `[(Text, Text)]`
  placeholder for a generic CDP `object` value — ported as `List (String ×
  String)` via the generic `List`/`Prod` JSON instances, matching
  `CDP.Domains.Runtime`'s `hints`/`auxData` and `CDP.Domains.Media`'s `data`.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.Overlay

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Configuration types ──

/-- `Overlay.SourceOrderConfig`. Configuration data for drawing the source
    order of an element's children. -/
structure SourceOrderConfig where
  /-- The color to outline the given element in. -/
  parentOutlineColor : DOMPageNetworkEmulationSecurity.DOM.RGBA
  /-- The color to outline the child elements in. -/
  childOutlineColor : DOMPageNetworkEmulationSecurity.DOM.RGBA
  deriving Repr, BEq, DecidableEq

instance : FromJSON SourceOrderConfig where
  parseJSON v := do
    .ok
      { parentOutlineColor := ← Value.getField v "parentOutlineColor" >>= FromJSON.parseJSON
        childOutlineColor := ← Value.getField v "childOutlineColor" >>= FromJSON.parseJSON }

instance : ToJSON SourceOrderConfig where
  toJSON p := Data.Json.object
    [ ("parentOutlineColor", ToJSON.toJSON p.parentOutlineColor)
    , ("childOutlineColor", ToJSON.toJSON p.childOutlineColor) ]

/-- `Overlay.GridHighlightConfig`. Configuration data for the highlighting of
    Grid elements. -/
structure GridHighlightConfig where
  /-- Whether the extension lines from grid cells to the rulers should be
      shown (default: false). -/
  showGridExtensionLines : Option Bool := none
  /-- Show positive line number labels (default: false). -/
  showPositiveLineNumbers : Option Bool := none
  /-- Show negative line number labels (default: false). -/
  showNegativeLineNumbers : Option Bool := none
  /-- Show area name labels (default: false). -/
  showAreaNames : Option Bool := none
  /-- Show line name labels (default: false). -/
  showLineNames : Option Bool := none
  /-- Show track size labels (default: false). -/
  showTrackSizes : Option Bool := none
  /-- The grid container border highlight color (default: transparent). -/
  gridBorderColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The row line color (default: transparent). -/
  rowLineColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The column line color (default: transparent). -/
  columnLineColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- Whether the grid border is dashed (default: false). -/
  gridBorderDash : Option Bool := none
  /-- Whether row lines are dashed (default: false). -/
  rowLineDash : Option Bool := none
  /-- Whether column lines are dashed (default: false). -/
  columnLineDash : Option Bool := none
  /-- The row gap highlight fill color (default: transparent). -/
  rowGapColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The row gap hatching fill color (default: transparent). -/
  rowHatchColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The column gap highlight fill color (default: transparent). -/
  columnGapColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The column gap hatching fill color (default: transparent). -/
  columnHatchColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The named grid areas border color (default: transparent). -/
  areaBorderColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The grid container background color (default: transparent). -/
  gridBackgroundColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON GridHighlightConfig where
  parseJSON v := do
    .ok
      { showGridExtensionLines := ← (← Value.getFieldOpt v "showGridExtensionLines").mapM FromJSON.parseJSON
        showPositiveLineNumbers := ← (← Value.getFieldOpt v "showPositiveLineNumbers").mapM FromJSON.parseJSON
        showNegativeLineNumbers := ← (← Value.getFieldOpt v "showNegativeLineNumbers").mapM FromJSON.parseJSON
        showAreaNames := ← (← Value.getFieldOpt v "showAreaNames").mapM FromJSON.parseJSON
        showLineNames := ← (← Value.getFieldOpt v "showLineNames").mapM FromJSON.parseJSON
        showTrackSizes := ← (← Value.getFieldOpt v "showTrackSizes").mapM FromJSON.parseJSON
        gridBorderColor := ← (← Value.getFieldOpt v "gridBorderColor").mapM FromJSON.parseJSON
        rowLineColor := ← (← Value.getFieldOpt v "rowLineColor").mapM FromJSON.parseJSON
        columnLineColor := ← (← Value.getFieldOpt v "columnLineColor").mapM FromJSON.parseJSON
        gridBorderDash := ← (← Value.getFieldOpt v "gridBorderDash").mapM FromJSON.parseJSON
        rowLineDash := ← (← Value.getFieldOpt v "rowLineDash").mapM FromJSON.parseJSON
        columnLineDash := ← (← Value.getFieldOpt v "columnLineDash").mapM FromJSON.parseJSON
        rowGapColor := ← (← Value.getFieldOpt v "rowGapColor").mapM FromJSON.parseJSON
        rowHatchColor := ← (← Value.getFieldOpt v "rowHatchColor").mapM FromJSON.parseJSON
        columnGapColor := ← (← Value.getFieldOpt v "columnGapColor").mapM FromJSON.parseJSON
        columnHatchColor := ← (← Value.getFieldOpt v "columnHatchColor").mapM FromJSON.parseJSON
        areaBorderColor := ← (← Value.getFieldOpt v "areaBorderColor").mapM FromJSON.parseJSON
        gridBackgroundColor := ← (← Value.getFieldOpt v "gridBackgroundColor").mapM FromJSON.parseJSON }

instance : ToJSON GridHighlightConfig where
  toJSON p := Data.Json.object <|
       (p.showGridExtensionLines.map fun v => ("showGridExtensionLines", ToJSON.toJSON v)).toList
    ++ (p.showPositiveLineNumbers.map fun v => ("showPositiveLineNumbers", ToJSON.toJSON v)).toList
    ++ (p.showNegativeLineNumbers.map fun v => ("showNegativeLineNumbers", ToJSON.toJSON v)).toList
    ++ (p.showAreaNames.map fun v => ("showAreaNames", ToJSON.toJSON v)).toList
    ++ (p.showLineNames.map fun v => ("showLineNames", ToJSON.toJSON v)).toList
    ++ (p.showTrackSizes.map fun v => ("showTrackSizes", ToJSON.toJSON v)).toList
    ++ (p.gridBorderColor.map fun v => ("gridBorderColor", ToJSON.toJSON v)).toList
    ++ (p.rowLineColor.map fun v => ("rowLineColor", ToJSON.toJSON v)).toList
    ++ (p.columnLineColor.map fun v => ("columnLineColor", ToJSON.toJSON v)).toList
    ++ (p.gridBorderDash.map fun v => ("gridBorderDash", ToJSON.toJSON v)).toList
    ++ (p.rowLineDash.map fun v => ("rowLineDash", ToJSON.toJSON v)).toList
    ++ (p.columnLineDash.map fun v => ("columnLineDash", ToJSON.toJSON v)).toList
    ++ (p.rowGapColor.map fun v => ("rowGapColor", ToJSON.toJSON v)).toList
    ++ (p.rowHatchColor.map fun v => ("rowHatchColor", ToJSON.toJSON v)).toList
    ++ (p.columnGapColor.map fun v => ("columnGapColor", ToJSON.toJSON v)).toList
    ++ (p.columnHatchColor.map fun v => ("columnHatchColor", ToJSON.toJSON v)).toList
    ++ (p.areaBorderColor.map fun v => ("areaBorderColor", ToJSON.toJSON v)).toList
    ++ (p.gridBackgroundColor.map fun v => ("gridBackgroundColor", ToJSON.toJSON v)).toList

/-- `Overlay.LineStyle`'s line pattern. -/
inductive LineStylePattern where
  | dashed | dotted
  deriving Repr, BEq, DecidableEq

instance : FromJSON LineStylePattern where
  parseJSON
    | .string "dashed" => .ok .dashed
    | .string "dotted" => .ok .dotted
    | v => .error s!"failed to parse LineStylePattern: {repr v}"

instance : ToJSON LineStylePattern where
  toJSON | .dashed => .string "dashed" | .dotted => .string "dotted"

/-- `Overlay.LineStyle`. Style information for drawing a line. -/
structure LineStyle where
  /-- The color of the line (default: transparent). -/
  color : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The line pattern (default: solid). -/
  pattern : Option LineStylePattern := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON LineStyle where
  parseJSON v := do
    .ok
      { color := ← (← Value.getFieldOpt v "color").mapM FromJSON.parseJSON
        pattern := ← (← Value.getFieldOpt v "pattern").mapM FromJSON.parseJSON }

instance : ToJSON LineStyle where
  toJSON p := Data.Json.object <|
       (p.color.map fun v => ("color", ToJSON.toJSON v)).toList
    ++ (p.pattern.map fun v => ("pattern", ToJSON.toJSON v)).toList

/-- `Overlay.BoxStyle`. Style information for drawing a box. -/
structure BoxStyle where
  /-- The background color for the box (default: transparent). -/
  fillColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The hatching color for the box (default: transparent). -/
  hatchColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON BoxStyle where
  parseJSON v := do
    .ok
      { fillColor := ← (← Value.getFieldOpt v "fillColor").mapM FromJSON.parseJSON
        hatchColor := ← (← Value.getFieldOpt v "hatchColor").mapM FromJSON.parseJSON }

instance : ToJSON BoxStyle where
  toJSON p := Data.Json.object <|
       (p.fillColor.map fun v => ("fillColor", ToJSON.toJSON v)).toList
    ++ (p.hatchColor.map fun v => ("hatchColor", ToJSON.toJSON v)).toList

/-- `Overlay.FlexContainerHighlightConfig`. Configuration data for the
    highlighting of Flex container elements. -/
structure FlexContainerHighlightConfig where
  /-- The style of the container border. -/
  containerBorder : Option LineStyle := none
  /-- The style of the separator between lines. -/
  lineSeparator : Option LineStyle := none
  /-- The style of the separator between items. -/
  itemSeparator : Option LineStyle := none
  /-- Style of content-distribution space on the main axis
      (`justify-content`). -/
  mainDistributedSpace : Option BoxStyle := none
  /-- Style of content-distribution space on the cross axis
      (`align-content`). -/
  crossDistributedSpace : Option BoxStyle := none
  /-- Style of empty space caused by row gaps (`gap`/`row-gap`). -/
  rowGapSpace : Option BoxStyle := none
  /-- Style of empty space caused by column gaps (`gap`/`column-gap`). -/
  columnGapSpace : Option BoxStyle := none
  /-- Style of the self-alignment line (`align-items`). -/
  crossAlignment : Option LineStyle := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON FlexContainerHighlightConfig where
  parseJSON v := do
    .ok
      { containerBorder := ← (← Value.getFieldOpt v "containerBorder").mapM FromJSON.parseJSON
        lineSeparator := ← (← Value.getFieldOpt v "lineSeparator").mapM FromJSON.parseJSON
        itemSeparator := ← (← Value.getFieldOpt v "itemSeparator").mapM FromJSON.parseJSON
        mainDistributedSpace := ← (← Value.getFieldOpt v "mainDistributedSpace").mapM FromJSON.parseJSON
        crossDistributedSpace := ← (← Value.getFieldOpt v "crossDistributedSpace").mapM FromJSON.parseJSON
        rowGapSpace := ← (← Value.getFieldOpt v "rowGapSpace").mapM FromJSON.parseJSON
        columnGapSpace := ← (← Value.getFieldOpt v "columnGapSpace").mapM FromJSON.parseJSON
        crossAlignment := ← (← Value.getFieldOpt v "crossAlignment").mapM FromJSON.parseJSON }

instance : ToJSON FlexContainerHighlightConfig where
  toJSON p := Data.Json.object <|
       (p.containerBorder.map fun v => ("containerBorder", ToJSON.toJSON v)).toList
    ++ (p.lineSeparator.map fun v => ("lineSeparator", ToJSON.toJSON v)).toList
    ++ (p.itemSeparator.map fun v => ("itemSeparator", ToJSON.toJSON v)).toList
    ++ (p.mainDistributedSpace.map fun v => ("mainDistributedSpace", ToJSON.toJSON v)).toList
    ++ (p.crossDistributedSpace.map fun v => ("crossDistributedSpace", ToJSON.toJSON v)).toList
    ++ (p.rowGapSpace.map fun v => ("rowGapSpace", ToJSON.toJSON v)).toList
    ++ (p.columnGapSpace.map fun v => ("columnGapSpace", ToJSON.toJSON v)).toList
    ++ (p.crossAlignment.map fun v => ("crossAlignment", ToJSON.toJSON v)).toList

/-- `Overlay.FlexItemHighlightConfig`. -/
structure FlexItemHighlightConfig where
  /-- Style of the box representing the item's base size. -/
  baseSizeBox : Option BoxStyle := none
  /-- Style of the border around the box representing the item's base
      size. -/
  baseSizeBorder : Option LineStyle := none
  /-- Style of the arrow representing if the item grew or shrank. -/
  flexibilityArrow : Option LineStyle := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON FlexItemHighlightConfig where
  parseJSON v := do
    .ok
      { baseSizeBox := ← (← Value.getFieldOpt v "baseSizeBox").mapM FromJSON.parseJSON
        baseSizeBorder := ← (← Value.getFieldOpt v "baseSizeBorder").mapM FromJSON.parseJSON
        flexibilityArrow := ← (← Value.getFieldOpt v "flexibilityArrow").mapM FromJSON.parseJSON }

instance : ToJSON FlexItemHighlightConfig where
  toJSON p := Data.Json.object <|
       (p.baseSizeBox.map fun v => ("baseSizeBox", ToJSON.toJSON v)).toList
    ++ (p.baseSizeBorder.map fun v => ("baseSizeBorder", ToJSON.toJSON v)).toList
    ++ (p.flexibilityArrow.map fun v => ("flexibilityArrow", ToJSON.toJSON v)).toList

/-- `Overlay.ContrastAlgorithm`. -/
inductive ContrastAlgorithm where
  | aa | aaa | apca
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContrastAlgorithm where
  parseJSON
    | .string "aa" => .ok .aa
    | .string "aaa" => .ok .aaa
    | .string "apca" => .ok .apca
    | v => .error s!"failed to parse ContrastAlgorithm: {repr v}"

instance : ToJSON ContrastAlgorithm where
  toJSON | .aa => .string "aa" | .aaa => .string "aaa" | .apca => .string "apca"

/-- `Overlay.ColorFormat`. -/
inductive ColorFormat where
  | rgb | hsl | hwb | hex
  deriving Repr, BEq, DecidableEq

instance : FromJSON ColorFormat where
  parseJSON
    | .string "rgb" => .ok .rgb
    | .string "hsl" => .ok .hsl
    | .string "hwb" => .ok .hwb
    | .string "hex" => .ok .hex
    | v => .error s!"failed to parse ColorFormat: {repr v}"

instance : ToJSON ColorFormat where
  toJSON | .rgb => .string "rgb" | .hsl => .string "hsl" | .hwb => .string "hwb" | .hex => .string "hex"

/-- `Overlay.ContainerQueryContainerHighlightConfig`. -/
structure ContainerQueryContainerHighlightConfig where
  /-- The style of the container border. -/
  containerBorder : Option LineStyle := none
  /-- The style of the descendants' borders. -/
  descendantBorder : Option LineStyle := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContainerQueryContainerHighlightConfig where
  parseJSON v := do
    .ok
      { containerBorder := ← (← Value.getFieldOpt v "containerBorder").mapM FromJSON.parseJSON
        descendantBorder := ← (← Value.getFieldOpt v "descendantBorder").mapM FromJSON.parseJSON }

instance : ToJSON ContainerQueryContainerHighlightConfig where
  toJSON p := Data.Json.object <|
       (p.containerBorder.map fun v => ("containerBorder", ToJSON.toJSON v)).toList
    ++ (p.descendantBorder.map fun v => ("descendantBorder", ToJSON.toJSON v)).toList

/-- `Overlay.HighlightConfig`. Configuration data for the highlighting of page
    elements. -/
structure HighlightConfig where
  /-- Whether the node info tooltip should be shown (default: false). -/
  showInfo : Option Bool := none
  /-- Whether the node styles in the tooltip (default: false). -/
  showStyles : Option Bool := none
  /-- Whether the rulers should be shown (default: false). -/
  showRulers : Option Bool := none
  /-- Whether the a11y info should be shown (default: true). -/
  showAccessibilityInfo : Option Bool := none
  /-- Whether the extension lines from node to the rulers should be shown
      (default: false). -/
  showExtensionLines : Option Bool := none
  /-- The content box highlight fill color (default: transparent). -/
  contentColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The padding highlight fill color (default: transparent). -/
  paddingColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The border highlight fill color (default: transparent). -/
  borderColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The margin highlight fill color (default: transparent). -/
  marginColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The event target element highlight fill color (default: transparent). -/
  eventTargetColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The shape outside fill color (default: transparent). -/
  shapeColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The shape margin fill color (default: transparent). -/
  shapeMarginColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The grid layout color (default: transparent). -/
  cssGridColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The color format used to format color styles (default: hex). -/
  colorFormat : Option ColorFormat := none
  /-- The grid layout highlight configuration (default: all transparent). -/
  gridHighlightConfig : Option GridHighlightConfig := none
  /-- The flex container highlight configuration (default: all
      transparent). -/
  flexContainerHighlightConfig : Option FlexContainerHighlightConfig := none
  /-- The flex item highlight configuration (default: all transparent). -/
  flexItemHighlightConfig : Option FlexItemHighlightConfig := none
  /-- The contrast algorithm to use for the contrast ratio (default: aa). -/
  contrastAlgorithm : Option ContrastAlgorithm := none
  /-- The container query container highlight configuration (default: all
      transparent). -/
  containerQueryContainerHighlightConfig : Option ContainerQueryContainerHighlightConfig := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON HighlightConfig where
  parseJSON v := do
    .ok
      { showInfo := ← (← Value.getFieldOpt v "showInfo").mapM FromJSON.parseJSON
        showStyles := ← (← Value.getFieldOpt v "showStyles").mapM FromJSON.parseJSON
        showRulers := ← (← Value.getFieldOpt v "showRulers").mapM FromJSON.parseJSON
        showAccessibilityInfo := ← (← Value.getFieldOpt v "showAccessibilityInfo").mapM FromJSON.parseJSON
        showExtensionLines := ← (← Value.getFieldOpt v "showExtensionLines").mapM FromJSON.parseJSON
        contentColor := ← (← Value.getFieldOpt v "contentColor").mapM FromJSON.parseJSON
        paddingColor := ← (← Value.getFieldOpt v "paddingColor").mapM FromJSON.parseJSON
        borderColor := ← (← Value.getFieldOpt v "borderColor").mapM FromJSON.parseJSON
        marginColor := ← (← Value.getFieldOpt v "marginColor").mapM FromJSON.parseJSON
        eventTargetColor := ← (← Value.getFieldOpt v "eventTargetColor").mapM FromJSON.parseJSON
        shapeColor := ← (← Value.getFieldOpt v "shapeColor").mapM FromJSON.parseJSON
        shapeMarginColor := ← (← Value.getFieldOpt v "shapeMarginColor").mapM FromJSON.parseJSON
        cssGridColor := ← (← Value.getFieldOpt v "cssGridColor").mapM FromJSON.parseJSON
        colorFormat := ← (← Value.getFieldOpt v "colorFormat").mapM FromJSON.parseJSON
        gridHighlightConfig := ← (← Value.getFieldOpt v "gridHighlightConfig").mapM FromJSON.parseJSON
        flexContainerHighlightConfig := ← (← Value.getFieldOpt v "flexContainerHighlightConfig").mapM FromJSON.parseJSON
        flexItemHighlightConfig := ← (← Value.getFieldOpt v "flexItemHighlightConfig").mapM FromJSON.parseJSON
        contrastAlgorithm := ← (← Value.getFieldOpt v "contrastAlgorithm").mapM FromJSON.parseJSON
        containerQueryContainerHighlightConfig := ← (← Value.getFieldOpt v "containerQueryContainerHighlightConfig").mapM FromJSON.parseJSON }

instance : ToJSON HighlightConfig where
  toJSON p := Data.Json.object <|
       (p.showInfo.map fun v => ("showInfo", ToJSON.toJSON v)).toList
    ++ (p.showStyles.map fun v => ("showStyles", ToJSON.toJSON v)).toList
    ++ (p.showRulers.map fun v => ("showRulers", ToJSON.toJSON v)).toList
    ++ (p.showAccessibilityInfo.map fun v => ("showAccessibilityInfo", ToJSON.toJSON v)).toList
    ++ (p.showExtensionLines.map fun v => ("showExtensionLines", ToJSON.toJSON v)).toList
    ++ (p.contentColor.map fun v => ("contentColor", ToJSON.toJSON v)).toList
    ++ (p.paddingColor.map fun v => ("paddingColor", ToJSON.toJSON v)).toList
    ++ (p.borderColor.map fun v => ("borderColor", ToJSON.toJSON v)).toList
    ++ (p.marginColor.map fun v => ("marginColor", ToJSON.toJSON v)).toList
    ++ (p.eventTargetColor.map fun v => ("eventTargetColor", ToJSON.toJSON v)).toList
    ++ (p.shapeColor.map fun v => ("shapeColor", ToJSON.toJSON v)).toList
    ++ (p.shapeMarginColor.map fun v => ("shapeMarginColor", ToJSON.toJSON v)).toList
    ++ (p.cssGridColor.map fun v => ("cssGridColor", ToJSON.toJSON v)).toList
    ++ (p.colorFormat.map fun v => ("colorFormat", ToJSON.toJSON v)).toList
    ++ (p.gridHighlightConfig.map fun v => ("gridHighlightConfig", ToJSON.toJSON v)).toList
    ++ (p.flexContainerHighlightConfig.map fun v => ("flexContainerHighlightConfig", ToJSON.toJSON v)).toList
    ++ (p.flexItemHighlightConfig.map fun v => ("flexItemHighlightConfig", ToJSON.toJSON v)).toList
    ++ (p.contrastAlgorithm.map fun v => ("contrastAlgorithm", ToJSON.toJSON v)).toList
    ++ (p.containerQueryContainerHighlightConfig.map fun v => ("containerQueryContainerHighlightConfig", ToJSON.toJSON v)).toList

/-- `Overlay.GridNodeHighlightConfig`. Configurations for Persistent Grid
    Highlight. -/
structure GridNodeHighlightConfig where
  /-- A descriptor for the highlight appearance. -/
  gridHighlightConfig : GridHighlightConfig
  /-- Identifier of the node to highlight. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : FromJSON GridNodeHighlightConfig where
  parseJSON v := do
    .ok
      { gridHighlightConfig := ← Value.getField v "gridHighlightConfig" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

instance : ToJSON GridNodeHighlightConfig where
  toJSON p := Data.Json.object
    [ ("gridHighlightConfig", ToJSON.toJSON p.gridHighlightConfig)
    , ("nodeId", ToJSON.toJSON p.nodeId) ]

/-- `Overlay.FlexNodeHighlightConfig`. -/
structure FlexNodeHighlightConfig where
  /-- A descriptor for the highlight appearance of flex containers. -/
  flexContainerHighlightConfig : FlexContainerHighlightConfig
  /-- Identifier of the node to highlight. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : FromJSON FlexNodeHighlightConfig where
  parseJSON v := do
    .ok
      { flexContainerHighlightConfig := ← Value.getField v "flexContainerHighlightConfig" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

instance : ToJSON FlexNodeHighlightConfig where
  toJSON p := Data.Json.object
    [ ("flexContainerHighlightConfig", ToJSON.toJSON p.flexContainerHighlightConfig)
    , ("nodeId", ToJSON.toJSON p.nodeId) ]

/-- `Overlay.ScrollSnapContainerHighlightConfig`. -/
structure ScrollSnapContainerHighlightConfig where
  /-- The style of the snapport border (default: transparent). -/
  snapportBorder : Option LineStyle := none
  /-- The style of the snap area border (default: transparent). -/
  snapAreaBorder : Option LineStyle := none
  /-- The margin highlight fill color (default: transparent). -/
  scrollMarginColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The padding highlight fill color (default: transparent). -/
  scrollPaddingColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScrollSnapContainerHighlightConfig where
  parseJSON v := do
    .ok
      { snapportBorder := ← (← Value.getFieldOpt v "snapportBorder").mapM FromJSON.parseJSON
        snapAreaBorder := ← (← Value.getFieldOpt v "snapAreaBorder").mapM FromJSON.parseJSON
        scrollMarginColor := ← (← Value.getFieldOpt v "scrollMarginColor").mapM FromJSON.parseJSON
        scrollPaddingColor := ← (← Value.getFieldOpt v "scrollPaddingColor").mapM FromJSON.parseJSON }

instance : ToJSON ScrollSnapContainerHighlightConfig where
  toJSON p := Data.Json.object <|
       (p.snapportBorder.map fun v => ("snapportBorder", ToJSON.toJSON v)).toList
    ++ (p.snapAreaBorder.map fun v => ("snapAreaBorder", ToJSON.toJSON v)).toList
    ++ (p.scrollMarginColor.map fun v => ("scrollMarginColor", ToJSON.toJSON v)).toList
    ++ (p.scrollPaddingColor.map fun v => ("scrollPaddingColor", ToJSON.toJSON v)).toList

/-- `Overlay.ScrollSnapHighlightConfig`. -/
structure ScrollSnapHighlightConfig where
  /-- A descriptor for the highlight appearance of scroll snap containers. -/
  scrollSnapContainerHighlightConfig : ScrollSnapContainerHighlightConfig
  /-- Identifier of the node to highlight. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScrollSnapHighlightConfig where
  parseJSON v := do
    .ok
      { scrollSnapContainerHighlightConfig := ← Value.getField v "scrollSnapContainerHighlightConfig" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

instance : ToJSON ScrollSnapHighlightConfig where
  toJSON p := Data.Json.object
    [ ("scrollSnapContainerHighlightConfig", ToJSON.toJSON p.scrollSnapContainerHighlightConfig)
    , ("nodeId", ToJSON.toJSON p.nodeId) ]

/-- `Overlay.HingeConfig`. Configuration for dual screen hinge. -/
structure HingeConfig where
  /-- A rectangle representing the hinge. -/
  rect : DOMPageNetworkEmulationSecurity.DOM.Rect
  /-- The content box highlight fill color (default: a dark color). -/
  contentColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The content box highlight outline color (default: transparent). -/
  outlineColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON HingeConfig where
  parseJSON v := do
    .ok
      { rect := ← Value.getField v "rect" >>= FromJSON.parseJSON
        contentColor := ← (← Value.getFieldOpt v "contentColor").mapM FromJSON.parseJSON
        outlineColor := ← (← Value.getFieldOpt v "outlineColor").mapM FromJSON.parseJSON }

instance : ToJSON HingeConfig where
  toJSON p := Data.Json.object <|
       [("rect", ToJSON.toJSON p.rect)]
    ++ (p.contentColor.map fun v => ("contentColor", ToJSON.toJSON v)).toList
    ++ (p.outlineColor.map fun v => ("outlineColor", ToJSON.toJSON v)).toList

/-- `Overlay.ContainerQueryHighlightConfig`. -/
structure ContainerQueryHighlightConfig where
  /-- A descriptor for the highlight appearance of container query
      containers. -/
  containerQueryContainerHighlightConfig : ContainerQueryContainerHighlightConfig
  /-- Identifier of the container node to highlight. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContainerQueryHighlightConfig where
  parseJSON v := do
    .ok
      { containerQueryContainerHighlightConfig := ← Value.getField v "containerQueryContainerHighlightConfig" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

instance : ToJSON ContainerQueryHighlightConfig where
  toJSON p := Data.Json.object
    [ ("containerQueryContainerHighlightConfig", ToJSON.toJSON p.containerQueryContainerHighlightConfig)
    , ("nodeId", ToJSON.toJSON p.nodeId) ]

/-- `Overlay.IsolationModeHighlightConfig`. -/
structure IsolationModeHighlightConfig where
  /-- The fill color of the resizers (default: transparent). -/
  resizerColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The fill color for resizer handles (default: transparent). -/
  resizerHandleColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The fill color for the mask covering non-isolated elements (default:
      transparent). -/
  maskColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON IsolationModeHighlightConfig where
  parseJSON v := do
    .ok
      { resizerColor := ← (← Value.getFieldOpt v "resizerColor").mapM FromJSON.parseJSON
        resizerHandleColor := ← (← Value.getFieldOpt v "resizerHandleColor").mapM FromJSON.parseJSON
        maskColor := ← (← Value.getFieldOpt v "maskColor").mapM FromJSON.parseJSON }

instance : ToJSON IsolationModeHighlightConfig where
  toJSON p := Data.Json.object <|
       (p.resizerColor.map fun v => ("resizerColor", ToJSON.toJSON v)).toList
    ++ (p.resizerHandleColor.map fun v => ("resizerHandleColor", ToJSON.toJSON v)).toList
    ++ (p.maskColor.map fun v => ("maskColor", ToJSON.toJSON v)).toList

/-- `Overlay.IsolatedElementHighlightConfig`. -/
structure IsolatedElementHighlightConfig where
  /-- A descriptor for the highlight appearance of an element in isolation
      mode. -/
  isolationModeHighlightConfig : IsolationModeHighlightConfig
  /-- Identifier of the isolated element to highlight. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : FromJSON IsolatedElementHighlightConfig where
  parseJSON v := do
    .ok
      { isolationModeHighlightConfig := ← Value.getField v "isolationModeHighlightConfig" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

instance : ToJSON IsolatedElementHighlightConfig where
  toJSON p := Data.Json.object
    [ ("isolationModeHighlightConfig", ToJSON.toJSON p.isolationModeHighlightConfig)
    , ("nodeId", ToJSON.toJSON p.nodeId) ]

/-- `Overlay.InspectMode`. -/
inductive InspectMode where
  | searchForNode | searchForUAShadowDOM | captureAreaScreenshot | showDistances | none
  deriving Repr, BEq, DecidableEq

instance : FromJSON InspectMode where
  parseJSON
    | .string "searchForNode" => .ok .searchForNode
    | .string "searchForUAShadowDOM" => .ok .searchForUAShadowDOM
    | .string "captureAreaScreenshot" => .ok .captureAreaScreenshot
    | .string "showDistances" => .ok .showDistances
    | .string "none" => .ok .none
    | v => .error s!"failed to parse InspectMode: {repr v}"

instance : ToJSON InspectMode where
  toJSON
    | .searchForNode => .string "searchForNode"
    | .searchForUAShadowDOM => .string "searchForUAShadowDOM"
    | .captureAreaScreenshot => .string "captureAreaScreenshot"
    | .showDistances => .string "showDistances"
    | .none => .string "none"

-- ── Events ──

/-- Type of the `Overlay.inspectNodeRequested` event, fired when the inspect
    mode has been set. -/
structure InspectNodeRequested where
  /-- Id of the node to inspect. -/
  backendNodeId : DOMPageNetworkEmulationSecurity.DOM.BackendNodeId
  deriving Repr, BEq, DecidableEq

instance : FromJSON InspectNodeRequested where
  parseJSON v := do
    .ok { backendNodeId := ← Value.getField v "backendNodeId" >>= FromJSON.parseJSON }

instance : ToJSON InspectNodeRequested where
  toJSON p := Data.Json.object [("backendNodeId", ToJSON.toJSON p.backendNodeId)]

instance : Event InspectNodeRequested where
  eventName := "Overlay.inspectNodeRequested"

/-- Type of the `Overlay.nodeHighlightRequested` event. -/
structure NodeHighlightRequested where
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : FromJSON NodeHighlightRequested where
  parseJSON v := do
    .ok { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

instance : ToJSON NodeHighlightRequested where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId)]

instance : Event NodeHighlightRequested where
  eventName := "Overlay.nodeHighlightRequested"

/-- Type of the `Overlay.screenshotRequested` event, fired when user asks to
    capture a screenshot of one of the elements. -/
structure ScreenshotRequested where
  /-- Viewport to capture, in device independent pixels (dip). -/
  viewport : DOMPageNetworkEmulationSecurity.Page.Viewport
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScreenshotRequested where
  parseJSON v := do
    .ok { viewport := ← Value.getField v "viewport" >>= FromJSON.parseJSON }

instance : ToJSON ScreenshotRequested where
  toJSON p := Data.Json.object [("viewport", ToJSON.toJSON p.viewport)]

instance : Event ScreenshotRequested where
  eventName := "Overlay.screenshotRequested"

/-- Type of the `Overlay.inspectModeCanceled` event, fired when user cancels
    the inspect mode. -/
structure InspectModeCanceled where
  deriving Repr, BEq, DecidableEq

instance : FromJSON InspectModeCanceled where
  parseJSON _ := .ok {}

instance : ToJSON InspectModeCanceled where
  toJSON _ := .null

instance : Event InspectModeCanceled where
  eventName := "Overlay.inspectModeCanceled"

-- ── Commands ──

/-- Parameters of the `Overlay.disable` command: disables domain
    notifications. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Overlay.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.enable` command: enables domain
    notifications. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Overlay.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.getHighlightObjectForTest` command: for
    testing. -/
structure PGetHighlightObjectForTest where
  /-- Id of the node to get highlight object for. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  /-- Whether to include distance info. -/
  includeDistance : Option Bool := none
  /-- Whether to include style info. -/
  includeStyle : Option Bool := none
  /-- The color format to get config with (default: hex). -/
  colorFormat : Option ColorFormat := none
  /-- Whether to show accessibility info (default: true). -/
  showAccessibilityInfo : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetHighlightObjectForTest where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ (p.includeDistance.map fun v => ("includeDistance", ToJSON.toJSON v)).toList
    ++ (p.includeStyle.map fun v => ("includeStyle", ToJSON.toJSON v)).toList
    ++ (p.colorFormat.map fun v => ("colorFormat", ToJSON.toJSON v)).toList
    ++ (p.showAccessibilityInfo.map fun v => ("showAccessibilityInfo", ToJSON.toJSON v)).toList

/-- Response of the `Overlay.getHighlightObjectForTest` command. -/
structure GetHighlightObjectForTest where
  /-- Highlight data for the node. -/
  highlight : List (String × String)
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetHighlightObjectForTest where
  parseJSON v := do
    .ok { highlight := ← Value.getField v "highlight" >>= FromJSON.parseJSON }

instance : Command PGetHighlightObjectForTest where
  Response := GetHighlightObjectForTest
  commandName _ := "Overlay.getHighlightObjectForTest"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Overlay.getGridHighlightObjectsForTest` command: for
    Persistent Grid testing. -/
structure PGetGridHighlightObjectsForTest where
  /-- Ids of the nodes to get highlight objects for. -/
  nodeIds : List DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetGridHighlightObjectsForTest where
  toJSON p := Data.Json.object [("nodeIds", ToJSON.toJSON p.nodeIds)]

/-- Response of the `Overlay.getGridHighlightObjectsForTest` command. -/
structure GetGridHighlightObjectsForTest where
  /-- Grid highlight data for the node ids provided. -/
  highlights : List (String × String)
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetGridHighlightObjectsForTest where
  parseJSON v := do
    .ok { highlights := ← Value.getField v "highlights" >>= FromJSON.parseJSON }

instance : Command PGetGridHighlightObjectsForTest where
  Response := GetGridHighlightObjectsForTest
  commandName _ := "Overlay.getGridHighlightObjectsForTest"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Overlay.getSourceOrderHighlightObjectForTest` command:
    for Source Order Viewer testing. -/
structure PGetSourceOrderHighlightObjectForTest where
  /-- Id of the node to highlight. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetSourceOrderHighlightObjectForTest where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId)]

/-- Response of the `Overlay.getSourceOrderHighlightObjectForTest` command. -/
structure GetSourceOrderHighlightObjectForTest where
  /-- Source order highlight data for the node id provided. -/
  highlight : List (String × String)
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetSourceOrderHighlightObjectForTest where
  parseJSON v := do
    .ok { highlight := ← Value.getField v "highlight" >>= FromJSON.parseJSON }

instance : Command PGetSourceOrderHighlightObjectForTest where
  Response := GetSourceOrderHighlightObjectForTest
  commandName _ := "Overlay.getSourceOrderHighlightObjectForTest"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Overlay.hideHighlight` command: hides any
    highlight. -/
structure PHideHighlight where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PHideHighlight where toJSON _ := .null

instance : Command PHideHighlight where
  Response := Unit
  commandName _ := "Overlay.hideHighlight"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.highlightNode` command: highlights the DOM
    node with given id or with the given JavaScript object wrapper. Either
    `nodeId` or `objectId` must be specified. -/
structure PHighlightNode where
  /-- A descriptor for the highlight appearance. -/
  highlightConfig : HighlightConfig
  /-- Identifier of the node to highlight. -/
  nodeId : Option DOMPageNetworkEmulationSecurity.DOM.NodeId := none
  /-- Identifier of the backend node to highlight. -/
  backendNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- JavaScript object id of the node to be highlighted. -/
  objectId : Option Runtime.RemoteObjectId := none
  /-- Selectors to highlight relevant nodes. -/
  selector : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PHighlightNode where
  toJSON p := Data.Json.object <|
       [("highlightConfig", ToJSON.toJSON p.highlightConfig)]
    ++ (p.nodeId.map fun v => ("nodeId", ToJSON.toJSON v)).toList
    ++ (p.backendNodeId.map fun v => ("backendNodeId", ToJSON.toJSON v)).toList
    ++ (p.objectId.map fun v => ("objectId", ToJSON.toJSON v)).toList
    ++ (p.selector.map fun v => ("selector", ToJSON.toJSON v)).toList

instance : Command PHighlightNode where
  Response := Unit
  commandName _ := "Overlay.highlightNode"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.highlightQuad` command: highlights the given
    quad. Coordinates are absolute with respect to the main frame
    viewport. -/
structure PHighlightQuad where
  /-- Quad to highlight. -/
  quad : DOMPageNetworkEmulationSecurity.DOM.Quad
  /-- The highlight fill color (default: transparent). -/
  color : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The highlight outline color (default: transparent). -/
  outlineColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PHighlightQuad where
  toJSON p := Data.Json.object <|
       [("quad", ToJSON.toJSON p.quad)]
    ++ (p.color.map fun v => ("color", ToJSON.toJSON v)).toList
    ++ (p.outlineColor.map fun v => ("outlineColor", ToJSON.toJSON v)).toList

instance : Command PHighlightQuad where
  Response := Unit
  commandName _ := "Overlay.highlightQuad"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.highlightRect` command: highlights the given
    rectangle. Coordinates are absolute with respect to the main frame
    viewport. -/
structure PHighlightRect where
  /-- X coordinate. -/
  x : Int
  /-- Y coordinate. -/
  y : Int
  /-- Rectangle width. -/
  width : Int
  /-- Rectangle height. -/
  height : Int
  /-- The highlight fill color (default: transparent). -/
  color : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  /-- The highlight outline color (default: transparent). -/
  outlineColor : Option DOMPageNetworkEmulationSecurity.DOM.RGBA := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PHighlightRect where
  toJSON p := Data.Json.object <|
       [ ("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y)
       , ("width", ToJSON.toJSON p.width), ("height", ToJSON.toJSON p.height) ]
    ++ (p.color.map fun v => ("color", ToJSON.toJSON v)).toList
    ++ (p.outlineColor.map fun v => ("outlineColor", ToJSON.toJSON v)).toList

instance : Command PHighlightRect where
  Response := Unit
  commandName _ := "Overlay.highlightRect"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.highlightSourceOrder` command: highlights the
    source order of the children of the DOM node with given id or with the
    given JavaScript object wrapper. Either `nodeId` or `objectId` must be
    specified. -/
structure PHighlightSourceOrder where
  /-- A descriptor for the appearance of the overlay drawing. -/
  sourceOrderConfig : SourceOrderConfig
  /-- Identifier of the node to highlight. -/
  nodeId : Option DOMPageNetworkEmulationSecurity.DOM.NodeId := none
  /-- Identifier of the backend node to highlight. -/
  backendNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- JavaScript object id of the node to be highlighted. -/
  objectId : Option Runtime.RemoteObjectId := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PHighlightSourceOrder where
  toJSON p := Data.Json.object <|
       [("sourceOrderConfig", ToJSON.toJSON p.sourceOrderConfig)]
    ++ (p.nodeId.map fun v => ("nodeId", ToJSON.toJSON v)).toList
    ++ (p.backendNodeId.map fun v => ("backendNodeId", ToJSON.toJSON v)).toList
    ++ (p.objectId.map fun v => ("objectId", ToJSON.toJSON v)).toList

instance : Command PHighlightSourceOrder where
  Response := Unit
  commandName _ := "Overlay.highlightSourceOrder"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setInspectMode` command: enters the 'inspect'
    mode. In this mode, elements that the user is hovering over are
    highlighted. Backend then generates an `inspectNodeRequested` event upon
    element selection. -/
structure PSetInspectMode where
  /-- Set an inspection mode. -/
  mode : InspectMode
  /-- A descriptor for the highlight appearance of hovered-over nodes. May be
      omitted if `enabled == false`. -/
  highlightConfig : Option HighlightConfig := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetInspectMode where
  toJSON p := Data.Json.object <|
       [("mode", ToJSON.toJSON p.mode)]
    ++ (p.highlightConfig.map fun v => ("highlightConfig", ToJSON.toJSON v)).toList

instance : Command PSetInspectMode where
  Response := Unit
  commandName _ := "Overlay.setInspectMode"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowAdHighlights` command: highlights the
    owner element of all frames detected to be ads. -/
structure PSetShowAdHighlights where
  /-- `true` for showing ad highlights. -/
  «show» : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowAdHighlights where
  toJSON p := Data.Json.object [("show", ToJSON.toJSON p.«show»)]

instance : Command PSetShowAdHighlights where
  Response := Unit
  commandName _ := "Overlay.setShowAdHighlights"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setPausedInDebuggerMessage` command. -/
structure PSetPausedInDebuggerMessage where
  /-- The message to display, also triggers resume and step over controls. -/
  message : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetPausedInDebuggerMessage where
  toJSON p := Data.Json.object <|
    (p.message.map fun v => ("message", ToJSON.toJSON v)).toList

instance : Command PSetPausedInDebuggerMessage where
  Response := Unit
  commandName _ := "Overlay.setPausedInDebuggerMessage"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowDebugBorders` command: requests that the
    backend shows debug borders on layers. -/
structure PSetShowDebugBorders where
  /-- `true` for showing debug borders. -/
  «show» : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowDebugBorders where
  toJSON p := Data.Json.object [("show", ToJSON.toJSON p.«show»)]

instance : Command PSetShowDebugBorders where
  Response := Unit
  commandName _ := "Overlay.setShowDebugBorders"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowFPSCounter` command: requests that the
    backend shows the FPS counter. -/
structure PSetShowFPSCounter where
  /-- `true` for showing the FPS counter. -/
  «show» : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowFPSCounter where
  toJSON p := Data.Json.object [("show", ToJSON.toJSON p.«show»)]

instance : Command PSetShowFPSCounter where
  Response := Unit
  commandName _ := "Overlay.setShowFPSCounter"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowGridOverlays` command: highlights
    multiple elements with the CSS Grid overlay. -/
structure PSetShowGridOverlays where
  /-- An array of node identifiers and descriptors for the highlight
      appearance. -/
  gridNodeHighlightConfigs : List GridNodeHighlightConfig
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowGridOverlays where
  toJSON p := Data.Json.object [("gridNodeHighlightConfigs", ToJSON.toJSON p.gridNodeHighlightConfigs)]

instance : Command PSetShowGridOverlays where
  Response := Unit
  commandName _ := "Overlay.setShowGridOverlays"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowFlexOverlays` command. -/
structure PSetShowFlexOverlays where
  /-- An array of node identifiers and descriptors for the highlight
      appearance. -/
  flexNodeHighlightConfigs : List FlexNodeHighlightConfig
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowFlexOverlays where
  toJSON p := Data.Json.object [("flexNodeHighlightConfigs", ToJSON.toJSON p.flexNodeHighlightConfigs)]

instance : Command PSetShowFlexOverlays where
  Response := Unit
  commandName _ := "Overlay.setShowFlexOverlays"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowScrollSnapOverlays` command. -/
structure PSetShowScrollSnapOverlays where
  /-- An array of node identifiers and descriptors for the highlight
      appearance. -/
  scrollSnapHighlightConfigs : List ScrollSnapHighlightConfig
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowScrollSnapOverlays where
  toJSON p := Data.Json.object [("scrollSnapHighlightConfigs", ToJSON.toJSON p.scrollSnapHighlightConfigs)]

instance : Command PSetShowScrollSnapOverlays where
  Response := Unit
  commandName _ := "Overlay.setShowScrollSnapOverlays"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowContainerQueryOverlays` command. -/
structure PSetShowContainerQueryOverlays where
  /-- An array of node identifiers and descriptors for the highlight
      appearance. -/
  containerQueryHighlightConfigs : List ContainerQueryHighlightConfig
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowContainerQueryOverlays where
  toJSON p := Data.Json.object [("containerQueryHighlightConfigs", ToJSON.toJSON p.containerQueryHighlightConfigs)]

instance : Command PSetShowContainerQueryOverlays where
  Response := Unit
  commandName _ := "Overlay.setShowContainerQueryOverlays"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowPaintRects` command: requests that the
    backend shows paint rectangles. -/
structure PSetShowPaintRects where
  /-- `true` for showing paint rectangles. -/
  result : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowPaintRects where
  toJSON p := Data.Json.object [("result", ToJSON.toJSON p.result)]

instance : Command PSetShowPaintRects where
  Response := Unit
  commandName _ := "Overlay.setShowPaintRects"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowLayoutShiftRegions` command: requests
    that the backend shows layout shift regions. -/
structure PSetShowLayoutShiftRegions where
  /-- `true` for showing layout shift regions. -/
  result : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowLayoutShiftRegions where
  toJSON p := Data.Json.object [("result", ToJSON.toJSON p.result)]

instance : Command PSetShowLayoutShiftRegions where
  Response := Unit
  commandName _ := "Overlay.setShowLayoutShiftRegions"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowScrollBottleneckRects` command:
    requests that the backend shows scroll bottleneck rects. -/
structure PSetShowScrollBottleneckRects where
  /-- `true` for showing scroll bottleneck rects. -/
  «show» : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowScrollBottleneckRects where
  toJSON p := Data.Json.object [("show", ToJSON.toJSON p.«show»)]

instance : Command PSetShowScrollBottleneckRects where
  Response := Unit
  commandName _ := "Overlay.setShowScrollBottleneckRects"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowWebVitals` command: requests that the
    backend shows an overlay with web vital metrics. -/
structure PSetShowWebVitals where
  «show» : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowWebVitals where
  toJSON p := Data.Json.object [("show", ToJSON.toJSON p.«show»)]

instance : Command PSetShowWebVitals where
  Response := Unit
  commandName _ := "Overlay.setShowWebVitals"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowViewportSizeOnResize` command: paints
    viewport size upon main frame resize. -/
structure PSetShowViewportSizeOnResize where
  /-- Whether to paint size or not. -/
  «show» : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowViewportSizeOnResize where
  toJSON p := Data.Json.object [("show", ToJSON.toJSON p.«show»)]

instance : Command PSetShowViewportSizeOnResize where
  Response := Unit
  commandName _ := "Overlay.setShowViewportSizeOnResize"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowHinge` command: adds a dual screen
    device hinge. -/
structure PSetShowHinge where
  /-- Hinge data; `none` means hide the hinge. -/
  hingeConfig : Option HingeConfig := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowHinge where
  toJSON p := Data.Json.object <|
    (p.hingeConfig.map fun v => ("hingeConfig", ToJSON.toJSON v)).toList

instance : Command PSetShowHinge where
  Response := Unit
  commandName _ := "Overlay.setShowHinge"
  decodeResponse _ := .ok ()

/-- Parameters of the `Overlay.setShowIsolatedElements` command: shows
    elements in isolation mode with overlays. -/
structure PSetShowIsolatedElements where
  /-- An array of node identifiers and descriptors for the highlight
      appearance. -/
  isolatedElementHighlightConfigs : List IsolatedElementHighlightConfig
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetShowIsolatedElements where
  toJSON p := Data.Json.object [("isolatedElementHighlightConfigs", ToJSON.toJSON p.isolatedElementHighlightConfigs)]

instance : Command PSetShowIsolatedElements where
  Response := Unit
  commandName _ := "Overlay.setShowIsolatedElements"
  decodeResponse _ := .ok ()

end CDP.Domains.Overlay
