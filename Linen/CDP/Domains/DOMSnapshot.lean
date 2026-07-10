/-
  Linen.CDP.Domains.DOMSnapshot — the `DOMSnapshot` CDP domain

  This domain facilitates obtaining document snapshots with DOM, layout, and
  style information. Ports `CDP.Domains.DOMSnapshot` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.CacheStorage`'s docstring.

  References `DOM.BackendNodeId`/`DOM.PseudoType`/`DOM.ShadowRootType`/
  `DOM.Rect`/`Page.FrameId` from `CDP.Domains.DOMPageNetworkEmulationSecurity`
  and `EventListener` from `CDP.Domains.DOMDebugger`. `DOMNode` embeds
  `Option (List DOMDebugger.EventListener)`, and `DOMDebugger.EventListener`
  derives only `Repr, BEq` (no `DecidableEq`, since it embeds
  `Option Runtime.RemoteObject`), so `DOMNode` itself derives only
  `Repr, BEq` as well. None of this module's own types are self- or
  mutually-recursive, so no termination proofs are needed here. Upstream
  declares no events for this domain — only commands.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.DOMDebugger

namespace CDP.Domains.DOMSnapshot

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

-- ── Basic aliases ──

/-- Index of the string in the strings table. -/
abbrev StringIndex := Int

/-- Index of the string in the strings table. -/
abbrev ArrayOfStrings := List StringIndex

/-- A rectangle, encoded as `[x, y, width, height]`. -/
abbrev Rectangle := List Float

-- ── Simple value types ──

/-- A name/value pair. -/
structure NameValue where
  /-- Attribute/property name. -/
  name : String
  /-- Attribute/property value. -/
  value : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON NameValue where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON NameValue where
  toJSON n := Data.Json.object [("name", ToJSON.toJSON n.name), ("value", ToJSON.toJSON n.value)]

/-- Details of post layout rendered text positions. The exact layout should
    not be regarded as stable and may change between versions. -/
structure InlineTextBox where
  /-- The bounding box in document coordinates. Note that scroll offset of
      the document is ignored. -/
  boundingBox : DOMPageNetworkEmulationSecurity.DOM.Rect
  /-- The starting index in characters, for this post layout textbox
      substring. Characters that would be represented as a surrogate pair in
      UTF-16 have length 2. -/
  startCharacterIndex : Int
  /-- The number of characters in this post layout textbox substring.
      Characters that would be represented as a surrogate pair in UTF-16
      have length 2. -/
  numCharacters : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON InlineTextBox where
  parseJSON v := do
    .ok
      { boundingBox := ← Value.getField v "boundingBox" >>= FromJSON.parseJSON
        startCharacterIndex := ← Value.getField v "startCharacterIndex" >>= FromJSON.parseJSON
        numCharacters := ← Value.getField v "numCharacters" >>= FromJSON.parseJSON }

instance : ToJSON InlineTextBox where
  toJSON t := Data.Json.object
    [ ("boundingBox", ToJSON.toJSON t.boundingBox)
    , ("startCharacterIndex", ToJSON.toJSON t.startCharacterIndex)
    , ("numCharacters", ToJSON.toJSON t.numCharacters) ]

/-- Details of an element in the DOM tree with a `LayoutObject`. -/
structure LayoutTreeNode where
  /-- The index of the related DOM node in the `domNodes` array returned by
      `getSnapshot`. -/
  domNodeIndex : Int
  /-- The bounding box in document coordinates. Note that scroll offset of
      the document is ignored. -/
  boundingBox : DOMPageNetworkEmulationSecurity.DOM.Rect
  /-- Contents of the `LayoutText`, if any. -/
  layoutText : Option String := none
  /-- The post-layout inline text nodes, if any. -/
  inlineTextNodes : Option (List InlineTextBox) := none
  /-- Index into the `computedStyles` array returned by `getSnapshot`. -/
  styleIndex : Option Int := none
  /-- Global paint order index, which is determined by the stacking order of
      the nodes. Nodes that are painted together will have the same index.
      Only provided if `includePaintOrder` in `getSnapshot` was true. -/
  paintOrder : Option Int := none
  /-- Set to true to indicate the element begins a new stacking context. -/
  isStackingContext : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON LayoutTreeNode where
  parseJSON v := do
    .ok
      { domNodeIndex := ← Value.getField v "domNodeIndex" >>= FromJSON.parseJSON
        boundingBox := ← Value.getField v "boundingBox" >>= FromJSON.parseJSON
        layoutText := ← (← Value.getFieldOpt v "layoutText").mapM FromJSON.parseJSON
        inlineTextNodes := ← (← Value.getFieldOpt v "inlineTextNodes").mapM FromJSON.parseJSON
        styleIndex := ← (← Value.getFieldOpt v "styleIndex").mapM FromJSON.parseJSON
        paintOrder := ← (← Value.getFieldOpt v "paintOrder").mapM FromJSON.parseJSON
        isStackingContext := ← (← Value.getFieldOpt v "isStackingContext").mapM FromJSON.parseJSON }

instance : ToJSON LayoutTreeNode where
  toJSON t := Data.Json.object <|
    [ ("domNodeIndex", ToJSON.toJSON t.domNodeIndex), ("boundingBox", ToJSON.toJSON t.boundingBox) ]
    ++ (t.layoutText.map fun x => ("layoutText", ToJSON.toJSON x)).toList
    ++ (t.inlineTextNodes.map fun x => ("inlineTextNodes", ToJSON.toJSON x)).toList
    ++ (t.styleIndex.map fun x => ("styleIndex", ToJSON.toJSON x)).toList
    ++ (t.paintOrder.map fun x => ("paintOrder", ToJSON.toJSON x)).toList
    ++ (t.isStackingContext.map fun x => ("isStackingContext", ToJSON.toJSON x)).toList

/-- A subset of the full `ComputedStyle` as defined by the request
    whitelist. -/
structure ComputedStyle where
  /-- Name/value pairs of computed style properties. -/
  properties : List NameValue
  deriving Repr, BEq, DecidableEq

instance : FromJSON ComputedStyle where
  parseJSON v := do .ok { properties := ← Value.getField v "properties" >>= FromJSON.parseJSON }

instance : ToJSON ComputedStyle where
  toJSON c := Data.Json.object [("properties", ToJSON.toJSON c.properties)]

-- ── Rare data tables ──

/-- Data that is only present on rare nodes. -/
structure RareStringData where
  index : List Int
  value : List StringIndex
  deriving Repr, BEq, DecidableEq

instance : FromJSON RareStringData where
  parseJSON v := do
    .ok
      { index := ← Value.getField v "index" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON RareStringData where
  toJSON r := Data.Json.object [("index", ToJSON.toJSON r.index), ("value", ToJSON.toJSON r.value)]

/-- Data that is only present on rare boolean-valued nodes. -/
structure RareBooleanData where
  index : List Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON RareBooleanData where
  parseJSON v := do .ok { index := ← Value.getField v "index" >>= FromJSON.parseJSON }

instance : ToJSON RareBooleanData where
  toJSON r := Data.Json.object [("index", ToJSON.toJSON r.index)]

/-- Data that is only present on rare integer-valued nodes. -/
structure RareIntegerData where
  index : List Int
  value : List Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON RareIntegerData where
  parseJSON v := do
    .ok
      { index := ← Value.getField v "index" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON RareIntegerData where
  toJSON r := Data.Json.object [("index", ToJSON.toJSON r.index), ("value", ToJSON.toJSON r.value)]

-- ── DOM node ──

/-- A `Node` in the DOM tree. -/
structure DOMNode where
  /-- `Node`'s `nodeType`. -/
  nodeType : Int
  /-- `Node`'s `nodeName`. -/
  nodeName : String
  /-- `Node`'s `nodeValue`. -/
  nodeValue : String
  /-- Only set for textarea elements, contains the text value. -/
  textValue : Option String := none
  /-- Only set for input elements, contains the input's associated text
      value. -/
  inputValue : Option String := none
  /-- Only set for radio and checkbox input elements, indicates if the
      element has been checked. -/
  inputChecked : Option Bool := none
  /-- Only set for option elements, indicates if the element has been
      selected. -/
  optionSelected : Option Bool := none
  /-- `Node`'s id, corresponds to `DOM.Node.backendNodeId`. -/
  backendNodeId : DOMPageNetworkEmulationSecurity.DOM.BackendNodeId
  /-- The indexes of the node's child nodes in the `domNodes` array returned
      by `getSnapshot`, if any. -/
  childNodeIndexes : Option (List Int) := none
  /-- Attributes of an `Element` node. -/
  attributes : Option (List NameValue) := none
  /-- Indexes of pseudo elements associated with this node in the
      `domNodes` array returned by `getSnapshot`, if any. -/
  pseudoElementIndexes : Option (List Int) := none
  /-- The index of the node's related layout tree node in the
      `layoutTreeNodes` array returned by `getSnapshot`, if any. -/
  layoutNodeIndex : Option Int := none
  /-- Document URL that `Document` or `FrameOwner` node points to. -/
  documentURL : Option String := none
  /-- Base URL that `Document` or `FrameOwner` node uses for URL
      completion. -/
  baseURL : Option String := none
  /-- Only set for documents, contains the document's content language. -/
  contentLanguage : Option String := none
  /-- Only set for documents, contains the document's character set
      encoding. -/
  documentEncoding : Option String := none
  /-- `DocumentType` node's `publicId`. -/
  publicId : Option String := none
  /-- `DocumentType` node's `systemId`. -/
  systemId : Option String := none
  /-- Frame ID for frame owner elements and also for the document node. -/
  frameId : Option DOMPageNetworkEmulationSecurity.Page.FrameId := none
  /-- The index of a frame owner element's content document in the
      `domNodes` array returned by `getSnapshot`, if any. -/
  contentDocumentIndex : Option Int := none
  /-- Type of a pseudo element node. -/
  pseudoType : Option DOMPageNetworkEmulationSecurity.DOM.PseudoType := none
  /-- Shadow root type. -/
  shadowRootType : Option DOMPageNetworkEmulationSecurity.DOM.ShadowRootType := none
  /-- Whether this DOM node responds to mouse clicks. This includes nodes
      that have had click event listeners attached via JavaScript as well
      as anchor tags that naturally navigate when clicked. -/
  isClickable : Option Bool := none
  /-- Details of the node's event listeners, if any. -/
  eventListeners : Option (List DOMDebugger.EventListener) := none
  /-- The selected url for nodes with a `srcset` attribute. -/
  currentSourceURL : Option String := none
  /-- The url of the script (if any) that generates this node. -/
  originURL : Option String := none
  /-- Scroll offset, set when this node is a `Document`. -/
  scrollOffsetX : Option Float := none
  /-- Scroll offset, set when this node is a `Document`. -/
  scrollOffsetY : Option Float := none
  deriving Repr, BEq

instance : FromJSON DOMNode where
  parseJSON v := do
    .ok
      { nodeType := ← Value.getField v "nodeType" >>= FromJSON.parseJSON
        nodeName := ← Value.getField v "nodeName" >>= FromJSON.parseJSON
        nodeValue := ← Value.getField v "nodeValue" >>= FromJSON.parseJSON
        textValue := ← (← Value.getFieldOpt v "textValue").mapM FromJSON.parseJSON
        inputValue := ← (← Value.getFieldOpt v "inputValue").mapM FromJSON.parseJSON
        inputChecked := ← (← Value.getFieldOpt v "inputChecked").mapM FromJSON.parseJSON
        optionSelected := ← (← Value.getFieldOpt v "optionSelected").mapM FromJSON.parseJSON
        backendNodeId := ← Value.getField v "backendNodeId" >>= FromJSON.parseJSON
        childNodeIndexes := ← (← Value.getFieldOpt v "childNodeIndexes").mapM FromJSON.parseJSON
        attributes := ← (← Value.getFieldOpt v "attributes").mapM FromJSON.parseJSON
        pseudoElementIndexes := ← (← Value.getFieldOpt v "pseudoElementIndexes").mapM FromJSON.parseJSON
        layoutNodeIndex := ← (← Value.getFieldOpt v "layoutNodeIndex").mapM FromJSON.parseJSON
        documentURL := ← (← Value.getFieldOpt v "documentURL").mapM FromJSON.parseJSON
        baseURL := ← (← Value.getFieldOpt v "baseURL").mapM FromJSON.parseJSON
        contentLanguage := ← (← Value.getFieldOpt v "contentLanguage").mapM FromJSON.parseJSON
        documentEncoding := ← (← Value.getFieldOpt v "documentEncoding").mapM FromJSON.parseJSON
        publicId := ← (← Value.getFieldOpt v "publicId").mapM FromJSON.parseJSON
        systemId := ← (← Value.getFieldOpt v "systemId").mapM FromJSON.parseJSON
        frameId := ← (← Value.getFieldOpt v "frameId").mapM FromJSON.parseJSON
        contentDocumentIndex := ← (← Value.getFieldOpt v "contentDocumentIndex").mapM FromJSON.parseJSON
        pseudoType := ← (← Value.getFieldOpt v "pseudoType").mapM FromJSON.parseJSON
        shadowRootType := ← (← Value.getFieldOpt v "shadowRootType").mapM FromJSON.parseJSON
        isClickable := ← (← Value.getFieldOpt v "isClickable").mapM FromJSON.parseJSON
        eventListeners := ← (← Value.getFieldOpt v "eventListeners").mapM FromJSON.parseJSON
        currentSourceURL := ← (← Value.getFieldOpt v "currentSourceURL").mapM FromJSON.parseJSON
        originURL := ← (← Value.getFieldOpt v "originURL").mapM FromJSON.parseJSON
        scrollOffsetX := ← (← Value.getFieldOpt v "scrollOffsetX").mapM FromJSON.parseJSON
        scrollOffsetY := ← (← Value.getFieldOpt v "scrollOffsetY").mapM FromJSON.parseJSON }

instance : ToJSON DOMNode where
  toJSON n := Data.Json.object <|
    [ ("nodeType", ToJSON.toJSON n.nodeType), ("nodeName", ToJSON.toJSON n.nodeName)
    , ("nodeValue", ToJSON.toJSON n.nodeValue), ("backendNodeId", ToJSON.toJSON n.backendNodeId) ]
    ++ (n.textValue.map fun x => ("textValue", ToJSON.toJSON x)).toList
    ++ (n.inputValue.map fun x => ("inputValue", ToJSON.toJSON x)).toList
    ++ (n.inputChecked.map fun x => ("inputChecked", ToJSON.toJSON x)).toList
    ++ (n.optionSelected.map fun x => ("optionSelected", ToJSON.toJSON x)).toList
    ++ (n.childNodeIndexes.map fun x => ("childNodeIndexes", ToJSON.toJSON x)).toList
    ++ (n.attributes.map fun x => ("attributes", ToJSON.toJSON x)).toList
    ++ (n.pseudoElementIndexes.map fun x => ("pseudoElementIndexes", ToJSON.toJSON x)).toList
    ++ (n.layoutNodeIndex.map fun x => ("layoutNodeIndex", ToJSON.toJSON x)).toList
    ++ (n.documentURL.map fun x => ("documentURL", ToJSON.toJSON x)).toList
    ++ (n.baseURL.map fun x => ("baseURL", ToJSON.toJSON x)).toList
    ++ (n.contentLanguage.map fun x => ("contentLanguage", ToJSON.toJSON x)).toList
    ++ (n.documentEncoding.map fun x => ("documentEncoding", ToJSON.toJSON x)).toList
    ++ (n.publicId.map fun x => ("publicId", ToJSON.toJSON x)).toList
    ++ (n.systemId.map fun x => ("systemId", ToJSON.toJSON x)).toList
    ++ (n.frameId.map fun x => ("frameId", ToJSON.toJSON x)).toList
    ++ (n.contentDocumentIndex.map fun x => ("contentDocumentIndex", ToJSON.toJSON x)).toList
    ++ (n.pseudoType.map fun x => ("pseudoType", ToJSON.toJSON x)).toList
    ++ (n.shadowRootType.map fun x => ("shadowRootType", ToJSON.toJSON x)).toList
    ++ (n.isClickable.map fun x => ("isClickable", ToJSON.toJSON x)).toList
    ++ (n.eventListeners.map fun x => ("eventListeners", ToJSON.toJSON x)).toList
    ++ (n.currentSourceURL.map fun x => ("currentSourceURL", ToJSON.toJSON x)).toList
    ++ (n.originURL.map fun x => ("originURL", ToJSON.toJSON x)).toList
    ++ (n.scrollOffsetX.map fun x => ("scrollOffsetX", ToJSON.toJSON x)).toList
    ++ (n.scrollOffsetY.map fun x => ("scrollOffsetY", ToJSON.toJSON x)).toList

-- ── Snapshot tables (`captureSnapshot`) ──

/-- Table containing nodes. -/
structure NodeTreeSnapshot where
  /-- Parent node index. -/
  parentIndex : Option (List Int) := none
  /-- `Node`'s `nodeType`. -/
  nodeType : Option (List Int) := none
  /-- Type of the shadow root the `Node` is in. String values are equal to
      the `ShadowRootType` enum. -/
  shadowRootType : Option RareStringData := none
  /-- `Node`'s `nodeName`. -/
  nodeName : Option (List StringIndex) := none
  /-- `Node`'s `nodeValue`. -/
  nodeValue : Option (List StringIndex) := none
  /-- `Node`'s id, corresponds to `DOM.Node.backendNodeId`. -/
  backendNodeId : Option (List DOMPageNetworkEmulationSecurity.DOM.BackendNodeId) := none
  /-- Attributes of an `Element` node. Flatten name, value pairs. -/
  attributes : Option (List ArrayOfStrings) := none
  /-- Only set for textarea elements, contains the text value. -/
  textValue : Option RareStringData := none
  /-- Only set for input elements, contains the input's associated text
      value. -/
  inputValue : Option RareStringData := none
  /-- Only set for radio and checkbox input elements, indicates if the
      element has been checked. -/
  inputChecked : Option RareBooleanData := none
  /-- Only set for option elements, indicates if the element has been
      selected. -/
  optionSelected : Option RareBooleanData := none
  /-- The index of the document in the list of the snapshot documents. -/
  contentDocumentIndex : Option RareIntegerData := none
  /-- Type of a pseudo element node. -/
  pseudoType : Option RareStringData := none
  /-- Pseudo element identifier for this node. Only present if there is a
      valid `pseudoType`. -/
  pseudoIdentifier : Option RareStringData := none
  /-- Whether this DOM node responds to mouse clicks. This includes nodes
      that have had click event listeners attached via JavaScript as well
      as anchor tags that naturally navigate when clicked. -/
  isClickable : Option RareBooleanData := none
  /-- The selected url for nodes with a `srcset` attribute. -/
  currentSourceURL : Option RareStringData := none
  /-- The url of the script (if any) that generates this node. -/
  originURL : Option RareStringData := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON NodeTreeSnapshot where
  parseJSON v := do
    .ok
      { parentIndex := ← (← Value.getFieldOpt v "parentIndex").mapM FromJSON.parseJSON
        nodeType := ← (← Value.getFieldOpt v "nodeType").mapM FromJSON.parseJSON
        shadowRootType := ← (← Value.getFieldOpt v "shadowRootType").mapM FromJSON.parseJSON
        nodeName := ← (← Value.getFieldOpt v "nodeName").mapM FromJSON.parseJSON
        nodeValue := ← (← Value.getFieldOpt v "nodeValue").mapM FromJSON.parseJSON
        backendNodeId := ← (← Value.getFieldOpt v "backendNodeId").mapM FromJSON.parseJSON
        attributes := ← (← Value.getFieldOpt v "attributes").mapM FromJSON.parseJSON
        textValue := ← (← Value.getFieldOpt v "textValue").mapM FromJSON.parseJSON
        inputValue := ← (← Value.getFieldOpt v "inputValue").mapM FromJSON.parseJSON
        inputChecked := ← (← Value.getFieldOpt v "inputChecked").mapM FromJSON.parseJSON
        optionSelected := ← (← Value.getFieldOpt v "optionSelected").mapM FromJSON.parseJSON
        contentDocumentIndex := ← (← Value.getFieldOpt v "contentDocumentIndex").mapM FromJSON.parseJSON
        pseudoType := ← (← Value.getFieldOpt v "pseudoType").mapM FromJSON.parseJSON
        pseudoIdentifier := ← (← Value.getFieldOpt v "pseudoIdentifier").mapM FromJSON.parseJSON
        isClickable := ← (← Value.getFieldOpt v "isClickable").mapM FromJSON.parseJSON
        currentSourceURL := ← (← Value.getFieldOpt v "currentSourceURL").mapM FromJSON.parseJSON
        originURL := ← (← Value.getFieldOpt v "originURL").mapM FromJSON.parseJSON }

instance : ToJSON NodeTreeSnapshot where
  toJSON n := Data.Json.object <|
    (n.parentIndex.map fun x => ("parentIndex", ToJSON.toJSON x)).toList
    ++ (n.nodeType.map fun x => ("nodeType", ToJSON.toJSON x)).toList
    ++ (n.shadowRootType.map fun x => ("shadowRootType", ToJSON.toJSON x)).toList
    ++ (n.nodeName.map fun x => ("nodeName", ToJSON.toJSON x)).toList
    ++ (n.nodeValue.map fun x => ("nodeValue", ToJSON.toJSON x)).toList
    ++ (n.backendNodeId.map fun x => ("backendNodeId", ToJSON.toJSON x)).toList
    ++ (n.attributes.map fun x => ("attributes", ToJSON.toJSON x)).toList
    ++ (n.textValue.map fun x => ("textValue", ToJSON.toJSON x)).toList
    ++ (n.inputValue.map fun x => ("inputValue", ToJSON.toJSON x)).toList
    ++ (n.inputChecked.map fun x => ("inputChecked", ToJSON.toJSON x)).toList
    ++ (n.optionSelected.map fun x => ("optionSelected", ToJSON.toJSON x)).toList
    ++ (n.contentDocumentIndex.map fun x => ("contentDocumentIndex", ToJSON.toJSON x)).toList
    ++ (n.pseudoType.map fun x => ("pseudoType", ToJSON.toJSON x)).toList
    ++ (n.pseudoIdentifier.map fun x => ("pseudoIdentifier", ToJSON.toJSON x)).toList
    ++ (n.isClickable.map fun x => ("isClickable", ToJSON.toJSON x)).toList
    ++ (n.currentSourceURL.map fun x => ("currentSourceURL", ToJSON.toJSON x)).toList
    ++ (n.originURL.map fun x => ("originURL", ToJSON.toJSON x)).toList

/-- Table of details of an element in the DOM tree with a `LayoutObject`. -/
structure LayoutTreeSnapshot where
  /-- Index of the corresponding node in the `NodeTreeSnapshot` array
      returned by `captureSnapshot`. -/
  nodeIndex : List Int
  /-- Array of indexes specifying computed style strings, filtered
      according to the `computedStyles` parameter passed to
      `captureSnapshot`. -/
  styles : List ArrayOfStrings
  /-- The absolute position bounding box. -/
  bounds : List Rectangle
  /-- Contents of the `LayoutText`, if any. -/
  text : List StringIndex
  /-- Stacking context information. -/
  stackingContexts : RareBooleanData
  /-- Global paint order index, which is determined by the stacking order of
      the nodes. Nodes that are painted together will have the same index.
      Only provided if `includePaintOrder` in `captureSnapshot` was true. -/
  paintOrders : Option (List Int) := none
  /-- The offset rect of nodes. Only available when `includeDOMRects` is set
      to true. -/
  offsetRects : Option (List Rectangle) := none
  /-- The scroll rect of nodes. Only available when `includeDOMRects` is set
      to true. -/
  scrollRects : Option (List Rectangle) := none
  /-- The client rect of nodes. Only available when `includeDOMRects` is set
      to true. -/
  clientRects : Option (List Rectangle) := none
  /-- The list of background colors that are blended with colors of
      overlapping elements. -/
  blendedBackgroundColors : Option (List StringIndex) := none
  /-- The list of computed text opacities. -/
  textColorOpacities : Option (List Float) := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON LayoutTreeSnapshot where
  parseJSON v := do
    .ok
      { nodeIndex := ← Value.getField v "nodeIndex" >>= FromJSON.parseJSON
        styles := ← Value.getField v "styles" >>= FromJSON.parseJSON
        bounds := ← Value.getField v "bounds" >>= FromJSON.parseJSON
        text := ← Value.getField v "text" >>= FromJSON.parseJSON
        stackingContexts := ← Value.getField v "stackingContexts" >>= FromJSON.parseJSON
        paintOrders := ← (← Value.getFieldOpt v "paintOrders").mapM FromJSON.parseJSON
        offsetRects := ← (← Value.getFieldOpt v "offsetRects").mapM FromJSON.parseJSON
        scrollRects := ← (← Value.getFieldOpt v "scrollRects").mapM FromJSON.parseJSON
        clientRects := ← (← Value.getFieldOpt v "clientRects").mapM FromJSON.parseJSON
        blendedBackgroundColors := ← (← Value.getFieldOpt v "blendedBackgroundColors").mapM FromJSON.parseJSON
        textColorOpacities := ← (← Value.getFieldOpt v "textColorOpacities").mapM FromJSON.parseJSON }

instance : ToJSON LayoutTreeSnapshot where
  toJSON t := Data.Json.object <|
    [ ("nodeIndex", ToJSON.toJSON t.nodeIndex), ("styles", ToJSON.toJSON t.styles)
    , ("bounds", ToJSON.toJSON t.bounds), ("text", ToJSON.toJSON t.text)
    , ("stackingContexts", ToJSON.toJSON t.stackingContexts) ]
    ++ (t.paintOrders.map fun x => ("paintOrders", ToJSON.toJSON x)).toList
    ++ (t.offsetRects.map fun x => ("offsetRects", ToJSON.toJSON x)).toList
    ++ (t.scrollRects.map fun x => ("scrollRects", ToJSON.toJSON x)).toList
    ++ (t.clientRects.map fun x => ("clientRects", ToJSON.toJSON x)).toList
    ++ (t.blendedBackgroundColors.map fun x => ("blendedBackgroundColors", ToJSON.toJSON x)).toList
    ++ (t.textColorOpacities.map fun x => ("textColorOpacities", ToJSON.toJSON x)).toList

/-- Table of details of the post layout rendered text positions. The exact
    layout should not be regarded as stable and may change between
    versions. -/
structure TextBoxSnapshot where
  /-- Index of the layout tree node that owns this box collection. -/
  layoutIndex : List Int
  /-- The absolute position bounding box. -/
  bounds : List Rectangle
  /-- The starting index in characters, for this post layout textbox
      substring. Characters that would be represented as a surrogate pair in
      UTF-16 have length 2. -/
  start : List Int
  /-- The number of characters in this post layout textbox substring.
      Characters that would be represented as a surrogate pair in UTF-16
      have length 2. -/
  length : List Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON TextBoxSnapshot where
  parseJSON v := do
    .ok
      { layoutIndex := ← Value.getField v "layoutIndex" >>= FromJSON.parseJSON
        bounds := ← Value.getField v "bounds" >>= FromJSON.parseJSON
        start := ← Value.getField v "start" >>= FromJSON.parseJSON
        length := ← Value.getField v "length" >>= FromJSON.parseJSON }

instance : ToJSON TextBoxSnapshot where
  toJSON t := Data.Json.object
    [ ("layoutIndex", ToJSON.toJSON t.layoutIndex), ("bounds", ToJSON.toJSON t.bounds)
    , ("start", ToJSON.toJSON t.start), ("length", ToJSON.toJSON t.length) ]

/-- Document snapshot. -/
structure DocumentSnapshot where
  /-- Document URL that `Document` or `FrameOwner` node points to. -/
  documentURL : StringIndex
  /-- Document title. -/
  title : StringIndex
  /-- Base URL that `Document` or `FrameOwner` node uses for URL
      completion. -/
  baseURL : StringIndex
  /-- Contains the document's content language. -/
  contentLanguage : StringIndex
  /-- Contains the document's character set encoding. -/
  encodingName : StringIndex
  /-- `DocumentType` node's `publicId`. -/
  publicId : StringIndex
  /-- `DocumentType` node's `systemId`. -/
  systemId : StringIndex
  /-- Frame ID for frame owner elements and also for the document node. -/
  frameId : StringIndex
  /-- A table with dom nodes. -/
  nodes : NodeTreeSnapshot
  /-- The nodes in the layout tree. -/
  layout : LayoutTreeSnapshot
  /-- The post-layout inline text nodes. -/
  textBoxes : TextBoxSnapshot
  /-- Horizontal scroll offset. -/
  scrollOffsetX : Option Float := none
  /-- Vertical scroll offset. -/
  scrollOffsetY : Option Float := none
  /-- Document content width. -/
  contentWidth : Option Float := none
  /-- Document content height. -/
  contentHeight : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON DocumentSnapshot where
  parseJSON v := do
    .ok
      { documentURL := ← Value.getField v "documentURL" >>= FromJSON.parseJSON
        title := ← Value.getField v "title" >>= FromJSON.parseJSON
        baseURL := ← Value.getField v "baseURL" >>= FromJSON.parseJSON
        contentLanguage := ← Value.getField v "contentLanguage" >>= FromJSON.parseJSON
        encodingName := ← Value.getField v "encodingName" >>= FromJSON.parseJSON
        publicId := ← Value.getField v "publicId" >>= FromJSON.parseJSON
        systemId := ← Value.getField v "systemId" >>= FromJSON.parseJSON
        frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON
        layout := ← Value.getField v "layout" >>= FromJSON.parseJSON
        textBoxes := ← Value.getField v "textBoxes" >>= FromJSON.parseJSON
        scrollOffsetX := ← (← Value.getFieldOpt v "scrollOffsetX").mapM FromJSON.parseJSON
        scrollOffsetY := ← (← Value.getFieldOpt v "scrollOffsetY").mapM FromJSON.parseJSON
        contentWidth := ← (← Value.getFieldOpt v "contentWidth").mapM FromJSON.parseJSON
        contentHeight := ← (← Value.getFieldOpt v "contentHeight").mapM FromJSON.parseJSON }

instance : ToJSON DocumentSnapshot where
  toJSON d := Data.Json.object <|
    [ ("documentURL", ToJSON.toJSON d.documentURL), ("title", ToJSON.toJSON d.title)
    , ("baseURL", ToJSON.toJSON d.baseURL), ("contentLanguage", ToJSON.toJSON d.contentLanguage)
    , ("encodingName", ToJSON.toJSON d.encodingName), ("publicId", ToJSON.toJSON d.publicId)
    , ("systemId", ToJSON.toJSON d.systemId), ("frameId", ToJSON.toJSON d.frameId)
    , ("nodes", ToJSON.toJSON d.nodes), ("layout", ToJSON.toJSON d.layout)
    , ("textBoxes", ToJSON.toJSON d.textBoxes) ]
    ++ (d.scrollOffsetX.map fun x => ("scrollOffsetX", ToJSON.toJSON x)).toList
    ++ (d.scrollOffsetY.map fun x => ("scrollOffsetY", ToJSON.toJSON x)).toList
    ++ (d.contentWidth.map fun x => ("contentWidth", ToJSON.toJSON x)).toList
    ++ (d.contentHeight.map fun x => ("contentHeight", ToJSON.toJSON x)).toList

-- ── Commands ──

/-- Parameters of the `DOMSnapshot.disable` command: disables DOM snapshot
    agent for the given page. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "DOMSnapshot.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMSnapshot.enable` command: enables DOM snapshot
    agent for the given page. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "DOMSnapshot.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMSnapshot.captureSnapshot` command: returns a
    document snapshot, including the full DOM tree of the root node
    (including iframes, template contents, and imported documents) in a
    flattened array, as well as layout and white-listed computed style
    information for the nodes. Shadow DOM in the returned DOM tree is
    flattened. -/
structure PCaptureSnapshot where
  /-- Whitelist of computed styles to return. -/
  computedStyles : List String
  /-- Whether to include layout object paint orders into the snapshot. -/
  includePaintOrder : Option Bool := none
  /-- Whether to include DOM rectangles (`offsetRects`, `clientRects`,
      `scrollRects`) into the snapshot. -/
  includeDOMRects : Option Bool := none
  /-- Whether to include blended background colors in the snapshot
      (default: false). Blended background color is achieved by blending
      background colors of all elements that overlap with the current
      element. -/
  includeBlendedBackgroundColors : Option Bool := none
  /-- Whether to include text color opacity in the snapshot (default:
      false). An element might have the opacity property set that affects
      the text color of the element. The final text color opacity is
      computed based on the opacity of all overlapping elements. -/
  includeTextColorOpacities : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PCaptureSnapshot where
  toJSON p := Data.Json.object <|
    [ ("computedStyles", ToJSON.toJSON p.computedStyles) ]
    ++ (p.includePaintOrder.map fun x => ("includePaintOrder", ToJSON.toJSON x)).toList
    ++ (p.includeDOMRects.map fun x => ("includeDOMRects", ToJSON.toJSON x)).toList
    ++ (p.includeBlendedBackgroundColors.map fun x => ("includeBlendedBackgroundColors", ToJSON.toJSON x)).toList
    ++ (p.includeTextColorOpacities.map fun x => ("includeTextColorOpacities", ToJSON.toJSON x)).toList

/-- Response of the `DOMSnapshot.captureSnapshot` command. -/
structure CaptureSnapshot where
  /-- The nodes in the DOM tree. The `DOMNode` at index 0 corresponds to the
      root document. -/
  documents : List DocumentSnapshot
  /-- Shared string table that all string properties refer to with
      indexes. -/
  strings : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON CaptureSnapshot where
  parseJSON v := do
    .ok
      { documents := ← Value.getField v "documents" >>= FromJSON.parseJSON
        strings := ← Value.getField v "strings" >>= FromJSON.parseJSON }

instance : Command PCaptureSnapshot where
  Response := CaptureSnapshot
  commandName _ := "DOMSnapshot.captureSnapshot"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.DOMSnapshot
