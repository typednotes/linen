/-
  Linen.CDP.Domains.DOMPageNetworkEmulationSecurity — the DOM, Emulation,
  Network, Page and Security CDP domains.

  Ports `CDP.Domains.DOMPageNetworkEmulationSecurity` from cdp-hs, which bundles
  these five mutually-referential domains into a single module. The five are
  kept in one Lean module (mirroring upstream) and separated into the nested
  namespaces `DOM`, `Emulation`, `Network`, `Page` and `Security` under
  `CDP.Domains.DOMPageNetworkEmulationSecurity`. Upstream's flat,
  domain-prefixed names (`dOMNodeId`, `pageFrameId`, …) become `DOM.NodeId`,
  `Page.FrameId`, …; command-parameter records keep their `P` prefix
  (`DOM.PCopyTo`, `Page.PNavigate`, …), matching the `CDP.Domains.Memory`
  convention. Each type carries exactly the JSON codecs / `Command` / `Event`
  instances upstream gives it.

  Because Lean elaborates top-to-bottom and forbids forward references, the
  declarations are emitted in a single GLOBAL topological order across all five
  domains rather than domain-by-domain: every leaf type (ids, enums, small
  structs) precedes anything embedding it.

  Four types are genuinely self-referential — `DOM.Node`, `Page.FrameTree`,
  `Page.FrameResourceTree` and `Page.BackForwardCacheNotRestoredExplanationTree`
  — and their `FromJSON`/`ToJSON` are hand-written mutually-recursive `def`s
  with `sizeOf` termination proofs, following `CDP.Domains.HeapProfiler` and
  `CDP.Domains.IndexedDB`. Types transitively containing one of these (or
  `Runtime.RemoteObject`/`Runtime.StackTrace`, which lack `DecidableEq`) derive
  only `Repr, BEq`.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.Debugger
import Linen.CDP.Domains.IO
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.DOMPageNetworkEmulationSecurity

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

/-- `DOM.NodeId`. -/
abbrev DOM.NodeId := Int

/-- `DOM.BackendNodeId`. -/
abbrev DOM.BackendNodeId := Int

/-- `DOM.BackendNode`. -/
structure DOM.BackendNode where
  nodeType : Int
  nodeName : String
  backendNodeId : DOM.BackendNodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.BackendNode where
  parseJSON v := do
    .ok
      { nodeType := ← Value.getField v "nodeType" >>= FromJSON.parseJSON
        nodeName := ← Value.getField v "nodeName" >>= FromJSON.parseJSON
        backendNodeId := ← Value.getField v "backendNodeId" >>= FromJSON.parseJSON }
instance : ToJSON DOM.BackendNode where
  toJSON p := Data.Json.object <|
       [("nodeType", ToJSON.toJSON p.nodeType)]
    ++ [("nodeName", ToJSON.toJSON p.nodeName)]
    ++ [("backendNodeId", ToJSON.toJSON p.backendNodeId)]

/-- `DOM.PseudoType`. -/
inductive DOM.PseudoType where
  | firstLine | firstLetter | before | after | marker | backdrop | selection | targetText | spellingError | grammarError | highlight | firstLineInherited | scrollbar | scrollbarThumb | scrollbarButton | scrollbarTrack | scrollbarTrackPiece | scrollbarCorner | resizer | inputListButton | pageTransition | pageTransitionContainer | pageTransitionImageWrapper | pageTransitionOutgoingImage | pageTransitionIncomingImage
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.PseudoType where
  parseJSON
    | .string "first-line" => .ok .firstLine
    | .string "first-letter" => .ok .firstLetter
    | .string "before" => .ok .before
    | .string "after" => .ok .after
    | .string "marker" => .ok .marker
    | .string "backdrop" => .ok .backdrop
    | .string "selection" => .ok .selection
    | .string "target-text" => .ok .targetText
    | .string "spelling-error" => .ok .spellingError
    | .string "grammar-error" => .ok .grammarError
    | .string "highlight" => .ok .highlight
    | .string "first-line-inherited" => .ok .firstLineInherited
    | .string "scrollbar" => .ok .scrollbar
    | .string "scrollbar-thumb" => .ok .scrollbarThumb
    | .string "scrollbar-button" => .ok .scrollbarButton
    | .string "scrollbar-track" => .ok .scrollbarTrack
    | .string "scrollbar-track-piece" => .ok .scrollbarTrackPiece
    | .string "scrollbar-corner" => .ok .scrollbarCorner
    | .string "resizer" => .ok .resizer
    | .string "input-list-button" => .ok .inputListButton
    | .string "page-transition" => .ok .pageTransition
    | .string "page-transition-container" => .ok .pageTransitionContainer
    | .string "page-transition-image-wrapper" => .ok .pageTransitionImageWrapper
    | .string "page-transition-outgoing-image" => .ok .pageTransitionOutgoingImage
    | .string "page-transition-incoming-image" => .ok .pageTransitionIncomingImage
    | v => .error s!"failed to parse DOM.PseudoType: {repr v}"
instance : ToJSON DOM.PseudoType where
  toJSON
    | .firstLine => .string "first-line"
    | .firstLetter => .string "first-letter"
    | .before => .string "before"
    | .after => .string "after"
    | .marker => .string "marker"
    | .backdrop => .string "backdrop"
    | .selection => .string "selection"
    | .targetText => .string "target-text"
    | .spellingError => .string "spelling-error"
    | .grammarError => .string "grammar-error"
    | .highlight => .string "highlight"
    | .firstLineInherited => .string "first-line-inherited"
    | .scrollbar => .string "scrollbar"
    | .scrollbarThumb => .string "scrollbar-thumb"
    | .scrollbarButton => .string "scrollbar-button"
    | .scrollbarTrack => .string "scrollbar-track"
    | .scrollbarTrackPiece => .string "scrollbar-track-piece"
    | .scrollbarCorner => .string "scrollbar-corner"
    | .resizer => .string "resizer"
    | .inputListButton => .string "input-list-button"
    | .pageTransition => .string "page-transition"
    | .pageTransitionContainer => .string "page-transition-container"
    | .pageTransitionImageWrapper => .string "page-transition-image-wrapper"
    | .pageTransitionOutgoingImage => .string "page-transition-outgoing-image"
    | .pageTransitionIncomingImage => .string "page-transition-incoming-image"

/-- `DOM.ShadowRootType`. -/
inductive DOM.ShadowRootType where
  | userAgent | «open» | closed
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.ShadowRootType where
  parseJSON
    | .string "user-agent" => .ok .userAgent
    | .string "open" => .ok .«open»
    | .string "closed" => .ok .closed
    | v => .error s!"failed to parse DOM.ShadowRootType: {repr v}"
instance : ToJSON DOM.ShadowRootType where
  toJSON
    | .userAgent => .string "user-agent"
    | .«open» => .string "open"
    | .closed => .string "closed"

/-- `DOM.CompatibilityMode`. -/
inductive DOM.CompatibilityMode where
  | quirksMode | limitedQuirksMode | noQuirksMode
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.CompatibilityMode where
  parseJSON
    | .string "QuirksMode" => .ok .quirksMode
    | .string "LimitedQuirksMode" => .ok .limitedQuirksMode
    | .string "NoQuirksMode" => .ok .noQuirksMode
    | v => .error s!"failed to parse DOM.CompatibilityMode: {repr v}"
instance : ToJSON DOM.CompatibilityMode where
  toJSON
    | .quirksMode => .string "QuirksMode"
    | .limitedQuirksMode => .string "LimitedQuirksMode"
    | .noQuirksMode => .string "NoQuirksMode"

/-- `Page.FrameId`. -/
abbrev Page.FrameId := String

/-- `DOM.Node`. Self-referential; `FromJSON`/`ToJSON` are hand-written
    mutually-recursive `def`s with `sizeOf` termination proofs (see the
    module header and `CDP.Domains.HeapProfiler`). -/
structure DOM.Node where
  nodeId : DOM.NodeId
  parentId : Option DOM.NodeId := none
  backendNodeId : DOM.BackendNodeId
  nodeType : Int
  nodeName : String
  localName : String
  nodeValue : String
  childNodeCount : Option Int := none
  children : Option (List DOM.Node) := none
  attributes : Option (List String) := none
  documentURL : Option String := none
  baseURL : Option String := none
  publicId : Option String := none
  systemId : Option String := none
  internalSubset : Option String := none
  xmlVersion : Option String := none
  name : Option String := none
  value : Option String := none
  pseudoType : Option DOM.PseudoType := none
  pseudoIdentifier : Option String := none
  shadowRootType : Option DOM.ShadowRootType := none
  frameId : Option Page.FrameId := none
  contentDocument : Option DOM.Node := none
  shadowRoots : Option (List DOM.Node) := none
  templateContent : Option DOM.Node := none
  pseudoElements : Option (List DOM.Node) := none
  distributedNodes : Option (List DOM.BackendNode) := none
  isSVG : Option Bool := none
  compatibilityMode : Option DOM.CompatibilityMode := none
  assignedSlot : Option DOM.BackendNode := none
  deriving Repr, BEq
/-- Assemble a `DOM.Node` from its already-decoded recursive fields. -/
def DOM.finishNode (v : Value) (children : Option (List DOM.Node)) (contentDocument : Option DOM.Node) (shadowRoots : Option (List DOM.Node)) (templateContent : Option DOM.Node) (pseudoElements : Option (List DOM.Node)) : Except String DOM.Node := do
  .ok
    { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
      parentId := ← (← Value.getFieldOpt v "parentId").mapM FromJSON.parseJSON
      backendNodeId := ← Value.getField v "backendNodeId" >>= FromJSON.parseJSON
      nodeType := ← Value.getField v "nodeType" >>= FromJSON.parseJSON
      nodeName := ← Value.getField v "nodeName" >>= FromJSON.parseJSON
      localName := ← Value.getField v "localName" >>= FromJSON.parseJSON
      nodeValue := ← Value.getField v "nodeValue" >>= FromJSON.parseJSON
      childNodeCount := ← (← Value.getFieldOpt v "childNodeCount").mapM FromJSON.parseJSON
      children
      attributes := ← (← Value.getFieldOpt v "attributes").mapM FromJSON.parseJSON
      documentURL := ← (← Value.getFieldOpt v "documentURL").mapM FromJSON.parseJSON
      baseURL := ← (← Value.getFieldOpt v "baseURL").mapM FromJSON.parseJSON
      publicId := ← (← Value.getFieldOpt v "publicId").mapM FromJSON.parseJSON
      systemId := ← (← Value.getFieldOpt v "systemId").mapM FromJSON.parseJSON
      internalSubset := ← (← Value.getFieldOpt v "internalSubset").mapM FromJSON.parseJSON
      xmlVersion := ← (← Value.getFieldOpt v "xmlVersion").mapM FromJSON.parseJSON
      name := ← (← Value.getFieldOpt v "name").mapM FromJSON.parseJSON
      value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
      pseudoType := ← (← Value.getFieldOpt v "pseudoType").mapM FromJSON.parseJSON
      pseudoIdentifier := ← (← Value.getFieldOpt v "pseudoIdentifier").mapM FromJSON.parseJSON
      shadowRootType := ← (← Value.getFieldOpt v "shadowRootType").mapM FromJSON.parseJSON
      frameId := ← (← Value.getFieldOpt v "frameId").mapM FromJSON.parseJSON
      contentDocument
      shadowRoots
      templateContent
      pseudoElements
      distributedNodes := ← (← Value.getFieldOpt v "distributedNodes").mapM FromJSON.parseJSON
      isSVG := ← (← Value.getFieldOpt v "isSVG").mapM FromJSON.parseJSON
      compatibilityMode := ← (← Value.getFieldOpt v "compatibilityMode").mapM FromJSON.parseJSON
      assignedSlot := ← (← Value.getFieldOpt v "assignedSlot").mapM FromJSON.parseJSON }
mutual
/-- Decode a `DOM.Node`. -/
def DOM.parseNode (v : Value) : Except String DOM.Node := do
  let children ← match h0 : v.lookup "children" with
    | some x => (DOM.parseNodeList x).map some
    | none => .ok none
  let contentDocument ← match h1 : v.lookup "contentDocument" with
    | some .null => .ok none
    | some x => (DOM.parseNode x).map some
    | none => .ok none
  let shadowRoots ← match h2 : v.lookup "shadowRoots" with
    | some x => (DOM.parseNodeList x).map some
    | none => .ok none
  let templateContent ← match h3 : v.lookup "templateContent" with
    | some .null => .ok none
    | some x => (DOM.parseNode x).map some
    | none => .ok none
  let pseudoElements ← match h4 : v.lookup "pseudoElements" with
    | some x => (DOM.parseNodeList x).map some
    | none => .ok none
  DOM.finishNode v children contentDocument shadowRoots templateContent pseudoElements
termination_by sizeOf v
decreasing_by all_goals first | exact Value.lookup_sizeOf_lt h0 | exact Value.lookup_sizeOf_lt h1 | exact Value.lookup_sizeOf_lt h2 | exact Value.lookup_sizeOf_lt h3 | exact Value.lookup_sizeOf_lt h4
/-- Decode a JSON array of `DOM.Node`. -/
def DOM.parseNodeList (v : Value) : Except String (List DOM.Node) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => DOM.parseNode p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega
end
instance : FromJSON DOM.Node where parseJSON := DOM.parseNode
/-- `DOM.Node.children = some x` implies `x` is structurally smaller. -/
private theorem DOM.Node_children_sizeOf_lt {p : DOM.Node} {x : List DOM.Node}
    (h : p.children = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [DOM.Node.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega
/-- `DOM.Node.contentDocument = some x` implies `x` is structurally smaller. -/
private theorem DOM.Node_contentDocument_sizeOf_lt {p : DOM.Node} {x : DOM.Node}
    (h : p.contentDocument = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [DOM.Node.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega
/-- `DOM.Node.shadowRoots = some x` implies `x` is structurally smaller. -/
private theorem DOM.Node_shadowRoots_sizeOf_lt {p : DOM.Node} {x : List DOM.Node}
    (h : p.shadowRoots = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [DOM.Node.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega
/-- `DOM.Node.templateContent = some x` implies `x` is structurally smaller. -/
private theorem DOM.Node_templateContent_sizeOf_lt {p : DOM.Node} {x : DOM.Node}
    (h : p.templateContent = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [DOM.Node.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega
/-- `DOM.Node.pseudoElements = some x` implies `x` is structurally smaller. -/
private theorem DOM.Node_pseudoElements_sizeOf_lt {p : DOM.Node} {x : List DOM.Node}
    (h : p.pseudoElements = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [DOM.Node.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega
mutual
/-- Encode a `DOM.Node`. -/
def DOM.encodeNode (p : DOM.Node) : Value :=
  Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ (p.parentId.map (fun x => ("parentId", ToJSON.toJSON x))).toList
    ++ [("backendNodeId", ToJSON.toJSON p.backendNodeId)]
    ++ [("nodeType", ToJSON.toJSON p.nodeType)]
    ++ [("nodeName", ToJSON.toJSON p.nodeName)]
    ++ [("localName", ToJSON.toJSON p.localName)]
    ++ [("nodeValue", ToJSON.toJSON p.nodeValue)]
    ++ (p.childNodeCount.map (fun x => ("childNodeCount", ToJSON.toJSON x))).toList
    ++ (match h : p.children with | some x => [("children", DOM.encodeNodeList x)] | none => [])
    ++ (p.attributes.map (fun x => ("attributes", ToJSON.toJSON x))).toList
    ++ (p.documentURL.map (fun x => ("documentURL", ToJSON.toJSON x))).toList
    ++ (p.baseURL.map (fun x => ("baseURL", ToJSON.toJSON x))).toList
    ++ (p.publicId.map (fun x => ("publicId", ToJSON.toJSON x))).toList
    ++ (p.systemId.map (fun x => ("systemId", ToJSON.toJSON x))).toList
    ++ (p.internalSubset.map (fun x => ("internalSubset", ToJSON.toJSON x))).toList
    ++ (p.xmlVersion.map (fun x => ("xmlVersion", ToJSON.toJSON x))).toList
    ++ (p.name.map (fun x => ("name", ToJSON.toJSON x))).toList
    ++ (p.value.map (fun x => ("value", ToJSON.toJSON x))).toList
    ++ (p.pseudoType.map (fun x => ("pseudoType", ToJSON.toJSON x))).toList
    ++ (p.pseudoIdentifier.map (fun x => ("pseudoIdentifier", ToJSON.toJSON x))).toList
    ++ (p.shadowRootType.map (fun x => ("shadowRootType", ToJSON.toJSON x))).toList
    ++ (p.frameId.map (fun x => ("frameId", ToJSON.toJSON x))).toList
    ++ (match h : p.contentDocument with | some x => [("contentDocument", DOM.encodeNode x)] | none => [])
    ++ (match h : p.shadowRoots with | some x => [("shadowRoots", DOM.encodeNodeList x)] | none => [])
    ++ (match h : p.templateContent with | some x => [("templateContent", DOM.encodeNode x)] | none => [])
    ++ (match h : p.pseudoElements with | some x => [("pseudoElements", DOM.encodeNodeList x)] | none => [])
    ++ (p.distributedNodes.map (fun x => ("distributedNodes", ToJSON.toJSON x))).toList
    ++ (p.isSVG.map (fun x => ("isSVG", ToJSON.toJSON x))).toList
    ++ (p.compatibilityMode.map (fun x => ("compatibilityMode", ToJSON.toJSON x))).toList
    ++ (p.assignedSlot.map (fun x => ("assignedSlot", ToJSON.toJSON x))).toList
termination_by sizeOf p
decreasing_by
  all_goals first
    | exact DOM.Node_children_sizeOf_lt h
    | exact DOM.Node_contentDocument_sizeOf_lt h
    | exact DOM.Node_shadowRoots_sizeOf_lt h
    | exact DOM.Node_templateContent_sizeOf_lt h
    | exact DOM.Node_pseudoElements_sizeOf_lt h
    | (cases p; simp only [DOM.Node.mk.sizeOf_spec]; omega)
/-- Encode a list of `DOM.Node`. -/
def DOM.encodeNodeList (l : List DOM.Node) : Value :=
  Value.array (l.map DOM.encodeNode).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega
end
instance : ToJSON DOM.Node where toJSON := DOM.encodeNode

/-- `DOM.RGBA`. -/
structure DOM.RGBA where
  r : Int
  g : Int
  b : Int
  a : Option Float := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.RGBA where
  parseJSON v := do
    .ok
      { r := ← Value.getField v "r" >>= FromJSON.parseJSON
        g := ← Value.getField v "g" >>= FromJSON.parseJSON
        b := ← Value.getField v "b" >>= FromJSON.parseJSON
        a := ← (← Value.getFieldOpt v "a").mapM FromJSON.parseJSON }
instance : ToJSON DOM.RGBA where
  toJSON p := Data.Json.object <|
       [("r", ToJSON.toJSON p.r)]
    ++ [("g", ToJSON.toJSON p.g)]
    ++ [("b", ToJSON.toJSON p.b)]
    ++ (p.a.map (fun x => ("a", ToJSON.toJSON x))).toList

/-- `DOM.Quad`. -/
abbrev DOM.Quad := List Float

/-- `DOM.ShapeOutsideInfo`. -/
structure DOM.ShapeOutsideInfo where
  bounds : DOM.Quad
  shape : List Data.Json.Value
  marginShape : List Data.Json.Value
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.ShapeOutsideInfo where
  parseJSON v := do
    .ok
      { bounds := ← Value.getField v "bounds" >>= FromJSON.parseJSON
        shape := ← Value.getField v "shape" >>= FromJSON.parseJSON
        marginShape := ← Value.getField v "marginShape" >>= FromJSON.parseJSON }
instance : ToJSON DOM.ShapeOutsideInfo where
  toJSON p := Data.Json.object <|
       [("bounds", ToJSON.toJSON p.bounds)]
    ++ [("shape", ToJSON.toJSON p.shape)]
    ++ [("marginShape", ToJSON.toJSON p.marginShape)]

/-- `DOM.BoxModel`. -/
structure DOM.BoxModel where
  content : DOM.Quad
  padding : DOM.Quad
  border : DOM.Quad
  margin : DOM.Quad
  width : Int
  height : Int
  shapeOutside : Option DOM.ShapeOutsideInfo := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.BoxModel where
  parseJSON v := do
    .ok
      { content := ← Value.getField v "content" >>= FromJSON.parseJSON
        padding := ← Value.getField v "padding" >>= FromJSON.parseJSON
        border := ← Value.getField v "border" >>= FromJSON.parseJSON
        margin := ← Value.getField v "margin" >>= FromJSON.parseJSON
        width := ← Value.getField v "width" >>= FromJSON.parseJSON
        height := ← Value.getField v "height" >>= FromJSON.parseJSON
        shapeOutside := ← (← Value.getFieldOpt v "shapeOutside").mapM FromJSON.parseJSON }
instance : ToJSON DOM.BoxModel where
  toJSON p := Data.Json.object <|
       [("content", ToJSON.toJSON p.content)]
    ++ [("padding", ToJSON.toJSON p.padding)]
    ++ [("border", ToJSON.toJSON p.border)]
    ++ [("margin", ToJSON.toJSON p.margin)]
    ++ [("width", ToJSON.toJSON p.width)]
    ++ [("height", ToJSON.toJSON p.height)]
    ++ (p.shapeOutside.map (fun x => ("shapeOutside", ToJSON.toJSON x))).toList

/-- `DOM.Rect`. -/
structure DOM.Rect where
  x : Float
  y : Float
  width : Float
  height : Float
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.Rect where
  parseJSON v := do
    .ok
      { x := ← Value.getField v "x" >>= FromJSON.parseJSON
        y := ← Value.getField v "y" >>= FromJSON.parseJSON
        width := ← Value.getField v "width" >>= FromJSON.parseJSON
        height := ← Value.getField v "height" >>= FromJSON.parseJSON }
instance : ToJSON DOM.Rect where
  toJSON p := Data.Json.object <|
       [("x", ToJSON.toJSON p.x)]
    ++ [("y", ToJSON.toJSON p.y)]
    ++ [("width", ToJSON.toJSON p.width)]
    ++ [("height", ToJSON.toJSON p.height)]

/-- `DOM.CSSComputedStyleProperty`. -/
structure DOM.CSSComputedStyleProperty where
  name : String
  value : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.CSSComputedStyleProperty where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }
instance : ToJSON DOM.CSSComputedStyleProperty where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ [("value", ToJSON.toJSON p.value)]

/-- `DOM.AttributeModified`. -/
structure DOM.AttributeModified where
  nodeId : DOM.NodeId
  name : String
  value : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.AttributeModified where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }
instance : Event DOM.AttributeModified where
  eventName := "DOM.attributeModified"

/-- `DOM.AttributeRemoved`. -/
structure DOM.AttributeRemoved where
  nodeId : DOM.NodeId
  name : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.AttributeRemoved where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON }
instance : Event DOM.AttributeRemoved where
  eventName := "DOM.attributeRemoved"

/-- `DOM.CharacterDataModified`. -/
structure DOM.CharacterDataModified where
  nodeId : DOM.NodeId
  characterData : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.CharacterDataModified where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        characterData := ← Value.getField v "characterData" >>= FromJSON.parseJSON }
instance : Event DOM.CharacterDataModified where
  eventName := "DOM.characterDataModified"

/-- `DOM.ChildNodeCountUpdated`. -/
structure DOM.ChildNodeCountUpdated where
  nodeId : DOM.NodeId
  childNodeCount : Int
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.ChildNodeCountUpdated where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        childNodeCount := ← Value.getField v "childNodeCount" >>= FromJSON.parseJSON }
instance : Event DOM.ChildNodeCountUpdated where
  eventName := "DOM.childNodeCountUpdated"

/-- `DOM.ChildNodeInserted`. -/
structure DOM.ChildNodeInserted where
  parentNodeId : DOM.NodeId
  previousNodeId : DOM.NodeId
  node : DOM.Node
  deriving Repr, BEq
instance : FromJSON DOM.ChildNodeInserted where
  parseJSON v := do
    .ok
      { parentNodeId := ← Value.getField v "parentNodeId" >>= FromJSON.parseJSON
        previousNodeId := ← Value.getField v "previousNodeId" >>= FromJSON.parseJSON
        node := ← Value.getField v "node" >>= FromJSON.parseJSON }
instance : Event DOM.ChildNodeInserted where
  eventName := "DOM.childNodeInserted"

/-- `DOM.ChildNodeRemoved`. -/
structure DOM.ChildNodeRemoved where
  parentNodeId : DOM.NodeId
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.ChildNodeRemoved where
  parseJSON v := do
    .ok
      { parentNodeId := ← Value.getField v "parentNodeId" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }
instance : Event DOM.ChildNodeRemoved where
  eventName := "DOM.childNodeRemoved"

/-- `DOM.DistributedNodesUpdated`. -/
structure DOM.DistributedNodesUpdated where
  insertionPointId : DOM.NodeId
  distributedNodes : List DOM.BackendNode
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.DistributedNodesUpdated where
  parseJSON v := do
    .ok
      { insertionPointId := ← Value.getField v "insertionPointId" >>= FromJSON.parseJSON
        distributedNodes := ← Value.getField v "distributedNodes" >>= FromJSON.parseJSON }
instance : Event DOM.DistributedNodesUpdated where
  eventName := "DOM.distributedNodesUpdated"

/-- `DOM.DocumentUpdated`. -/
structure DOM.DocumentUpdated where
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.DocumentUpdated where parseJSON _ := .ok {}
instance : Event DOM.DocumentUpdated where
  eventName := "DOM.documentUpdated"

/-- `DOM.InlineStyleInvalidated`. -/
structure DOM.InlineStyleInvalidated where
  nodeIds : List DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.InlineStyleInvalidated where
  parseJSON v := do
    .ok
      { nodeIds := ← Value.getField v "nodeIds" >>= FromJSON.parseJSON }
instance : Event DOM.InlineStyleInvalidated where
  eventName := "DOM.inlineStyleInvalidated"

/-- `DOM.PseudoElementAdded`. -/
structure DOM.PseudoElementAdded where
  parentId : DOM.NodeId
  pseudoElement : DOM.Node
  deriving Repr, BEq
instance : FromJSON DOM.PseudoElementAdded where
  parseJSON v := do
    .ok
      { parentId := ← Value.getField v "parentId" >>= FromJSON.parseJSON
        pseudoElement := ← Value.getField v "pseudoElement" >>= FromJSON.parseJSON }
instance : Event DOM.PseudoElementAdded where
  eventName := "DOM.pseudoElementAdded"

/-- `DOM.TopLayerElementsUpdated`. -/
structure DOM.TopLayerElementsUpdated where
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.TopLayerElementsUpdated where parseJSON _ := .ok {}
instance : Event DOM.TopLayerElementsUpdated where
  eventName := "DOM.topLayerElementsUpdated"

/-- `DOM.PseudoElementRemoved`. -/
structure DOM.PseudoElementRemoved where
  parentId : DOM.NodeId
  pseudoElementId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.PseudoElementRemoved where
  parseJSON v := do
    .ok
      { parentId := ← Value.getField v "parentId" >>= FromJSON.parseJSON
        pseudoElementId := ← Value.getField v "pseudoElementId" >>= FromJSON.parseJSON }
instance : Event DOM.PseudoElementRemoved where
  eventName := "DOM.pseudoElementRemoved"

/-- `DOM.SetChildNodes`. -/
structure DOM.SetChildNodes where
  parentId : DOM.NodeId
  nodes : List DOM.Node
  deriving Repr, BEq
instance : FromJSON DOM.SetChildNodes where
  parseJSON v := do
    .ok
      { parentId := ← Value.getField v "parentId" >>= FromJSON.parseJSON
        nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON }
instance : Event DOM.SetChildNodes where
  eventName := "DOM.setChildNodes"

/-- `DOM.ShadowRootPopped`. -/
structure DOM.ShadowRootPopped where
  hostId : DOM.NodeId
  rootId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.ShadowRootPopped where
  parseJSON v := do
    .ok
      { hostId := ← Value.getField v "hostId" >>= FromJSON.parseJSON
        rootId := ← Value.getField v "rootId" >>= FromJSON.parseJSON }
instance : Event DOM.ShadowRootPopped where
  eventName := "DOM.shadowRootPopped"

/-- `DOM.ShadowRootPushed`. -/
structure DOM.ShadowRootPushed where
  hostId : DOM.NodeId
  root : DOM.Node
  deriving Repr, BEq
instance : FromJSON DOM.ShadowRootPushed where
  parseJSON v := do
    .ok
      { hostId := ← Value.getField v "hostId" >>= FromJSON.parseJSON
        root := ← Value.getField v "root" >>= FromJSON.parseJSON }
instance : Event DOM.ShadowRootPushed where
  eventName := "DOM.shadowRootPushed"

/-- `DOM.CollectClassNamesFromSubtree`. -/
structure DOM.CollectClassNamesFromSubtree where
  classNames : List String
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.CollectClassNamesFromSubtree where
  parseJSON v := do
    .ok
      { classNames := ← Value.getField v "classNames" >>= FromJSON.parseJSON }

/-- `DOM.PCollectClassNamesFromSubtree`. -/
structure DOM.PCollectClassNamesFromSubtree where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PCollectClassNamesFromSubtree where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
instance : Command DOM.PCollectClassNamesFromSubtree where
  Response := DOM.CollectClassNamesFromSubtree
  commandName _ := "DOM.collectClassNamesFromSubtree"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.CopyTo`. -/
structure DOM.CopyTo where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.CopyTo where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

/-- `DOM.PCopyTo`. -/
structure DOM.PCopyTo where
  nodeId : DOM.NodeId
  targetNodeId : DOM.NodeId
  insertBeforeNodeId : Option DOM.NodeId := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PCopyTo where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("targetNodeId", ToJSON.toJSON p.targetNodeId)]
    ++ (p.insertBeforeNodeId.map (fun x => ("insertBeforeNodeId", ToJSON.toJSON x))).toList
instance : Command DOM.PCopyTo where
  Response := DOM.CopyTo
  commandName _ := "DOM.copyTo"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.DescribeNode`. -/
structure DOM.DescribeNode where
  node : DOM.Node
  deriving Repr, BEq
instance : FromJSON DOM.DescribeNode where
  parseJSON v := do
    .ok
      { node := ← Value.getField v "node" >>= FromJSON.parseJSON }

/-- `DOM.PDescribeNode`. -/
structure DOM.PDescribeNode where
  nodeId : Option DOM.NodeId := none
  backendNodeId : Option DOM.BackendNodeId := none
  objectId : Option Runtime.RemoteObjectId := none
  depth : Option Int := none
  pierce : Option Bool := none
  deriving Repr, BEq
instance : ToJSON DOM.PDescribeNode where
  toJSON p := Data.Json.object <|
       (p.nodeId.map (fun x => ("nodeId", ToJSON.toJSON x))).toList
    ++ (p.backendNodeId.map (fun x => ("backendNodeId", ToJSON.toJSON x))).toList
    ++ (p.objectId.map (fun x => ("objectId", ToJSON.toJSON x))).toList
    ++ (p.depth.map (fun x => ("depth", ToJSON.toJSON x))).toList
    ++ (p.pierce.map (fun x => ("pierce", ToJSON.toJSON x))).toList
instance : Command DOM.PDescribeNode where
  Response := DOM.DescribeNode
  commandName _ := "DOM.describeNode"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PScrollIntoViewIfNeeded`. -/
structure DOM.PScrollIntoViewIfNeeded where
  nodeId : Option DOM.NodeId := none
  backendNodeId : Option DOM.BackendNodeId := none
  objectId : Option Runtime.RemoteObjectId := none
  rect : Option DOM.Rect := none
  deriving Repr, BEq
instance : ToJSON DOM.PScrollIntoViewIfNeeded where
  toJSON p := Data.Json.object <|
       (p.nodeId.map (fun x => ("nodeId", ToJSON.toJSON x))).toList
    ++ (p.backendNodeId.map (fun x => ("backendNodeId", ToJSON.toJSON x))).toList
    ++ (p.objectId.map (fun x => ("objectId", ToJSON.toJSON x))).toList
    ++ (p.rect.map (fun x => ("rect", ToJSON.toJSON x))).toList
instance : Command DOM.PScrollIntoViewIfNeeded where
  Response := Unit
  commandName _ := "DOM.scrollIntoViewIfNeeded"
  decodeResponse _ := .ok ()

/-- `DOM.PDisable`. -/
structure DOM.PDisable where
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PDisable where toJSON _ := .null
instance : Command DOM.PDisable where
  Response := Unit
  commandName _ := "DOM.disable"
  decodeResponse _ := .ok ()

/-- `DOM.PDiscardSearchResults`. -/
structure DOM.PDiscardSearchResults where
  searchId : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PDiscardSearchResults where
  toJSON p := Data.Json.object <|
       [("searchId", ToJSON.toJSON p.searchId)]
instance : Command DOM.PDiscardSearchResults where
  Response := Unit
  commandName _ := "DOM.discardSearchResults"
  decodeResponse _ := .ok ()

/-- `DOM.PEnableIncludeWhitespace`. -/
inductive DOM.PEnableIncludeWhitespace where
  | none | all
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.PEnableIncludeWhitespace where
  parseJSON
    | .string "none" => .ok .none
    | .string "all" => .ok .all
    | v => .error s!"failed to parse DOM.PEnableIncludeWhitespace: {repr v}"
instance : ToJSON DOM.PEnableIncludeWhitespace where
  toJSON
    | .none => .string "none"
    | .all => .string "all"

/-- `DOM.PEnable`. -/
structure DOM.PEnable where
  includeWhitespace : Option DOM.PEnableIncludeWhitespace := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PEnable where
  toJSON p := Data.Json.object <|
       (p.includeWhitespace.map (fun x => ("includeWhitespace", ToJSON.toJSON x))).toList
instance : Command DOM.PEnable where
  Response := Unit
  commandName _ := "DOM.enable"
  decodeResponse _ := .ok ()

/-- `DOM.PFocus`. -/
structure DOM.PFocus where
  nodeId : Option DOM.NodeId := none
  backendNodeId : Option DOM.BackendNodeId := none
  objectId : Option Runtime.RemoteObjectId := none
  deriving Repr, BEq
instance : ToJSON DOM.PFocus where
  toJSON p := Data.Json.object <|
       (p.nodeId.map (fun x => ("nodeId", ToJSON.toJSON x))).toList
    ++ (p.backendNodeId.map (fun x => ("backendNodeId", ToJSON.toJSON x))).toList
    ++ (p.objectId.map (fun x => ("objectId", ToJSON.toJSON x))).toList
instance : Command DOM.PFocus where
  Response := Unit
  commandName _ := "DOM.focus"
  decodeResponse _ := .ok ()

/-- `DOM.GetAttributes`. -/
structure DOM.GetAttributes where
  attributes : List String
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetAttributes where
  parseJSON v := do
    .ok
      { attributes := ← Value.getField v "attributes" >>= FromJSON.parseJSON }

/-- `DOM.PGetAttributes`. -/
structure DOM.PGetAttributes where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetAttributes where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
instance : Command DOM.PGetAttributes where
  Response := DOM.GetAttributes
  commandName _ := "DOM.getAttributes"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetBoxModel`. -/
structure DOM.GetBoxModel where
  model : DOM.BoxModel
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetBoxModel where
  parseJSON v := do
    .ok
      { model := ← Value.getField v "model" >>= FromJSON.parseJSON }

/-- `DOM.PGetBoxModel`. -/
structure DOM.PGetBoxModel where
  nodeId : Option DOM.NodeId := none
  backendNodeId : Option DOM.BackendNodeId := none
  objectId : Option Runtime.RemoteObjectId := none
  deriving Repr, BEq
instance : ToJSON DOM.PGetBoxModel where
  toJSON p := Data.Json.object <|
       (p.nodeId.map (fun x => ("nodeId", ToJSON.toJSON x))).toList
    ++ (p.backendNodeId.map (fun x => ("backendNodeId", ToJSON.toJSON x))).toList
    ++ (p.objectId.map (fun x => ("objectId", ToJSON.toJSON x))).toList
instance : Command DOM.PGetBoxModel where
  Response := DOM.GetBoxModel
  commandName _ := "DOM.getBoxModel"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetContentQuads`. -/
structure DOM.GetContentQuads where
  quads : List DOM.Quad
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetContentQuads where
  parseJSON v := do
    .ok
      { quads := ← Value.getField v "quads" >>= FromJSON.parseJSON }

/-- `DOM.PGetContentQuads`. -/
structure DOM.PGetContentQuads where
  nodeId : Option DOM.NodeId := none
  backendNodeId : Option DOM.BackendNodeId := none
  objectId : Option Runtime.RemoteObjectId := none
  deriving Repr, BEq
instance : ToJSON DOM.PGetContentQuads where
  toJSON p := Data.Json.object <|
       (p.nodeId.map (fun x => ("nodeId", ToJSON.toJSON x))).toList
    ++ (p.backendNodeId.map (fun x => ("backendNodeId", ToJSON.toJSON x))).toList
    ++ (p.objectId.map (fun x => ("objectId", ToJSON.toJSON x))).toList
instance : Command DOM.PGetContentQuads where
  Response := DOM.GetContentQuads
  commandName _ := "DOM.getContentQuads"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetDocument`. -/
structure DOM.GetDocument where
  root : DOM.Node
  deriving Repr, BEq
instance : FromJSON DOM.GetDocument where
  parseJSON v := do
    .ok
      { root := ← Value.getField v "root" >>= FromJSON.parseJSON }

/-- `DOM.PGetDocument`. -/
structure DOM.PGetDocument where
  depth : Option Int := none
  pierce : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetDocument where
  toJSON p := Data.Json.object <|
       (p.depth.map (fun x => ("depth", ToJSON.toJSON x))).toList
    ++ (p.pierce.map (fun x => ("pierce", ToJSON.toJSON x))).toList
instance : Command DOM.PGetDocument where
  Response := DOM.GetDocument
  commandName _ := "DOM.getDocument"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetNodesForSubtreeByStyle`. -/
structure DOM.GetNodesForSubtreeByStyle where
  nodeIds : List DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetNodesForSubtreeByStyle where
  parseJSON v := do
    .ok
      { nodeIds := ← Value.getField v "nodeIds" >>= FromJSON.parseJSON }

/-- `DOM.PGetNodesForSubtreeByStyle`. -/
structure DOM.PGetNodesForSubtreeByStyle where
  nodeId : DOM.NodeId
  computedStyles : List DOM.CSSComputedStyleProperty
  pierce : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetNodesForSubtreeByStyle where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("computedStyles", ToJSON.toJSON p.computedStyles)]
    ++ (p.pierce.map (fun x => ("pierce", ToJSON.toJSON x))).toList
instance : Command DOM.PGetNodesForSubtreeByStyle where
  Response := DOM.GetNodesForSubtreeByStyle
  commandName _ := "DOM.getNodesForSubtreeByStyle"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetNodeForLocation`. -/
structure DOM.GetNodeForLocation where
  backendNodeId : DOM.BackendNodeId
  frameId : Page.FrameId
  nodeId : Option DOM.NodeId := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetNodeForLocation where
  parseJSON v := do
    .ok
      { backendNodeId := ← Value.getField v "backendNodeId" >>= FromJSON.parseJSON
        frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        nodeId := ← (← Value.getFieldOpt v "nodeId").mapM FromJSON.parseJSON }

/-- `DOM.PGetNodeForLocation`. -/
structure DOM.PGetNodeForLocation where
  x : Int
  y : Int
  includeUserAgentShadowDOM : Option Bool := none
  ignorePointerEventsNone : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetNodeForLocation where
  toJSON p := Data.Json.object <|
       [("x", ToJSON.toJSON p.x)]
    ++ [("y", ToJSON.toJSON p.y)]
    ++ (p.includeUserAgentShadowDOM.map (fun x => ("includeUserAgentShadowDOM", ToJSON.toJSON x))).toList
    ++ (p.ignorePointerEventsNone.map (fun x => ("ignorePointerEventsNone", ToJSON.toJSON x))).toList
instance : Command DOM.PGetNodeForLocation where
  Response := DOM.GetNodeForLocation
  commandName _ := "DOM.getNodeForLocation"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetOuterHTML`. -/
structure DOM.GetOuterHTML where
  outerHTML : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetOuterHTML where
  parseJSON v := do
    .ok
      { outerHTML := ← Value.getField v "outerHTML" >>= FromJSON.parseJSON }

/-- `DOM.PGetOuterHTML`. -/
structure DOM.PGetOuterHTML where
  nodeId : Option DOM.NodeId := none
  backendNodeId : Option DOM.BackendNodeId := none
  objectId : Option Runtime.RemoteObjectId := none
  deriving Repr, BEq
instance : ToJSON DOM.PGetOuterHTML where
  toJSON p := Data.Json.object <|
       (p.nodeId.map (fun x => ("nodeId", ToJSON.toJSON x))).toList
    ++ (p.backendNodeId.map (fun x => ("backendNodeId", ToJSON.toJSON x))).toList
    ++ (p.objectId.map (fun x => ("objectId", ToJSON.toJSON x))).toList
instance : Command DOM.PGetOuterHTML where
  Response := DOM.GetOuterHTML
  commandName _ := "DOM.getOuterHTML"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetRelayoutBoundary`. -/
structure DOM.GetRelayoutBoundary where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetRelayoutBoundary where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

/-- `DOM.PGetRelayoutBoundary`. -/
structure DOM.PGetRelayoutBoundary where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetRelayoutBoundary where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
instance : Command DOM.PGetRelayoutBoundary where
  Response := DOM.GetRelayoutBoundary
  commandName _ := "DOM.getRelayoutBoundary"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetSearchResults`. -/
structure DOM.GetSearchResults where
  nodeIds : List DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetSearchResults where
  parseJSON v := do
    .ok
      { nodeIds := ← Value.getField v "nodeIds" >>= FromJSON.parseJSON }

/-- `DOM.PGetSearchResults`. -/
structure DOM.PGetSearchResults where
  searchId : String
  fromIndex : Int
  toIndex : Int
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetSearchResults where
  toJSON p := Data.Json.object <|
       [("searchId", ToJSON.toJSON p.searchId)]
    ++ [("fromIndex", ToJSON.toJSON p.fromIndex)]
    ++ [("toIndex", ToJSON.toJSON p.toIndex)]
instance : Command DOM.PGetSearchResults where
  Response := DOM.GetSearchResults
  commandName _ := "DOM.getSearchResults"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PHideHighlight`. -/
structure DOM.PHideHighlight where
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PHideHighlight where toJSON _ := .null
instance : Command DOM.PHideHighlight where
  Response := Unit
  commandName _ := "DOM.hideHighlight"
  decodeResponse _ := .ok ()

/-- `DOM.PHighlightNode`. -/
structure DOM.PHighlightNode where
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PHighlightNode where toJSON _ := .null
instance : Command DOM.PHighlightNode where
  Response := Unit
  commandName _ := "DOM.highlightNode"
  decodeResponse _ := .ok ()

/-- `DOM.PHighlightRect`. -/
structure DOM.PHighlightRect where
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PHighlightRect where toJSON _ := .null
instance : Command DOM.PHighlightRect where
  Response := Unit
  commandName _ := "DOM.highlightRect"
  decodeResponse _ := .ok ()

/-- `DOM.PMarkUndoableState`. -/
structure DOM.PMarkUndoableState where
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PMarkUndoableState where toJSON _ := .null
instance : Command DOM.PMarkUndoableState where
  Response := Unit
  commandName _ := "DOM.markUndoableState"
  decodeResponse _ := .ok ()

/-- `DOM.MoveTo`. -/
structure DOM.MoveTo where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.MoveTo where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

/-- `DOM.PMoveTo`. -/
structure DOM.PMoveTo where
  nodeId : DOM.NodeId
  targetNodeId : DOM.NodeId
  insertBeforeNodeId : Option DOM.NodeId := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PMoveTo where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("targetNodeId", ToJSON.toJSON p.targetNodeId)]
    ++ (p.insertBeforeNodeId.map (fun x => ("insertBeforeNodeId", ToJSON.toJSON x))).toList
instance : Command DOM.PMoveTo where
  Response := DOM.MoveTo
  commandName _ := "DOM.moveTo"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PerformSearch`. -/
structure DOM.PerformSearch where
  searchId : String
  resultCount : Int
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.PerformSearch where
  parseJSON v := do
    .ok
      { searchId := ← Value.getField v "searchId" >>= FromJSON.parseJSON
        resultCount := ← Value.getField v "resultCount" >>= FromJSON.parseJSON }

/-- `DOM.PPerformSearch`. -/
structure DOM.PPerformSearch where
  query : String
  includeUserAgentShadowDOM : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PPerformSearch where
  toJSON p := Data.Json.object <|
       [("query", ToJSON.toJSON p.query)]
    ++ (p.includeUserAgentShadowDOM.map (fun x => ("includeUserAgentShadowDOM", ToJSON.toJSON x))).toList
instance : Command DOM.PPerformSearch where
  Response := DOM.PerformSearch
  commandName _ := "DOM.performSearch"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PushNodeByPathToFrontend`. -/
structure DOM.PushNodeByPathToFrontend where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.PushNodeByPathToFrontend where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

/-- `DOM.PPushNodeByPathToFrontend`. -/
structure DOM.PPushNodeByPathToFrontend where
  path : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PPushNodeByPathToFrontend where
  toJSON p := Data.Json.object <|
       [("path", ToJSON.toJSON p.path)]
instance : Command DOM.PPushNodeByPathToFrontend where
  Response := DOM.PushNodeByPathToFrontend
  commandName _ := "DOM.pushNodeByPathToFrontend"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PushNodesByBackendIdsToFrontend`. -/
structure DOM.PushNodesByBackendIdsToFrontend where
  nodeIds : List DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.PushNodesByBackendIdsToFrontend where
  parseJSON v := do
    .ok
      { nodeIds := ← Value.getField v "nodeIds" >>= FromJSON.parseJSON }

/-- `DOM.PPushNodesByBackendIdsToFrontend`. -/
structure DOM.PPushNodesByBackendIdsToFrontend where
  backendNodeIds : List DOM.BackendNodeId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PPushNodesByBackendIdsToFrontend where
  toJSON p := Data.Json.object <|
       [("backendNodeIds", ToJSON.toJSON p.backendNodeIds)]
instance : Command DOM.PPushNodesByBackendIdsToFrontend where
  Response := DOM.PushNodesByBackendIdsToFrontend
  commandName _ := "DOM.pushNodesByBackendIdsToFrontend"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.QuerySelector`. -/
structure DOM.QuerySelector where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.QuerySelector where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

/-- `DOM.PQuerySelector`. -/
structure DOM.PQuerySelector where
  nodeId : DOM.NodeId
  selector : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PQuerySelector where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("selector", ToJSON.toJSON p.selector)]
instance : Command DOM.PQuerySelector where
  Response := DOM.QuerySelector
  commandName _ := "DOM.querySelector"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.QuerySelectorAll`. -/
structure DOM.QuerySelectorAll where
  nodeIds : List DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.QuerySelectorAll where
  parseJSON v := do
    .ok
      { nodeIds := ← Value.getField v "nodeIds" >>= FromJSON.parseJSON }

/-- `DOM.PQuerySelectorAll`. -/
structure DOM.PQuerySelectorAll where
  nodeId : DOM.NodeId
  selector : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PQuerySelectorAll where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("selector", ToJSON.toJSON p.selector)]
instance : Command DOM.PQuerySelectorAll where
  Response := DOM.QuerySelectorAll
  commandName _ := "DOM.querySelectorAll"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetTopLayerElements`. -/
structure DOM.GetTopLayerElements where
  nodeIds : List DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetTopLayerElements where
  parseJSON v := do
    .ok
      { nodeIds := ← Value.getField v "nodeIds" >>= FromJSON.parseJSON }

/-- `DOM.PGetTopLayerElements`. -/
structure DOM.PGetTopLayerElements where
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetTopLayerElements where toJSON _ := .null
instance : Command DOM.PGetTopLayerElements where
  Response := DOM.GetTopLayerElements
  commandName _ := "DOM.getTopLayerElements"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PRedo`. -/
structure DOM.PRedo where
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PRedo where toJSON _ := .null
instance : Command DOM.PRedo where
  Response := Unit
  commandName _ := "DOM.redo"
  decodeResponse _ := .ok ()

/-- `DOM.PRemoveAttribute`. -/
structure DOM.PRemoveAttribute where
  nodeId : DOM.NodeId
  name : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PRemoveAttribute where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("name", ToJSON.toJSON p.name)]
instance : Command DOM.PRemoveAttribute where
  Response := Unit
  commandName _ := "DOM.removeAttribute"
  decodeResponse _ := .ok ()

/-- `DOM.PRemoveNode`. -/
structure DOM.PRemoveNode where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PRemoveNode where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
instance : Command DOM.PRemoveNode where
  Response := Unit
  commandName _ := "DOM.removeNode"
  decodeResponse _ := .ok ()

/-- `DOM.PRequestChildNodes`. -/
structure DOM.PRequestChildNodes where
  nodeId : DOM.NodeId
  depth : Option Int := none
  pierce : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PRequestChildNodes where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ (p.depth.map (fun x => ("depth", ToJSON.toJSON x))).toList
    ++ (p.pierce.map (fun x => ("pierce", ToJSON.toJSON x))).toList
instance : Command DOM.PRequestChildNodes where
  Response := Unit
  commandName _ := "DOM.requestChildNodes"
  decodeResponse _ := .ok ()

/-- `DOM.RequestNode`. -/
structure DOM.RequestNode where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.RequestNode where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

/-- `DOM.PRequestNode`. -/
structure DOM.PRequestNode where
  objectId : Runtime.RemoteObjectId
  deriving Repr, BEq
instance : ToJSON DOM.PRequestNode where
  toJSON p := Data.Json.object <|
       [("objectId", ToJSON.toJSON p.objectId)]
instance : Command DOM.PRequestNode where
  Response := DOM.RequestNode
  commandName _ := "DOM.requestNode"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.ResolveNode`. -/
structure DOM.ResolveNode where
  object : Runtime.RemoteObject
  deriving Repr, BEq
instance : FromJSON DOM.ResolveNode where
  parseJSON v := do
    .ok
      { object := ← Value.getField v "object" >>= FromJSON.parseJSON }

/-- `DOM.PResolveNode`. -/
structure DOM.PResolveNode where
  nodeId : Option DOM.NodeId := none
  backendNodeId : Option DOM.BackendNodeId := none
  objectGroup : Option String := none
  executionContextId : Option Runtime.ExecutionContextId := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PResolveNode where
  toJSON p := Data.Json.object <|
       (p.nodeId.map (fun x => ("nodeId", ToJSON.toJSON x))).toList
    ++ (p.backendNodeId.map (fun x => ("backendNodeId", ToJSON.toJSON x))).toList
    ++ (p.objectGroup.map (fun x => ("objectGroup", ToJSON.toJSON x))).toList
    ++ (p.executionContextId.map (fun x => ("executionContextId", ToJSON.toJSON x))).toList
instance : Command DOM.PResolveNode where
  Response := DOM.ResolveNode
  commandName _ := "DOM.resolveNode"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PSetAttributeValue`. -/
structure DOM.PSetAttributeValue where
  nodeId : DOM.NodeId
  name : String
  value : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PSetAttributeValue where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("name", ToJSON.toJSON p.name)]
    ++ [("value", ToJSON.toJSON p.value)]
instance : Command DOM.PSetAttributeValue where
  Response := Unit
  commandName _ := "DOM.setAttributeValue"
  decodeResponse _ := .ok ()

/-- `DOM.PSetAttributesAsText`. -/
structure DOM.PSetAttributesAsText where
  nodeId : DOM.NodeId
  text : String
  name : Option String := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PSetAttributesAsText where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("text", ToJSON.toJSON p.text)]
    ++ (p.name.map (fun x => ("name", ToJSON.toJSON x))).toList
instance : Command DOM.PSetAttributesAsText where
  Response := Unit
  commandName _ := "DOM.setAttributesAsText"
  decodeResponse _ := .ok ()

/-- `DOM.PSetFileInputFiles`. -/
structure DOM.PSetFileInputFiles where
  files : List String
  nodeId : Option DOM.NodeId := none
  backendNodeId : Option DOM.BackendNodeId := none
  objectId : Option Runtime.RemoteObjectId := none
  deriving Repr, BEq
instance : ToJSON DOM.PSetFileInputFiles where
  toJSON p := Data.Json.object <|
       [("files", ToJSON.toJSON p.files)]
    ++ (p.nodeId.map (fun x => ("nodeId", ToJSON.toJSON x))).toList
    ++ (p.backendNodeId.map (fun x => ("backendNodeId", ToJSON.toJSON x))).toList
    ++ (p.objectId.map (fun x => ("objectId", ToJSON.toJSON x))).toList
instance : Command DOM.PSetFileInputFiles where
  Response := Unit
  commandName _ := "DOM.setFileInputFiles"
  decodeResponse _ := .ok ()

/-- `DOM.PSetNodeStackTracesEnabled`. -/
structure DOM.PSetNodeStackTracesEnabled where
  enable : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PSetNodeStackTracesEnabled where
  toJSON p := Data.Json.object <|
       [("enable", ToJSON.toJSON p.enable)]
instance : Command DOM.PSetNodeStackTracesEnabled where
  Response := Unit
  commandName _ := "DOM.setNodeStackTracesEnabled"
  decodeResponse _ := .ok ()

/-- `DOM.GetNodeStackTraces`. -/
structure DOM.GetNodeStackTraces where
  creation : Option Runtime.StackTrace := none
  deriving Repr, BEq
instance : FromJSON DOM.GetNodeStackTraces where
  parseJSON v := do
    .ok
      { creation := ← (← Value.getFieldOpt v "creation").mapM FromJSON.parseJSON }

/-- `DOM.PGetNodeStackTraces`. -/
structure DOM.PGetNodeStackTraces where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetNodeStackTraces where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
instance : Command DOM.PGetNodeStackTraces where
  Response := DOM.GetNodeStackTraces
  commandName _ := "DOM.getNodeStackTraces"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetFileInfo`. -/
structure DOM.GetFileInfo where
  path : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetFileInfo where
  parseJSON v := do
    .ok
      { path := ← Value.getField v "path" >>= FromJSON.parseJSON }

/-- `DOM.PGetFileInfo`. -/
structure DOM.PGetFileInfo where
  objectId : Runtime.RemoteObjectId
  deriving Repr, BEq
instance : ToJSON DOM.PGetFileInfo where
  toJSON p := Data.Json.object <|
       [("objectId", ToJSON.toJSON p.objectId)]
instance : Command DOM.PGetFileInfo where
  Response := DOM.GetFileInfo
  commandName _ := "DOM.getFileInfo"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PSetInspectedNode`. -/
structure DOM.PSetInspectedNode where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PSetInspectedNode where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
instance : Command DOM.PSetInspectedNode where
  Response := Unit
  commandName _ := "DOM.setInspectedNode"
  decodeResponse _ := .ok ()

/-- `DOM.SetNodeName`. -/
structure DOM.SetNodeName where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.SetNodeName where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

/-- `DOM.PSetNodeName`. -/
structure DOM.PSetNodeName where
  nodeId : DOM.NodeId
  name : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PSetNodeName where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("name", ToJSON.toJSON p.name)]
instance : Command DOM.PSetNodeName where
  Response := DOM.SetNodeName
  commandName _ := "DOM.setNodeName"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.PSetNodeValue`. -/
structure DOM.PSetNodeValue where
  nodeId : DOM.NodeId
  value : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PSetNodeValue where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("value", ToJSON.toJSON p.value)]
instance : Command DOM.PSetNodeValue where
  Response := Unit
  commandName _ := "DOM.setNodeValue"
  decodeResponse _ := .ok ()

/-- `DOM.PSetOuterHTML`. -/
structure DOM.PSetOuterHTML where
  nodeId : DOM.NodeId
  outerHTML : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PSetOuterHTML where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("outerHTML", ToJSON.toJSON p.outerHTML)]
instance : Command DOM.PSetOuterHTML where
  Response := Unit
  commandName _ := "DOM.setOuterHTML"
  decodeResponse _ := .ok ()

/-- `DOM.PUndo`. -/
structure DOM.PUndo where
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PUndo where toJSON _ := .null
instance : Command DOM.PUndo where
  Response := Unit
  commandName _ := "DOM.undo"
  decodeResponse _ := .ok ()

/-- `DOM.GetFrameOwner`. -/
structure DOM.GetFrameOwner where
  backendNodeId : DOM.BackendNodeId
  nodeId : Option DOM.NodeId := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetFrameOwner where
  parseJSON v := do
    .ok
      { backendNodeId := ← Value.getField v "backendNodeId" >>= FromJSON.parseJSON
        nodeId := ← (← Value.getFieldOpt v "nodeId").mapM FromJSON.parseJSON }

/-- `DOM.PGetFrameOwner`. -/
structure DOM.PGetFrameOwner where
  frameId : Page.FrameId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetFrameOwner where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
instance : Command DOM.PGetFrameOwner where
  Response := DOM.GetFrameOwner
  commandName _ := "DOM.getFrameOwner"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetContainerForNode`. -/
structure DOM.GetContainerForNode where
  nodeId : Option DOM.NodeId := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetContainerForNode where
  parseJSON v := do
    .ok
      { nodeId := ← (← Value.getFieldOpt v "nodeId").mapM FromJSON.parseJSON }

/-- `DOM.PGetContainerForNode`. -/
structure DOM.PGetContainerForNode where
  nodeId : DOM.NodeId
  containerName : Option String := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetContainerForNode where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ (p.containerName.map (fun x => ("containerName", ToJSON.toJSON x))).toList
instance : Command DOM.PGetContainerForNode where
  Response := DOM.GetContainerForNode
  commandName _ := "DOM.getContainerForNode"
  decodeResponse := FromJSON.parseJSON

/-- `DOM.GetQueryingDescendantsForContainer`. -/
structure DOM.GetQueryingDescendantsForContainer where
  nodeIds : List DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : FromJSON DOM.GetQueryingDescendantsForContainer where
  parseJSON v := do
    .ok
      { nodeIds := ← Value.getField v "nodeIds" >>= FromJSON.parseJSON }

/-- `DOM.PGetQueryingDescendantsForContainer`. -/
structure DOM.PGetQueryingDescendantsForContainer where
  nodeId : DOM.NodeId
  deriving Repr, BEq, DecidableEq
instance : ToJSON DOM.PGetQueryingDescendantsForContainer where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
instance : Command DOM.PGetQueryingDescendantsForContainer where
  Response := DOM.GetQueryingDescendantsForContainer
  commandName _ := "DOM.getQueryingDescendantsForContainer"
  decodeResponse := FromJSON.parseJSON

/-- `Emulation.ScreenOrientationType`. -/
inductive Emulation.ScreenOrientationType where
  | portraitPrimary | portraitSecondary | landscapePrimary | landscapeSecondary
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.ScreenOrientationType where
  parseJSON
    | .string "portraitPrimary" => .ok .portraitPrimary
    | .string "portraitSecondary" => .ok .portraitSecondary
    | .string "landscapePrimary" => .ok .landscapePrimary
    | .string "landscapeSecondary" => .ok .landscapeSecondary
    | v => .error s!"failed to parse Emulation.ScreenOrientationType: {repr v}"
instance : ToJSON Emulation.ScreenOrientationType where
  toJSON
    | .portraitPrimary => .string "portraitPrimary"
    | .portraitSecondary => .string "portraitSecondary"
    | .landscapePrimary => .string "landscapePrimary"
    | .landscapeSecondary => .string "landscapeSecondary"

/-- `Emulation.ScreenOrientation`. -/
structure Emulation.ScreenOrientation where
  type : Emulation.ScreenOrientationType
  angle : Int
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.ScreenOrientation where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        angle := ← Value.getField v "angle" >>= FromJSON.parseJSON }
instance : ToJSON Emulation.ScreenOrientation where
  toJSON p := Data.Json.object <|
       [("type", ToJSON.toJSON p.type)]
    ++ [("angle", ToJSON.toJSON p.angle)]

/-- `Emulation.DisplayFeatureOrientation`. -/
inductive Emulation.DisplayFeatureOrientation where
  | vertical | horizontal
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.DisplayFeatureOrientation where
  parseJSON
    | .string "vertical" => .ok .vertical
    | .string "horizontal" => .ok .horizontal
    | v => .error s!"failed to parse Emulation.DisplayFeatureOrientation: {repr v}"
instance : ToJSON Emulation.DisplayFeatureOrientation where
  toJSON
    | .vertical => .string "vertical"
    | .horizontal => .string "horizontal"

/-- `Emulation.DisplayFeature`. -/
structure Emulation.DisplayFeature where
  orientation : Emulation.DisplayFeatureOrientation
  offset : Int
  maskLength : Int
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.DisplayFeature where
  parseJSON v := do
    .ok
      { orientation := ← Value.getField v "orientation" >>= FromJSON.parseJSON
        offset := ← Value.getField v "offset" >>= FromJSON.parseJSON
        maskLength := ← Value.getField v "maskLength" >>= FromJSON.parseJSON }
instance : ToJSON Emulation.DisplayFeature where
  toJSON p := Data.Json.object <|
       [("orientation", ToJSON.toJSON p.orientation)]
    ++ [("offset", ToJSON.toJSON p.offset)]
    ++ [("maskLength", ToJSON.toJSON p.maskLength)]

/-- `Emulation.MediaFeature`. -/
structure Emulation.MediaFeature where
  name : String
  value : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.MediaFeature where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }
instance : ToJSON Emulation.MediaFeature where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ [("value", ToJSON.toJSON p.value)]

/-- `Emulation.VirtualTimePolicy`. -/
inductive Emulation.VirtualTimePolicy where
  | advance | pause | pauseIfNetworkFetchesPending
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.VirtualTimePolicy where
  parseJSON
    | .string "advance" => .ok .advance
    | .string "pause" => .ok .pause
    | .string "pauseIfNetworkFetchesPending" => .ok .pauseIfNetworkFetchesPending
    | v => .error s!"failed to parse Emulation.VirtualTimePolicy: {repr v}"
instance : ToJSON Emulation.VirtualTimePolicy where
  toJSON
    | .advance => .string "advance"
    | .pause => .string "pause"
    | .pauseIfNetworkFetchesPending => .string "pauseIfNetworkFetchesPending"

/-- `Emulation.UserAgentBrandVersion`. -/
structure Emulation.UserAgentBrandVersion where
  brand : String
  version : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.UserAgentBrandVersion where
  parseJSON v := do
    .ok
      { brand := ← Value.getField v "brand" >>= FromJSON.parseJSON
        version := ← Value.getField v "version" >>= FromJSON.parseJSON }
instance : ToJSON Emulation.UserAgentBrandVersion where
  toJSON p := Data.Json.object <|
       [("brand", ToJSON.toJSON p.brand)]
    ++ [("version", ToJSON.toJSON p.version)]

/-- `Emulation.UserAgentMetadata`. -/
structure Emulation.UserAgentMetadata where
  brands : Option (List Emulation.UserAgentBrandVersion) := none
  fullVersionList : Option (List Emulation.UserAgentBrandVersion) := none
  platform : String
  platformVersion : String
  architecture : String
  model : String
  mobile : Bool
  bitness : Option String := none
  wow64 : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.UserAgentMetadata where
  parseJSON v := do
    .ok
      { brands := ← (← Value.getFieldOpt v "brands").mapM FromJSON.parseJSON
        fullVersionList := ← (← Value.getFieldOpt v "fullVersionList").mapM FromJSON.parseJSON
        platform := ← Value.getField v "platform" >>= FromJSON.parseJSON
        platformVersion := ← Value.getField v "platformVersion" >>= FromJSON.parseJSON
        architecture := ← Value.getField v "architecture" >>= FromJSON.parseJSON
        model := ← Value.getField v "model" >>= FromJSON.parseJSON
        mobile := ← Value.getField v "mobile" >>= FromJSON.parseJSON
        bitness := ← (← Value.getFieldOpt v "bitness").mapM FromJSON.parseJSON
        wow64 := ← (← Value.getFieldOpt v "wow64").mapM FromJSON.parseJSON }
instance : ToJSON Emulation.UserAgentMetadata where
  toJSON p := Data.Json.object <|
       (p.brands.map (fun x => ("brands", ToJSON.toJSON x))).toList
    ++ (p.fullVersionList.map (fun x => ("fullVersionList", ToJSON.toJSON x))).toList
    ++ [("platform", ToJSON.toJSON p.platform)]
    ++ [("platformVersion", ToJSON.toJSON p.platformVersion)]
    ++ [("architecture", ToJSON.toJSON p.architecture)]
    ++ [("model", ToJSON.toJSON p.model)]
    ++ [("mobile", ToJSON.toJSON p.mobile)]
    ++ (p.bitness.map (fun x => ("bitness", ToJSON.toJSON x))).toList
    ++ (p.wow64.map (fun x => ("wow64", ToJSON.toJSON x))).toList

/-- `Emulation.DisabledImageType`. -/
inductive Emulation.DisabledImageType where
  | avif | jxl | webp
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.DisabledImageType where
  parseJSON
    | .string "avif" => .ok .avif
    | .string "jxl" => .ok .jxl
    | .string "webp" => .ok .webp
    | v => .error s!"failed to parse Emulation.DisabledImageType: {repr v}"
instance : ToJSON Emulation.DisabledImageType where
  toJSON
    | .avif => .string "avif"
    | .jxl => .string "jxl"
    | .webp => .string "webp"

/-- `Emulation.VirtualTimeBudgetExpired`. -/
structure Emulation.VirtualTimeBudgetExpired where
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.VirtualTimeBudgetExpired where parseJSON _ := .ok {}
instance : Event Emulation.VirtualTimeBudgetExpired where
  eventName := "Emulation.virtualTimeBudgetExpired"

/-- `Emulation.CanEmulate`. -/
structure Emulation.CanEmulate where
  result : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.CanEmulate where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON }

/-- `Emulation.PCanEmulate`. -/
structure Emulation.PCanEmulate where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PCanEmulate where toJSON _ := .null
instance : Command Emulation.PCanEmulate where
  Response := Emulation.CanEmulate
  commandName _ := "Emulation.canEmulate"
  decodeResponse := FromJSON.parseJSON

/-- `Emulation.PClearDeviceMetricsOverride`. -/
structure Emulation.PClearDeviceMetricsOverride where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PClearDeviceMetricsOverride where toJSON _ := .null
instance : Command Emulation.PClearDeviceMetricsOverride where
  Response := Unit
  commandName _ := "Emulation.clearDeviceMetricsOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PClearGeolocationOverride`. -/
structure Emulation.PClearGeolocationOverride where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PClearGeolocationOverride where toJSON _ := .null
instance : Command Emulation.PClearGeolocationOverride where
  Response := Unit
  commandName _ := "Emulation.clearGeolocationOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PResetPageScaleFactor`. -/
structure Emulation.PResetPageScaleFactor where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PResetPageScaleFactor where toJSON _ := .null
instance : Command Emulation.PResetPageScaleFactor where
  Response := Unit
  commandName _ := "Emulation.resetPageScaleFactor"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetFocusEmulationEnabled`. -/
structure Emulation.PSetFocusEmulationEnabled where
  enabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetFocusEmulationEnabled where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
instance : Command Emulation.PSetFocusEmulationEnabled where
  Response := Unit
  commandName _ := "Emulation.setFocusEmulationEnabled"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetAutoDarkModeOverride`. -/
structure Emulation.PSetAutoDarkModeOverride where
  enabled : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetAutoDarkModeOverride where
  toJSON p := Data.Json.object <|
       (p.enabled.map (fun x => ("enabled", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetAutoDarkModeOverride where
  Response := Unit
  commandName _ := "Emulation.setAutoDarkModeOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetCPUThrottlingRate`. -/
structure Emulation.PSetCPUThrottlingRate where
  rate : Float
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetCPUThrottlingRate where
  toJSON p := Data.Json.object <|
       [("rate", ToJSON.toJSON p.rate)]
instance : Command Emulation.PSetCPUThrottlingRate where
  Response := Unit
  commandName _ := "Emulation.setCPUThrottlingRate"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetDefaultBackgroundColorOverride`. -/
structure Emulation.PSetDefaultBackgroundColorOverride where
  color : Option DOM.RGBA := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetDefaultBackgroundColorOverride where
  toJSON p := Data.Json.object <|
       (p.color.map (fun x => ("color", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetDefaultBackgroundColorOverride where
  Response := Unit
  commandName _ := "Emulation.setDefaultBackgroundColorOverride"
  decodeResponse _ := .ok ()

/-- `Page.Viewport`. -/
structure Page.Viewport where
  x : Float
  y : Float
  width : Float
  height : Float
  scale : Float
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.Viewport where
  parseJSON v := do
    .ok
      { x := ← Value.getField v "x" >>= FromJSON.parseJSON
        y := ← Value.getField v "y" >>= FromJSON.parseJSON
        width := ← Value.getField v "width" >>= FromJSON.parseJSON
        height := ← Value.getField v "height" >>= FromJSON.parseJSON
        scale := ← Value.getField v "scale" >>= FromJSON.parseJSON }
instance : ToJSON Page.Viewport where
  toJSON p := Data.Json.object <|
       [("x", ToJSON.toJSON p.x)]
    ++ [("y", ToJSON.toJSON p.y)]
    ++ [("width", ToJSON.toJSON p.width)]
    ++ [("height", ToJSON.toJSON p.height)]
    ++ [("scale", ToJSON.toJSON p.scale)]

/-- `Emulation.PSetDeviceMetricsOverride`. -/
structure Emulation.PSetDeviceMetricsOverride where
  width : Int
  height : Int
  deviceScaleFactor : Float
  mobile : Bool
  scale : Option Float := none
  screenWidth : Option Int := none
  screenHeight : Option Int := none
  positionX : Option Int := none
  positionY : Option Int := none
  dontSetVisibleSize : Option Bool := none
  screenOrientation : Option Emulation.ScreenOrientation := none
  viewport : Option Page.Viewport := none
  displayFeature : Option Emulation.DisplayFeature := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetDeviceMetricsOverride where
  toJSON p := Data.Json.object <|
       [("width", ToJSON.toJSON p.width)]
    ++ [("height", ToJSON.toJSON p.height)]
    ++ [("deviceScaleFactor", ToJSON.toJSON p.deviceScaleFactor)]
    ++ [("mobile", ToJSON.toJSON p.mobile)]
    ++ (p.scale.map (fun x => ("scale", ToJSON.toJSON x))).toList
    ++ (p.screenWidth.map (fun x => ("screenWidth", ToJSON.toJSON x))).toList
    ++ (p.screenHeight.map (fun x => ("screenHeight", ToJSON.toJSON x))).toList
    ++ (p.positionX.map (fun x => ("positionX", ToJSON.toJSON x))).toList
    ++ (p.positionY.map (fun x => ("positionY", ToJSON.toJSON x))).toList
    ++ (p.dontSetVisibleSize.map (fun x => ("dontSetVisibleSize", ToJSON.toJSON x))).toList
    ++ (p.screenOrientation.map (fun x => ("screenOrientation", ToJSON.toJSON x))).toList
    ++ (p.viewport.map (fun x => ("viewport", ToJSON.toJSON x))).toList
    ++ (p.displayFeature.map (fun x => ("displayFeature", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetDeviceMetricsOverride where
  Response := Unit
  commandName _ := "Emulation.setDeviceMetricsOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetScrollbarsHidden`. -/
structure Emulation.PSetScrollbarsHidden where
  hidden : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetScrollbarsHidden where
  toJSON p := Data.Json.object <|
       [("hidden", ToJSON.toJSON p.hidden)]
instance : Command Emulation.PSetScrollbarsHidden where
  Response := Unit
  commandName _ := "Emulation.setScrollbarsHidden"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetDocumentCookieDisabled`. -/
structure Emulation.PSetDocumentCookieDisabled where
  disabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetDocumentCookieDisabled where
  toJSON p := Data.Json.object <|
       [("disabled", ToJSON.toJSON p.disabled)]
instance : Command Emulation.PSetDocumentCookieDisabled where
  Response := Unit
  commandName _ := "Emulation.setDocumentCookieDisabled"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetEmitTouchEventsForMouseConfiguration`. -/
inductive Emulation.PSetEmitTouchEventsForMouseConfiguration where
  | mobile | desktop
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.PSetEmitTouchEventsForMouseConfiguration where
  parseJSON
    | .string "mobile" => .ok .mobile
    | .string "desktop" => .ok .desktop
    | v => .error s!"failed to parse Emulation.PSetEmitTouchEventsForMouseConfiguration: {repr v}"
instance : ToJSON Emulation.PSetEmitTouchEventsForMouseConfiguration where
  toJSON
    | .mobile => .string "mobile"
    | .desktop => .string "desktop"

/-- `Emulation.PSetEmitTouchEventsForMouse`. -/
structure Emulation.PSetEmitTouchEventsForMouse where
  enabled : Bool
  configuration : Option Emulation.PSetEmitTouchEventsForMouseConfiguration := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetEmitTouchEventsForMouse where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
    ++ (p.configuration.map (fun x => ("configuration", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetEmitTouchEventsForMouse where
  Response := Unit
  commandName _ := "Emulation.setEmitTouchEventsForMouse"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetEmulatedMedia`. -/
structure Emulation.PSetEmulatedMedia where
  media : Option String := none
  features : Option (List Emulation.MediaFeature) := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetEmulatedMedia where
  toJSON p := Data.Json.object <|
       (p.media.map (fun x => ("media", ToJSON.toJSON x))).toList
    ++ (p.features.map (fun x => ("features", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetEmulatedMedia where
  Response := Unit
  commandName _ := "Emulation.setEmulatedMedia"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetEmulatedVisionDeficiencyType`. -/
inductive Emulation.PSetEmulatedVisionDeficiencyType where
  | none | achromatopsia | blurredVision | deuteranopia | protanopia | tritanopia
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.PSetEmulatedVisionDeficiencyType where
  parseJSON
    | .string "none" => .ok .none
    | .string "achromatopsia" => .ok .achromatopsia
    | .string "blurredVision" => .ok .blurredVision
    | .string "deuteranopia" => .ok .deuteranopia
    | .string "protanopia" => .ok .protanopia
    | .string "tritanopia" => .ok .tritanopia
    | v => .error s!"failed to parse Emulation.PSetEmulatedVisionDeficiencyType: {repr v}"
instance : ToJSON Emulation.PSetEmulatedVisionDeficiencyType where
  toJSON
    | .none => .string "none"
    | .achromatopsia => .string "achromatopsia"
    | .blurredVision => .string "blurredVision"
    | .deuteranopia => .string "deuteranopia"
    | .protanopia => .string "protanopia"
    | .tritanopia => .string "tritanopia"

/-- `Emulation.PSetEmulatedVisionDeficiency`. -/
structure Emulation.PSetEmulatedVisionDeficiency where
  type : Emulation.PSetEmulatedVisionDeficiencyType
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetEmulatedVisionDeficiency where
  toJSON p := Data.Json.object <|
       [("type", ToJSON.toJSON p.type)]
instance : Command Emulation.PSetEmulatedVisionDeficiency where
  Response := Unit
  commandName _ := "Emulation.setEmulatedVisionDeficiency"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetGeolocationOverride`. -/
structure Emulation.PSetGeolocationOverride where
  latitude : Option Float := none
  longitude : Option Float := none
  accuracy : Option Float := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetGeolocationOverride where
  toJSON p := Data.Json.object <|
       (p.latitude.map (fun x => ("latitude", ToJSON.toJSON x))).toList
    ++ (p.longitude.map (fun x => ("longitude", ToJSON.toJSON x))).toList
    ++ (p.accuracy.map (fun x => ("accuracy", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetGeolocationOverride where
  Response := Unit
  commandName _ := "Emulation.setGeolocationOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetIdleOverride`. -/
structure Emulation.PSetIdleOverride where
  isUserActive : Bool
  isScreenUnlocked : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetIdleOverride where
  toJSON p := Data.Json.object <|
       [("isUserActive", ToJSON.toJSON p.isUserActive)]
    ++ [("isScreenUnlocked", ToJSON.toJSON p.isScreenUnlocked)]
instance : Command Emulation.PSetIdleOverride where
  Response := Unit
  commandName _ := "Emulation.setIdleOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PClearIdleOverride`. -/
structure Emulation.PClearIdleOverride where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PClearIdleOverride where toJSON _ := .null
instance : Command Emulation.PClearIdleOverride where
  Response := Unit
  commandName _ := "Emulation.clearIdleOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetPageScaleFactor`. -/
structure Emulation.PSetPageScaleFactor where
  pageScaleFactor : Float
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetPageScaleFactor where
  toJSON p := Data.Json.object <|
       [("pageScaleFactor", ToJSON.toJSON p.pageScaleFactor)]
instance : Command Emulation.PSetPageScaleFactor where
  Response := Unit
  commandName _ := "Emulation.setPageScaleFactor"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetScriptExecutionDisabled`. -/
structure Emulation.PSetScriptExecutionDisabled where
  value : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetScriptExecutionDisabled where
  toJSON p := Data.Json.object <|
       [("value", ToJSON.toJSON p.value)]
instance : Command Emulation.PSetScriptExecutionDisabled where
  Response := Unit
  commandName _ := "Emulation.setScriptExecutionDisabled"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetTouchEmulationEnabled`. -/
structure Emulation.PSetTouchEmulationEnabled where
  enabled : Bool
  maxTouchPoints : Option Int := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetTouchEmulationEnabled where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
    ++ (p.maxTouchPoints.map (fun x => ("maxTouchPoints", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetTouchEmulationEnabled where
  Response := Unit
  commandName _ := "Emulation.setTouchEmulationEnabled"
  decodeResponse _ := .ok ()

/-- `Emulation.SetVirtualTimePolicy`. -/
structure Emulation.SetVirtualTimePolicy where
  virtualTimeTicksBase : Float
  deriving Repr, BEq, DecidableEq
instance : FromJSON Emulation.SetVirtualTimePolicy where
  parseJSON v := do
    .ok
      { virtualTimeTicksBase := ← Value.getField v "virtualTimeTicksBase" >>= FromJSON.parseJSON }

/-- `Network.TimeSinceEpoch`. -/
abbrev Network.TimeSinceEpoch := Float

/-- `Emulation.PSetVirtualTimePolicy`. -/
structure Emulation.PSetVirtualTimePolicy where
  policy : Emulation.VirtualTimePolicy
  budget : Option Float := none
  maxVirtualTimeTaskStarvationCount : Option Int := none
  initialVirtualTime : Option Network.TimeSinceEpoch := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetVirtualTimePolicy where
  toJSON p := Data.Json.object <|
       [("policy", ToJSON.toJSON p.policy)]
    ++ (p.budget.map (fun x => ("budget", ToJSON.toJSON x))).toList
    ++ (p.maxVirtualTimeTaskStarvationCount.map (fun x => ("maxVirtualTimeTaskStarvationCount", ToJSON.toJSON x))).toList
    ++ (p.initialVirtualTime.map (fun x => ("initialVirtualTime", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetVirtualTimePolicy where
  Response := Emulation.SetVirtualTimePolicy
  commandName _ := "Emulation.setVirtualTimePolicy"
  decodeResponse := FromJSON.parseJSON

/-- `Emulation.PSetLocaleOverride`. -/
structure Emulation.PSetLocaleOverride where
  locale : Option String := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetLocaleOverride where
  toJSON p := Data.Json.object <|
       (p.locale.map (fun x => ("locale", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetLocaleOverride where
  Response := Unit
  commandName _ := "Emulation.setLocaleOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetTimezoneOverride`. -/
structure Emulation.PSetTimezoneOverride where
  timezoneId : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetTimezoneOverride where
  toJSON p := Data.Json.object <|
       [("timezoneId", ToJSON.toJSON p.timezoneId)]
instance : Command Emulation.PSetTimezoneOverride where
  Response := Unit
  commandName _ := "Emulation.setTimezoneOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetDisabledImageTypes`. -/
structure Emulation.PSetDisabledImageTypes where
  imageTypes : List Emulation.DisabledImageType
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetDisabledImageTypes where
  toJSON p := Data.Json.object <|
       [("imageTypes", ToJSON.toJSON p.imageTypes)]
instance : Command Emulation.PSetDisabledImageTypes where
  Response := Unit
  commandName _ := "Emulation.setDisabledImageTypes"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetHardwareConcurrencyOverride`. -/
structure Emulation.PSetHardwareConcurrencyOverride where
  hardwareConcurrency : Int
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetHardwareConcurrencyOverride where
  toJSON p := Data.Json.object <|
       [("hardwareConcurrency", ToJSON.toJSON p.hardwareConcurrency)]
instance : Command Emulation.PSetHardwareConcurrencyOverride where
  Response := Unit
  commandName _ := "Emulation.setHardwareConcurrencyOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetUserAgentOverride`. -/
structure Emulation.PSetUserAgentOverride where
  userAgent : String
  acceptLanguage : Option String := none
  platform : Option String := none
  userAgentMetadata : Option Emulation.UserAgentMetadata := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetUserAgentOverride where
  toJSON p := Data.Json.object <|
       [("userAgent", ToJSON.toJSON p.userAgent)]
    ++ (p.acceptLanguage.map (fun x => ("acceptLanguage", ToJSON.toJSON x))).toList
    ++ (p.platform.map (fun x => ("platform", ToJSON.toJSON x))).toList
    ++ (p.userAgentMetadata.map (fun x => ("userAgentMetadata", ToJSON.toJSON x))).toList
instance : Command Emulation.PSetUserAgentOverride where
  Response := Unit
  commandName _ := "Emulation.setUserAgentOverride"
  decodeResponse _ := .ok ()

/-- `Emulation.PSetAutomationOverride`. -/
structure Emulation.PSetAutomationOverride where
  enabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Emulation.PSetAutomationOverride where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
instance : Command Emulation.PSetAutomationOverride where
  Response := Unit
  commandName _ := "Emulation.setAutomationOverride"
  decodeResponse _ := .ok ()

/-- `Network.ResourceType`. -/
inductive Network.ResourceType where
  | document | stylesheet | image | media | font | script | textTrack | xHR | fetch | prefetch | eventSource | webSocket | manifest | signedExchange | ping | cSPViolationReport | preflight | other
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ResourceType where
  parseJSON
    | .string "Document" => .ok .document
    | .string "Stylesheet" => .ok .stylesheet
    | .string "Image" => .ok .image
    | .string "Media" => .ok .media
    | .string "Font" => .ok .font
    | .string "Script" => .ok .script
    | .string "TextTrack" => .ok .textTrack
    | .string "XHR" => .ok .xHR
    | .string "Fetch" => .ok .fetch
    | .string "Prefetch" => .ok .prefetch
    | .string "EventSource" => .ok .eventSource
    | .string "WebSocket" => .ok .webSocket
    | .string "Manifest" => .ok .manifest
    | .string "SignedExchange" => .ok .signedExchange
    | .string "Ping" => .ok .ping
    | .string "CSPViolationReport" => .ok .cSPViolationReport
    | .string "Preflight" => .ok .preflight
    | .string "Other" => .ok .other
    | v => .error s!"failed to parse Network.ResourceType: {repr v}"
instance : ToJSON Network.ResourceType where
  toJSON
    | .document => .string "Document"
    | .stylesheet => .string "Stylesheet"
    | .image => .string "Image"
    | .media => .string "Media"
    | .font => .string "Font"
    | .script => .string "Script"
    | .textTrack => .string "TextTrack"
    | .xHR => .string "XHR"
    | .fetch => .string "Fetch"
    | .prefetch => .string "Prefetch"
    | .eventSource => .string "EventSource"
    | .webSocket => .string "WebSocket"
    | .manifest => .string "Manifest"
    | .signedExchange => .string "SignedExchange"
    | .ping => .string "Ping"
    | .cSPViolationReport => .string "CSPViolationReport"
    | .preflight => .string "Preflight"
    | .other => .string "Other"

/-- `Network.LoaderId`. -/
abbrev Network.LoaderId := String

/-- `Network.RequestId`. -/
abbrev Network.RequestId := String

/-- `Network.InterceptionId`. -/
abbrev Network.InterceptionId := String

/-- `Network.ErrorReason`. -/
inductive Network.ErrorReason where
  | failed | aborted | timedOut | accessDenied | connectionClosed | connectionReset | connectionRefused | connectionAborted | connectionFailed | nameNotResolved | internetDisconnected | addressUnreachable | blockedByClient | blockedByResponse
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ErrorReason where
  parseJSON
    | .string "Failed" => .ok .failed
    | .string "Aborted" => .ok .aborted
    | .string "TimedOut" => .ok .timedOut
    | .string "AccessDenied" => .ok .accessDenied
    | .string "ConnectionClosed" => .ok .connectionClosed
    | .string "ConnectionReset" => .ok .connectionReset
    | .string "ConnectionRefused" => .ok .connectionRefused
    | .string "ConnectionAborted" => .ok .connectionAborted
    | .string "ConnectionFailed" => .ok .connectionFailed
    | .string "NameNotResolved" => .ok .nameNotResolved
    | .string "InternetDisconnected" => .ok .internetDisconnected
    | .string "AddressUnreachable" => .ok .addressUnreachable
    | .string "BlockedByClient" => .ok .blockedByClient
    | .string "BlockedByResponse" => .ok .blockedByResponse
    | v => .error s!"failed to parse Network.ErrorReason: {repr v}"
instance : ToJSON Network.ErrorReason where
  toJSON
    | .failed => .string "Failed"
    | .aborted => .string "Aborted"
    | .timedOut => .string "TimedOut"
    | .accessDenied => .string "AccessDenied"
    | .connectionClosed => .string "ConnectionClosed"
    | .connectionReset => .string "ConnectionReset"
    | .connectionRefused => .string "ConnectionRefused"
    | .connectionAborted => .string "ConnectionAborted"
    | .connectionFailed => .string "ConnectionFailed"
    | .nameNotResolved => .string "NameNotResolved"
    | .internetDisconnected => .string "InternetDisconnected"
    | .addressUnreachable => .string "AddressUnreachable"
    | .blockedByClient => .string "BlockedByClient"
    | .blockedByResponse => .string "BlockedByResponse"

/-- `Network.MonotonicTime`. -/
abbrev Network.MonotonicTime := Float

/-- `Network.Headers`. -/
abbrev Network.Headers := List (String × String)

/-- `Network.ConnectionType`. -/
inductive Network.ConnectionType where
  | none | cellular2g | cellular3g | cellular4g | bluetooth | ethernet | wifi | wimax | other
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ConnectionType where
  parseJSON
    | .string "none" => .ok .none
    | .string "cellular2g" => .ok .cellular2g
    | .string "cellular3g" => .ok .cellular3g
    | .string "cellular4g" => .ok .cellular4g
    | .string "bluetooth" => .ok .bluetooth
    | .string "ethernet" => .ok .ethernet
    | .string "wifi" => .ok .wifi
    | .string "wimax" => .ok .wimax
    | .string "other" => .ok .other
    | v => .error s!"failed to parse Network.ConnectionType: {repr v}"
instance : ToJSON Network.ConnectionType where
  toJSON
    | .none => .string "none"
    | .cellular2g => .string "cellular2g"
    | .cellular3g => .string "cellular3g"
    | .cellular4g => .string "cellular4g"
    | .bluetooth => .string "bluetooth"
    | .ethernet => .string "ethernet"
    | .wifi => .string "wifi"
    | .wimax => .string "wimax"
    | .other => .string "other"

/-- `Network.CookieSameSite`. -/
inductive Network.CookieSameSite where
  | strict | lax | none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CookieSameSite where
  parseJSON
    | .string "Strict" => .ok .strict
    | .string "Lax" => .ok .lax
    | .string "None" => .ok .none
    | v => .error s!"failed to parse Network.CookieSameSite: {repr v}"
instance : ToJSON Network.CookieSameSite where
  toJSON
    | .strict => .string "Strict"
    | .lax => .string "Lax"
    | .none => .string "None"

/-- `Network.CookiePriority`. -/
inductive Network.CookiePriority where
  | low | medium | high
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CookiePriority where
  parseJSON
    | .string "Low" => .ok .low
    | .string "Medium" => .ok .medium
    | .string "High" => .ok .high
    | v => .error s!"failed to parse Network.CookiePriority: {repr v}"
instance : ToJSON Network.CookiePriority where
  toJSON
    | .low => .string "Low"
    | .medium => .string "Medium"
    | .high => .string "High"

/-- `Network.CookieSourceScheme`. -/
inductive Network.CookieSourceScheme where
  | unset | nonSecure | secure
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CookieSourceScheme where
  parseJSON
    | .string "Unset" => .ok .unset
    | .string "NonSecure" => .ok .nonSecure
    | .string "Secure" => .ok .secure
    | v => .error s!"failed to parse Network.CookieSourceScheme: {repr v}"
instance : ToJSON Network.CookieSourceScheme where
  toJSON
    | .unset => .string "Unset"
    | .nonSecure => .string "NonSecure"
    | .secure => .string "Secure"

/-- `Network.ResourceTiming`. -/
structure Network.ResourceTiming where
  requestTime : Float
  proxyStart : Float
  proxyEnd : Float
  dnsStart : Float
  dnsEnd : Float
  connectStart : Float
  connectEnd : Float
  sslStart : Float
  sslEnd : Float
  workerStart : Float
  workerReady : Float
  workerFetchStart : Float
  workerRespondWithSettled : Float
  sendStart : Float
  sendEnd : Float
  pushStart : Float
  pushEnd : Float
  receiveHeadersEnd : Float
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ResourceTiming where
  parseJSON v := do
    .ok
      { requestTime := ← Value.getField v "requestTime" >>= FromJSON.parseJSON
        proxyStart := ← Value.getField v "proxyStart" >>= FromJSON.parseJSON
        proxyEnd := ← Value.getField v "proxyEnd" >>= FromJSON.parseJSON
        dnsStart := ← Value.getField v "dnsStart" >>= FromJSON.parseJSON
        dnsEnd := ← Value.getField v "dnsEnd" >>= FromJSON.parseJSON
        connectStart := ← Value.getField v "connectStart" >>= FromJSON.parseJSON
        connectEnd := ← Value.getField v "connectEnd" >>= FromJSON.parseJSON
        sslStart := ← Value.getField v "sslStart" >>= FromJSON.parseJSON
        sslEnd := ← Value.getField v "sslEnd" >>= FromJSON.parseJSON
        workerStart := ← Value.getField v "workerStart" >>= FromJSON.parseJSON
        workerReady := ← Value.getField v "workerReady" >>= FromJSON.parseJSON
        workerFetchStart := ← Value.getField v "workerFetchStart" >>= FromJSON.parseJSON
        workerRespondWithSettled := ← Value.getField v "workerRespondWithSettled" >>= FromJSON.parseJSON
        sendStart := ← Value.getField v "sendStart" >>= FromJSON.parseJSON
        sendEnd := ← Value.getField v "sendEnd" >>= FromJSON.parseJSON
        pushStart := ← Value.getField v "pushStart" >>= FromJSON.parseJSON
        pushEnd := ← Value.getField v "pushEnd" >>= FromJSON.parseJSON
        receiveHeadersEnd := ← Value.getField v "receiveHeadersEnd" >>= FromJSON.parseJSON }
instance : ToJSON Network.ResourceTiming where
  toJSON p := Data.Json.object <|
       [("requestTime", ToJSON.toJSON p.requestTime)]
    ++ [("proxyStart", ToJSON.toJSON p.proxyStart)]
    ++ [("proxyEnd", ToJSON.toJSON p.proxyEnd)]
    ++ [("dnsStart", ToJSON.toJSON p.dnsStart)]
    ++ [("dnsEnd", ToJSON.toJSON p.dnsEnd)]
    ++ [("connectStart", ToJSON.toJSON p.connectStart)]
    ++ [("connectEnd", ToJSON.toJSON p.connectEnd)]
    ++ [("sslStart", ToJSON.toJSON p.sslStart)]
    ++ [("sslEnd", ToJSON.toJSON p.sslEnd)]
    ++ [("workerStart", ToJSON.toJSON p.workerStart)]
    ++ [("workerReady", ToJSON.toJSON p.workerReady)]
    ++ [("workerFetchStart", ToJSON.toJSON p.workerFetchStart)]
    ++ [("workerRespondWithSettled", ToJSON.toJSON p.workerRespondWithSettled)]
    ++ [("sendStart", ToJSON.toJSON p.sendStart)]
    ++ [("sendEnd", ToJSON.toJSON p.sendEnd)]
    ++ [("pushStart", ToJSON.toJSON p.pushStart)]
    ++ [("pushEnd", ToJSON.toJSON p.pushEnd)]
    ++ [("receiveHeadersEnd", ToJSON.toJSON p.receiveHeadersEnd)]

/-- `Network.ResourcePriority`. -/
inductive Network.ResourcePriority where
  | veryLow | low | medium | high | veryHigh
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ResourcePriority where
  parseJSON
    | .string "VeryLow" => .ok .veryLow
    | .string "Low" => .ok .low
    | .string "Medium" => .ok .medium
    | .string "High" => .ok .high
    | .string "VeryHigh" => .ok .veryHigh
    | v => .error s!"failed to parse Network.ResourcePriority: {repr v}"
instance : ToJSON Network.ResourcePriority where
  toJSON
    | .veryLow => .string "VeryLow"
    | .low => .string "Low"
    | .medium => .string "Medium"
    | .high => .string "High"
    | .veryHigh => .string "VeryHigh"

/-- `Network.PostDataEntry`. -/
structure Network.PostDataEntry where
  bytes : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.PostDataEntry where
  parseJSON v := do
    .ok
      { bytes := ← (← Value.getFieldOpt v "bytes").mapM FromJSON.parseJSON }
instance : ToJSON Network.PostDataEntry where
  toJSON p := Data.Json.object <|
       (p.bytes.map (fun x => ("bytes", ToJSON.toJSON x))).toList

/-- `Network.RequestReferrerPolicy`. -/
inductive Network.RequestReferrerPolicy where
  | unsafeUrl | noReferrerWhenDowngrade | noReferrer | origin | originWhenCrossOrigin | sameOrigin | strictOrigin | strictOriginWhenCrossOrigin
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.RequestReferrerPolicy where
  parseJSON
    | .string "unsafe-url" => .ok .unsafeUrl
    | .string "no-referrer-when-downgrade" => .ok .noReferrerWhenDowngrade
    | .string "no-referrer" => .ok .noReferrer
    | .string "origin" => .ok .origin
    | .string "origin-when-cross-origin" => .ok .originWhenCrossOrigin
    | .string "same-origin" => .ok .sameOrigin
    | .string "strict-origin" => .ok .strictOrigin
    | .string "strict-origin-when-cross-origin" => .ok .strictOriginWhenCrossOrigin
    | v => .error s!"failed to parse Network.RequestReferrerPolicy: {repr v}"
instance : ToJSON Network.RequestReferrerPolicy where
  toJSON
    | .unsafeUrl => .string "unsafe-url"
    | .noReferrerWhenDowngrade => .string "no-referrer-when-downgrade"
    | .noReferrer => .string "no-referrer"
    | .origin => .string "origin"
    | .originWhenCrossOrigin => .string "origin-when-cross-origin"
    | .sameOrigin => .string "same-origin"
    | .strictOrigin => .string "strict-origin"
    | .strictOriginWhenCrossOrigin => .string "strict-origin-when-cross-origin"

/-- `Network.TrustTokenParamsRefreshPolicy`. -/
inductive Network.TrustTokenParamsRefreshPolicy where
  | useCached | refresh
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.TrustTokenParamsRefreshPolicy where
  parseJSON
    | .string "UseCached" => .ok .useCached
    | .string "Refresh" => .ok .refresh
    | v => .error s!"failed to parse Network.TrustTokenParamsRefreshPolicy: {repr v}"
instance : ToJSON Network.TrustTokenParamsRefreshPolicy where
  toJSON
    | .useCached => .string "UseCached"
    | .refresh => .string "Refresh"

/-- `Network.TrustTokenOperationType`. -/
inductive Network.TrustTokenOperationType where
  | issuance | redemption | signing
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.TrustTokenOperationType where
  parseJSON
    | .string "Issuance" => .ok .issuance
    | .string "Redemption" => .ok .redemption
    | .string "Signing" => .ok .signing
    | v => .error s!"failed to parse Network.TrustTokenOperationType: {repr v}"
instance : ToJSON Network.TrustTokenOperationType where
  toJSON
    | .issuance => .string "Issuance"
    | .redemption => .string "Redemption"
    | .signing => .string "Signing"

/-- `Network.TrustTokenParams`. -/
structure Network.TrustTokenParams where
  type : Network.TrustTokenOperationType
  refreshPolicy : Network.TrustTokenParamsRefreshPolicy
  issuers : Option (List String) := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.TrustTokenParams where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        refreshPolicy := ← Value.getField v "refreshPolicy" >>= FromJSON.parseJSON
        issuers := ← (← Value.getFieldOpt v "issuers").mapM FromJSON.parseJSON }
instance : ToJSON Network.TrustTokenParams where
  toJSON p := Data.Json.object <|
       [("type", ToJSON.toJSON p.type)]
    ++ [("refreshPolicy", ToJSON.toJSON p.refreshPolicy)]
    ++ (p.issuers.map (fun x => ("issuers", ToJSON.toJSON x))).toList

/-- `Security.MixedContentType`. -/
inductive Security.MixedContentType where
  | blockable | optionallyBlockable | none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.MixedContentType where
  parseJSON
    | .string "blockable" => .ok .blockable
    | .string "optionally-blockable" => .ok .optionallyBlockable
    | .string "none" => .ok .none
    | v => .error s!"failed to parse Security.MixedContentType: {repr v}"
instance : ToJSON Security.MixedContentType where
  toJSON
    | .blockable => .string "blockable"
    | .optionallyBlockable => .string "optionally-blockable"
    | .none => .string "none"

/-- `Network.Request`. -/
structure Network.Request where
  url : String
  urlFragment : Option String := none
  method : String
  headers : Network.Headers
  postData : Option String := none
  hasPostData : Option Bool := none
  postDataEntries : Option (List Network.PostDataEntry) := none
  mixedContentType : Option Security.MixedContentType := none
  initialPriority : Network.ResourcePriority
  referrerPolicy : Network.RequestReferrerPolicy
  isLinkPreload : Option Bool := none
  trustTokenParams : Option Network.TrustTokenParams := none
  isSameSite : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.Request where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        urlFragment := ← (← Value.getFieldOpt v "urlFragment").mapM FromJSON.parseJSON
        method := ← Value.getField v "method" >>= FromJSON.parseJSON
        headers := ← Value.getField v "headers" >>= FromJSON.parseJSON
        postData := ← (← Value.getFieldOpt v "postData").mapM FromJSON.parseJSON
        hasPostData := ← (← Value.getFieldOpt v "hasPostData").mapM FromJSON.parseJSON
        postDataEntries := ← (← Value.getFieldOpt v "postDataEntries").mapM FromJSON.parseJSON
        mixedContentType := ← (← Value.getFieldOpt v "mixedContentType").mapM FromJSON.parseJSON
        initialPriority := ← Value.getField v "initialPriority" >>= FromJSON.parseJSON
        referrerPolicy := ← Value.getField v "referrerPolicy" >>= FromJSON.parseJSON
        isLinkPreload := ← (← Value.getFieldOpt v "isLinkPreload").mapM FromJSON.parseJSON
        trustTokenParams := ← (← Value.getFieldOpt v "trustTokenParams").mapM FromJSON.parseJSON
        isSameSite := ← (← Value.getFieldOpt v "isSameSite").mapM FromJSON.parseJSON }
instance : ToJSON Network.Request where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ (p.urlFragment.map (fun x => ("urlFragment", ToJSON.toJSON x))).toList
    ++ [("method", ToJSON.toJSON p.method)]
    ++ [("headers", ToJSON.toJSON p.headers)]
    ++ (p.postData.map (fun x => ("postData", ToJSON.toJSON x))).toList
    ++ (p.hasPostData.map (fun x => ("hasPostData", ToJSON.toJSON x))).toList
    ++ (p.postDataEntries.map (fun x => ("postDataEntries", ToJSON.toJSON x))).toList
    ++ (p.mixedContentType.map (fun x => ("mixedContentType", ToJSON.toJSON x))).toList
    ++ [("initialPriority", ToJSON.toJSON p.initialPriority)]
    ++ [("referrerPolicy", ToJSON.toJSON p.referrerPolicy)]
    ++ (p.isLinkPreload.map (fun x => ("isLinkPreload", ToJSON.toJSON x))).toList
    ++ (p.trustTokenParams.map (fun x => ("trustTokenParams", ToJSON.toJSON x))).toList
    ++ (p.isSameSite.map (fun x => ("isSameSite", ToJSON.toJSON x))).toList

/-- `Network.SignedCertificateTimestamp`. -/
structure Network.SignedCertificateTimestamp where
  status : String
  origin : String
  logDescription : String
  logId : String
  timestamp : Float
  hashAlgorithm : String
  signatureAlgorithm : String
  signatureData : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SignedCertificateTimestamp where
  parseJSON v := do
    .ok
      { status := ← Value.getField v "status" >>= FromJSON.parseJSON
        origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        logDescription := ← Value.getField v "logDescription" >>= FromJSON.parseJSON
        logId := ← Value.getField v "logId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        hashAlgorithm := ← Value.getField v "hashAlgorithm" >>= FromJSON.parseJSON
        signatureAlgorithm := ← Value.getField v "signatureAlgorithm" >>= FromJSON.parseJSON
        signatureData := ← Value.getField v "signatureData" >>= FromJSON.parseJSON }
instance : ToJSON Network.SignedCertificateTimestamp where
  toJSON p := Data.Json.object <|
       [("status", ToJSON.toJSON p.status)]
    ++ [("origin", ToJSON.toJSON p.origin)]
    ++ [("logDescription", ToJSON.toJSON p.logDescription)]
    ++ [("logId", ToJSON.toJSON p.logId)]
    ++ [("timestamp", ToJSON.toJSON p.timestamp)]
    ++ [("hashAlgorithm", ToJSON.toJSON p.hashAlgorithm)]
    ++ [("signatureAlgorithm", ToJSON.toJSON p.signatureAlgorithm)]
    ++ [("signatureData", ToJSON.toJSON p.signatureData)]

/-- `Network.CertificateTransparencyCompliance`. -/
inductive Network.CertificateTransparencyCompliance where
  | unknown | notCompliant | compliant
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CertificateTransparencyCompliance where
  parseJSON
    | .string "unknown" => .ok .unknown
    | .string "not-compliant" => .ok .notCompliant
    | .string "compliant" => .ok .compliant
    | v => .error s!"failed to parse Network.CertificateTransparencyCompliance: {repr v}"
instance : ToJSON Network.CertificateTransparencyCompliance where
  toJSON
    | .unknown => .string "unknown"
    | .notCompliant => .string "not-compliant"
    | .compliant => .string "compliant"

/-- `Security.CertificateId`. -/
abbrev Security.CertificateId := Int

/-- `Network.SecurityDetails`. -/
structure Network.SecurityDetails where
  protocol : String
  keyExchange : String
  keyExchangeGroup : Option String := none
  cipher : String
  mac : Option String := none
  certificateId : Security.CertificateId
  subjectName : String
  sanList : List String
  issuer : String
  validFrom : Network.TimeSinceEpoch
  validTo : Network.TimeSinceEpoch
  signedCertificateTimestampList : List Network.SignedCertificateTimestamp
  certificateTransparencyCompliance : Network.CertificateTransparencyCompliance
  serverSignatureAlgorithm : Option Int := none
  encryptedClientHello : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SecurityDetails where
  parseJSON v := do
    .ok
      { protocol := ← Value.getField v "protocol" >>= FromJSON.parseJSON
        keyExchange := ← Value.getField v "keyExchange" >>= FromJSON.parseJSON
        keyExchangeGroup := ← (← Value.getFieldOpt v "keyExchangeGroup").mapM FromJSON.parseJSON
        cipher := ← Value.getField v "cipher" >>= FromJSON.parseJSON
        mac := ← (← Value.getFieldOpt v "mac").mapM FromJSON.parseJSON
        certificateId := ← Value.getField v "certificateId" >>= FromJSON.parseJSON
        subjectName := ← Value.getField v "subjectName" >>= FromJSON.parseJSON
        sanList := ← Value.getField v "sanList" >>= FromJSON.parseJSON
        issuer := ← Value.getField v "issuer" >>= FromJSON.parseJSON
        validFrom := ← Value.getField v "validFrom" >>= FromJSON.parseJSON
        validTo := ← Value.getField v "validTo" >>= FromJSON.parseJSON
        signedCertificateTimestampList := ← Value.getField v "signedCertificateTimestampList" >>= FromJSON.parseJSON
        certificateTransparencyCompliance := ← Value.getField v "certificateTransparencyCompliance" >>= FromJSON.parseJSON
        serverSignatureAlgorithm := ← (← Value.getFieldOpt v "serverSignatureAlgorithm").mapM FromJSON.parseJSON
        encryptedClientHello := ← Value.getField v "encryptedClientHello" >>= FromJSON.parseJSON }
instance : ToJSON Network.SecurityDetails where
  toJSON p := Data.Json.object <|
       [("protocol", ToJSON.toJSON p.protocol)]
    ++ [("keyExchange", ToJSON.toJSON p.keyExchange)]
    ++ (p.keyExchangeGroup.map (fun x => ("keyExchangeGroup", ToJSON.toJSON x))).toList
    ++ [("cipher", ToJSON.toJSON p.cipher)]
    ++ (p.mac.map (fun x => ("mac", ToJSON.toJSON x))).toList
    ++ [("certificateId", ToJSON.toJSON p.certificateId)]
    ++ [("subjectName", ToJSON.toJSON p.subjectName)]
    ++ [("sanList", ToJSON.toJSON p.sanList)]
    ++ [("issuer", ToJSON.toJSON p.issuer)]
    ++ [("validFrom", ToJSON.toJSON p.validFrom)]
    ++ [("validTo", ToJSON.toJSON p.validTo)]
    ++ [("signedCertificateTimestampList", ToJSON.toJSON p.signedCertificateTimestampList)]
    ++ [("certificateTransparencyCompliance", ToJSON.toJSON p.certificateTransparencyCompliance)]
    ++ (p.serverSignatureAlgorithm.map (fun x => ("serverSignatureAlgorithm", ToJSON.toJSON x))).toList
    ++ [("encryptedClientHello", ToJSON.toJSON p.encryptedClientHello)]

/-- `Network.BlockedReason`. -/
inductive Network.BlockedReason where
  | other | csp | mixedContent | origin | inspector | subresourceFilter | contentType | coepFrameResourceNeedsCoepHeader | coopSandboxedIframeCannotNavigateToCoopPage | corpNotSameOrigin | corpNotSameOriginAfterDefaultedToSameOriginByCoep | corpNotSameSite
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.BlockedReason where
  parseJSON
    | .string "other" => .ok .other
    | .string "csp" => .ok .csp
    | .string "mixed-content" => .ok .mixedContent
    | .string "origin" => .ok .origin
    | .string "inspector" => .ok .inspector
    | .string "subresource-filter" => .ok .subresourceFilter
    | .string "content-type" => .ok .contentType
    | .string "coep-frame-resource-needs-coep-header" => .ok .coepFrameResourceNeedsCoepHeader
    | .string "coop-sandboxed-iframe-cannot-navigate-to-coop-page" => .ok .coopSandboxedIframeCannotNavigateToCoopPage
    | .string "corp-not-same-origin" => .ok .corpNotSameOrigin
    | .string "corp-not-same-origin-after-defaulted-to-same-origin-by-coep" => .ok .corpNotSameOriginAfterDefaultedToSameOriginByCoep
    | .string "corp-not-same-site" => .ok .corpNotSameSite
    | v => .error s!"failed to parse Network.BlockedReason: {repr v}"
instance : ToJSON Network.BlockedReason where
  toJSON
    | .other => .string "other"
    | .csp => .string "csp"
    | .mixedContent => .string "mixed-content"
    | .origin => .string "origin"
    | .inspector => .string "inspector"
    | .subresourceFilter => .string "subresource-filter"
    | .contentType => .string "content-type"
    | .coepFrameResourceNeedsCoepHeader => .string "coep-frame-resource-needs-coep-header"
    | .coopSandboxedIframeCannotNavigateToCoopPage => .string "coop-sandboxed-iframe-cannot-navigate-to-coop-page"
    | .corpNotSameOrigin => .string "corp-not-same-origin"
    | .corpNotSameOriginAfterDefaultedToSameOriginByCoep => .string "corp-not-same-origin-after-defaulted-to-same-origin-by-coep"
    | .corpNotSameSite => .string "corp-not-same-site"

/-- `Network.CorsError`. -/
inductive Network.CorsError where
  | disallowedByMode | invalidResponse | wildcardOriginNotAllowed | missingAllowOriginHeader | multipleAllowOriginValues | invalidAllowOriginValue | allowOriginMismatch | invalidAllowCredentials | corsDisabledScheme | preflightInvalidStatus | preflightDisallowedRedirect | preflightWildcardOriginNotAllowed | preflightMissingAllowOriginHeader | preflightMultipleAllowOriginValues | preflightInvalidAllowOriginValue | preflightAllowOriginMismatch | preflightInvalidAllowCredentials | preflightMissingAllowExternal | preflightInvalidAllowExternal | preflightMissingAllowPrivateNetwork | preflightInvalidAllowPrivateNetwork | invalidAllowMethodsPreflightResponse | invalidAllowHeadersPreflightResponse | methodDisallowedByPreflightResponse | headerDisallowedByPreflightResponse | redirectContainsCredentials | insecurePrivateNetwork | invalidPrivateNetworkAccess | unexpectedPrivateNetworkAccess | noCorsRedirectModeNotFollow
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CorsError where
  parseJSON
    | .string "DisallowedByMode" => .ok .disallowedByMode
    | .string "InvalidResponse" => .ok .invalidResponse
    | .string "WildcardOriginNotAllowed" => .ok .wildcardOriginNotAllowed
    | .string "MissingAllowOriginHeader" => .ok .missingAllowOriginHeader
    | .string "MultipleAllowOriginValues" => .ok .multipleAllowOriginValues
    | .string "InvalidAllowOriginValue" => .ok .invalidAllowOriginValue
    | .string "AllowOriginMismatch" => .ok .allowOriginMismatch
    | .string "InvalidAllowCredentials" => .ok .invalidAllowCredentials
    | .string "CorsDisabledScheme" => .ok .corsDisabledScheme
    | .string "PreflightInvalidStatus" => .ok .preflightInvalidStatus
    | .string "PreflightDisallowedRedirect" => .ok .preflightDisallowedRedirect
    | .string "PreflightWildcardOriginNotAllowed" => .ok .preflightWildcardOriginNotAllowed
    | .string "PreflightMissingAllowOriginHeader" => .ok .preflightMissingAllowOriginHeader
    | .string "PreflightMultipleAllowOriginValues" => .ok .preflightMultipleAllowOriginValues
    | .string "PreflightInvalidAllowOriginValue" => .ok .preflightInvalidAllowOriginValue
    | .string "PreflightAllowOriginMismatch" => .ok .preflightAllowOriginMismatch
    | .string "PreflightInvalidAllowCredentials" => .ok .preflightInvalidAllowCredentials
    | .string "PreflightMissingAllowExternal" => .ok .preflightMissingAllowExternal
    | .string "PreflightInvalidAllowExternal" => .ok .preflightInvalidAllowExternal
    | .string "PreflightMissingAllowPrivateNetwork" => .ok .preflightMissingAllowPrivateNetwork
    | .string "PreflightInvalidAllowPrivateNetwork" => .ok .preflightInvalidAllowPrivateNetwork
    | .string "InvalidAllowMethodsPreflightResponse" => .ok .invalidAllowMethodsPreflightResponse
    | .string "InvalidAllowHeadersPreflightResponse" => .ok .invalidAllowHeadersPreflightResponse
    | .string "MethodDisallowedByPreflightResponse" => .ok .methodDisallowedByPreflightResponse
    | .string "HeaderDisallowedByPreflightResponse" => .ok .headerDisallowedByPreflightResponse
    | .string "RedirectContainsCredentials" => .ok .redirectContainsCredentials
    | .string "InsecurePrivateNetwork" => .ok .insecurePrivateNetwork
    | .string "InvalidPrivateNetworkAccess" => .ok .invalidPrivateNetworkAccess
    | .string "UnexpectedPrivateNetworkAccess" => .ok .unexpectedPrivateNetworkAccess
    | .string "NoCorsRedirectModeNotFollow" => .ok .noCorsRedirectModeNotFollow
    | v => .error s!"failed to parse Network.CorsError: {repr v}"
instance : ToJSON Network.CorsError where
  toJSON
    | .disallowedByMode => .string "DisallowedByMode"
    | .invalidResponse => .string "InvalidResponse"
    | .wildcardOriginNotAllowed => .string "WildcardOriginNotAllowed"
    | .missingAllowOriginHeader => .string "MissingAllowOriginHeader"
    | .multipleAllowOriginValues => .string "MultipleAllowOriginValues"
    | .invalidAllowOriginValue => .string "InvalidAllowOriginValue"
    | .allowOriginMismatch => .string "AllowOriginMismatch"
    | .invalidAllowCredentials => .string "InvalidAllowCredentials"
    | .corsDisabledScheme => .string "CorsDisabledScheme"
    | .preflightInvalidStatus => .string "PreflightInvalidStatus"
    | .preflightDisallowedRedirect => .string "PreflightDisallowedRedirect"
    | .preflightWildcardOriginNotAllowed => .string "PreflightWildcardOriginNotAllowed"
    | .preflightMissingAllowOriginHeader => .string "PreflightMissingAllowOriginHeader"
    | .preflightMultipleAllowOriginValues => .string "PreflightMultipleAllowOriginValues"
    | .preflightInvalidAllowOriginValue => .string "PreflightInvalidAllowOriginValue"
    | .preflightAllowOriginMismatch => .string "PreflightAllowOriginMismatch"
    | .preflightInvalidAllowCredentials => .string "PreflightInvalidAllowCredentials"
    | .preflightMissingAllowExternal => .string "PreflightMissingAllowExternal"
    | .preflightInvalidAllowExternal => .string "PreflightInvalidAllowExternal"
    | .preflightMissingAllowPrivateNetwork => .string "PreflightMissingAllowPrivateNetwork"
    | .preflightInvalidAllowPrivateNetwork => .string "PreflightInvalidAllowPrivateNetwork"
    | .invalidAllowMethodsPreflightResponse => .string "InvalidAllowMethodsPreflightResponse"
    | .invalidAllowHeadersPreflightResponse => .string "InvalidAllowHeadersPreflightResponse"
    | .methodDisallowedByPreflightResponse => .string "MethodDisallowedByPreflightResponse"
    | .headerDisallowedByPreflightResponse => .string "HeaderDisallowedByPreflightResponse"
    | .redirectContainsCredentials => .string "RedirectContainsCredentials"
    | .insecurePrivateNetwork => .string "InsecurePrivateNetwork"
    | .invalidPrivateNetworkAccess => .string "InvalidPrivateNetworkAccess"
    | .unexpectedPrivateNetworkAccess => .string "UnexpectedPrivateNetworkAccess"
    | .noCorsRedirectModeNotFollow => .string "NoCorsRedirectModeNotFollow"

/-- `Network.CorsErrorStatus`. -/
structure Network.CorsErrorStatus where
  corsError : Network.CorsError
  failedParameter : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CorsErrorStatus where
  parseJSON v := do
    .ok
      { corsError := ← Value.getField v "corsError" >>= FromJSON.parseJSON
        failedParameter := ← Value.getField v "failedParameter" >>= FromJSON.parseJSON }
instance : ToJSON Network.CorsErrorStatus where
  toJSON p := Data.Json.object <|
       [("corsError", ToJSON.toJSON p.corsError)]
    ++ [("failedParameter", ToJSON.toJSON p.failedParameter)]

/-- `Network.ServiceWorkerResponseSource`. -/
inductive Network.ServiceWorkerResponseSource where
  | cacheStorage | httpCache | fallbackCode | network
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ServiceWorkerResponseSource where
  parseJSON
    | .string "cache-storage" => .ok .cacheStorage
    | .string "http-cache" => .ok .httpCache
    | .string "fallback-code" => .ok .fallbackCode
    | .string "network" => .ok .network
    | v => .error s!"failed to parse Network.ServiceWorkerResponseSource: {repr v}"
instance : ToJSON Network.ServiceWorkerResponseSource where
  toJSON
    | .cacheStorage => .string "cache-storage"
    | .httpCache => .string "http-cache"
    | .fallbackCode => .string "fallback-code"
    | .network => .string "network"

/-- `Network.AlternateProtocolUsage`. -/
inductive Network.AlternateProtocolUsage where
  | alternativeJobWonWithoutRace | alternativeJobWonRace | mainJobWonRace | mappingMissing | broken | dnsAlpnH3JobWonWithoutRace | dnsAlpnH3JobWonRace | unspecifiedReason
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.AlternateProtocolUsage where
  parseJSON
    | .string "alternativeJobWonWithoutRace" => .ok .alternativeJobWonWithoutRace
    | .string "alternativeJobWonRace" => .ok .alternativeJobWonRace
    | .string "mainJobWonRace" => .ok .mainJobWonRace
    | .string "mappingMissing" => .ok .mappingMissing
    | .string "broken" => .ok .broken
    | .string "dnsAlpnH3JobWonWithoutRace" => .ok .dnsAlpnH3JobWonWithoutRace
    | .string "dnsAlpnH3JobWonRace" => .ok .dnsAlpnH3JobWonRace
    | .string "unspecifiedReason" => .ok .unspecifiedReason
    | v => .error s!"failed to parse Network.AlternateProtocolUsage: {repr v}"
instance : ToJSON Network.AlternateProtocolUsage where
  toJSON
    | .alternativeJobWonWithoutRace => .string "alternativeJobWonWithoutRace"
    | .alternativeJobWonRace => .string "alternativeJobWonRace"
    | .mainJobWonRace => .string "mainJobWonRace"
    | .mappingMissing => .string "mappingMissing"
    | .broken => .string "broken"
    | .dnsAlpnH3JobWonWithoutRace => .string "dnsAlpnH3JobWonWithoutRace"
    | .dnsAlpnH3JobWonRace => .string "dnsAlpnH3JobWonRace"
    | .unspecifiedReason => .string "unspecifiedReason"

/-- `Security.SecurityState`. -/
inductive Security.SecurityState where
  | unknown | neutral | insecure | secure | info | insecureBroken
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.SecurityState where
  parseJSON
    | .string "unknown" => .ok .unknown
    | .string "neutral" => .ok .neutral
    | .string "insecure" => .ok .insecure
    | .string "secure" => .ok .secure
    | .string "info" => .ok .info
    | .string "insecure-broken" => .ok .insecureBroken
    | v => .error s!"failed to parse Security.SecurityState: {repr v}"
instance : ToJSON Security.SecurityState where
  toJSON
    | .unknown => .string "unknown"
    | .neutral => .string "neutral"
    | .insecure => .string "insecure"
    | .secure => .string "secure"
    | .info => .string "info"
    | .insecureBroken => .string "insecure-broken"

/-- `Network.Response`. -/
structure Network.Response where
  url : String
  status : Int
  statusText : String
  headers : Network.Headers
  mimeType : String
  requestHeaders : Option Network.Headers := none
  connectionReused : Bool
  connectionId : Float
  remoteIPAddress : Option String := none
  remotePort : Option Int := none
  fromDiskCache : Option Bool := none
  fromServiceWorker : Option Bool := none
  fromPrefetchCache : Option Bool := none
  encodedDataLength : Float
  timing : Option Network.ResourceTiming := none
  serviceWorkerResponseSource : Option Network.ServiceWorkerResponseSource := none
  responseTime : Option Network.TimeSinceEpoch := none
  cacheStorageCacheName : Option String := none
  protocol : Option String := none
  alternateProtocolUsage : Option Network.AlternateProtocolUsage := none
  securityState : Security.SecurityState
  securityDetails : Option Network.SecurityDetails := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.Response where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        status := ← Value.getField v "status" >>= FromJSON.parseJSON
        statusText := ← Value.getField v "statusText" >>= FromJSON.parseJSON
        headers := ← Value.getField v "headers" >>= FromJSON.parseJSON
        mimeType := ← Value.getField v "mimeType" >>= FromJSON.parseJSON
        requestHeaders := ← (← Value.getFieldOpt v "requestHeaders").mapM FromJSON.parseJSON
        connectionReused := ← Value.getField v "connectionReused" >>= FromJSON.parseJSON
        connectionId := ← Value.getField v "connectionId" >>= FromJSON.parseJSON
        remoteIPAddress := ← (← Value.getFieldOpt v "remoteIPAddress").mapM FromJSON.parseJSON
        remotePort := ← (← Value.getFieldOpt v "remotePort").mapM FromJSON.parseJSON
        fromDiskCache := ← (← Value.getFieldOpt v "fromDiskCache").mapM FromJSON.parseJSON
        fromServiceWorker := ← (← Value.getFieldOpt v "fromServiceWorker").mapM FromJSON.parseJSON
        fromPrefetchCache := ← (← Value.getFieldOpt v "fromPrefetchCache").mapM FromJSON.parseJSON
        encodedDataLength := ← Value.getField v "encodedDataLength" >>= FromJSON.parseJSON
        timing := ← (← Value.getFieldOpt v "timing").mapM FromJSON.parseJSON
        serviceWorkerResponseSource := ← (← Value.getFieldOpt v "serviceWorkerResponseSource").mapM FromJSON.parseJSON
        responseTime := ← (← Value.getFieldOpt v "responseTime").mapM FromJSON.parseJSON
        cacheStorageCacheName := ← (← Value.getFieldOpt v "cacheStorageCacheName").mapM FromJSON.parseJSON
        protocol := ← (← Value.getFieldOpt v "protocol").mapM FromJSON.parseJSON
        alternateProtocolUsage := ← (← Value.getFieldOpt v "alternateProtocolUsage").mapM FromJSON.parseJSON
        securityState := ← Value.getField v "securityState" >>= FromJSON.parseJSON
        securityDetails := ← (← Value.getFieldOpt v "securityDetails").mapM FromJSON.parseJSON }
instance : ToJSON Network.Response where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ [("status", ToJSON.toJSON p.status)]
    ++ [("statusText", ToJSON.toJSON p.statusText)]
    ++ [("headers", ToJSON.toJSON p.headers)]
    ++ [("mimeType", ToJSON.toJSON p.mimeType)]
    ++ (p.requestHeaders.map (fun x => ("requestHeaders", ToJSON.toJSON x))).toList
    ++ [("connectionReused", ToJSON.toJSON p.connectionReused)]
    ++ [("connectionId", ToJSON.toJSON p.connectionId)]
    ++ (p.remoteIPAddress.map (fun x => ("remoteIPAddress", ToJSON.toJSON x))).toList
    ++ (p.remotePort.map (fun x => ("remotePort", ToJSON.toJSON x))).toList
    ++ (p.fromDiskCache.map (fun x => ("fromDiskCache", ToJSON.toJSON x))).toList
    ++ (p.fromServiceWorker.map (fun x => ("fromServiceWorker", ToJSON.toJSON x))).toList
    ++ (p.fromPrefetchCache.map (fun x => ("fromPrefetchCache", ToJSON.toJSON x))).toList
    ++ [("encodedDataLength", ToJSON.toJSON p.encodedDataLength)]
    ++ (p.timing.map (fun x => ("timing", ToJSON.toJSON x))).toList
    ++ (p.serviceWorkerResponseSource.map (fun x => ("serviceWorkerResponseSource", ToJSON.toJSON x))).toList
    ++ (p.responseTime.map (fun x => ("responseTime", ToJSON.toJSON x))).toList
    ++ (p.cacheStorageCacheName.map (fun x => ("cacheStorageCacheName", ToJSON.toJSON x))).toList
    ++ (p.protocol.map (fun x => ("protocol", ToJSON.toJSON x))).toList
    ++ (p.alternateProtocolUsage.map (fun x => ("alternateProtocolUsage", ToJSON.toJSON x))).toList
    ++ [("securityState", ToJSON.toJSON p.securityState)]
    ++ (p.securityDetails.map (fun x => ("securityDetails", ToJSON.toJSON x))).toList

/-- `Network.WebSocketRequest`. -/
structure Network.WebSocketRequest where
  headers : Network.Headers
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketRequest where
  parseJSON v := do
    .ok
      { headers := ← Value.getField v "headers" >>= FromJSON.parseJSON }
instance : ToJSON Network.WebSocketRequest where
  toJSON p := Data.Json.object <|
       [("headers", ToJSON.toJSON p.headers)]

/-- `Network.WebSocketResponse`. -/
structure Network.WebSocketResponse where
  status : Int
  statusText : String
  headers : Network.Headers
  headersText : Option String := none
  requestHeaders : Option Network.Headers := none
  requestHeadersText : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketResponse where
  parseJSON v := do
    .ok
      { status := ← Value.getField v "status" >>= FromJSON.parseJSON
        statusText := ← Value.getField v "statusText" >>= FromJSON.parseJSON
        headers := ← Value.getField v "headers" >>= FromJSON.parseJSON
        headersText := ← (← Value.getFieldOpt v "headersText").mapM FromJSON.parseJSON
        requestHeaders := ← (← Value.getFieldOpt v "requestHeaders").mapM FromJSON.parseJSON
        requestHeadersText := ← (← Value.getFieldOpt v "requestHeadersText").mapM FromJSON.parseJSON }
instance : ToJSON Network.WebSocketResponse where
  toJSON p := Data.Json.object <|
       [("status", ToJSON.toJSON p.status)]
    ++ [("statusText", ToJSON.toJSON p.statusText)]
    ++ [("headers", ToJSON.toJSON p.headers)]
    ++ (p.headersText.map (fun x => ("headersText", ToJSON.toJSON x))).toList
    ++ (p.requestHeaders.map (fun x => ("requestHeaders", ToJSON.toJSON x))).toList
    ++ (p.requestHeadersText.map (fun x => ("requestHeadersText", ToJSON.toJSON x))).toList

/-- `Network.WebSocketFrame`. -/
structure Network.WebSocketFrame where
  opcode : Float
  mask : Bool
  payloadData : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketFrame where
  parseJSON v := do
    .ok
      { opcode := ← Value.getField v "opcode" >>= FromJSON.parseJSON
        mask := ← Value.getField v "mask" >>= FromJSON.parseJSON
        payloadData := ← Value.getField v "payloadData" >>= FromJSON.parseJSON }
instance : ToJSON Network.WebSocketFrame where
  toJSON p := Data.Json.object <|
       [("opcode", ToJSON.toJSON p.opcode)]
    ++ [("mask", ToJSON.toJSON p.mask)]
    ++ [("payloadData", ToJSON.toJSON p.payloadData)]

/-- `Network.CachedResource`. -/
structure Network.CachedResource where
  url : String
  type : Network.ResourceType
  response : Option Network.Response := none
  bodySize : Float
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CachedResource where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        response := ← (← Value.getFieldOpt v "response").mapM FromJSON.parseJSON
        bodySize := ← Value.getField v "bodySize" >>= FromJSON.parseJSON }
instance : ToJSON Network.CachedResource where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ [("type", ToJSON.toJSON p.type)]
    ++ (p.response.map (fun x => ("response", ToJSON.toJSON x))).toList
    ++ [("bodySize", ToJSON.toJSON p.bodySize)]

/-- `Network.InitiatorType`. -/
inductive Network.InitiatorType where
  | parser | script | preload | signedExchange | preflight | other
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.InitiatorType where
  parseJSON
    | .string "parser" => .ok .parser
    | .string "script" => .ok .script
    | .string "preload" => .ok .preload
    | .string "SignedExchange" => .ok .signedExchange
    | .string "preflight" => .ok .preflight
    | .string "other" => .ok .other
    | v => .error s!"failed to parse Network.InitiatorType: {repr v}"
instance : ToJSON Network.InitiatorType where
  toJSON
    | .parser => .string "parser"
    | .script => .string "script"
    | .preload => .string "preload"
    | .signedExchange => .string "SignedExchange"
    | .preflight => .string "preflight"
    | .other => .string "other"

/-- `Network.Initiator`. -/
structure Network.Initiator where
  type : Network.InitiatorType
  stack : Option Runtime.StackTrace := none
  url : Option String := none
  lineNumber : Option Float := none
  columnNumber : Option Float := none
  requestId : Option Network.RequestId := none
  deriving Repr, BEq
instance : FromJSON Network.Initiator where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        stack := ← (← Value.getFieldOpt v "stack").mapM FromJSON.parseJSON
        url := ← (← Value.getFieldOpt v "url").mapM FromJSON.parseJSON
        lineNumber := ← (← Value.getFieldOpt v "lineNumber").mapM FromJSON.parseJSON
        columnNumber := ← (← Value.getFieldOpt v "columnNumber").mapM FromJSON.parseJSON
        requestId := ← (← Value.getFieldOpt v "requestId").mapM FromJSON.parseJSON }
instance : ToJSON Network.Initiator where
  toJSON p := Data.Json.object <|
       [("type", ToJSON.toJSON p.type)]
    ++ (p.stack.map (fun x => ("stack", ToJSON.toJSON x))).toList
    ++ (p.url.map (fun x => ("url", ToJSON.toJSON x))).toList
    ++ (p.lineNumber.map (fun x => ("lineNumber", ToJSON.toJSON x))).toList
    ++ (p.columnNumber.map (fun x => ("columnNumber", ToJSON.toJSON x))).toList
    ++ (p.requestId.map (fun x => ("requestId", ToJSON.toJSON x))).toList

/-- `Network.Cookie`. -/
structure Network.Cookie where
  name : String
  value : String
  domain : String
  path : String
  expires : Float
  size : Int
  httpOnly : Bool
  secure : Bool
  session : Bool
  sameSite : Option Network.CookieSameSite := none
  priority : Network.CookiePriority
  sameParty : Bool
  sourceScheme : Network.CookieSourceScheme
  sourcePort : Int
  partitionKey : Option String := none
  partitionKeyOpaque : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.Cookie where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON
        domain := ← Value.getField v "domain" >>= FromJSON.parseJSON
        path := ← Value.getField v "path" >>= FromJSON.parseJSON
        expires := ← Value.getField v "expires" >>= FromJSON.parseJSON
        size := ← Value.getField v "size" >>= FromJSON.parseJSON
        httpOnly := ← Value.getField v "httpOnly" >>= FromJSON.parseJSON
        secure := ← Value.getField v "secure" >>= FromJSON.parseJSON
        session := ← Value.getField v "session" >>= FromJSON.parseJSON
        sameSite := ← (← Value.getFieldOpt v "sameSite").mapM FromJSON.parseJSON
        priority := ← Value.getField v "priority" >>= FromJSON.parseJSON
        sameParty := ← Value.getField v "sameParty" >>= FromJSON.parseJSON
        sourceScheme := ← Value.getField v "sourceScheme" >>= FromJSON.parseJSON
        sourcePort := ← Value.getField v "sourcePort" >>= FromJSON.parseJSON
        partitionKey := ← (← Value.getFieldOpt v "partitionKey").mapM FromJSON.parseJSON
        partitionKeyOpaque := ← (← Value.getFieldOpt v "partitionKeyOpaque").mapM FromJSON.parseJSON }
instance : ToJSON Network.Cookie where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ [("value", ToJSON.toJSON p.value)]
    ++ [("domain", ToJSON.toJSON p.domain)]
    ++ [("path", ToJSON.toJSON p.path)]
    ++ [("expires", ToJSON.toJSON p.expires)]
    ++ [("size", ToJSON.toJSON p.size)]
    ++ [("httpOnly", ToJSON.toJSON p.httpOnly)]
    ++ [("secure", ToJSON.toJSON p.secure)]
    ++ [("session", ToJSON.toJSON p.session)]
    ++ (p.sameSite.map (fun x => ("sameSite", ToJSON.toJSON x))).toList
    ++ [("priority", ToJSON.toJSON p.priority)]
    ++ [("sameParty", ToJSON.toJSON p.sameParty)]
    ++ [("sourceScheme", ToJSON.toJSON p.sourceScheme)]
    ++ [("sourcePort", ToJSON.toJSON p.sourcePort)]
    ++ (p.partitionKey.map (fun x => ("partitionKey", ToJSON.toJSON x))).toList
    ++ (p.partitionKeyOpaque.map (fun x => ("partitionKeyOpaque", ToJSON.toJSON x))).toList

/-- `Network.SetCookieBlockedReason`. -/
inductive Network.SetCookieBlockedReason where
  | secureOnly | sameSiteStrict | sameSiteLax | sameSiteUnspecifiedTreatedAsLax | sameSiteNoneInsecure | userPreferences | syntaxError | schemeNotSupported | overwriteSecure | invalidDomain | invalidPrefix | unknownError | schemefulSameSiteStrict | schemefulSameSiteLax | schemefulSameSiteUnspecifiedTreatedAsLax | samePartyFromCrossPartyContext | samePartyConflictsWithOtherAttributes | nameValuePairExceedsMaxSize
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SetCookieBlockedReason where
  parseJSON
    | .string "SecureOnly" => .ok .secureOnly
    | .string "SameSiteStrict" => .ok .sameSiteStrict
    | .string "SameSiteLax" => .ok .sameSiteLax
    | .string "SameSiteUnspecifiedTreatedAsLax" => .ok .sameSiteUnspecifiedTreatedAsLax
    | .string "SameSiteNoneInsecure" => .ok .sameSiteNoneInsecure
    | .string "UserPreferences" => .ok .userPreferences
    | .string "SyntaxError" => .ok .syntaxError
    | .string "SchemeNotSupported" => .ok .schemeNotSupported
    | .string "OverwriteSecure" => .ok .overwriteSecure
    | .string "InvalidDomain" => .ok .invalidDomain
    | .string "InvalidPrefix" => .ok .invalidPrefix
    | .string "UnknownError" => .ok .unknownError
    | .string "SchemefulSameSiteStrict" => .ok .schemefulSameSiteStrict
    | .string "SchemefulSameSiteLax" => .ok .schemefulSameSiteLax
    | .string "SchemefulSameSiteUnspecifiedTreatedAsLax" => .ok .schemefulSameSiteUnspecifiedTreatedAsLax
    | .string "SamePartyFromCrossPartyContext" => .ok .samePartyFromCrossPartyContext
    | .string "SamePartyConflictsWithOtherAttributes" => .ok .samePartyConflictsWithOtherAttributes
    | .string "NameValuePairExceedsMaxSize" => .ok .nameValuePairExceedsMaxSize
    | v => .error s!"failed to parse Network.SetCookieBlockedReason: {repr v}"
instance : ToJSON Network.SetCookieBlockedReason where
  toJSON
    | .secureOnly => .string "SecureOnly"
    | .sameSiteStrict => .string "SameSiteStrict"
    | .sameSiteLax => .string "SameSiteLax"
    | .sameSiteUnspecifiedTreatedAsLax => .string "SameSiteUnspecifiedTreatedAsLax"
    | .sameSiteNoneInsecure => .string "SameSiteNoneInsecure"
    | .userPreferences => .string "UserPreferences"
    | .syntaxError => .string "SyntaxError"
    | .schemeNotSupported => .string "SchemeNotSupported"
    | .overwriteSecure => .string "OverwriteSecure"
    | .invalidDomain => .string "InvalidDomain"
    | .invalidPrefix => .string "InvalidPrefix"
    | .unknownError => .string "UnknownError"
    | .schemefulSameSiteStrict => .string "SchemefulSameSiteStrict"
    | .schemefulSameSiteLax => .string "SchemefulSameSiteLax"
    | .schemefulSameSiteUnspecifiedTreatedAsLax => .string "SchemefulSameSiteUnspecifiedTreatedAsLax"
    | .samePartyFromCrossPartyContext => .string "SamePartyFromCrossPartyContext"
    | .samePartyConflictsWithOtherAttributes => .string "SamePartyConflictsWithOtherAttributes"
    | .nameValuePairExceedsMaxSize => .string "NameValuePairExceedsMaxSize"

/-- `Network.CookieBlockedReason`. -/
inductive Network.CookieBlockedReason where
  | secureOnly | notOnPath | domainMismatch | sameSiteStrict | sameSiteLax | sameSiteUnspecifiedTreatedAsLax | sameSiteNoneInsecure | userPreferences | unknownError | schemefulSameSiteStrict | schemefulSameSiteLax | schemefulSameSiteUnspecifiedTreatedAsLax | samePartyFromCrossPartyContext | nameValuePairExceedsMaxSize
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CookieBlockedReason where
  parseJSON
    | .string "SecureOnly" => .ok .secureOnly
    | .string "NotOnPath" => .ok .notOnPath
    | .string "DomainMismatch" => .ok .domainMismatch
    | .string "SameSiteStrict" => .ok .sameSiteStrict
    | .string "SameSiteLax" => .ok .sameSiteLax
    | .string "SameSiteUnspecifiedTreatedAsLax" => .ok .sameSiteUnspecifiedTreatedAsLax
    | .string "SameSiteNoneInsecure" => .ok .sameSiteNoneInsecure
    | .string "UserPreferences" => .ok .userPreferences
    | .string "UnknownError" => .ok .unknownError
    | .string "SchemefulSameSiteStrict" => .ok .schemefulSameSiteStrict
    | .string "SchemefulSameSiteLax" => .ok .schemefulSameSiteLax
    | .string "SchemefulSameSiteUnspecifiedTreatedAsLax" => .ok .schemefulSameSiteUnspecifiedTreatedAsLax
    | .string "SamePartyFromCrossPartyContext" => .ok .samePartyFromCrossPartyContext
    | .string "NameValuePairExceedsMaxSize" => .ok .nameValuePairExceedsMaxSize
    | v => .error s!"failed to parse Network.CookieBlockedReason: {repr v}"
instance : ToJSON Network.CookieBlockedReason where
  toJSON
    | .secureOnly => .string "SecureOnly"
    | .notOnPath => .string "NotOnPath"
    | .domainMismatch => .string "DomainMismatch"
    | .sameSiteStrict => .string "SameSiteStrict"
    | .sameSiteLax => .string "SameSiteLax"
    | .sameSiteUnspecifiedTreatedAsLax => .string "SameSiteUnspecifiedTreatedAsLax"
    | .sameSiteNoneInsecure => .string "SameSiteNoneInsecure"
    | .userPreferences => .string "UserPreferences"
    | .unknownError => .string "UnknownError"
    | .schemefulSameSiteStrict => .string "SchemefulSameSiteStrict"
    | .schemefulSameSiteLax => .string "SchemefulSameSiteLax"
    | .schemefulSameSiteUnspecifiedTreatedAsLax => .string "SchemefulSameSiteUnspecifiedTreatedAsLax"
    | .samePartyFromCrossPartyContext => .string "SamePartyFromCrossPartyContext"
    | .nameValuePairExceedsMaxSize => .string "NameValuePairExceedsMaxSize"

/-- `Network.BlockedSetCookieWithReason`. -/
structure Network.BlockedSetCookieWithReason where
  blockedReasons : List Network.SetCookieBlockedReason
  cookieLine : String
  cookie : Option Network.Cookie := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.BlockedSetCookieWithReason where
  parseJSON v := do
    .ok
      { blockedReasons := ← Value.getField v "blockedReasons" >>= FromJSON.parseJSON
        cookieLine := ← Value.getField v "cookieLine" >>= FromJSON.parseJSON
        cookie := ← (← Value.getFieldOpt v "cookie").mapM FromJSON.parseJSON }
instance : ToJSON Network.BlockedSetCookieWithReason where
  toJSON p := Data.Json.object <|
       [("blockedReasons", ToJSON.toJSON p.blockedReasons)]
    ++ [("cookieLine", ToJSON.toJSON p.cookieLine)]
    ++ (p.cookie.map (fun x => ("cookie", ToJSON.toJSON x))).toList

/-- `Network.BlockedCookieWithReason`. -/
structure Network.BlockedCookieWithReason where
  blockedReasons : List Network.CookieBlockedReason
  cookie : Network.Cookie
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.BlockedCookieWithReason where
  parseJSON v := do
    .ok
      { blockedReasons := ← Value.getField v "blockedReasons" >>= FromJSON.parseJSON
        cookie := ← Value.getField v "cookie" >>= FromJSON.parseJSON }
instance : ToJSON Network.BlockedCookieWithReason where
  toJSON p := Data.Json.object <|
       [("blockedReasons", ToJSON.toJSON p.blockedReasons)]
    ++ [("cookie", ToJSON.toJSON p.cookie)]

/-- `Network.CookieParam`. -/
structure Network.CookieParam where
  name : String
  value : String
  url : Option String := none
  domain : Option String := none
  path : Option String := none
  secure : Option Bool := none
  httpOnly : Option Bool := none
  sameSite : Option Network.CookieSameSite := none
  expires : Option Network.TimeSinceEpoch := none
  priority : Option Network.CookiePriority := none
  sameParty : Option Bool := none
  sourceScheme : Option Network.CookieSourceScheme := none
  sourcePort : Option Int := none
  partitionKey : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CookieParam where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON
        url := ← (← Value.getFieldOpt v "url").mapM FromJSON.parseJSON
        domain := ← (← Value.getFieldOpt v "domain").mapM FromJSON.parseJSON
        path := ← (← Value.getFieldOpt v "path").mapM FromJSON.parseJSON
        secure := ← (← Value.getFieldOpt v "secure").mapM FromJSON.parseJSON
        httpOnly := ← (← Value.getFieldOpt v "httpOnly").mapM FromJSON.parseJSON
        sameSite := ← (← Value.getFieldOpt v "sameSite").mapM FromJSON.parseJSON
        expires := ← (← Value.getFieldOpt v "expires").mapM FromJSON.parseJSON
        priority := ← (← Value.getFieldOpt v "priority").mapM FromJSON.parseJSON
        sameParty := ← (← Value.getFieldOpt v "sameParty").mapM FromJSON.parseJSON
        sourceScheme := ← (← Value.getFieldOpt v "sourceScheme").mapM FromJSON.parseJSON
        sourcePort := ← (← Value.getFieldOpt v "sourcePort").mapM FromJSON.parseJSON
        partitionKey := ← (← Value.getFieldOpt v "partitionKey").mapM FromJSON.parseJSON }
instance : ToJSON Network.CookieParam where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ [("value", ToJSON.toJSON p.value)]
    ++ (p.url.map (fun x => ("url", ToJSON.toJSON x))).toList
    ++ (p.domain.map (fun x => ("domain", ToJSON.toJSON x))).toList
    ++ (p.path.map (fun x => ("path", ToJSON.toJSON x))).toList
    ++ (p.secure.map (fun x => ("secure", ToJSON.toJSON x))).toList
    ++ (p.httpOnly.map (fun x => ("httpOnly", ToJSON.toJSON x))).toList
    ++ (p.sameSite.map (fun x => ("sameSite", ToJSON.toJSON x))).toList
    ++ (p.expires.map (fun x => ("expires", ToJSON.toJSON x))).toList
    ++ (p.priority.map (fun x => ("priority", ToJSON.toJSON x))).toList
    ++ (p.sameParty.map (fun x => ("sameParty", ToJSON.toJSON x))).toList
    ++ (p.sourceScheme.map (fun x => ("sourceScheme", ToJSON.toJSON x))).toList
    ++ (p.sourcePort.map (fun x => ("sourcePort", ToJSON.toJSON x))).toList
    ++ (p.partitionKey.map (fun x => ("partitionKey", ToJSON.toJSON x))).toList

/-- `Network.AuthChallengeSource`. -/
inductive Network.AuthChallengeSource where
  | server | proxy
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.AuthChallengeSource where
  parseJSON
    | .string "Server" => .ok .server
    | .string "Proxy" => .ok .proxy
    | v => .error s!"failed to parse Network.AuthChallengeSource: {repr v}"
instance : ToJSON Network.AuthChallengeSource where
  toJSON
    | .server => .string "Server"
    | .proxy => .string "Proxy"

/-- `Network.AuthChallenge`. -/
structure Network.AuthChallenge where
  source : Option Network.AuthChallengeSource := none
  origin : String
  scheme : String
  realm : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.AuthChallenge where
  parseJSON v := do
    .ok
      { source := ← (← Value.getFieldOpt v "source").mapM FromJSON.parseJSON
        origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        scheme := ← Value.getField v "scheme" >>= FromJSON.parseJSON
        realm := ← Value.getField v "realm" >>= FromJSON.parseJSON }
instance : ToJSON Network.AuthChallenge where
  toJSON p := Data.Json.object <|
       (p.source.map (fun x => ("source", ToJSON.toJSON x))).toList
    ++ [("origin", ToJSON.toJSON p.origin)]
    ++ [("scheme", ToJSON.toJSON p.scheme)]
    ++ [("realm", ToJSON.toJSON p.realm)]

/-- `Network.AuthChallengeResponseResponse`. -/
inductive Network.AuthChallengeResponseResponse where
  | default | cancelAuth | provideCredentials
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.AuthChallengeResponseResponse where
  parseJSON
    | .string "Default" => .ok .default
    | .string "CancelAuth" => .ok .cancelAuth
    | .string "ProvideCredentials" => .ok .provideCredentials
    | v => .error s!"failed to parse Network.AuthChallengeResponseResponse: {repr v}"
instance : ToJSON Network.AuthChallengeResponseResponse where
  toJSON
    | .default => .string "Default"
    | .cancelAuth => .string "CancelAuth"
    | .provideCredentials => .string "ProvideCredentials"

/-- `Network.AuthChallengeResponse`. -/
structure Network.AuthChallengeResponse where
  response : Network.AuthChallengeResponseResponse
  username : Option String := none
  password : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.AuthChallengeResponse where
  parseJSON v := do
    .ok
      { response := ← Value.getField v "response" >>= FromJSON.parseJSON
        username := ← (← Value.getFieldOpt v "username").mapM FromJSON.parseJSON
        password := ← (← Value.getFieldOpt v "password").mapM FromJSON.parseJSON }
instance : ToJSON Network.AuthChallengeResponse where
  toJSON p := Data.Json.object <|
       [("response", ToJSON.toJSON p.response)]
    ++ (p.username.map (fun x => ("username", ToJSON.toJSON x))).toList
    ++ (p.password.map (fun x => ("password", ToJSON.toJSON x))).toList

/-- `Network.InterceptionStage`. -/
inductive Network.InterceptionStage where
  | request | headersReceived
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.InterceptionStage where
  parseJSON
    | .string "Request" => .ok .request
    | .string "HeadersReceived" => .ok .headersReceived
    | v => .error s!"failed to parse Network.InterceptionStage: {repr v}"
instance : ToJSON Network.InterceptionStage where
  toJSON
    | .request => .string "Request"
    | .headersReceived => .string "HeadersReceived"

/-- `Network.RequestPattern`. -/
structure Network.RequestPattern where
  urlPattern : Option String := none
  resourceType : Option Network.ResourceType := none
  interceptionStage : Option Network.InterceptionStage := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.RequestPattern where
  parseJSON v := do
    .ok
      { urlPattern := ← (← Value.getFieldOpt v "urlPattern").mapM FromJSON.parseJSON
        resourceType := ← (← Value.getFieldOpt v "resourceType").mapM FromJSON.parseJSON
        interceptionStage := ← (← Value.getFieldOpt v "interceptionStage").mapM FromJSON.parseJSON }
instance : ToJSON Network.RequestPattern where
  toJSON p := Data.Json.object <|
       (p.urlPattern.map (fun x => ("urlPattern", ToJSON.toJSON x))).toList
    ++ (p.resourceType.map (fun x => ("resourceType", ToJSON.toJSON x))).toList
    ++ (p.interceptionStage.map (fun x => ("interceptionStage", ToJSON.toJSON x))).toList

/-- `Network.SignedExchangeSignature`. -/
structure Network.SignedExchangeSignature where
  label : String
  signature : String
  integrity : String
  certUrl : Option String := none
  certSha256 : Option String := none
  validityUrl : String
  date : Int
  expires : Int
  certificates : Option (List String) := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SignedExchangeSignature where
  parseJSON v := do
    .ok
      { label := ← Value.getField v "label" >>= FromJSON.parseJSON
        signature := ← Value.getField v "signature" >>= FromJSON.parseJSON
        integrity := ← Value.getField v "integrity" >>= FromJSON.parseJSON
        certUrl := ← (← Value.getFieldOpt v "certUrl").mapM FromJSON.parseJSON
        certSha256 := ← (← Value.getFieldOpt v "certSha256").mapM FromJSON.parseJSON
        validityUrl := ← Value.getField v "validityUrl" >>= FromJSON.parseJSON
        date := ← Value.getField v "date" >>= FromJSON.parseJSON
        expires := ← Value.getField v "expires" >>= FromJSON.parseJSON
        certificates := ← (← Value.getFieldOpt v "certificates").mapM FromJSON.parseJSON }
instance : ToJSON Network.SignedExchangeSignature where
  toJSON p := Data.Json.object <|
       [("label", ToJSON.toJSON p.label)]
    ++ [("signature", ToJSON.toJSON p.signature)]
    ++ [("integrity", ToJSON.toJSON p.integrity)]
    ++ (p.certUrl.map (fun x => ("certUrl", ToJSON.toJSON x))).toList
    ++ (p.certSha256.map (fun x => ("certSha256", ToJSON.toJSON x))).toList
    ++ [("validityUrl", ToJSON.toJSON p.validityUrl)]
    ++ [("date", ToJSON.toJSON p.date)]
    ++ [("expires", ToJSON.toJSON p.expires)]
    ++ (p.certificates.map (fun x => ("certificates", ToJSON.toJSON x))).toList

/-- `Network.SignedExchangeHeader`. -/
structure Network.SignedExchangeHeader where
  requestUrl : String
  responseCode : Int
  responseHeaders : Network.Headers
  signatures : List Network.SignedExchangeSignature
  headerIntegrity : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SignedExchangeHeader where
  parseJSON v := do
    .ok
      { requestUrl := ← Value.getField v "requestUrl" >>= FromJSON.parseJSON
        responseCode := ← Value.getField v "responseCode" >>= FromJSON.parseJSON
        responseHeaders := ← Value.getField v "responseHeaders" >>= FromJSON.parseJSON
        signatures := ← Value.getField v "signatures" >>= FromJSON.parseJSON
        headerIntegrity := ← Value.getField v "headerIntegrity" >>= FromJSON.parseJSON }
instance : ToJSON Network.SignedExchangeHeader where
  toJSON p := Data.Json.object <|
       [("requestUrl", ToJSON.toJSON p.requestUrl)]
    ++ [("responseCode", ToJSON.toJSON p.responseCode)]
    ++ [("responseHeaders", ToJSON.toJSON p.responseHeaders)]
    ++ [("signatures", ToJSON.toJSON p.signatures)]
    ++ [("headerIntegrity", ToJSON.toJSON p.headerIntegrity)]

/-- `Network.SignedExchangeErrorField`. -/
inductive Network.SignedExchangeErrorField where
  | signatureSig | signatureIntegrity | signatureCertUrl | signatureCertSha256 | signatureValidityUrl | signatureTimestamps
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SignedExchangeErrorField where
  parseJSON
    | .string "signatureSig" => .ok .signatureSig
    | .string "signatureIntegrity" => .ok .signatureIntegrity
    | .string "signatureCertUrl" => .ok .signatureCertUrl
    | .string "signatureCertSha256" => .ok .signatureCertSha256
    | .string "signatureValidityUrl" => .ok .signatureValidityUrl
    | .string "signatureTimestamps" => .ok .signatureTimestamps
    | v => .error s!"failed to parse Network.SignedExchangeErrorField: {repr v}"
instance : ToJSON Network.SignedExchangeErrorField where
  toJSON
    | .signatureSig => .string "signatureSig"
    | .signatureIntegrity => .string "signatureIntegrity"
    | .signatureCertUrl => .string "signatureCertUrl"
    | .signatureCertSha256 => .string "signatureCertSha256"
    | .signatureValidityUrl => .string "signatureValidityUrl"
    | .signatureTimestamps => .string "signatureTimestamps"

/-- `Network.SignedExchangeError`. -/
structure Network.SignedExchangeError where
  message : String
  signatureIndex : Option Int := none
  errorField : Option Network.SignedExchangeErrorField := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SignedExchangeError where
  parseJSON v := do
    .ok
      { message := ← Value.getField v "message" >>= FromJSON.parseJSON
        signatureIndex := ← (← Value.getFieldOpt v "signatureIndex").mapM FromJSON.parseJSON
        errorField := ← (← Value.getFieldOpt v "errorField").mapM FromJSON.parseJSON }
instance : ToJSON Network.SignedExchangeError where
  toJSON p := Data.Json.object <|
       [("message", ToJSON.toJSON p.message)]
    ++ (p.signatureIndex.map (fun x => ("signatureIndex", ToJSON.toJSON x))).toList
    ++ (p.errorField.map (fun x => ("errorField", ToJSON.toJSON x))).toList

/-- `Network.SignedExchangeInfo`. -/
structure Network.SignedExchangeInfo where
  outerResponse : Network.Response
  header : Option Network.SignedExchangeHeader := none
  securityDetails : Option Network.SecurityDetails := none
  errors : Option (List Network.SignedExchangeError) := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SignedExchangeInfo where
  parseJSON v := do
    .ok
      { outerResponse := ← Value.getField v "outerResponse" >>= FromJSON.parseJSON
        header := ← (← Value.getFieldOpt v "header").mapM FromJSON.parseJSON
        securityDetails := ← (← Value.getFieldOpt v "securityDetails").mapM FromJSON.parseJSON
        errors := ← (← Value.getFieldOpt v "errors").mapM FromJSON.parseJSON }
instance : ToJSON Network.SignedExchangeInfo where
  toJSON p := Data.Json.object <|
       [("outerResponse", ToJSON.toJSON p.outerResponse)]
    ++ (p.header.map (fun x => ("header", ToJSON.toJSON x))).toList
    ++ (p.securityDetails.map (fun x => ("securityDetails", ToJSON.toJSON x))).toList
    ++ (p.errors.map (fun x => ("errors", ToJSON.toJSON x))).toList

/-- `Network.ContentEncoding`. -/
inductive Network.ContentEncoding where
  | deflate | gzip | br
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ContentEncoding where
  parseJSON
    | .string "deflate" => .ok .deflate
    | .string "gzip" => .ok .gzip
    | .string "br" => .ok .br
    | v => .error s!"failed to parse Network.ContentEncoding: {repr v}"
instance : ToJSON Network.ContentEncoding where
  toJSON
    | .deflate => .string "deflate"
    | .gzip => .string "gzip"
    | .br => .string "br"

/-- `Network.PrivateNetworkRequestPolicy`. -/
inductive Network.PrivateNetworkRequestPolicy where
  | allow | blockFromInsecureToMorePrivate | warnFromInsecureToMorePrivate | preflightBlock | preflightWarn
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.PrivateNetworkRequestPolicy where
  parseJSON
    | .string "Allow" => .ok .allow
    | .string "BlockFromInsecureToMorePrivate" => .ok .blockFromInsecureToMorePrivate
    | .string "WarnFromInsecureToMorePrivate" => .ok .warnFromInsecureToMorePrivate
    | .string "PreflightBlock" => .ok .preflightBlock
    | .string "PreflightWarn" => .ok .preflightWarn
    | v => .error s!"failed to parse Network.PrivateNetworkRequestPolicy: {repr v}"
instance : ToJSON Network.PrivateNetworkRequestPolicy where
  toJSON
    | .allow => .string "Allow"
    | .blockFromInsecureToMorePrivate => .string "BlockFromInsecureToMorePrivate"
    | .warnFromInsecureToMorePrivate => .string "WarnFromInsecureToMorePrivate"
    | .preflightBlock => .string "PreflightBlock"
    | .preflightWarn => .string "PreflightWarn"

/-- `Network.IPAddressSpace`. -/
inductive Network.IPAddressSpace where
  | «local» | «private» | «public» | unknown
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.IPAddressSpace where
  parseJSON
    | .string "Local" => .ok .«local»
    | .string "Private" => .ok .«private»
    | .string "Public" => .ok .«public»
    | .string "Unknown" => .ok .unknown
    | v => .error s!"failed to parse Network.IPAddressSpace: {repr v}"
instance : ToJSON Network.IPAddressSpace where
  toJSON
    | .«local» => .string "Local"
    | .«private» => .string "Private"
    | .«public» => .string "Public"
    | .unknown => .string "Unknown"

/-- `Network.ConnectTiming`. -/
structure Network.ConnectTiming where
  requestTime : Float
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ConnectTiming where
  parseJSON v := do
    .ok
      { requestTime := ← Value.getField v "requestTime" >>= FromJSON.parseJSON }
instance : ToJSON Network.ConnectTiming where
  toJSON p := Data.Json.object <|
       [("requestTime", ToJSON.toJSON p.requestTime)]

/-- `Network.ClientSecurityState`. -/
structure Network.ClientSecurityState where
  initiatorIsSecureContext : Bool
  initiatorIPAddressSpace : Network.IPAddressSpace
  privateNetworkRequestPolicy : Network.PrivateNetworkRequestPolicy
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ClientSecurityState where
  parseJSON v := do
    .ok
      { initiatorIsSecureContext := ← Value.getField v "initiatorIsSecureContext" >>= FromJSON.parseJSON
        initiatorIPAddressSpace := ← Value.getField v "initiatorIPAddressSpace" >>= FromJSON.parseJSON
        privateNetworkRequestPolicy := ← Value.getField v "privateNetworkRequestPolicy" >>= FromJSON.parseJSON }
instance : ToJSON Network.ClientSecurityState where
  toJSON p := Data.Json.object <|
       [("initiatorIsSecureContext", ToJSON.toJSON p.initiatorIsSecureContext)]
    ++ [("initiatorIPAddressSpace", ToJSON.toJSON p.initiatorIPAddressSpace)]
    ++ [("privateNetworkRequestPolicy", ToJSON.toJSON p.privateNetworkRequestPolicy)]

/-- `Network.CrossOriginOpenerPolicyValue`. -/
inductive Network.CrossOriginOpenerPolicyValue where
  | sameOrigin | sameOriginAllowPopups | restrictProperties | unsafeNone | sameOriginPlusCoep | restrictPropertiesPlusCoep
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CrossOriginOpenerPolicyValue where
  parseJSON
    | .string "SameOrigin" => .ok .sameOrigin
    | .string "SameOriginAllowPopups" => .ok .sameOriginAllowPopups
    | .string "RestrictProperties" => .ok .restrictProperties
    | .string "UnsafeNone" => .ok .unsafeNone
    | .string "SameOriginPlusCoep" => .ok .sameOriginPlusCoep
    | .string "RestrictPropertiesPlusCoep" => .ok .restrictPropertiesPlusCoep
    | v => .error s!"failed to parse Network.CrossOriginOpenerPolicyValue: {repr v}"
instance : ToJSON Network.CrossOriginOpenerPolicyValue where
  toJSON
    | .sameOrigin => .string "SameOrigin"
    | .sameOriginAllowPopups => .string "SameOriginAllowPopups"
    | .restrictProperties => .string "RestrictProperties"
    | .unsafeNone => .string "UnsafeNone"
    | .sameOriginPlusCoep => .string "SameOriginPlusCoep"
    | .restrictPropertiesPlusCoep => .string "RestrictPropertiesPlusCoep"

/-- `Network.CrossOriginOpenerPolicyStatus`. -/
structure Network.CrossOriginOpenerPolicyStatus where
  value : Network.CrossOriginOpenerPolicyValue
  reportOnlyValue : Network.CrossOriginOpenerPolicyValue
  reportingEndpoint : Option String := none
  reportOnlyReportingEndpoint : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CrossOriginOpenerPolicyStatus where
  parseJSON v := do
    .ok
      { value := ← Value.getField v "value" >>= FromJSON.parseJSON
        reportOnlyValue := ← Value.getField v "reportOnlyValue" >>= FromJSON.parseJSON
        reportingEndpoint := ← (← Value.getFieldOpt v "reportingEndpoint").mapM FromJSON.parseJSON
        reportOnlyReportingEndpoint := ← (← Value.getFieldOpt v "reportOnlyReportingEndpoint").mapM FromJSON.parseJSON }
instance : ToJSON Network.CrossOriginOpenerPolicyStatus where
  toJSON p := Data.Json.object <|
       [("value", ToJSON.toJSON p.value)]
    ++ [("reportOnlyValue", ToJSON.toJSON p.reportOnlyValue)]
    ++ (p.reportingEndpoint.map (fun x => ("reportingEndpoint", ToJSON.toJSON x))).toList
    ++ (p.reportOnlyReportingEndpoint.map (fun x => ("reportOnlyReportingEndpoint", ToJSON.toJSON x))).toList

/-- `Network.CrossOriginEmbedderPolicyValue`. -/
inductive Network.CrossOriginEmbedderPolicyValue where
  | none | credentialless | requireCorp
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CrossOriginEmbedderPolicyValue where
  parseJSON
    | .string "None" => .ok .none
    | .string "Credentialless" => .ok .credentialless
    | .string "RequireCorp" => .ok .requireCorp
    | v => .error s!"failed to parse Network.CrossOriginEmbedderPolicyValue: {repr v}"
instance : ToJSON Network.CrossOriginEmbedderPolicyValue where
  toJSON
    | .none => .string "None"
    | .credentialless => .string "Credentialless"
    | .requireCorp => .string "RequireCorp"

/-- `Network.CrossOriginEmbedderPolicyStatus`. -/
structure Network.CrossOriginEmbedderPolicyStatus where
  value : Network.CrossOriginEmbedderPolicyValue
  reportOnlyValue : Network.CrossOriginEmbedderPolicyValue
  reportingEndpoint : Option String := none
  reportOnlyReportingEndpoint : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.CrossOriginEmbedderPolicyStatus where
  parseJSON v := do
    .ok
      { value := ← Value.getField v "value" >>= FromJSON.parseJSON
        reportOnlyValue := ← Value.getField v "reportOnlyValue" >>= FromJSON.parseJSON
        reportingEndpoint := ← (← Value.getFieldOpt v "reportingEndpoint").mapM FromJSON.parseJSON
        reportOnlyReportingEndpoint := ← (← Value.getFieldOpt v "reportOnlyReportingEndpoint").mapM FromJSON.parseJSON }
instance : ToJSON Network.CrossOriginEmbedderPolicyStatus where
  toJSON p := Data.Json.object <|
       [("value", ToJSON.toJSON p.value)]
    ++ [("reportOnlyValue", ToJSON.toJSON p.reportOnlyValue)]
    ++ (p.reportingEndpoint.map (fun x => ("reportingEndpoint", ToJSON.toJSON x))).toList
    ++ (p.reportOnlyReportingEndpoint.map (fun x => ("reportOnlyReportingEndpoint", ToJSON.toJSON x))).toList

/-- `Network.SecurityIsolationStatus`. -/
structure Network.SecurityIsolationStatus where
  coop : Option Network.CrossOriginOpenerPolicyStatus := none
  coep : Option Network.CrossOriginEmbedderPolicyStatus := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SecurityIsolationStatus where
  parseJSON v := do
    .ok
      { coop := ← (← Value.getFieldOpt v "coop").mapM FromJSON.parseJSON
        coep := ← (← Value.getFieldOpt v "coep").mapM FromJSON.parseJSON }
instance : ToJSON Network.SecurityIsolationStatus where
  toJSON p := Data.Json.object <|
       (p.coop.map (fun x => ("coop", ToJSON.toJSON x))).toList
    ++ (p.coep.map (fun x => ("coep", ToJSON.toJSON x))).toList

/-- `Network.ReportStatus`. -/
inductive Network.ReportStatus where
  | queued | pending | markedForRemoval | success
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ReportStatus where
  parseJSON
    | .string "Queued" => .ok .queued
    | .string "Pending" => .ok .pending
    | .string "MarkedForRemoval" => .ok .markedForRemoval
    | .string "Success" => .ok .success
    | v => .error s!"failed to parse Network.ReportStatus: {repr v}"
instance : ToJSON Network.ReportStatus where
  toJSON
    | .queued => .string "Queued"
    | .pending => .string "Pending"
    | .markedForRemoval => .string "MarkedForRemoval"
    | .success => .string "Success"

/-- `Network.ReportId`. -/
abbrev Network.ReportId := String

/-- `Network.ReportingApiReport`. -/
structure Network.ReportingApiReport where
  id : Network.ReportId
  initiatorUrl : String
  destination : String
  type : String
  timestamp : Network.TimeSinceEpoch
  depth : Int
  completedAttempts : Int
  body : List (String × String)
  status : Network.ReportStatus
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ReportingApiReport where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        initiatorUrl := ← Value.getField v "initiatorUrl" >>= FromJSON.parseJSON
        destination := ← Value.getField v "destination" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        depth := ← Value.getField v "depth" >>= FromJSON.parseJSON
        completedAttempts := ← Value.getField v "completedAttempts" >>= FromJSON.parseJSON
        body := ← Value.getField v "body" >>= FromJSON.parseJSON
        status := ← Value.getField v "status" >>= FromJSON.parseJSON }
instance : ToJSON Network.ReportingApiReport where
  toJSON p := Data.Json.object <|
       [("id", ToJSON.toJSON p.id)]
    ++ [("initiatorUrl", ToJSON.toJSON p.initiatorUrl)]
    ++ [("destination", ToJSON.toJSON p.destination)]
    ++ [("type", ToJSON.toJSON p.type)]
    ++ [("timestamp", ToJSON.toJSON p.timestamp)]
    ++ [("depth", ToJSON.toJSON p.depth)]
    ++ [("completedAttempts", ToJSON.toJSON p.completedAttempts)]
    ++ [("body", ToJSON.toJSON p.body)]
    ++ [("status", ToJSON.toJSON p.status)]

/-- `Network.ReportingApiEndpoint`. -/
structure Network.ReportingApiEndpoint where
  url : String
  groupName : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ReportingApiEndpoint where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        groupName := ← Value.getField v "groupName" >>= FromJSON.parseJSON }
instance : ToJSON Network.ReportingApiEndpoint where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ [("groupName", ToJSON.toJSON p.groupName)]

/-- `Network.LoadNetworkResourcePageResult`. -/
structure Network.LoadNetworkResourcePageResult where
  success : Bool
  netError : Option Float := none
  netErrorName : Option String := none
  httpStatusCode : Option Float := none
  stream : Option CDP.Domains.IO.StreamHandle := none
  headers : Option Network.Headers := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.LoadNetworkResourcePageResult where
  parseJSON v := do
    .ok
      { success := ← Value.getField v "success" >>= FromJSON.parseJSON
        netError := ← (← Value.getFieldOpt v "netError").mapM FromJSON.parseJSON
        netErrorName := ← (← Value.getFieldOpt v "netErrorName").mapM FromJSON.parseJSON
        httpStatusCode := ← (← Value.getFieldOpt v "httpStatusCode").mapM FromJSON.parseJSON
        stream := ← (← Value.getFieldOpt v "stream").mapM FromJSON.parseJSON
        headers := ← (← Value.getFieldOpt v "headers").mapM FromJSON.parseJSON }
instance : ToJSON Network.LoadNetworkResourcePageResult where
  toJSON p := Data.Json.object <|
       [("success", ToJSON.toJSON p.success)]
    ++ (p.netError.map (fun x => ("netError", ToJSON.toJSON x))).toList
    ++ (p.netErrorName.map (fun x => ("netErrorName", ToJSON.toJSON x))).toList
    ++ (p.httpStatusCode.map (fun x => ("httpStatusCode", ToJSON.toJSON x))).toList
    ++ (p.stream.map (fun x => ("stream", ToJSON.toJSON x))).toList
    ++ (p.headers.map (fun x => ("headers", ToJSON.toJSON x))).toList

/-- `Network.LoadNetworkResourceOptions`. -/
structure Network.LoadNetworkResourceOptions where
  disableCache : Bool
  includeCredentials : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.LoadNetworkResourceOptions where
  parseJSON v := do
    .ok
      { disableCache := ← Value.getField v "disableCache" >>= FromJSON.parseJSON
        includeCredentials := ← Value.getField v "includeCredentials" >>= FromJSON.parseJSON }
instance : ToJSON Network.LoadNetworkResourceOptions where
  toJSON p := Data.Json.object <|
       [("disableCache", ToJSON.toJSON p.disableCache)]
    ++ [("includeCredentials", ToJSON.toJSON p.includeCredentials)]

/-- `Network.DataReceived`. -/
structure Network.DataReceived where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  dataLength : Int
  encodedDataLength : Int
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.DataReceived where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        dataLength := ← Value.getField v "dataLength" >>= FromJSON.parseJSON
        encodedDataLength := ← Value.getField v "encodedDataLength" >>= FromJSON.parseJSON }
instance : Event Network.DataReceived where
  eventName := "Network.dataReceived"

/-- `Network.EventSourceMessageReceived`. -/
structure Network.EventSourceMessageReceived where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  eventName : String
  eventId : String
  data : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.EventSourceMessageReceived where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        eventName := ← Value.getField v "eventName" >>= FromJSON.parseJSON
        eventId := ← Value.getField v "eventId" >>= FromJSON.parseJSON
        data := ← Value.getField v "data" >>= FromJSON.parseJSON }
instance : Event Network.EventSourceMessageReceived where
  eventName := "Network.eventSourceMessageReceived"

/-- `Network.LoadingFailed`. -/
structure Network.LoadingFailed where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  type : Network.ResourceType
  errorText : String
  canceled : Option Bool := none
  blockedReason : Option Network.BlockedReason := none
  corsErrorStatus : Option Network.CorsErrorStatus := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.LoadingFailed where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        errorText := ← Value.getField v "errorText" >>= FromJSON.parseJSON
        canceled := ← (← Value.getFieldOpt v "canceled").mapM FromJSON.parseJSON
        blockedReason := ← (← Value.getFieldOpt v "blockedReason").mapM FromJSON.parseJSON
        corsErrorStatus := ← (← Value.getFieldOpt v "corsErrorStatus").mapM FromJSON.parseJSON }
instance : Event Network.LoadingFailed where
  eventName := "Network.loadingFailed"

/-- `Network.LoadingFinished`. -/
structure Network.LoadingFinished where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  encodedDataLength : Float
  shouldReportCorbBlocking : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.LoadingFinished where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        encodedDataLength := ← Value.getField v "encodedDataLength" >>= FromJSON.parseJSON
        shouldReportCorbBlocking := ← (← Value.getFieldOpt v "shouldReportCorbBlocking").mapM FromJSON.parseJSON }
instance : Event Network.LoadingFinished where
  eventName := "Network.loadingFinished"

/-- `Network.RequestServedFromCache`. -/
structure Network.RequestServedFromCache where
  requestId : Network.RequestId
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.RequestServedFromCache where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON }
instance : Event Network.RequestServedFromCache where
  eventName := "Network.requestServedFromCache"

/-- `Network.RequestWillBeSent`. -/
structure Network.RequestWillBeSent where
  requestId : Network.RequestId
  loaderId : Network.LoaderId
  documentURL : String
  request : Network.Request
  timestamp : Network.MonotonicTime
  wallTime : Network.TimeSinceEpoch
  initiator : Network.Initiator
  redirectHasExtraInfo : Bool
  redirectResponse : Option Network.Response := none
  type : Option Network.ResourceType := none
  frameId : Option Page.FrameId := none
  hasUserGesture : Option Bool := none
  deriving Repr, BEq
instance : FromJSON Network.RequestWillBeSent where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        loaderId := ← Value.getField v "loaderId" >>= FromJSON.parseJSON
        documentURL := ← Value.getField v "documentURL" >>= FromJSON.parseJSON
        request := ← Value.getField v "request" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        wallTime := ← Value.getField v "wallTime" >>= FromJSON.parseJSON
        initiator := ← Value.getField v "initiator" >>= FromJSON.parseJSON
        redirectHasExtraInfo := ← Value.getField v "redirectHasExtraInfo" >>= FromJSON.parseJSON
        redirectResponse := ← (← Value.getFieldOpt v "redirectResponse").mapM FromJSON.parseJSON
        type := ← (← Value.getFieldOpt v "type").mapM FromJSON.parseJSON
        frameId := ← (← Value.getFieldOpt v "frameId").mapM FromJSON.parseJSON
        hasUserGesture := ← (← Value.getFieldOpt v "hasUserGesture").mapM FromJSON.parseJSON }
instance : Event Network.RequestWillBeSent where
  eventName := "Network.requestWillBeSent"

/-- `Network.ResourceChangedPriority`. -/
structure Network.ResourceChangedPriority where
  requestId : Network.RequestId
  newPriority : Network.ResourcePriority
  timestamp : Network.MonotonicTime
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ResourceChangedPriority where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        newPriority := ← Value.getField v "newPriority" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }
instance : Event Network.ResourceChangedPriority where
  eventName := "Network.resourceChangedPriority"

/-- `Network.SignedExchangeReceived`. -/
structure Network.SignedExchangeReceived where
  requestId : Network.RequestId
  info : Network.SignedExchangeInfo
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SignedExchangeReceived where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        info := ← Value.getField v "info" >>= FromJSON.parseJSON }
instance : Event Network.SignedExchangeReceived where
  eventName := "Network.signedExchangeReceived"

/-- `Network.ResponseReceived`. -/
structure Network.ResponseReceived where
  requestId : Network.RequestId
  loaderId : Network.LoaderId
  timestamp : Network.MonotonicTime
  type : Network.ResourceType
  response : Network.Response
  hasExtraInfo : Bool
  frameId : Option Page.FrameId := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ResponseReceived where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        loaderId := ← Value.getField v "loaderId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        response := ← Value.getField v "response" >>= FromJSON.parseJSON
        hasExtraInfo := ← Value.getField v "hasExtraInfo" >>= FromJSON.parseJSON
        frameId := ← (← Value.getFieldOpt v "frameId").mapM FromJSON.parseJSON }
instance : Event Network.ResponseReceived where
  eventName := "Network.responseReceived"

/-- `Network.WebSocketClosed`. -/
structure Network.WebSocketClosed where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketClosed where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }
instance : Event Network.WebSocketClosed where
  eventName := "Network.webSocketClosed"

/-- `Network.WebSocketCreated`. -/
structure Network.WebSocketCreated where
  requestId : Network.RequestId
  url : String
  initiator : Option Network.Initiator := none
  deriving Repr, BEq
instance : FromJSON Network.WebSocketCreated where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        initiator := ← (← Value.getFieldOpt v "initiator").mapM FromJSON.parseJSON }
instance : Event Network.WebSocketCreated where
  eventName := "Network.webSocketCreated"

/-- `Network.WebSocketFrameError`. -/
structure Network.WebSocketFrameError where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  errorMessage : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketFrameError where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        errorMessage := ← Value.getField v "errorMessage" >>= FromJSON.parseJSON }
instance : Event Network.WebSocketFrameError where
  eventName := "Network.webSocketFrameError"

/-- `Network.WebSocketFrameReceived`. -/
structure Network.WebSocketFrameReceived where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  response : Network.WebSocketFrame
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketFrameReceived where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        response := ← Value.getField v "response" >>= FromJSON.parseJSON }
instance : Event Network.WebSocketFrameReceived where
  eventName := "Network.webSocketFrameReceived"

/-- `Network.WebSocketFrameSent`. -/
structure Network.WebSocketFrameSent where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  response : Network.WebSocketFrame
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketFrameSent where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        response := ← Value.getField v "response" >>= FromJSON.parseJSON }
instance : Event Network.WebSocketFrameSent where
  eventName := "Network.webSocketFrameSent"

/-- `Network.WebSocketHandshakeResponseReceived`. -/
structure Network.WebSocketHandshakeResponseReceived where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  response : Network.WebSocketResponse
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketHandshakeResponseReceived where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        response := ← Value.getField v "response" >>= FromJSON.parseJSON }
instance : Event Network.WebSocketHandshakeResponseReceived where
  eventName := "Network.webSocketHandshakeResponseReceived"

/-- `Network.WebSocketWillSendHandshakeRequest`. -/
structure Network.WebSocketWillSendHandshakeRequest where
  requestId : Network.RequestId
  timestamp : Network.MonotonicTime
  wallTime : Network.TimeSinceEpoch
  request : Network.WebSocketRequest
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebSocketWillSendHandshakeRequest where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        wallTime := ← Value.getField v "wallTime" >>= FromJSON.parseJSON
        request := ← Value.getField v "request" >>= FromJSON.parseJSON }
instance : Event Network.WebSocketWillSendHandshakeRequest where
  eventName := "Network.webSocketWillSendHandshakeRequest"

/-- `Network.WebTransportCreated`. -/
structure Network.WebTransportCreated where
  transportId : Network.RequestId
  url : String
  timestamp : Network.MonotonicTime
  initiator : Option Network.Initiator := none
  deriving Repr, BEq
instance : FromJSON Network.WebTransportCreated where
  parseJSON v := do
    .ok
      { transportId := ← Value.getField v "transportId" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        initiator := ← (← Value.getFieldOpt v "initiator").mapM FromJSON.parseJSON }
instance : Event Network.WebTransportCreated where
  eventName := "Network.webTransportCreated"

/-- `Network.WebTransportConnectionEstablished`. -/
structure Network.WebTransportConnectionEstablished where
  transportId : Network.RequestId
  timestamp : Network.MonotonicTime
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebTransportConnectionEstablished where
  parseJSON v := do
    .ok
      { transportId := ← Value.getField v "transportId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }
instance : Event Network.WebTransportConnectionEstablished where
  eventName := "Network.webTransportConnectionEstablished"

/-- `Network.WebTransportClosed`. -/
structure Network.WebTransportClosed where
  transportId : Network.RequestId
  timestamp : Network.MonotonicTime
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.WebTransportClosed where
  parseJSON v := do
    .ok
      { transportId := ← Value.getField v "transportId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }
instance : Event Network.WebTransportClosed where
  eventName := "Network.webTransportClosed"

/-- `Network.RequestWillBeSentExtraInfo`. -/
structure Network.RequestWillBeSentExtraInfo where
  requestId : Network.RequestId
  associatedCookies : List Network.BlockedCookieWithReason
  headers : Network.Headers
  connectTiming : Network.ConnectTiming
  clientSecurityState : Option Network.ClientSecurityState := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.RequestWillBeSentExtraInfo where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        associatedCookies := ← Value.getField v "associatedCookies" >>= FromJSON.parseJSON
        headers := ← Value.getField v "headers" >>= FromJSON.parseJSON
        connectTiming := ← Value.getField v "connectTiming" >>= FromJSON.parseJSON
        clientSecurityState := ← (← Value.getFieldOpt v "clientSecurityState").mapM FromJSON.parseJSON }
instance : Event Network.RequestWillBeSentExtraInfo where
  eventName := "Network.requestWillBeSentExtraInfo"

/-- `Network.ResponseReceivedExtraInfo`. -/
structure Network.ResponseReceivedExtraInfo where
  requestId : Network.RequestId
  blockedCookies : List Network.BlockedSetCookieWithReason
  headers : Network.Headers
  resourceIPAddressSpace : Network.IPAddressSpace
  statusCode : Int
  headersText : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ResponseReceivedExtraInfo where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        blockedCookies := ← Value.getField v "blockedCookies" >>= FromJSON.parseJSON
        headers := ← Value.getField v "headers" >>= FromJSON.parseJSON
        resourceIPAddressSpace := ← Value.getField v "resourceIPAddressSpace" >>= FromJSON.parseJSON
        statusCode := ← Value.getField v "statusCode" >>= FromJSON.parseJSON
        headersText := ← (← Value.getFieldOpt v "headersText").mapM FromJSON.parseJSON }
instance : Event Network.ResponseReceivedExtraInfo where
  eventName := "Network.responseReceivedExtraInfo"

/-- `Network.TrustTokenOperationDoneStatus`. -/
inductive Network.TrustTokenOperationDoneStatus where
  | ok | invalidArgument | failedPrecondition | resourceExhausted | alreadyExists | unavailable | badResponse | internalError | unknownError | fulfilledLocally
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.TrustTokenOperationDoneStatus where
  parseJSON
    | .string "Ok" => .ok .ok
    | .string "InvalidArgument" => .ok .invalidArgument
    | .string "FailedPrecondition" => .ok .failedPrecondition
    | .string "ResourceExhausted" => .ok .resourceExhausted
    | .string "AlreadyExists" => .ok .alreadyExists
    | .string "Unavailable" => .ok .unavailable
    | .string "BadResponse" => .ok .badResponse
    | .string "InternalError" => .ok .internalError
    | .string "UnknownError" => .ok .unknownError
    | .string "FulfilledLocally" => .ok .fulfilledLocally
    | v => .error s!"failed to parse Network.TrustTokenOperationDoneStatus: {repr v}"
instance : ToJSON Network.TrustTokenOperationDoneStatus where
  toJSON
    | .ok => .string "Ok"
    | .invalidArgument => .string "InvalidArgument"
    | .failedPrecondition => .string "FailedPrecondition"
    | .resourceExhausted => .string "ResourceExhausted"
    | .alreadyExists => .string "AlreadyExists"
    | .unavailable => .string "Unavailable"
    | .badResponse => .string "BadResponse"
    | .internalError => .string "InternalError"
    | .unknownError => .string "UnknownError"
    | .fulfilledLocally => .string "FulfilledLocally"

/-- `Network.TrustTokenOperationDone`. -/
structure Network.TrustTokenOperationDone where
  status : Network.TrustTokenOperationDoneStatus
  type : Network.TrustTokenOperationType
  requestId : Network.RequestId
  topLevelOrigin : Option String := none
  issuerOrigin : Option String := none
  issuedTokenCount : Option Int := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.TrustTokenOperationDone where
  parseJSON v := do
    .ok
      { status := ← Value.getField v "status" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        topLevelOrigin := ← (← Value.getFieldOpt v "topLevelOrigin").mapM FromJSON.parseJSON
        issuerOrigin := ← (← Value.getFieldOpt v "issuerOrigin").mapM FromJSON.parseJSON
        issuedTokenCount := ← (← Value.getFieldOpt v "issuedTokenCount").mapM FromJSON.parseJSON }
instance : Event Network.TrustTokenOperationDone where
  eventName := "Network.trustTokenOperationDone"

/-- `Network.SubresourceWebBundleMetadataReceived`. -/
structure Network.SubresourceWebBundleMetadataReceived where
  requestId : Network.RequestId
  urls : List String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SubresourceWebBundleMetadataReceived where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        urls := ← Value.getField v "urls" >>= FromJSON.parseJSON }
instance : Event Network.SubresourceWebBundleMetadataReceived where
  eventName := "Network.subresourceWebBundleMetadataReceived"

/-- `Network.SubresourceWebBundleMetadataError`. -/
structure Network.SubresourceWebBundleMetadataError where
  requestId : Network.RequestId
  errorMessage : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SubresourceWebBundleMetadataError where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        errorMessage := ← Value.getField v "errorMessage" >>= FromJSON.parseJSON }
instance : Event Network.SubresourceWebBundleMetadataError where
  eventName := "Network.subresourceWebBundleMetadataError"

/-- `Network.SubresourceWebBundleInnerResponseParsed`. -/
structure Network.SubresourceWebBundleInnerResponseParsed where
  innerRequestId : Network.RequestId
  innerRequestURL : String
  bundleRequestId : Option Network.RequestId := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SubresourceWebBundleInnerResponseParsed where
  parseJSON v := do
    .ok
      { innerRequestId := ← Value.getField v "innerRequestId" >>= FromJSON.parseJSON
        innerRequestURL := ← Value.getField v "innerRequestURL" >>= FromJSON.parseJSON
        bundleRequestId := ← (← Value.getFieldOpt v "bundleRequestId").mapM FromJSON.parseJSON }
instance : Event Network.SubresourceWebBundleInnerResponseParsed where
  eventName := "Network.subresourceWebBundleInnerResponseParsed"

/-- `Network.SubresourceWebBundleInnerResponseError`. -/
structure Network.SubresourceWebBundleInnerResponseError where
  innerRequestId : Network.RequestId
  innerRequestURL : String
  errorMessage : String
  bundleRequestId : Option Network.RequestId := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SubresourceWebBundleInnerResponseError where
  parseJSON v := do
    .ok
      { innerRequestId := ← Value.getField v "innerRequestId" >>= FromJSON.parseJSON
        innerRequestURL := ← Value.getField v "innerRequestURL" >>= FromJSON.parseJSON
        errorMessage := ← Value.getField v "errorMessage" >>= FromJSON.parseJSON
        bundleRequestId := ← (← Value.getFieldOpt v "bundleRequestId").mapM FromJSON.parseJSON }
instance : Event Network.SubresourceWebBundleInnerResponseError where
  eventName := "Network.subresourceWebBundleInnerResponseError"

/-- `Network.ReportingApiReportAdded`. -/
structure Network.ReportingApiReportAdded where
  report : Network.ReportingApiReport
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ReportingApiReportAdded where
  parseJSON v := do
    .ok
      { report := ← Value.getField v "report" >>= FromJSON.parseJSON }
instance : Event Network.ReportingApiReportAdded where
  eventName := "Network.reportingApiReportAdded"

/-- `Network.ReportingApiReportUpdated`. -/
structure Network.ReportingApiReportUpdated where
  report : Network.ReportingApiReport
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ReportingApiReportUpdated where
  parseJSON v := do
    .ok
      { report := ← Value.getField v "report" >>= FromJSON.parseJSON }
instance : Event Network.ReportingApiReportUpdated where
  eventName := "Network.reportingApiReportUpdated"

/-- `Network.ReportingApiEndpointsChangedForOrigin`. -/
structure Network.ReportingApiEndpointsChangedForOrigin where
  origin : String
  endpoints : List Network.ReportingApiEndpoint
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.ReportingApiEndpointsChangedForOrigin where
  parseJSON v := do
    .ok
      { origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        endpoints := ← Value.getField v "endpoints" >>= FromJSON.parseJSON }
instance : Event Network.ReportingApiEndpointsChangedForOrigin where
  eventName := "Network.reportingApiEndpointsChangedForOrigin"

/-- `Network.PSetAcceptedEncodings`. -/
structure Network.PSetAcceptedEncodings where
  encodings : List Network.ContentEncoding
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetAcceptedEncodings where
  toJSON p := Data.Json.object <|
       [("encodings", ToJSON.toJSON p.encodings)]
instance : Command Network.PSetAcceptedEncodings where
  Response := Unit
  commandName _ := "Network.setAcceptedEncodings"
  decodeResponse _ := .ok ()

/-- `Network.PClearAcceptedEncodingsOverride`. -/
structure Network.PClearAcceptedEncodingsOverride where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PClearAcceptedEncodingsOverride where toJSON _ := .null
instance : Command Network.PClearAcceptedEncodingsOverride where
  Response := Unit
  commandName _ := "Network.clearAcceptedEncodingsOverride"
  decodeResponse _ := .ok ()

/-- `Network.PClearBrowserCache`. -/
structure Network.PClearBrowserCache where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PClearBrowserCache where toJSON _ := .null
instance : Command Network.PClearBrowserCache where
  Response := Unit
  commandName _ := "Network.clearBrowserCache"
  decodeResponse _ := .ok ()

/-- `Network.PClearBrowserCookies`. -/
structure Network.PClearBrowserCookies where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PClearBrowserCookies where toJSON _ := .null
instance : Command Network.PClearBrowserCookies where
  Response := Unit
  commandName _ := "Network.clearBrowserCookies"
  decodeResponse _ := .ok ()

/-- `Network.PDeleteCookies`. -/
structure Network.PDeleteCookies where
  name : String
  url : Option String := none
  domain : Option String := none
  path : Option String := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PDeleteCookies where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ (p.url.map (fun x => ("url", ToJSON.toJSON x))).toList
    ++ (p.domain.map (fun x => ("domain", ToJSON.toJSON x))).toList
    ++ (p.path.map (fun x => ("path", ToJSON.toJSON x))).toList
instance : Command Network.PDeleteCookies where
  Response := Unit
  commandName _ := "Network.deleteCookies"
  decodeResponse _ := .ok ()

/-- `Network.PDisable`. -/
structure Network.PDisable where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PDisable where toJSON _ := .null
instance : Command Network.PDisable where
  Response := Unit
  commandName _ := "Network.disable"
  decodeResponse _ := .ok ()

/-- `Network.PEmulateNetworkConditions`. -/
structure Network.PEmulateNetworkConditions where
  offline : Bool
  latency : Float
  downloadThroughput : Float
  uploadThroughput : Float
  connectionType : Option Network.ConnectionType := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PEmulateNetworkConditions where
  toJSON p := Data.Json.object <|
       [("offline", ToJSON.toJSON p.offline)]
    ++ [("latency", ToJSON.toJSON p.latency)]
    ++ [("downloadThroughput", ToJSON.toJSON p.downloadThroughput)]
    ++ [("uploadThroughput", ToJSON.toJSON p.uploadThroughput)]
    ++ (p.connectionType.map (fun x => ("connectionType", ToJSON.toJSON x))).toList
instance : Command Network.PEmulateNetworkConditions where
  Response := Unit
  commandName _ := "Network.emulateNetworkConditions"
  decodeResponse _ := .ok ()

/-- `Network.PEnable`. -/
structure Network.PEnable where
  maxTotalBufferSize : Option Int := none
  maxResourceBufferSize : Option Int := none
  maxPostDataSize : Option Int := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PEnable where
  toJSON p := Data.Json.object <|
       (p.maxTotalBufferSize.map (fun x => ("maxTotalBufferSize", ToJSON.toJSON x))).toList
    ++ (p.maxResourceBufferSize.map (fun x => ("maxResourceBufferSize", ToJSON.toJSON x))).toList
    ++ (p.maxPostDataSize.map (fun x => ("maxPostDataSize", ToJSON.toJSON x))).toList
instance : Command Network.PEnable where
  Response := Unit
  commandName _ := "Network.enable"
  decodeResponse _ := .ok ()

/-- `Network.GetAllCookies`. -/
structure Network.GetAllCookies where
  cookies : List Network.Cookie
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.GetAllCookies where
  parseJSON v := do
    .ok
      { cookies := ← Value.getField v "cookies" >>= FromJSON.parseJSON }

/-- `Network.PGetAllCookies`. -/
structure Network.PGetAllCookies where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PGetAllCookies where toJSON _ := .null
instance : Command Network.PGetAllCookies where
  Response := Network.GetAllCookies
  commandName _ := "Network.getAllCookies"
  decodeResponse := FromJSON.parseJSON

/-- `Network.GetCertificate`. -/
structure Network.GetCertificate where
  tableNames : List String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.GetCertificate where
  parseJSON v := do
    .ok
      { tableNames := ← Value.getField v "tableNames" >>= FromJSON.parseJSON }

/-- `Network.PGetCertificate`. -/
structure Network.PGetCertificate where
  origin : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PGetCertificate where
  toJSON p := Data.Json.object <|
       [("origin", ToJSON.toJSON p.origin)]
instance : Command Network.PGetCertificate where
  Response := Network.GetCertificate
  commandName _ := "Network.getCertificate"
  decodeResponse := FromJSON.parseJSON

/-- `Network.GetCookies`. -/
structure Network.GetCookies where
  cookies : List Network.Cookie
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.GetCookies where
  parseJSON v := do
    .ok
      { cookies := ← Value.getField v "cookies" >>= FromJSON.parseJSON }

/-- `Network.PGetCookies`. -/
structure Network.PGetCookies where
  urls : Option (List String) := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PGetCookies where
  toJSON p := Data.Json.object <|
       (p.urls.map (fun x => ("urls", ToJSON.toJSON x))).toList
instance : Command Network.PGetCookies where
  Response := Network.GetCookies
  commandName _ := "Network.getCookies"
  decodeResponse := FromJSON.parseJSON

/-- `Network.GetResponseBody`. -/
structure Network.GetResponseBody where
  body : String
  base64Encoded : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.GetResponseBody where
  parseJSON v := do
    .ok
      { body := ← Value.getField v "body" >>= FromJSON.parseJSON
        base64Encoded := ← Value.getField v "base64Encoded" >>= FromJSON.parseJSON }

/-- `Network.PGetResponseBody`. -/
structure Network.PGetResponseBody where
  requestId : Network.RequestId
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PGetResponseBody where
  toJSON p := Data.Json.object <|
       [("requestId", ToJSON.toJSON p.requestId)]
instance : Command Network.PGetResponseBody where
  Response := Network.GetResponseBody
  commandName _ := "Network.getResponseBody"
  decodeResponse := FromJSON.parseJSON

/-- `Network.GetRequestPostData`. -/
structure Network.GetRequestPostData where
  postData : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.GetRequestPostData where
  parseJSON v := do
    .ok
      { postData := ← Value.getField v "postData" >>= FromJSON.parseJSON }

/-- `Network.PGetRequestPostData`. -/
structure Network.PGetRequestPostData where
  requestId : Network.RequestId
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PGetRequestPostData where
  toJSON p := Data.Json.object <|
       [("requestId", ToJSON.toJSON p.requestId)]
instance : Command Network.PGetRequestPostData where
  Response := Network.GetRequestPostData
  commandName _ := "Network.getRequestPostData"
  decodeResponse := FromJSON.parseJSON

/-- `Network.GetResponseBodyForInterception`. -/
structure Network.GetResponseBodyForInterception where
  body : String
  base64Encoded : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.GetResponseBodyForInterception where
  parseJSON v := do
    .ok
      { body := ← Value.getField v "body" >>= FromJSON.parseJSON
        base64Encoded := ← Value.getField v "base64Encoded" >>= FromJSON.parseJSON }

/-- `Network.PGetResponseBodyForInterception`. -/
structure Network.PGetResponseBodyForInterception where
  interceptionId : Network.InterceptionId
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PGetResponseBodyForInterception where
  toJSON p := Data.Json.object <|
       [("interceptionId", ToJSON.toJSON p.interceptionId)]
instance : Command Network.PGetResponseBodyForInterception where
  Response := Network.GetResponseBodyForInterception
  commandName _ := "Network.getResponseBodyForInterception"
  decodeResponse := FromJSON.parseJSON

/-- `Network.TakeResponseBodyForInterceptionAsStream`. -/
structure Network.TakeResponseBodyForInterceptionAsStream where
  stream : CDP.Domains.IO.StreamHandle
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.TakeResponseBodyForInterceptionAsStream where
  parseJSON v := do
    .ok
      { stream := ← Value.getField v "stream" >>= FromJSON.parseJSON }

/-- `Network.PTakeResponseBodyForInterceptionAsStream`. -/
structure Network.PTakeResponseBodyForInterceptionAsStream where
  interceptionId : Network.InterceptionId
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PTakeResponseBodyForInterceptionAsStream where
  toJSON p := Data.Json.object <|
       [("interceptionId", ToJSON.toJSON p.interceptionId)]
instance : Command Network.PTakeResponseBodyForInterceptionAsStream where
  Response := Network.TakeResponseBodyForInterceptionAsStream
  commandName _ := "Network.takeResponseBodyForInterceptionAsStream"
  decodeResponse := FromJSON.parseJSON

/-- `Network.PReplayXHR`. -/
structure Network.PReplayXHR where
  requestId : Network.RequestId
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PReplayXHR where
  toJSON p := Data.Json.object <|
       [("requestId", ToJSON.toJSON p.requestId)]
instance : Command Network.PReplayXHR where
  Response := Unit
  commandName _ := "Network.replayXHR"
  decodeResponse _ := .ok ()

/-- `Network.SearchInResponseBody`. -/
structure Network.SearchInResponseBody where
  result : List Debugger.SearchMatch
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.SearchInResponseBody where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON }

/-- `Network.PSearchInResponseBody`. -/
structure Network.PSearchInResponseBody where
  requestId : Network.RequestId
  query : String
  caseSensitive : Option Bool := none
  isRegex : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSearchInResponseBody where
  toJSON p := Data.Json.object <|
       [("requestId", ToJSON.toJSON p.requestId)]
    ++ [("query", ToJSON.toJSON p.query)]
    ++ (p.caseSensitive.map (fun x => ("caseSensitive", ToJSON.toJSON x))).toList
    ++ (p.isRegex.map (fun x => ("isRegex", ToJSON.toJSON x))).toList
instance : Command Network.PSearchInResponseBody where
  Response := Network.SearchInResponseBody
  commandName _ := "Network.searchInResponseBody"
  decodeResponse := FromJSON.parseJSON

/-- `Network.PSetBlockedURLs`. -/
structure Network.PSetBlockedURLs where
  urls : List String
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetBlockedURLs where
  toJSON p := Data.Json.object <|
       [("urls", ToJSON.toJSON p.urls)]
instance : Command Network.PSetBlockedURLs where
  Response := Unit
  commandName _ := "Network.setBlockedURLs"
  decodeResponse _ := .ok ()

/-- `Network.PSetBypassServiceWorker`. -/
structure Network.PSetBypassServiceWorker where
  bypass : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetBypassServiceWorker where
  toJSON p := Data.Json.object <|
       [("bypass", ToJSON.toJSON p.bypass)]
instance : Command Network.PSetBypassServiceWorker where
  Response := Unit
  commandName _ := "Network.setBypassServiceWorker"
  decodeResponse _ := .ok ()

/-- `Network.PSetCacheDisabled`. -/
structure Network.PSetCacheDisabled where
  cacheDisabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetCacheDisabled where
  toJSON p := Data.Json.object <|
       [("cacheDisabled", ToJSON.toJSON p.cacheDisabled)]
instance : Command Network.PSetCacheDisabled where
  Response := Unit
  commandName _ := "Network.setCacheDisabled"
  decodeResponse _ := .ok ()

/-- `Network.PSetCookie`. -/
structure Network.PSetCookie where
  name : String
  value : String
  url : Option String := none
  domain : Option String := none
  path : Option String := none
  secure : Option Bool := none
  httpOnly : Option Bool := none
  sameSite : Option Network.CookieSameSite := none
  expires : Option Network.TimeSinceEpoch := none
  priority : Option Network.CookiePriority := none
  sameParty : Option Bool := none
  sourceScheme : Option Network.CookieSourceScheme := none
  sourcePort : Option Int := none
  partitionKey : Option String := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetCookie where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ [("value", ToJSON.toJSON p.value)]
    ++ (p.url.map (fun x => ("url", ToJSON.toJSON x))).toList
    ++ (p.domain.map (fun x => ("domain", ToJSON.toJSON x))).toList
    ++ (p.path.map (fun x => ("path", ToJSON.toJSON x))).toList
    ++ (p.secure.map (fun x => ("secure", ToJSON.toJSON x))).toList
    ++ (p.httpOnly.map (fun x => ("httpOnly", ToJSON.toJSON x))).toList
    ++ (p.sameSite.map (fun x => ("sameSite", ToJSON.toJSON x))).toList
    ++ (p.expires.map (fun x => ("expires", ToJSON.toJSON x))).toList
    ++ (p.priority.map (fun x => ("priority", ToJSON.toJSON x))).toList
    ++ (p.sameParty.map (fun x => ("sameParty", ToJSON.toJSON x))).toList
    ++ (p.sourceScheme.map (fun x => ("sourceScheme", ToJSON.toJSON x))).toList
    ++ (p.sourcePort.map (fun x => ("sourcePort", ToJSON.toJSON x))).toList
    ++ (p.partitionKey.map (fun x => ("partitionKey", ToJSON.toJSON x))).toList
instance : Command Network.PSetCookie where
  Response := Unit
  commandName _ := "Network.setCookie"
  decodeResponse _ := .ok ()

/-- `Network.PSetCookies`. -/
structure Network.PSetCookies where
  cookies : List Network.CookieParam
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetCookies where
  toJSON p := Data.Json.object <|
       [("cookies", ToJSON.toJSON p.cookies)]
instance : Command Network.PSetCookies where
  Response := Unit
  commandName _ := "Network.setCookies"
  decodeResponse _ := .ok ()

/-- `Network.PSetExtraHTTPHeaders`. -/
structure Network.PSetExtraHTTPHeaders where
  headers : Network.Headers
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetExtraHTTPHeaders where
  toJSON p := Data.Json.object <|
       [("headers", ToJSON.toJSON p.headers)]
instance : Command Network.PSetExtraHTTPHeaders where
  Response := Unit
  commandName _ := "Network.setExtraHTTPHeaders"
  decodeResponse _ := .ok ()

/-- `Network.PSetAttachDebugStack`. -/
structure Network.PSetAttachDebugStack where
  enabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetAttachDebugStack where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
instance : Command Network.PSetAttachDebugStack where
  Response := Unit
  commandName _ := "Network.setAttachDebugStack"
  decodeResponse _ := .ok ()

/-- `Network.PSetUserAgentOverride`. -/
structure Network.PSetUserAgentOverride where
  userAgent : String
  acceptLanguage : Option String := none
  platform : Option String := none
  userAgentMetadata : Option Emulation.UserAgentMetadata := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PSetUserAgentOverride where
  toJSON p := Data.Json.object <|
       [("userAgent", ToJSON.toJSON p.userAgent)]
    ++ (p.acceptLanguage.map (fun x => ("acceptLanguage", ToJSON.toJSON x))).toList
    ++ (p.platform.map (fun x => ("platform", ToJSON.toJSON x))).toList
    ++ (p.userAgentMetadata.map (fun x => ("userAgentMetadata", ToJSON.toJSON x))).toList
instance : Command Network.PSetUserAgentOverride where
  Response := Unit
  commandName _ := "Network.setUserAgentOverride"
  decodeResponse _ := .ok ()

/-- `Network.GetSecurityIsolationStatus`. -/
structure Network.GetSecurityIsolationStatus where
  status : Network.SecurityIsolationStatus
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.GetSecurityIsolationStatus where
  parseJSON v := do
    .ok
      { status := ← Value.getField v "status" >>= FromJSON.parseJSON }

/-- `Network.PGetSecurityIsolationStatus`. -/
structure Network.PGetSecurityIsolationStatus where
  frameId : Option Page.FrameId := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PGetSecurityIsolationStatus where
  toJSON p := Data.Json.object <|
       (p.frameId.map (fun x => ("frameId", ToJSON.toJSON x))).toList
instance : Command Network.PGetSecurityIsolationStatus where
  Response := Network.GetSecurityIsolationStatus
  commandName _ := "Network.getSecurityIsolationStatus"
  decodeResponse := FromJSON.parseJSON

/-- `Network.PEnableReportingApi`. -/
structure Network.PEnableReportingApi where
  enable : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PEnableReportingApi where
  toJSON p := Data.Json.object <|
       [("enable", ToJSON.toJSON p.enable)]
instance : Command Network.PEnableReportingApi where
  Response := Unit
  commandName _ := "Network.enableReportingApi"
  decodeResponse _ := .ok ()

/-- `Network.LoadNetworkResource`. -/
structure Network.LoadNetworkResource where
  resource : Network.LoadNetworkResourcePageResult
  deriving Repr, BEq, DecidableEq
instance : FromJSON Network.LoadNetworkResource where
  parseJSON v := do
    .ok
      { resource := ← Value.getField v "resource" >>= FromJSON.parseJSON }

/-- `Network.PLoadNetworkResource`. -/
structure Network.PLoadNetworkResource where
  frameId : Option Page.FrameId := none
  url : String
  options : Network.LoadNetworkResourceOptions
  deriving Repr, BEq, DecidableEq
instance : ToJSON Network.PLoadNetworkResource where
  toJSON p := Data.Json.object <|
       (p.frameId.map (fun x => ("frameId", ToJSON.toJSON x))).toList
    ++ [("url", ToJSON.toJSON p.url)]
    ++ [("options", ToJSON.toJSON p.options)]
instance : Command Network.PLoadNetworkResource where
  Response := Network.LoadNetworkResource
  commandName _ := "Network.loadNetworkResource"
  decodeResponse := FromJSON.parseJSON

/-- `Page.AdFrameType`. -/
inductive Page.AdFrameType where
  | none | child | root
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.AdFrameType where
  parseJSON
    | .string "none" => .ok .none
    | .string "child" => .ok .child
    | .string "root" => .ok .root
    | v => .error s!"failed to parse Page.AdFrameType: {repr v}"
instance : ToJSON Page.AdFrameType where
  toJSON
    | .none => .string "none"
    | .child => .string "child"
    | .root => .string "root"

/-- `Page.AdFrameExplanation`. -/
inductive Page.AdFrameExplanation where
  | parentIsAd | createdByAdScript | matchedBlockingRule
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.AdFrameExplanation where
  parseJSON
    | .string "ParentIsAd" => .ok .parentIsAd
    | .string "CreatedByAdScript" => .ok .createdByAdScript
    | .string "MatchedBlockingRule" => .ok .matchedBlockingRule
    | v => .error s!"failed to parse Page.AdFrameExplanation: {repr v}"
instance : ToJSON Page.AdFrameExplanation where
  toJSON
    | .parentIsAd => .string "ParentIsAd"
    | .createdByAdScript => .string "CreatedByAdScript"
    | .matchedBlockingRule => .string "MatchedBlockingRule"

/-- `Page.AdFrameStatus`. -/
structure Page.AdFrameStatus where
  adFrameType : Page.AdFrameType
  explanations : Option (List Page.AdFrameExplanation) := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.AdFrameStatus where
  parseJSON v := do
    .ok
      { adFrameType := ← Value.getField v "adFrameType" >>= FromJSON.parseJSON
        explanations := ← (← Value.getFieldOpt v "explanations").mapM FromJSON.parseJSON }
instance : ToJSON Page.AdFrameStatus where
  toJSON p := Data.Json.object <|
       [("adFrameType", ToJSON.toJSON p.adFrameType)]
    ++ (p.explanations.map (fun x => ("explanations", ToJSON.toJSON x))).toList

/-- `Page.AdScriptId`. -/
structure Page.AdScriptId where
  scriptId : Runtime.ScriptId
  debuggerId : Runtime.UniqueDebuggerId
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.AdScriptId where
  parseJSON v := do
    .ok
      { scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        debuggerId := ← Value.getField v "debuggerId" >>= FromJSON.parseJSON }
instance : ToJSON Page.AdScriptId where
  toJSON p := Data.Json.object <|
       [("scriptId", ToJSON.toJSON p.scriptId)]
    ++ [("debuggerId", ToJSON.toJSON p.debuggerId)]

/-- `Page.SecureContextType`. -/
inductive Page.SecureContextType where
  | secure | secureLocalhost | insecureScheme | insecureAncestor
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.SecureContextType where
  parseJSON
    | .string "Secure" => .ok .secure
    | .string "SecureLocalhost" => .ok .secureLocalhost
    | .string "InsecureScheme" => .ok .insecureScheme
    | .string "InsecureAncestor" => .ok .insecureAncestor
    | v => .error s!"failed to parse Page.SecureContextType: {repr v}"
instance : ToJSON Page.SecureContextType where
  toJSON
    | .secure => .string "Secure"
    | .secureLocalhost => .string "SecureLocalhost"
    | .insecureScheme => .string "InsecureScheme"
    | .insecureAncestor => .string "InsecureAncestor"

/-- `Page.CrossOriginIsolatedContextType`. -/
inductive Page.CrossOriginIsolatedContextType where
  | isolated | notIsolated | notIsolatedFeatureDisabled
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.CrossOriginIsolatedContextType where
  parseJSON
    | .string "Isolated" => .ok .isolated
    | .string "NotIsolated" => .ok .notIsolated
    | .string "NotIsolatedFeatureDisabled" => .ok .notIsolatedFeatureDisabled
    | v => .error s!"failed to parse Page.CrossOriginIsolatedContextType: {repr v}"
instance : ToJSON Page.CrossOriginIsolatedContextType where
  toJSON
    | .isolated => .string "Isolated"
    | .notIsolated => .string "NotIsolated"
    | .notIsolatedFeatureDisabled => .string "NotIsolatedFeatureDisabled"

/-- `Page.GatedAPIFeatures`. -/
inductive Page.GatedAPIFeatures where
  | sharedArrayBuffers | sharedArrayBuffersTransferAllowed | performanceMeasureMemory | performanceProfile
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GatedAPIFeatures where
  parseJSON
    | .string "SharedArrayBuffers" => .ok .sharedArrayBuffers
    | .string "SharedArrayBuffersTransferAllowed" => .ok .sharedArrayBuffersTransferAllowed
    | .string "PerformanceMeasureMemory" => .ok .performanceMeasureMemory
    | .string "PerformanceProfile" => .ok .performanceProfile
    | v => .error s!"failed to parse Page.GatedAPIFeatures: {repr v}"
instance : ToJSON Page.GatedAPIFeatures where
  toJSON
    | .sharedArrayBuffers => .string "SharedArrayBuffers"
    | .sharedArrayBuffersTransferAllowed => .string "SharedArrayBuffersTransferAllowed"
    | .performanceMeasureMemory => .string "PerformanceMeasureMemory"
    | .performanceProfile => .string "PerformanceProfile"

/-- `Page.PermissionsPolicyFeature`. -/
inductive Page.PermissionsPolicyFeature where
  | accelerometer | ambientLightSensor | attributionReporting | autoplay | bluetooth | browsingTopics | camera | chDpr | chDeviceMemory | chDownlink | chEct | chPrefersColorScheme | chPrefersReducedMotion | chRtt | chSaveData | chUa | chUaArch | chUaBitness | chUaPlatform | chUaModel | chUaMobile | chUaFull | chUaFullVersion | chUaFullVersionList | chUaPlatformVersion | chUaReduced | chUaWow64 | chViewportHeight | chViewportWidth | chWidth | clipboardRead | clipboardWrite | crossOriginIsolated | directSockets | displayCapture | documentDomain | encryptedMedia | executionWhileOutOfViewport | executionWhileNotRendered | focusWithoutUserActivation | fullscreen | frobulate | gamepad | geolocation | gyroscope | hid | identityCredentialsGet | idleDetection | interestCohort | joinAdInterestGroup | keyboardMap | localFonts | magnetometer | microphone | midi | otpCredentials | payment | pictureInPicture | publickeyCredentialsGet | runAdAuction | screenWakeLock | serial | sharedAutofill | sharedStorage | storageAccess | syncXhr | trustTokenRedemption | unload | usb | verticalScroll | webShare | windowPlacement | xrSpatialTracking
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PermissionsPolicyFeature where
  parseJSON
    | .string "accelerometer" => .ok .accelerometer
    | .string "ambient-light-sensor" => .ok .ambientLightSensor
    | .string "attribution-reporting" => .ok .attributionReporting
    | .string "autoplay" => .ok .autoplay
    | .string "bluetooth" => .ok .bluetooth
    | .string "browsing-topics" => .ok .browsingTopics
    | .string "camera" => .ok .camera
    | .string "ch-dpr" => .ok .chDpr
    | .string "ch-device-memory" => .ok .chDeviceMemory
    | .string "ch-downlink" => .ok .chDownlink
    | .string "ch-ect" => .ok .chEct
    | .string "ch-prefers-color-scheme" => .ok .chPrefersColorScheme
    | .string "ch-prefers-reduced-motion" => .ok .chPrefersReducedMotion
    | .string "ch-rtt" => .ok .chRtt
    | .string "ch-save-data" => .ok .chSaveData
    | .string "ch-ua" => .ok .chUa
    | .string "ch-ua-arch" => .ok .chUaArch
    | .string "ch-ua-bitness" => .ok .chUaBitness
    | .string "ch-ua-platform" => .ok .chUaPlatform
    | .string "ch-ua-model" => .ok .chUaModel
    | .string "ch-ua-mobile" => .ok .chUaMobile
    | .string "ch-ua-full" => .ok .chUaFull
    | .string "ch-ua-full-version" => .ok .chUaFullVersion
    | .string "ch-ua-full-version-list" => .ok .chUaFullVersionList
    | .string "ch-ua-platform-version" => .ok .chUaPlatformVersion
    | .string "ch-ua-reduced" => .ok .chUaReduced
    | .string "ch-ua-wow64" => .ok .chUaWow64
    | .string "ch-viewport-height" => .ok .chViewportHeight
    | .string "ch-viewport-width" => .ok .chViewportWidth
    | .string "ch-width" => .ok .chWidth
    | .string "clipboard-read" => .ok .clipboardRead
    | .string "clipboard-write" => .ok .clipboardWrite
    | .string "cross-origin-isolated" => .ok .crossOriginIsolated
    | .string "direct-sockets" => .ok .directSockets
    | .string "display-capture" => .ok .displayCapture
    | .string "document-domain" => .ok .documentDomain
    | .string "encrypted-media" => .ok .encryptedMedia
    | .string "execution-while-out-of-viewport" => .ok .executionWhileOutOfViewport
    | .string "execution-while-not-rendered" => .ok .executionWhileNotRendered
    | .string "focus-without-user-activation" => .ok .focusWithoutUserActivation
    | .string "fullscreen" => .ok .fullscreen
    | .string "frobulate" => .ok .frobulate
    | .string "gamepad" => .ok .gamepad
    | .string "geolocation" => .ok .geolocation
    | .string "gyroscope" => .ok .gyroscope
    | .string "hid" => .ok .hid
    | .string "identity-credentials-get" => .ok .identityCredentialsGet
    | .string "idle-detection" => .ok .idleDetection
    | .string "interest-cohort" => .ok .interestCohort
    | .string "join-ad-interest-group" => .ok .joinAdInterestGroup
    | .string "keyboard-map" => .ok .keyboardMap
    | .string "local-fonts" => .ok .localFonts
    | .string "magnetometer" => .ok .magnetometer
    | .string "microphone" => .ok .microphone
    | .string "midi" => .ok .midi
    | .string "otp-credentials" => .ok .otpCredentials
    | .string "payment" => .ok .payment
    | .string "picture-in-picture" => .ok .pictureInPicture
    | .string "publickey-credentials-get" => .ok .publickeyCredentialsGet
    | .string "run-ad-auction" => .ok .runAdAuction
    | .string "screen-wake-lock" => .ok .screenWakeLock
    | .string "serial" => .ok .serial
    | .string "shared-autofill" => .ok .sharedAutofill
    | .string "shared-storage" => .ok .sharedStorage
    | .string "storage-access" => .ok .storageAccess
    | .string "sync-xhr" => .ok .syncXhr
    | .string "trust-token-redemption" => .ok .trustTokenRedemption
    | .string "unload" => .ok .unload
    | .string "usb" => .ok .usb
    | .string "vertical-scroll" => .ok .verticalScroll
    | .string "web-share" => .ok .webShare
    | .string "window-placement" => .ok .windowPlacement
    | .string "xr-spatial-tracking" => .ok .xrSpatialTracking
    | v => .error s!"failed to parse Page.PermissionsPolicyFeature: {repr v}"
instance : ToJSON Page.PermissionsPolicyFeature where
  toJSON
    | .accelerometer => .string "accelerometer"
    | .ambientLightSensor => .string "ambient-light-sensor"
    | .attributionReporting => .string "attribution-reporting"
    | .autoplay => .string "autoplay"
    | .bluetooth => .string "bluetooth"
    | .browsingTopics => .string "browsing-topics"
    | .camera => .string "camera"
    | .chDpr => .string "ch-dpr"
    | .chDeviceMemory => .string "ch-device-memory"
    | .chDownlink => .string "ch-downlink"
    | .chEct => .string "ch-ect"
    | .chPrefersColorScheme => .string "ch-prefers-color-scheme"
    | .chPrefersReducedMotion => .string "ch-prefers-reduced-motion"
    | .chRtt => .string "ch-rtt"
    | .chSaveData => .string "ch-save-data"
    | .chUa => .string "ch-ua"
    | .chUaArch => .string "ch-ua-arch"
    | .chUaBitness => .string "ch-ua-bitness"
    | .chUaPlatform => .string "ch-ua-platform"
    | .chUaModel => .string "ch-ua-model"
    | .chUaMobile => .string "ch-ua-mobile"
    | .chUaFull => .string "ch-ua-full"
    | .chUaFullVersion => .string "ch-ua-full-version"
    | .chUaFullVersionList => .string "ch-ua-full-version-list"
    | .chUaPlatformVersion => .string "ch-ua-platform-version"
    | .chUaReduced => .string "ch-ua-reduced"
    | .chUaWow64 => .string "ch-ua-wow64"
    | .chViewportHeight => .string "ch-viewport-height"
    | .chViewportWidth => .string "ch-viewport-width"
    | .chWidth => .string "ch-width"
    | .clipboardRead => .string "clipboard-read"
    | .clipboardWrite => .string "clipboard-write"
    | .crossOriginIsolated => .string "cross-origin-isolated"
    | .directSockets => .string "direct-sockets"
    | .displayCapture => .string "display-capture"
    | .documentDomain => .string "document-domain"
    | .encryptedMedia => .string "encrypted-media"
    | .executionWhileOutOfViewport => .string "execution-while-out-of-viewport"
    | .executionWhileNotRendered => .string "execution-while-not-rendered"
    | .focusWithoutUserActivation => .string "focus-without-user-activation"
    | .fullscreen => .string "fullscreen"
    | .frobulate => .string "frobulate"
    | .gamepad => .string "gamepad"
    | .geolocation => .string "geolocation"
    | .gyroscope => .string "gyroscope"
    | .hid => .string "hid"
    | .identityCredentialsGet => .string "identity-credentials-get"
    | .idleDetection => .string "idle-detection"
    | .interestCohort => .string "interest-cohort"
    | .joinAdInterestGroup => .string "join-ad-interest-group"
    | .keyboardMap => .string "keyboard-map"
    | .localFonts => .string "local-fonts"
    | .magnetometer => .string "magnetometer"
    | .microphone => .string "microphone"
    | .midi => .string "midi"
    | .otpCredentials => .string "otp-credentials"
    | .payment => .string "payment"
    | .pictureInPicture => .string "picture-in-picture"
    | .publickeyCredentialsGet => .string "publickey-credentials-get"
    | .runAdAuction => .string "run-ad-auction"
    | .screenWakeLock => .string "screen-wake-lock"
    | .serial => .string "serial"
    | .sharedAutofill => .string "shared-autofill"
    | .sharedStorage => .string "shared-storage"
    | .storageAccess => .string "storage-access"
    | .syncXhr => .string "sync-xhr"
    | .trustTokenRedemption => .string "trust-token-redemption"
    | .unload => .string "unload"
    | .usb => .string "usb"
    | .verticalScroll => .string "vertical-scroll"
    | .webShare => .string "web-share"
    | .windowPlacement => .string "window-placement"
    | .xrSpatialTracking => .string "xr-spatial-tracking"

/-- `Page.PermissionsPolicyBlockReason`. -/
inductive Page.PermissionsPolicyBlockReason where
  | header | iframeAttribute | inFencedFrameTree | inIsolatedApp
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PermissionsPolicyBlockReason where
  parseJSON
    | .string "Header" => .ok .header
    | .string "IframeAttribute" => .ok .iframeAttribute
    | .string "InFencedFrameTree" => .ok .inFencedFrameTree
    | .string "InIsolatedApp" => .ok .inIsolatedApp
    | v => .error s!"failed to parse Page.PermissionsPolicyBlockReason: {repr v}"
instance : ToJSON Page.PermissionsPolicyBlockReason where
  toJSON
    | .header => .string "Header"
    | .iframeAttribute => .string "IframeAttribute"
    | .inFencedFrameTree => .string "InFencedFrameTree"
    | .inIsolatedApp => .string "InIsolatedApp"

/-- `Page.PermissionsPolicyBlockLocator`. -/
structure Page.PermissionsPolicyBlockLocator where
  frameId : Page.FrameId
  blockReason : Page.PermissionsPolicyBlockReason
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PermissionsPolicyBlockLocator where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        blockReason := ← Value.getField v "blockReason" >>= FromJSON.parseJSON }
instance : ToJSON Page.PermissionsPolicyBlockLocator where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
    ++ [("blockReason", ToJSON.toJSON p.blockReason)]

/-- `Page.PermissionsPolicyFeatureState`. -/
structure Page.PermissionsPolicyFeatureState where
  feature : Page.PermissionsPolicyFeature
  allowed : Bool
  locator : Option Page.PermissionsPolicyBlockLocator := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PermissionsPolicyFeatureState where
  parseJSON v := do
    .ok
      { feature := ← Value.getField v "feature" >>= FromJSON.parseJSON
        allowed := ← Value.getField v "allowed" >>= FromJSON.parseJSON
        locator := ← (← Value.getFieldOpt v "locator").mapM FromJSON.parseJSON }
instance : ToJSON Page.PermissionsPolicyFeatureState where
  toJSON p := Data.Json.object <|
       [("feature", ToJSON.toJSON p.feature)]
    ++ [("allowed", ToJSON.toJSON p.allowed)]
    ++ (p.locator.map (fun x => ("locator", ToJSON.toJSON x))).toList

/-- `Page.OriginTrialTokenStatus`. -/
inductive Page.OriginTrialTokenStatus where
  | success | notSupported | insecure | expired | wrongOrigin | invalidSignature | malformed | wrongVersion | featureDisabled | tokenDisabled | featureDisabledForUser | unknownTrial
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.OriginTrialTokenStatus where
  parseJSON
    | .string "Success" => .ok .success
    | .string "NotSupported" => .ok .notSupported
    | .string "Insecure" => .ok .insecure
    | .string "Expired" => .ok .expired
    | .string "WrongOrigin" => .ok .wrongOrigin
    | .string "InvalidSignature" => .ok .invalidSignature
    | .string "Malformed" => .ok .malformed
    | .string "WrongVersion" => .ok .wrongVersion
    | .string "FeatureDisabled" => .ok .featureDisabled
    | .string "TokenDisabled" => .ok .tokenDisabled
    | .string "FeatureDisabledForUser" => .ok .featureDisabledForUser
    | .string "UnknownTrial" => .ok .unknownTrial
    | v => .error s!"failed to parse Page.OriginTrialTokenStatus: {repr v}"
instance : ToJSON Page.OriginTrialTokenStatus where
  toJSON
    | .success => .string "Success"
    | .notSupported => .string "NotSupported"
    | .insecure => .string "Insecure"
    | .expired => .string "Expired"
    | .wrongOrigin => .string "WrongOrigin"
    | .invalidSignature => .string "InvalidSignature"
    | .malformed => .string "Malformed"
    | .wrongVersion => .string "WrongVersion"
    | .featureDisabled => .string "FeatureDisabled"
    | .tokenDisabled => .string "TokenDisabled"
    | .featureDisabledForUser => .string "FeatureDisabledForUser"
    | .unknownTrial => .string "UnknownTrial"

/-- `Page.OriginTrialStatus`. -/
inductive Page.OriginTrialStatus where
  | enabled | validTokenNotProvided | oSNotSupported | trialNotAllowed
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.OriginTrialStatus where
  parseJSON
    | .string "Enabled" => .ok .enabled
    | .string "ValidTokenNotProvided" => .ok .validTokenNotProvided
    | .string "OSNotSupported" => .ok .oSNotSupported
    | .string "TrialNotAllowed" => .ok .trialNotAllowed
    | v => .error s!"failed to parse Page.OriginTrialStatus: {repr v}"
instance : ToJSON Page.OriginTrialStatus where
  toJSON
    | .enabled => .string "Enabled"
    | .validTokenNotProvided => .string "ValidTokenNotProvided"
    | .oSNotSupported => .string "OSNotSupported"
    | .trialNotAllowed => .string "TrialNotAllowed"

/-- `Page.OriginTrialUsageRestriction`. -/
inductive Page.OriginTrialUsageRestriction where
  | none | subset
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.OriginTrialUsageRestriction where
  parseJSON
    | .string "None" => .ok .none
    | .string "Subset" => .ok .subset
    | v => .error s!"failed to parse Page.OriginTrialUsageRestriction: {repr v}"
instance : ToJSON Page.OriginTrialUsageRestriction where
  toJSON
    | .none => .string "None"
    | .subset => .string "Subset"

/-- `Page.OriginTrialToken`. -/
structure Page.OriginTrialToken where
  origin : String
  matchSubDomains : Bool
  trialName : String
  expiryTime : Network.TimeSinceEpoch
  isThirdParty : Bool
  usageRestriction : Page.OriginTrialUsageRestriction
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.OriginTrialToken where
  parseJSON v := do
    .ok
      { origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        matchSubDomains := ← Value.getField v "matchSubDomains" >>= FromJSON.parseJSON
        trialName := ← Value.getField v "trialName" >>= FromJSON.parseJSON
        expiryTime := ← Value.getField v "expiryTime" >>= FromJSON.parseJSON
        isThirdParty := ← Value.getField v "isThirdParty" >>= FromJSON.parseJSON
        usageRestriction := ← Value.getField v "usageRestriction" >>= FromJSON.parseJSON }
instance : ToJSON Page.OriginTrialToken where
  toJSON p := Data.Json.object <|
       [("origin", ToJSON.toJSON p.origin)]
    ++ [("matchSubDomains", ToJSON.toJSON p.matchSubDomains)]
    ++ [("trialName", ToJSON.toJSON p.trialName)]
    ++ [("expiryTime", ToJSON.toJSON p.expiryTime)]
    ++ [("isThirdParty", ToJSON.toJSON p.isThirdParty)]
    ++ [("usageRestriction", ToJSON.toJSON p.usageRestriction)]

/-- `Page.OriginTrialTokenWithStatus`. -/
structure Page.OriginTrialTokenWithStatus where
  rawTokenText : String
  parsedToken : Option Page.OriginTrialToken := none
  status : Page.OriginTrialTokenStatus
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.OriginTrialTokenWithStatus where
  parseJSON v := do
    .ok
      { rawTokenText := ← Value.getField v "rawTokenText" >>= FromJSON.parseJSON
        parsedToken := ← (← Value.getFieldOpt v "parsedToken").mapM FromJSON.parseJSON
        status := ← Value.getField v "status" >>= FromJSON.parseJSON }
instance : ToJSON Page.OriginTrialTokenWithStatus where
  toJSON p := Data.Json.object <|
       [("rawTokenText", ToJSON.toJSON p.rawTokenText)]
    ++ (p.parsedToken.map (fun x => ("parsedToken", ToJSON.toJSON x))).toList
    ++ [("status", ToJSON.toJSON p.status)]

/-- `Page.OriginTrial`. -/
structure Page.OriginTrial where
  trialName : String
  status : Page.OriginTrialStatus
  tokensWithStatus : List Page.OriginTrialTokenWithStatus
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.OriginTrial where
  parseJSON v := do
    .ok
      { trialName := ← Value.getField v "trialName" >>= FromJSON.parseJSON
        status := ← Value.getField v "status" >>= FromJSON.parseJSON
        tokensWithStatus := ← Value.getField v "tokensWithStatus" >>= FromJSON.parseJSON }
instance : ToJSON Page.OriginTrial where
  toJSON p := Data.Json.object <|
       [("trialName", ToJSON.toJSON p.trialName)]
    ++ [("status", ToJSON.toJSON p.status)]
    ++ [("tokensWithStatus", ToJSON.toJSON p.tokensWithStatus)]

/-- `Page.Frame`. -/
structure Page.Frame where
  id : Page.FrameId
  parentId : Option Page.FrameId := none
  loaderId : Network.LoaderId
  name : Option String := none
  url : String
  urlFragment : Option String := none
  domainAndRegistry : String
  securityOrigin : String
  mimeType : String
  unreachableUrl : Option String := none
  adFrameStatus : Option Page.AdFrameStatus := none
  secureContextType : Page.SecureContextType
  crossOriginIsolatedContextType : Page.CrossOriginIsolatedContextType
  gatedAPIFeatures : List Page.GatedAPIFeatures
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.Frame where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        parentId := ← (← Value.getFieldOpt v "parentId").mapM FromJSON.parseJSON
        loaderId := ← Value.getField v "loaderId" >>= FromJSON.parseJSON
        name := ← (← Value.getFieldOpt v "name").mapM FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        urlFragment := ← (← Value.getFieldOpt v "urlFragment").mapM FromJSON.parseJSON
        domainAndRegistry := ← Value.getField v "domainAndRegistry" >>= FromJSON.parseJSON
        securityOrigin := ← Value.getField v "securityOrigin" >>= FromJSON.parseJSON
        mimeType := ← Value.getField v "mimeType" >>= FromJSON.parseJSON
        unreachableUrl := ← (← Value.getFieldOpt v "unreachableUrl").mapM FromJSON.parseJSON
        adFrameStatus := ← (← Value.getFieldOpt v "adFrameStatus").mapM FromJSON.parseJSON
        secureContextType := ← Value.getField v "secureContextType" >>= FromJSON.parseJSON
        crossOriginIsolatedContextType := ← Value.getField v "crossOriginIsolatedContextType" >>= FromJSON.parseJSON
        gatedAPIFeatures := ← Value.getField v "gatedAPIFeatures" >>= FromJSON.parseJSON }
instance : ToJSON Page.Frame where
  toJSON p := Data.Json.object <|
       [("id", ToJSON.toJSON p.id)]
    ++ (p.parentId.map (fun x => ("parentId", ToJSON.toJSON x))).toList
    ++ [("loaderId", ToJSON.toJSON p.loaderId)]
    ++ (p.name.map (fun x => ("name", ToJSON.toJSON x))).toList
    ++ [("url", ToJSON.toJSON p.url)]
    ++ (p.urlFragment.map (fun x => ("urlFragment", ToJSON.toJSON x))).toList
    ++ [("domainAndRegistry", ToJSON.toJSON p.domainAndRegistry)]
    ++ [("securityOrigin", ToJSON.toJSON p.securityOrigin)]
    ++ [("mimeType", ToJSON.toJSON p.mimeType)]
    ++ (p.unreachableUrl.map (fun x => ("unreachableUrl", ToJSON.toJSON x))).toList
    ++ (p.adFrameStatus.map (fun x => ("adFrameStatus", ToJSON.toJSON x))).toList
    ++ [("secureContextType", ToJSON.toJSON p.secureContextType)]
    ++ [("crossOriginIsolatedContextType", ToJSON.toJSON p.crossOriginIsolatedContextType)]
    ++ [("gatedAPIFeatures", ToJSON.toJSON p.gatedAPIFeatures)]

/-- `Page.FrameResource`. -/
structure Page.FrameResource where
  url : String
  type : Network.ResourceType
  mimeType : String
  lastModified : Option Network.TimeSinceEpoch := none
  contentSize : Option Float := none
  failed : Option Bool := none
  canceled : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FrameResource where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        mimeType := ← Value.getField v "mimeType" >>= FromJSON.parseJSON
        lastModified := ← (← Value.getFieldOpt v "lastModified").mapM FromJSON.parseJSON
        contentSize := ← (← Value.getFieldOpt v "contentSize").mapM FromJSON.parseJSON
        failed := ← (← Value.getFieldOpt v "failed").mapM FromJSON.parseJSON
        canceled := ← (← Value.getFieldOpt v "canceled").mapM FromJSON.parseJSON }
instance : ToJSON Page.FrameResource where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ [("type", ToJSON.toJSON p.type)]
    ++ [("mimeType", ToJSON.toJSON p.mimeType)]
    ++ (p.lastModified.map (fun x => ("lastModified", ToJSON.toJSON x))).toList
    ++ (p.contentSize.map (fun x => ("contentSize", ToJSON.toJSON x))).toList
    ++ (p.failed.map (fun x => ("failed", ToJSON.toJSON x))).toList
    ++ (p.canceled.map (fun x => ("canceled", ToJSON.toJSON x))).toList

/-- `Page.FrameResourceTree`. Self-referential; `FromJSON`/`ToJSON` are hand-written
    mutually-recursive `def`s with `sizeOf` termination proofs (see the
    module header and `CDP.Domains.HeapProfiler`). -/
structure Page.FrameResourceTree where
  frame : Page.Frame
  childFrames : Option (List Page.FrameResourceTree) := none
  resources : List Page.FrameResource
  deriving Repr, BEq
/-- Assemble a `Page.FrameResourceTree` from its already-decoded recursive fields. -/
def Page.finishFrameResourceTree (v : Value) (childFrames : Option (List Page.FrameResourceTree)) : Except String Page.FrameResourceTree := do
  .ok
    { frame := ← Value.getField v "frame" >>= FromJSON.parseJSON
      childFrames
      resources := ← Value.getField v "resources" >>= FromJSON.parseJSON }
mutual
/-- Decode a `Page.FrameResourceTree`. -/
def Page.parseFrameResourceTree (v : Value) : Except String Page.FrameResourceTree := do
  let childFrames ← match h0 : v.lookup "childFrames" with
    | some x => (Page.parseFrameResourceTreeList x).map some
    | none => .ok none
  Page.finishFrameResourceTree v childFrames
termination_by sizeOf v
decreasing_by all_goals first | exact Value.lookup_sizeOf_lt h0
/-- Decode a JSON array of `Page.FrameResourceTree`. -/
def Page.parseFrameResourceTreeList (v : Value) : Except String (List Page.FrameResourceTree) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => Page.parseFrameResourceTree p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega
end
instance : FromJSON Page.FrameResourceTree where parseJSON := Page.parseFrameResourceTree
/-- `Page.FrameResourceTree.childFrames = some x` implies `x` is structurally smaller. -/
private theorem Page.FrameResourceTree_childFrames_sizeOf_lt {p : Page.FrameResourceTree} {x : List Page.FrameResourceTree}
    (h : p.childFrames = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [Page.FrameResourceTree.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega
mutual
/-- Encode a `Page.FrameResourceTree`. -/
def Page.encodeFrameResourceTree (p : Page.FrameResourceTree) : Value :=
  Data.Json.object <|
       [("frame", ToJSON.toJSON p.frame)]
    ++ (match h : p.childFrames with | some x => [("childFrames", Page.encodeFrameResourceTreeList x)] | none => [])
    ++ [("resources", ToJSON.toJSON p.resources)]
termination_by sizeOf p
decreasing_by
  all_goals first
    | exact Page.FrameResourceTree_childFrames_sizeOf_lt h
    | (cases p; simp only [Page.FrameResourceTree.mk.sizeOf_spec]; omega)
/-- Encode a list of `Page.FrameResourceTree`. -/
def Page.encodeFrameResourceTreeList (l : List Page.FrameResourceTree) : Value :=
  Value.array (l.map Page.encodeFrameResourceTree).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega
end
instance : ToJSON Page.FrameResourceTree where toJSON := Page.encodeFrameResourceTree

/-- `Page.FrameTree`. Self-referential; `FromJSON`/`ToJSON` are hand-written
    mutually-recursive `def`s with `sizeOf` termination proofs (see the
    module header and `CDP.Domains.HeapProfiler`). -/
structure Page.FrameTree where
  frame : Page.Frame
  childFrames : Option (List Page.FrameTree) := none
  deriving Repr, BEq
/-- Assemble a `Page.FrameTree` from its already-decoded recursive fields. -/
def Page.finishFrameTree (v : Value) (childFrames : Option (List Page.FrameTree)) : Except String Page.FrameTree := do
  .ok
    { frame := ← Value.getField v "frame" >>= FromJSON.parseJSON
      childFrames }
mutual
/-- Decode a `Page.FrameTree`. -/
def Page.parseFrameTree (v : Value) : Except String Page.FrameTree := do
  let childFrames ← match h0 : v.lookup "childFrames" with
    | some x => (Page.parseFrameTreeList x).map some
    | none => .ok none
  Page.finishFrameTree v childFrames
termination_by sizeOf v
decreasing_by all_goals first | exact Value.lookup_sizeOf_lt h0
/-- Decode a JSON array of `Page.FrameTree`. -/
def Page.parseFrameTreeList (v : Value) : Except String (List Page.FrameTree) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => Page.parseFrameTree p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega
end
instance : FromJSON Page.FrameTree where parseJSON := Page.parseFrameTree
/-- `Page.FrameTree.childFrames = some x` implies `x` is structurally smaller. -/
private theorem Page.FrameTree_childFrames_sizeOf_lt {p : Page.FrameTree} {x : List Page.FrameTree}
    (h : p.childFrames = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [Page.FrameTree.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega
mutual
/-- Encode a `Page.FrameTree`. -/
def Page.encodeFrameTree (p : Page.FrameTree) : Value :=
  Data.Json.object <|
       [("frame", ToJSON.toJSON p.frame)]
    ++ (match h : p.childFrames with | some x => [("childFrames", Page.encodeFrameTreeList x)] | none => [])
termination_by sizeOf p
decreasing_by
  all_goals first
    | exact Page.FrameTree_childFrames_sizeOf_lt h
    | (cases p; simp only [Page.FrameTree.mk.sizeOf_spec]; omega)
/-- Encode a list of `Page.FrameTree`. -/
def Page.encodeFrameTreeList (l : List Page.FrameTree) : Value :=
  Value.array (l.map Page.encodeFrameTree).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega
end
instance : ToJSON Page.FrameTree where toJSON := Page.encodeFrameTree

/-- `Page.ScriptIdentifier`. -/
abbrev Page.ScriptIdentifier := String

/-- `Page.TransitionType`. -/
inductive Page.TransitionType where
  | link | typed | address_bar | auto_bookmark | auto_subframe | manual_subframe | generated | auto_toplevel | form_submit | reload | keyword | keyword_generated | other
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.TransitionType where
  parseJSON
    | .string "link" => .ok .link
    | .string "typed" => .ok .typed
    | .string "address_bar" => .ok .address_bar
    | .string "auto_bookmark" => .ok .auto_bookmark
    | .string "auto_subframe" => .ok .auto_subframe
    | .string "manual_subframe" => .ok .manual_subframe
    | .string "generated" => .ok .generated
    | .string "auto_toplevel" => .ok .auto_toplevel
    | .string "form_submit" => .ok .form_submit
    | .string "reload" => .ok .reload
    | .string "keyword" => .ok .keyword
    | .string "keyword_generated" => .ok .keyword_generated
    | .string "other" => .ok .other
    | v => .error s!"failed to parse Page.TransitionType: {repr v}"
instance : ToJSON Page.TransitionType where
  toJSON
    | .link => .string "link"
    | .typed => .string "typed"
    | .address_bar => .string "address_bar"
    | .auto_bookmark => .string "auto_bookmark"
    | .auto_subframe => .string "auto_subframe"
    | .manual_subframe => .string "manual_subframe"
    | .generated => .string "generated"
    | .auto_toplevel => .string "auto_toplevel"
    | .form_submit => .string "form_submit"
    | .reload => .string "reload"
    | .keyword => .string "keyword"
    | .keyword_generated => .string "keyword_generated"
    | .other => .string "other"

/-- `Page.NavigationEntry`. -/
structure Page.NavigationEntry where
  id : Int
  url : String
  userTypedURL : String
  title : String
  transitionType : Page.TransitionType
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.NavigationEntry where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        userTypedURL := ← Value.getField v "userTypedURL" >>= FromJSON.parseJSON
        title := ← Value.getField v "title" >>= FromJSON.parseJSON
        transitionType := ← Value.getField v "transitionType" >>= FromJSON.parseJSON }
instance : ToJSON Page.NavigationEntry where
  toJSON p := Data.Json.object <|
       [("id", ToJSON.toJSON p.id)]
    ++ [("url", ToJSON.toJSON p.url)]
    ++ [("userTypedURL", ToJSON.toJSON p.userTypedURL)]
    ++ [("title", ToJSON.toJSON p.title)]
    ++ [("transitionType", ToJSON.toJSON p.transitionType)]

/-- `Page.ScreencastFrameMetadata`. -/
structure Page.ScreencastFrameMetadata where
  offsetTop : Float
  pageScaleFactor : Float
  deviceWidth : Float
  deviceHeight : Float
  scrollOffsetX : Float
  scrollOffsetY : Float
  timestamp : Option Network.TimeSinceEpoch := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.ScreencastFrameMetadata where
  parseJSON v := do
    .ok
      { offsetTop := ← Value.getField v "offsetTop" >>= FromJSON.parseJSON
        pageScaleFactor := ← Value.getField v "pageScaleFactor" >>= FromJSON.parseJSON
        deviceWidth := ← Value.getField v "deviceWidth" >>= FromJSON.parseJSON
        deviceHeight := ← Value.getField v "deviceHeight" >>= FromJSON.parseJSON
        scrollOffsetX := ← Value.getField v "scrollOffsetX" >>= FromJSON.parseJSON
        scrollOffsetY := ← Value.getField v "scrollOffsetY" >>= FromJSON.parseJSON
        timestamp := ← (← Value.getFieldOpt v "timestamp").mapM FromJSON.parseJSON }
instance : ToJSON Page.ScreencastFrameMetadata where
  toJSON p := Data.Json.object <|
       [("offsetTop", ToJSON.toJSON p.offsetTop)]
    ++ [("pageScaleFactor", ToJSON.toJSON p.pageScaleFactor)]
    ++ [("deviceWidth", ToJSON.toJSON p.deviceWidth)]
    ++ [("deviceHeight", ToJSON.toJSON p.deviceHeight)]
    ++ [("scrollOffsetX", ToJSON.toJSON p.scrollOffsetX)]
    ++ [("scrollOffsetY", ToJSON.toJSON p.scrollOffsetY)]
    ++ (p.timestamp.map (fun x => ("timestamp", ToJSON.toJSON x))).toList

/-- `Page.DialogType`. -/
inductive Page.DialogType where
  | alert | confirm | prompt | beforeunload
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.DialogType where
  parseJSON
    | .string "alert" => .ok .alert
    | .string "confirm" => .ok .confirm
    | .string "prompt" => .ok .prompt
    | .string "beforeunload" => .ok .beforeunload
    | v => .error s!"failed to parse Page.DialogType: {repr v}"
instance : ToJSON Page.DialogType where
  toJSON
    | .alert => .string "alert"
    | .confirm => .string "confirm"
    | .prompt => .string "prompt"
    | .beforeunload => .string "beforeunload"

/-- `Page.AppManifestError`. -/
structure Page.AppManifestError where
  message : String
  critical : Int
  line : Int
  column : Int
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.AppManifestError where
  parseJSON v := do
    .ok
      { message := ← Value.getField v "message" >>= FromJSON.parseJSON
        critical := ← Value.getField v "critical" >>= FromJSON.parseJSON
        line := ← Value.getField v "line" >>= FromJSON.parseJSON
        column := ← Value.getField v "column" >>= FromJSON.parseJSON }
instance : ToJSON Page.AppManifestError where
  toJSON p := Data.Json.object <|
       [("message", ToJSON.toJSON p.message)]
    ++ [("critical", ToJSON.toJSON p.critical)]
    ++ [("line", ToJSON.toJSON p.line)]
    ++ [("column", ToJSON.toJSON p.column)]

/-- `Page.AppManifestParsedProperties`. -/
structure Page.AppManifestParsedProperties where
  scope : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.AppManifestParsedProperties where
  parseJSON v := do
    .ok
      { scope := ← Value.getField v "scope" >>= FromJSON.parseJSON }
instance : ToJSON Page.AppManifestParsedProperties where
  toJSON p := Data.Json.object <|
       [("scope", ToJSON.toJSON p.scope)]

/-- `Page.LayoutViewport`. -/
structure Page.LayoutViewport where
  pageX : Int
  pageY : Int
  clientWidth : Int
  clientHeight : Int
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.LayoutViewport where
  parseJSON v := do
    .ok
      { pageX := ← Value.getField v "pageX" >>= FromJSON.parseJSON
        pageY := ← Value.getField v "pageY" >>= FromJSON.parseJSON
        clientWidth := ← Value.getField v "clientWidth" >>= FromJSON.parseJSON
        clientHeight := ← Value.getField v "clientHeight" >>= FromJSON.parseJSON }
instance : ToJSON Page.LayoutViewport where
  toJSON p := Data.Json.object <|
       [("pageX", ToJSON.toJSON p.pageX)]
    ++ [("pageY", ToJSON.toJSON p.pageY)]
    ++ [("clientWidth", ToJSON.toJSON p.clientWidth)]
    ++ [("clientHeight", ToJSON.toJSON p.clientHeight)]

/-- `Page.VisualViewport`. -/
structure Page.VisualViewport where
  offsetX : Float
  offsetY : Float
  pageX : Float
  pageY : Float
  clientWidth : Float
  clientHeight : Float
  scale : Float
  zoom : Option Float := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.VisualViewport where
  parseJSON v := do
    .ok
      { offsetX := ← Value.getField v "offsetX" >>= FromJSON.parseJSON
        offsetY := ← Value.getField v "offsetY" >>= FromJSON.parseJSON
        pageX := ← Value.getField v "pageX" >>= FromJSON.parseJSON
        pageY := ← Value.getField v "pageY" >>= FromJSON.parseJSON
        clientWidth := ← Value.getField v "clientWidth" >>= FromJSON.parseJSON
        clientHeight := ← Value.getField v "clientHeight" >>= FromJSON.parseJSON
        scale := ← Value.getField v "scale" >>= FromJSON.parseJSON
        zoom := ← (← Value.getFieldOpt v "zoom").mapM FromJSON.parseJSON }
instance : ToJSON Page.VisualViewport where
  toJSON p := Data.Json.object <|
       [("offsetX", ToJSON.toJSON p.offsetX)]
    ++ [("offsetY", ToJSON.toJSON p.offsetY)]
    ++ [("pageX", ToJSON.toJSON p.pageX)]
    ++ [("pageY", ToJSON.toJSON p.pageY)]
    ++ [("clientWidth", ToJSON.toJSON p.clientWidth)]
    ++ [("clientHeight", ToJSON.toJSON p.clientHeight)]
    ++ [("scale", ToJSON.toJSON p.scale)]
    ++ (p.zoom.map (fun x => ("zoom", ToJSON.toJSON x))).toList

/-- `Page.FontFamilies`. -/
structure Page.FontFamilies where
  standard : Option String := none
  fixed : Option String := none
  serif : Option String := none
  sansSerif : Option String := none
  cursive : Option String := none
  fantasy : Option String := none
  math : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FontFamilies where
  parseJSON v := do
    .ok
      { standard := ← (← Value.getFieldOpt v "standard").mapM FromJSON.parseJSON
        fixed := ← (← Value.getFieldOpt v "fixed").mapM FromJSON.parseJSON
        serif := ← (← Value.getFieldOpt v "serif").mapM FromJSON.parseJSON
        sansSerif := ← (← Value.getFieldOpt v "sansSerif").mapM FromJSON.parseJSON
        cursive := ← (← Value.getFieldOpt v "cursive").mapM FromJSON.parseJSON
        fantasy := ← (← Value.getFieldOpt v "fantasy").mapM FromJSON.parseJSON
        math := ← (← Value.getFieldOpt v "math").mapM FromJSON.parseJSON }
instance : ToJSON Page.FontFamilies where
  toJSON p := Data.Json.object <|
       (p.standard.map (fun x => ("standard", ToJSON.toJSON x))).toList
    ++ (p.fixed.map (fun x => ("fixed", ToJSON.toJSON x))).toList
    ++ (p.serif.map (fun x => ("serif", ToJSON.toJSON x))).toList
    ++ (p.sansSerif.map (fun x => ("sansSerif", ToJSON.toJSON x))).toList
    ++ (p.cursive.map (fun x => ("cursive", ToJSON.toJSON x))).toList
    ++ (p.fantasy.map (fun x => ("fantasy", ToJSON.toJSON x))).toList
    ++ (p.math.map (fun x => ("math", ToJSON.toJSON x))).toList

/-- `Page.ScriptFontFamilies`. -/
structure Page.ScriptFontFamilies where
  script : String
  fontFamilies : Page.FontFamilies
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.ScriptFontFamilies where
  parseJSON v := do
    .ok
      { script := ← Value.getField v "script" >>= FromJSON.parseJSON
        fontFamilies := ← Value.getField v "fontFamilies" >>= FromJSON.parseJSON }
instance : ToJSON Page.ScriptFontFamilies where
  toJSON p := Data.Json.object <|
       [("script", ToJSON.toJSON p.script)]
    ++ [("fontFamilies", ToJSON.toJSON p.fontFamilies)]

/-- `Page.FontSizes`. -/
structure Page.FontSizes where
  standard : Option Int := none
  fixed : Option Int := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FontSizes where
  parseJSON v := do
    .ok
      { standard := ← (← Value.getFieldOpt v "standard").mapM FromJSON.parseJSON
        fixed := ← (← Value.getFieldOpt v "fixed").mapM FromJSON.parseJSON }
instance : ToJSON Page.FontSizes where
  toJSON p := Data.Json.object <|
       (p.standard.map (fun x => ("standard", ToJSON.toJSON x))).toList
    ++ (p.fixed.map (fun x => ("fixed", ToJSON.toJSON x))).toList

/-- `Page.ClientNavigationReason`. -/
inductive Page.ClientNavigationReason where
  | formSubmissionGet | formSubmissionPost | httpHeaderRefresh | scriptInitiated | metaTagRefresh | pageBlockInterstitial | reload | anchorClick
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.ClientNavigationReason where
  parseJSON
    | .string "formSubmissionGet" => .ok .formSubmissionGet
    | .string "formSubmissionPost" => .ok .formSubmissionPost
    | .string "httpHeaderRefresh" => .ok .httpHeaderRefresh
    | .string "scriptInitiated" => .ok .scriptInitiated
    | .string "metaTagRefresh" => .ok .metaTagRefresh
    | .string "pageBlockInterstitial" => .ok .pageBlockInterstitial
    | .string "reload" => .ok .reload
    | .string "anchorClick" => .ok .anchorClick
    | v => .error s!"failed to parse Page.ClientNavigationReason: {repr v}"
instance : ToJSON Page.ClientNavigationReason where
  toJSON
    | .formSubmissionGet => .string "formSubmissionGet"
    | .formSubmissionPost => .string "formSubmissionPost"
    | .httpHeaderRefresh => .string "httpHeaderRefresh"
    | .scriptInitiated => .string "scriptInitiated"
    | .metaTagRefresh => .string "metaTagRefresh"
    | .pageBlockInterstitial => .string "pageBlockInterstitial"
    | .reload => .string "reload"
    | .anchorClick => .string "anchorClick"

/-- `Page.ClientNavigationDisposition`. -/
inductive Page.ClientNavigationDisposition where
  | currentTab | newTab | newWindow | download
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.ClientNavigationDisposition where
  parseJSON
    | .string "currentTab" => .ok .currentTab
    | .string "newTab" => .ok .newTab
    | .string "newWindow" => .ok .newWindow
    | .string "download" => .ok .download
    | v => .error s!"failed to parse Page.ClientNavigationDisposition: {repr v}"
instance : ToJSON Page.ClientNavigationDisposition where
  toJSON
    | .currentTab => .string "currentTab"
    | .newTab => .string "newTab"
    | .newWindow => .string "newWindow"
    | .download => .string "download"

/-- `Page.InstallabilityErrorArgument`. -/
structure Page.InstallabilityErrorArgument where
  name : String
  value : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.InstallabilityErrorArgument where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }
instance : ToJSON Page.InstallabilityErrorArgument where
  toJSON p := Data.Json.object <|
       [("name", ToJSON.toJSON p.name)]
    ++ [("value", ToJSON.toJSON p.value)]

/-- `Page.InstallabilityError`. -/
structure Page.InstallabilityError where
  errorId : String
  errorArguments : List Page.InstallabilityErrorArgument
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.InstallabilityError where
  parseJSON v := do
    .ok
      { errorId := ← Value.getField v "errorId" >>= FromJSON.parseJSON
        errorArguments := ← Value.getField v "errorArguments" >>= FromJSON.parseJSON }
instance : ToJSON Page.InstallabilityError where
  toJSON p := Data.Json.object <|
       [("errorId", ToJSON.toJSON p.errorId)]
    ++ [("errorArguments", ToJSON.toJSON p.errorArguments)]

/-- `Page.ReferrerPolicy`. -/
inductive Page.ReferrerPolicy where
  | noReferrer | noReferrerWhenDowngrade | origin | originWhenCrossOrigin | sameOrigin | strictOrigin | strictOriginWhenCrossOrigin | unsafeUrl
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.ReferrerPolicy where
  parseJSON
    | .string "noReferrer" => .ok .noReferrer
    | .string "noReferrerWhenDowngrade" => .ok .noReferrerWhenDowngrade
    | .string "origin" => .ok .origin
    | .string "originWhenCrossOrigin" => .ok .originWhenCrossOrigin
    | .string "sameOrigin" => .ok .sameOrigin
    | .string "strictOrigin" => .ok .strictOrigin
    | .string "strictOriginWhenCrossOrigin" => .ok .strictOriginWhenCrossOrigin
    | .string "unsafeUrl" => .ok .unsafeUrl
    | v => .error s!"failed to parse Page.ReferrerPolicy: {repr v}"
instance : ToJSON Page.ReferrerPolicy where
  toJSON
    | .noReferrer => .string "noReferrer"
    | .noReferrerWhenDowngrade => .string "noReferrerWhenDowngrade"
    | .origin => .string "origin"
    | .originWhenCrossOrigin => .string "originWhenCrossOrigin"
    | .sameOrigin => .string "sameOrigin"
    | .strictOrigin => .string "strictOrigin"
    | .strictOriginWhenCrossOrigin => .string "strictOriginWhenCrossOrigin"
    | .unsafeUrl => .string "unsafeUrl"

/-- `Page.CompilationCacheParams`. -/
structure Page.CompilationCacheParams where
  url : String
  eager : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.CompilationCacheParams where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        eager := ← (← Value.getFieldOpt v "eager").mapM FromJSON.parseJSON }
instance : ToJSON Page.CompilationCacheParams where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ (p.eager.map (fun x => ("eager", ToJSON.toJSON x))).toList

/-- `Page.NavigationType`. -/
inductive Page.NavigationType where
  | navigation | backForwardCacheRestore
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.NavigationType where
  parseJSON
    | .string "Navigation" => .ok .navigation
    | .string "BackForwardCacheRestore" => .ok .backForwardCacheRestore
    | v => .error s!"failed to parse Page.NavigationType: {repr v}"
instance : ToJSON Page.NavigationType where
  toJSON
    | .navigation => .string "Navigation"
    | .backForwardCacheRestore => .string "BackForwardCacheRestore"

/-- `Page.BackForwardCacheNotRestoredReason`. -/
inductive Page.BackForwardCacheNotRestoredReason where
  | notPrimaryMainFrame | backForwardCacheDisabled | relatedActiveContentsExist | hTTPStatusNotOK | schemeNotHTTPOrHTTPS | loading | wasGrantedMediaAccess | disableForRenderFrameHostCalled | domainNotAllowed | hTTPMethodNotGET | subframeIsNavigating | timeout | cacheLimit | javaScriptExecution | rendererProcessKilled | rendererProcessCrashed | schedulerTrackedFeatureUsed | conflictingBrowsingInstance | cacheFlushed | serviceWorkerVersionActivation | sessionRestored | serviceWorkerPostMessage | enteredBackForwardCacheBeforeServiceWorkerHostAdded | renderFrameHostReused_SameSite | renderFrameHostReused_CrossSite | serviceWorkerClaim | ignoreEventAndEvict | haveInnerContents | timeoutPuttingInCache | backForwardCacheDisabledByLowMemory | backForwardCacheDisabledByCommandLine | networkRequestDatapipeDrainedAsBytesConsumer | networkRequestRedirected | networkRequestTimeout | networkExceedsBufferLimit | navigationCancelledWhileRestoring | notMostRecentNavigationEntry | backForwardCacheDisabledForPrerender | userAgentOverrideDiffers | foregroundCacheLimit | browsingInstanceNotSwapped | backForwardCacheDisabledForDelegate | unloadHandlerExistsInMainFrame | unloadHandlerExistsInSubFrame | serviceWorkerUnregistration | cacheControlNoStore | cacheControlNoStoreCookieModified | cacheControlNoStoreHTTPOnlyCookieModified | noResponseHead | unknown | activationNavigationsDisallowedForBug1234857 | errorDocument | fencedFramesEmbedder | webSocket | webTransport | webRTC | mainResourceHasCacheControlNoStore | mainResourceHasCacheControlNoCache | subresourceHasCacheControlNoStore | subresourceHasCacheControlNoCache | containsPlugins | documentLoaded | dedicatedWorkerOrWorklet | outstandingNetworkRequestOthers | outstandingIndexedDBTransaction | requestedNotificationsPermission | requestedMIDIPermission | requestedAudioCapturePermission | requestedVideoCapturePermission | requestedBackForwardCacheBlockedSensors | requestedBackgroundWorkPermission | broadcastChannel | indexedDBConnection | webXR | sharedWorker | webLocks | webHID | webShare | requestedStorageAccessGrant | webNfc | outstandingNetworkRequestFetch | outstandingNetworkRequestXHR | appBanner | printing | webDatabase | pictureInPicture | portal | speechRecognizer | idleManager | paymentManager | speechSynthesis | keyboardLock | webOTPService | outstandingNetworkRequestDirectSocket | injectedJavascript | injectedStyleSheet | dummy | contentSecurityHandler | contentWebAuthenticationAPI | contentFileChooser | contentSerial | contentFileSystemAccess | contentMediaDevicesDispatcherHost | contentWebBluetooth | contentWebUSB | contentMediaSessionService | contentScreenReader | embedderPopupBlockerTabHelper | embedderSafeBrowsingTriggeredPopupBlocker | embedderSafeBrowsingThreatDetails | embedderAppBannerManager | embedderDomDistillerViewerSource | embedderDomDistillerSelfDeletingRequestDelegate | embedderOomInterventionTabHelper | embedderOfflinePage | embedderChromePasswordManagerClientBindCredentialManager | embedderPermissionRequestManager | embedderModalDialog | embedderExtensions | embedderExtensionMessaging | embedderExtensionMessagingForOpenPort | embedderExtensionSentMessageToCachedFrame
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.BackForwardCacheNotRestoredReason where
  parseJSON
    | .string "NotPrimaryMainFrame" => .ok .notPrimaryMainFrame
    | .string "BackForwardCacheDisabled" => .ok .backForwardCacheDisabled
    | .string "RelatedActiveContentsExist" => .ok .relatedActiveContentsExist
    | .string "HTTPStatusNotOK" => .ok .hTTPStatusNotOK
    | .string "SchemeNotHTTPOrHTTPS" => .ok .schemeNotHTTPOrHTTPS
    | .string "Loading" => .ok .loading
    | .string "WasGrantedMediaAccess" => .ok .wasGrantedMediaAccess
    | .string "DisableForRenderFrameHostCalled" => .ok .disableForRenderFrameHostCalled
    | .string "DomainNotAllowed" => .ok .domainNotAllowed
    | .string "HTTPMethodNotGET" => .ok .hTTPMethodNotGET
    | .string "SubframeIsNavigating" => .ok .subframeIsNavigating
    | .string "Timeout" => .ok .timeout
    | .string "CacheLimit" => .ok .cacheLimit
    | .string "JavaScriptExecution" => .ok .javaScriptExecution
    | .string "RendererProcessKilled" => .ok .rendererProcessKilled
    | .string "RendererProcessCrashed" => .ok .rendererProcessCrashed
    | .string "SchedulerTrackedFeatureUsed" => .ok .schedulerTrackedFeatureUsed
    | .string "ConflictingBrowsingInstance" => .ok .conflictingBrowsingInstance
    | .string "CacheFlushed" => .ok .cacheFlushed
    | .string "ServiceWorkerVersionActivation" => .ok .serviceWorkerVersionActivation
    | .string "SessionRestored" => .ok .sessionRestored
    | .string "ServiceWorkerPostMessage" => .ok .serviceWorkerPostMessage
    | .string "EnteredBackForwardCacheBeforeServiceWorkerHostAdded" => .ok .enteredBackForwardCacheBeforeServiceWorkerHostAdded
    | .string "RenderFrameHostReused_SameSite" => .ok .renderFrameHostReused_SameSite
    | .string "RenderFrameHostReused_CrossSite" => .ok .renderFrameHostReused_CrossSite
    | .string "ServiceWorkerClaim" => .ok .serviceWorkerClaim
    | .string "IgnoreEventAndEvict" => .ok .ignoreEventAndEvict
    | .string "HaveInnerContents" => .ok .haveInnerContents
    | .string "TimeoutPuttingInCache" => .ok .timeoutPuttingInCache
    | .string "BackForwardCacheDisabledByLowMemory" => .ok .backForwardCacheDisabledByLowMemory
    | .string "BackForwardCacheDisabledByCommandLine" => .ok .backForwardCacheDisabledByCommandLine
    | .string "NetworkRequestDatapipeDrainedAsBytesConsumer" => .ok .networkRequestDatapipeDrainedAsBytesConsumer
    | .string "NetworkRequestRedirected" => .ok .networkRequestRedirected
    | .string "NetworkRequestTimeout" => .ok .networkRequestTimeout
    | .string "NetworkExceedsBufferLimit" => .ok .networkExceedsBufferLimit
    | .string "NavigationCancelledWhileRestoring" => .ok .navigationCancelledWhileRestoring
    | .string "NotMostRecentNavigationEntry" => .ok .notMostRecentNavigationEntry
    | .string "BackForwardCacheDisabledForPrerender" => .ok .backForwardCacheDisabledForPrerender
    | .string "UserAgentOverrideDiffers" => .ok .userAgentOverrideDiffers
    | .string "ForegroundCacheLimit" => .ok .foregroundCacheLimit
    | .string "BrowsingInstanceNotSwapped" => .ok .browsingInstanceNotSwapped
    | .string "BackForwardCacheDisabledForDelegate" => .ok .backForwardCacheDisabledForDelegate
    | .string "UnloadHandlerExistsInMainFrame" => .ok .unloadHandlerExistsInMainFrame
    | .string "UnloadHandlerExistsInSubFrame" => .ok .unloadHandlerExistsInSubFrame
    | .string "ServiceWorkerUnregistration" => .ok .serviceWorkerUnregistration
    | .string "CacheControlNoStore" => .ok .cacheControlNoStore
    | .string "CacheControlNoStoreCookieModified" => .ok .cacheControlNoStoreCookieModified
    | .string "CacheControlNoStoreHTTPOnlyCookieModified" => .ok .cacheControlNoStoreHTTPOnlyCookieModified
    | .string "NoResponseHead" => .ok .noResponseHead
    | .string "Unknown" => .ok .unknown
    | .string "ActivationNavigationsDisallowedForBug1234857" => .ok .activationNavigationsDisallowedForBug1234857
    | .string "ErrorDocument" => .ok .errorDocument
    | .string "FencedFramesEmbedder" => .ok .fencedFramesEmbedder
    | .string "WebSocket" => .ok .webSocket
    | .string "WebTransport" => .ok .webTransport
    | .string "WebRTC" => .ok .webRTC
    | .string "MainResourceHasCacheControlNoStore" => .ok .mainResourceHasCacheControlNoStore
    | .string "MainResourceHasCacheControlNoCache" => .ok .mainResourceHasCacheControlNoCache
    | .string "SubresourceHasCacheControlNoStore" => .ok .subresourceHasCacheControlNoStore
    | .string "SubresourceHasCacheControlNoCache" => .ok .subresourceHasCacheControlNoCache
    | .string "ContainsPlugins" => .ok .containsPlugins
    | .string "DocumentLoaded" => .ok .documentLoaded
    | .string "DedicatedWorkerOrWorklet" => .ok .dedicatedWorkerOrWorklet
    | .string "OutstandingNetworkRequestOthers" => .ok .outstandingNetworkRequestOthers
    | .string "OutstandingIndexedDBTransaction" => .ok .outstandingIndexedDBTransaction
    | .string "RequestedNotificationsPermission" => .ok .requestedNotificationsPermission
    | .string "RequestedMIDIPermission" => .ok .requestedMIDIPermission
    | .string "RequestedAudioCapturePermission" => .ok .requestedAudioCapturePermission
    | .string "RequestedVideoCapturePermission" => .ok .requestedVideoCapturePermission
    | .string "RequestedBackForwardCacheBlockedSensors" => .ok .requestedBackForwardCacheBlockedSensors
    | .string "RequestedBackgroundWorkPermission" => .ok .requestedBackgroundWorkPermission
    | .string "BroadcastChannel" => .ok .broadcastChannel
    | .string "IndexedDBConnection" => .ok .indexedDBConnection
    | .string "WebXR" => .ok .webXR
    | .string "SharedWorker" => .ok .sharedWorker
    | .string "WebLocks" => .ok .webLocks
    | .string "WebHID" => .ok .webHID
    | .string "WebShare" => .ok .webShare
    | .string "RequestedStorageAccessGrant" => .ok .requestedStorageAccessGrant
    | .string "WebNfc" => .ok .webNfc
    | .string "OutstandingNetworkRequestFetch" => .ok .outstandingNetworkRequestFetch
    | .string "OutstandingNetworkRequestXHR" => .ok .outstandingNetworkRequestXHR
    | .string "AppBanner" => .ok .appBanner
    | .string "Printing" => .ok .printing
    | .string "WebDatabase" => .ok .webDatabase
    | .string "PictureInPicture" => .ok .pictureInPicture
    | .string "Portal" => .ok .portal
    | .string "SpeechRecognizer" => .ok .speechRecognizer
    | .string "IdleManager" => .ok .idleManager
    | .string "PaymentManager" => .ok .paymentManager
    | .string "SpeechSynthesis" => .ok .speechSynthesis
    | .string "KeyboardLock" => .ok .keyboardLock
    | .string "WebOTPService" => .ok .webOTPService
    | .string "OutstandingNetworkRequestDirectSocket" => .ok .outstandingNetworkRequestDirectSocket
    | .string "InjectedJavascript" => .ok .injectedJavascript
    | .string "InjectedStyleSheet" => .ok .injectedStyleSheet
    | .string "Dummy" => .ok .dummy
    | .string "ContentSecurityHandler" => .ok .contentSecurityHandler
    | .string "ContentWebAuthenticationAPI" => .ok .contentWebAuthenticationAPI
    | .string "ContentFileChooser" => .ok .contentFileChooser
    | .string "ContentSerial" => .ok .contentSerial
    | .string "ContentFileSystemAccess" => .ok .contentFileSystemAccess
    | .string "ContentMediaDevicesDispatcherHost" => .ok .contentMediaDevicesDispatcherHost
    | .string "ContentWebBluetooth" => .ok .contentWebBluetooth
    | .string "ContentWebUSB" => .ok .contentWebUSB
    | .string "ContentMediaSessionService" => .ok .contentMediaSessionService
    | .string "ContentScreenReader" => .ok .contentScreenReader
    | .string "EmbedderPopupBlockerTabHelper" => .ok .embedderPopupBlockerTabHelper
    | .string "EmbedderSafeBrowsingTriggeredPopupBlocker" => .ok .embedderSafeBrowsingTriggeredPopupBlocker
    | .string "EmbedderSafeBrowsingThreatDetails" => .ok .embedderSafeBrowsingThreatDetails
    | .string "EmbedderAppBannerManager" => .ok .embedderAppBannerManager
    | .string "EmbedderDomDistillerViewerSource" => .ok .embedderDomDistillerViewerSource
    | .string "EmbedderDomDistillerSelfDeletingRequestDelegate" => .ok .embedderDomDistillerSelfDeletingRequestDelegate
    | .string "EmbedderOomInterventionTabHelper" => .ok .embedderOomInterventionTabHelper
    | .string "EmbedderOfflinePage" => .ok .embedderOfflinePage
    | .string "EmbedderChromePasswordManagerClientBindCredentialManager" => .ok .embedderChromePasswordManagerClientBindCredentialManager
    | .string "EmbedderPermissionRequestManager" => .ok .embedderPermissionRequestManager
    | .string "EmbedderModalDialog" => .ok .embedderModalDialog
    | .string "EmbedderExtensions" => .ok .embedderExtensions
    | .string "EmbedderExtensionMessaging" => .ok .embedderExtensionMessaging
    | .string "EmbedderExtensionMessagingForOpenPort" => .ok .embedderExtensionMessagingForOpenPort
    | .string "EmbedderExtensionSentMessageToCachedFrame" => .ok .embedderExtensionSentMessageToCachedFrame
    | v => .error s!"failed to parse Page.BackForwardCacheNotRestoredReason: {repr v}"
instance : ToJSON Page.BackForwardCacheNotRestoredReason where
  toJSON
    | .notPrimaryMainFrame => .string "NotPrimaryMainFrame"
    | .backForwardCacheDisabled => .string "BackForwardCacheDisabled"
    | .relatedActiveContentsExist => .string "RelatedActiveContentsExist"
    | .hTTPStatusNotOK => .string "HTTPStatusNotOK"
    | .schemeNotHTTPOrHTTPS => .string "SchemeNotHTTPOrHTTPS"
    | .loading => .string "Loading"
    | .wasGrantedMediaAccess => .string "WasGrantedMediaAccess"
    | .disableForRenderFrameHostCalled => .string "DisableForRenderFrameHostCalled"
    | .domainNotAllowed => .string "DomainNotAllowed"
    | .hTTPMethodNotGET => .string "HTTPMethodNotGET"
    | .subframeIsNavigating => .string "SubframeIsNavigating"
    | .timeout => .string "Timeout"
    | .cacheLimit => .string "CacheLimit"
    | .javaScriptExecution => .string "JavaScriptExecution"
    | .rendererProcessKilled => .string "RendererProcessKilled"
    | .rendererProcessCrashed => .string "RendererProcessCrashed"
    | .schedulerTrackedFeatureUsed => .string "SchedulerTrackedFeatureUsed"
    | .conflictingBrowsingInstance => .string "ConflictingBrowsingInstance"
    | .cacheFlushed => .string "CacheFlushed"
    | .serviceWorkerVersionActivation => .string "ServiceWorkerVersionActivation"
    | .sessionRestored => .string "SessionRestored"
    | .serviceWorkerPostMessage => .string "ServiceWorkerPostMessage"
    | .enteredBackForwardCacheBeforeServiceWorkerHostAdded => .string "EnteredBackForwardCacheBeforeServiceWorkerHostAdded"
    | .renderFrameHostReused_SameSite => .string "RenderFrameHostReused_SameSite"
    | .renderFrameHostReused_CrossSite => .string "RenderFrameHostReused_CrossSite"
    | .serviceWorkerClaim => .string "ServiceWorkerClaim"
    | .ignoreEventAndEvict => .string "IgnoreEventAndEvict"
    | .haveInnerContents => .string "HaveInnerContents"
    | .timeoutPuttingInCache => .string "TimeoutPuttingInCache"
    | .backForwardCacheDisabledByLowMemory => .string "BackForwardCacheDisabledByLowMemory"
    | .backForwardCacheDisabledByCommandLine => .string "BackForwardCacheDisabledByCommandLine"
    | .networkRequestDatapipeDrainedAsBytesConsumer => .string "NetworkRequestDatapipeDrainedAsBytesConsumer"
    | .networkRequestRedirected => .string "NetworkRequestRedirected"
    | .networkRequestTimeout => .string "NetworkRequestTimeout"
    | .networkExceedsBufferLimit => .string "NetworkExceedsBufferLimit"
    | .navigationCancelledWhileRestoring => .string "NavigationCancelledWhileRestoring"
    | .notMostRecentNavigationEntry => .string "NotMostRecentNavigationEntry"
    | .backForwardCacheDisabledForPrerender => .string "BackForwardCacheDisabledForPrerender"
    | .userAgentOverrideDiffers => .string "UserAgentOverrideDiffers"
    | .foregroundCacheLimit => .string "ForegroundCacheLimit"
    | .browsingInstanceNotSwapped => .string "BrowsingInstanceNotSwapped"
    | .backForwardCacheDisabledForDelegate => .string "BackForwardCacheDisabledForDelegate"
    | .unloadHandlerExistsInMainFrame => .string "UnloadHandlerExistsInMainFrame"
    | .unloadHandlerExistsInSubFrame => .string "UnloadHandlerExistsInSubFrame"
    | .serviceWorkerUnregistration => .string "ServiceWorkerUnregistration"
    | .cacheControlNoStore => .string "CacheControlNoStore"
    | .cacheControlNoStoreCookieModified => .string "CacheControlNoStoreCookieModified"
    | .cacheControlNoStoreHTTPOnlyCookieModified => .string "CacheControlNoStoreHTTPOnlyCookieModified"
    | .noResponseHead => .string "NoResponseHead"
    | .unknown => .string "Unknown"
    | .activationNavigationsDisallowedForBug1234857 => .string "ActivationNavigationsDisallowedForBug1234857"
    | .errorDocument => .string "ErrorDocument"
    | .fencedFramesEmbedder => .string "FencedFramesEmbedder"
    | .webSocket => .string "WebSocket"
    | .webTransport => .string "WebTransport"
    | .webRTC => .string "WebRTC"
    | .mainResourceHasCacheControlNoStore => .string "MainResourceHasCacheControlNoStore"
    | .mainResourceHasCacheControlNoCache => .string "MainResourceHasCacheControlNoCache"
    | .subresourceHasCacheControlNoStore => .string "SubresourceHasCacheControlNoStore"
    | .subresourceHasCacheControlNoCache => .string "SubresourceHasCacheControlNoCache"
    | .containsPlugins => .string "ContainsPlugins"
    | .documentLoaded => .string "DocumentLoaded"
    | .dedicatedWorkerOrWorklet => .string "DedicatedWorkerOrWorklet"
    | .outstandingNetworkRequestOthers => .string "OutstandingNetworkRequestOthers"
    | .outstandingIndexedDBTransaction => .string "OutstandingIndexedDBTransaction"
    | .requestedNotificationsPermission => .string "RequestedNotificationsPermission"
    | .requestedMIDIPermission => .string "RequestedMIDIPermission"
    | .requestedAudioCapturePermission => .string "RequestedAudioCapturePermission"
    | .requestedVideoCapturePermission => .string "RequestedVideoCapturePermission"
    | .requestedBackForwardCacheBlockedSensors => .string "RequestedBackForwardCacheBlockedSensors"
    | .requestedBackgroundWorkPermission => .string "RequestedBackgroundWorkPermission"
    | .broadcastChannel => .string "BroadcastChannel"
    | .indexedDBConnection => .string "IndexedDBConnection"
    | .webXR => .string "WebXR"
    | .sharedWorker => .string "SharedWorker"
    | .webLocks => .string "WebLocks"
    | .webHID => .string "WebHID"
    | .webShare => .string "WebShare"
    | .requestedStorageAccessGrant => .string "RequestedStorageAccessGrant"
    | .webNfc => .string "WebNfc"
    | .outstandingNetworkRequestFetch => .string "OutstandingNetworkRequestFetch"
    | .outstandingNetworkRequestXHR => .string "OutstandingNetworkRequestXHR"
    | .appBanner => .string "AppBanner"
    | .printing => .string "Printing"
    | .webDatabase => .string "WebDatabase"
    | .pictureInPicture => .string "PictureInPicture"
    | .portal => .string "Portal"
    | .speechRecognizer => .string "SpeechRecognizer"
    | .idleManager => .string "IdleManager"
    | .paymentManager => .string "PaymentManager"
    | .speechSynthesis => .string "SpeechSynthesis"
    | .keyboardLock => .string "KeyboardLock"
    | .webOTPService => .string "WebOTPService"
    | .outstandingNetworkRequestDirectSocket => .string "OutstandingNetworkRequestDirectSocket"
    | .injectedJavascript => .string "InjectedJavascript"
    | .injectedStyleSheet => .string "InjectedStyleSheet"
    | .dummy => .string "Dummy"
    | .contentSecurityHandler => .string "ContentSecurityHandler"
    | .contentWebAuthenticationAPI => .string "ContentWebAuthenticationAPI"
    | .contentFileChooser => .string "ContentFileChooser"
    | .contentSerial => .string "ContentSerial"
    | .contentFileSystemAccess => .string "ContentFileSystemAccess"
    | .contentMediaDevicesDispatcherHost => .string "ContentMediaDevicesDispatcherHost"
    | .contentWebBluetooth => .string "ContentWebBluetooth"
    | .contentWebUSB => .string "ContentWebUSB"
    | .contentMediaSessionService => .string "ContentMediaSessionService"
    | .contentScreenReader => .string "ContentScreenReader"
    | .embedderPopupBlockerTabHelper => .string "EmbedderPopupBlockerTabHelper"
    | .embedderSafeBrowsingTriggeredPopupBlocker => .string "EmbedderSafeBrowsingTriggeredPopupBlocker"
    | .embedderSafeBrowsingThreatDetails => .string "EmbedderSafeBrowsingThreatDetails"
    | .embedderAppBannerManager => .string "EmbedderAppBannerManager"
    | .embedderDomDistillerViewerSource => .string "EmbedderDomDistillerViewerSource"
    | .embedderDomDistillerSelfDeletingRequestDelegate => .string "EmbedderDomDistillerSelfDeletingRequestDelegate"
    | .embedderOomInterventionTabHelper => .string "EmbedderOomInterventionTabHelper"
    | .embedderOfflinePage => .string "EmbedderOfflinePage"
    | .embedderChromePasswordManagerClientBindCredentialManager => .string "EmbedderChromePasswordManagerClientBindCredentialManager"
    | .embedderPermissionRequestManager => .string "EmbedderPermissionRequestManager"
    | .embedderModalDialog => .string "EmbedderModalDialog"
    | .embedderExtensions => .string "EmbedderExtensions"
    | .embedderExtensionMessaging => .string "EmbedderExtensionMessaging"
    | .embedderExtensionMessagingForOpenPort => .string "EmbedderExtensionMessagingForOpenPort"
    | .embedderExtensionSentMessageToCachedFrame => .string "EmbedderExtensionSentMessageToCachedFrame"

/-- `Page.BackForwardCacheNotRestoredReasonType`. -/
inductive Page.BackForwardCacheNotRestoredReasonType where
  | supportPending | pageSupportNeeded | circumstantial
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.BackForwardCacheNotRestoredReasonType where
  parseJSON
    | .string "SupportPending" => .ok .supportPending
    | .string "PageSupportNeeded" => .ok .pageSupportNeeded
    | .string "Circumstantial" => .ok .circumstantial
    | v => .error s!"failed to parse Page.BackForwardCacheNotRestoredReasonType: {repr v}"
instance : ToJSON Page.BackForwardCacheNotRestoredReasonType where
  toJSON
    | .supportPending => .string "SupportPending"
    | .pageSupportNeeded => .string "PageSupportNeeded"
    | .circumstantial => .string "Circumstantial"

/-- `Page.BackForwardCacheNotRestoredExplanation`. -/
structure Page.BackForwardCacheNotRestoredExplanation where
  type : Page.BackForwardCacheNotRestoredReasonType
  reason : Page.BackForwardCacheNotRestoredReason
  context : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.BackForwardCacheNotRestoredExplanation where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        reason := ← Value.getField v "reason" >>= FromJSON.parseJSON
        context := ← (← Value.getFieldOpt v "context").mapM FromJSON.parseJSON }
instance : ToJSON Page.BackForwardCacheNotRestoredExplanation where
  toJSON p := Data.Json.object <|
       [("type", ToJSON.toJSON p.type)]
    ++ [("reason", ToJSON.toJSON p.reason)]
    ++ (p.context.map (fun x => ("context", ToJSON.toJSON x))).toList

/-- `Page.BackForwardCacheNotRestoredExplanationTree`. Self-referential; `FromJSON`/`ToJSON` are hand-written
    mutually-recursive `def`s with `sizeOf` termination proofs (see the
    module header and `CDP.Domains.HeapProfiler`). -/
structure Page.BackForwardCacheNotRestoredExplanationTree where
  url : String
  explanations : List Page.BackForwardCacheNotRestoredExplanation
  children : List Page.BackForwardCacheNotRestoredExplanationTree
  deriving Repr, BEq
/-- Assemble a `Page.BackForwardCacheNotRestoredExplanationTree` from its already-decoded recursive fields. -/
def Page.finishBackForwardCacheNotRestoredExplanationTree (v : Value) (children : List Page.BackForwardCacheNotRestoredExplanationTree) : Except String Page.BackForwardCacheNotRestoredExplanationTree := do
  .ok
    { url := ← Value.getField v "url" >>= FromJSON.parseJSON
      explanations := ← Value.getField v "explanations" >>= FromJSON.parseJSON
      children }
mutual
/-- Decode a `Page.BackForwardCacheNotRestoredExplanationTree`. -/
def Page.parseBackForwardCacheNotRestoredExplanationTree (v : Value) : Except String Page.BackForwardCacheNotRestoredExplanationTree := do
  let children ← match h0 : v.lookup "children" with
    | some x => Page.parseBackForwardCacheNotRestoredExplanationTreeList x
    | none => .error "expected field children"
  Page.finishBackForwardCacheNotRestoredExplanationTree v children
termination_by sizeOf v
decreasing_by all_goals first | exact Value.lookup_sizeOf_lt h0
/-- Decode a JSON array of `Page.BackForwardCacheNotRestoredExplanationTree`. -/
def Page.parseBackForwardCacheNotRestoredExplanationTreeList (v : Value) : Except String (List Page.BackForwardCacheNotRestoredExplanationTree) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => Page.parseBackForwardCacheNotRestoredExplanationTree p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega
end
instance : FromJSON Page.BackForwardCacheNotRestoredExplanationTree where parseJSON := Page.parseBackForwardCacheNotRestoredExplanationTree
mutual
/-- Encode a `Page.BackForwardCacheNotRestoredExplanationTree`. -/
def Page.encodeBackForwardCacheNotRestoredExplanationTree (p : Page.BackForwardCacheNotRestoredExplanationTree) : Value :=
  Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ [("explanations", ToJSON.toJSON p.explanations)]
    ++ [("children", Page.encodeBackForwardCacheNotRestoredExplanationTreeList p.children)]
termination_by sizeOf p
decreasing_by
  all_goals first
    | (cases p; simp only [Page.BackForwardCacheNotRestoredExplanationTree.mk.sizeOf_spec]; omega)
/-- Encode a list of `Page.BackForwardCacheNotRestoredExplanationTree`. -/
def Page.encodeBackForwardCacheNotRestoredExplanationTreeList (l : List Page.BackForwardCacheNotRestoredExplanationTree) : Value :=
  Value.array (l.map Page.encodeBackForwardCacheNotRestoredExplanationTree).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega
end
instance : ToJSON Page.BackForwardCacheNotRestoredExplanationTree where toJSON := Page.encodeBackForwardCacheNotRestoredExplanationTree

/-- `Page.PrerenderFinalStatus`. -/
inductive Page.PrerenderFinalStatus where
  | activated | destroyed | lowEndDevice | crossOriginRedirect | crossOriginNavigation | invalidSchemeRedirect | invalidSchemeNavigation | inProgressNavigation | navigationRequestBlockedByCsp | mainFrameNavigation | mojoBinderPolicy | rendererProcessCrashed | rendererProcessKilled | download | triggerDestroyed | navigationNotCommitted | navigationBadHttpStatus | clientCertRequested | navigationRequestNetworkError | maxNumOfRunningPrerendersExceeded | cancelAllHostsForTesting | didFailLoad | stop | sslCertificateError | loginAuthRequested | uaChangeRequiresReload | blockedByClient | audioOutputDeviceRequested | mixedContent | triggerBackgrounded | embedderTriggeredAndCrossOriginRedirected | memoryLimitExceeded | failToGetMemoryUsage | dataSaverEnabled | hasEffectiveUrl | activatedBeforeStarted | inactivePageRestriction | startFailed | timeoutBackgrounded
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PrerenderFinalStatus where
  parseJSON
    | .string "Activated" => .ok .activated
    | .string "Destroyed" => .ok .destroyed
    | .string "LowEndDevice" => .ok .lowEndDevice
    | .string "CrossOriginRedirect" => .ok .crossOriginRedirect
    | .string "CrossOriginNavigation" => .ok .crossOriginNavigation
    | .string "InvalidSchemeRedirect" => .ok .invalidSchemeRedirect
    | .string "InvalidSchemeNavigation" => .ok .invalidSchemeNavigation
    | .string "InProgressNavigation" => .ok .inProgressNavigation
    | .string "NavigationRequestBlockedByCsp" => .ok .navigationRequestBlockedByCsp
    | .string "MainFrameNavigation" => .ok .mainFrameNavigation
    | .string "MojoBinderPolicy" => .ok .mojoBinderPolicy
    | .string "RendererProcessCrashed" => .ok .rendererProcessCrashed
    | .string "RendererProcessKilled" => .ok .rendererProcessKilled
    | .string "Download" => .ok .download
    | .string "TriggerDestroyed" => .ok .triggerDestroyed
    | .string "NavigationNotCommitted" => .ok .navigationNotCommitted
    | .string "NavigationBadHttpStatus" => .ok .navigationBadHttpStatus
    | .string "ClientCertRequested" => .ok .clientCertRequested
    | .string "NavigationRequestNetworkError" => .ok .navigationRequestNetworkError
    | .string "MaxNumOfRunningPrerendersExceeded" => .ok .maxNumOfRunningPrerendersExceeded
    | .string "CancelAllHostsForTesting" => .ok .cancelAllHostsForTesting
    | .string "DidFailLoad" => .ok .didFailLoad
    | .string "Stop" => .ok .stop
    | .string "SslCertificateError" => .ok .sslCertificateError
    | .string "LoginAuthRequested" => .ok .loginAuthRequested
    | .string "UaChangeRequiresReload" => .ok .uaChangeRequiresReload
    | .string "BlockedByClient" => .ok .blockedByClient
    | .string "AudioOutputDeviceRequested" => .ok .audioOutputDeviceRequested
    | .string "MixedContent" => .ok .mixedContent
    | .string "TriggerBackgrounded" => .ok .triggerBackgrounded
    | .string "EmbedderTriggeredAndCrossOriginRedirected" => .ok .embedderTriggeredAndCrossOriginRedirected
    | .string "MemoryLimitExceeded" => .ok .memoryLimitExceeded
    | .string "FailToGetMemoryUsage" => .ok .failToGetMemoryUsage
    | .string "DataSaverEnabled" => .ok .dataSaverEnabled
    | .string "HasEffectiveUrl" => .ok .hasEffectiveUrl
    | .string "ActivatedBeforeStarted" => .ok .activatedBeforeStarted
    | .string "InactivePageRestriction" => .ok .inactivePageRestriction
    | .string "StartFailed" => .ok .startFailed
    | .string "TimeoutBackgrounded" => .ok .timeoutBackgrounded
    | v => .error s!"failed to parse Page.PrerenderFinalStatus: {repr v}"
instance : ToJSON Page.PrerenderFinalStatus where
  toJSON
    | .activated => .string "Activated"
    | .destroyed => .string "Destroyed"
    | .lowEndDevice => .string "LowEndDevice"
    | .crossOriginRedirect => .string "CrossOriginRedirect"
    | .crossOriginNavigation => .string "CrossOriginNavigation"
    | .invalidSchemeRedirect => .string "InvalidSchemeRedirect"
    | .invalidSchemeNavigation => .string "InvalidSchemeNavigation"
    | .inProgressNavigation => .string "InProgressNavigation"
    | .navigationRequestBlockedByCsp => .string "NavigationRequestBlockedByCsp"
    | .mainFrameNavigation => .string "MainFrameNavigation"
    | .mojoBinderPolicy => .string "MojoBinderPolicy"
    | .rendererProcessCrashed => .string "RendererProcessCrashed"
    | .rendererProcessKilled => .string "RendererProcessKilled"
    | .download => .string "Download"
    | .triggerDestroyed => .string "TriggerDestroyed"
    | .navigationNotCommitted => .string "NavigationNotCommitted"
    | .navigationBadHttpStatus => .string "NavigationBadHttpStatus"
    | .clientCertRequested => .string "ClientCertRequested"
    | .navigationRequestNetworkError => .string "NavigationRequestNetworkError"
    | .maxNumOfRunningPrerendersExceeded => .string "MaxNumOfRunningPrerendersExceeded"
    | .cancelAllHostsForTesting => .string "CancelAllHostsForTesting"
    | .didFailLoad => .string "DidFailLoad"
    | .stop => .string "Stop"
    | .sslCertificateError => .string "SslCertificateError"
    | .loginAuthRequested => .string "LoginAuthRequested"
    | .uaChangeRequiresReload => .string "UaChangeRequiresReload"
    | .blockedByClient => .string "BlockedByClient"
    | .audioOutputDeviceRequested => .string "AudioOutputDeviceRequested"
    | .mixedContent => .string "MixedContent"
    | .triggerBackgrounded => .string "TriggerBackgrounded"
    | .embedderTriggeredAndCrossOriginRedirected => .string "EmbedderTriggeredAndCrossOriginRedirected"
    | .memoryLimitExceeded => .string "MemoryLimitExceeded"
    | .failToGetMemoryUsage => .string "FailToGetMemoryUsage"
    | .dataSaverEnabled => .string "DataSaverEnabled"
    | .hasEffectiveUrl => .string "HasEffectiveUrl"
    | .activatedBeforeStarted => .string "ActivatedBeforeStarted"
    | .inactivePageRestriction => .string "InactivePageRestriction"
    | .startFailed => .string "StartFailed"
    | .timeoutBackgrounded => .string "TimeoutBackgrounded"

/-- `Page.DomContentEventFired`. -/
structure Page.DomContentEventFired where
  timestamp : Network.MonotonicTime
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.DomContentEventFired where
  parseJSON v := do
    .ok
      { timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }
instance : Event Page.DomContentEventFired where
  eventName := "Page.domContentEventFired"

/-- `Page.FileChooserOpenedMode`. -/
inductive Page.FileChooserOpenedMode where
  | selectSingle | selectMultiple
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FileChooserOpenedMode where
  parseJSON
    | .string "selectSingle" => .ok .selectSingle
    | .string "selectMultiple" => .ok .selectMultiple
    | v => .error s!"failed to parse Page.FileChooserOpenedMode: {repr v}"
instance : ToJSON Page.FileChooserOpenedMode where
  toJSON
    | .selectSingle => .string "selectSingle"
    | .selectMultiple => .string "selectMultiple"

/-- `Page.FileChooserOpened`. -/
structure Page.FileChooserOpened where
  frameId : Page.FrameId
  mode : Page.FileChooserOpenedMode
  backendNodeId : Option DOM.BackendNodeId := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FileChooserOpened where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        mode := ← Value.getField v "mode" >>= FromJSON.parseJSON
        backendNodeId := ← (← Value.getFieldOpt v "backendNodeId").mapM FromJSON.parseJSON }
instance : Event Page.FileChooserOpened where
  eventName := "Page.fileChooserOpened"

/-- `Page.FrameAttached`. -/
structure Page.FrameAttached where
  frameId : Page.FrameId
  parentFrameId : Page.FrameId
  stack : Option Runtime.StackTrace := none
  deriving Repr, BEq
instance : FromJSON Page.FrameAttached where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        parentFrameId := ← Value.getField v "parentFrameId" >>= FromJSON.parseJSON
        stack := ← (← Value.getFieldOpt v "stack").mapM FromJSON.parseJSON }
instance : Event Page.FrameAttached where
  eventName := "Page.frameAttached"

/-- `Page.FrameDetachedReason`. -/
inductive Page.FrameDetachedReason where
  | remove | swap
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FrameDetachedReason where
  parseJSON
    | .string "remove" => .ok .remove
    | .string "swap" => .ok .swap
    | v => .error s!"failed to parse Page.FrameDetachedReason: {repr v}"
instance : ToJSON Page.FrameDetachedReason where
  toJSON
    | .remove => .string "remove"
    | .swap => .string "swap"

/-- `Page.FrameDetached`. -/
structure Page.FrameDetached where
  frameId : Page.FrameId
  reason : Page.FrameDetachedReason
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FrameDetached where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        reason := ← Value.getField v "reason" >>= FromJSON.parseJSON }
instance : Event Page.FrameDetached where
  eventName := "Page.frameDetached"

/-- `Page.FrameNavigated`. -/
structure Page.FrameNavigated where
  frame : Page.Frame
  type : Page.NavigationType
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FrameNavigated where
  parseJSON v := do
    .ok
      { frame := ← Value.getField v "frame" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON }
instance : Event Page.FrameNavigated where
  eventName := "Page.frameNavigated"

/-- `Page.DocumentOpened`. -/
structure Page.DocumentOpened where
  frame : Page.Frame
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.DocumentOpened where
  parseJSON v := do
    .ok
      { frame := ← Value.getField v "frame" >>= FromJSON.parseJSON }
instance : Event Page.DocumentOpened where
  eventName := "Page.documentOpened"

/-- `Page.FrameResized`. -/
structure Page.FrameResized where
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FrameResized where parseJSON _ := .ok {}
instance : Event Page.FrameResized where
  eventName := "Page.frameResized"

/-- `Page.FrameRequestedNavigation`. -/
structure Page.FrameRequestedNavigation where
  frameId : Page.FrameId
  reason : Page.ClientNavigationReason
  url : String
  disposition : Page.ClientNavigationDisposition
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FrameRequestedNavigation where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        reason := ← Value.getField v "reason" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        disposition := ← Value.getField v "disposition" >>= FromJSON.parseJSON }
instance : Event Page.FrameRequestedNavigation where
  eventName := "Page.frameRequestedNavigation"

/-- `Page.FrameStartedLoading`. -/
structure Page.FrameStartedLoading where
  frameId : Page.FrameId
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FrameStartedLoading where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON }
instance : Event Page.FrameStartedLoading where
  eventName := "Page.frameStartedLoading"

/-- `Page.FrameStoppedLoading`. -/
structure Page.FrameStoppedLoading where
  frameId : Page.FrameId
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.FrameStoppedLoading where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON }
instance : Event Page.FrameStoppedLoading where
  eventName := "Page.frameStoppedLoading"

/-- `Page.InterstitialHidden`. -/
structure Page.InterstitialHidden where
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.InterstitialHidden where parseJSON _ := .ok {}
instance : Event Page.InterstitialHidden where
  eventName := "Page.interstitialHidden"

/-- `Page.InterstitialShown`. -/
structure Page.InterstitialShown where
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.InterstitialShown where parseJSON _ := .ok {}
instance : Event Page.InterstitialShown where
  eventName := "Page.interstitialShown"

/-- `Page.JavascriptDialogClosed`. -/
structure Page.JavascriptDialogClosed where
  result : Bool
  userInput : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.JavascriptDialogClosed where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON
        userInput := ← Value.getField v "userInput" >>= FromJSON.parseJSON }
instance : Event Page.JavascriptDialogClosed where
  eventName := "Page.javascriptDialogClosed"

/-- `Page.JavascriptDialogOpening`. -/
structure Page.JavascriptDialogOpening where
  url : String
  message : String
  type : Page.DialogType
  hasBrowserHandler : Bool
  defaultPrompt : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.JavascriptDialogOpening where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        message := ← Value.getField v "message" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        hasBrowserHandler := ← Value.getField v "hasBrowserHandler" >>= FromJSON.parseJSON
        defaultPrompt := ← (← Value.getFieldOpt v "defaultPrompt").mapM FromJSON.parseJSON }
instance : Event Page.JavascriptDialogOpening where
  eventName := "Page.javascriptDialogOpening"

/-- `Page.LifecycleEvent`. -/
structure Page.LifecycleEvent where
  frameId : Page.FrameId
  loaderId : Network.LoaderId
  name : String
  timestamp : Network.MonotonicTime
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.LifecycleEvent where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        loaderId := ← Value.getField v "loaderId" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }
instance : Event Page.LifecycleEvent where
  eventName := "Page.lifecycleEvent"

/-- `Page.BackForwardCacheNotUsed`. -/
structure Page.BackForwardCacheNotUsed where
  loaderId : Network.LoaderId
  frameId : Page.FrameId
  notRestoredExplanations : List Page.BackForwardCacheNotRestoredExplanation
  notRestoredExplanationsTree : Option Page.BackForwardCacheNotRestoredExplanationTree := none
  deriving Repr, BEq
instance : FromJSON Page.BackForwardCacheNotUsed where
  parseJSON v := do
    .ok
      { loaderId := ← Value.getField v "loaderId" >>= FromJSON.parseJSON
        frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        notRestoredExplanations := ← Value.getField v "notRestoredExplanations" >>= FromJSON.parseJSON
        notRestoredExplanationsTree := ← (← Value.getFieldOpt v "notRestoredExplanationsTree").mapM FromJSON.parseJSON }
instance : Event Page.BackForwardCacheNotUsed where
  eventName := "Page.backForwardCacheNotUsed"

/-- `Page.PrerenderAttemptCompleted`. -/
structure Page.PrerenderAttemptCompleted where
  initiatingFrameId : Page.FrameId
  prerenderingUrl : String
  finalStatus : Page.PrerenderFinalStatus
  disallowedApiMethod : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PrerenderAttemptCompleted where
  parseJSON v := do
    .ok
      { initiatingFrameId := ← Value.getField v "initiatingFrameId" >>= FromJSON.parseJSON
        prerenderingUrl := ← Value.getField v "prerenderingUrl" >>= FromJSON.parseJSON
        finalStatus := ← Value.getField v "finalStatus" >>= FromJSON.parseJSON
        disallowedApiMethod := ← (← Value.getFieldOpt v "disallowedApiMethod").mapM FromJSON.parseJSON }
instance : Event Page.PrerenderAttemptCompleted where
  eventName := "Page.prerenderAttemptCompleted"

/-- `Page.LoadEventFired`. -/
structure Page.LoadEventFired where
  timestamp : Network.MonotonicTime
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.LoadEventFired where
  parseJSON v := do
    .ok
      { timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }
instance : Event Page.LoadEventFired where
  eventName := "Page.loadEventFired"

/-- `Page.NavigatedWithinDocument`. -/
structure Page.NavigatedWithinDocument where
  frameId : Page.FrameId
  url : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.NavigatedWithinDocument where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON }
instance : Event Page.NavigatedWithinDocument where
  eventName := "Page.navigatedWithinDocument"

/-- `Page.ScreencastFrame`. -/
structure Page.ScreencastFrame where
  data : String
  metadata : Page.ScreencastFrameMetadata
  sessionId : Int
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.ScreencastFrame where
  parseJSON v := do
    .ok
      { data := ← Value.getField v "data" >>= FromJSON.parseJSON
        metadata := ← Value.getField v "metadata" >>= FromJSON.parseJSON
        sessionId := ← Value.getField v "sessionId" >>= FromJSON.parseJSON }
instance : Event Page.ScreencastFrame where
  eventName := "Page.screencastFrame"

/-- `Page.ScreencastVisibilityChanged`. -/
structure Page.ScreencastVisibilityChanged where
  visible : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.ScreencastVisibilityChanged where
  parseJSON v := do
    .ok
      { visible := ← Value.getField v "visible" >>= FromJSON.parseJSON }
instance : Event Page.ScreencastVisibilityChanged where
  eventName := "Page.screencastVisibilityChanged"

/-- `Page.WindowOpen`. -/
structure Page.WindowOpen where
  url : String
  windowName : String
  windowFeatures : List String
  userGesture : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.WindowOpen where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        windowName := ← Value.getField v "windowName" >>= FromJSON.parseJSON
        windowFeatures := ← Value.getField v "windowFeatures" >>= FromJSON.parseJSON
        userGesture := ← Value.getField v "userGesture" >>= FromJSON.parseJSON }
instance : Event Page.WindowOpen where
  eventName := "Page.windowOpen"

/-- `Page.CompilationCacheProduced`. -/
structure Page.CompilationCacheProduced where
  url : String
  data : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.CompilationCacheProduced where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        data := ← Value.getField v "data" >>= FromJSON.parseJSON }
instance : Event Page.CompilationCacheProduced where
  eventName := "Page.compilationCacheProduced"

/-- `Page.AddScriptToEvaluateOnNewDocument`. -/
structure Page.AddScriptToEvaluateOnNewDocument where
  identifier : Page.ScriptIdentifier
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.AddScriptToEvaluateOnNewDocument where
  parseJSON v := do
    .ok
      { identifier := ← Value.getField v "identifier" >>= FromJSON.parseJSON }

/-- `Page.PAddScriptToEvaluateOnNewDocument`. -/
structure Page.PAddScriptToEvaluateOnNewDocument where
  source : String
  worldName : Option String := none
  includeCommandLineAPI : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PAddScriptToEvaluateOnNewDocument where
  toJSON p := Data.Json.object <|
       [("source", ToJSON.toJSON p.source)]
    ++ (p.worldName.map (fun x => ("worldName", ToJSON.toJSON x))).toList
    ++ (p.includeCommandLineAPI.map (fun x => ("includeCommandLineAPI", ToJSON.toJSON x))).toList
instance : Command Page.PAddScriptToEvaluateOnNewDocument where
  Response := Page.AddScriptToEvaluateOnNewDocument
  commandName _ := "Page.addScriptToEvaluateOnNewDocument"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PBringToFront`. -/
structure Page.PBringToFront where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PBringToFront where toJSON _ := .null
instance : Command Page.PBringToFront where
  Response := Unit
  commandName _ := "Page.bringToFront"
  decodeResponse _ := .ok ()

/-- `Page.PCaptureScreenshotFormat`. -/
inductive Page.PCaptureScreenshotFormat where
  | jpeg | png | webp
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PCaptureScreenshotFormat where
  parseJSON
    | .string "jpeg" => .ok .jpeg
    | .string "png" => .ok .png
    | .string "webp" => .ok .webp
    | v => .error s!"failed to parse Page.PCaptureScreenshotFormat: {repr v}"
instance : ToJSON Page.PCaptureScreenshotFormat where
  toJSON
    | .jpeg => .string "jpeg"
    | .png => .string "png"
    | .webp => .string "webp"

/-- `Page.CaptureScreenshot`. -/
structure Page.CaptureScreenshot where
  data : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.CaptureScreenshot where
  parseJSON v := do
    .ok
      { data := ← Value.getField v "data" >>= FromJSON.parseJSON }

/-- `Page.PCaptureScreenshot`. -/
structure Page.PCaptureScreenshot where
  format : Option Page.PCaptureScreenshotFormat := none
  quality : Option Int := none
  clip : Option Page.Viewport := none
  fromSurface : Option Bool := none
  captureBeyondViewport : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PCaptureScreenshot where
  toJSON p := Data.Json.object <|
       (p.format.map (fun x => ("format", ToJSON.toJSON x))).toList
    ++ (p.quality.map (fun x => ("quality", ToJSON.toJSON x))).toList
    ++ (p.clip.map (fun x => ("clip", ToJSON.toJSON x))).toList
    ++ (p.fromSurface.map (fun x => ("fromSurface", ToJSON.toJSON x))).toList
    ++ (p.captureBeyondViewport.map (fun x => ("captureBeyondViewport", ToJSON.toJSON x))).toList
instance : Command Page.PCaptureScreenshot where
  Response := Page.CaptureScreenshot
  commandName _ := "Page.captureScreenshot"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PCaptureSnapshotFormat`. -/
inductive Page.PCaptureSnapshotFormat where
  | mhtml
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PCaptureSnapshotFormat where
  parseJSON
    | .string "mhtml" => .ok .mhtml
    | v => .error s!"failed to parse Page.PCaptureSnapshotFormat: {repr v}"
instance : ToJSON Page.PCaptureSnapshotFormat where
  toJSON
    | .mhtml => .string "mhtml"

/-- `Page.CaptureSnapshot`. -/
structure Page.CaptureSnapshot where
  data : String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.CaptureSnapshot where
  parseJSON v := do
    .ok
      { data := ← Value.getField v "data" >>= FromJSON.parseJSON }

/-- `Page.PCaptureSnapshot`. -/
structure Page.PCaptureSnapshot where
  format : Option Page.PCaptureSnapshotFormat := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PCaptureSnapshot where
  toJSON p := Data.Json.object <|
       (p.format.map (fun x => ("format", ToJSON.toJSON x))).toList
instance : Command Page.PCaptureSnapshot where
  Response := Page.CaptureSnapshot
  commandName _ := "Page.captureSnapshot"
  decodeResponse := FromJSON.parseJSON

/-- `Page.CreateIsolatedWorld`. -/
structure Page.CreateIsolatedWorld where
  executionContextId : Runtime.ExecutionContextId
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.CreateIsolatedWorld where
  parseJSON v := do
    .ok
      { executionContextId := ← Value.getField v "executionContextId" >>= FromJSON.parseJSON }

/-- `Page.PCreateIsolatedWorld`. -/
structure Page.PCreateIsolatedWorld where
  frameId : Page.FrameId
  worldName : Option String := none
  grantUniveralAccess : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PCreateIsolatedWorld where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
    ++ (p.worldName.map (fun x => ("worldName", ToJSON.toJSON x))).toList
    ++ (p.grantUniveralAccess.map (fun x => ("grantUniveralAccess", ToJSON.toJSON x))).toList
instance : Command Page.PCreateIsolatedWorld where
  Response := Page.CreateIsolatedWorld
  commandName _ := "Page.createIsolatedWorld"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PDisable`. -/
structure Page.PDisable where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PDisable where toJSON _ := .null
instance : Command Page.PDisable where
  Response := Unit
  commandName _ := "Page.disable"
  decodeResponse _ := .ok ()

/-- `Page.PEnable`. -/
structure Page.PEnable where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PEnable where toJSON _ := .null
instance : Command Page.PEnable where
  Response := Unit
  commandName _ := "Page.enable"
  decodeResponse _ := .ok ()

/-- `Page.GetAppManifest`. -/
structure Page.GetAppManifest where
  url : String
  errors : List Page.AppManifestError
  data : Option String := none
  parsed : Option Page.AppManifestParsedProperties := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetAppManifest where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        errors := ← Value.getField v "errors" >>= FromJSON.parseJSON
        data := ← (← Value.getFieldOpt v "data").mapM FromJSON.parseJSON
        parsed := ← (← Value.getFieldOpt v "parsed").mapM FromJSON.parseJSON }

/-- `Page.PGetAppManifest`. -/
structure Page.PGetAppManifest where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetAppManifest where toJSON _ := .null
instance : Command Page.PGetAppManifest where
  Response := Page.GetAppManifest
  commandName _ := "Page.getAppManifest"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetInstallabilityErrors`. -/
structure Page.GetInstallabilityErrors where
  installabilityErrors : List Page.InstallabilityError
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetInstallabilityErrors where
  parseJSON v := do
    .ok
      { installabilityErrors := ← Value.getField v "installabilityErrors" >>= FromJSON.parseJSON }

/-- `Page.PGetInstallabilityErrors`. -/
structure Page.PGetInstallabilityErrors where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetInstallabilityErrors where toJSON _ := .null
instance : Command Page.PGetInstallabilityErrors where
  Response := Page.GetInstallabilityErrors
  commandName _ := "Page.getInstallabilityErrors"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetManifestIcons`. -/
structure Page.GetManifestIcons where
  primaryIcon : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetManifestIcons where
  parseJSON v := do
    .ok
      { primaryIcon := ← (← Value.getFieldOpt v "primaryIcon").mapM FromJSON.parseJSON }

/-- `Page.PGetManifestIcons`. -/
structure Page.PGetManifestIcons where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetManifestIcons where toJSON _ := .null
instance : Command Page.PGetManifestIcons where
  Response := Page.GetManifestIcons
  commandName _ := "Page.getManifestIcons"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetAppId`. -/
structure Page.GetAppId where
  appId : Option String := none
  recommendedId : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetAppId where
  parseJSON v := do
    .ok
      { appId := ← (← Value.getFieldOpt v "appId").mapM FromJSON.parseJSON
        recommendedId := ← (← Value.getFieldOpt v "recommendedId").mapM FromJSON.parseJSON }

/-- `Page.PGetAppId`. -/
structure Page.PGetAppId where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetAppId where toJSON _ := .null
instance : Command Page.PGetAppId where
  Response := Page.GetAppId
  commandName _ := "Page.getAppId"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetAdScriptId`. -/
structure Page.GetAdScriptId where
  adScriptId : Option Page.AdScriptId := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetAdScriptId where
  parseJSON v := do
    .ok
      { adScriptId := ← (← Value.getFieldOpt v "adScriptId").mapM FromJSON.parseJSON }

/-- `Page.PGetAdScriptId`. -/
structure Page.PGetAdScriptId where
  frameId : Page.FrameId
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetAdScriptId where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
instance : Command Page.PGetAdScriptId where
  Response := Page.GetAdScriptId
  commandName _ := "Page.getAdScriptId"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetFrameTree`. -/
structure Page.GetFrameTree where
  frameTree : Page.FrameTree
  deriving Repr, BEq
instance : FromJSON Page.GetFrameTree where
  parseJSON v := do
    .ok
      { frameTree := ← Value.getField v "frameTree" >>= FromJSON.parseJSON }

/-- `Page.PGetFrameTree`. -/
structure Page.PGetFrameTree where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetFrameTree where toJSON _ := .null
instance : Command Page.PGetFrameTree where
  Response := Page.GetFrameTree
  commandName _ := "Page.getFrameTree"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetLayoutMetrics`. -/
structure Page.GetLayoutMetrics where
  cssLayoutViewport : Page.LayoutViewport
  cssVisualViewport : Page.VisualViewport
  cssContentSize : DOM.Rect
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetLayoutMetrics where
  parseJSON v := do
    .ok
      { cssLayoutViewport := ← Value.getField v "cssLayoutViewport" >>= FromJSON.parseJSON
        cssVisualViewport := ← Value.getField v "cssVisualViewport" >>= FromJSON.parseJSON
        cssContentSize := ← Value.getField v "cssContentSize" >>= FromJSON.parseJSON }

/-- `Page.PGetLayoutMetrics`. -/
structure Page.PGetLayoutMetrics where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetLayoutMetrics where toJSON _ := .null
instance : Command Page.PGetLayoutMetrics where
  Response := Page.GetLayoutMetrics
  commandName _ := "Page.getLayoutMetrics"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetNavigationHistory`. -/
structure Page.GetNavigationHistory where
  currentIndex : Int
  entries : List Page.NavigationEntry
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetNavigationHistory where
  parseJSON v := do
    .ok
      { currentIndex := ← Value.getField v "currentIndex" >>= FromJSON.parseJSON
        entries := ← Value.getField v "entries" >>= FromJSON.parseJSON }

/-- `Page.PGetNavigationHistory`. -/
structure Page.PGetNavigationHistory where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetNavigationHistory where toJSON _ := .null
instance : Command Page.PGetNavigationHistory where
  Response := Page.GetNavigationHistory
  commandName _ := "Page.getNavigationHistory"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PResetNavigationHistory`. -/
structure Page.PResetNavigationHistory where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PResetNavigationHistory where toJSON _ := .null
instance : Command Page.PResetNavigationHistory where
  Response := Unit
  commandName _ := "Page.resetNavigationHistory"
  decodeResponse _ := .ok ()

/-- `Page.GetResourceContent`. -/
structure Page.GetResourceContent where
  content : String
  base64Encoded : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetResourceContent where
  parseJSON v := do
    .ok
      { content := ← Value.getField v "content" >>= FromJSON.parseJSON
        base64Encoded := ← Value.getField v "base64Encoded" >>= FromJSON.parseJSON }

/-- `Page.PGetResourceContent`. -/
structure Page.PGetResourceContent where
  frameId : Page.FrameId
  url : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetResourceContent where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
    ++ [("url", ToJSON.toJSON p.url)]
instance : Command Page.PGetResourceContent where
  Response := Page.GetResourceContent
  commandName _ := "Page.getResourceContent"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetResourceTree`. -/
structure Page.GetResourceTree where
  frameTree : Page.FrameResourceTree
  deriving Repr, BEq
instance : FromJSON Page.GetResourceTree where
  parseJSON v := do
    .ok
      { frameTree := ← Value.getField v "frameTree" >>= FromJSON.parseJSON }

/-- `Page.PGetResourceTree`. -/
structure Page.PGetResourceTree where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetResourceTree where toJSON _ := .null
instance : Command Page.PGetResourceTree where
  Response := Page.GetResourceTree
  commandName _ := "Page.getResourceTree"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PHandleJavaScriptDialog`. -/
structure Page.PHandleJavaScriptDialog where
  accept : Bool
  promptText : Option String := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PHandleJavaScriptDialog where
  toJSON p := Data.Json.object <|
       [("accept", ToJSON.toJSON p.accept)]
    ++ (p.promptText.map (fun x => ("promptText", ToJSON.toJSON x))).toList
instance : Command Page.PHandleJavaScriptDialog where
  Response := Unit
  commandName _ := "Page.handleJavaScriptDialog"
  decodeResponse _ := .ok ()

/-- `Page.Navigate`. -/
structure Page.Navigate where
  frameId : Page.FrameId
  loaderId : Option Network.LoaderId := none
  errorText : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.Navigate where
  parseJSON v := do
    .ok
      { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        loaderId := ← (← Value.getFieldOpt v "loaderId").mapM FromJSON.parseJSON
        errorText := ← (← Value.getFieldOpt v "errorText").mapM FromJSON.parseJSON }

/-- `Page.PNavigate`. -/
structure Page.PNavigate where
  url : String
  referrer : Option String := none
  transitionType : Option Page.TransitionType := none
  frameId : Option Page.FrameId := none
  referrerPolicy : Option Page.ReferrerPolicy := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PNavigate where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ (p.referrer.map (fun x => ("referrer", ToJSON.toJSON x))).toList
    ++ (p.transitionType.map (fun x => ("transitionType", ToJSON.toJSON x))).toList
    ++ (p.frameId.map (fun x => ("frameId", ToJSON.toJSON x))).toList
    ++ (p.referrerPolicy.map (fun x => ("referrerPolicy", ToJSON.toJSON x))).toList
instance : Command Page.PNavigate where
  Response := Page.Navigate
  commandName _ := "Page.navigate"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PNavigateToHistoryEntry`. -/
structure Page.PNavigateToHistoryEntry where
  entryId : Int
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PNavigateToHistoryEntry where
  toJSON p := Data.Json.object <|
       [("entryId", ToJSON.toJSON p.entryId)]
instance : Command Page.PNavigateToHistoryEntry where
  Response := Unit
  commandName _ := "Page.navigateToHistoryEntry"
  decodeResponse _ := .ok ()

/-- `Page.PPrintToPDFTransferMode`. -/
inductive Page.PPrintToPDFTransferMode where
  | returnAsBase64 | returnAsStream
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PPrintToPDFTransferMode where
  parseJSON
    | .string "ReturnAsBase64" => .ok .returnAsBase64
    | .string "ReturnAsStream" => .ok .returnAsStream
    | v => .error s!"failed to parse Page.PPrintToPDFTransferMode: {repr v}"
instance : ToJSON Page.PPrintToPDFTransferMode where
  toJSON
    | .returnAsBase64 => .string "ReturnAsBase64"
    | .returnAsStream => .string "ReturnAsStream"

/-- `Page.PrintToPDF`. -/
structure Page.PrintToPDF where
  data : String
  stream : Option CDP.Domains.IO.StreamHandle := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PrintToPDF where
  parseJSON v := do
    .ok
      { data := ← Value.getField v "data" >>= FromJSON.parseJSON
        stream := ← (← Value.getFieldOpt v "stream").mapM FromJSON.parseJSON }

/-- `Page.PPrintToPDF`. -/
structure Page.PPrintToPDF where
  landscape : Option Bool := none
  displayHeaderFooter : Option Bool := none
  printBackground : Option Bool := none
  scale : Option Float := none
  paperWidth : Option Float := none
  paperHeight : Option Float := none
  marginTop : Option Float := none
  marginBottom : Option Float := none
  marginLeft : Option Float := none
  marginRight : Option Float := none
  pageRanges : Option String := none
  headerTemplate : Option String := none
  footerTemplate : Option String := none
  preferCSSPageSize : Option Bool := none
  transferMode : Option Page.PPrintToPDFTransferMode := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PPrintToPDF where
  toJSON p := Data.Json.object <|
       (p.landscape.map (fun x => ("landscape", ToJSON.toJSON x))).toList
    ++ (p.displayHeaderFooter.map (fun x => ("displayHeaderFooter", ToJSON.toJSON x))).toList
    ++ (p.printBackground.map (fun x => ("printBackground", ToJSON.toJSON x))).toList
    ++ (p.scale.map (fun x => ("scale", ToJSON.toJSON x))).toList
    ++ (p.paperWidth.map (fun x => ("paperWidth", ToJSON.toJSON x))).toList
    ++ (p.paperHeight.map (fun x => ("paperHeight", ToJSON.toJSON x))).toList
    ++ (p.marginTop.map (fun x => ("marginTop", ToJSON.toJSON x))).toList
    ++ (p.marginBottom.map (fun x => ("marginBottom", ToJSON.toJSON x))).toList
    ++ (p.marginLeft.map (fun x => ("marginLeft", ToJSON.toJSON x))).toList
    ++ (p.marginRight.map (fun x => ("marginRight", ToJSON.toJSON x))).toList
    ++ (p.pageRanges.map (fun x => ("pageRanges", ToJSON.toJSON x))).toList
    ++ (p.headerTemplate.map (fun x => ("headerTemplate", ToJSON.toJSON x))).toList
    ++ (p.footerTemplate.map (fun x => ("footerTemplate", ToJSON.toJSON x))).toList
    ++ (p.preferCSSPageSize.map (fun x => ("preferCSSPageSize", ToJSON.toJSON x))).toList
    ++ (p.transferMode.map (fun x => ("transferMode", ToJSON.toJSON x))).toList
instance : Command Page.PPrintToPDF where
  Response := Page.PrintToPDF
  commandName _ := "Page.printToPDF"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PReload`. -/
structure Page.PReload where
  ignoreCache : Option Bool := none
  scriptToEvaluateOnLoad : Option String := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PReload where
  toJSON p := Data.Json.object <|
       (p.ignoreCache.map (fun x => ("ignoreCache", ToJSON.toJSON x))).toList
    ++ (p.scriptToEvaluateOnLoad.map (fun x => ("scriptToEvaluateOnLoad", ToJSON.toJSON x))).toList
instance : Command Page.PReload where
  Response := Unit
  commandName _ := "Page.reload"
  decodeResponse _ := .ok ()

/-- `Page.PRemoveScriptToEvaluateOnNewDocument`. -/
structure Page.PRemoveScriptToEvaluateOnNewDocument where
  identifier : Page.ScriptIdentifier
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PRemoveScriptToEvaluateOnNewDocument where
  toJSON p := Data.Json.object <|
       [("identifier", ToJSON.toJSON p.identifier)]
instance : Command Page.PRemoveScriptToEvaluateOnNewDocument where
  Response := Unit
  commandName _ := "Page.removeScriptToEvaluateOnNewDocument"
  decodeResponse _ := .ok ()

/-- `Page.PScreencastFrameAck`. -/
structure Page.PScreencastFrameAck where
  sessionId : Int
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PScreencastFrameAck where
  toJSON p := Data.Json.object <|
       [("sessionId", ToJSON.toJSON p.sessionId)]
instance : Command Page.PScreencastFrameAck where
  Response := Unit
  commandName _ := "Page.screencastFrameAck"
  decodeResponse _ := .ok ()

/-- `Page.SearchInResource`. -/
structure Page.SearchInResource where
  result : List Debugger.SearchMatch
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.SearchInResource where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON }

/-- `Page.PSearchInResource`. -/
structure Page.PSearchInResource where
  frameId : Page.FrameId
  url : String
  query : String
  caseSensitive : Option Bool := none
  isRegex : Option Bool := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSearchInResource where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
    ++ [("url", ToJSON.toJSON p.url)]
    ++ [("query", ToJSON.toJSON p.query)]
    ++ (p.caseSensitive.map (fun x => ("caseSensitive", ToJSON.toJSON x))).toList
    ++ (p.isRegex.map (fun x => ("isRegex", ToJSON.toJSON x))).toList
instance : Command Page.PSearchInResource where
  Response := Page.SearchInResource
  commandName _ := "Page.searchInResource"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PSetAdBlockingEnabled`. -/
structure Page.PSetAdBlockingEnabled where
  enabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetAdBlockingEnabled where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
instance : Command Page.PSetAdBlockingEnabled where
  Response := Unit
  commandName _ := "Page.setAdBlockingEnabled"
  decodeResponse _ := .ok ()

/-- `Page.PSetBypassCSP`. -/
structure Page.PSetBypassCSP where
  enabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetBypassCSP where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
instance : Command Page.PSetBypassCSP where
  Response := Unit
  commandName _ := "Page.setBypassCSP"
  decodeResponse _ := .ok ()

/-- `Page.GetPermissionsPolicyState`. -/
structure Page.GetPermissionsPolicyState where
  states : List Page.PermissionsPolicyFeatureState
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetPermissionsPolicyState where
  parseJSON v := do
    .ok
      { states := ← Value.getField v "states" >>= FromJSON.parseJSON }

/-- `Page.PGetPermissionsPolicyState`. -/
structure Page.PGetPermissionsPolicyState where
  frameId : Page.FrameId
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetPermissionsPolicyState where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
instance : Command Page.PGetPermissionsPolicyState where
  Response := Page.GetPermissionsPolicyState
  commandName _ := "Page.getPermissionsPolicyState"
  decodeResponse := FromJSON.parseJSON

/-- `Page.GetOriginTrials`. -/
structure Page.GetOriginTrials where
  originTrials : List Page.OriginTrial
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.GetOriginTrials where
  parseJSON v := do
    .ok
      { originTrials := ← Value.getField v "originTrials" >>= FromJSON.parseJSON }

/-- `Page.PGetOriginTrials`. -/
structure Page.PGetOriginTrials where
  frameId : Page.FrameId
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGetOriginTrials where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
instance : Command Page.PGetOriginTrials where
  Response := Page.GetOriginTrials
  commandName _ := "Page.getOriginTrials"
  decodeResponse := FromJSON.parseJSON

/-- `Page.PSetFontFamilies`. -/
structure Page.PSetFontFamilies where
  fontFamilies : Page.FontFamilies
  forScripts : Option (List Page.ScriptFontFamilies) := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetFontFamilies where
  toJSON p := Data.Json.object <|
       [("fontFamilies", ToJSON.toJSON p.fontFamilies)]
    ++ (p.forScripts.map (fun x => ("forScripts", ToJSON.toJSON x))).toList
instance : Command Page.PSetFontFamilies where
  Response := Unit
  commandName _ := "Page.setFontFamilies"
  decodeResponse _ := .ok ()

/-- `Page.PSetFontSizes`. -/
structure Page.PSetFontSizes where
  fontSizes : Page.FontSizes
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetFontSizes where
  toJSON p := Data.Json.object <|
       [("fontSizes", ToJSON.toJSON p.fontSizes)]
instance : Command Page.PSetFontSizes where
  Response := Unit
  commandName _ := "Page.setFontSizes"
  decodeResponse _ := .ok ()

/-- `Page.PSetDocumentContent`. -/
structure Page.PSetDocumentContent where
  frameId : Page.FrameId
  html : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetDocumentContent where
  toJSON p := Data.Json.object <|
       [("frameId", ToJSON.toJSON p.frameId)]
    ++ [("html", ToJSON.toJSON p.html)]
instance : Command Page.PSetDocumentContent where
  Response := Unit
  commandName _ := "Page.setDocumentContent"
  decodeResponse _ := .ok ()

/-- `Page.PSetLifecycleEventsEnabled`. -/
structure Page.PSetLifecycleEventsEnabled where
  enabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetLifecycleEventsEnabled where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
instance : Command Page.PSetLifecycleEventsEnabled where
  Response := Unit
  commandName _ := "Page.setLifecycleEventsEnabled"
  decodeResponse _ := .ok ()

/-- `Page.PStartScreencastFormat`. -/
inductive Page.PStartScreencastFormat where
  | jpeg | png
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PStartScreencastFormat where
  parseJSON
    | .string "jpeg" => .ok .jpeg
    | .string "png" => .ok .png
    | v => .error s!"failed to parse Page.PStartScreencastFormat: {repr v}"
instance : ToJSON Page.PStartScreencastFormat where
  toJSON
    | .jpeg => .string "jpeg"
    | .png => .string "png"

/-- `Page.PStartScreencast`. -/
structure Page.PStartScreencast where
  format : Option Page.PStartScreencastFormat := none
  quality : Option Int := none
  maxWidth : Option Int := none
  maxHeight : Option Int := none
  everyNthFrame : Option Int := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PStartScreencast where
  toJSON p := Data.Json.object <|
       (p.format.map (fun x => ("format", ToJSON.toJSON x))).toList
    ++ (p.quality.map (fun x => ("quality", ToJSON.toJSON x))).toList
    ++ (p.maxWidth.map (fun x => ("maxWidth", ToJSON.toJSON x))).toList
    ++ (p.maxHeight.map (fun x => ("maxHeight", ToJSON.toJSON x))).toList
    ++ (p.everyNthFrame.map (fun x => ("everyNthFrame", ToJSON.toJSON x))).toList
instance : Command Page.PStartScreencast where
  Response := Unit
  commandName _ := "Page.startScreencast"
  decodeResponse _ := .ok ()

/-- `Page.PStopLoading`. -/
structure Page.PStopLoading where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PStopLoading where toJSON _ := .null
instance : Command Page.PStopLoading where
  Response := Unit
  commandName _ := "Page.stopLoading"
  decodeResponse _ := .ok ()

/-- `Page.PCrash`. -/
structure Page.PCrash where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PCrash where toJSON _ := .null
instance : Command Page.PCrash where
  Response := Unit
  commandName _ := "Page.crash"
  decodeResponse _ := .ok ()

/-- `Page.PClose`. -/
structure Page.PClose where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PClose where toJSON _ := .null
instance : Command Page.PClose where
  Response := Unit
  commandName _ := "Page.close"
  decodeResponse _ := .ok ()

/-- `Page.PSetWebLifecycleStateState`. -/
inductive Page.PSetWebLifecycleStateState where
  | frozen | active
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PSetWebLifecycleStateState where
  parseJSON
    | .string "frozen" => .ok .frozen
    | .string "active" => .ok .active
    | v => .error s!"failed to parse Page.PSetWebLifecycleStateState: {repr v}"
instance : ToJSON Page.PSetWebLifecycleStateState where
  toJSON
    | .frozen => .string "frozen"
    | .active => .string "active"

/-- `Page.PSetWebLifecycleState`. -/
structure Page.PSetWebLifecycleState where
  state : Page.PSetWebLifecycleStateState
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetWebLifecycleState where
  toJSON p := Data.Json.object <|
       [("state", ToJSON.toJSON p.state)]
instance : Command Page.PSetWebLifecycleState where
  Response := Unit
  commandName _ := "Page.setWebLifecycleState"
  decodeResponse _ := .ok ()

/-- `Page.PStopScreencast`. -/
structure Page.PStopScreencast where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PStopScreencast where toJSON _ := .null
instance : Command Page.PStopScreencast where
  Response := Unit
  commandName _ := "Page.stopScreencast"
  decodeResponse _ := .ok ()

/-- `Page.PProduceCompilationCache`. -/
structure Page.PProduceCompilationCache where
  scripts : List Page.CompilationCacheParams
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PProduceCompilationCache where
  toJSON p := Data.Json.object <|
       [("scripts", ToJSON.toJSON p.scripts)]
instance : Command Page.PProduceCompilationCache where
  Response := Unit
  commandName _ := "Page.produceCompilationCache"
  decodeResponse _ := .ok ()

/-- `Page.PAddCompilationCache`. -/
structure Page.PAddCompilationCache where
  url : String
  data : String
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PAddCompilationCache where
  toJSON p := Data.Json.object <|
       [("url", ToJSON.toJSON p.url)]
    ++ [("data", ToJSON.toJSON p.data)]
instance : Command Page.PAddCompilationCache where
  Response := Unit
  commandName _ := "Page.addCompilationCache"
  decodeResponse _ := .ok ()

/-- `Page.PClearCompilationCache`. -/
structure Page.PClearCompilationCache where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PClearCompilationCache where toJSON _ := .null
instance : Command Page.PClearCompilationCache where
  Response := Unit
  commandName _ := "Page.clearCompilationCache"
  decodeResponse _ := .ok ()

/-- `Page.PSetSPCTransactionModeMode`. -/
inductive Page.PSetSPCTransactionModeMode where
  | none | autoaccept | autoreject
  deriving Repr, BEq, DecidableEq
instance : FromJSON Page.PSetSPCTransactionModeMode where
  parseJSON
    | .string "none" => .ok .none
    | .string "autoaccept" => .ok .autoaccept
    | .string "autoreject" => .ok .autoreject
    | v => .error s!"failed to parse Page.PSetSPCTransactionModeMode: {repr v}"
instance : ToJSON Page.PSetSPCTransactionModeMode where
  toJSON
    | .none => .string "none"
    | .autoaccept => .string "autoaccept"
    | .autoreject => .string "autoreject"

/-- `Page.PSetSPCTransactionMode`. -/
structure Page.PSetSPCTransactionMode where
  mode : Page.PSetSPCTransactionModeMode
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetSPCTransactionMode where
  toJSON p := Data.Json.object <|
       [("mode", ToJSON.toJSON p.mode)]
instance : Command Page.PSetSPCTransactionMode where
  Response := Unit
  commandName _ := "Page.setSPCTransactionMode"
  decodeResponse _ := .ok ()

/-- `Page.PGenerateTestReport`. -/
structure Page.PGenerateTestReport where
  message : String
  group : Option String := none
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PGenerateTestReport where
  toJSON p := Data.Json.object <|
       [("message", ToJSON.toJSON p.message)]
    ++ (p.group.map (fun x => ("group", ToJSON.toJSON x))).toList
instance : Command Page.PGenerateTestReport where
  Response := Unit
  commandName _ := "Page.generateTestReport"
  decodeResponse _ := .ok ()

/-- `Page.PWaitForDebugger`. -/
structure Page.PWaitForDebugger where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PWaitForDebugger where toJSON _ := .null
instance : Command Page.PWaitForDebugger where
  Response := Unit
  commandName _ := "Page.waitForDebugger"
  decodeResponse _ := .ok ()

/-- `Page.PSetInterceptFileChooserDialog`. -/
structure Page.PSetInterceptFileChooserDialog where
  enabled : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Page.PSetInterceptFileChooserDialog where
  toJSON p := Data.Json.object <|
       [("enabled", ToJSON.toJSON p.enabled)]
instance : Command Page.PSetInterceptFileChooserDialog where
  Response := Unit
  commandName _ := "Page.setInterceptFileChooserDialog"
  decodeResponse _ := .ok ()

/-- `Security.CertificateSecurityState`. -/
structure Security.CertificateSecurityState where
  protocol : String
  keyExchange : String
  keyExchangeGroup : Option String := none
  cipher : String
  mac : Option String := none
  certificate : List String
  subjectName : String
  issuer : String
  validFrom : Network.TimeSinceEpoch
  validTo : Network.TimeSinceEpoch
  certificateNetworkError : Option String := none
  certificateHasWeakSignature : Bool
  certificateHasSha1Signature : Bool
  modernSSL : Bool
  obsoleteSslProtocol : Bool
  obsoleteSslKeyExchange : Bool
  obsoleteSslCipher : Bool
  obsoleteSslSignature : Bool
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.CertificateSecurityState where
  parseJSON v := do
    .ok
      { protocol := ← Value.getField v "protocol" >>= FromJSON.parseJSON
        keyExchange := ← Value.getField v "keyExchange" >>= FromJSON.parseJSON
        keyExchangeGroup := ← (← Value.getFieldOpt v "keyExchangeGroup").mapM FromJSON.parseJSON
        cipher := ← Value.getField v "cipher" >>= FromJSON.parseJSON
        mac := ← (← Value.getFieldOpt v "mac").mapM FromJSON.parseJSON
        certificate := ← Value.getField v "certificate" >>= FromJSON.parseJSON
        subjectName := ← Value.getField v "subjectName" >>= FromJSON.parseJSON
        issuer := ← Value.getField v "issuer" >>= FromJSON.parseJSON
        validFrom := ← Value.getField v "validFrom" >>= FromJSON.parseJSON
        validTo := ← Value.getField v "validTo" >>= FromJSON.parseJSON
        certificateNetworkError := ← (← Value.getFieldOpt v "certificateNetworkError").mapM FromJSON.parseJSON
        certificateHasWeakSignature := ← Value.getField v "certificateHasWeakSignature" >>= FromJSON.parseJSON
        certificateHasSha1Signature := ← Value.getField v "certificateHasSha1Signature" >>= FromJSON.parseJSON
        modernSSL := ← Value.getField v "modernSSL" >>= FromJSON.parseJSON
        obsoleteSslProtocol := ← Value.getField v "obsoleteSslProtocol" >>= FromJSON.parseJSON
        obsoleteSslKeyExchange := ← Value.getField v "obsoleteSslKeyExchange" >>= FromJSON.parseJSON
        obsoleteSslCipher := ← Value.getField v "obsoleteSslCipher" >>= FromJSON.parseJSON
        obsoleteSslSignature := ← Value.getField v "obsoleteSslSignature" >>= FromJSON.parseJSON }
instance : ToJSON Security.CertificateSecurityState where
  toJSON p := Data.Json.object <|
       [("protocol", ToJSON.toJSON p.protocol)]
    ++ [("keyExchange", ToJSON.toJSON p.keyExchange)]
    ++ (p.keyExchangeGroup.map (fun x => ("keyExchangeGroup", ToJSON.toJSON x))).toList
    ++ [("cipher", ToJSON.toJSON p.cipher)]
    ++ (p.mac.map (fun x => ("mac", ToJSON.toJSON x))).toList
    ++ [("certificate", ToJSON.toJSON p.certificate)]
    ++ [("subjectName", ToJSON.toJSON p.subjectName)]
    ++ [("issuer", ToJSON.toJSON p.issuer)]
    ++ [("validFrom", ToJSON.toJSON p.validFrom)]
    ++ [("validTo", ToJSON.toJSON p.validTo)]
    ++ (p.certificateNetworkError.map (fun x => ("certificateNetworkError", ToJSON.toJSON x))).toList
    ++ [("certificateHasWeakSignature", ToJSON.toJSON p.certificateHasWeakSignature)]
    ++ [("certificateHasSha1Signature", ToJSON.toJSON p.certificateHasSha1Signature)]
    ++ [("modernSSL", ToJSON.toJSON p.modernSSL)]
    ++ [("obsoleteSslProtocol", ToJSON.toJSON p.obsoleteSslProtocol)]
    ++ [("obsoleteSslKeyExchange", ToJSON.toJSON p.obsoleteSslKeyExchange)]
    ++ [("obsoleteSslCipher", ToJSON.toJSON p.obsoleteSslCipher)]
    ++ [("obsoleteSslSignature", ToJSON.toJSON p.obsoleteSslSignature)]

/-- `Security.SafetyTipStatus`. -/
inductive Security.SafetyTipStatus where
  | badReputation | lookalike
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.SafetyTipStatus where
  parseJSON
    | .string "badReputation" => .ok .badReputation
    | .string "lookalike" => .ok .lookalike
    | v => .error s!"failed to parse Security.SafetyTipStatus: {repr v}"
instance : ToJSON Security.SafetyTipStatus where
  toJSON
    | .badReputation => .string "badReputation"
    | .lookalike => .string "lookalike"

/-- `Security.SafetyTipInfo`. -/
structure Security.SafetyTipInfo where
  safetyTipStatus : Security.SafetyTipStatus
  safeUrl : Option String := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.SafetyTipInfo where
  parseJSON v := do
    .ok
      { safetyTipStatus := ← Value.getField v "safetyTipStatus" >>= FromJSON.parseJSON
        safeUrl := ← (← Value.getFieldOpt v "safeUrl").mapM FromJSON.parseJSON }
instance : ToJSON Security.SafetyTipInfo where
  toJSON p := Data.Json.object <|
       [("safetyTipStatus", ToJSON.toJSON p.safetyTipStatus)]
    ++ (p.safeUrl.map (fun x => ("safeUrl", ToJSON.toJSON x))).toList

/-- `Security.VisibleSecurityState`. -/
structure Security.VisibleSecurityState where
  securityState : Security.SecurityState
  certificateSecurityState : Option Security.CertificateSecurityState := none
  safetyTipInfo : Option Security.SafetyTipInfo := none
  securityStateIssueIds : List String
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.VisibleSecurityState where
  parseJSON v := do
    .ok
      { securityState := ← Value.getField v "securityState" >>= FromJSON.parseJSON
        certificateSecurityState := ← (← Value.getFieldOpt v "certificateSecurityState").mapM FromJSON.parseJSON
        safetyTipInfo := ← (← Value.getFieldOpt v "safetyTipInfo").mapM FromJSON.parseJSON
        securityStateIssueIds := ← Value.getField v "securityStateIssueIds" >>= FromJSON.parseJSON }
instance : ToJSON Security.VisibleSecurityState where
  toJSON p := Data.Json.object <|
       [("securityState", ToJSON.toJSON p.securityState)]
    ++ (p.certificateSecurityState.map (fun x => ("certificateSecurityState", ToJSON.toJSON x))).toList
    ++ (p.safetyTipInfo.map (fun x => ("safetyTipInfo", ToJSON.toJSON x))).toList
    ++ [("securityStateIssueIds", ToJSON.toJSON p.securityStateIssueIds)]

/-- `Security.SecurityStateExplanation`. -/
structure Security.SecurityStateExplanation where
  securityState : Security.SecurityState
  title : String
  summary : String
  description : String
  mixedContentType : Security.MixedContentType
  certificate : List String
  recommendations : Option (List String) := none
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.SecurityStateExplanation where
  parseJSON v := do
    .ok
      { securityState := ← Value.getField v "securityState" >>= FromJSON.parseJSON
        title := ← Value.getField v "title" >>= FromJSON.parseJSON
        summary := ← Value.getField v "summary" >>= FromJSON.parseJSON
        description := ← Value.getField v "description" >>= FromJSON.parseJSON
        mixedContentType := ← Value.getField v "mixedContentType" >>= FromJSON.parseJSON
        certificate := ← Value.getField v "certificate" >>= FromJSON.parseJSON
        recommendations := ← (← Value.getFieldOpt v "recommendations").mapM FromJSON.parseJSON }
instance : ToJSON Security.SecurityStateExplanation where
  toJSON p := Data.Json.object <|
       [("securityState", ToJSON.toJSON p.securityState)]
    ++ [("title", ToJSON.toJSON p.title)]
    ++ [("summary", ToJSON.toJSON p.summary)]
    ++ [("description", ToJSON.toJSON p.description)]
    ++ [("mixedContentType", ToJSON.toJSON p.mixedContentType)]
    ++ [("certificate", ToJSON.toJSON p.certificate)]
    ++ (p.recommendations.map (fun x => ("recommendations", ToJSON.toJSON x))).toList

/-- `Security.CertificateErrorAction`. -/
inductive Security.CertificateErrorAction where
  | continue | cancel
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.CertificateErrorAction where
  parseJSON
    | .string "continue" => .ok .continue
    | .string "cancel" => .ok .cancel
    | v => .error s!"failed to parse Security.CertificateErrorAction: {repr v}"
instance : ToJSON Security.CertificateErrorAction where
  toJSON
    | .continue => .string "continue"
    | .cancel => .string "cancel"

/-- `Security.VisibleSecurityStateChanged`. -/
structure Security.VisibleSecurityStateChanged where
  visibleSecurityState : Security.VisibleSecurityState
  deriving Repr, BEq, DecidableEq
instance : FromJSON Security.VisibleSecurityStateChanged where
  parseJSON v := do
    .ok
      { visibleSecurityState := ← Value.getField v "visibleSecurityState" >>= FromJSON.parseJSON }
instance : Event Security.VisibleSecurityStateChanged where
  eventName := "Security.visibleSecurityStateChanged"

/-- `Security.PDisable`. -/
structure Security.PDisable where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Security.PDisable where toJSON _ := .null
instance : Command Security.PDisable where
  Response := Unit
  commandName _ := "Security.disable"
  decodeResponse _ := .ok ()

/-- `Security.PEnable`. -/
structure Security.PEnable where
  deriving Repr, BEq, DecidableEq
instance : ToJSON Security.PEnable where toJSON _ := .null
instance : Command Security.PEnable where
  Response := Unit
  commandName _ := "Security.enable"
  decodeResponse _ := .ok ()

/-- `Security.PSetIgnoreCertificateErrors`. -/
structure Security.PSetIgnoreCertificateErrors where
  ignore : Bool
  deriving Repr, BEq, DecidableEq
instance : ToJSON Security.PSetIgnoreCertificateErrors where
  toJSON p := Data.Json.object <|
       [("ignore", ToJSON.toJSON p.ignore)]
instance : Command Security.PSetIgnoreCertificateErrors where
  Response := Unit
  commandName _ := "Security.setIgnoreCertificateErrors"
  decodeResponse _ := .ok ()

end CDP.Domains.DOMPageNetworkEmulationSecurity
