/-
  Linen.CDP.Domains.Accessibility — the `Accessibility` CDP domain

  Ports `CDP.Domains.Accessibility` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.Memory`'s docstring. Types from
  `CDP.Domains.DOMPageNetworkEmulationSecurity` are referenced through that
  module's nested namespaces (`DOM.NodeId`, `DOM.BackendNodeId`,
  `Page.FrameId`), and `Runtime.RemoteObjectId` from `CDP.Domains.Runtime`.

  `AXValue.value` carries an arbitrary computed property value; upstream keeps
  it as a raw `A.Value`, ported here as `Data.Json.Value` (which already has
  `FromJSON`/`ToJSON` instances that pass the value through unchanged).

  `AXValue` and `AXValueSource` are mutually self-referential: an `AXValue`'s
  `sources` field is a list of `AXValueSource`, and an `AXValueSource`'s
  `value`/`attributeValue`/`nativeSourceValue` fields are each an `AXValue`.
  Following `CDP.Domains.DOMPageNetworkEmulationSecurity`'s `DOM.Node` (itself
  singly self-referential) and `CDP.Domains.HeapProfiler`'s
  `SamplingHeapProfileNode`, both types derive only `Repr, BEq` (no
  `DecidableEq`, which mutual-inductive deriving handlers don't support here),
  and their `FromJSON`/`ToJSON` are hand-written mutually-recursive `def`s with
  `sizeOf` termination proofs. `AXProperty` and `AXNode` transitively embed
  `AXValue`, so they too derive only `Repr, BEq`.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.Accessibility

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Identifiers ──

/-- Unique accessibility node identifier. -/
abbrev AXNodeId := String

-- ── Enums ──

/-- Enum of possible property types. -/
inductive AXValueType where
  | boolean | tristate | booleanOrUndefined | idref | idrefList | integer | node
  | nodeList | number | string | computedString | token | tokenList | domRelation
  | role | internalRole | valueUndefined
  deriving Repr, BEq, DecidableEq

instance : FromJSON AXValueType where
  parseJSON
    | .string "boolean" => .ok .boolean
    | .string "tristate" => .ok .tristate
    | .string "booleanOrUndefined" => .ok .booleanOrUndefined
    | .string "idref" => .ok .idref
    | .string "idrefList" => .ok .idrefList
    | .string "integer" => .ok .integer
    | .string "node" => .ok .node
    | .string "nodeList" => .ok .nodeList
    | .string "number" => .ok .number
    | .string "string" => .ok .string
    | .string "computedString" => .ok .computedString
    | .string "token" => .ok .token
    | .string "tokenList" => .ok .tokenList
    | .string "domRelation" => .ok .domRelation
    | .string "role" => .ok .role
    | .string "internalRole" => .ok .internalRole
    | .string "valueUndefined" => .ok .valueUndefined
    | v => .error s!"failed to parse AXValueType: {repr v}"

instance : ToJSON AXValueType where
  toJSON
    | .boolean => .string "boolean"
    | .tristate => .string "tristate"
    | .booleanOrUndefined => .string "booleanOrUndefined"
    | .idref => .string "idref"
    | .idrefList => .string "idrefList"
    | .integer => .string "integer"
    | .node => .string "node"
    | .nodeList => .string "nodeList"
    | .number => .string "number"
    | .string => .string "string"
    | .computedString => .string "computedString"
    | .token => .string "token"
    | .tokenList => .string "tokenList"
    | .domRelation => .string "domRelation"
    | .role => .string "role"
    | .internalRole => .string "internalRole"
    | .valueUndefined => .string "valueUndefined"

/-- Enum of possible property sources. -/
inductive AXValueSourceType where
  | attribute | implicit | style | contents | placeholder | relatedElement
  deriving Repr, BEq, DecidableEq

instance : FromJSON AXValueSourceType where
  parseJSON
    | .string "attribute" => .ok .attribute
    | .string "implicit" => .ok .implicit
    | .string "style" => .ok .style
    | .string "contents" => .ok .contents
    | .string "placeholder" => .ok .placeholder
    | .string "relatedElement" => .ok .relatedElement
    | v => .error s!"failed to parse AXValueSourceType: {repr v}"

instance : ToJSON AXValueSourceType where
  toJSON
    | .attribute => .string "attribute"
    | .implicit => .string "implicit"
    | .style => .string "style"
    | .contents => .string "contents"
    | .placeholder => .string "placeholder"
    | .relatedElement => .string "relatedElement"

/-- Enum of possible native property sources (as a subtype of a particular
    `AXValueSourceType`). -/
inductive AXValueNativeSourceType where
  | description | figcaption | label | labelfor | labelwrapped | legend
  | rubyannotation | tablecaption | title | other
  deriving Repr, BEq, DecidableEq

instance : FromJSON AXValueNativeSourceType where
  parseJSON
    | .string "description" => .ok .description
    | .string "figcaption" => .ok .figcaption
    | .string "label" => .ok .label
    | .string "labelfor" => .ok .labelfor
    | .string "labelwrapped" => .ok .labelwrapped
    | .string "legend" => .ok .legend
    | .string "rubyannotation" => .ok .rubyannotation
    | .string "tablecaption" => .ok .tablecaption
    | .string "title" => .ok .title
    | .string "other" => .ok .other
    | v => .error s!"failed to parse AXValueNativeSourceType: {repr v}"

instance : ToJSON AXValueNativeSourceType where
  toJSON
    | .description => .string "description"
    | .figcaption => .string "figcaption"
    | .label => .string "label"
    | .labelfor => .string "labelfor"
    | .labelwrapped => .string "labelwrapped"
    | .legend => .string "legend"
    | .rubyannotation => .string "rubyannotation"
    | .tablecaption => .string "tablecaption"
    | .title => .string "title"
    | .other => .string "other"

/-- Values of `AXProperty` name:
    - from `busy` to `roledescription`: states which apply to every AX node
    - from `live` to `root`: attributes which apply to nodes in live regions
    - from `autocomplete` to `valuetext`: attributes which apply to widgets
    - from `checked` to `selected`: states which apply to widgets
    - from `activedescendant` to `owns`: relationships between elements other
      than parent/child/sibling. -/
inductive AXPropertyName where
  | busy | disabled | editable | focusable | focused | hidden | hiddenRoot
  | invalid | keyshortcuts | settable | roledescription | live | atomic | relevant
  | root | autocomplete | hasPopup | level | multiselectable | orientation
  | multiline | readonly | required | valuemin | valuemax | valuetext | checked
  | expanded | modal | pressed | selected | activedescendant | controls
  | describedby | details | errormessage | flowto | labelledby | owns
  deriving Repr, BEq, DecidableEq

instance : FromJSON AXPropertyName where
  parseJSON
    | .string "busy" => .ok .busy
    | .string "disabled" => .ok .disabled
    | .string "editable" => .ok .editable
    | .string "focusable" => .ok .focusable
    | .string "focused" => .ok .focused
    | .string "hidden" => .ok .hidden
    | .string "hiddenRoot" => .ok .hiddenRoot
    | .string "invalid" => .ok .invalid
    | .string "keyshortcuts" => .ok .keyshortcuts
    | .string "settable" => .ok .settable
    | .string "roledescription" => .ok .roledescription
    | .string "live" => .ok .live
    | .string "atomic" => .ok .atomic
    | .string "relevant" => .ok .relevant
    | .string "root" => .ok .root
    | .string "autocomplete" => .ok .autocomplete
    | .string "hasPopup" => .ok .hasPopup
    | .string "level" => .ok .level
    | .string "multiselectable" => .ok .multiselectable
    | .string "orientation" => .ok .orientation
    | .string "multiline" => .ok .multiline
    | .string "readonly" => .ok .readonly
    | .string "required" => .ok .required
    | .string "valuemin" => .ok .valuemin
    | .string "valuemax" => .ok .valuemax
    | .string "valuetext" => .ok .valuetext
    | .string "checked" => .ok .checked
    | .string "expanded" => .ok .expanded
    | .string "modal" => .ok .modal
    | .string "pressed" => .ok .pressed
    | .string "selected" => .ok .selected
    | .string "activedescendant" => .ok .activedescendant
    | .string "controls" => .ok .controls
    | .string "describedby" => .ok .describedby
    | .string "details" => .ok .details
    | .string "errormessage" => .ok .errormessage
    | .string "flowto" => .ok .flowto
    | .string "labelledby" => .ok .labelledby
    | .string "owns" => .ok .owns
    | v => .error s!"failed to parse AXPropertyName: {repr v}"

instance : ToJSON AXPropertyName where
  toJSON
    | .busy => .string "busy"
    | .disabled => .string "disabled"
    | .editable => .string "editable"
    | .focusable => .string "focusable"
    | .focused => .string "focused"
    | .hidden => .string "hidden"
    | .hiddenRoot => .string "hiddenRoot"
    | .invalid => .string "invalid"
    | .keyshortcuts => .string "keyshortcuts"
    | .settable => .string "settable"
    | .roledescription => .string "roledescription"
    | .live => .string "live"
    | .atomic => .string "atomic"
    | .relevant => .string "relevant"
    | .root => .string "root"
    | .autocomplete => .string "autocomplete"
    | .hasPopup => .string "hasPopup"
    | .level => .string "level"
    | .multiselectable => .string "multiselectable"
    | .orientation => .string "orientation"
    | .multiline => .string "multiline"
    | .readonly => .string "readonly"
    | .required => .string "required"
    | .valuemin => .string "valuemin"
    | .valuemax => .string "valuemax"
    | .valuetext => .string "valuetext"
    | .checked => .string "checked"
    | .expanded => .string "expanded"
    | .modal => .string "modal"
    | .pressed => .string "pressed"
    | .selected => .string "selected"
    | .activedescendant => .string "activedescendant"
    | .controls => .string "controls"
    | .describedby => .string "describedby"
    | .details => .string "details"
    | .errormessage => .string "errormessage"
    | .flowto => .string "flowto"
    | .labelledby => .string "labelledby"
    | .owns => .string "owns"

-- ── Related nodes ──

/-- A related DOM node referenced from an `AXValue`. -/
structure AXRelatedNode where
  /-- The `BackendNodeId` of the related DOM node. -/
  backendDOMNodeId : DOMPageNetworkEmulationSecurity.DOM.BackendNodeId
  /-- The IDRef value provided, if any. -/
  idref : Option String := none
  /-- The text alternative of this node in the current context. -/
  text : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON AXRelatedNode where
  parseJSON v := do
    .ok
      { backendDOMNodeId := ← Value.getField v "backendDOMNodeId" >>= FromJSON.parseJSON
        idref := ← (← Value.getFieldOpt v "idref").mapM FromJSON.parseJSON
        text := ← (← Value.getFieldOpt v "text").mapM FromJSON.parseJSON }

instance : ToJSON AXRelatedNode where
  toJSON p := Data.Json.object <|
       [("backendDOMNodeId", ToJSON.toJSON p.backendDOMNodeId)]
    ++ (p.idref.map fun x => ("idref", ToJSON.toJSON x)).toList
    ++ (p.text.map fun x => ("text", ToJSON.toJSON x)).toList

-- ── Mutually self-referential values and sources ──

-- `AXValue` and `AXValueSource` are declared inside an explicit `mutual`
-- block (rather than relying on Lean's implicit forward-reference
-- bracketing) because leaving them as two separate top-level `structure`s
-- that merely refer to each other causes Lean to infer a universe level of
-- `Type 1` for both (each is nested inside the other via `Option`/`List`,
-- and the auto-mutual elaborator does not find the smallest universe on its
-- own) — which then breaks every generic `Type`-parametrised class
-- (`FromJSON`, `ToJSON`, `Command`, `Event`, …) applied to them or to
-- anything that mentions them. Wrapping the structures themselves in an
-- explicit `mutual ... end` block keeps them at `Type` as expected.
mutual
/-- A single computed AX property; mutually self-referential with
    `AXValueSource` (see the module header). -/
structure AXValue where
  /-- The type of this value. -/
  type : AXValueType
  /-- The computed value of this property. -/
  value : Option Value := none
  /-- One or more related nodes, if applicable. -/
  relatedNodes : Option (List AXRelatedNode) := none
  /-- The sources which contributed to the computation of this property. -/
  sources : Option (List AXValueSource) := none
  deriving Repr, BEq

/-- A single source for a computed AX property; mutually self-referential
    with `AXValue` (see the module header). -/
structure AXValueSource where
  /-- What type of source this is. -/
  type : AXValueSourceType
  /-- The value of this property source. -/
  value : Option AXValue := none
  /-- The name of the relevant attribute, if any. -/
  «attribute» : Option String := none
  /-- The value of the relevant attribute, if any. -/
  attributeValue : Option AXValue := none
  /-- Whether this source is superseded by a higher priority source. -/
  superseded : Option Bool := none
  /-- The native markup source for this value, e.g. a `<label>` element. -/
  nativeSource : Option AXValueNativeSourceType := none
  /-- The value, such as a node or node list, of the native source. -/
  nativeSourceValue : Option AXValue := none
  /-- Whether the value for this property is invalid. -/
  invalid : Option Bool := none
  /-- Reason for the value being invalid, if it is. -/
  invalidReason : Option String := none
  deriving Repr, BEq
end

/-- Assemble an `AXValue` from its already-decoded `sources`. -/
def finishAXValue (v : Value) (sources : Option (List AXValueSource)) : Except String AXValue := do
  .ok
    { type := ← Value.getField v "type" >>= FromJSON.parseJSON
      value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
      relatedNodes := ← (← Value.getFieldOpt v "relatedNodes").mapM FromJSON.parseJSON
      sources }

/-- Assemble an `AXValueSource` from its already-decoded recursive fields. -/
def finishAXValueSource (v : Value) (value : Option AXValue) (attributeValue : Option AXValue)
    (nativeSourceValue : Option AXValue) : Except String AXValueSource := do
  .ok
    { type := ← Value.getField v "type" >>= FromJSON.parseJSON
      value
      «attribute» := ← (← Value.getFieldOpt v "attribute").mapM FromJSON.parseJSON
      attributeValue
      superseded := ← (← Value.getFieldOpt v "superseded").mapM FromJSON.parseJSON
      nativeSource := ← (← Value.getFieldOpt v "nativeSource").mapM FromJSON.parseJSON
      nativeSourceValue
      invalid := ← (← Value.getFieldOpt v "invalid").mapM FromJSON.parseJSON
      invalidReason := ← (← Value.getFieldOpt v "invalidReason").mapM FromJSON.parseJSON }

mutual
/-- Decode an `AXValue`. -/
def parseAXValue (v : Value) : Except String AXValue := do
  let sources ← match h0 : v.lookup "sources" with
    | some x => (parseAXValueSourceList x).map some
    | none => .ok none
  finishAXValue v sources
termination_by sizeOf v
decreasing_by exact Value.lookup_sizeOf_lt h0

/-- Decode an `AXValueSource`. -/
def parseAXValueSource (v : Value) : Except String AXValueSource := do
  let value ← match h0 : v.lookup "value" with
    | some .null => .ok none
    | some x => (parseAXValue x).map some
    | none => .ok none
  let attributeValue ← match h1 : v.lookup "attributeValue" with
    | some .null => .ok none
    | some x => (parseAXValue x).map some
    | none => .ok none
  let nativeSourceValue ← match h2 : v.lookup "nativeSourceValue" with
    | some .null => .ok none
    | some x => (parseAXValue x).map some
    | none => .ok none
  finishAXValueSource v value attributeValue nativeSourceValue
termination_by sizeOf v
decreasing_by
  all_goals first
    | exact Value.lookup_sizeOf_lt h0
    | exact Value.lookup_sizeOf_lt h1
    | exact Value.lookup_sizeOf_lt h2

/-- Decode a JSON array of `AXValueSource`. -/
def parseAXValueSourceList (v : Value) : Except String (List AXValueSource) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => parseAXValueSource p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega
end

instance : FromJSON AXValue where parseJSON := parseAXValue
instance : FromJSON AXValueSource where parseJSON := parseAXValueSource

/-- `AXValue.sources = some x` implies `x` is structurally smaller. -/
private theorem AXValue_sources_sizeOf_lt {p : AXValue} {x : List AXValueSource}
    (h : p.sources = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [AXValue.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega

/-- `AXValueSource.value = some x` implies `x` is structurally smaller. -/
private theorem AXValueSource_value_sizeOf_lt {p : AXValueSource} {x : AXValue}
    (h : p.value = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [AXValueSource.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega

/-- `AXValueSource.attributeValue = some x` implies `x` is structurally smaller. -/
private theorem AXValueSource_attributeValue_sizeOf_lt {p : AXValueSource} {x : AXValue}
    (h : p.attributeValue = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [AXValueSource.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega

/-- `AXValueSource.nativeSourceValue = some x` implies `x` is structurally smaller. -/
private theorem AXValueSource_nativeSourceValue_sizeOf_lt {p : AXValueSource} {x : AXValue}
    (h : p.nativeSourceValue = some x) : sizeOf x < sizeOf p := by
  cases p; simp_all only [AXValueSource.mk.sizeOf_spec, Option.some.sizeOf_spec]; omega

mutual
/-- Encode an `AXValue`. -/
def encodeAXValue (p : AXValue) : Value :=
  Data.Json.object <|
       [("type", ToJSON.toJSON p.type)]
    ++ (p.value.map fun x => ("value", ToJSON.toJSON x)).toList
    ++ (p.relatedNodes.map fun x => ("relatedNodes", ToJSON.toJSON x)).toList
    ++ (match h : p.sources with | some x => [("sources", encodeAXValueSourceList x)] | none => [])
termination_by sizeOf p
decreasing_by
  all_goals first
    | exact AXValue_sources_sizeOf_lt h
    | (cases p; simp only [AXValue.mk.sizeOf_spec]; omega)

/-- Encode an `AXValueSource`. -/
def encodeAXValueSource (p : AXValueSource) : Value :=
  Data.Json.object <|
       [("type", ToJSON.toJSON p.type)]
    ++ (match h : p.value with | some x => [("value", encodeAXValue x)] | none => [])
    ++ (p.attribute.map fun x => ("attribute", ToJSON.toJSON x)).toList
    ++ (match h : p.attributeValue with | some x => [("attributeValue", encodeAXValue x)] | none => [])
    ++ (p.superseded.map fun x => ("superseded", ToJSON.toJSON x)).toList
    ++ (p.nativeSource.map fun x => ("nativeSource", ToJSON.toJSON x)).toList
    ++ (match h : p.nativeSourceValue with | some x => [("nativeSourceValue", encodeAXValue x)] | none => [])
    ++ (p.invalid.map fun x => ("invalid", ToJSON.toJSON x)).toList
    ++ (p.invalidReason.map fun x => ("invalidReason", ToJSON.toJSON x)).toList
termination_by sizeOf p
decreasing_by
  all_goals first
    | exact AXValueSource_value_sizeOf_lt h
    | exact AXValueSource_attributeValue_sizeOf_lt h
    | exact AXValueSource_nativeSourceValue_sizeOf_lt h
    | (cases p; simp only [AXValueSource.mk.sizeOf_spec]; omega)

/-- Encode a list of `AXValueSource`. -/
def encodeAXValueSourceList (l : List AXValueSource) : Value :=
  Value.array (l.map encodeAXValueSource).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega
end

instance : ToJSON AXValue where toJSON := encodeAXValue
instance : ToJSON AXValueSource where toJSON := encodeAXValueSource

-- ── Properties and nodes ──

/-- A single accessibility property (name/value pair). Transitively embeds
    `AXValue`, so — like `AXValue`/`AXValueSource` — it derives only
    `Repr, BEq`. -/
structure AXProperty where
  /-- The name of this property. -/
  name : AXPropertyName
  /-- The value of this property. -/
  value : AXValue
  deriving Repr, BEq

instance : FromJSON AXProperty where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON AXProperty where
  toJSON p := Data.Json.object [("name", ToJSON.toJSON p.name), ("value", ToJSON.toJSON p.value)]

/-- A node in the accessibility tree. Transitively embeds `AXValue` (via
    `role`, `chromeRole`, `name`, `description`, `value`) and `AXProperty`, so
    it too derives only `Repr, BEq`. -/
structure AXNode where
  /-- Unique identifier for this node. -/
  nodeId : AXNodeId
  /-- Whether this node is ignored for accessibility. -/
  ignored : Bool
  /-- Collection of reasons why this node is hidden. -/
  ignoredReasons : Option (List AXProperty) := none
  /-- This node's role, whether explicit or implicit. -/
  role : Option AXValue := none
  /-- This node's Chrome raw role. -/
  chromeRole : Option AXValue := none
  /-- The accessible name for this node. -/
  name : Option AXValue := none
  /-- The accessible description for this node. -/
  description : Option AXValue := none
  /-- The value for this node. -/
  value : Option AXValue := none
  /-- All other properties. -/
  properties : Option (List AXProperty) := none
  /-- ID for this node's parent. -/
  parentId : Option AXNodeId := none
  /-- IDs for each of this node's child nodes. -/
  childIds : Option (List AXNodeId) := none
  /-- The backend ID for the associated DOM node, if any. -/
  backendDOMNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- The frame ID for the frame associated with this node's document. -/
  frameId : Option DOMPageNetworkEmulationSecurity.Page.FrameId := none
  deriving Repr, BEq

instance : FromJSON AXNode where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        ignored := ← Value.getField v "ignored" >>= FromJSON.parseJSON
        ignoredReasons := ← (← Value.getFieldOpt v "ignoredReasons").mapM FromJSON.parseJSON
        role := ← (← Value.getFieldOpt v "role").mapM FromJSON.parseJSON
        chromeRole := ← (← Value.getFieldOpt v "chromeRole").mapM FromJSON.parseJSON
        name := ← (← Value.getFieldOpt v "name").mapM FromJSON.parseJSON
        description := ← (← Value.getFieldOpt v "description").mapM FromJSON.parseJSON
        value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
        properties := ← (← Value.getFieldOpt v "properties").mapM FromJSON.parseJSON
        parentId := ← (← Value.getFieldOpt v "parentId").mapM FromJSON.parseJSON
        childIds := ← (← Value.getFieldOpt v "childIds").mapM FromJSON.parseJSON
        backendDOMNodeId := ← (← Value.getFieldOpt v "backendDOMNodeId").mapM FromJSON.parseJSON
        frameId := ← (← Value.getFieldOpt v "frameId").mapM FromJSON.parseJSON }

instance : ToJSON AXNode where
  toJSON p := Data.Json.object <|
       [("nodeId", ToJSON.toJSON p.nodeId)]
    ++ [("ignored", ToJSON.toJSON p.ignored)]
    ++ (p.ignoredReasons.map fun x => ("ignoredReasons", ToJSON.toJSON x)).toList
    ++ (p.role.map fun x => ("role", ToJSON.toJSON x)).toList
    ++ (p.chromeRole.map fun x => ("chromeRole", ToJSON.toJSON x)).toList
    ++ (p.name.map fun x => ("name", ToJSON.toJSON x)).toList
    ++ (p.description.map fun x => ("description", ToJSON.toJSON x)).toList
    ++ (p.value.map fun x => ("value", ToJSON.toJSON x)).toList
    ++ (p.properties.map fun x => ("properties", ToJSON.toJSON x)).toList
    ++ (p.parentId.map fun x => ("parentId", ToJSON.toJSON x)).toList
    ++ (p.childIds.map fun x => ("childIds", ToJSON.toJSON x)).toList
    ++ (p.backendDOMNodeId.map fun x => ("backendDOMNodeId", ToJSON.toJSON x)).toList
    ++ (p.frameId.map fun x => ("frameId", ToJSON.toJSON x)).toList

-- ── Events ──

/-- `Accessibility.loadComplete`: fired when a new document root's
    accessibility tree becomes available. -/
structure LoadComplete where
  /-- New document root node. -/
  root : AXNode
  deriving Repr, BEq

instance : FromJSON LoadComplete where
  parseJSON v := do .ok { root := ← Value.getField v "root" >>= FromJSON.parseJSON }

instance : Event LoadComplete where
  eventName := "Accessibility.loadComplete"

/-- `Accessibility.nodesUpdated`: fired when nodes' data changes. -/
structure NodesUpdated where
  /-- Updated node data. -/
  nodes : List AXNode
  deriving Repr, BEq

instance : FromJSON NodesUpdated where
  parseJSON v := do .ok { nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON }

instance : Event NodesUpdated where
  eventName := "Accessibility.nodesUpdated"

-- ── Commands ──

/-- Parameters of the `Accessibility.disable` command: disables the
    accessibility domain. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Accessibility.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Accessibility.enable` command: enables the
    accessibility domain, which causes `AXNodeId`s to remain consistent
    between method calls. This turns on accessibility for the page, which can
    impact performance until accessibility is disabled. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Accessibility.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Accessibility.getPartialAXTree` command: fetches the
    accessibility node and partial accessibility tree for this DOM node, if it
    exists. -/
structure PGetPartialAXTree where
  /-- Identifier of the node to get the partial accessibility tree for. -/
  nodeId : Option DOMPageNetworkEmulationSecurity.DOM.NodeId := none
  /-- Identifier of the backend node to get the partial accessibility tree
      for. -/
  backendNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- JavaScript object id of the node wrapper to get the partial
      accessibility tree for. -/
  objectId : Option Runtime.RemoteObjectId := none
  /-- Whether to fetch this node's ancestors, siblings and children. Defaults
      to `true`. -/
  fetchRelatives : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetPartialAXTree where
  toJSON p := Data.Json.object <|
       (p.nodeId.map fun x => ("nodeId", ToJSON.toJSON x)).toList
    ++ (p.backendNodeId.map fun x => ("backendNodeId", ToJSON.toJSON x)).toList
    ++ (p.objectId.map fun x => ("objectId", ToJSON.toJSON x)).toList
    ++ (p.fetchRelatives.map fun x => ("fetchRelatives", ToJSON.toJSON x)).toList

/-- Response of the `Accessibility.getPartialAXTree` command. -/
structure GetPartialAXTree where
  /-- The `AXNode` for this DOM node, if it exists, plus its ancestors,
      siblings and children, if requested. -/
  nodes : List AXNode
  deriving Repr, BEq

instance : FromJSON GetPartialAXTree where
  parseJSON v := do .ok { nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON }

instance : Command PGetPartialAXTree where
  Response := GetPartialAXTree
  commandName _ := "Accessibility.getPartialAXTree"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Accessibility.getFullAXTree` command: fetches the
    entire accessibility tree for the root document. -/
structure PGetFullAXTree where
  /-- The maximum depth at which descendants of the root node should be
      retrieved. If omitted, the full tree is returned. -/
  depth : Option Int := none
  /-- The frame for whose document the AX tree should be retrieved. If
      omitted, the root frame is used. -/
  frameId : Option DOMPageNetworkEmulationSecurity.Page.FrameId := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetFullAXTree where
  toJSON p := Data.Json.object <|
       (p.depth.map fun x => ("depth", ToJSON.toJSON x)).toList
    ++ (p.frameId.map fun x => ("frameId", ToJSON.toJSON x)).toList

/-- Response of the `Accessibility.getFullAXTree` command. -/
structure GetFullAXTree where
  nodes : List AXNode
  deriving Repr, BEq

instance : FromJSON GetFullAXTree where
  parseJSON v := do .ok { nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON }

instance : Command PGetFullAXTree where
  Response := GetFullAXTree
  commandName _ := "Accessibility.getFullAXTree"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Accessibility.getRootAXNode` command: fetches the root
    node. Requires `enable` to have been called previously. -/
structure PGetRootAXNode where
  /-- The frame in whose document the node resides. If omitted, the root
      frame is used. -/
  frameId : Option DOMPageNetworkEmulationSecurity.Page.FrameId := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetRootAXNode where
  toJSON p := Data.Json.object <| (p.frameId.map fun x => ("frameId", ToJSON.toJSON x)).toList

/-- Response of the `Accessibility.getRootAXNode` command. -/
structure GetRootAXNode where
  node : AXNode
  deriving Repr, BEq

instance : FromJSON GetRootAXNode where
  parseJSON v := do .ok { node := ← Value.getField v "node" >>= FromJSON.parseJSON }

instance : Command PGetRootAXNode where
  Response := GetRootAXNode
  commandName _ := "Accessibility.getRootAXNode"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Accessibility.getAXNodeAndAncestors` command: fetches a
    node and all ancestors up to and including the root. Requires `enable` to
    have been called previously. -/
structure PGetAXNodeAndAncestors where
  /-- Identifier of the node to get. -/
  nodeId : Option DOMPageNetworkEmulationSecurity.DOM.NodeId := none
  /-- Identifier of the backend node to get. -/
  backendNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- JavaScript object id of the node wrapper to get. -/
  objectId : Option Runtime.RemoteObjectId := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetAXNodeAndAncestors where
  toJSON p := Data.Json.object <|
       (p.nodeId.map fun x => ("nodeId", ToJSON.toJSON x)).toList
    ++ (p.backendNodeId.map fun x => ("backendNodeId", ToJSON.toJSON x)).toList
    ++ (p.objectId.map fun x => ("objectId", ToJSON.toJSON x)).toList

/-- Response of the `Accessibility.getAXNodeAndAncestors` command. -/
structure GetAXNodeAndAncestors where
  nodes : List AXNode
  deriving Repr, BEq

instance : FromJSON GetAXNodeAndAncestors where
  parseJSON v := do .ok { nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON }

instance : Command PGetAXNodeAndAncestors where
  Response := GetAXNodeAndAncestors
  commandName _ := "Accessibility.getAXNodeAndAncestors"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Accessibility.getChildAXNodes` command: fetches a
    particular accessibility node by `AXNodeId`. Requires `enable` to have
    been called previously. -/
structure PGetChildAXNodes where
  /-- Identifier of the node to get children for. -/
  id : AXNodeId
  /-- The frame in whose document the node resides. If omitted, the root
      frame is used. -/
  frameId : Option DOMPageNetworkEmulationSecurity.Page.FrameId := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetChildAXNodes where
  toJSON p := Data.Json.object <|
       [("id", ToJSON.toJSON p.id)]
    ++ (p.frameId.map fun x => ("frameId", ToJSON.toJSON x)).toList

/-- Response of the `Accessibility.getChildAXNodes` command. -/
structure GetChildAXNodes where
  nodes : List AXNode
  deriving Repr, BEq

instance : FromJSON GetChildAXNodes where
  parseJSON v := do .ok { nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON }

instance : Command PGetChildAXNodes where
  Response := GetChildAXNodes
  commandName _ := "Accessibility.getChildAXNodes"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Accessibility.queryAXTree` command: queries a DOM
    node's accessibility subtree for accessible name and role. This command
    computes the name and role for all nodes in the subtree, including those
    that are ignored for accessibility, and returns those that match the
    specified name and role. If no DOM node is specified, or the DOM node does
    not exist, the command returns an error. If neither `accessibleName` nor
    `role` is specified, it returns all the accessibility nodes in the
    subtree. -/
structure PQueryAXTree where
  /-- Identifier of the node for the root to query. -/
  nodeId : Option DOMPageNetworkEmulationSecurity.DOM.NodeId := none
  /-- Identifier of the backend node for the root to query. -/
  backendNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- JavaScript object id of the node wrapper for the root to query. -/
  objectId : Option Runtime.RemoteObjectId := none
  /-- Find nodes with this computed name. -/
  accessibleName : Option String := none
  /-- Find nodes with this computed role. -/
  role : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PQueryAXTree where
  toJSON p := Data.Json.object <|
       (p.nodeId.map fun x => ("nodeId", ToJSON.toJSON x)).toList
    ++ (p.backendNodeId.map fun x => ("backendNodeId", ToJSON.toJSON x)).toList
    ++ (p.objectId.map fun x => ("objectId", ToJSON.toJSON x)).toList
    ++ (p.accessibleName.map fun x => ("accessibleName", ToJSON.toJSON x)).toList
    ++ (p.role.map fun x => ("role", ToJSON.toJSON x)).toList

/-- Response of the `Accessibility.queryAXTree` command. -/
structure QueryAXTree where
  /-- A list of `AXNode` matching the specified attributes, including nodes
      that are ignored for accessibility. -/
  nodes : List AXNode
  deriving Repr, BEq

instance : FromJSON QueryAXTree where
  parseJSON v := do .ok { nodes := ← Value.getField v "nodes" >>= FromJSON.parseJSON }

instance : Command PQueryAXTree where
  Response := QueryAXTree
  commandName _ := "Accessibility.queryAXTree"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.Accessibility
