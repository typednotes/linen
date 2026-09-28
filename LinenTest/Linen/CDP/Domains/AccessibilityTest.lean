/-
  Tests for `Linen.CDP.Domains.Accessibility`.
-/
import Linen.CDP.Domains.Accessibility

open CDP.Domains.Accessibility
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Accessibility

-- ── Enums ──

#guard decodeAs "\"idrefList\"" (α := AXValueType) = .ok .idrefList
#guard encode (ToJSON.toJSON AXValueType.computedString) = "\"computedString\""

#guard decodeAs "\"relatedElement\"" (α := AXValueSourceType) = .ok .relatedElement
#guard encode (ToJSON.toJSON AXValueSourceType.style) = "\"style\""

#guard decodeAs "\"labelfor\"" (α := AXValueNativeSourceType) = .ok .labelfor
#guard encode (ToJSON.toJSON AXValueNativeSourceType.figcaption) = "\"figcaption\""

#guard decodeAs "\"activedescendant\"" (α := AXPropertyName) = .ok .activedescendant
#guard encode (ToJSON.toJSON AXPropertyName.roledescription) = "\"roledescription\""

-- ── Related nodes ──

#guard decodeAs "{\"backendDOMNodeId\": 7}" (α := AXRelatedNode) = .ok { backendDOMNodeId := 7 }
#guard decodeAs "{\"backendDOMNodeId\": 7, \"idref\": \"x\", \"text\": \"y\"}" (α := AXRelatedNode)
  = .ok { backendDOMNodeId := 7, idref := some "x", text := some "y" }
#guard encode (ToJSON.toJSON ({ backendDOMNodeId := 7 } : AXRelatedNode)) = "{\"backendDOMNodeId\":7}"

-- ── AXValue / AXValueSource (mutually self-referential) ──

-- `AXValue`/`AXValueSource` derive only `Repr, BEq` (not `DecidableEq`, which
-- Lean cannot derive for mutually self-referential structures), so equality
-- checks below compare via `BEq`'s `==` (through `Except.map`) rather than `=`.

#guard (decodeAs "{\"type\": \"string\"}" (α := AXValue)).map (· == ({ type := .string } : AXValue))
  = .ok true
#guard
    (decodeAs "{\"type\": \"string\", \"value\": \"hi\"}" (α := AXValue)).map
      (· == ({ type := .string, value := some (.string "hi") } : AXValue))
  = .ok true

-- A value with a nested source that itself carries no nested value.
#guard
    (decodeAs
        "{\"type\": \"string\", \"sources\": [{\"type\": \"attribute\", \"attribute\": \"aria-label\"}]}"
        (α := AXValue)).map
      (· == ({ type := .string,
                sources := some [{ type := .attribute, «attribute» := some "aria-label" }] } : AXValue))
  = .ok true

-- A source whose own `value` is a nested `AXValue` (two levels of mutual recursion).
#guard
    (decodeAs "{\"type\": \"attribute\", \"value\": {\"type\": \"boolean\", \"value\": true}}"
        (α := AXValueSource)).map
      (· == ({ type := .attribute, value := some { type := .boolean, value := some (.bool true) } }
          : AXValueSource))
  = .ok true

#guard encode (ToJSON.toJSON ({ type := .string } : AXValue)) = "{\"type\":\"string\"}"
#guard encode
    (ToJSON.toJSON
      ({ type := .string
         sources := some [{ type := .attribute, «attribute» := some "aria-label" }] } : AXValue))
  = "{\"type\":\"string\",\"sources\":[{\"type\":\"attribute\",\"attribute\":\"aria-label\"}]}"
#guard encode
    (ToJSON.toJSON
      ({ type := .attribute, value := some { type := .boolean, value := some (.bool true) } }
        : AXValueSource))
  = "{\"type\":\"attribute\",\"value\":{\"type\":\"boolean\",\"value\":true}}"

-- Round-trip through decode ∘ encode for a value with a nested source.
#guard
    let v : AXValue :=
      { type := .string, sources := some [{ type := .attribute, «attribute» := some "aria-label" }] };
    (decodeAs (encode (ToJSON.toJSON v)) (α := AXValue)).map (· == v) = .ok true

-- ── Properties and nodes ──

#guard
    (decodeAs "{\"name\": \"busy\", \"value\": {\"type\": \"boolean\", \"value\": true}}"
        (α := AXProperty)).map
      (· == ({ name := .busy, value := { type := .boolean, value := some (.bool true) } } : AXProperty))
  = .ok true

#guard
    (decodeAs "{\"nodeId\": \"1\", \"ignored\": false}" (α := AXNode)).map
      (· == ({ nodeId := "1", ignored := false } : AXNode))
  = .ok true
#guard
    (decodeAs
        "{\"nodeId\": \"1\", \"ignored\": true, \"role\": {\"type\": \"role\", \"value\": \"button\"}, \"backendDOMNodeId\": 3, \"frameId\": \"f1\"}"
        (α := AXNode)).map
      (· == ({ nodeId := "1", ignored := true,
                role := some { type := .role, value := some (.string "button") },
                backendDOMNodeId := some 3, frameId := some "f1" } : AXNode))
  = .ok true
#guard encode (ToJSON.toJSON ({ nodeId := "1", ignored := false } : AXNode))
  = "{\"nodeId\":\"1\",\"ignored\":false}"

-- ── Events ──

#guard
    (decodeAs "{\"root\": {\"nodeId\": \"1\", \"ignored\": false}}" (α := LoadComplete)).map
      (· == ({ root := { nodeId := "1", ignored := false } } : LoadComplete))
  = .ok true
#guard Event.eventName (α := LoadComplete) = "Accessibility.loadComplete"

#guard
    (decodeAs "{\"nodes\": [{\"nodeId\": \"1\", \"ignored\": false}]}" (α := NodesUpdated)).map
      (· == ({ nodes := [{ nodeId := "1", ignored := false }] } : NodesUpdated))
  = .ok true
#guard Event.eventName (α := NodesUpdated) = "Accessibility.nodesUpdated"

-- ── Commands ──

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Accessibility.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "Accessibility.enable"

#guard encode (ToJSON.toJSON ({} : PGetPartialAXTree)) = "{}"
#guard encode (ToJSON.toJSON ({ nodeId := some 3, fetchRelatives := some false } : PGetPartialAXTree))
  = "{\"nodeId\":3,\"fetchRelatives\":false}"
#guard Command.commandName ({} : PGetPartialAXTree) = "Accessibility.getPartialAXTree"
#guard
    (decodeAs "{\"nodes\": []}" (α := GetPartialAXTree)).map (· == ({ nodes := [] } : GetPartialAXTree))
  = .ok true

#guard encode (ToJSON.toJSON ({} : PGetFullAXTree)) = "{}"
#guard encode (ToJSON.toJSON ({ depth := some 2 } : PGetFullAXTree)) = "{\"depth\":2}"
#guard Command.commandName ({} : PGetFullAXTree) = "Accessibility.getFullAXTree"
#guard
    (decodeAs "{\"nodes\": []}" (α := GetFullAXTree)).map (· == ({ nodes := [] } : GetFullAXTree))
  = .ok true

#guard encode (ToJSON.toJSON ({} : PGetRootAXNode)) = "{}"
#guard encode (ToJSON.toJSON ({ frameId := some "f1" } : PGetRootAXNode)) = "{\"frameId\":\"f1\"}"
#guard Command.commandName ({} : PGetRootAXNode) = "Accessibility.getRootAXNode"
#guard
    (decodeAs "{\"node\": {\"nodeId\": \"1\", \"ignored\": false}}" (α := GetRootAXNode)).map
      (· == ({ node := { nodeId := "1", ignored := false } } : GetRootAXNode))
  = .ok true

#guard encode (ToJSON.toJSON ({} : PGetAXNodeAndAncestors)) = "{}"
#guard encode (ToJSON.toJSON ({ backendNodeId := some 5 } : PGetAXNodeAndAncestors))
  = "{\"backendNodeId\":5}"
#guard Command.commandName ({} : PGetAXNodeAndAncestors) = "Accessibility.getAXNodeAndAncestors"
#guard
    (decodeAs "{\"nodes\": []}" (α := GetAXNodeAndAncestors)).map (· == ({ nodes := [] } : GetAXNodeAndAncestors))
  = .ok true

#guard encode (ToJSON.toJSON ({ id := "1" } : PGetChildAXNodes)) = "{\"id\":\"1\"}"
#guard encode (ToJSON.toJSON ({ id := "1", frameId := some "f1" } : PGetChildAXNodes))
  = "{\"id\":\"1\",\"frameId\":\"f1\"}"
#guard Command.commandName ({ id := "1" } : PGetChildAXNodes) = "Accessibility.getChildAXNodes"
#guard
    (decodeAs "{\"nodes\": []}" (α := GetChildAXNodes)).map (· == ({ nodes := [] } : GetChildAXNodes))
  = .ok true

#guard encode (ToJSON.toJSON ({} : PQueryAXTree)) = "{}"
#guard encode (ToJSON.toJSON ({ accessibleName := some "OK", role := some "button" } : PQueryAXTree))
  = "{\"accessibleName\":\"OK\",\"role\":\"button\"}"
#guard Command.commandName ({} : PQueryAXTree) = "Accessibility.queryAXTree"
#guard
    (decodeAs "{\"nodes\": []}" (α := QueryAXTree)).map (· == ({ nodes := [] } : QueryAXTree))
  = .ok true

end Tests.CDP.Domains.Accessibility
