/-
  Tests for `Linen.CDP.Domains.DOMSnapshot`.
-/
import Linen.CDP.Domains.DOMSnapshot

open CDP.Domains.DOMSnapshot
open CDP.Domains.DOMDebugger (EventListener)
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.DOMSnapshot

/-! ### NameValue -/

#guard decodeAs "{\"name\": \"id\", \"value\": \"main\"}" (α := NameValue)
  = .ok { name := "id", value := "main" }
#guard encode (ToJSON.toJSON ({ name := "id", value := "main" } : NameValue))
  = "{\"name\":\"id\",\"value\":\"main\"}"

/-! ### InlineTextBox -/

#guard decodeAs
    "{\"boundingBox\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2}, \"startCharacterIndex\": 0, \"numCharacters\": 5}"
    (α := InlineTextBox)
  = .ok { boundingBox := { x := 0, y := 0, width := 1, height := 2 }
        , startCharacterIndex := 0, numCharacters := 5 }

/-! ### LayoutTreeNode -/

#guard decodeAs
    "{\"domNodeIndex\": 0, \"boundingBox\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2}}"
    (α := LayoutTreeNode)
  = .ok { domNodeIndex := 0, boundingBox := { x := 0, y := 0, width := 1, height := 2 } }
#guard encode
    (ToJSON.toJSON
      ({ domNodeIndex := 0, boundingBox := { x := 0, y := 0, width := 1, height := 2 }
       , isStackingContext := some true } : LayoutTreeNode))
  = "{\"domNodeIndex\":0,\"boundingBox\":{\"x\":0,\"y\":0,\"width\":1,\"height\":2},\"isStackingContext\":true}"

/-! ### ComputedStyle -/

#guard decodeAs "{\"properties\": [{\"name\": \"color\", \"value\": \"red\"}]}" (α := ComputedStyle)
  = .ok { properties := [{ name := "color", value := "red" }] }

/-! ### Rare*Data -/

#guard decodeAs "{\"index\": [0, 1], \"value\": [2, 3]}" (α := RareStringData)
  = .ok { index := [0, 1], value := [2, 3] }
#guard decodeAs "{\"index\": [0, 1]}" (α := RareBooleanData) = .ok { index := [0, 1] }
#guard decodeAs "{\"index\": [0], \"value\": [7]}" (α := RareIntegerData)
  = .ok { index := [0], value := [7] }

/-! ### DOMNode — embeds `DOMDebugger.EventListener`, which only derives
    `BEq` (no `DecidableEq`), so `DOMNode` equality checks match on the
    decode result explicitly. -/

#guard
  match decodeAs
      "{\"nodeType\": 1, \"nodeName\": \"DIV\", \"nodeValue\": \"\", \"backendNodeId\": 42}"
      (α := DOMNode) with
  | .ok v => v == { nodeType := 1, nodeName := "DIV", nodeValue := "", backendNodeId := 42 }
  | .error _ => false

#guard
  match decodeAs
      ("{\"nodeType\": 3, \"nodeName\": \"#text\", \"nodeValue\": \"hi\", \"backendNodeId\": 1, " ++
       "\"eventListeners\": [{\"type\": \"click\", \"useCapture\": true, \"passive\": false, " ++
       "\"once\": false, \"scriptId\": \"1\", \"lineNumber\": 0, \"columnNumber\": 0}]}")
      (α := DOMNode) with
  | .ok v =>
    v.eventListeners ==
      some
        [{ type := "click", useCapture := true, passive := false, once := false
           scriptId := "1", lineNumber := 0, columnNumber := 0 : EventListener }]
  | .error _ => false

#guard
  encode
      (ToJSON.toJSON
        ({ nodeType := 1, nodeName := "DIV", nodeValue := "", backendNodeId := 42
         , pseudoType := some .before } : DOMNode))
  = "{\"nodeType\":1,\"nodeName\":\"DIV\",\"nodeValue\":\"\",\"backendNodeId\":42,\"pseudoType\":\"before\"}"

/-! ### NodeTreeSnapshot / LayoutTreeSnapshot / TextBoxSnapshot -/

#guard decodeAs "{}" (α := NodeTreeSnapshot) = .ok {}

#guard decodeAs
    ("{\"nodeIndex\": [0], \"styles\": [[0]], \"bounds\": [[0, 0, 1, 1]], \"text\": [0], " ++
     "\"stackingContexts\": {\"index\": []}}")
    (α := LayoutTreeSnapshot)
  = .ok
      { nodeIndex := [0], styles := [[0]], bounds := [[0, 0, 1, 1]], text := [0]
      , stackingContexts := { index := [] } }

#guard decodeAs "{\"layoutIndex\": [0], \"bounds\": [[0, 0, 1, 1]], \"start\": [0], \"length\": [1]}"
    (α := TextBoxSnapshot)
  = .ok { layoutIndex := [0], bounds := [[0, 0, 1, 1]], start := [0], length := [1] }

/-! ### DocumentSnapshot -/

#guard decodeAs
    ("{\"documentURL\": 0, \"title\": 1, \"baseURL\": 2, \"contentLanguage\": 3, " ++
     "\"encodingName\": 4, \"publicId\": 5, \"systemId\": 6, \"frameId\": 7, " ++
     "\"nodes\": {}, " ++
     "\"layout\": {\"nodeIndex\": [], \"styles\": [], \"bounds\": [], \"text\": [], " ++
     "\"stackingContexts\": {\"index\": []}}, " ++
     "\"textBoxes\": {\"layoutIndex\": [], \"bounds\": [], \"start\": [], \"length\": []}}")
    (α := DocumentSnapshot)
  = .ok
      { documentURL := 0, title := 1, baseURL := 2, contentLanguage := 3, encodingName := 4
      , publicId := 5, systemId := 6, frameId := 7, nodes := {}
      , layout := { nodeIndex := [], styles := [], bounds := [], text := []
                   , stackingContexts := { index := [] } }
      , textBoxes := { layoutIndex := [], bounds := [], start := [], length := [] } }

/-! ### Commands -/

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PDisable) = "DOMSnapshot.disable"
#guard Command.commandName ({} : PEnable) = "DOMSnapshot.enable"

#guard encode (ToJSON.toJSON ({ computedStyles := ["color"] } : PCaptureSnapshot))
  = "{\"computedStyles\":[\"color\"]}"
#guard encode
    (ToJSON.toJSON
      ({ computedStyles := ["color"], includePaintOrder := some true } : PCaptureSnapshot))
  = "{\"computedStyles\":[\"color\"],\"includePaintOrder\":true}"
#guard Command.commandName ({ computedStyles := [] } : PCaptureSnapshot) = "DOMSnapshot.captureSnapshot"

#guard decodeAs "{\"documents\": [], \"strings\": [\"a\", \"b\"]}" (α := CaptureSnapshot)
  = .ok { documents := [], strings := ["a", "b"] }

end Tests.CDP.Domains.DOMSnapshot
