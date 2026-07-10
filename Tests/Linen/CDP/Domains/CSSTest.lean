/-
  Tests for `Linen.CDP.Domains.CSS`.

  Types that transitively contain the self-referential `CSSProperty`/
  `CSSLayerData` (and hence derive only `Repr, BEq`, no `DecidableEq`) use the
  `match … | .ok _ => true | .error _ => false` round-trip idiom instead of a
  direct `= .ok …` equality guard, matching
  `DOMPageNetworkEmulationSecurityTest.lean`'s convention.
-/
import Linen.CDP.Domains.CSS

open CDP.Domains.CSS
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.CSS

-- ── Simple leaf types ──

#guard decodeAs "\"regular\"" (α := StyleSheetOrigin) = .ok .regular
#guard encode (ToJSON.toJSON StyleSheetOrigin.userAgent) = "\"user-agent\""

#guard decodeAs "{\"startLine\":0,\"startColumn\":1,\"endLine\":2,\"endColumn\":3}" (α := SourceRange)
  = .ok { startLine := 0, startColumn := 1, endLine := 2, endColumn := 3 }
#guard encode (ToJSON.toJSON ({ startLine := 0, startColumn := 1, endLine := 2, endColumn := 3 } : SourceRange))
  = "{\"startLine\":0,\"startColumn\":1,\"endLine\":2,\"endColumn\":3}"

#guard decodeAs "{\"name\":\"margin\",\"value\":\"1px\"}" (α := ShorthandEntry)
  = .ok { name := "margin", value := "1px" }

#guard decodeAs "{\"name\":\"color\",\"value\":\"red\"}" (α := CSSComputedStyleProperty)
  = .ok { name := "color", value := "red" }

#guard decodeAs "{\"familyName\":\"Arial\",\"isCustomFont\":false,\"glyphCount\":3}" (α := PlatformFontUsage)
  = .ok { familyName := "Arial", isCustomFont := false, glyphCount := 3 }

#guard decodeAs "{\"tag\":\"wght\",\"name\":\"Weight\",\"minValue\":1,\"maxValue\":9,\"defaultValue\":4}"
    (α := FontVariationAxis)
  = .ok { tag := "wght", name := "Weight", minValue := 1, maxValue := 9, defaultValue := 4 }

#guard decodeAs
    ("{\"fontFamily\":\"Arial\",\"fontStyle\":\"normal\",\"fontVariant\":\"normal\","
    ++ "\"fontWeight\":\"400\",\"fontStretch\":\"normal\",\"fontDisplay\":\"auto\","
    ++ "\"unicodeRange\":\"U+0-10FFFF\",\"src\":\"local\",\"platformFontFamily\":\"Arial\"}")
    (α := FontFace)
  = .ok
    { fontFamily := "Arial", fontStyle := "normal", fontVariant := "normal", fontWeight := "400"
      fontStretch := "normal", fontDisplay := "auto", unicodeRange := "U+0-10FFFF", src := "local"
      platformFontFamily := "Arial" }

#guard decodeAs "\"mediaRule\"" (α := CSSMediaSource) = .ok .mediaRule
#guard encode (ToJSON.toJSON CSSMediaSource.inlineSheet) = "\"inlineSheet\""

#guard decodeAs "{\"styleSheetId\":\"1\",\"startOffset\":0,\"endOffset\":1,\"used\":true}" (α := RuleUsage)
  = .ok { styleSheetId := "1", startOffset := 0, endOffset := 1, used := true }

#guard decodeAs
    "{\"styleSheetId\":\"1\",\"range\":{\"startLine\":0,\"startColumn\":0,\"endLine\":0,\"endColumn\":1},\"text\":\"x\"}"
    (α := StyleDeclarationEdit)
  = .ok
    { styleSheetId := "1"
      range := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 1 }
      text := "x" }

#guard decodeAs "{\"text\":\".foo\"}" (α := CSSValue) = .ok { text := ".foo" }
#guard encode (ToJSON.toJSON ({ text := ".foo" } : CSSValue)) = "{\"text\":\".foo\"}"

#guard decodeAs "{\"selectors\":[{\"text\":\".foo\"}],\"text\":\".foo\"}" (α := SelectorList)
  = .ok { selectors := [{ text := ".foo" }], text := ".foo" }

#guard decodeAs "{\"value\":600,\"unit\":\"px\",\"feature\":\"width\"}" (α := MediaQueryExpression)
  = .ok { value := 600, unit := "px", feature := "width" }

#guard decodeAs "{\"expressions\":[],\"active\":true}" (α := MediaQuery)
  = .ok { expressions := [], active := true }

#guard decodeAs "{\"text\":\"(min-width: 1px)\",\"source\":\"mediaRule\"}" (α := CSSMedia)
  = .ok { text := "(min-width: 1px)", source := .mediaRule }

#guard decodeAs "{\"text\":\"(min-width: 1px)\"}" (α := CSSContainerQuery)
  = .ok { text := "(min-width: 1px)" }

#guard decodeAs "{\"text\":\"(display: grid)\",\"active\":true}" (α := CSSSupports)
  = .ok { text := "(display: grid)", active := true }

#guard decodeAs "{\"text\":\".a\"}" (α := CSSScope) = .ok { text := ".a" }

#guard decodeAs "{\"text\":\"base\"}" (α := CSSLayer) = .ok { text := "base" }

-- ── Self-referential types ──

#guard match decodeAs "{\"name\":\"outer\",\"order\":1}" (α := CSSLayerData) with
  | .ok v => v == { name := "outer", order := 1 }
  | .error _ => false
#guard match decodeAs "{\"name\":\"outer\",\"subLayers\":[{\"name\":\"inner\",\"order\":2}],\"order\":1}"
    (α := CSSLayerData) with
  | .ok v => v == { name := "outer", subLayers := some [{ name := "inner", order := 2 }], order := 1 }
  | .error _ => false
#guard encode (ToJSON.toJSON ({ name := "outer", order := 1 } : CSSLayerData))
  = "{\"name\":\"outer\",\"order\":1}"
#guard encode
    (ToJSON.toJSON
      ({ name := "outer", subLayers := some [{ name := "inner", order := 2 }], order := 1 }
        : CSSLayerData))
  = "{\"name\":\"outer\",\"subLayers\":[{\"name\":\"inner\",\"order\":2}],\"order\":1}"
#guard match decodeAs
    (encode
      (ToJSON.toJSON
        ({ name := "outer", subLayers := some [{ name := "inner", order := 2 }], order := 1 }
          : CSSLayerData)))
    (α := CSSLayerData) with
  | .ok v => v.subLayers.map (·.length) = some 1
  | .error _ => false

#guard match decodeAs "{\"name\":\"margin\",\"value\":\"1px\"}" (α := CSSProperty) with
  | .ok v => v == { name := "margin", value := "1px" }
  | .error _ => false
#guard match decodeAs
    "{\"name\":\"margin\",\"value\":\"1px 2px\",\"longhandProperties\":[{\"name\":\"margin-top\",\"value\":\"1px\"}]}"
    (α := CSSProperty) with
  | .ok v =>
    v == { name := "margin", value := "1px 2px"
           longhandProperties := some [{ name := "margin-top", value := "1px" }] }
  | .error _ => false
#guard encode (ToJSON.toJSON ({ name := "margin", value := "1px" } : CSSProperty))
  = "{\"name\":\"margin\",\"value\":\"1px\"}"
#guard match decodeAs
    (encode
      (ToJSON.toJSON
        ({ name := "margin", value := "1px 2px"
           longhandProperties := some [{ name := "margin-top", value := "1px" }] } : CSSProperty)))
    (α := CSSProperty) with
  | .ok v => v.longhandProperties.map (·.length) = some 1
  | .error _ => false

-- ── Types built on the above ──

#guard match decodeAs
    "{\"cssProperties\":[{\"name\":\"margin\",\"value\":\"1px\"}],\"shorthandEntries\":[]}"
    (α := CSSStyle) with
  | .ok _ => true | .error _ => false

#guard match decodeAs
    ("{\"selectorList\":{\"selectors\":[{\"text\":\".a\"}],\"text\":\".a\"},\"origin\":\"regular\","
    ++ "\"style\":{\"cssProperties\":[],\"shorthandEntries\":[]}}")
    (α := CSSRule) with
  | .ok _ => true | .error _ => false

#guard match decodeAs
    ("{\"rule\":{\"selectorList\":{\"selectors\":[],\"text\":\".a\"},\"origin\":\"regular\","
    ++ "\"style\":{\"cssProperties\":[],\"shorthandEntries\":[]}},\"matchingSelectors\":[0]}")
    (α := RuleMatch) with
  | .ok _ => true | .error _ => false

#guard match decodeAs "{\"pseudoType\":\"before\",\"matches\":[]}" (α := PseudoElementMatches) with
  | .ok _ => true | .error _ => false

#guard match decodeAs "{\"matchedCSSRules\":[]}" (α := InheritedStyleEntry) with
  | .ok _ => true | .error _ => false

#guard match decodeAs "{\"pseudoElements\":[]}" (α := InheritedPseudoElementMatches) with
  | .ok _ => true | .error _ => false

#guard decodeAs
    ("{\"styleSheetId\":\"1\",\"frameId\":\"f\",\"sourceURL\":\"\",\"origin\":\"regular\","
    ++ "\"title\":\"\",\"disabled\":false,\"isInline\":false,\"isMutable\":false,"
    ++ "\"isConstructed\":false,\"startLine\":0,\"startColumn\":0,\"length\":0,\"endLine\":0,"
    ++ "\"endColumn\":0}")
    (α := CSSStyleSheetHeader)
  = .ok
    { styleSheetId := "1", frameId := "f", sourceURL := "", origin := .regular, title := ""
      disabled := false, isInline := false, isMutable := false, isConstructed := false
      startLine := 0, startColumn := 0, length := 0, endLine := 0, endColumn := 0 }

#guard match decodeAs
    ("{\"origin\":\"regular\",\"keyText\":{\"text\":\"0%\"},"
    ++ "\"style\":{\"cssProperties\":[],\"shorthandEntries\":[]}}")
    (α := CSSKeyframeRule) with
  | .ok _ => true | .error _ => false

#guard match decodeAs "{\"animationName\":{\"text\":\"spin\"},\"keyframes\":[]}" (α := CSSKeyframesRule) with
  | .ok _ => true | .error _ => false

-- ── Events ──

#guard match decodeAs "{}" (α := FontsUpdated) with | .ok _ => true | .error _ => false
#guard Event.eventName (α := FontsUpdated) = "CSS.fontsUpdated"

#guard decodeAs "{}" (α := MediaQueryResultChanged) = .ok {}
#guard Event.eventName (α := MediaQueryResultChanged) = "CSS.mediaQueryResultChanged"

#guard match decodeAs
    ("{\"header\":{\"styleSheetId\":\"1\",\"frameId\":\"f\",\"sourceURL\":\"\",\"origin\":\"regular\","
    ++ "\"title\":\"\",\"disabled\":false,\"isInline\":false,\"isMutable\":false,"
    ++ "\"isConstructed\":false,\"startLine\":0,\"startColumn\":0,\"length\":0,\"endLine\":0,"
    ++ "\"endColumn\":0}}")
    (α := StyleSheetAdded) with
  | .ok _ => true | .error _ => false
#guard Event.eventName (α := StyleSheetAdded) = "CSS.styleSheetAdded"

#guard decodeAs "{\"styleSheetId\":\"1\"}" (α := StyleSheetChanged) = .ok { styleSheetId := "1" }
#guard Event.eventName (α := StyleSheetChanged) = "CSS.styleSheetChanged"

#guard decodeAs "{\"styleSheetId\":\"1\"}" (α := StyleSheetRemoved) = .ok { styleSheetId := "1" }
#guard Event.eventName (α := StyleSheetRemoved) = "CSS.styleSheetRemoved"

-- ── Commands ──

#guard encode
    (ToJSON.toJSON
      ({ styleSheetId := "1", ruleText := ".a{}"
         location := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 } } : PAddRule))
  = "{\"styleSheetId\":\"1\",\"ruleText\":\".a{}\",\"location\":{\"startLine\":0,\"startColumn\":0,\"endLine\":0,\"endColumn\":0}}"
#guard Command.commandName
    ({ styleSheetId := "1", ruleText := ".a{}"
       location := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 } } : PAddRule)
  = "CSS.addRule"
#guard match decodeAs
    ("{\"rule\":{\"selectorList\":{\"selectors\":[],\"text\":\".a\"},\"origin\":\"regular\","
    ++ "\"style\":{\"cssProperties\":[],\"shorthandEntries\":[]}}}")
    (α := AddRule) with
  | .ok _ => true | .error _ => false

#guard encode (ToJSON.toJSON ({ styleSheetId := "1" } : PCollectClassNames)) = "{\"styleSheetId\":\"1\"}"
#guard Command.commandName ({ styleSheetId := "1" } : PCollectClassNames) = "CSS.collectClassNames"
#guard decodeAs "{\"classNames\":[\"a\"]}" (α := CollectClassNames) = .ok { classNames := ["a"] }

#guard encode (ToJSON.toJSON ({ frameId := "f" } : PCreateStyleSheet)) = "{\"frameId\":\"f\"}"
#guard Command.commandName ({ frameId := "f" } : PCreateStyleSheet) = "CSS.createStyleSheet"
#guard decodeAs "{\"styleSheetId\":\"1\"}" (α := CreateStyleSheet) = .ok { styleSheetId := "1" }

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "CSS.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "CSS.enable"

#guard encode (ToJSON.toJSON ({ nodeId := 1, forcedPseudoClasses := ["hover"] } : PForcePseudoState))
  = "{\"nodeId\":1,\"forcedPseudoClasses\":[\"hover\"]}"
#guard Command.commandName ({ nodeId := 1, forcedPseudoClasses := ["hover"] } : PForcePseudoState)
  = "CSS.forcePseudoState"

#guard encode (ToJSON.toJSON ({ nodeId := 1 } : PGetBackgroundColors)) = "{\"nodeId\":1}"
#guard Command.commandName ({ nodeId := 1 } : PGetBackgroundColors) = "CSS.getBackgroundColors"
#guard decodeAs "{}" (α := GetBackgroundColors) = .ok {}

#guard encode (ToJSON.toJSON ({ nodeId := 1 } : PGetComputedStyleForNode)) = "{\"nodeId\":1}"
#guard Command.commandName ({ nodeId := 1 } : PGetComputedStyleForNode) = "CSS.getComputedStyleForNode"
#guard decodeAs "{\"computedStyle\":[]}" (α := GetComputedStyleForNode) = .ok { computedStyle := [] }

#guard encode (ToJSON.toJSON ({ nodeId := 1 } : PGetInlineStylesForNode)) = "{\"nodeId\":1}"
#guard Command.commandName ({ nodeId := 1 } : PGetInlineStylesForNode) = "CSS.getInlineStylesForNode"
#guard match decodeAs "{}" (α := GetInlineStylesForNode) with | .ok _ => true | .error _ => false

#guard encode (ToJSON.toJSON ({ nodeId := 1 } : PGetMatchedStylesForNode)) = "{\"nodeId\":1}"
#guard Command.commandName ({ nodeId := 1 } : PGetMatchedStylesForNode) = "CSS.getMatchedStylesForNode"
#guard match decodeAs "{}" (α := GetMatchedStylesForNode) with | .ok _ => true | .error _ => false

#guard encode (ToJSON.toJSON ({} : PGetMediaQueries)) = "null"
#guard Command.commandName ({} : PGetMediaQueries) = "CSS.getMediaQueries"
#guard decodeAs "{\"medias\":[]}" (α := GetMediaQueries) = .ok { medias := [] }

#guard encode (ToJSON.toJSON ({ nodeId := 1 } : PGetPlatformFontsForNode)) = "{\"nodeId\":1}"
#guard Command.commandName ({ nodeId := 1 } : PGetPlatformFontsForNode) = "CSS.getPlatformFontsForNode"
#guard decodeAs "{\"fonts\":[]}" (α := GetPlatformFontsForNode) = .ok { fonts := [] }

#guard encode (ToJSON.toJSON ({ styleSheetId := "1" } : PGetStyleSheetText)) = "{\"styleSheetId\":\"1\"}"
#guard Command.commandName ({ styleSheetId := "1" } : PGetStyleSheetText) = "CSS.getStyleSheetText"
#guard decodeAs "{\"text\":\"a{}\"}" (α := GetStyleSheetText) = .ok { text := "a{}" }

#guard encode (ToJSON.toJSON ({ nodeId := 1 } : PGetLayersForNode)) = "{\"nodeId\":1}"
#guard Command.commandName ({ nodeId := 1 } : PGetLayersForNode) = "CSS.getLayersForNode"
#guard match decodeAs "{\"rootLayer\":{\"name\":\"root\",\"order\":0}}" (α := GetLayersForNode) with
  | .ok _ => true | .error _ => false

#guard encode (ToJSON.toJSON ({ propertiesToTrack := [] } : PTrackComputedStyleUpdates))
  = "{\"propertiesToTrack\":[]}"
#guard Command.commandName ({ propertiesToTrack := [] } : PTrackComputedStyleUpdates)
  = "CSS.trackComputedStyleUpdates"

#guard encode (ToJSON.toJSON ({} : PTakeComputedStyleUpdates)) = "null"
#guard Command.commandName ({} : PTakeComputedStyleUpdates) = "CSS.takeComputedStyleUpdates"
#guard decodeAs "{\"nodeIds\":[1,2]}" (α := TakeComputedStyleUpdates) = .ok { nodeIds := [1, 2] }

#guard encode
    (ToJSON.toJSON
      ({ nodeId := 1, propertyName := "color", value := "red" } : PSetEffectivePropertyValueForNode))
  = "{\"nodeId\":1,\"propertyName\":\"color\",\"value\":\"red\"}"
#guard Command.commandName
    ({ nodeId := 1, propertyName := "color", value := "red" } : PSetEffectivePropertyValueForNode)
  = "CSS.setEffectivePropertyValueForNode"

#guard encode
    (ToJSON.toJSON
      ({ styleSheetId := "1", range := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 }
         keyText := "0%" } : PSetKeyframeKey))
  = "{\"styleSheetId\":\"1\",\"range\":{\"startLine\":0,\"startColumn\":0,\"endLine\":0,\"endColumn\":0},\"keyText\":\"0%\"}"
#guard Command.commandName
    ({ styleSheetId := "1", range := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 }
       keyText := "0%" } : PSetKeyframeKey)
  = "CSS.setKeyframeKey"
#guard decodeAs "{\"keyText\":{\"text\":\"0%\"}}" (α := SetKeyframeKey) = .ok { keyText := { text := "0%" } }

#guard Command.commandName
    ({ styleSheetId := "1", range := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 }
       text := "@media (min-width: 1px) {}" } : PSetMediaText)
  = "CSS.setMediaText"
#guard match decodeAs "{\"media\":{\"text\":\"x\",\"source\":\"mediaRule\"}}" (α := SetMediaText) with
  | .ok _ => true | .error _ => false

#guard Command.commandName
    ({ styleSheetId := "1", range := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 }
       text := "(min-width: 1px)" } : PSetContainerQueryText)
  = "CSS.setContainerQueryText"
#guard decodeAs "{\"containerQuery\":{\"text\":\"x\"}}" (α := SetContainerQueryText)
  = .ok { containerQuery := { text := "x" } }

#guard Command.commandName
    ({ styleSheetId := "1", range := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 }
       text := "(display: grid)" } : PSetSupportsText)
  = "CSS.setSupportsText"
#guard decodeAs "{\"supports\":{\"text\":\"x\",\"active\":true}}" (α := SetSupportsText)
  = .ok { supports := { text := "x", active := true } }

#guard Command.commandName
    ({ styleSheetId := "1", range := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 }
       text := ".a" } : PSetScopeText)
  = "CSS.setScopeText"
#guard decodeAs "{\"scope\":{\"text\":\"x\"}}" (α := SetScopeText) = .ok { scope := { text := "x" } }

#guard Command.commandName
    ({ styleSheetId := "1", range := { startLine := 0, startColumn := 0, endLine := 0, endColumn := 0 }
       selector := ".a" } : PSetRuleSelector)
  = "CSS.setRuleSelector"
#guard decodeAs "{\"selectorList\":{\"selectors\":[],\"text\":\".a\"}}" (α := SetRuleSelector)
  = .ok { selectorList := { selectors := [], text := ".a" } }

#guard encode (ToJSON.toJSON ({ styleSheetId := "1", text := "a{}" } : PSetStyleSheetText))
  = "{\"styleSheetId\":\"1\",\"text\":\"a{}\"}"
#guard Command.commandName ({ styleSheetId := "1", text := "a{}" } : PSetStyleSheetText)
  = "CSS.setStyleSheetText"
#guard decodeAs "{}" (α := SetStyleSheetText) = .ok {}

#guard Command.commandName ({ edits := [] } : PSetStyleTexts) = "CSS.setStyleTexts"
#guard match decodeAs "{\"styles\":[]}" (α := SetStyleTexts) with | .ok _ => true | .error _ => false

#guard encode (ToJSON.toJSON ({} : PStartRuleUsageTracking)) = "null"
#guard Command.commandName ({} : PStartRuleUsageTracking) = "CSS.startRuleUsageTracking"

#guard encode (ToJSON.toJSON ({} : PStopRuleUsageTracking)) = "null"
#guard Command.commandName ({} : PStopRuleUsageTracking) = "CSS.stopRuleUsageTracking"
#guard decodeAs "{\"ruleUsage\":[]}" (α := StopRuleUsageTracking) = .ok { ruleUsage := [] }

#guard encode (ToJSON.toJSON ({} : PTakeCoverageDelta)) = "null"
#guard Command.commandName ({} : PTakeCoverageDelta) = "CSS.takeCoverageDelta"
#guard decodeAs "{\"coverage\":[],\"timestamp\":1}" (α := TakeCoverageDelta)
  = .ok { coverage := [], timestamp := 1 }

#guard encode (ToJSON.toJSON ({ enabled := true } : PSetLocalFontsEnabled)) = "{\"enabled\":true}"
#guard Command.commandName ({ enabled := true } : PSetLocalFontsEnabled) = "CSS.setLocalFontsEnabled"

end Tests.CDP.Domains.CSS
