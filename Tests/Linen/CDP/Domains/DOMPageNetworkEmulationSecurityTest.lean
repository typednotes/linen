/-
  Tests for `Linen.CDP.Domains.DOMPageNetworkEmulationSecurity`.

  Auto-generated breadth tests: at least one `#guard` per type, command and
  event across the five sub-domains (enums check an exact decode/encode; plain
  types round-trip through `encode`/`decodeAs`; responses and events decode a
  minimal synthesized payload; commands check `commandName`; events check
  `eventName`). Types without `DecidableEq` use the `match … | .ok _ => true`
  idiom, as in `HeapProfilerTest`.
-/
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity

open CDP.Domains.DOMPageNetworkEmulationSecurity
open CDP.Internal.Utils (Command Event)
open Data.Json (Value ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.DOMPageNetworkEmulationSecurity


-- ── DOM ──
#guard match decodeAs (encode (ToJSON.toJSON ({ nodeType := 0, nodeName := "x", backendNodeId := 0 } : DOM.BackendNode))) (α := DOM.BackendNode) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"first-line\"" (α := DOM.PseudoType) = .ok .firstLine
#guard encode (ToJSON.toJSON (DOM.PseudoType.firstLine)) = "\"first-line\""
#guard decodeAs "\"user-agent\"" (α := DOM.ShadowRootType) = .ok .userAgent
#guard encode (ToJSON.toJSON (DOM.ShadowRootType.userAgent)) = "\"user-agent\""
#guard decodeAs "\"QuirksMode\"" (α := DOM.CompatibilityMode) = .ok .quirksMode
#guard encode (ToJSON.toJSON (DOM.CompatibilityMode.quirksMode)) = "\"QuirksMode\""
#guard match decodeAs (encode (ToJSON.toJSON ({ nodeId := 0, backendNodeId := 0, nodeType := 0, nodeName := "x", localName := "x", nodeValue := "x" } : DOM.Node))) (α := DOM.Node) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ r := 0, g := 0, b := 0 } : DOM.RGBA))) (α := DOM.RGBA) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ content := [], padding := [], border := [], margin := [], width := 0, height := 0 } : DOM.BoxModel))) (α := DOM.BoxModel) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ bounds := [], shape := [], marginShape := [] } : DOM.ShapeOutsideInfo))) (α := DOM.ShapeOutsideInfo) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ x := 0, y := 0, width := 0, height := 0 } : DOM.Rect))) (α := DOM.Rect) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ name := "x", value := "x" } : DOM.CSSComputedStyleProperty))) (α := DOM.CSSComputedStyleProperty) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.AttributeModified)) = "DOM.attributeModified"
#guard match decodeAs "{\"nodeId\":0,\"name\":\"x\",\"value\":\"x\"}" (α := DOM.AttributeModified) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.AttributeRemoved)) = "DOM.attributeRemoved"
#guard match decodeAs "{\"nodeId\":0,\"name\":\"x\"}" (α := DOM.AttributeRemoved) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.CharacterDataModified)) = "DOM.characterDataModified"
#guard match decodeAs "{\"nodeId\":0,\"characterData\":\"x\"}" (α := DOM.CharacterDataModified) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.ChildNodeCountUpdated)) = "DOM.childNodeCountUpdated"
#guard match decodeAs "{\"nodeId\":0,\"childNodeCount\":0}" (α := DOM.ChildNodeCountUpdated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.ChildNodeInserted)) = "DOM.childNodeInserted"
#guard match decodeAs "{\"parentNodeId\":0,\"previousNodeId\":0,\"node\":{\"nodeId\":0,\"backendNodeId\":0,\"nodeType\":0,\"nodeName\":\"x\",\"localName\":\"x\",\"nodeValue\":\"x\"}}" (α := DOM.ChildNodeInserted) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.ChildNodeRemoved)) = "DOM.childNodeRemoved"
#guard match decodeAs "{\"parentNodeId\":0,\"nodeId\":0}" (α := DOM.ChildNodeRemoved) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.DistributedNodesUpdated)) = "DOM.distributedNodesUpdated"
#guard match decodeAs "{\"insertionPointId\":0,\"distributedNodes\":[]}" (α := DOM.DistributedNodesUpdated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.DocumentUpdated)) = "DOM.documentUpdated"
#guard match decodeAs "{}" (α := DOM.DocumentUpdated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.InlineStyleInvalidated)) = "DOM.inlineStyleInvalidated"
#guard match decodeAs "{\"nodeIds\":[]}" (α := DOM.InlineStyleInvalidated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.PseudoElementAdded)) = "DOM.pseudoElementAdded"
#guard match decodeAs "{\"parentId\":0,\"pseudoElement\":{\"nodeId\":0,\"backendNodeId\":0,\"nodeType\":0,\"nodeName\":\"x\",\"localName\":\"x\",\"nodeValue\":\"x\"}}" (α := DOM.PseudoElementAdded) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.TopLayerElementsUpdated)) = "DOM.topLayerElementsUpdated"
#guard match decodeAs "{}" (α := DOM.TopLayerElementsUpdated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.PseudoElementRemoved)) = "DOM.pseudoElementRemoved"
#guard match decodeAs "{\"parentId\":0,\"pseudoElementId\":0}" (α := DOM.PseudoElementRemoved) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.SetChildNodes)) = "DOM.setChildNodes"
#guard match decodeAs "{\"parentId\":0,\"nodes\":[]}" (α := DOM.SetChildNodes) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.ShadowRootPopped)) = "DOM.shadowRootPopped"
#guard match decodeAs "{\"hostId\":0,\"rootId\":0}" (α := DOM.ShadowRootPopped) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := DOM.ShadowRootPushed)) = "DOM.shadowRootPushed"
#guard match decodeAs "{\"hostId\":0,\"root\":{\"nodeId\":0,\"backendNodeId\":0,\"nodeType\":0,\"nodeName\":\"x\",\"localName\":\"x\",\"nodeValue\":\"x\"}}" (α := DOM.ShadowRootPushed) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0 } : DOM.PCollectClassNamesFromSubtree) = "DOM.collectClassNamesFromSubtree"
#guard match decodeAs "{\"classNames\":[]}" (α := DOM.CollectClassNamesFromSubtree) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0, targetNodeId := 0 } : DOM.PCopyTo) = "DOM.copyTo"
#guard match decodeAs "{\"nodeId\":0}" (α := DOM.CopyTo) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PDescribeNode) = "DOM.describeNode"
#guard match decodeAs "{\"node\":{\"nodeId\":0,\"backendNodeId\":0,\"nodeType\":0,\"nodeName\":\"x\",\"localName\":\"x\",\"nodeValue\":\"x\"}}" (α := DOM.DescribeNode) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PScrollIntoViewIfNeeded) = "DOM.scrollIntoViewIfNeeded"
#guard Command.commandName ({} : DOM.PDisable) = "DOM.disable"
#guard Command.commandName ({ searchId := "x" } : DOM.PDiscardSearchResults) = "DOM.discardSearchResults"
#guard decodeAs "\"none\"" (α := DOM.PEnableIncludeWhitespace) = .ok .none
#guard encode (ToJSON.toJSON (DOM.PEnableIncludeWhitespace.none)) = "\"none\""
#guard Command.commandName ({} : DOM.PEnable) = "DOM.enable"
#guard Command.commandName ({} : DOM.PFocus) = "DOM.focus"
#guard Command.commandName ({ nodeId := 0 } : DOM.PGetAttributes) = "DOM.getAttributes"
#guard match decodeAs "{\"attributes\":[]}" (α := DOM.GetAttributes) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PGetBoxModel) = "DOM.getBoxModel"
#guard match decodeAs "{\"model\":{\"content\":[],\"padding\":[],\"border\":[],\"margin\":[],\"width\":0,\"height\":0}}" (α := DOM.GetBoxModel) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PGetContentQuads) = "DOM.getContentQuads"
#guard match decodeAs "{\"quads\":[]}" (α := DOM.GetContentQuads) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PGetDocument) = "DOM.getDocument"
#guard match decodeAs "{\"root\":{\"nodeId\":0,\"backendNodeId\":0,\"nodeType\":0,\"nodeName\":\"x\",\"localName\":\"x\",\"nodeValue\":\"x\"}}" (α := DOM.GetDocument) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0, computedStyles := [] } : DOM.PGetNodesForSubtreeByStyle) = "DOM.getNodesForSubtreeByStyle"
#guard match decodeAs "{\"nodeIds\":[]}" (α := DOM.GetNodesForSubtreeByStyle) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ x := 0, y := 0 } : DOM.PGetNodeForLocation) = "DOM.getNodeForLocation"
#guard match decodeAs "{\"backendNodeId\":0,\"frameId\":\"x\"}" (α := DOM.GetNodeForLocation) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PGetOuterHTML) = "DOM.getOuterHTML"
#guard match decodeAs "{\"outerHTML\":\"x\"}" (α := DOM.GetOuterHTML) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0 } : DOM.PGetRelayoutBoundary) = "DOM.getRelayoutBoundary"
#guard match decodeAs "{\"nodeId\":0}" (α := DOM.GetRelayoutBoundary) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ searchId := "x", fromIndex := 0, toIndex := 0 } : DOM.PGetSearchResults) = "DOM.getSearchResults"
#guard match decodeAs "{\"nodeIds\":[]}" (α := DOM.GetSearchResults) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PHideHighlight) = "DOM.hideHighlight"
#guard Command.commandName ({} : DOM.PHighlightNode) = "DOM.highlightNode"
#guard Command.commandName ({} : DOM.PHighlightRect) = "DOM.highlightRect"
#guard Command.commandName ({} : DOM.PMarkUndoableState) = "DOM.markUndoableState"
#guard Command.commandName ({ nodeId := 0, targetNodeId := 0 } : DOM.PMoveTo) = "DOM.moveTo"
#guard match decodeAs "{\"nodeId\":0}" (α := DOM.MoveTo) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ query := "x" } : DOM.PPerformSearch) = "DOM.performSearch"
#guard match decodeAs "{\"searchId\":\"x\",\"resultCount\":0}" (α := DOM.PerformSearch) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ path := "x" } : DOM.PPushNodeByPathToFrontend) = "DOM.pushNodeByPathToFrontend"
#guard match decodeAs "{\"nodeId\":0}" (α := DOM.PushNodeByPathToFrontend) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ backendNodeIds := [] } : DOM.PPushNodesByBackendIdsToFrontend) = "DOM.pushNodesByBackendIdsToFrontend"
#guard match decodeAs "{\"nodeIds\":[]}" (α := DOM.PushNodesByBackendIdsToFrontend) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0, selector := "x" } : DOM.PQuerySelector) = "DOM.querySelector"
#guard match decodeAs "{\"nodeId\":0}" (α := DOM.QuerySelector) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0, selector := "x" } : DOM.PQuerySelectorAll) = "DOM.querySelectorAll"
#guard match decodeAs "{\"nodeIds\":[]}" (α := DOM.QuerySelectorAll) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PGetTopLayerElements) = "DOM.getTopLayerElements"
#guard match decodeAs "{\"nodeIds\":[]}" (α := DOM.GetTopLayerElements) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PRedo) = "DOM.redo"
#guard Command.commandName ({ nodeId := 0, name := "x" } : DOM.PRemoveAttribute) = "DOM.removeAttribute"
#guard Command.commandName ({ nodeId := 0 } : DOM.PRemoveNode) = "DOM.removeNode"
#guard Command.commandName ({ nodeId := 0 } : DOM.PRequestChildNodes) = "DOM.requestChildNodes"
#guard Command.commandName ({ objectId := "x" } : DOM.PRequestNode) = "DOM.requestNode"
#guard match decodeAs "{\"nodeId\":0}" (α := DOM.RequestNode) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : DOM.PResolveNode) = "DOM.resolveNode"
#guard match decodeAs "{\"object\":{\"type\":\"object\"}}" (α := DOM.ResolveNode) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0, name := "x", value := "x" } : DOM.PSetAttributeValue) = "DOM.setAttributeValue"
#guard Command.commandName ({ nodeId := 0, text := "x" } : DOM.PSetAttributesAsText) = "DOM.setAttributesAsText"
#guard Command.commandName ({ files := [] } : DOM.PSetFileInputFiles) = "DOM.setFileInputFiles"
#guard Command.commandName ({ enable := false } : DOM.PSetNodeStackTracesEnabled) = "DOM.setNodeStackTracesEnabled"
#guard Command.commandName ({ nodeId := 0 } : DOM.PGetNodeStackTraces) = "DOM.getNodeStackTraces"
#guard match decodeAs "{}" (α := DOM.GetNodeStackTraces) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ objectId := "x" } : DOM.PGetFileInfo) = "DOM.getFileInfo"
#guard match decodeAs "{\"path\":\"x\"}" (α := DOM.GetFileInfo) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0 } : DOM.PSetInspectedNode) = "DOM.setInspectedNode"
#guard Command.commandName ({ nodeId := 0, name := "x" } : DOM.PSetNodeName) = "DOM.setNodeName"
#guard match decodeAs "{\"nodeId\":0}" (α := DOM.SetNodeName) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0, value := "x" } : DOM.PSetNodeValue) = "DOM.setNodeValue"
#guard Command.commandName ({ nodeId := 0, outerHTML := "x" } : DOM.PSetOuterHTML) = "DOM.setOuterHTML"
#guard Command.commandName ({} : DOM.PUndo) = "DOM.undo"
#guard Command.commandName ({ frameId := "x" } : DOM.PGetFrameOwner) = "DOM.getFrameOwner"
#guard match decodeAs "{\"backendNodeId\":0}" (α := DOM.GetFrameOwner) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0 } : DOM.PGetContainerForNode) = "DOM.getContainerForNode"
#guard match decodeAs "{}" (α := DOM.GetContainerForNode) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ nodeId := 0 } : DOM.PGetQueryingDescendantsForContainer) = "DOM.getQueryingDescendantsForContainer"
#guard match decodeAs "{\"nodeIds\":[]}" (α := DOM.GetQueryingDescendantsForContainer) with
  | .ok _ => true | .error _ => false

-- ── Emulation ──
#guard decodeAs "\"portraitPrimary\"" (α := Emulation.ScreenOrientationType) = .ok .portraitPrimary
#guard encode (ToJSON.toJSON (Emulation.ScreenOrientationType.portraitPrimary)) = "\"portraitPrimary\""
#guard match decodeAs (encode (ToJSON.toJSON ({ type := .portraitPrimary, angle := 0 } : Emulation.ScreenOrientation))) (α := Emulation.ScreenOrientation) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"vertical\"" (α := Emulation.DisplayFeatureOrientation) = .ok .vertical
#guard encode (ToJSON.toJSON (Emulation.DisplayFeatureOrientation.vertical)) = "\"vertical\""
#guard match decodeAs (encode (ToJSON.toJSON ({ orientation := .vertical, offset := 0, maskLength := 0 } : Emulation.DisplayFeature))) (α := Emulation.DisplayFeature) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ name := "x", value := "x" } : Emulation.MediaFeature))) (α := Emulation.MediaFeature) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"advance\"" (α := Emulation.VirtualTimePolicy) = .ok .advance
#guard encode (ToJSON.toJSON (Emulation.VirtualTimePolicy.advance)) = "\"advance\""
#guard match decodeAs (encode (ToJSON.toJSON ({ brand := "x", version := "x" } : Emulation.UserAgentBrandVersion))) (α := Emulation.UserAgentBrandVersion) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ platform := "x", platformVersion := "x", architecture := "x", model := "x", mobile := false } : Emulation.UserAgentMetadata))) (α := Emulation.UserAgentMetadata) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"avif\"" (α := Emulation.DisabledImageType) = .ok .avif
#guard encode (ToJSON.toJSON (Emulation.DisabledImageType.avif)) = "\"avif\""
#guard (Event.eventName (α := Emulation.VirtualTimeBudgetExpired)) = "Emulation.virtualTimeBudgetExpired"
#guard match decodeAs "{}" (α := Emulation.VirtualTimeBudgetExpired) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Emulation.PCanEmulate) = "Emulation.canEmulate"
#guard match decodeAs "{\"result\":false}" (α := Emulation.CanEmulate) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Emulation.PClearDeviceMetricsOverride) = "Emulation.clearDeviceMetricsOverride"
#guard Command.commandName ({} : Emulation.PClearGeolocationOverride) = "Emulation.clearGeolocationOverride"
#guard Command.commandName ({} : Emulation.PResetPageScaleFactor) = "Emulation.resetPageScaleFactor"
#guard Command.commandName ({ enabled := false } : Emulation.PSetFocusEmulationEnabled) = "Emulation.setFocusEmulationEnabled"
#guard Command.commandName ({} : Emulation.PSetAutoDarkModeOverride) = "Emulation.setAutoDarkModeOverride"
#guard Command.commandName ({ rate := 0 } : Emulation.PSetCPUThrottlingRate) = "Emulation.setCPUThrottlingRate"
#guard Command.commandName ({} : Emulation.PSetDefaultBackgroundColorOverride) = "Emulation.setDefaultBackgroundColorOverride"
#guard Command.commandName ({ width := 0, height := 0, deviceScaleFactor := 0, mobile := false } : Emulation.PSetDeviceMetricsOverride) = "Emulation.setDeviceMetricsOverride"
#guard Command.commandName ({ hidden := false } : Emulation.PSetScrollbarsHidden) = "Emulation.setScrollbarsHidden"
#guard Command.commandName ({ disabled := false } : Emulation.PSetDocumentCookieDisabled) = "Emulation.setDocumentCookieDisabled"
#guard decodeAs "\"mobile\"" (α := Emulation.PSetEmitTouchEventsForMouseConfiguration) = .ok .mobile
#guard encode (ToJSON.toJSON (Emulation.PSetEmitTouchEventsForMouseConfiguration.mobile)) = "\"mobile\""
#guard Command.commandName ({ enabled := false } : Emulation.PSetEmitTouchEventsForMouse) = "Emulation.setEmitTouchEventsForMouse"
#guard Command.commandName ({} : Emulation.PSetEmulatedMedia) = "Emulation.setEmulatedMedia"
#guard decodeAs "\"none\"" (α := Emulation.PSetEmulatedVisionDeficiencyType) = .ok .none
#guard encode (ToJSON.toJSON (Emulation.PSetEmulatedVisionDeficiencyType.none)) = "\"none\""
#guard Command.commandName ({ type := .none } : Emulation.PSetEmulatedVisionDeficiency) = "Emulation.setEmulatedVisionDeficiency"
#guard Command.commandName ({} : Emulation.PSetGeolocationOverride) = "Emulation.setGeolocationOverride"
#guard Command.commandName ({ isUserActive := false, isScreenUnlocked := false } : Emulation.PSetIdleOverride) = "Emulation.setIdleOverride"
#guard Command.commandName ({} : Emulation.PClearIdleOverride) = "Emulation.clearIdleOverride"
#guard Command.commandName ({ pageScaleFactor := 0 } : Emulation.PSetPageScaleFactor) = "Emulation.setPageScaleFactor"
#guard Command.commandName ({ value := false } : Emulation.PSetScriptExecutionDisabled) = "Emulation.setScriptExecutionDisabled"
#guard Command.commandName ({ enabled := false } : Emulation.PSetTouchEmulationEnabled) = "Emulation.setTouchEmulationEnabled"
#guard Command.commandName ({ policy := .advance } : Emulation.PSetVirtualTimePolicy) = "Emulation.setVirtualTimePolicy"
#guard match decodeAs "{\"virtualTimeTicksBase\":0}" (α := Emulation.SetVirtualTimePolicy) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Emulation.PSetLocaleOverride) = "Emulation.setLocaleOverride"
#guard Command.commandName ({ timezoneId := "x" } : Emulation.PSetTimezoneOverride) = "Emulation.setTimezoneOverride"
#guard Command.commandName ({ imageTypes := [] } : Emulation.PSetDisabledImageTypes) = "Emulation.setDisabledImageTypes"
#guard Command.commandName ({ hardwareConcurrency := 0 } : Emulation.PSetHardwareConcurrencyOverride) = "Emulation.setHardwareConcurrencyOverride"
#guard Command.commandName ({ userAgent := "x" } : Emulation.PSetUserAgentOverride) = "Emulation.setUserAgentOverride"
#guard Command.commandName ({ enabled := false } : Emulation.PSetAutomationOverride) = "Emulation.setAutomationOverride"

-- ── Network ──
#guard decodeAs "\"Document\"" (α := Network.ResourceType) = .ok .document
#guard encode (ToJSON.toJSON (Network.ResourceType.document)) = "\"Document\""
#guard decodeAs "\"Failed\"" (α := Network.ErrorReason) = .ok .failed
#guard encode (ToJSON.toJSON (Network.ErrorReason.failed)) = "\"Failed\""
#guard decodeAs "\"none\"" (α := Network.ConnectionType) = .ok .none
#guard encode (ToJSON.toJSON (Network.ConnectionType.none)) = "\"none\""
#guard decodeAs "\"Strict\"" (α := Network.CookieSameSite) = .ok .strict
#guard encode (ToJSON.toJSON (Network.CookieSameSite.strict)) = "\"Strict\""
#guard decodeAs "\"Low\"" (α := Network.CookiePriority) = .ok .low
#guard encode (ToJSON.toJSON (Network.CookiePriority.low)) = "\"Low\""
#guard decodeAs "\"Unset\"" (α := Network.CookieSourceScheme) = .ok .unset
#guard encode (ToJSON.toJSON (Network.CookieSourceScheme.unset)) = "\"Unset\""
#guard match decodeAs (encode (ToJSON.toJSON ({ requestTime := 0, proxyStart := 0, proxyEnd := 0, dnsStart := 0, dnsEnd := 0, connectStart := 0, connectEnd := 0, sslStart := 0, sslEnd := 0, workerStart := 0, workerReady := 0, workerFetchStart := 0, workerRespondWithSettled := 0, sendStart := 0, sendEnd := 0, pushStart := 0, pushEnd := 0, receiveHeadersEnd := 0 } : Network.ResourceTiming))) (α := Network.ResourceTiming) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"VeryLow\"" (α := Network.ResourcePriority) = .ok .veryLow
#guard encode (ToJSON.toJSON (Network.ResourcePriority.veryLow)) = "\"VeryLow\""
#guard match decodeAs (encode (ToJSON.toJSON ({} : Network.PostDataEntry))) (α := Network.PostDataEntry) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"unsafe-url\"" (α := Network.RequestReferrerPolicy) = .ok .unsafeUrl
#guard encode (ToJSON.toJSON (Network.RequestReferrerPolicy.unsafeUrl)) = "\"unsafe-url\""
#guard match decodeAs (encode (ToJSON.toJSON ({ url := "x", method := "x", headers := [], initialPriority := .veryLow, referrerPolicy := .unsafeUrl } : Network.Request))) (α := Network.Request) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ status := "x", origin := "x", logDescription := "x", logId := "x", timestamp := 0, hashAlgorithm := "x", signatureAlgorithm := "x", signatureData := "x" } : Network.SignedCertificateTimestamp))) (α := Network.SignedCertificateTimestamp) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ protocol := "x", keyExchange := "x", cipher := "x", certificateId := 0, subjectName := "x", sanList := [], issuer := "x", validFrom := 0, validTo := 0, signedCertificateTimestampList := [], certificateTransparencyCompliance := .unknown, encryptedClientHello := false } : Network.SecurityDetails))) (α := Network.SecurityDetails) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"unknown\"" (α := Network.CertificateTransparencyCompliance) = .ok .unknown
#guard encode (ToJSON.toJSON (Network.CertificateTransparencyCompliance.unknown)) = "\"unknown\""
#guard decodeAs "\"other\"" (α := Network.BlockedReason) = .ok .other
#guard encode (ToJSON.toJSON (Network.BlockedReason.other)) = "\"other\""
#guard decodeAs "\"DisallowedByMode\"" (α := Network.CorsError) = .ok .disallowedByMode
#guard encode (ToJSON.toJSON (Network.CorsError.disallowedByMode)) = "\"DisallowedByMode\""
#guard match decodeAs (encode (ToJSON.toJSON ({ corsError := .disallowedByMode, failedParameter := "x" } : Network.CorsErrorStatus))) (α := Network.CorsErrorStatus) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"cache-storage\"" (α := Network.ServiceWorkerResponseSource) = .ok .cacheStorage
#guard encode (ToJSON.toJSON (Network.ServiceWorkerResponseSource.cacheStorage)) = "\"cache-storage\""
#guard decodeAs "\"UseCached\"" (α := Network.TrustTokenParamsRefreshPolicy) = .ok .useCached
#guard encode (ToJSON.toJSON (Network.TrustTokenParamsRefreshPolicy.useCached)) = "\"UseCached\""
#guard match decodeAs (encode (ToJSON.toJSON ({ type := .issuance, refreshPolicy := .useCached } : Network.TrustTokenParams))) (α := Network.TrustTokenParams) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Issuance\"" (α := Network.TrustTokenOperationType) = .ok .issuance
#guard encode (ToJSON.toJSON (Network.TrustTokenOperationType.issuance)) = "\"Issuance\""
#guard decodeAs "\"alternativeJobWonWithoutRace\"" (α := Network.AlternateProtocolUsage) = .ok .alternativeJobWonWithoutRace
#guard encode (ToJSON.toJSON (Network.AlternateProtocolUsage.alternativeJobWonWithoutRace)) = "\"alternativeJobWonWithoutRace\""
#guard match decodeAs (encode (ToJSON.toJSON ({ url := "x", status := 0, statusText := "x", headers := [], mimeType := "x", connectionReused := false, connectionId := 0, encodedDataLength := 0, securityState := .unknown } : Network.Response))) (α := Network.Response) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ headers := [] } : Network.WebSocketRequest))) (α := Network.WebSocketRequest) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ status := 0, statusText := "x", headers := [] } : Network.WebSocketResponse))) (α := Network.WebSocketResponse) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ opcode := 0, mask := false, payloadData := "x" } : Network.WebSocketFrame))) (α := Network.WebSocketFrame) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ url := "x", type := .document, bodySize := 0 } : Network.CachedResource))) (α := Network.CachedResource) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"parser\"" (α := Network.InitiatorType) = .ok .parser
#guard encode (ToJSON.toJSON (Network.InitiatorType.parser)) = "\"parser\""
#guard match decodeAs (encode (ToJSON.toJSON ({ type := .parser } : Network.Initiator))) (α := Network.Initiator) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ name := "x", value := "x", domain := "x", path := "x", expires := 0, size := 0, httpOnly := false, secure := false, session := false, priority := .low, sameParty := false, sourceScheme := .unset, sourcePort := 0 } : Network.Cookie))) (α := Network.Cookie) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"SecureOnly\"" (α := Network.SetCookieBlockedReason) = .ok .secureOnly
#guard encode (ToJSON.toJSON (Network.SetCookieBlockedReason.secureOnly)) = "\"SecureOnly\""
#guard decodeAs "\"SecureOnly\"" (α := Network.CookieBlockedReason) = .ok .secureOnly
#guard encode (ToJSON.toJSON (Network.CookieBlockedReason.secureOnly)) = "\"SecureOnly\""
#guard match decodeAs (encode (ToJSON.toJSON ({ blockedReasons := [], cookieLine := "x" } : Network.BlockedSetCookieWithReason))) (α := Network.BlockedSetCookieWithReason) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ blockedReasons := [], cookie := { name := "x", value := "x", domain := "x", path := "x", expires := 0, size := 0, httpOnly := false, secure := false, session := false, priority := .low, sameParty := false, sourceScheme := .unset, sourcePort := 0 } } : Network.BlockedCookieWithReason))) (α := Network.BlockedCookieWithReason) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ name := "x", value := "x" } : Network.CookieParam))) (α := Network.CookieParam) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Server\"" (α := Network.AuthChallengeSource) = .ok .server
#guard encode (ToJSON.toJSON (Network.AuthChallengeSource.server)) = "\"Server\""
#guard match decodeAs (encode (ToJSON.toJSON ({ origin := "x", scheme := "x", realm := "x" } : Network.AuthChallenge))) (α := Network.AuthChallenge) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Default\"" (α := Network.AuthChallengeResponseResponse) = .ok .default
#guard encode (ToJSON.toJSON (Network.AuthChallengeResponseResponse.default)) = "\"Default\""
#guard match decodeAs (encode (ToJSON.toJSON ({ response := .default } : Network.AuthChallengeResponse))) (α := Network.AuthChallengeResponse) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Request\"" (α := Network.InterceptionStage) = .ok .request
#guard encode (ToJSON.toJSON (Network.InterceptionStage.request)) = "\"Request\""
#guard match decodeAs (encode (ToJSON.toJSON ({} : Network.RequestPattern))) (α := Network.RequestPattern) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ label := "x", signature := "x", integrity := "x", validityUrl := "x", date := 0, expires := 0 } : Network.SignedExchangeSignature))) (α := Network.SignedExchangeSignature) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ requestUrl := "x", responseCode := 0, responseHeaders := [], signatures := [], headerIntegrity := "x" } : Network.SignedExchangeHeader))) (α := Network.SignedExchangeHeader) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"signatureSig\"" (α := Network.SignedExchangeErrorField) = .ok .signatureSig
#guard encode (ToJSON.toJSON (Network.SignedExchangeErrorField.signatureSig)) = "\"signatureSig\""
#guard match decodeAs (encode (ToJSON.toJSON ({ message := "x" } : Network.SignedExchangeError))) (α := Network.SignedExchangeError) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ outerResponse := { url := "x", status := 0, statusText := "x", headers := [], mimeType := "x", connectionReused := false, connectionId := 0, encodedDataLength := 0, securityState := .unknown } } : Network.SignedExchangeInfo))) (α := Network.SignedExchangeInfo) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"deflate\"" (α := Network.ContentEncoding) = .ok .deflate
#guard encode (ToJSON.toJSON (Network.ContentEncoding.deflate)) = "\"deflate\""
#guard decodeAs "\"Allow\"" (α := Network.PrivateNetworkRequestPolicy) = .ok .allow
#guard encode (ToJSON.toJSON (Network.PrivateNetworkRequestPolicy.allow)) = "\"Allow\""
#guard decodeAs "\"Local\"" (α := Network.IPAddressSpace) = .ok .«local»
#guard encode (ToJSON.toJSON (Network.IPAddressSpace.«local»)) = "\"Local\""
#guard match decodeAs (encode (ToJSON.toJSON ({ requestTime := 0 } : Network.ConnectTiming))) (α := Network.ConnectTiming) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ initiatorIsSecureContext := false, initiatorIPAddressSpace := .«local», privateNetworkRequestPolicy := .allow } : Network.ClientSecurityState))) (α := Network.ClientSecurityState) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"SameOrigin\"" (α := Network.CrossOriginOpenerPolicyValue) = .ok .sameOrigin
#guard encode (ToJSON.toJSON (Network.CrossOriginOpenerPolicyValue.sameOrigin)) = "\"SameOrigin\""
#guard match decodeAs (encode (ToJSON.toJSON ({ value := .sameOrigin, reportOnlyValue := .sameOrigin } : Network.CrossOriginOpenerPolicyStatus))) (α := Network.CrossOriginOpenerPolicyStatus) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"None\"" (α := Network.CrossOriginEmbedderPolicyValue) = .ok .none
#guard encode (ToJSON.toJSON (Network.CrossOriginEmbedderPolicyValue.none)) = "\"None\""
#guard match decodeAs (encode (ToJSON.toJSON ({ value := .none, reportOnlyValue := .none } : Network.CrossOriginEmbedderPolicyStatus))) (α := Network.CrossOriginEmbedderPolicyStatus) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({} : Network.SecurityIsolationStatus))) (α := Network.SecurityIsolationStatus) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Queued\"" (α := Network.ReportStatus) = .ok .queued
#guard encode (ToJSON.toJSON (Network.ReportStatus.queued)) = "\"Queued\""
#guard match decodeAs (encode (ToJSON.toJSON ({ id := "x", initiatorUrl := "x", destination := "x", type := "x", timestamp := 0, depth := 0, completedAttempts := 0, body := [], status := .queued } : Network.ReportingApiReport))) (α := Network.ReportingApiReport) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ url := "x", groupName := "x" } : Network.ReportingApiEndpoint))) (α := Network.ReportingApiEndpoint) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ success := false } : Network.LoadNetworkResourcePageResult))) (α := Network.LoadNetworkResourcePageResult) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ disableCache := false, includeCredentials := false } : Network.LoadNetworkResourceOptions))) (α := Network.LoadNetworkResourceOptions) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.DataReceived)) = "Network.dataReceived"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"dataLength\":0,\"encodedDataLength\":0}" (α := Network.DataReceived) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.EventSourceMessageReceived)) = "Network.eventSourceMessageReceived"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"eventName\":\"x\",\"eventId\":\"x\",\"data\":\"x\"}" (α := Network.EventSourceMessageReceived) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.LoadingFailed)) = "Network.loadingFailed"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"type\":\"Document\",\"errorText\":\"x\"}" (α := Network.LoadingFailed) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.LoadingFinished)) = "Network.loadingFinished"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"encodedDataLength\":0}" (α := Network.LoadingFinished) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.RequestServedFromCache)) = "Network.requestServedFromCache"
#guard match decodeAs "{\"requestId\":\"x\"}" (α := Network.RequestServedFromCache) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.RequestWillBeSent)) = "Network.requestWillBeSent"
#guard match decodeAs "{\"requestId\":\"x\",\"loaderId\":\"x\",\"documentURL\":\"x\",\"request\":{\"url\":\"x\",\"method\":\"x\",\"headers\":[],\"initialPriority\":\"VeryLow\",\"referrerPolicy\":\"unsafe-url\"},\"timestamp\":0,\"wallTime\":0,\"initiator\":{\"type\":\"parser\"},\"redirectHasExtraInfo\":false}" (α := Network.RequestWillBeSent) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.ResourceChangedPriority)) = "Network.resourceChangedPriority"
#guard match decodeAs "{\"requestId\":\"x\",\"newPriority\":\"VeryLow\",\"timestamp\":0}" (α := Network.ResourceChangedPriority) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.SignedExchangeReceived)) = "Network.signedExchangeReceived"
#guard match decodeAs "{\"requestId\":\"x\",\"info\":{\"outerResponse\":{\"url\":\"x\",\"status\":0,\"statusText\":\"x\",\"headers\":[],\"mimeType\":\"x\",\"connectionReused\":false,\"connectionId\":0,\"encodedDataLength\":0,\"securityState\":\"unknown\"}}}" (α := Network.SignedExchangeReceived) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.ResponseReceived)) = "Network.responseReceived"
#guard match decodeAs "{\"requestId\":\"x\",\"loaderId\":\"x\",\"timestamp\":0,\"type\":\"Document\",\"response\":{\"url\":\"x\",\"status\":0,\"statusText\":\"x\",\"headers\":[],\"mimeType\":\"x\",\"connectionReused\":false,\"connectionId\":0,\"encodedDataLength\":0,\"securityState\":\"unknown\"},\"hasExtraInfo\":false}" (α := Network.ResponseReceived) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebSocketClosed)) = "Network.webSocketClosed"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0}" (α := Network.WebSocketClosed) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebSocketCreated)) = "Network.webSocketCreated"
#guard match decodeAs "{\"requestId\":\"x\",\"url\":\"x\"}" (α := Network.WebSocketCreated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebSocketFrameError)) = "Network.webSocketFrameError"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"errorMessage\":\"x\"}" (α := Network.WebSocketFrameError) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebSocketFrameReceived)) = "Network.webSocketFrameReceived"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"response\":{\"opcode\":0,\"mask\":false,\"payloadData\":\"x\"}}" (α := Network.WebSocketFrameReceived) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebSocketFrameSent)) = "Network.webSocketFrameSent"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"response\":{\"opcode\":0,\"mask\":false,\"payloadData\":\"x\"}}" (α := Network.WebSocketFrameSent) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebSocketHandshakeResponseReceived)) = "Network.webSocketHandshakeResponseReceived"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"response\":{\"status\":0,\"statusText\":\"x\",\"headers\":[]}}" (α := Network.WebSocketHandshakeResponseReceived) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebSocketWillSendHandshakeRequest)) = "Network.webSocketWillSendHandshakeRequest"
#guard match decodeAs "{\"requestId\":\"x\",\"timestamp\":0,\"wallTime\":0,\"request\":{\"headers\":[]}}" (α := Network.WebSocketWillSendHandshakeRequest) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebTransportCreated)) = "Network.webTransportCreated"
#guard match decodeAs "{\"transportId\":\"x\",\"url\":\"x\",\"timestamp\":0}" (α := Network.WebTransportCreated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebTransportConnectionEstablished)) = "Network.webTransportConnectionEstablished"
#guard match decodeAs "{\"transportId\":\"x\",\"timestamp\":0}" (α := Network.WebTransportConnectionEstablished) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.WebTransportClosed)) = "Network.webTransportClosed"
#guard match decodeAs "{\"transportId\":\"x\",\"timestamp\":0}" (α := Network.WebTransportClosed) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.RequestWillBeSentExtraInfo)) = "Network.requestWillBeSentExtraInfo"
#guard match decodeAs "{\"requestId\":\"x\",\"associatedCookies\":[],\"headers\":[],\"connectTiming\":{\"requestTime\":0}}" (α := Network.RequestWillBeSentExtraInfo) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.ResponseReceivedExtraInfo)) = "Network.responseReceivedExtraInfo"
#guard match decodeAs "{\"requestId\":\"x\",\"blockedCookies\":[],\"headers\":[],\"resourceIPAddressSpace\":\"Local\",\"statusCode\":0}" (α := Network.ResponseReceivedExtraInfo) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Ok\"" (α := Network.TrustTokenOperationDoneStatus) = .ok .ok
#guard encode (ToJSON.toJSON (Network.TrustTokenOperationDoneStatus.ok)) = "\"Ok\""
#guard (Event.eventName (α := Network.TrustTokenOperationDone)) = "Network.trustTokenOperationDone"
#guard match decodeAs "{\"status\":\"Ok\",\"type\":\"Issuance\",\"requestId\":\"x\"}" (α := Network.TrustTokenOperationDone) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.SubresourceWebBundleMetadataReceived)) = "Network.subresourceWebBundleMetadataReceived"
#guard match decodeAs "{\"requestId\":\"x\",\"urls\":[]}" (α := Network.SubresourceWebBundleMetadataReceived) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.SubresourceWebBundleMetadataError)) = "Network.subresourceWebBundleMetadataError"
#guard match decodeAs "{\"requestId\":\"x\",\"errorMessage\":\"x\"}" (α := Network.SubresourceWebBundleMetadataError) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.SubresourceWebBundleInnerResponseParsed)) = "Network.subresourceWebBundleInnerResponseParsed"
#guard match decodeAs "{\"innerRequestId\":\"x\",\"innerRequestURL\":\"x\"}" (α := Network.SubresourceWebBundleInnerResponseParsed) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.SubresourceWebBundleInnerResponseError)) = "Network.subresourceWebBundleInnerResponseError"
#guard match decodeAs "{\"innerRequestId\":\"x\",\"innerRequestURL\":\"x\",\"errorMessage\":\"x\"}" (α := Network.SubresourceWebBundleInnerResponseError) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.ReportingApiReportAdded)) = "Network.reportingApiReportAdded"
#guard match decodeAs "{\"report\":{\"id\":\"x\",\"initiatorUrl\":\"x\",\"destination\":\"x\",\"type\":\"x\",\"timestamp\":0,\"depth\":0,\"completedAttempts\":0,\"body\":[],\"status\":\"Queued\"}}" (α := Network.ReportingApiReportAdded) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.ReportingApiReportUpdated)) = "Network.reportingApiReportUpdated"
#guard match decodeAs "{\"report\":{\"id\":\"x\",\"initiatorUrl\":\"x\",\"destination\":\"x\",\"type\":\"x\",\"timestamp\":0,\"depth\":0,\"completedAttempts\":0,\"body\":[],\"status\":\"Queued\"}}" (α := Network.ReportingApiReportUpdated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Network.ReportingApiEndpointsChangedForOrigin)) = "Network.reportingApiEndpointsChangedForOrigin"
#guard match decodeAs "{\"origin\":\"x\",\"endpoints\":[]}" (α := Network.ReportingApiEndpointsChangedForOrigin) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ encodings := [] } : Network.PSetAcceptedEncodings) = "Network.setAcceptedEncodings"
#guard Command.commandName ({} : Network.PClearAcceptedEncodingsOverride) = "Network.clearAcceptedEncodingsOverride"
#guard Command.commandName ({} : Network.PClearBrowserCache) = "Network.clearBrowserCache"
#guard Command.commandName ({} : Network.PClearBrowserCookies) = "Network.clearBrowserCookies"
#guard Command.commandName ({ name := "x" } : Network.PDeleteCookies) = "Network.deleteCookies"
#guard Command.commandName ({} : Network.PDisable) = "Network.disable"
#guard Command.commandName ({ offline := false, latency := 0, downloadThroughput := 0, uploadThroughput := 0 } : Network.PEmulateNetworkConditions) = "Network.emulateNetworkConditions"
#guard Command.commandName ({} : Network.PEnable) = "Network.enable"
#guard Command.commandName ({} : Network.PGetAllCookies) = "Network.getAllCookies"
#guard match decodeAs "{\"cookies\":[]}" (α := Network.GetAllCookies) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ origin := "x" } : Network.PGetCertificate) = "Network.getCertificate"
#guard match decodeAs "{\"tableNames\":[]}" (α := Network.GetCertificate) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Network.PGetCookies) = "Network.getCookies"
#guard match decodeAs "{\"cookies\":[]}" (α := Network.GetCookies) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ requestId := "x" } : Network.PGetResponseBody) = "Network.getResponseBody"
#guard match decodeAs "{\"body\":\"x\",\"base64Encoded\":false}" (α := Network.GetResponseBody) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ requestId := "x" } : Network.PGetRequestPostData) = "Network.getRequestPostData"
#guard match decodeAs "{\"postData\":\"x\"}" (α := Network.GetRequestPostData) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ interceptionId := "x" } : Network.PGetResponseBodyForInterception) = "Network.getResponseBodyForInterception"
#guard match decodeAs "{\"body\":\"x\",\"base64Encoded\":false}" (α := Network.GetResponseBodyForInterception) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ interceptionId := "x" } : Network.PTakeResponseBodyForInterceptionAsStream) = "Network.takeResponseBodyForInterceptionAsStream"
#guard match decodeAs "{\"stream\":\"x\"}" (α := Network.TakeResponseBodyForInterceptionAsStream) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ requestId := "x" } : Network.PReplayXHR) = "Network.replayXHR"
#guard Command.commandName ({ requestId := "x", query := "x" } : Network.PSearchInResponseBody) = "Network.searchInResponseBody"
#guard match decodeAs "{\"result\":[]}" (α := Network.SearchInResponseBody) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ urls := [] } : Network.PSetBlockedURLs) = "Network.setBlockedURLs"
#guard Command.commandName ({ bypass := false } : Network.PSetBypassServiceWorker) = "Network.setBypassServiceWorker"
#guard Command.commandName ({ cacheDisabled := false } : Network.PSetCacheDisabled) = "Network.setCacheDisabled"
#guard Command.commandName ({ name := "x", value := "x" } : Network.PSetCookie) = "Network.setCookie"
#guard Command.commandName ({ cookies := [] } : Network.PSetCookies) = "Network.setCookies"
#guard Command.commandName ({ headers := [] } : Network.PSetExtraHTTPHeaders) = "Network.setExtraHTTPHeaders"
#guard Command.commandName ({ enabled := false } : Network.PSetAttachDebugStack) = "Network.setAttachDebugStack"
#guard Command.commandName ({ userAgent := "x" } : Network.PSetUserAgentOverride) = "Network.setUserAgentOverride"
#guard Command.commandName ({} : Network.PGetSecurityIsolationStatus) = "Network.getSecurityIsolationStatus"
#guard match decodeAs "{\"status\":{}}" (α := Network.GetSecurityIsolationStatus) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ enable := false } : Network.PEnableReportingApi) = "Network.enableReportingApi"
#guard Command.commandName ({ url := "x", options := { disableCache := false, includeCredentials := false } } : Network.PLoadNetworkResource) = "Network.loadNetworkResource"
#guard match decodeAs "{\"resource\":{\"success\":false}}" (α := Network.LoadNetworkResource) with
  | .ok _ => true | .error _ => false

-- ── Page ──
#guard decodeAs "\"none\"" (α := Page.AdFrameType) = .ok .none
#guard encode (ToJSON.toJSON (Page.AdFrameType.none)) = "\"none\""
#guard decodeAs "\"ParentIsAd\"" (α := Page.AdFrameExplanation) = .ok .parentIsAd
#guard encode (ToJSON.toJSON (Page.AdFrameExplanation.parentIsAd)) = "\"ParentIsAd\""
#guard match decodeAs (encode (ToJSON.toJSON ({ adFrameType := .none } : Page.AdFrameStatus))) (α := Page.AdFrameStatus) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ scriptId := "x", debuggerId := "x" } : Page.AdScriptId))) (α := Page.AdScriptId) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Secure\"" (α := Page.SecureContextType) = .ok .secure
#guard encode (ToJSON.toJSON (Page.SecureContextType.secure)) = "\"Secure\""
#guard decodeAs "\"Isolated\"" (α := Page.CrossOriginIsolatedContextType) = .ok .isolated
#guard encode (ToJSON.toJSON (Page.CrossOriginIsolatedContextType.isolated)) = "\"Isolated\""
#guard decodeAs "\"SharedArrayBuffers\"" (α := Page.GatedAPIFeatures) = .ok .sharedArrayBuffers
#guard encode (ToJSON.toJSON (Page.GatedAPIFeatures.sharedArrayBuffers)) = "\"SharedArrayBuffers\""
#guard decodeAs "\"accelerometer\"" (α := Page.PermissionsPolicyFeature) = .ok .accelerometer
#guard encode (ToJSON.toJSON (Page.PermissionsPolicyFeature.accelerometer)) = "\"accelerometer\""
#guard decodeAs "\"Header\"" (α := Page.PermissionsPolicyBlockReason) = .ok .header
#guard encode (ToJSON.toJSON (Page.PermissionsPolicyBlockReason.header)) = "\"Header\""
#guard match decodeAs (encode (ToJSON.toJSON ({ frameId := "x", blockReason := .header } : Page.PermissionsPolicyBlockLocator))) (α := Page.PermissionsPolicyBlockLocator) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ feature := .accelerometer, allowed := false } : Page.PermissionsPolicyFeatureState))) (α := Page.PermissionsPolicyFeatureState) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Success\"" (α := Page.OriginTrialTokenStatus) = .ok .success
#guard encode (ToJSON.toJSON (Page.OriginTrialTokenStatus.success)) = "\"Success\""
#guard decodeAs "\"Enabled\"" (α := Page.OriginTrialStatus) = .ok .enabled
#guard encode (ToJSON.toJSON (Page.OriginTrialStatus.enabled)) = "\"Enabled\""
#guard decodeAs "\"None\"" (α := Page.OriginTrialUsageRestriction) = .ok .none
#guard encode (ToJSON.toJSON (Page.OriginTrialUsageRestriction.none)) = "\"None\""
#guard match decodeAs (encode (ToJSON.toJSON ({ origin := "x", matchSubDomains := false, trialName := "x", expiryTime := 0, isThirdParty := false, usageRestriction := .none } : Page.OriginTrialToken))) (α := Page.OriginTrialToken) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ rawTokenText := "x", status := .success } : Page.OriginTrialTokenWithStatus))) (α := Page.OriginTrialTokenWithStatus) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ trialName := "x", status := .enabled, tokensWithStatus := [] } : Page.OriginTrial))) (α := Page.OriginTrial) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ id := "x", loaderId := "x", url := "x", domainAndRegistry := "x", securityOrigin := "x", mimeType := "x", secureContextType := .secure, crossOriginIsolatedContextType := .isolated, gatedAPIFeatures := [] } : Page.Frame))) (α := Page.Frame) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ url := "x", type := .document, mimeType := "x" } : Page.FrameResource))) (α := Page.FrameResource) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ frame := { id := "x", loaderId := "x", url := "x", domainAndRegistry := "x", securityOrigin := "x", mimeType := "x", secureContextType := .secure, crossOriginIsolatedContextType := .isolated, gatedAPIFeatures := [] }, resources := [] } : Page.FrameResourceTree))) (α := Page.FrameResourceTree) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ frame := { id := "x", loaderId := "x", url := "x", domainAndRegistry := "x", securityOrigin := "x", mimeType := "x", secureContextType := .secure, crossOriginIsolatedContextType := .isolated, gatedAPIFeatures := [] } } : Page.FrameTree))) (α := Page.FrameTree) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"link\"" (α := Page.TransitionType) = .ok .link
#guard encode (ToJSON.toJSON (Page.TransitionType.link)) = "\"link\""
#guard match decodeAs (encode (ToJSON.toJSON ({ id := 0, url := "x", userTypedURL := "x", title := "x", transitionType := .link } : Page.NavigationEntry))) (α := Page.NavigationEntry) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ offsetTop := 0, pageScaleFactor := 0, deviceWidth := 0, deviceHeight := 0, scrollOffsetX := 0, scrollOffsetY := 0 } : Page.ScreencastFrameMetadata))) (α := Page.ScreencastFrameMetadata) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"alert\"" (α := Page.DialogType) = .ok .alert
#guard encode (ToJSON.toJSON (Page.DialogType.alert)) = "\"alert\""
#guard match decodeAs (encode (ToJSON.toJSON ({ message := "x", critical := 0, line := 0, column := 0 } : Page.AppManifestError))) (α := Page.AppManifestError) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ scope := "x" } : Page.AppManifestParsedProperties))) (α := Page.AppManifestParsedProperties) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ pageX := 0, pageY := 0, clientWidth := 0, clientHeight := 0 } : Page.LayoutViewport))) (α := Page.LayoutViewport) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ offsetX := 0, offsetY := 0, pageX := 0, pageY := 0, clientWidth := 0, clientHeight := 0, scale := 0 } : Page.VisualViewport))) (α := Page.VisualViewport) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ x := 0, y := 0, width := 0, height := 0, scale := 0 } : Page.Viewport))) (α := Page.Viewport) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({} : Page.FontFamilies))) (α := Page.FontFamilies) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ script := "x", fontFamilies := ({} : Page.FontFamilies) } : Page.ScriptFontFamilies))) (α := Page.ScriptFontFamilies) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({} : Page.FontSizes))) (α := Page.FontSizes) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"formSubmissionGet\"" (α := Page.ClientNavigationReason) = .ok .formSubmissionGet
#guard encode (ToJSON.toJSON (Page.ClientNavigationReason.formSubmissionGet)) = "\"formSubmissionGet\""
#guard decodeAs "\"currentTab\"" (α := Page.ClientNavigationDisposition) = .ok .currentTab
#guard encode (ToJSON.toJSON (Page.ClientNavigationDisposition.currentTab)) = "\"currentTab\""
#guard match decodeAs (encode (ToJSON.toJSON ({ name := "x", value := "x" } : Page.InstallabilityErrorArgument))) (α := Page.InstallabilityErrorArgument) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ errorId := "x", errorArguments := [] } : Page.InstallabilityError))) (α := Page.InstallabilityError) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"noReferrer\"" (α := Page.ReferrerPolicy) = .ok .noReferrer
#guard encode (ToJSON.toJSON (Page.ReferrerPolicy.noReferrer)) = "\"noReferrer\""
#guard match decodeAs (encode (ToJSON.toJSON ({ url := "x" } : Page.CompilationCacheParams))) (α := Page.CompilationCacheParams) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Navigation\"" (α := Page.NavigationType) = .ok .navigation
#guard encode (ToJSON.toJSON (Page.NavigationType.navigation)) = "\"Navigation\""
#guard decodeAs "\"NotPrimaryMainFrame\"" (α := Page.BackForwardCacheNotRestoredReason) = .ok .notPrimaryMainFrame
#guard encode (ToJSON.toJSON (Page.BackForwardCacheNotRestoredReason.notPrimaryMainFrame)) = "\"NotPrimaryMainFrame\""
#guard decodeAs "\"SupportPending\"" (α := Page.BackForwardCacheNotRestoredReasonType) = .ok .supportPending
#guard encode (ToJSON.toJSON (Page.BackForwardCacheNotRestoredReasonType.supportPending)) = "\"SupportPending\""
#guard match decodeAs (encode (ToJSON.toJSON ({ type := .supportPending, reason := .notPrimaryMainFrame } : Page.BackForwardCacheNotRestoredExplanation))) (α := Page.BackForwardCacheNotRestoredExplanation) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ url := "x", explanations := [], children := [] } : Page.BackForwardCacheNotRestoredExplanationTree))) (α := Page.BackForwardCacheNotRestoredExplanationTree) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"Activated\"" (α := Page.PrerenderFinalStatus) = .ok .activated
#guard encode (ToJSON.toJSON (Page.PrerenderFinalStatus.activated)) = "\"Activated\""
#guard (Event.eventName (α := Page.DomContentEventFired)) = "Page.domContentEventFired"
#guard match decodeAs "{\"timestamp\":0}" (α := Page.DomContentEventFired) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"selectSingle\"" (α := Page.FileChooserOpenedMode) = .ok .selectSingle
#guard encode (ToJSON.toJSON (Page.FileChooserOpenedMode.selectSingle)) = "\"selectSingle\""
#guard (Event.eventName (α := Page.FileChooserOpened)) = "Page.fileChooserOpened"
#guard match decodeAs "{\"frameId\":\"x\",\"mode\":\"selectSingle\"}" (α := Page.FileChooserOpened) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.FrameAttached)) = "Page.frameAttached"
#guard match decodeAs "{\"frameId\":\"x\",\"parentFrameId\":\"x\"}" (α := Page.FrameAttached) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"remove\"" (α := Page.FrameDetachedReason) = .ok .remove
#guard encode (ToJSON.toJSON (Page.FrameDetachedReason.remove)) = "\"remove\""
#guard (Event.eventName (α := Page.FrameDetached)) = "Page.frameDetached"
#guard match decodeAs "{\"frameId\":\"x\",\"reason\":\"remove\"}" (α := Page.FrameDetached) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.FrameNavigated)) = "Page.frameNavigated"
#guard match decodeAs "{\"frame\":{\"id\":\"x\",\"loaderId\":\"x\",\"url\":\"x\",\"domainAndRegistry\":\"x\",\"securityOrigin\":\"x\",\"mimeType\":\"x\",\"secureContextType\":\"Secure\",\"crossOriginIsolatedContextType\":\"Isolated\",\"gatedAPIFeatures\":[]},\"type\":\"Navigation\"}" (α := Page.FrameNavigated) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.DocumentOpened)) = "Page.documentOpened"
#guard match decodeAs "{\"frame\":{\"id\":\"x\",\"loaderId\":\"x\",\"url\":\"x\",\"domainAndRegistry\":\"x\",\"securityOrigin\":\"x\",\"mimeType\":\"x\",\"secureContextType\":\"Secure\",\"crossOriginIsolatedContextType\":\"Isolated\",\"gatedAPIFeatures\":[]}}" (α := Page.DocumentOpened) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.FrameResized)) = "Page.frameResized"
#guard match decodeAs "{}" (α := Page.FrameResized) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.FrameRequestedNavigation)) = "Page.frameRequestedNavigation"
#guard match decodeAs "{\"frameId\":\"x\",\"reason\":\"formSubmissionGet\",\"url\":\"x\",\"disposition\":\"currentTab\"}" (α := Page.FrameRequestedNavigation) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.FrameStartedLoading)) = "Page.frameStartedLoading"
#guard match decodeAs "{\"frameId\":\"x\"}" (α := Page.FrameStartedLoading) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.FrameStoppedLoading)) = "Page.frameStoppedLoading"
#guard match decodeAs "{\"frameId\":\"x\"}" (α := Page.FrameStoppedLoading) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.InterstitialHidden)) = "Page.interstitialHidden"
#guard match decodeAs "{}" (α := Page.InterstitialHidden) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.InterstitialShown)) = "Page.interstitialShown"
#guard match decodeAs "{}" (α := Page.InterstitialShown) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.JavascriptDialogClosed)) = "Page.javascriptDialogClosed"
#guard match decodeAs "{\"result\":false,\"userInput\":\"x\"}" (α := Page.JavascriptDialogClosed) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.JavascriptDialogOpening)) = "Page.javascriptDialogOpening"
#guard match decodeAs "{\"url\":\"x\",\"message\":\"x\",\"type\":\"alert\",\"hasBrowserHandler\":false}" (α := Page.JavascriptDialogOpening) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.LifecycleEvent)) = "Page.lifecycleEvent"
#guard match decodeAs "{\"frameId\":\"x\",\"loaderId\":\"x\",\"name\":\"x\",\"timestamp\":0}" (α := Page.LifecycleEvent) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.BackForwardCacheNotUsed)) = "Page.backForwardCacheNotUsed"
#guard match decodeAs "{\"loaderId\":\"x\",\"frameId\":\"x\",\"notRestoredExplanations\":[]}" (α := Page.BackForwardCacheNotUsed) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.PrerenderAttemptCompleted)) = "Page.prerenderAttemptCompleted"
#guard match decodeAs "{\"initiatingFrameId\":\"x\",\"prerenderingUrl\":\"x\",\"finalStatus\":\"Activated\"}" (α := Page.PrerenderAttemptCompleted) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.LoadEventFired)) = "Page.loadEventFired"
#guard match decodeAs "{\"timestamp\":0}" (α := Page.LoadEventFired) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.NavigatedWithinDocument)) = "Page.navigatedWithinDocument"
#guard match decodeAs "{\"frameId\":\"x\",\"url\":\"x\"}" (α := Page.NavigatedWithinDocument) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.ScreencastFrame)) = "Page.screencastFrame"
#guard match decodeAs "{\"data\":\"x\",\"metadata\":{\"offsetTop\":0,\"pageScaleFactor\":0,\"deviceWidth\":0,\"deviceHeight\":0,\"scrollOffsetX\":0,\"scrollOffsetY\":0},\"sessionId\":0}" (α := Page.ScreencastFrame) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.ScreencastVisibilityChanged)) = "Page.screencastVisibilityChanged"
#guard match decodeAs "{\"visible\":false}" (α := Page.ScreencastVisibilityChanged) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.WindowOpen)) = "Page.windowOpen"
#guard match decodeAs "{\"url\":\"x\",\"windowName\":\"x\",\"windowFeatures\":[],\"userGesture\":false}" (α := Page.WindowOpen) with
  | .ok _ => true | .error _ => false
#guard (Event.eventName (α := Page.CompilationCacheProduced)) = "Page.compilationCacheProduced"
#guard match decodeAs "{\"url\":\"x\",\"data\":\"x\"}" (α := Page.CompilationCacheProduced) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ source := "x" } : Page.PAddScriptToEvaluateOnNewDocument) = "Page.addScriptToEvaluateOnNewDocument"
#guard match decodeAs "{\"identifier\":\"x\"}" (α := Page.AddScriptToEvaluateOnNewDocument) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PBringToFront) = "Page.bringToFront"
#guard decodeAs "\"jpeg\"" (α := Page.PCaptureScreenshotFormat) = .ok .jpeg
#guard encode (ToJSON.toJSON (Page.PCaptureScreenshotFormat.jpeg)) = "\"jpeg\""
#guard Command.commandName ({} : Page.PCaptureScreenshot) = "Page.captureScreenshot"
#guard match decodeAs "{\"data\":\"x\"}" (α := Page.CaptureScreenshot) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"mhtml\"" (α := Page.PCaptureSnapshotFormat) = .ok .mhtml
#guard encode (ToJSON.toJSON (Page.PCaptureSnapshotFormat.mhtml)) = "\"mhtml\""
#guard Command.commandName ({} : Page.PCaptureSnapshot) = "Page.captureSnapshot"
#guard match decodeAs "{\"data\":\"x\"}" (α := Page.CaptureSnapshot) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ frameId := "x" } : Page.PCreateIsolatedWorld) = "Page.createIsolatedWorld"
#guard match decodeAs "{\"executionContextId\":0}" (α := Page.CreateIsolatedWorld) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PDisable) = "Page.disable"
#guard Command.commandName ({} : Page.PEnable) = "Page.enable"
#guard Command.commandName ({} : Page.PGetAppManifest) = "Page.getAppManifest"
#guard match decodeAs "{\"url\":\"x\",\"errors\":[]}" (α := Page.GetAppManifest) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PGetInstallabilityErrors) = "Page.getInstallabilityErrors"
#guard match decodeAs "{\"installabilityErrors\":[]}" (α := Page.GetInstallabilityErrors) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PGetManifestIcons) = "Page.getManifestIcons"
#guard match decodeAs "{}" (α := Page.GetManifestIcons) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PGetAppId) = "Page.getAppId"
#guard match decodeAs "{}" (α := Page.GetAppId) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ frameId := "x" } : Page.PGetAdScriptId) = "Page.getAdScriptId"
#guard match decodeAs "{}" (α := Page.GetAdScriptId) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PGetFrameTree) = "Page.getFrameTree"
#guard match decodeAs "{\"frameTree\":{\"frame\":{\"id\":\"x\",\"loaderId\":\"x\",\"url\":\"x\",\"domainAndRegistry\":\"x\",\"securityOrigin\":\"x\",\"mimeType\":\"x\",\"secureContextType\":\"Secure\",\"crossOriginIsolatedContextType\":\"Isolated\",\"gatedAPIFeatures\":[]}}}" (α := Page.GetFrameTree) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PGetLayoutMetrics) = "Page.getLayoutMetrics"
#guard match decodeAs "{\"cssLayoutViewport\":{\"pageX\":0,\"pageY\":0,\"clientWidth\":0,\"clientHeight\":0},\"cssVisualViewport\":{\"offsetX\":0,\"offsetY\":0,\"pageX\":0,\"pageY\":0,\"clientWidth\":0,\"clientHeight\":0,\"scale\":0},\"cssContentSize\":{\"x\":0,\"y\":0,\"width\":0,\"height\":0}}" (α := Page.GetLayoutMetrics) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PGetNavigationHistory) = "Page.getNavigationHistory"
#guard match decodeAs "{\"currentIndex\":0,\"entries\":[]}" (α := Page.GetNavigationHistory) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PResetNavigationHistory) = "Page.resetNavigationHistory"
#guard Command.commandName ({ frameId := "x", url := "x" } : Page.PGetResourceContent) = "Page.getResourceContent"
#guard match decodeAs "{\"content\":\"x\",\"base64Encoded\":false}" (α := Page.GetResourceContent) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PGetResourceTree) = "Page.getResourceTree"
#guard match decodeAs "{\"frameTree\":{\"frame\":{\"id\":\"x\",\"loaderId\":\"x\",\"url\":\"x\",\"domainAndRegistry\":\"x\",\"securityOrigin\":\"x\",\"mimeType\":\"x\",\"secureContextType\":\"Secure\",\"crossOriginIsolatedContextType\":\"Isolated\",\"gatedAPIFeatures\":[]},\"resources\":[]}}" (α := Page.GetResourceTree) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ accept := false } : Page.PHandleJavaScriptDialog) = "Page.handleJavaScriptDialog"
#guard Command.commandName ({ url := "x" } : Page.PNavigate) = "Page.navigate"
#guard match decodeAs "{\"frameId\":\"x\"}" (α := Page.Navigate) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ entryId := 0 } : Page.PNavigateToHistoryEntry) = "Page.navigateToHistoryEntry"
#guard decodeAs "\"ReturnAsBase64\"" (α := Page.PPrintToPDFTransferMode) = .ok .returnAsBase64
#guard encode (ToJSON.toJSON (Page.PPrintToPDFTransferMode.returnAsBase64)) = "\"ReturnAsBase64\""
#guard Command.commandName ({} : Page.PPrintToPDF) = "Page.printToPDF"
#guard match decodeAs "{\"data\":\"x\"}" (α := Page.PrintToPDF) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Page.PReload) = "Page.reload"
#guard Command.commandName ({ identifier := "x" } : Page.PRemoveScriptToEvaluateOnNewDocument) = "Page.removeScriptToEvaluateOnNewDocument"
#guard Command.commandName ({ sessionId := 0 } : Page.PScreencastFrameAck) = "Page.screencastFrameAck"
#guard Command.commandName ({ frameId := "x", url := "x", query := "x" } : Page.PSearchInResource) = "Page.searchInResource"
#guard match decodeAs "{\"result\":[]}" (α := Page.SearchInResource) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ enabled := false } : Page.PSetAdBlockingEnabled) = "Page.setAdBlockingEnabled"
#guard Command.commandName ({ enabled := false } : Page.PSetBypassCSP) = "Page.setBypassCSP"
#guard Command.commandName ({ frameId := "x" } : Page.PGetPermissionsPolicyState) = "Page.getPermissionsPolicyState"
#guard match decodeAs "{\"states\":[]}" (α := Page.GetPermissionsPolicyState) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ frameId := "x" } : Page.PGetOriginTrials) = "Page.getOriginTrials"
#guard match decodeAs "{\"originTrials\":[]}" (α := Page.GetOriginTrials) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({ fontFamilies := ({} : Page.FontFamilies) } : Page.PSetFontFamilies) = "Page.setFontFamilies"
#guard Command.commandName ({ fontSizes := ({} : Page.FontSizes) } : Page.PSetFontSizes) = "Page.setFontSizes"
#guard Command.commandName ({ frameId := "x", html := "x" } : Page.PSetDocumentContent) = "Page.setDocumentContent"
#guard Command.commandName ({ enabled := false } : Page.PSetLifecycleEventsEnabled) = "Page.setLifecycleEventsEnabled"
#guard decodeAs "\"jpeg\"" (α := Page.PStartScreencastFormat) = .ok .jpeg
#guard encode (ToJSON.toJSON (Page.PStartScreencastFormat.jpeg)) = "\"jpeg\""
#guard Command.commandName ({} : Page.PStartScreencast) = "Page.startScreencast"
#guard Command.commandName ({} : Page.PStopLoading) = "Page.stopLoading"
#guard Command.commandName ({} : Page.PCrash) = "Page.crash"
#guard Command.commandName ({} : Page.PClose) = "Page.close"
#guard decodeAs "\"frozen\"" (α := Page.PSetWebLifecycleStateState) = .ok .frozen
#guard encode (ToJSON.toJSON (Page.PSetWebLifecycleStateState.frozen)) = "\"frozen\""
#guard Command.commandName ({ state := .frozen } : Page.PSetWebLifecycleState) = "Page.setWebLifecycleState"
#guard Command.commandName ({} : Page.PStopScreencast) = "Page.stopScreencast"
#guard Command.commandName ({ scripts := [] } : Page.PProduceCompilationCache) = "Page.produceCompilationCache"
#guard Command.commandName ({ url := "x", data := "x" } : Page.PAddCompilationCache) = "Page.addCompilationCache"
#guard Command.commandName ({} : Page.PClearCompilationCache) = "Page.clearCompilationCache"
#guard decodeAs "\"none\"" (α := Page.PSetSPCTransactionModeMode) = .ok .none
#guard encode (ToJSON.toJSON (Page.PSetSPCTransactionModeMode.none)) = "\"none\""
#guard Command.commandName ({ mode := .none } : Page.PSetSPCTransactionMode) = "Page.setSPCTransactionMode"
#guard Command.commandName ({ message := "x" } : Page.PGenerateTestReport) = "Page.generateTestReport"
#guard Command.commandName ({} : Page.PWaitForDebugger) = "Page.waitForDebugger"
#guard Command.commandName ({ enabled := false } : Page.PSetInterceptFileChooserDialog) = "Page.setInterceptFileChooserDialog"

-- ── Security ──
#guard decodeAs "\"blockable\"" (α := Security.MixedContentType) = .ok .blockable
#guard encode (ToJSON.toJSON (Security.MixedContentType.blockable)) = "\"blockable\""
#guard decodeAs "\"unknown\"" (α := Security.SecurityState) = .ok .unknown
#guard encode (ToJSON.toJSON (Security.SecurityState.unknown)) = "\"unknown\""
#guard match decodeAs (encode (ToJSON.toJSON ({ protocol := "x", keyExchange := "x", cipher := "x", certificate := [], subjectName := "x", issuer := "x", validFrom := 0, validTo := 0, certificateHasWeakSignature := false, certificateHasSha1Signature := false, modernSSL := false, obsoleteSslProtocol := false, obsoleteSslKeyExchange := false, obsoleteSslCipher := false, obsoleteSslSignature := false } : Security.CertificateSecurityState))) (α := Security.CertificateSecurityState) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"badReputation\"" (α := Security.SafetyTipStatus) = .ok .badReputation
#guard encode (ToJSON.toJSON (Security.SafetyTipStatus.badReputation)) = "\"badReputation\""
#guard match decodeAs (encode (ToJSON.toJSON ({ safetyTipStatus := .badReputation } : Security.SafetyTipInfo))) (α := Security.SafetyTipInfo) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ securityState := .unknown, securityStateIssueIds := [] } : Security.VisibleSecurityState))) (α := Security.VisibleSecurityState) with
  | .ok _ => true | .error _ => false
#guard match decodeAs (encode (ToJSON.toJSON ({ securityState := .unknown, title := "x", summary := "x", description := "x", mixedContentType := .blockable, certificate := [] } : Security.SecurityStateExplanation))) (α := Security.SecurityStateExplanation) with
  | .ok _ => true | .error _ => false
#guard decodeAs "\"continue\"" (α := Security.CertificateErrorAction) = .ok .continue
#guard encode (ToJSON.toJSON (Security.CertificateErrorAction.continue)) = "\"continue\""
#guard (Event.eventName (α := Security.VisibleSecurityStateChanged)) = "Security.visibleSecurityStateChanged"
#guard match decodeAs "{\"visibleSecurityState\":{\"securityState\":\"unknown\",\"securityStateIssueIds\":[]}}" (α := Security.VisibleSecurityStateChanged) with
  | .ok _ => true | .error _ => false
#guard Command.commandName ({} : Security.PDisable) = "Security.disable"
#guard Command.commandName ({} : Security.PEnable) = "Security.enable"
#guard Command.commandName ({ ignore := false } : Security.PSetIgnoreCertificateErrors) = "Security.setIgnoreCertificateErrors"

end Tests.CDP.Domains.DOMPageNetworkEmulationSecurity
