/-
  Tests for `Linen.CDP.Domains.Audits`.
-/
import Linen.CDP.Domains.Audits

open CDP.Domains.Audits
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Audits

-- ── Affected resources ──

#guard decodeAs "{\"name\": \"n\", \"path\": \"/\", \"domain\": \"d\"}" (α := AffectedCookie)
  = .ok { name := "n", path := "/", domain := "d" }
#guard decodeAs "{\"requestId\": \"r1\"}" (α := AffectedRequest) = .ok { requestId := "r1" }
#guard encode (ToJSON.toJSON ({ requestId := "r1", url := some "http://x" } : AffectedRequest))
  = "{\"requestId\":\"r1\",\"url\":\"http:\\/\\/x\"}"
#guard decodeAs "{\"frameId\": \"f1\"}" (α := AffectedFrame) = .ok { frameId := "f1" }

-- ── Cookie issues ──

#guard decodeAs "\"ExcludeSameSiteStrict\"" (α := CookieExclusionReason) = .ok .excludeSameSiteStrict
#guard encode (ToJSON.toJSON CookieExclusionReason.excludeDomainNonASCII) = "\"ExcludeDomainNonASCII\""
#guard decodeAs "\"WarnDomainNonASCII\"" (α := CookieWarningReason) = .ok .warnDomainNonASCII
#guard decodeAs "\"SetCookie\"" (α := CookieOperation) = .ok .setCookie
#guard encode (ToJSON.toJSON CookieOperation.readCookie) = "\"ReadCookie\""

#guard decodeAs
  "{\"cookieWarningReasons\": [], \"cookieExclusionReasons\": [\"ExcludeSameSiteStrict\"], \"operation\": \"SetCookie\"}"
  (α := CookieIssueDetails)
  = .ok { cookieWarningReasons := [], cookieExclusionReasons := [.excludeSameSiteStrict], operation := .setCookie }

-- ── Mixed content issues ──

#guard decodeAs "\"MixedContentBlocked\"" (α := MixedContentResolutionStatus) = .ok .mixedContentBlocked
#guard decodeAs "\"XMLHttpRequest\"" (α := MixedContentResourceType) = .ok .xMLHttpRequest
#guard encode (ToJSON.toJSON MixedContentResourceType.import_) = "\"Import\""
#guard decodeAs
  "{\"resolutionStatus\": \"MixedContentBlocked\", \"insecureURL\": \"http://a\", \"mainResourceURL\": \"http://b\"}"
  (α := MixedContentIssueDetails)
  = .ok { resolutionStatus := .mixedContentBlocked, insecureURL := "http://a", mainResourceURL := "http://b" }

-- ── Blocked-by-response issues ──

#guard decodeAs "\"CorpNotSameOrigin\"" (α := BlockedByResponseReason) = .ok .corpNotSameOrigin
#guard decodeAs "{\"request\": {\"requestId\": \"r1\"}, \"reason\": \"CorpNotSameOrigin\"}"
    (α := BlockedByResponseIssueDetails)
  = .ok { request := { requestId := "r1" }, reason := .corpNotSameOrigin }

-- ── Heavy ad issues ──

#guard decodeAs "\"HeavyAdBlocked\"" (α := HeavyAdResolutionStatus) = .ok .heavyAdBlocked
#guard decodeAs "\"CpuPeakLimit\"" (α := HeavyAdReason) = .ok .cpuPeakLimit
#guard decodeAs "{\"resolution\": \"HeavyAdBlocked\", \"reason\": \"CpuPeakLimit\", \"frame\": {\"frameId\": \"f1\"}}"
    (α := HeavyAdIssueDetails)
  = .ok { resolution := .heavyAdBlocked, reason := .cpuPeakLimit, frame := { frameId := "f1" } }

-- ── Content security policy issues ──

#guard decodeAs "\"kInlineViolation\"" (α := ContentSecurityPolicyViolationType) = .ok .kInlineViolation
#guard decodeAs "{\"url\": \"http://a\", \"lineNumber\": 1, \"columnNumber\": 2}" (α := SourceCodeLocation)
  = .ok { url := "http://a", lineNumber := 1, columnNumber := 2 }
#guard decodeAs
  "{\"violatedDirective\": \"script-src\", \"isReportOnly\": false, \"contentSecurityPolicyViolationType\": \"kInlineViolation\"}"
  (α := ContentSecurityPolicyIssueDetails)
  = .ok { violatedDirective := "script-src", isReportOnly := false
        , contentSecurityPolicyViolationType := .kInlineViolation }

-- ── SharedArrayBuffer issues ──

#guard decodeAs "\"TransferIssue\"" (α := SharedArrayBufferIssueType) = .ok .transferIssue
#guard decodeAs
  "{\"sourceCodeLocation\": {\"url\": \"u\", \"lineNumber\": 0, \"columnNumber\": 0}, \"isWarning\": true, \"type\": \"TransferIssue\"}"
  (α := SharedArrayBufferIssueDetails)
  = .ok { sourceCodeLocation := { url := "u", lineNumber := 0, columnNumber := 0 }, isWarning := true
        , type := .transferIssue }

-- ── Trusted Web Activity issues ──

#guard decodeAs "\"kHttpError\"" (α := TwaQualityEnforcementViolationType) = .ok .kHttpError
#guard decodeAs "{\"url\": \"http://a\", \"violationType\": \"kHttpError\"}" (α := TrustedWebActivityIssueDetails)
  = .ok { url := "http://a", violationType := .kHttpError }

-- ── Low text contrast issues ──

#guard decodeAs
  "{\"violatingNodeId\": 1, \"violatingNodeSelector\": \".a\", \"contrastRatio\": 1.5, \"thresholdAA\": 3, \"thresholdAAA\": 4.5, \"fontSize\": \"12px\", \"fontWeight\": \"400\"}"
  (α := LowTextContrastIssueDetails)
  = .ok { violatingNodeId := 1, violatingNodeSelector := ".a", contrastRatio := 1.5, thresholdAA := 3
        , thresholdAAA := 4.5, fontSize := "12px", fontWeight := "400" }

-- ── CORS issues ──

#guard decodeAs
  "{\"corsErrorStatus\": {\"corsError\": \"DisallowedByMode\", \"failedParameter\": \"p\"}, \"isWarning\": false, \"request\": {\"requestId\": \"r1\"}}"
  (α := CorsIssueDetails)
  = .ok { corsErrorStatus := { corsError := .disallowedByMode, failedParameter := "p" }, isWarning := false
        , request := { requestId := "r1" } }

-- ── Attribution reporting issues ──

#guard decodeAs "\"InsecureContext\"" (α := AttributionReportingIssueType) = .ok .insecureContext
#guard decodeAs "{\"violationType\": \"InsecureContext\"}" (α := AttributionReportingIssueDetails)
  = .ok { violationType := .insecureContext }

-- ── Quirks mode issues ──

#guard decodeAs
  "{\"isLimitedQuirksMode\": true, \"documentNodeId\": 1, \"url\": \"http://a\", \"frameId\": \"f1\", \"loaderId\": \"l1\"}"
  (α := QuirksModeIssueDetails)
  = .ok { isLimitedQuirksMode := true, documentNodeId := 1, url := "http://a", frameId := "f1", loaderId := "l1" }

-- ── Navigator user-agent issues ──

#guard decodeAs "{\"url\": \"http://a\"}" (α := NavigatorUserAgentIssueDetails) = .ok { url := "http://a" }

-- ── Generic issues ──

#guard decodeAs "\"CrossOriginPortalPostMessageError\"" (α := GenericIssueErrorType)
  = .ok .crossOriginPortalPostMessageError
#guard decodeAs "{\"errorType\": \"CrossOriginPortalPostMessageError\"}" (α := GenericIssueDetails)
  = .ok { errorType := .crossOriginPortalPostMessageError }

-- ── Deprecation issues ──

#guard decodeAs "\"XMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload\"" (α := DeprecationIssueType)
  = .ok .xMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload
#guard encode (ToJSON.toJSON DeprecationIssueType.rTCPeerConnectionSdpSemanticsPlanB)
  = "\"RTCPeerConnectionSdpSemanticsPlanB\""
#guard decodeAs
  "{\"sourceCodeLocation\": {\"url\": \"u\", \"lineNumber\": 0, \"columnNumber\": 0}, \"type\": \"EventPath\"}"
  (α := DeprecationIssueDetails)
  = .ok { sourceCodeLocation := { url := "u", lineNumber := 0, columnNumber := 0 }, type := .eventPath }

-- ── Client hint issues ──

#guard decodeAs "\"MetaTagModifiedHTML\"" (α := ClientHintIssueReason) = .ok .metaTagModifiedHTML
#guard decodeAs
  "{\"sourceCodeLocation\": {\"url\": \"u\", \"lineNumber\": 0, \"columnNumber\": 0}, \"clientHintIssueReason\": \"MetaTagModifiedHTML\"}"
  (α := ClientHintIssueDetails)
  = .ok { sourceCodeLocation := { url := "u", lineNumber := 0, columnNumber := 0 }
        , clientHintIssueReason := .metaTagModifiedHTML }

-- ── Federated auth request issues ──

#guard decodeAs "\"ShouldEmbargo\"" (α := FederatedAuthRequestIssueReason) = .ok .shouldEmbargo
#guard decodeAs "{\"federatedAuthRequestIssueReason\": \"ShouldEmbargo\"}" (α := FederatedAuthRequestIssueDetails)
  = .ok { federatedAuthRequestIssueReason := .shouldEmbargo }

-- ── Inspector issues ──

#guard decodeAs "\"CookieIssue\"" (α := InspectorIssueCode) = .ok .cookieIssue
#guard decodeAs "{}" (α := InspectorIssueDetails) = .ok {}
#guard decodeAs "{\"code\": \"CookieIssue\", \"details\": {}}" (α := InspectorIssue)
  = .ok { code := .cookieIssue, details := {} }

-- ── Events ──

#guard decodeAs "{\"issue\": {\"code\": \"CookieIssue\", \"details\": {}}}" (α := IssueAdded)
  = .ok { issue := { code := .cookieIssue, details := {} } }
#guard Event.eventName (α := IssueAdded) = "Audits.issueAdded"

-- ── Commands ──

#guard decodeAs "\"webp\"" (α := GetEncodedResponseEncoding) = .ok .webp
#guard encode (ToJSON.toJSON GetEncodedResponseEncoding.png) = "\"png\""

#guard encode (ToJSON.toJSON ({ requestId := "r1", encoding := .jpeg } : PGetEncodedResponse))
  = "{\"requestId\":\"r1\",\"encoding\":\"jpeg\"}"
#guard Command.commandName ({ requestId := "r1", encoding := .jpeg } : PGetEncodedResponse)
  = "Audits.getEncodedResponse"
#guard decodeAs "{\"originalSize\": 100, \"encodedSize\": 50}" (α := GetEncodedResponse)
  = .ok { originalSize := 100, encodedSize := 50 }

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Audits.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "Audits.enable"

#guard encode (ToJSON.toJSON ({} : PCheckContrast)) = "{}"
#guard encode (ToJSON.toJSON ({ reportAAA := some true } : PCheckContrast)) = "{\"reportAAA\":true}"
#guard Command.commandName ({} : PCheckContrast) = "Audits.checkContrast"

end Tests.CDP.Domains.Audits
