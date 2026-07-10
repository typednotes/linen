/-
  Linen.CDP.Domains.Audits — the `Audits` CDP domain

  Ports `CDP.Domains.Audits` (see `docs/imports/cdp/dependencies.md`). The
  Audits domain allows investigation of page violations and possible
  improvements. Naming conventions as in `CDP.Domains.CacheStorage`'s
  docstring; qualified references into the merged `DOM`/`Page`/`Network`/
  `Emulation`/`Security` domain follow `Linen.CDP.Domains.
  DOMPageNetworkEmulationSecurity`'s nested-namespace convention (e.g.
  `Network.RequestId`, `DOM.BackendNodeId`, `Page.FrameId`).
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.Audits

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Affected resources ──

/-- Information about a cookie that is affected by an inspector issue. -/
structure AffectedCookie where
  /-- The following three properties uniquely identify a cookie. -/
  name : String
  path : String
  domain : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON AffectedCookie where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        path := ← Value.getField v "path" >>= FromJSON.parseJSON
        domain := ← Value.getField v "domain" >>= FromJSON.parseJSON }

instance : ToJSON AffectedCookie where
  toJSON p := Data.Json.object
    [("name", ToJSON.toJSON p.name), ("path", ToJSON.toJSON p.path), ("domain", ToJSON.toJSON p.domain)]

/-- Information about a request that is affected by an inspector issue. -/
structure AffectedRequest where
  /-- The unique request id. -/
  requestId : DOMPageNetworkEmulationSecurity.Network.RequestId
  url : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON AffectedRequest where
  parseJSON v := do
    .ok
      { requestId := ← Value.getField v "requestId" >>= FromJSON.parseJSON
        url := ← (← Value.getFieldOpt v "url").mapM FromJSON.parseJSON }

instance : ToJSON AffectedRequest where
  toJSON p := Data.Json.object <|
    [("requestId", ToJSON.toJSON p.requestId)]
    ++ (p.url.map fun v => ("url", ToJSON.toJSON v)).toList

/-- Information about the frame affected by an inspector issue. -/
structure AffectedFrame where
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  deriving Repr, BEq, DecidableEq

instance : FromJSON AffectedFrame where
  parseJSON v := do .ok { frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON }

instance : ToJSON AffectedFrame where
  toJSON p := Data.Json.object [("frameId", ToJSON.toJSON p.frameId)]

-- ── Cookie issues ──

/-- Reasons a cookie has been excluded from a request/response. -/
inductive CookieExclusionReason where
  | excludeSameSiteUnspecifiedTreatedAsLax | excludeSameSiteNoneInsecure
  | excludeSameSiteLax | excludeSameSiteStrict | excludeInvalidSameParty
  | excludeSamePartyCrossPartyContext | excludeDomainNonASCII
  deriving Repr, BEq, DecidableEq

instance : FromJSON CookieExclusionReason where
  parseJSON
    | .string "ExcludeSameSiteUnspecifiedTreatedAsLax" => .ok .excludeSameSiteUnspecifiedTreatedAsLax
    | .string "ExcludeSameSiteNoneInsecure" => .ok .excludeSameSiteNoneInsecure
    | .string "ExcludeSameSiteLax" => .ok .excludeSameSiteLax
    | .string "ExcludeSameSiteStrict" => .ok .excludeSameSiteStrict
    | .string "ExcludeInvalidSameParty" => .ok .excludeInvalidSameParty
    | .string "ExcludeSamePartyCrossPartyContext" => .ok .excludeSamePartyCrossPartyContext
    | .string "ExcludeDomainNonASCII" => .ok .excludeDomainNonASCII
    | v => .error s!"failed to parse CookieExclusionReason: {repr v}"

instance : ToJSON CookieExclusionReason where
  toJSON
    | .excludeSameSiteUnspecifiedTreatedAsLax => .string "ExcludeSameSiteUnspecifiedTreatedAsLax"
    | .excludeSameSiteNoneInsecure => .string "ExcludeSameSiteNoneInsecure"
    | .excludeSameSiteLax => .string "ExcludeSameSiteLax"
    | .excludeSameSiteStrict => .string "ExcludeSameSiteStrict"
    | .excludeInvalidSameParty => .string "ExcludeInvalidSameParty"
    | .excludeSamePartyCrossPartyContext => .string "ExcludeSamePartyCrossPartyContext"
    | .excludeDomainNonASCII => .string "ExcludeDomainNonASCII"

/-- Warning reasons associated with a cookie. -/
inductive CookieWarningReason where
  | warnSameSiteUnspecifiedCrossSiteContext | warnSameSiteNoneInsecure
  | warnSameSiteUnspecifiedLaxAllowUnsafe | warnSameSiteStrictLaxDowngradeStrict
  | warnSameSiteStrictCrossDowngradeStrict | warnSameSiteStrictCrossDowngradeLax
  | warnSameSiteLaxCrossDowngradeStrict | warnSameSiteLaxCrossDowngradeLax
  | warnAttributeValueExceedsMaxSize | warnDomainNonASCII
  deriving Repr, BEq, DecidableEq

instance : FromJSON CookieWarningReason where
  parseJSON
    | .string "WarnSameSiteUnspecifiedCrossSiteContext" => .ok .warnSameSiteUnspecifiedCrossSiteContext
    | .string "WarnSameSiteNoneInsecure" => .ok .warnSameSiteNoneInsecure
    | .string "WarnSameSiteUnspecifiedLaxAllowUnsafe" => .ok .warnSameSiteUnspecifiedLaxAllowUnsafe
    | .string "WarnSameSiteStrictLaxDowngradeStrict" => .ok .warnSameSiteStrictLaxDowngradeStrict
    | .string "WarnSameSiteStrictCrossDowngradeStrict" => .ok .warnSameSiteStrictCrossDowngradeStrict
    | .string "WarnSameSiteStrictCrossDowngradeLax" => .ok .warnSameSiteStrictCrossDowngradeLax
    | .string "WarnSameSiteLaxCrossDowngradeStrict" => .ok .warnSameSiteLaxCrossDowngradeStrict
    | .string "WarnSameSiteLaxCrossDowngradeLax" => .ok .warnSameSiteLaxCrossDowngradeLax
    | .string "WarnAttributeValueExceedsMaxSize" => .ok .warnAttributeValueExceedsMaxSize
    | .string "WarnDomainNonASCII" => .ok .warnDomainNonASCII
    | v => .error s!"failed to parse CookieWarningReason: {repr v}"

instance : ToJSON CookieWarningReason where
  toJSON
    | .warnSameSiteUnspecifiedCrossSiteContext => .string "WarnSameSiteUnspecifiedCrossSiteContext"
    | .warnSameSiteNoneInsecure => .string "WarnSameSiteNoneInsecure"
    | .warnSameSiteUnspecifiedLaxAllowUnsafe => .string "WarnSameSiteUnspecifiedLaxAllowUnsafe"
    | .warnSameSiteStrictLaxDowngradeStrict => .string "WarnSameSiteStrictLaxDowngradeStrict"
    | .warnSameSiteStrictCrossDowngradeStrict => .string "WarnSameSiteStrictCrossDowngradeStrict"
    | .warnSameSiteStrictCrossDowngradeLax => .string "WarnSameSiteStrictCrossDowngradeLax"
    | .warnSameSiteLaxCrossDowngradeStrict => .string "WarnSameSiteLaxCrossDowngradeStrict"
    | .warnSameSiteLaxCrossDowngradeLax => .string "WarnSameSiteLaxCrossDowngradeLax"
    | .warnAttributeValueExceedsMaxSize => .string "WarnAttributeValueExceedsMaxSize"
    | .warnDomainNonASCII => .string "WarnDomainNonASCII"

/-- Whether a cookie is being set or read. -/
inductive CookieOperation where
  | setCookie | readCookie
  deriving Repr, BEq, DecidableEq

instance : FromJSON CookieOperation where
  parseJSON
    | .string "SetCookie" => .ok .setCookie
    | .string "ReadCookie" => .ok .readCookie
    | v => .error s!"failed to parse CookieOperation: {repr v}"

instance : ToJSON CookieOperation where
  toJSON | .setCookie => .string "SetCookie" | .readCookie => .string "ReadCookie"

/-- This information is currently necessary, as the front-end has a difficult
    time finding a specific cookie. With this, we can convey specific error
    information without the cookie. -/
structure CookieIssueDetails where
  /-- If `cookie` is not set then `rawCookieLine` contains the raw Set-Cookie
      header string. This hints at a problem where the cookie line is
      syntactically or semantically malformed in a way that no valid cookie
      could be created. -/
  cookie : Option AffectedCookie := none
  rawCookieLine : Option String := none
  cookieWarningReasons : List CookieWarningReason
  cookieExclusionReasons : List CookieExclusionReason
  /-- Optionally identifies the site-for-cookies and the cookie url, which may
      be used by the front-end as additional context. -/
  operation : CookieOperation
  siteForCookies : Option String := none
  cookieUrl : Option String := none
  request : Option AffectedRequest := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CookieIssueDetails where
  parseJSON v := do
    .ok
      { cookie := ← (← Value.getFieldOpt v "cookie").mapM FromJSON.parseJSON
        rawCookieLine := ← (← Value.getFieldOpt v "rawCookieLine").mapM FromJSON.parseJSON
        cookieWarningReasons := ← Value.getField v "cookieWarningReasons" >>= FromJSON.parseJSON
        cookieExclusionReasons := ← Value.getField v "cookieExclusionReasons" >>= FromJSON.parseJSON
        operation := ← Value.getField v "operation" >>= FromJSON.parseJSON
        siteForCookies := ← (← Value.getFieldOpt v "siteForCookies").mapM FromJSON.parseJSON
        cookieUrl := ← (← Value.getFieldOpt v "cookieUrl").mapM FromJSON.parseJSON
        request := ← (← Value.getFieldOpt v "request").mapM FromJSON.parseJSON }

instance : ToJSON CookieIssueDetails where
  toJSON p := Data.Json.object <|
    (p.cookie.map fun v => ("cookie", ToJSON.toJSON v)).toList
    ++ (p.rawCookieLine.map fun v => ("rawCookieLine", ToJSON.toJSON v)).toList
    ++ [ ("cookieWarningReasons", ToJSON.toJSON p.cookieWarningReasons)
       , ("cookieExclusionReasons", ToJSON.toJSON p.cookieExclusionReasons)
       , ("operation", ToJSON.toJSON p.operation) ]
    ++ (p.siteForCookies.map fun v => ("siteForCookies", ToJSON.toJSON v)).toList
    ++ (p.cookieUrl.map fun v => ("cookieUrl", ToJSON.toJSON v)).toList
    ++ (p.request.map fun v => ("request", ToJSON.toJSON v)).toList

-- ── Mixed content issues ──

/-- How a mixed content issue is being resolved. -/
inductive MixedContentResolutionStatus where
  | mixedContentBlocked | mixedContentAutomaticallyUpgraded | mixedContentWarning
  deriving Repr, BEq, DecidableEq

instance : FromJSON MixedContentResolutionStatus where
  parseJSON
    | .string "MixedContentBlocked" => .ok .mixedContentBlocked
    | .string "MixedContentAutomaticallyUpgraded" => .ok .mixedContentAutomaticallyUpgraded
    | .string "MixedContentWarning" => .ok .mixedContentWarning
    | v => .error s!"failed to parse MixedContentResolutionStatus: {repr v}"

instance : ToJSON MixedContentResolutionStatus where
  toJSON
    | .mixedContentBlocked => .string "MixedContentBlocked"
    | .mixedContentAutomaticallyUpgraded => .string "MixedContentAutomaticallyUpgraded"
    | .mixedContentWarning => .string "MixedContentWarning"

/-- The type of resource causing a mixed content issue (css, js, iframe,
    form, ...). -/
inductive MixedContentResourceType where
  | attributionSrc | audio | beacon | cSPReport | download | eventSource
  | favicon | font | form | frame | image | import_ | manifest | ping
  | pluginData | pluginResource | prefetch | resource | script | serviceWorker
  | sharedWorker | stylesheet | track | video | worker | xMLHttpRequest | xSLT
  deriving Repr, BEq, DecidableEq

instance : FromJSON MixedContentResourceType where
  parseJSON
    | .string "AttributionSrc" => .ok .attributionSrc
    | .string "Audio" => .ok .audio
    | .string "Beacon" => .ok .beacon
    | .string "CSPReport" => .ok .cSPReport
    | .string "Download" => .ok .download
    | .string "EventSource" => .ok .eventSource
    | .string "Favicon" => .ok .favicon
    | .string "Font" => .ok .font
    | .string "Form" => .ok .form
    | .string "Frame" => .ok .frame
    | .string "Image" => .ok .image
    | .string "Import" => .ok .import_
    | .string "Manifest" => .ok .manifest
    | .string "Ping" => .ok .ping
    | .string "PluginData" => .ok .pluginData
    | .string "PluginResource" => .ok .pluginResource
    | .string "Prefetch" => .ok .prefetch
    | .string "Resource" => .ok .resource
    | .string "Script" => .ok .script
    | .string "ServiceWorker" => .ok .serviceWorker
    | .string "SharedWorker" => .ok .sharedWorker
    | .string "Stylesheet" => .ok .stylesheet
    | .string "Track" => .ok .track
    | .string "Video" => .ok .video
    | .string "Worker" => .ok .worker
    | .string "XMLHttpRequest" => .ok .xMLHttpRequest
    | .string "XSLT" => .ok .xSLT
    | v => .error s!"failed to parse MixedContentResourceType: {repr v}"

instance : ToJSON MixedContentResourceType where
  toJSON
    | .attributionSrc => .string "AttributionSrc"
    | .audio => .string "Audio"
    | .beacon => .string "Beacon"
    | .cSPReport => .string "CSPReport"
    | .download => .string "Download"
    | .eventSource => .string "EventSource"
    | .favicon => .string "Favicon"
    | .font => .string "Font"
    | .form => .string "Form"
    | .frame => .string "Frame"
    | .image => .string "Image"
    | .import_ => .string "Import"
    | .manifest => .string "Manifest"
    | .ping => .string "Ping"
    | .pluginData => .string "PluginData"
    | .pluginResource => .string "PluginResource"
    | .prefetch => .string "Prefetch"
    | .resource => .string "Resource"
    | .script => .string "Script"
    | .serviceWorker => .string "ServiceWorker"
    | .sharedWorker => .string "SharedWorker"
    | .stylesheet => .string "Stylesheet"
    | .track => .string "Track"
    | .video => .string "Video"
    | .worker => .string "Worker"
    | .xMLHttpRequest => .string "XMLHttpRequest"
    | .xSLT => .string "XSLT"

/-- Details of a mixed content issue. -/
structure MixedContentIssueDetails where
  /-- The type of resource causing the mixed content issue (css, js, iframe,
      form, ...). Marked as optional because it is mapped to from
      `blink::mojom::RequestContextType`, which will be replaced by
      `network::mojom::RequestDestination`. -/
  resourceType : Option MixedContentResourceType := none
  /-- The way the mixed content issue is being resolved. -/
  resolutionStatus : MixedContentResolutionStatus
  /-- The unsafe http url causing the mixed content issue. -/
  insecureURL : String
  /-- The url responsible for the call to an unsafe url. -/
  mainResourceURL : String
  /-- The mixed content request. Does not always exist (e.g. for unsafe form
      submission urls). -/
  request : Option AffectedRequest := none
  /-- Optional because not every mixed content issue is necessarily linked to
      a frame. -/
  frame : Option AffectedFrame := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON MixedContentIssueDetails where
  parseJSON v := do
    .ok
      { resourceType := ← (← Value.getFieldOpt v "resourceType").mapM FromJSON.parseJSON
        resolutionStatus := ← Value.getField v "resolutionStatus" >>= FromJSON.parseJSON
        insecureURL := ← Value.getField v "insecureURL" >>= FromJSON.parseJSON
        mainResourceURL := ← Value.getField v "mainResourceURL" >>= FromJSON.parseJSON
        request := ← (← Value.getFieldOpt v "request").mapM FromJSON.parseJSON
        frame := ← (← Value.getFieldOpt v "frame").mapM FromJSON.parseJSON }

instance : ToJSON MixedContentIssueDetails where
  toJSON p := Data.Json.object <|
    (p.resourceType.map fun v => ("resourceType", ToJSON.toJSON v)).toList
    ++ [ ("resolutionStatus", ToJSON.toJSON p.resolutionStatus)
       , ("insecureURL", ToJSON.toJSON p.insecureURL)
       , ("mainResourceURL", ToJSON.toJSON p.mainResourceURL) ]
    ++ (p.request.map fun v => ("request", ToJSON.toJSON v)).toList
    ++ (p.frame.map fun v => ("frame", ToJSON.toJSON v)).toList

-- ── Blocked-by-response issues ──

/-- The reason a response has been blocked. These reasons are refinements of
    the net error `BLOCKED_BY_RESPONSE`. -/
inductive BlockedByResponseReason where
  | coepFrameResourceNeedsCoepHeader | coopSandboxedIFrameCannotNavigateToCoopPage
  | corpNotSameOrigin | corpNotSameOriginAfterDefaultedToSameOriginByCoep
  | corpNotSameSite
  deriving Repr, BEq, DecidableEq

instance : FromJSON BlockedByResponseReason where
  parseJSON
    | .string "CoepFrameResourceNeedsCoepHeader" => .ok .coepFrameResourceNeedsCoepHeader
    | .string "CoopSandboxedIFrameCannotNavigateToCoopPage" => .ok .coopSandboxedIFrameCannotNavigateToCoopPage
    | .string "CorpNotSameOrigin" => .ok .corpNotSameOrigin
    | .string "CorpNotSameOriginAfterDefaultedToSameOriginByCoep" => .ok .corpNotSameOriginAfterDefaultedToSameOriginByCoep
    | .string "CorpNotSameSite" => .ok .corpNotSameSite
    | v => .error s!"failed to parse BlockedByResponseReason: {repr v}"

instance : ToJSON BlockedByResponseReason where
  toJSON
    | .coepFrameResourceNeedsCoepHeader => .string "CoepFrameResourceNeedsCoepHeader"
    | .coopSandboxedIFrameCannotNavigateToCoopPage => .string "CoopSandboxedIFrameCannotNavigateToCoopPage"
    | .corpNotSameOrigin => .string "CorpNotSameOrigin"
    | .corpNotSameOriginAfterDefaultedToSameOriginByCoep => .string "CorpNotSameOriginAfterDefaultedToSameOriginByCoep"
    | .corpNotSameSite => .string "CorpNotSameSite"

/-- Details for a request that has been blocked with the
    `BLOCKED_BY_RESPONSE` code. Currently only used for COEP/COOP, but may be
    extended to include some CSP errors in the future. -/
structure BlockedByResponseIssueDetails where
  request : AffectedRequest
  parentFrame : Option AffectedFrame := none
  blockedFrame : Option AffectedFrame := none
  reason : BlockedByResponseReason
  deriving Repr, BEq, DecidableEq

instance : FromJSON BlockedByResponseIssueDetails where
  parseJSON v := do
    .ok
      { request := ← Value.getField v "request" >>= FromJSON.parseJSON
        parentFrame := ← (← Value.getFieldOpt v "parentFrame").mapM FromJSON.parseJSON
        blockedFrame := ← (← Value.getFieldOpt v "blockedFrame").mapM FromJSON.parseJSON
        reason := ← Value.getField v "reason" >>= FromJSON.parseJSON }

instance : ToJSON BlockedByResponseIssueDetails where
  toJSON p := Data.Json.object <|
    [("request", ToJSON.toJSON p.request)]
    ++ (p.parentFrame.map fun v => ("parentFrame", ToJSON.toJSON v)).toList
    ++ (p.blockedFrame.map fun v => ("blockedFrame", ToJSON.toJSON v)).toList
    ++ [("reason", ToJSON.toJSON p.reason)]

-- ── Heavy ad issues ──

/-- The resolution status of a heavy ad, either blocking the content or
    warning. -/
inductive HeavyAdResolutionStatus where
  | heavyAdBlocked | heavyAdWarning
  deriving Repr, BEq, DecidableEq

instance : FromJSON HeavyAdResolutionStatus where
  parseJSON
    | .string "HeavyAdBlocked" => .ok .heavyAdBlocked
    | .string "HeavyAdWarning" => .ok .heavyAdWarning
    | v => .error s!"failed to parse HeavyAdResolutionStatus: {repr v}"

instance : ToJSON HeavyAdResolutionStatus where
  toJSON | .heavyAdBlocked => .string "HeavyAdBlocked" | .heavyAdWarning => .string "HeavyAdWarning"

/-- The reason an ad was flagged heavy: total network, total cpu, or peak
    cpu. -/
inductive HeavyAdReason where
  | networkTotalLimit | cpuTotalLimit | cpuPeakLimit
  deriving Repr, BEq, DecidableEq

instance : FromJSON HeavyAdReason where
  parseJSON
    | .string "NetworkTotalLimit" => .ok .networkTotalLimit
    | .string "CpuTotalLimit" => .ok .cpuTotalLimit
    | .string "CpuPeakLimit" => .ok .cpuPeakLimit
    | v => .error s!"failed to parse HeavyAdReason: {repr v}"

instance : ToJSON HeavyAdReason where
  toJSON
    | .networkTotalLimit => .string "NetworkTotalLimit"
    | .cpuTotalLimit => .string "CpuTotalLimit"
    | .cpuPeakLimit => .string "CpuPeakLimit"

/-- Details of a heavy-ad issue. -/
structure HeavyAdIssueDetails where
  /-- The resolution status, either blocking the content or warning. -/
  resolution : HeavyAdResolutionStatus
  /-- The reason the ad was blocked, total network or cpu or peak cpu. -/
  reason : HeavyAdReason
  /-- The frame that was blocked. -/
  frame : AffectedFrame
  deriving Repr, BEq, DecidableEq

instance : FromJSON HeavyAdIssueDetails where
  parseJSON v := do
    .ok
      { resolution := ← Value.getField v "resolution" >>= FromJSON.parseJSON
        reason := ← Value.getField v "reason" >>= FromJSON.parseJSON
        frame := ← Value.getField v "frame" >>= FromJSON.parseJSON }

instance : ToJSON HeavyAdIssueDetails where
  toJSON p := Data.Json.object
    [("resolution", ToJSON.toJSON p.resolution), ("reason", ToJSON.toJSON p.reason), ("frame", ToJSON.toJSON p.frame)]

-- ── Content security policy issues ──

/-- The kind of CSP violation. -/
inductive ContentSecurityPolicyViolationType where
  | kInlineViolation | kEvalViolation | kURLViolation
  | kTrustedTypesSinkViolation | kTrustedTypesPolicyViolation | kWasmEvalViolation
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContentSecurityPolicyViolationType where
  parseJSON
    | .string "kInlineViolation" => .ok .kInlineViolation
    | .string "kEvalViolation" => .ok .kEvalViolation
    | .string "kURLViolation" => .ok .kURLViolation
    | .string "kTrustedTypesSinkViolation" => .ok .kTrustedTypesSinkViolation
    | .string "kTrustedTypesPolicyViolation" => .ok .kTrustedTypesPolicyViolation
    | .string "kWasmEvalViolation" => .ok .kWasmEvalViolation
    | v => .error s!"failed to parse ContentSecurityPolicyViolationType: {repr v}"

instance : ToJSON ContentSecurityPolicyViolationType where
  toJSON
    | .kInlineViolation => .string "kInlineViolation"
    | .kEvalViolation => .string "kEvalViolation"
    | .kURLViolation => .string "kURLViolation"
    | .kTrustedTypesSinkViolation => .string "kTrustedTypesSinkViolation"
    | .kTrustedTypesPolicyViolation => .string "kTrustedTypesPolicyViolation"
    | .kWasmEvalViolation => .string "kWasmEvalViolation"

/-- A location in JavaScript source code. -/
structure SourceCodeLocation where
  scriptId : Option Runtime.ScriptId := none
  url : String
  lineNumber : Int
  columnNumber : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON SourceCodeLocation where
  parseJSON v := do
    .ok
      { scriptId := ← (← Value.getFieldOpt v "scriptId").mapM FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        columnNumber := ← Value.getField v "columnNumber" >>= FromJSON.parseJSON }

instance : ToJSON SourceCodeLocation where
  toJSON p := Data.Json.object <|
    (p.scriptId.map fun v => ("scriptId", ToJSON.toJSON v)).toList
    ++ [ ("url", ToJSON.toJSON p.url), ("lineNumber", ToJSON.toJSON p.lineNumber)
       , ("columnNumber", ToJSON.toJSON p.columnNumber) ]

/-- Details of a content-security-policy issue. -/
structure ContentSecurityPolicyIssueDetails where
  /-- The url not included in allowed sources. -/
  blockedURL : Option String := none
  /-- Specific directive that is violated, causing the CSP issue. -/
  violatedDirective : String
  isReportOnly : Bool
  contentSecurityPolicyViolationType : ContentSecurityPolicyViolationType
  frameAncestor : Option AffectedFrame := none
  sourceCodeLocation : Option SourceCodeLocation := none
  violatingNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContentSecurityPolicyIssueDetails where
  parseJSON v := do
    .ok
      { blockedURL := ← (← Value.getFieldOpt v "blockedURL").mapM FromJSON.parseJSON
        violatedDirective := ← Value.getField v "violatedDirective" >>= FromJSON.parseJSON
        isReportOnly := ← Value.getField v "isReportOnly" >>= FromJSON.parseJSON
        contentSecurityPolicyViolationType :=
          ← Value.getField v "contentSecurityPolicyViolationType" >>= FromJSON.parseJSON
        frameAncestor := ← (← Value.getFieldOpt v "frameAncestor").mapM FromJSON.parseJSON
        sourceCodeLocation := ← (← Value.getFieldOpt v "sourceCodeLocation").mapM FromJSON.parseJSON
        violatingNodeId := ← (← Value.getFieldOpt v "violatingNodeId").mapM FromJSON.parseJSON }

instance : ToJSON ContentSecurityPolicyIssueDetails where
  toJSON p := Data.Json.object <|
    (p.blockedURL.map fun v => ("blockedURL", ToJSON.toJSON v)).toList
    ++ [ ("violatedDirective", ToJSON.toJSON p.violatedDirective)
       , ("isReportOnly", ToJSON.toJSON p.isReportOnly)
       , ("contentSecurityPolicyViolationType", ToJSON.toJSON p.contentSecurityPolicyViolationType) ]
    ++ (p.frameAncestor.map fun v => ("frameAncestor", ToJSON.toJSON v)).toList
    ++ (p.sourceCodeLocation.map fun v => ("sourceCodeLocation", ToJSON.toJSON v)).toList
    ++ (p.violatingNodeId.map fun v => ("violatingNodeId", ToJSON.toJSON v)).toList

-- ── SharedArrayBuffer issues ──

/-- Whether a SharedArrayBuffer issue arises from a transfer or a
    creation. -/
inductive SharedArrayBufferIssueType where
  | transferIssue | creationIssue
  deriving Repr, BEq, DecidableEq

instance : FromJSON SharedArrayBufferIssueType where
  parseJSON
    | .string "TransferIssue" => .ok .transferIssue
    | .string "CreationIssue" => .ok .creationIssue
    | v => .error s!"failed to parse SharedArrayBufferIssueType: {repr v}"

instance : ToJSON SharedArrayBufferIssueType where
  toJSON | .transferIssue => .string "TransferIssue" | .creationIssue => .string "CreationIssue"

/-- Details for an issue arising from a SAB being instantiated in, or
    transferred to, a context that is not cross-origin isolated. -/
structure SharedArrayBufferIssueDetails where
  sourceCodeLocation : SourceCodeLocation
  isWarning : Bool
  type : SharedArrayBufferIssueType
  deriving Repr, BEq, DecidableEq

instance : FromJSON SharedArrayBufferIssueDetails where
  parseJSON v := do
    .ok
      { sourceCodeLocation := ← Value.getField v "sourceCodeLocation" >>= FromJSON.parseJSON
        isWarning := ← Value.getField v "isWarning" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON }

instance : ToJSON SharedArrayBufferIssueDetails where
  toJSON p := Data.Json.object
    [ ("sourceCodeLocation", ToJSON.toJSON p.sourceCodeLocation), ("isWarning", ToJSON.toJSON p.isWarning)
    , ("type", ToJSON.toJSON p.type) ]

-- ── Trusted Web Activity issues ──

/-- The kind of Trusted Web Activity quality-enforcement violation. -/
inductive TwaQualityEnforcementViolationType where
  | kHttpError | kUnavailableOffline | kDigitalAssetLinks
  deriving Repr, BEq, DecidableEq

instance : FromJSON TwaQualityEnforcementViolationType where
  parseJSON
    | .string "kHttpError" => .ok .kHttpError
    | .string "kUnavailableOffline" => .ok .kUnavailableOffline
    | .string "kDigitalAssetLinks" => .ok .kDigitalAssetLinks
    | v => .error s!"failed to parse TwaQualityEnforcementViolationType: {repr v}"

instance : ToJSON TwaQualityEnforcementViolationType where
  toJSON
    | .kHttpError => .string "kHttpError"
    | .kUnavailableOffline => .string "kUnavailableOffline"
    | .kDigitalAssetLinks => .string "kDigitalAssetLinks"

/-- Details of a Trusted Web Activity quality-enforcement issue. -/
structure TrustedWebActivityIssueDetails where
  /-- The url that triggers the violation. -/
  url : String
  violationType : TwaQualityEnforcementViolationType
  httpStatusCode : Option Int := none
  /-- The package name of the Trusted Web Activity client app. This field is
      only used when violation type is `kDigitalAssetLinks`. -/
  packageName : Option String := none
  /-- The signature of the Trusted Web Activity client app. This field is
      only used when violation type is `kDigitalAssetLinks`. -/
  signature : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON TrustedWebActivityIssueDetails where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        violationType := ← Value.getField v "violationType" >>= FromJSON.parseJSON
        httpStatusCode := ← (← Value.getFieldOpt v "httpStatusCode").mapM FromJSON.parseJSON
        packageName := ← (← Value.getFieldOpt v "packageName").mapM FromJSON.parseJSON
        signature := ← (← Value.getFieldOpt v "signature").mapM FromJSON.parseJSON }

instance : ToJSON TrustedWebActivityIssueDetails where
  toJSON p := Data.Json.object <|
    [("url", ToJSON.toJSON p.url), ("violationType", ToJSON.toJSON p.violationType)]
    ++ (p.httpStatusCode.map fun v => ("httpStatusCode", ToJSON.toJSON v)).toList
    ++ (p.packageName.map fun v => ("packageName", ToJSON.toJSON v)).toList
    ++ (p.signature.map fun v => ("signature", ToJSON.toJSON v)).toList

-- ── Low text contrast issues ──

/-- Details of a low-text-contrast accessibility issue. -/
structure LowTextContrastIssueDetails where
  violatingNodeId : DOMPageNetworkEmulationSecurity.DOM.BackendNodeId
  violatingNodeSelector : String
  contrastRatio : Float
  thresholdAA : Float
  thresholdAAA : Float
  fontSize : String
  fontWeight : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON LowTextContrastIssueDetails where
  parseJSON v := do
    .ok
      { violatingNodeId := ← Value.getField v "violatingNodeId" >>= FromJSON.parseJSON
        violatingNodeSelector := ← Value.getField v "violatingNodeSelector" >>= FromJSON.parseJSON
        contrastRatio := ← Value.getField v "contrastRatio" >>= FromJSON.parseJSON
        thresholdAA := ← Value.getField v "thresholdAA" >>= FromJSON.parseJSON
        thresholdAAA := ← Value.getField v "thresholdAAA" >>= FromJSON.parseJSON
        fontSize := ← Value.getField v "fontSize" >>= FromJSON.parseJSON
        fontWeight := ← Value.getField v "fontWeight" >>= FromJSON.parseJSON }

instance : ToJSON LowTextContrastIssueDetails where
  toJSON p := Data.Json.object
    [ ("violatingNodeId", ToJSON.toJSON p.violatingNodeId)
    , ("violatingNodeSelector", ToJSON.toJSON p.violatingNodeSelector)
    , ("contrastRatio", ToJSON.toJSON p.contrastRatio)
    , ("thresholdAA", ToJSON.toJSON p.thresholdAA)
    , ("thresholdAAA", ToJSON.toJSON p.thresholdAAA)
    , ("fontSize", ToJSON.toJSON p.fontSize)
    , ("fontWeight", ToJSON.toJSON p.fontWeight) ]

-- ── CORS issues ──

/-- Details for a CORS-related issue, e.g. a warning or error related to CORS
    RFC1918 enforcement. -/
structure CorsIssueDetails where
  corsErrorStatus : DOMPageNetworkEmulationSecurity.Network.CorsErrorStatus
  isWarning : Bool
  request : AffectedRequest
  location : Option SourceCodeLocation := none
  initiatorOrigin : Option String := none
  resourceIPAddressSpace : Option DOMPageNetworkEmulationSecurity.Network.IPAddressSpace := none
  clientSecurityState : Option DOMPageNetworkEmulationSecurity.Network.ClientSecurityState := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CorsIssueDetails where
  parseJSON v := do
    .ok
      { corsErrorStatus := ← Value.getField v "corsErrorStatus" >>= FromJSON.parseJSON
        isWarning := ← Value.getField v "isWarning" >>= FromJSON.parseJSON
        request := ← Value.getField v "request" >>= FromJSON.parseJSON
        location := ← (← Value.getFieldOpt v "location").mapM FromJSON.parseJSON
        initiatorOrigin := ← (← Value.getFieldOpt v "initiatorOrigin").mapM FromJSON.parseJSON
        resourceIPAddressSpace := ← (← Value.getFieldOpt v "resourceIPAddressSpace").mapM FromJSON.parseJSON
        clientSecurityState := ← (← Value.getFieldOpt v "clientSecurityState").mapM FromJSON.parseJSON }

instance : ToJSON CorsIssueDetails where
  toJSON p := Data.Json.object <|
    [ ("corsErrorStatus", ToJSON.toJSON p.corsErrorStatus), ("isWarning", ToJSON.toJSON p.isWarning)
    , ("request", ToJSON.toJSON p.request) ]
    ++ (p.location.map fun v => ("location", ToJSON.toJSON v)).toList
    ++ (p.initiatorOrigin.map fun v => ("initiatorOrigin", ToJSON.toJSON v)).toList
    ++ (p.resourceIPAddressSpace.map fun v => ("resourceIPAddressSpace", ToJSON.toJSON v)).toList
    ++ (p.clientSecurityState.map fun v => ("clientSecurityState", ToJSON.toJSON v)).toList

-- ── Attribution reporting issues ──

/-- Types of issues around the "Attribution Reporting API"
    (https://github.com/WICG/attribution-reporting-api). -/
inductive AttributionReportingIssueType where
  | permissionPolicyDisabled | permissionPolicyNotDelegated
  | untrustworthyReportingOrigin | insecureContext | invalidHeader
  | invalidRegisterTriggerHeader | invalidEligibleHeader
  | tooManyConcurrentRequests | sourceAndTriggerHeaders | sourceIgnored
  | triggerIgnored
  deriving Repr, BEq, DecidableEq

instance : FromJSON AttributionReportingIssueType where
  parseJSON
    | .string "PermissionPolicyDisabled" => .ok .permissionPolicyDisabled
    | .string "PermissionPolicyNotDelegated" => .ok .permissionPolicyNotDelegated
    | .string "UntrustworthyReportingOrigin" => .ok .untrustworthyReportingOrigin
    | .string "InsecureContext" => .ok .insecureContext
    | .string "InvalidHeader" => .ok .invalidHeader
    | .string "InvalidRegisterTriggerHeader" => .ok .invalidRegisterTriggerHeader
    | .string "InvalidEligibleHeader" => .ok .invalidEligibleHeader
    | .string "TooManyConcurrentRequests" => .ok .tooManyConcurrentRequests
    | .string "SourceAndTriggerHeaders" => .ok .sourceAndTriggerHeaders
    | .string "SourceIgnored" => .ok .sourceIgnored
    | .string "TriggerIgnored" => .ok .triggerIgnored
    | v => .error s!"failed to parse AttributionReportingIssueType: {repr v}"

instance : ToJSON AttributionReportingIssueType where
  toJSON
    | .permissionPolicyDisabled => .string "PermissionPolicyDisabled"
    | .permissionPolicyNotDelegated => .string "PermissionPolicyNotDelegated"
    | .untrustworthyReportingOrigin => .string "UntrustworthyReportingOrigin"
    | .insecureContext => .string "InsecureContext"
    | .invalidHeader => .string "InvalidHeader"
    | .invalidRegisterTriggerHeader => .string "InvalidRegisterTriggerHeader"
    | .invalidEligibleHeader => .string "InvalidEligibleHeader"
    | .tooManyConcurrentRequests => .string "TooManyConcurrentRequests"
    | .sourceAndTriggerHeaders => .string "SourceAndTriggerHeaders"
    | .sourceIgnored => .string "SourceIgnored"
    | .triggerIgnored => .string "TriggerIgnored"

/-- Details for issues around "Attribution Reporting API" usage. -/
structure AttributionReportingIssueDetails where
  violationType : AttributionReportingIssueType
  request : Option AffectedRequest := none
  violatingNodeId : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  invalidParameter : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON AttributionReportingIssueDetails where
  parseJSON v := do
    .ok
      { violationType := ← Value.getField v "violationType" >>= FromJSON.parseJSON
        request := ← (← Value.getFieldOpt v "request").mapM FromJSON.parseJSON
        violatingNodeId := ← (← Value.getFieldOpt v "violatingNodeId").mapM FromJSON.parseJSON
        invalidParameter := ← (← Value.getFieldOpt v "invalidParameter").mapM FromJSON.parseJSON }

instance : ToJSON AttributionReportingIssueDetails where
  toJSON p := Data.Json.object <|
    [("violationType", ToJSON.toJSON p.violationType)]
    ++ (p.request.map fun v => ("request", ToJSON.toJSON v)).toList
    ++ (p.violatingNodeId.map fun v => ("violatingNodeId", ToJSON.toJSON v)).toList
    ++ (p.invalidParameter.map fun v => ("invalidParameter", ToJSON.toJSON v)).toList

-- ── Quirks mode issues ──

/-- Details for issues about documents in Quirks Mode or Limited Quirks Mode
    that affect page layouting. -/
structure QuirksModeIssueDetails where
  /-- If `false`, it means the document's mode is "quirks" instead of
      "limited-quirks". -/
  isLimitedQuirksMode : Bool
  documentNodeId : DOMPageNetworkEmulationSecurity.DOM.BackendNodeId
  url : String
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  loaderId : DOMPageNetworkEmulationSecurity.Network.LoaderId
  deriving Repr, BEq, DecidableEq

instance : FromJSON QuirksModeIssueDetails where
  parseJSON v := do
    .ok
      { isLimitedQuirksMode := ← Value.getField v "isLimitedQuirksMode" >>= FromJSON.parseJSON
        documentNodeId := ← Value.getField v "documentNodeId" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        loaderId := ← Value.getField v "loaderId" >>= FromJSON.parseJSON }

instance : ToJSON QuirksModeIssueDetails where
  toJSON p := Data.Json.object
    [ ("isLimitedQuirksMode", ToJSON.toJSON p.isLimitedQuirksMode)
    , ("documentNodeId", ToJSON.toJSON p.documentNodeId)
    , ("url", ToJSON.toJSON p.url)
    , ("frameId", ToJSON.toJSON p.frameId)
    , ("loaderId", ToJSON.toJSON p.loaderId) ]

-- ── Navigator user-agent issues ──

/-- Details of a `navigator.userAgent` deprecation issue. -/
structure NavigatorUserAgentIssueDetails where
  url : String
  location : Option SourceCodeLocation := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON NavigatorUserAgentIssueDetails where
  parseJSON v := do
    .ok
      { url := ← Value.getField v "url" >>= FromJSON.parseJSON
        location := ← (← Value.getFieldOpt v "location").mapM FromJSON.parseJSON }

instance : ToJSON NavigatorUserAgentIssueDetails where
  toJSON p := Data.Json.object <|
    [("url", ToJSON.toJSON p.url)] ++ (p.location.map fun v => ("location", ToJSON.toJSON v)).toList

-- ── Generic issues ──

/-- The specific kind of generic issue. -/
inductive GenericIssueErrorType where
  | crossOriginPortalPostMessageError
  deriving Repr, BEq, DecidableEq

instance : FromJSON GenericIssueErrorType where
  parseJSON
    | .string "CrossOriginPortalPostMessageError" => .ok .crossOriginPortalPostMessageError
    | v => .error s!"failed to parse GenericIssueErrorType: {repr v}"

instance : ToJSON GenericIssueErrorType where
  toJSON | .crossOriginPortalPostMessageError => .string "CrossOriginPortalPostMessageError"

/-- Depending on the concrete `errorType`, different properties are set.
    Issues with the same `errorType` are aggregated in the frontend. -/
structure GenericIssueDetails where
  errorType : GenericIssueErrorType
  frameId : Option DOMPageNetworkEmulationSecurity.Page.FrameId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON GenericIssueDetails where
  parseJSON v := do
    .ok
      { errorType := ← Value.getField v "errorType" >>= FromJSON.parseJSON
        frameId := ← (← Value.getFieldOpt v "frameId").mapM FromJSON.parseJSON }

instance : ToJSON GenericIssueDetails where
  toJSON p := Data.Json.object <|
    [("errorType", ToJSON.toJSON p.errorType)] ++ (p.frameId.map fun v => ("frameId", ToJSON.toJSON v)).toList

-- ── Deprecation issues ──

/-- The specific deprecated feature being used, tracked to print a
    deprecation message. -/
inductive DeprecationIssueType where
  | authorizationCoveredByWildcard | canRequestURLHTTPContainingNewline
  | chromeLoadTimesConnectionInfo | chromeLoadTimesFirstPaintAfterLoadTime
  | chromeLoadTimesWasAlternateProtocolAvailable | cookieWithTruncatingChar
  | crossOriginAccessBasedOnDocumentDomain | crossOriginWindowAlert
  | crossOriginWindowConfirm | cSSSelectorInternalMediaControlsOverlayCastButton
  | deprecationExample | documentDomainSettingWithoutOriginAgentClusterHeader
  | eventPath | expectCTHeader | geolocationInsecureOrigin
  | geolocationInsecureOriginDeprecatedNotRemoved | getUserMediaInsecureOrigin
  | hostCandidateAttributeGetter | identityInCanMakePaymentEvent
  | insecurePrivateNetworkSubresourceRequest | localCSSFileExtensionRejected
  | mediaSourceAbortRemove | mediaSourceDurationTruncatingBuffered
  | noSysexWebMIDIWithoutPermission | notificationInsecureOrigin
  | notificationPermissionRequestedIframe | obsoleteWebRtcCipherSuite
  | openWebDatabaseInsecureContext | overflowVisibleOnReplacedElement
  | paymentInstruments | paymentRequestCSPViolation | persistentQuotaType
  | pictureSourceSrc | prefixedCancelAnimationFrame
  | prefixedRequestAnimationFrame | prefixedStorageInfo
  | prefixedVideoDisplayingFullscreen | prefixedVideoEnterFullscreen
  | prefixedVideoEnterFullScreen | prefixedVideoExitFullscreen
  | prefixedVideoExitFullScreen | prefixedVideoSupportsFullscreen
  | rangeExpand | requestedSubresourceWithEmbeddedCredentials
  | rTCConstraintEnableDtlsSrtpFalse | rTCConstraintEnableDtlsSrtpTrue
  | rTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics
  | rTCPeerConnectionSdpSemanticsPlanB | rtcpMuxPolicyNegotiate
  | sharedArrayBufferConstructedWithoutIsolation
  | textToSpeech_DisallowedByAutoplay
  | v8SharedArrayBufferConstructedInExtensionWithoutIsolation
  | xHRJSONEncodingDetection
  | xMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload | xRSupportsSession
  deriving Repr, BEq, DecidableEq

instance : FromJSON DeprecationIssueType where
  parseJSON
    | .string "AuthorizationCoveredByWildcard" => .ok .authorizationCoveredByWildcard
    | .string "CanRequestURLHTTPContainingNewline" => .ok .canRequestURLHTTPContainingNewline
    | .string "ChromeLoadTimesConnectionInfo" => .ok .chromeLoadTimesConnectionInfo
    | .string "ChromeLoadTimesFirstPaintAfterLoadTime" => .ok .chromeLoadTimesFirstPaintAfterLoadTime
    | .string "ChromeLoadTimesWasAlternateProtocolAvailable" => .ok .chromeLoadTimesWasAlternateProtocolAvailable
    | .string "CookieWithTruncatingChar" => .ok .cookieWithTruncatingChar
    | .string "CrossOriginAccessBasedOnDocumentDomain" => .ok .crossOriginAccessBasedOnDocumentDomain
    | .string "CrossOriginWindowAlert" => .ok .crossOriginWindowAlert
    | .string "CrossOriginWindowConfirm" => .ok .crossOriginWindowConfirm
    | .string "CSSSelectorInternalMediaControlsOverlayCastButton" => .ok .cSSSelectorInternalMediaControlsOverlayCastButton
    | .string "DeprecationExample" => .ok .deprecationExample
    | .string "DocumentDomainSettingWithoutOriginAgentClusterHeader" => .ok .documentDomainSettingWithoutOriginAgentClusterHeader
    | .string "EventPath" => .ok .eventPath
    | .string "ExpectCTHeader" => .ok .expectCTHeader
    | .string "GeolocationInsecureOrigin" => .ok .geolocationInsecureOrigin
    | .string "GeolocationInsecureOriginDeprecatedNotRemoved" => .ok .geolocationInsecureOriginDeprecatedNotRemoved
    | .string "GetUserMediaInsecureOrigin" => .ok .getUserMediaInsecureOrigin
    | .string "HostCandidateAttributeGetter" => .ok .hostCandidateAttributeGetter
    | .string "IdentityInCanMakePaymentEvent" => .ok .identityInCanMakePaymentEvent
    | .string "InsecurePrivateNetworkSubresourceRequest" => .ok .insecurePrivateNetworkSubresourceRequest
    | .string "LocalCSSFileExtensionRejected" => .ok .localCSSFileExtensionRejected
    | .string "MediaSourceAbortRemove" => .ok .mediaSourceAbortRemove
    | .string "MediaSourceDurationTruncatingBuffered" => .ok .mediaSourceDurationTruncatingBuffered
    | .string "NoSysexWebMIDIWithoutPermission" => .ok .noSysexWebMIDIWithoutPermission
    | .string "NotificationInsecureOrigin" => .ok .notificationInsecureOrigin
    | .string "NotificationPermissionRequestedIframe" => .ok .notificationPermissionRequestedIframe
    | .string "ObsoleteWebRtcCipherSuite" => .ok .obsoleteWebRtcCipherSuite
    | .string "OpenWebDatabaseInsecureContext" => .ok .openWebDatabaseInsecureContext
    | .string "OverflowVisibleOnReplacedElement" => .ok .overflowVisibleOnReplacedElement
    | .string "PaymentInstruments" => .ok .paymentInstruments
    | .string "PaymentRequestCSPViolation" => .ok .paymentRequestCSPViolation
    | .string "PersistentQuotaType" => .ok .persistentQuotaType
    | .string "PictureSourceSrc" => .ok .pictureSourceSrc
    | .string "PrefixedCancelAnimationFrame" => .ok .prefixedCancelAnimationFrame
    | .string "PrefixedRequestAnimationFrame" => .ok .prefixedRequestAnimationFrame
    | .string "PrefixedStorageInfo" => .ok .prefixedStorageInfo
    | .string "PrefixedVideoDisplayingFullscreen" => .ok .prefixedVideoDisplayingFullscreen
    | .string "PrefixedVideoEnterFullscreen" => .ok .prefixedVideoEnterFullscreen
    | .string "PrefixedVideoEnterFullScreen" => .ok .prefixedVideoEnterFullScreen
    | .string "PrefixedVideoExitFullscreen" => .ok .prefixedVideoExitFullscreen
    | .string "PrefixedVideoExitFullScreen" => .ok .prefixedVideoExitFullScreen
    | .string "PrefixedVideoSupportsFullscreen" => .ok .prefixedVideoSupportsFullscreen
    | .string "RangeExpand" => .ok .rangeExpand
    | .string "RequestedSubresourceWithEmbeddedCredentials" => .ok .requestedSubresourceWithEmbeddedCredentials
    | .string "RTCConstraintEnableDtlsSrtpFalse" => .ok .rTCConstraintEnableDtlsSrtpFalse
    | .string "RTCConstraintEnableDtlsSrtpTrue" => .ok .rTCConstraintEnableDtlsSrtpTrue
    | .string "RTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics" => .ok .rTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics
    | .string "RTCPeerConnectionSdpSemanticsPlanB" => .ok .rTCPeerConnectionSdpSemanticsPlanB
    | .string "RtcpMuxPolicyNegotiate" => .ok .rtcpMuxPolicyNegotiate
    | .string "SharedArrayBufferConstructedWithoutIsolation" => .ok .sharedArrayBufferConstructedWithoutIsolation
    | .string "TextToSpeech_DisallowedByAutoplay" => .ok .textToSpeech_DisallowedByAutoplay
    | .string "V8SharedArrayBufferConstructedInExtensionWithoutIsolation" => .ok .v8SharedArrayBufferConstructedInExtensionWithoutIsolation
    | .string "XHRJSONEncodingDetection" => .ok .xHRJSONEncodingDetection
    | .string "XMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload" => .ok .xMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload
    | .string "XRSupportsSession" => .ok .xRSupportsSession
    | v => .error s!"failed to parse DeprecationIssueType: {repr v}"

instance : ToJSON DeprecationIssueType where
  toJSON
    | .authorizationCoveredByWildcard => .string "AuthorizationCoveredByWildcard"
    | .canRequestURLHTTPContainingNewline => .string "CanRequestURLHTTPContainingNewline"
    | .chromeLoadTimesConnectionInfo => .string "ChromeLoadTimesConnectionInfo"
    | .chromeLoadTimesFirstPaintAfterLoadTime => .string "ChromeLoadTimesFirstPaintAfterLoadTime"
    | .chromeLoadTimesWasAlternateProtocolAvailable => .string "ChromeLoadTimesWasAlternateProtocolAvailable"
    | .cookieWithTruncatingChar => .string "CookieWithTruncatingChar"
    | .crossOriginAccessBasedOnDocumentDomain => .string "CrossOriginAccessBasedOnDocumentDomain"
    | .crossOriginWindowAlert => .string "CrossOriginWindowAlert"
    | .crossOriginWindowConfirm => .string "CrossOriginWindowConfirm"
    | .cSSSelectorInternalMediaControlsOverlayCastButton => .string "CSSSelectorInternalMediaControlsOverlayCastButton"
    | .deprecationExample => .string "DeprecationExample"
    | .documentDomainSettingWithoutOriginAgentClusterHeader => .string "DocumentDomainSettingWithoutOriginAgentClusterHeader"
    | .eventPath => .string "EventPath"
    | .expectCTHeader => .string "ExpectCTHeader"
    | .geolocationInsecureOrigin => .string "GeolocationInsecureOrigin"
    | .geolocationInsecureOriginDeprecatedNotRemoved => .string "GeolocationInsecureOriginDeprecatedNotRemoved"
    | .getUserMediaInsecureOrigin => .string "GetUserMediaInsecureOrigin"
    | .hostCandidateAttributeGetter => .string "HostCandidateAttributeGetter"
    | .identityInCanMakePaymentEvent => .string "IdentityInCanMakePaymentEvent"
    | .insecurePrivateNetworkSubresourceRequest => .string "InsecurePrivateNetworkSubresourceRequest"
    | .localCSSFileExtensionRejected => .string "LocalCSSFileExtensionRejected"
    | .mediaSourceAbortRemove => .string "MediaSourceAbortRemove"
    | .mediaSourceDurationTruncatingBuffered => .string "MediaSourceDurationTruncatingBuffered"
    | .noSysexWebMIDIWithoutPermission => .string "NoSysexWebMIDIWithoutPermission"
    | .notificationInsecureOrigin => .string "NotificationInsecureOrigin"
    | .notificationPermissionRequestedIframe => .string "NotificationPermissionRequestedIframe"
    | .obsoleteWebRtcCipherSuite => .string "ObsoleteWebRtcCipherSuite"
    | .openWebDatabaseInsecureContext => .string "OpenWebDatabaseInsecureContext"
    | .overflowVisibleOnReplacedElement => .string "OverflowVisibleOnReplacedElement"
    | .paymentInstruments => .string "PaymentInstruments"
    | .paymentRequestCSPViolation => .string "PaymentRequestCSPViolation"
    | .persistentQuotaType => .string "PersistentQuotaType"
    | .pictureSourceSrc => .string "PictureSourceSrc"
    | .prefixedCancelAnimationFrame => .string "PrefixedCancelAnimationFrame"
    | .prefixedRequestAnimationFrame => .string "PrefixedRequestAnimationFrame"
    | .prefixedStorageInfo => .string "PrefixedStorageInfo"
    | .prefixedVideoDisplayingFullscreen => .string "PrefixedVideoDisplayingFullscreen"
    | .prefixedVideoEnterFullscreen => .string "PrefixedVideoEnterFullscreen"
    | .prefixedVideoEnterFullScreen => .string "PrefixedVideoEnterFullScreen"
    | .prefixedVideoExitFullscreen => .string "PrefixedVideoExitFullscreen"
    | .prefixedVideoExitFullScreen => .string "PrefixedVideoExitFullScreen"
    | .prefixedVideoSupportsFullscreen => .string "PrefixedVideoSupportsFullscreen"
    | .rangeExpand => .string "RangeExpand"
    | .requestedSubresourceWithEmbeddedCredentials => .string "RequestedSubresourceWithEmbeddedCredentials"
    | .rTCConstraintEnableDtlsSrtpFalse => .string "RTCConstraintEnableDtlsSrtpFalse"
    | .rTCConstraintEnableDtlsSrtpTrue => .string "RTCConstraintEnableDtlsSrtpTrue"
    | .rTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics => .string "RTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics"
    | .rTCPeerConnectionSdpSemanticsPlanB => .string "RTCPeerConnectionSdpSemanticsPlanB"
    | .rtcpMuxPolicyNegotiate => .string "RtcpMuxPolicyNegotiate"
    | .sharedArrayBufferConstructedWithoutIsolation => .string "SharedArrayBufferConstructedWithoutIsolation"
    | .textToSpeech_DisallowedByAutoplay => .string "TextToSpeech_DisallowedByAutoplay"
    | .v8SharedArrayBufferConstructedInExtensionWithoutIsolation => .string "V8SharedArrayBufferConstructedInExtensionWithoutIsolation"
    | .xHRJSONEncodingDetection => .string "XHRJSONEncodingDetection"
    | .xMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload => .string "XMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload"
    | .xRSupportsSession => .string "XRSupportsSession"

/-- This issue tracks information needed to print a deprecation message. -/
structure DeprecationIssueDetails where
  affectedFrame : Option AffectedFrame := none
  sourceCodeLocation : SourceCodeLocation
  type : DeprecationIssueType
  deriving Repr, BEq, DecidableEq

instance : FromJSON DeprecationIssueDetails where
  parseJSON v := do
    .ok
      { affectedFrame := ← (← Value.getFieldOpt v "affectedFrame").mapM FromJSON.parseJSON
        sourceCodeLocation := ← Value.getField v "sourceCodeLocation" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON }

instance : ToJSON DeprecationIssueDetails where
  toJSON p := Data.Json.object <|
    (p.affectedFrame.map fun v => ("affectedFrame", ToJSON.toJSON v)).toList
    ++ [("sourceCodeLocation", ToJSON.toJSON p.sourceCodeLocation), ("type", ToJSON.toJSON p.type)]

-- ── Client hint issues ──

/-- The reason a client-hint issue was raised. -/
inductive ClientHintIssueReason where
  | metaTagAllowListInvalidOrigin | metaTagModifiedHTML
  deriving Repr, BEq, DecidableEq

instance : FromJSON ClientHintIssueReason where
  parseJSON
    | .string "MetaTagAllowListInvalidOrigin" => .ok .metaTagAllowListInvalidOrigin
    | .string "MetaTagModifiedHTML" => .ok .metaTagModifiedHTML
    | v => .error s!"failed to parse ClientHintIssueReason: {repr v}"

instance : ToJSON ClientHintIssueReason where
  toJSON
    | .metaTagAllowListInvalidOrigin => .string "MetaTagAllowListInvalidOrigin"
    | .metaTagModifiedHTML => .string "MetaTagModifiedHTML"

/-- This issue tracks client hints related issues. It's used to deprecate old
    features, encourage the use of new ones, and provide general
    guidance. -/
structure ClientHintIssueDetails where
  sourceCodeLocation : SourceCodeLocation
  clientHintIssueReason : ClientHintIssueReason
  deriving Repr, BEq, DecidableEq

instance : FromJSON ClientHintIssueDetails where
  parseJSON v := do
    .ok
      { sourceCodeLocation := ← Value.getField v "sourceCodeLocation" >>= FromJSON.parseJSON
        clientHintIssueReason := ← Value.getField v "clientHintIssueReason" >>= FromJSON.parseJSON }

instance : ToJSON ClientHintIssueDetails where
  toJSON p := Data.Json.object
    [ ("sourceCodeLocation", ToJSON.toJSON p.sourceCodeLocation)
    , ("clientHintIssueReason", ToJSON.toJSON p.clientHintIssueReason) ]

-- ── Federated auth request issues ──

/-- The failure reason when a federated authentication request fails. Should
    be updated alongside `RequestIdTokenStatus` in
    `third_party/blink/public/mojom/devtools/inspector_issue.mojom` to
    include all cases except for success. -/
inductive FederatedAuthRequestIssueReason where
  | shouldEmbargo | tooManyRequests | manifestListHttpNotFound
  | manifestListNoResponse | manifestListInvalidResponse
  | manifestNotInManifestList | manifestListTooBig | manifestHttpNotFound
  | manifestNoResponse | manifestInvalidResponse
  | clientMetadataHttpNotFound | clientMetadataNoResponse
  | clientMetadataInvalidResponse | disabledInSettings | errorFetchingSignin
  | invalidSigninResponse | accountsHttpNotFound | accountsNoResponse
  | accountsInvalidResponse | idTokenHttpNotFound | idTokenNoResponse
  | idTokenInvalidResponse | idTokenInvalidRequest | errorIdToken | canceled
  | rpPageNotVisible
  deriving Repr, BEq, DecidableEq

instance : FromJSON FederatedAuthRequestIssueReason where
  parseJSON
    | .string "ShouldEmbargo" => .ok .shouldEmbargo
    | .string "TooManyRequests" => .ok .tooManyRequests
    | .string "ManifestListHttpNotFound" => .ok .manifestListHttpNotFound
    | .string "ManifestListNoResponse" => .ok .manifestListNoResponse
    | .string "ManifestListInvalidResponse" => .ok .manifestListInvalidResponse
    | .string "ManifestNotInManifestList" => .ok .manifestNotInManifestList
    | .string "ManifestListTooBig" => .ok .manifestListTooBig
    | .string "ManifestHttpNotFound" => .ok .manifestHttpNotFound
    | .string "ManifestNoResponse" => .ok .manifestNoResponse
    | .string "ManifestInvalidResponse" => .ok .manifestInvalidResponse
    | .string "ClientMetadataHttpNotFound" => .ok .clientMetadataHttpNotFound
    | .string "ClientMetadataNoResponse" => .ok .clientMetadataNoResponse
    | .string "ClientMetadataInvalidResponse" => .ok .clientMetadataInvalidResponse
    | .string "DisabledInSettings" => .ok .disabledInSettings
    | .string "ErrorFetchingSignin" => .ok .errorFetchingSignin
    | .string "InvalidSigninResponse" => .ok .invalidSigninResponse
    | .string "AccountsHttpNotFound" => .ok .accountsHttpNotFound
    | .string "AccountsNoResponse" => .ok .accountsNoResponse
    | .string "AccountsInvalidResponse" => .ok .accountsInvalidResponse
    | .string "IdTokenHttpNotFound" => .ok .idTokenHttpNotFound
    | .string "IdTokenNoResponse" => .ok .idTokenNoResponse
    | .string "IdTokenInvalidResponse" => .ok .idTokenInvalidResponse
    | .string "IdTokenInvalidRequest" => .ok .idTokenInvalidRequest
    | .string "ErrorIdToken" => .ok .errorIdToken
    | .string "Canceled" => .ok .canceled
    | .string "RpPageNotVisible" => .ok .rpPageNotVisible
    | v => .error s!"failed to parse FederatedAuthRequestIssueReason: {repr v}"

instance : ToJSON FederatedAuthRequestIssueReason where
  toJSON
    | .shouldEmbargo => .string "ShouldEmbargo"
    | .tooManyRequests => .string "TooManyRequests"
    | .manifestListHttpNotFound => .string "ManifestListHttpNotFound"
    | .manifestListNoResponse => .string "ManifestListNoResponse"
    | .manifestListInvalidResponse => .string "ManifestListInvalidResponse"
    | .manifestNotInManifestList => .string "ManifestNotInManifestList"
    | .manifestListTooBig => .string "ManifestListTooBig"
    | .manifestHttpNotFound => .string "ManifestHttpNotFound"
    | .manifestNoResponse => .string "ManifestNoResponse"
    | .manifestInvalidResponse => .string "ManifestInvalidResponse"
    | .clientMetadataHttpNotFound => .string "ClientMetadataHttpNotFound"
    | .clientMetadataNoResponse => .string "ClientMetadataNoResponse"
    | .clientMetadataInvalidResponse => .string "ClientMetadataInvalidResponse"
    | .disabledInSettings => .string "DisabledInSettings"
    | .errorFetchingSignin => .string "ErrorFetchingSignin"
    | .invalidSigninResponse => .string "InvalidSigninResponse"
    | .accountsHttpNotFound => .string "AccountsHttpNotFound"
    | .accountsNoResponse => .string "AccountsNoResponse"
    | .accountsInvalidResponse => .string "AccountsInvalidResponse"
    | .idTokenHttpNotFound => .string "IdTokenHttpNotFound"
    | .idTokenNoResponse => .string "IdTokenNoResponse"
    | .idTokenInvalidResponse => .string "IdTokenInvalidResponse"
    | .idTokenInvalidRequest => .string "IdTokenInvalidRequest"
    | .errorIdToken => .string "ErrorIdToken"
    | .canceled => .string "Canceled"
    | .rpPageNotVisible => .string "RpPageNotVisible"

/-- Details of a federated-auth-request issue. -/
structure FederatedAuthRequestIssueDetails where
  federatedAuthRequestIssueReason : FederatedAuthRequestIssueReason
  deriving Repr, BEq, DecidableEq

instance : FromJSON FederatedAuthRequestIssueDetails where
  parseJSON v := do
    .ok { federatedAuthRequestIssueReason := ← Value.getField v "federatedAuthRequestIssueReason" >>= FromJSON.parseJSON }

instance : ToJSON FederatedAuthRequestIssueDetails where
  toJSON p := Data.Json.object [("federatedAuthRequestIssueReason", ToJSON.toJSON p.federatedAuthRequestIssueReason)]

-- ── Inspector issues ──

/-- A unique identifier for the type of issue. Each type may use one of the
    optional fields in `InspectorIssueDetails` to convey more specific
    information about the kind of issue. -/
inductive InspectorIssueCode where
  | cookieIssue | mixedContentIssue | blockedByResponseIssue | heavyAdIssue
  | contentSecurityPolicyIssue | sharedArrayBufferIssue
  | trustedWebActivityIssue | lowTextContrastIssue | corsIssue
  | attributionReportingIssue | quirksModeIssue | navigatorUserAgentIssue
  | genericIssue | deprecationIssue | clientHintIssue
  | federatedAuthRequestIssue
  deriving Repr, BEq, DecidableEq

instance : FromJSON InspectorIssueCode where
  parseJSON
    | .string "CookieIssue" => .ok .cookieIssue
    | .string "MixedContentIssue" => .ok .mixedContentIssue
    | .string "BlockedByResponseIssue" => .ok .blockedByResponseIssue
    | .string "HeavyAdIssue" => .ok .heavyAdIssue
    | .string "ContentSecurityPolicyIssue" => .ok .contentSecurityPolicyIssue
    | .string "SharedArrayBufferIssue" => .ok .sharedArrayBufferIssue
    | .string "TrustedWebActivityIssue" => .ok .trustedWebActivityIssue
    | .string "LowTextContrastIssue" => .ok .lowTextContrastIssue
    | .string "CorsIssue" => .ok .corsIssue
    | .string "AttributionReportingIssue" => .ok .attributionReportingIssue
    | .string "QuirksModeIssue" => .ok .quirksModeIssue
    | .string "NavigatorUserAgentIssue" => .ok .navigatorUserAgentIssue
    | .string "GenericIssue" => .ok .genericIssue
    | .string "DeprecationIssue" => .ok .deprecationIssue
    | .string "ClientHintIssue" => .ok .clientHintIssue
    | .string "FederatedAuthRequestIssue" => .ok .federatedAuthRequestIssue
    | v => .error s!"failed to parse InspectorIssueCode: {repr v}"

instance : ToJSON InspectorIssueCode where
  toJSON
    | .cookieIssue => .string "CookieIssue"
    | .mixedContentIssue => .string "MixedContentIssue"
    | .blockedByResponseIssue => .string "BlockedByResponseIssue"
    | .heavyAdIssue => .string "HeavyAdIssue"
    | .contentSecurityPolicyIssue => .string "ContentSecurityPolicyIssue"
    | .sharedArrayBufferIssue => .string "SharedArrayBufferIssue"
    | .trustedWebActivityIssue => .string "TrustedWebActivityIssue"
    | .lowTextContrastIssue => .string "LowTextContrastIssue"
    | .corsIssue => .string "CorsIssue"
    | .attributionReportingIssue => .string "AttributionReportingIssue"
    | .quirksModeIssue => .string "QuirksModeIssue"
    | .navigatorUserAgentIssue => .string "NavigatorUserAgentIssue"
    | .genericIssue => .string "GenericIssue"
    | .deprecationIssue => .string "DeprecationIssue"
    | .clientHintIssue => .string "ClientHintIssue"
    | .federatedAuthRequestIssue => .string "FederatedAuthRequestIssue"

/-- This struct holds a list of optional fields with additional information
    specific to the kind of issue. When adding a new issue code, please also
    add a new optional field to this type. -/
structure InspectorIssueDetails where
  cookieIssueDetails : Option CookieIssueDetails := none
  mixedContentIssueDetails : Option MixedContentIssueDetails := none
  blockedByResponseIssueDetails : Option BlockedByResponseIssueDetails := none
  heavyAdIssueDetails : Option HeavyAdIssueDetails := none
  contentSecurityPolicyIssueDetails : Option ContentSecurityPolicyIssueDetails := none
  sharedArrayBufferIssueDetails : Option SharedArrayBufferIssueDetails := none
  twaQualityEnforcementDetails : Option TrustedWebActivityIssueDetails := none
  lowTextContrastIssueDetails : Option LowTextContrastIssueDetails := none
  corsIssueDetails : Option CorsIssueDetails := none
  attributionReportingIssueDetails : Option AttributionReportingIssueDetails := none
  quirksModeIssueDetails : Option QuirksModeIssueDetails := none
  navigatorUserAgentIssueDetails : Option NavigatorUserAgentIssueDetails := none
  genericIssueDetails : Option GenericIssueDetails := none
  deprecationIssueDetails : Option DeprecationIssueDetails := none
  clientHintIssueDetails : Option ClientHintIssueDetails := none
  federatedAuthRequestIssueDetails : Option FederatedAuthRequestIssueDetails := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON InspectorIssueDetails where
  parseJSON v := do
    .ok
      { cookieIssueDetails := ← (← Value.getFieldOpt v "cookieIssueDetails").mapM FromJSON.parseJSON
        mixedContentIssueDetails := ← (← Value.getFieldOpt v "mixedContentIssueDetails").mapM FromJSON.parseJSON
        blockedByResponseIssueDetails :=
          ← (← Value.getFieldOpt v "blockedByResponseIssueDetails").mapM FromJSON.parseJSON
        heavyAdIssueDetails := ← (← Value.getFieldOpt v "heavyAdIssueDetails").mapM FromJSON.parseJSON
        contentSecurityPolicyIssueDetails :=
          ← (← Value.getFieldOpt v "contentSecurityPolicyIssueDetails").mapM FromJSON.parseJSON
        sharedArrayBufferIssueDetails :=
          ← (← Value.getFieldOpt v "sharedArrayBufferIssueDetails").mapM FromJSON.parseJSON
        twaQualityEnforcementDetails := ← (← Value.getFieldOpt v "twaQualityEnforcementDetails").mapM FromJSON.parseJSON
        lowTextContrastIssueDetails := ← (← Value.getFieldOpt v "lowTextContrastIssueDetails").mapM FromJSON.parseJSON
        corsIssueDetails := ← (← Value.getFieldOpt v "corsIssueDetails").mapM FromJSON.parseJSON
        attributionReportingIssueDetails :=
          ← (← Value.getFieldOpt v "attributionReportingIssueDetails").mapM FromJSON.parseJSON
        quirksModeIssueDetails := ← (← Value.getFieldOpt v "quirksModeIssueDetails").mapM FromJSON.parseJSON
        navigatorUserAgentIssueDetails :=
          ← (← Value.getFieldOpt v "navigatorUserAgentIssueDetails").mapM FromJSON.parseJSON
        genericIssueDetails := ← (← Value.getFieldOpt v "genericIssueDetails").mapM FromJSON.parseJSON
        deprecationIssueDetails := ← (← Value.getFieldOpt v "deprecationIssueDetails").mapM FromJSON.parseJSON
        clientHintIssueDetails := ← (← Value.getFieldOpt v "clientHintIssueDetails").mapM FromJSON.parseJSON
        federatedAuthRequestIssueDetails :=
          ← (← Value.getFieldOpt v "federatedAuthRequestIssueDetails").mapM FromJSON.parseJSON }

instance : ToJSON InspectorIssueDetails where
  toJSON p := Data.Json.object <|
    (p.cookieIssueDetails.map fun v => ("cookieIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.mixedContentIssueDetails.map fun v => ("mixedContentIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.blockedByResponseIssueDetails.map fun v => ("blockedByResponseIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.heavyAdIssueDetails.map fun v => ("heavyAdIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.contentSecurityPolicyIssueDetails.map fun v => ("contentSecurityPolicyIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.sharedArrayBufferIssueDetails.map fun v => ("sharedArrayBufferIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.twaQualityEnforcementDetails.map fun v => ("twaQualityEnforcementDetails", ToJSON.toJSON v)).toList
    ++ (p.lowTextContrastIssueDetails.map fun v => ("lowTextContrastIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.corsIssueDetails.map fun v => ("corsIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.attributionReportingIssueDetails.map fun v => ("attributionReportingIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.quirksModeIssueDetails.map fun v => ("quirksModeIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.navigatorUserAgentIssueDetails.map fun v => ("navigatorUserAgentIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.genericIssueDetails.map fun v => ("genericIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.deprecationIssueDetails.map fun v => ("deprecationIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.clientHintIssueDetails.map fun v => ("clientHintIssueDetails", ToJSON.toJSON v)).toList
    ++ (p.federatedAuthRequestIssueDetails.map fun v => ("federatedAuthRequestIssueDetails", ToJSON.toJSON v)).toList

/-- A unique id for a DevTools inspector issue. Allows other entities (e.g.
    exceptions, CDP messages, console messages, etc.) to reference an
    issue. -/
abbrev IssueId := String

/-- An inspector issue reported from the back-end. -/
structure InspectorIssue where
  code : InspectorIssueCode
  details : InspectorIssueDetails
  /-- A unique id for this issue. May be omitted if no other entity (e.g.
      exception, CDP message, etc.) is referencing this issue. -/
  issueId : Option IssueId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON InspectorIssue where
  parseJSON v := do
    .ok
      { code := ← Value.getField v "code" >>= FromJSON.parseJSON
        details := ← Value.getField v "details" >>= FromJSON.parseJSON
        issueId := ← (← Value.getFieldOpt v "issueId").mapM FromJSON.parseJSON }

instance : ToJSON InspectorIssue where
  toJSON p := Data.Json.object <|
    [("code", ToJSON.toJSON p.code), ("details", ToJSON.toJSON p.details)]
    ++ (p.issueId.map fun v => ("issueId", ToJSON.toJSON v)).toList

-- ── Events ──

/-- Emitted when an inspector issue has been reported. -/
structure IssueAdded where
  issue : InspectorIssue
  deriving Repr, BEq, DecidableEq

instance : FromJSON IssueAdded where
  parseJSON v := do .ok { issue := ← Value.getField v "issue" >>= FromJSON.parseJSON }

instance : Event IssueAdded where
  eventName := "Audits.issueAdded"

-- ── Commands ──

/-- The encoding to re-encode a response image with, for
    `Audits.getEncodedResponse`. -/
inductive GetEncodedResponseEncoding where
  | webp | jpeg | png
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetEncodedResponseEncoding where
  parseJSON
    | .string "webp" => .ok .webp
    | .string "jpeg" => .ok .jpeg
    | .string "png" => .ok .png
    | v => .error s!"failed to parse GetEncodedResponseEncoding: {repr v}"

instance : ToJSON GetEncodedResponseEncoding where
  toJSON | .webp => .string "webp" | .jpeg => .string "jpeg" | .png => .string "png"

/-- Parameters of the `Audits.getEncodedResponse` command: returns the
    response body and size if it were re-encoded with the specified
    settings. Only applies to images. -/
structure PGetEncodedResponse where
  /-- Identifier of the network request to get content for. -/
  requestId : DOMPageNetworkEmulationSecurity.Network.RequestId
  /-- The encoding to use. -/
  encoding : GetEncodedResponseEncoding
  /-- The quality of the encoding (0-1). (defaults to 1) -/
  quality : Option Float := none
  /-- Whether to only return the size information (defaults to false). -/
  sizeOnly : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetEncodedResponse where
  toJSON p := Data.Json.object <|
    [("requestId", ToJSON.toJSON p.requestId), ("encoding", ToJSON.toJSON p.encoding)]
    ++ (p.quality.map fun v => ("quality", ToJSON.toJSON v)).toList
    ++ (p.sizeOnly.map fun v => ("sizeOnly", ToJSON.toJSON v)).toList

/-- Response of the `Audits.getEncodedResponse` command. -/
structure GetEncodedResponse where
  /-- The encoded body as a base64 string. Omitted if `sizeOnly` is true.
      (Encoded as a base64 string when passed over JSON.) -/
  body : Option String := none
  /-- Size before re-encoding. -/
  originalSize : Int
  /-- Size after re-encoding. -/
  encodedSize : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetEncodedResponse where
  parseJSON v := do
    .ok
      { body := ← (← Value.getFieldOpt v "body").mapM FromJSON.parseJSON
        originalSize := ← Value.getField v "originalSize" >>= FromJSON.parseJSON
        encodedSize := ← Value.getField v "encodedSize" >>= FromJSON.parseJSON }

instance : Command PGetEncodedResponse where
  Response := GetEncodedResponse
  commandName _ := "Audits.getEncodedResponse"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Audits.disable` command: disables issues domain,
    preventing further issues from being reported to the client. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Audits.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Audits.enable` command: enables issues domain, sends
    the issues collected so far to the client by means of the `issueAdded`
    event. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Audits.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Audits.checkContrast` command: runs the contrast check
    for the target page. Found issues are reported using the `issueAdded`
    event. -/
structure PCheckContrast where
  /-- Whether to report WCAG AAA level issues. Default is false. -/
  reportAAA : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PCheckContrast where
  toJSON p := Data.Json.object <| (p.reportAAA.map fun v => ("reportAAA", ToJSON.toJSON v)).toList

instance : Command PCheckContrast where
  Response := Unit
  commandName _ := "Audits.checkContrast"
  decodeResponse _ := .ok ()

end CDP.Domains.Audits
