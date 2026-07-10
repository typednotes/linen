/-
  Linen.CDP.Domains.Storage — the `Storage` CDP domain

  Ports `CDP.Domains.Storage` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring. Cross-domain
  references follow `CDP.Domains.Fetch`'s docstring: `open CDP.Domains` and
  refer to `BrowserTarget.Browser.BrowserContextID` /
  `DOMPageNetworkEmulationSecurity.Network.…` /
  `DOMPageNetworkEmulationSecurity.Page.…` unambiguously.

  None of this module's own types are self- or mutually-recursive, so every
  type here derives full `DecidableEq`.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.BrowserTarget

namespace CDP.Domains.Storage

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)
open CDP.Domains

-- ── Types ──

/-- A serialized storage key. -/
abbrev SerializedStorageKey := String

/-- Enum of possible storage types. -/
inductive StorageType where
  | appcache | cookies | fileSystems | indexeddb | localStorage | shaderCache | websql
  | serviceWorkers | cacheStorage | interestGroups | all | other
  deriving Repr, BEq, DecidableEq

instance : FromJSON StorageType where
  parseJSON
    | .string "appcache" => .ok .appcache
    | .string "cookies" => .ok .cookies
    | .string "file_systems" => .ok .fileSystems
    | .string "indexeddb" => .ok .indexeddb
    | .string "local_storage" => .ok .localStorage
    | .string "shader_cache" => .ok .shaderCache
    | .string "websql" => .ok .websql
    | .string "service_workers" => .ok .serviceWorkers
    | .string "cache_storage" => .ok .cacheStorage
    | .string "interest_groups" => .ok .interestGroups
    | .string "all" => .ok .all
    | .string "other" => .ok .other
    | v => .error s!"failed to parse Storage.StorageType: {repr v}"

instance : ToJSON StorageType where
  toJSON
    | .appcache => .string "appcache"
    | .cookies => .string "cookies"
    | .fileSystems => .string "file_systems"
    | .indexeddb => .string "indexeddb"
    | .localStorage => .string "local_storage"
    | .shaderCache => .string "shader_cache"
    | .websql => .string "websql"
    | .serviceWorkers => .string "service_workers"
    | .cacheStorage => .string "cache_storage"
    | .interestGroups => .string "interest_groups"
    | .all => .string "all"
    | .other => .string "other"

/-- Usage for a storage type. -/
structure UsageForType where
  /-- Name of storage type. -/
  storageType : StorageType
  /-- Storage usage (bytes). -/
  usage : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON UsageForType where
  parseJSON v := do
    .ok
      { storageType := ← Value.getField v "storageType" >>= FromJSON.parseJSON
        usage := ← Value.getField v "usage" >>= FromJSON.parseJSON }

instance : ToJSON UsageForType where
  toJSON p := Data.Json.object
    [("storageType", ToJSON.toJSON p.storageType), ("usage", ToJSON.toJSON p.usage)]

/-- Pair of issuer origin and number of available (signed, but not used) Trust
    Tokens from that issuer. -/
structure TrustTokens where
  issuerOrigin : String
  count : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON TrustTokens where
  parseJSON v := do
    .ok
      { issuerOrigin := ← Value.getField v "issuerOrigin" >>= FromJSON.parseJSON
        count := ← Value.getField v "count" >>= FromJSON.parseJSON }

instance : ToJSON TrustTokens where
  toJSON p := Data.Json.object
    [("issuerOrigin", ToJSON.toJSON p.issuerOrigin), ("count", ToJSON.toJSON p.count)]

/-- Enum of interest group access types. -/
inductive InterestGroupAccessType where
  | join | leave | update | bid | win
  deriving Repr, BEq, DecidableEq

instance : FromJSON InterestGroupAccessType where
  parseJSON
    | .string "join" => .ok .join
    | .string "leave" => .ok .leave
    | .string "update" => .ok .update
    | .string "bid" => .ok .bid
    | .string "win" => .ok .win
    | v => .error s!"failed to parse Storage.InterestGroupAccessType: {repr v}"

instance : ToJSON InterestGroupAccessType where
  toJSON
    | .join => .string "join"
    | .leave => .string "leave"
    | .update => .string "update"
    | .bid => .string "bid"
    | .win => .string "win"

/-- Ad advertising element inside an interest group. -/
structure InterestGroupAd where
  renderUrl : String
  metadata : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON InterestGroupAd where
  parseJSON v := do
    .ok
      { renderUrl := ← Value.getField v "renderUrl" >>= FromJSON.parseJSON
        metadata := ← (← Value.getFieldOpt v "metadata").mapM FromJSON.parseJSON }

instance : ToJSON InterestGroupAd where
  toJSON p := Data.Json.object <|
    [("renderUrl", ToJSON.toJSON p.renderUrl)]
    ++ (p.metadata.map fun v => ("metadata", ToJSON.toJSON v)).toList

/-- The full details of an interest group. -/
structure InterestGroupDetails where
  ownerOrigin : String
  name : String
  expirationTime : DOMPageNetworkEmulationSecurity.Network.TimeSinceEpoch
  joiningOrigin : String
  biddingUrl : Option String := none
  biddingWasmHelperUrl : Option String := none
  updateUrl : Option String := none
  trustedBiddingSignalsUrl : Option String := none
  trustedBiddingSignalsKeys : List String
  userBiddingSignals : Option String := none
  ads : List InterestGroupAd
  adComponents : List InterestGroupAd
  deriving Repr, BEq, DecidableEq

instance : FromJSON InterestGroupDetails where
  parseJSON v := do
    .ok
      { ownerOrigin := ← Value.getField v "ownerOrigin" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        expirationTime := ← Value.getField v "expirationTime" >>= FromJSON.parseJSON
        joiningOrigin := ← Value.getField v "joiningOrigin" >>= FromJSON.parseJSON
        biddingUrl := ← (← Value.getFieldOpt v "biddingUrl").mapM FromJSON.parseJSON
        biddingWasmHelperUrl := ← (← Value.getFieldOpt v "biddingWasmHelperUrl").mapM FromJSON.parseJSON
        updateUrl := ← (← Value.getFieldOpt v "updateUrl").mapM FromJSON.parseJSON
        trustedBiddingSignalsUrl := ← (← Value.getFieldOpt v "trustedBiddingSignalsUrl").mapM FromJSON.parseJSON
        trustedBiddingSignalsKeys := ← Value.getField v "trustedBiddingSignalsKeys" >>= FromJSON.parseJSON
        userBiddingSignals := ← (← Value.getFieldOpt v "userBiddingSignals").mapM FromJSON.parseJSON
        ads := ← Value.getField v "ads" >>= FromJSON.parseJSON
        adComponents := ← Value.getField v "adComponents" >>= FromJSON.parseJSON }

instance : ToJSON InterestGroupDetails where
  toJSON p := Data.Json.object <|
    [("ownerOrigin", ToJSON.toJSON p.ownerOrigin)]
    ++ [("name", ToJSON.toJSON p.name)]
    ++ [("expirationTime", ToJSON.toJSON p.expirationTime)]
    ++ [("joiningOrigin", ToJSON.toJSON p.joiningOrigin)]
    ++ (p.biddingUrl.map fun v => ("biddingUrl", ToJSON.toJSON v)).toList
    ++ (p.biddingWasmHelperUrl.map fun v => ("biddingWasmHelperUrl", ToJSON.toJSON v)).toList
    ++ (p.updateUrl.map fun v => ("updateUrl", ToJSON.toJSON v)).toList
    ++ (p.trustedBiddingSignalsUrl.map fun v => ("trustedBiddingSignalsUrl", ToJSON.toJSON v)).toList
    ++ [("trustedBiddingSignalsKeys", ToJSON.toJSON p.trustedBiddingSignalsKeys)]
    ++ (p.userBiddingSignals.map fun v => ("userBiddingSignals", ToJSON.toJSON v)).toList
    ++ [("ads", ToJSON.toJSON p.ads)]
    ++ [("adComponents", ToJSON.toJSON p.adComponents)]

-- ── Events ──

/-- The `Storage.cacheStorageContentUpdated` event. -/
structure CacheStorageContentUpdated where
  /-- Origin to update. -/
  origin : String
  /-- Name of cache in origin. -/
  cacheName : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON CacheStorageContentUpdated where
  parseJSON v := do
    .ok
      { origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        cacheName := ← Value.getField v "cacheName" >>= FromJSON.parseJSON }

instance : Event CacheStorageContentUpdated where
  eventName := "Storage.cacheStorageContentUpdated"

/-- The `Storage.cacheStorageListUpdated` event. -/
structure CacheStorageListUpdated where
  /-- Origin to update. -/
  origin : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON CacheStorageListUpdated where
  parseJSON v := do .ok { origin := ← Value.getField v "origin" >>= FromJSON.parseJSON }

instance : Event CacheStorageListUpdated where
  eventName := "Storage.cacheStorageListUpdated"

/-- The `Storage.indexedDBContentUpdated` event. -/
structure IndexedDBContentUpdated where
  /-- Origin to update. -/
  origin : String
  /-- Storage key to update. -/
  storageKey : String
  /-- Database to update. -/
  databaseName : String
  /-- ObjectStore to update. -/
  objectStoreName : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON IndexedDBContentUpdated where
  parseJSON v := do
    .ok
      { origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        storageKey := ← Value.getField v "storageKey" >>= FromJSON.parseJSON
        databaseName := ← Value.getField v "databaseName" >>= FromJSON.parseJSON
        objectStoreName := ← Value.getField v "objectStoreName" >>= FromJSON.parseJSON }

instance : Event IndexedDBContentUpdated where
  eventName := "Storage.indexedDBContentUpdated"

/-- The `Storage.indexedDBListUpdated` event. -/
structure IndexedDBListUpdated where
  /-- Origin to update. -/
  origin : String
  /-- Storage key to update. -/
  storageKey : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON IndexedDBListUpdated where
  parseJSON v := do
    .ok
      { origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        storageKey := ← Value.getField v "storageKey" >>= FromJSON.parseJSON }

instance : Event IndexedDBListUpdated where
  eventName := "Storage.indexedDBListUpdated"

/-- The `Storage.interestGroupAccessed` event. -/
structure InterestGroupAccessed where
  accessTime : DOMPageNetworkEmulationSecurity.Network.TimeSinceEpoch
  type : InterestGroupAccessType
  ownerOrigin : String
  name : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON InterestGroupAccessed where
  parseJSON v := do
    .ok
      { accessTime := ← Value.getField v "accessTime" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        ownerOrigin := ← Value.getField v "ownerOrigin" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON }

instance : Event InterestGroupAccessed where
  eventName := "Storage.interestGroupAccessed"

-- ── Commands ──

/-- Parameters of the `Storage.getStorageKeyForFrame` command: returns a
    storage key given a frame id. -/
structure PGetStorageKeyForFrame where
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetStorageKeyForFrame where
  toJSON p := Data.Json.object [("frameId", ToJSON.toJSON p.frameId)]

/-- Response of the `Storage.getStorageKeyForFrame` command. -/
structure GetStorageKeyForFrame where
  storageKey : SerializedStorageKey
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetStorageKeyForFrame where
  parseJSON v := do .ok { storageKey := ← Value.getField v "storageKey" >>= FromJSON.parseJSON }

instance : Command PGetStorageKeyForFrame where
  Response := GetStorageKeyForFrame
  commandName _ := "Storage.getStorageKeyForFrame"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Storage.clearDataForOrigin` command: clears storage for
    origin. -/
structure PClearDataForOrigin where
  /-- Security origin. -/
  origin : String
  /-- Comma separated list of `StorageType` to clear. -/
  storageTypes : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClearDataForOrigin where
  toJSON p := Data.Json.object
    [("origin", ToJSON.toJSON p.origin), ("storageTypes", ToJSON.toJSON p.storageTypes)]

instance : Command PClearDataForOrigin where
  Response := Unit
  commandName _ := "Storage.clearDataForOrigin"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.clearDataForStorageKey` command: clears storage
    for storage key. -/
structure PClearDataForStorageKey where
  /-- Storage key. -/
  storageKey : String
  /-- Comma separated list of `StorageType` to clear. -/
  storageTypes : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClearDataForStorageKey where
  toJSON p := Data.Json.object
    [("storageKey", ToJSON.toJSON p.storageKey), ("storageTypes", ToJSON.toJSON p.storageTypes)]

instance : Command PClearDataForStorageKey where
  Response := Unit
  commandName _ := "Storage.clearDataForStorageKey"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.getCookies` command: returns all browser
    cookies. -/
structure PGetCookies where
  /-- Browser context to use when called on the browser endpoint. -/
  browserContextId : Option BrowserTarget.Browser.BrowserContextID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetCookies where
  toJSON p := Data.Json.object <|
    (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList

/-- Response of the `Storage.getCookies` command. -/
structure GetCookies where
  /-- Array of cookie objects. -/
  cookies : List DOMPageNetworkEmulationSecurity.Network.Cookie
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetCookies where
  parseJSON v := do .ok { cookies := ← Value.getField v "cookies" >>= FromJSON.parseJSON }

instance : Command PGetCookies where
  Response := GetCookies
  commandName _ := "Storage.getCookies"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Storage.setCookies` command: sets given cookies. -/
structure PSetCookies where
  /-- Cookies to be set. -/
  cookies : List DOMPageNetworkEmulationSecurity.Network.CookieParam
  /-- Browser context to use when called on the browser endpoint. -/
  browserContextId : Option BrowserTarget.Browser.BrowserContextID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetCookies where
  toJSON p := Data.Json.object <|
    [("cookies", ToJSON.toJSON p.cookies)]
    ++ (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList

instance : Command PSetCookies where
  Response := Unit
  commandName _ := "Storage.setCookies"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.clearCookies` command: clears cookies. -/
structure PClearCookies where
  /-- Browser context to use when called on the browser endpoint. -/
  browserContextId : Option BrowserTarget.Browser.BrowserContextID := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClearCookies where
  toJSON p := Data.Json.object <|
    (p.browserContextId.map fun v => ("browserContextId", ToJSON.toJSON v)).toList

instance : Command PClearCookies where
  Response := Unit
  commandName _ := "Storage.clearCookies"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.getUsageAndQuota` command: returns usage and
    quota in bytes. -/
structure PGetUsageAndQuota where
  /-- Security origin. -/
  origin : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetUsageAndQuota where
  toJSON p := Data.Json.object [("origin", ToJSON.toJSON p.origin)]

/-- Response of the `Storage.getUsageAndQuota` command. -/
structure GetUsageAndQuota where
  /-- Storage usage (bytes). -/
  usage : Float
  /-- Storage quota (bytes). -/
  quota : Float
  /-- Whether or not the origin has an active storage quota override. -/
  overrideActive : Bool
  /-- Storage usage per type (bytes). -/
  usageBreakdown : List UsageForType
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetUsageAndQuota where
  parseJSON v := do
    .ok
      { usage := ← Value.getField v "usage" >>= FromJSON.parseJSON
        quota := ← Value.getField v "quota" >>= FromJSON.parseJSON
        overrideActive := ← Value.getField v "overrideActive" >>= FromJSON.parseJSON
        usageBreakdown := ← Value.getField v "usageBreakdown" >>= FromJSON.parseJSON }

instance : Command PGetUsageAndQuota where
  Response := GetUsageAndQuota
  commandName _ := "Storage.getUsageAndQuota"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Storage.overrideQuotaForOrigin` command: override quota
    for the specified origin. -/
structure POverrideQuotaForOrigin where
  /-- Security origin. -/
  origin : String
  /-- The quota size (in bytes) to override the original quota with.
      If this is called multiple times, the overridden quota will be equal to
      the `quotaSize` provided in the final call. If this is called without
      specifying a `quotaSize`, the quota will be reset to the default value
      for the specified origin. If this is called multiple times with
      different origins, the override will be maintained for each origin
      until it is disabled (called without a `quotaSize`). -/
  quotaSize : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON POverrideQuotaForOrigin where
  toJSON p := Data.Json.object <|
    [("origin", ToJSON.toJSON p.origin)]
    ++ (p.quotaSize.map fun v => ("quotaSize", ToJSON.toJSON v)).toList

instance : Command POverrideQuotaForOrigin where
  Response := Unit
  commandName _ := "Storage.overrideQuotaForOrigin"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.trackCacheStorageForOrigin` command: registers
    origin to be notified when an update occurs to its cache storage list. -/
structure PTrackCacheStorageForOrigin where
  /-- Security origin. -/
  origin : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTrackCacheStorageForOrigin where
  toJSON p := Data.Json.object [("origin", ToJSON.toJSON p.origin)]

instance : Command PTrackCacheStorageForOrigin where
  Response := Unit
  commandName _ := "Storage.trackCacheStorageForOrigin"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.trackIndexedDBForOrigin` command: registers
    origin to be notified when an update occurs to its IndexedDB. -/
structure PTrackIndexedDBForOrigin where
  /-- Security origin. -/
  origin : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTrackIndexedDBForOrigin where
  toJSON p := Data.Json.object [("origin", ToJSON.toJSON p.origin)]

instance : Command PTrackIndexedDBForOrigin where
  Response := Unit
  commandName _ := "Storage.trackIndexedDBForOrigin"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.trackIndexedDBForStorageKey` command: registers
    storage key to be notified when an update occurs to its IndexedDB. -/
structure PTrackIndexedDBForStorageKey where
  /-- Storage key. -/
  storageKey : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTrackIndexedDBForStorageKey where
  toJSON p := Data.Json.object [("storageKey", ToJSON.toJSON p.storageKey)]

instance : Command PTrackIndexedDBForStorageKey where
  Response := Unit
  commandName _ := "Storage.trackIndexedDBForStorageKey"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.untrackCacheStorageForOrigin` command:
    unregisters origin from receiving notifications for cache storage. -/
structure PUntrackCacheStorageForOrigin where
  /-- Security origin. -/
  origin : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PUntrackCacheStorageForOrigin where
  toJSON p := Data.Json.object [("origin", ToJSON.toJSON p.origin)]

instance : Command PUntrackCacheStorageForOrigin where
  Response := Unit
  commandName _ := "Storage.untrackCacheStorageForOrigin"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.untrackIndexedDBForOrigin` command:
    unregisters origin from receiving notifications for IndexedDB. -/
structure PUntrackIndexedDBForOrigin where
  /-- Security origin. -/
  origin : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PUntrackIndexedDBForOrigin where
  toJSON p := Data.Json.object [("origin", ToJSON.toJSON p.origin)]

instance : Command PUntrackIndexedDBForOrigin where
  Response := Unit
  commandName _ := "Storage.untrackIndexedDBForOrigin"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.untrackIndexedDBForStorageKey` command:
    unregisters storage key from receiving notifications for IndexedDB. -/
structure PUntrackIndexedDBForStorageKey where
  /-- Storage key. -/
  storageKey : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PUntrackIndexedDBForStorageKey where
  toJSON p := Data.Json.object [("storageKey", ToJSON.toJSON p.storageKey)]

instance : Command PUntrackIndexedDBForStorageKey where
  Response := Unit
  commandName _ := "Storage.untrackIndexedDBForStorageKey"
  decodeResponse _ := .ok ()

/-- Parameters of the `Storage.getTrustTokens` command: returns the number of
    stored Trust Tokens per issuer for the current browsing context. -/
structure PGetTrustTokens where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetTrustTokens where
  toJSON _ := .null

/-- Response of the `Storage.getTrustTokens` command. -/
structure GetTrustTokens where
  tokens : List TrustTokens
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetTrustTokens where
  parseJSON v := do .ok { tokens := ← Value.getField v "tokens" >>= FromJSON.parseJSON }

instance : Command PGetTrustTokens where
  Response := GetTrustTokens
  commandName _ := "Storage.getTrustTokens"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Storage.clearTrustTokens` command: removes all Trust
    Tokens issued by the provided `issuerOrigin`. Leaves other stored data,
    including the issuer's Redemption Records, intact. -/
structure PClearTrustTokens where
  issuerOrigin : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClearTrustTokens where
  toJSON p := Data.Json.object [("issuerOrigin", ToJSON.toJSON p.issuerOrigin)]

/-- Response of the `Storage.clearTrustTokens` command. -/
structure ClearTrustTokens where
  /-- `true` if any tokens were deleted, `false` otherwise. -/
  didDeleteTokens : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON ClearTrustTokens where
  parseJSON v := do
    .ok { didDeleteTokens := ← Value.getField v "didDeleteTokens" >>= FromJSON.parseJSON }

instance : Command PClearTrustTokens where
  Response := ClearTrustTokens
  commandName _ := "Storage.clearTrustTokens"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Storage.getInterestGroupDetails` command: gets details
    for a named interest group. -/
structure PGetInterestGroupDetails where
  ownerOrigin : String
  name : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetInterestGroupDetails where
  toJSON p := Data.Json.object
    [("ownerOrigin", ToJSON.toJSON p.ownerOrigin), ("name", ToJSON.toJSON p.name)]

/-- Response of the `Storage.getInterestGroupDetails` command. -/
structure GetInterestGroupDetails where
  details : InterestGroupDetails
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetInterestGroupDetails where
  parseJSON v := do .ok { details := ← Value.getField v "details" >>= FromJSON.parseJSON }

instance : Command PGetInterestGroupDetails where
  Response := GetInterestGroupDetails
  commandName _ := "Storage.getInterestGroupDetails"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Storage.setInterestGroupTracking` command:
    enables/disables issuing of `interestGroupAccessed` events. -/
structure PSetInterestGroupTracking where
  enable : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetInterestGroupTracking where
  toJSON p := Data.Json.object [("enable", ToJSON.toJSON p.enable)]

instance : Command PSetInterestGroupTracking where
  Response := Unit
  commandName _ := "Storage.setInterestGroupTracking"
  decodeResponse _ := .ok ()

end CDP.Domains.Storage
