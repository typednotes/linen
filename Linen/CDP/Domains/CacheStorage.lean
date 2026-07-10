/-
  Linen.CDP.Domains.CacheStorage — the `CacheStorage` CDP domain

  Ports `CDP.Domains.CacheStorage` (see `docs/imports/cdp/dependencies.md`).
  Every command/response/type name below drops the upstream `CacheStorage`
  prefix (redundant once inside this namespace) and every record field drops
  its `cacheStorage`/`pCacheStorage...` prefix likewise — Lean's namespaces and
  dot-notation already disambiguate what Haskell's single flat module
  namespace needed name-mangling to achieve. Upstream's separate smart
  constructor functions (`pCacheStorageDeleteCache`, ...) are also dropped:
  Lean's own structure field defaults (`optional : Bool := false` etc.) and
  `{ field := val }` literal syntax already give the same "only specify the
  required fields" ergonomics.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.CacheStorage

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

/-- Unique identifier of the Cache object. -/
abbrev CacheId := String

/-- Type of HTTP response cached. -/
inductive CachedResponseType where
  | basic | cors | default | error | opaqueResponse | opaqueRedirect
  deriving Repr, BEq, DecidableEq

instance : FromJSON CachedResponseType where
  parseJSON
    | .string "basic" => .ok .basic
    | .string "cors" => .ok .cors
    | .string "default" => .ok .default
    | .string "error" => .ok .error
    | .string "opaqueResponse" => .ok .opaqueResponse
    | .string "opaqueRedirect" => .ok .opaqueRedirect
    | v => .error s!"failed to parse CachedResponseType: {repr v}"

instance : ToJSON CachedResponseType where
  toJSON
    | .basic => .string "basic"
    | .cors => .string "cors"
    | .default => .string "default"
    | .error => .string "error"
    | .opaqueResponse => .string "opaqueResponse"
    | .opaqueRedirect => .string "opaqueRedirect"

/-- A cache request/response header. -/
structure Header where
  name : String
  value : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Header where
  parseJSON v := do
    .ok { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        , value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON Header where
  toJSON h := Data.Json.object [("name", ToJSON.toJSON h.name), ("value", ToJSON.toJSON h.value)]

/-- A cache data entry. -/
structure DataEntry where
  requestURL : String
  requestMethod : String
  requestHeaders : List Header
  /-- Number of seconds since epoch. -/
  responseTime : Float
  responseStatus : Int
  responseStatusText : String
  responseType : CachedResponseType
  responseHeaders : List Header
  deriving Repr, BEq, DecidableEq

instance : FromJSON DataEntry where
  parseJSON v := do
    .ok
      { requestURL := ← Value.getField v "requestURL" >>= FromJSON.parseJSON
        requestMethod := ← Value.getField v "requestMethod" >>= FromJSON.parseJSON
        requestHeaders := ← Value.getField v "requestHeaders" >>= FromJSON.parseJSON
        responseTime := ← Value.getField v "responseTime" >>= FromJSON.parseJSON
        responseStatus := ← Value.getField v "responseStatus" >>= FromJSON.parseJSON
        responseStatusText := ← Value.getField v "responseStatusText" >>= FromJSON.parseJSON
        responseType := ← Value.getField v "responseType" >>= FromJSON.parseJSON
        responseHeaders := ← Value.getField v "responseHeaders" >>= FromJSON.parseJSON }

instance : ToJSON DataEntry where
  toJSON e := Data.Json.object
    [ ("requestURL", ToJSON.toJSON e.requestURL), ("requestMethod", ToJSON.toJSON e.requestMethod)
    , ("requestHeaders", ToJSON.toJSON e.requestHeaders), ("responseTime", ToJSON.toJSON e.responseTime)
    , ("responseStatus", ToJSON.toJSON e.responseStatus)
    , ("responseStatusText", ToJSON.toJSON e.responseStatusText)
    , ("responseType", ToJSON.toJSON e.responseType), ("responseHeaders", ToJSON.toJSON e.responseHeaders) ]

/-- Cache identifier. -/
structure Cache where
  cacheId : CacheId
  securityOrigin : String
  cacheName : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Cache where
  parseJSON v := do
    .ok
      { cacheId := ← Value.getField v "cacheId" >>= FromJSON.parseJSON
        securityOrigin := ← Value.getField v "securityOrigin" >>= FromJSON.parseJSON
        cacheName := ← Value.getField v "cacheName" >>= FromJSON.parseJSON }

instance : ToJSON Cache where
  toJSON c := Data.Json.object
    [ ("cacheId", ToJSON.toJSON c.cacheId), ("securityOrigin", ToJSON.toJSON c.securityOrigin)
    , ("cacheName", ToJSON.toJSON c.cacheName) ]

/-- A cached response. -/
structure CachedResponse where
  /-- Entry content, base64-encoded. -/
  body : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON CachedResponse where
  parseJSON v := do .ok { body := ← Value.getField v "body" >>= FromJSON.parseJSON }

instance : ToJSON CachedResponse where
  toJSON r := Data.Json.object [("body", ToJSON.toJSON r.body)]

-- ── deleteCache ──

/-- Parameters of the `CacheStorage.deleteCache` command. -/
structure PDeleteCache where
  /-- Id of cache for deletion. -/
  cacheId : CacheId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDeleteCache where
  toJSON p := Data.Json.object [("cacheId", ToJSON.toJSON p.cacheId)]

instance : Command PDeleteCache where
  Response := Unit
  commandName _ := "CacheStorage.deleteCache"
  decodeResponse _ := .ok ()

-- ── deleteEntry ──

/-- Parameters of the `CacheStorage.deleteEntry` command. -/
structure PDeleteEntry where
  /-- Id of cache where the entry will be deleted. -/
  cacheId : CacheId
  /-- URL spec of the request. -/
  request : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDeleteEntry where
  toJSON p := Data.Json.object [("cacheId", ToJSON.toJSON p.cacheId), ("request", ToJSON.toJSON p.request)]

instance : Command PDeleteEntry where
  Response := Unit
  commandName _ := "CacheStorage.deleteEntry"
  decodeResponse _ := .ok ()

-- ── requestCacheNames ──

/-- Parameters of the `CacheStorage.requestCacheNames` command. -/
structure PRequestCacheNames where
  /-- Security origin. -/
  securityOrigin : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRequestCacheNames where
  toJSON p := Data.Json.object [("securityOrigin", ToJSON.toJSON p.securityOrigin)]

/-- Response of the `CacheStorage.requestCacheNames` command. -/
structure RequestCacheNames where
  /-- Caches for the security origin. -/
  caches : List Cache
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestCacheNames where
  parseJSON v := do .ok { caches := ← Value.getField v "caches" >>= FromJSON.parseJSON }

instance : Command PRequestCacheNames where
  Response := RequestCacheNames
  commandName _ := "CacheStorage.requestCacheNames"
  decodeResponse := FromJSON.parseJSON

-- ── requestCachedResponse ──

/-- Parameters of the `CacheStorage.requestCachedResponse` command. -/
structure PRequestCachedResponse where
  /-- Id of cache that contains the entry. -/
  cacheId : CacheId
  /-- URL spec of the request. -/
  requestURL : String
  /-- Headers of the request. -/
  requestHeaders : List Header
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRequestCachedResponse where
  toJSON p := Data.Json.object
    [ ("cacheId", ToJSON.toJSON p.cacheId), ("requestURL", ToJSON.toJSON p.requestURL)
    , ("requestHeaders", ToJSON.toJSON p.requestHeaders) ]

/-- Response of the `CacheStorage.requestCachedResponse` command. -/
structure RequestCachedResponse where
  /-- Response read from the cache. -/
  response : CachedResponse
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestCachedResponse where
  parseJSON v := do .ok { response := ← Value.getField v "response" >>= FromJSON.parseJSON }

instance : Command PRequestCachedResponse where
  Response := RequestCachedResponse
  commandName _ := "CacheStorage.requestCachedResponse"
  decodeResponse := FromJSON.parseJSON

-- ── requestEntries ──

/-- Parameters of the `CacheStorage.requestEntries` command. -/
structure PRequestEntries where
  /-- ID of cache to get entries from. -/
  cacheId : CacheId
  /-- Number of records to skip. -/
  skipCount : Option Int := none
  /-- Number of records to fetch. -/
  pageSize : Option Int := none
  /-- If present, only return the entries containing this substring in the path. -/
  pathFilter : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRequestEntries where
  toJSON p := Data.Json.object <|
    [("cacheId", ToJSON.toJSON p.cacheId)]
    ++ (p.skipCount.map fun v => ("skipCount", ToJSON.toJSON v)).toList
    ++ (p.pageSize.map fun v => ("pageSize", ToJSON.toJSON v)).toList
    ++ (p.pathFilter.map fun v => ("pathFilter", ToJSON.toJSON v)).toList

/-- Response of the `CacheStorage.requestEntries` command. -/
structure RequestEntries where
  /-- Array of object store data entries. -/
  cacheDataEntries : List DataEntry
  /-- Count of returned entries from this storage. If `pathFilter` is empty, it
      is the count of all entries from this storage. -/
  returnCount : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestEntries where
  parseJSON v := do
    .ok
      { cacheDataEntries := ← Value.getField v "cacheDataEntries" >>= FromJSON.parseJSON
        returnCount := ← Value.getField v "returnCount" >>= FromJSON.parseJSON }

instance : Command PRequestEntries where
  Response := RequestEntries
  commandName _ := "CacheStorage.requestEntries"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.CacheStorage
