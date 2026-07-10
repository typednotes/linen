/-
  Tests for `Linen.CDP.Domains.CacheStorage`.
-/
import Linen.CDP.Domains.CacheStorage

open CDP.Domains.CacheStorage
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.CacheStorage

/-! ### CachedResponseType round-trip -/

#guard decodeAs "\"basic\"" (α := CachedResponseType) = .ok .basic
#guard decodeAs "\"opaqueRedirect\"" (α := CachedResponseType) = .ok .opaqueRedirect
#guard encode (ToJSON.toJSON CachedResponseType.cors) = "\"cors\""

/-! ### Header FromJSON/ToJSON -/

#guard decodeAs "{\"name\": \"Content-Type\", \"value\": \"text/plain\"}" (α := Header)
  = .ok { name := "Content-Type", value := "text/plain" }
#guard encode (ToJSON.toJSON ({ name := "n", value := "v" } : Header)) = "{\"name\":\"n\",\"value\":\"v\"}"

/-! ### PDeleteCache — command with a `()` response -/

#guard Command.commandName ({ cacheId := "c1" } : PDeleteCache) = "CacheStorage.deleteCache"
#guard encode (ToJSON.toJSON ({ cacheId := "c1" } : PDeleteCache)) = "{\"cacheId\":\"c1\"}"
#guard match Command.decodeResponse (α := PDeleteCache) (.object []) with | .ok () => true | _ => false

/-! ### PDeleteEntry -/

#guard Command.commandName ({ cacheId := "c1", request := "http://x" } : PDeleteEntry)
  = "CacheStorage.deleteEntry"

/-! ### PRequestCacheNames / RequestCacheNames -/

#guard Command.commandName ({ securityOrigin := "https://example.com" } : PRequestCacheNames)
  = "CacheStorage.requestCacheNames"
#guard decodeAs
    "{\"caches\": [{\"cacheId\": \"1\", \"securityOrigin\": \"o\", \"cacheName\": \"n\"}]}"
    (α := RequestCacheNames)
  = .ok { caches := [{ cacheId := "1", securityOrigin := "o", cacheName := "n" }] }

/-! ### PRequestEntries — optional fields only serialized when present -/

#guard encode (ToJSON.toJSON ({ cacheId := "c1" } : PRequestEntries)) = "{\"cacheId\":\"c1\"}"
#guard encode (ToJSON.toJSON ({ cacheId := "c1", skipCount := some 5, pageSize := some 10
                              , pathFilter := some "x" } : PRequestEntries))
  = "{\"cacheId\":\"c1\",\"skipCount\":5,\"pageSize\":10,\"pathFilter\":\"x\"}"

/-! ### RequestEntries decode -/

#guard decodeAs
    "{\"cacheDataEntries\": [], \"returnCount\": 0}"
    (α := RequestEntries)
  = .ok { cacheDataEntries := [], returnCount := 0 }

end Tests.CDP.Domains.CacheStorage
