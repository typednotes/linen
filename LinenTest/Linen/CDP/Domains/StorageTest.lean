/-
  Tests for `Linen.CDP.Domains.Storage`.
-/
import Linen.CDP.Domains.Storage

open CDP.Domains.Storage
open CDP.Domains
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Storage

/-! ### StorageType -/

#guard decodeAs "\"file_systems\"" (α := StorageType) = .ok .fileSystems
#guard decodeAs "\"cache_storage\"" (α := StorageType) = .ok .cacheStorage
#guard encode (ToJSON.toJSON StorageType.interestGroups) = "\"interest_groups\""
#guard encode (ToJSON.toJSON StorageType.all) = "\"all\""

/-! ### UsageForType / TrustTokens -/

#guard decodeAs "{\"storageType\": \"cookies\", \"usage\": 42}" (α := UsageForType)
  = .ok { storageType := .cookies, usage := 42 }
#guard encode (ToJSON.toJSON ({ storageType := .cookies, usage := 42 } : UsageForType))
  = "{\"storageType\":\"cookies\",\"usage\":42}"

#guard decodeAs "{\"issuerOrigin\": \"https://issuer.example\", \"count\": 3}" (α := TrustTokens)
  = .ok { issuerOrigin := "https://issuer.example", count := 3 }

/-! ### InterestGroupAccessType / InterestGroupAd / InterestGroupDetails -/

#guard decodeAs "\"bid\"" (α := InterestGroupAccessType) = .ok .bid
#guard encode (ToJSON.toJSON InterestGroupAccessType.win) = "\"win\""

#guard decodeAs "{\"renderUrl\": \"https://ad.example\"}" (α := InterestGroupAd)
  = .ok { renderUrl := "https://ad.example", metadata := none }
#guard decodeAs "{\"renderUrl\": \"https://ad.example\", \"metadata\": \"m\"}" (α := InterestGroupAd)
  = .ok { renderUrl := "https://ad.example", metadata := some "m" }

#guard decodeAs
    ("{\"ownerOrigin\": \"https://owner.example\", \"name\": \"n\", " ++
     "\"expirationTime\": 1.0, \"joiningOrigin\": \"https://joiner.example\", " ++
     "\"trustedBiddingSignalsKeys\": [], \"ads\": [], \"adComponents\": []}")
    (α := InterestGroupDetails)
  = .ok
    { ownerOrigin := "https://owner.example"
      name := "n"
      expirationTime := 1.0
      joiningOrigin := "https://joiner.example"
      trustedBiddingSignalsKeys := []
      ads := []
      adComponents := [] }

/-! ### Events -/

#guard Event.eventName (α := CacheStorageContentUpdated) = "Storage.cacheStorageContentUpdated"
#guard Event.eventName (α := CacheStorageListUpdated) = "Storage.cacheStorageListUpdated"
#guard Event.eventName (α := IndexedDBContentUpdated) = "Storage.indexedDBContentUpdated"
#guard Event.eventName (α := IndexedDBListUpdated) = "Storage.indexedDBListUpdated"
#guard Event.eventName (α := InterestGroupAccessed) = "Storage.interestGroupAccessed"

#guard decodeAs "{\"origin\": \"o\", \"cacheName\": \"c\"}" (α := CacheStorageContentUpdated)
  = .ok { origin := "o", cacheName := "c" }
#guard decodeAs "{\"origin\": \"o\"}" (α := CacheStorageListUpdated) = .ok { origin := "o" }
#guard decodeAs
    "{\"origin\": \"o\", \"storageKey\": \"k\", \"databaseName\": \"d\", \"objectStoreName\": \"s\"}"
    (α := IndexedDBContentUpdated)
  = .ok { origin := "o", storageKey := "k", databaseName := "d", objectStoreName := "s" }
#guard decodeAs "{\"origin\": \"o\", \"storageKey\": \"k\"}" (α := IndexedDBListUpdated)
  = .ok { origin := "o", storageKey := "k" }
#guard decodeAs
    "{\"accessTime\": 1.0, \"type\": \"join\", \"ownerOrigin\": \"o\", \"name\": \"n\"}"
    (α := InterestGroupAccessed)
  = .ok { accessTime := 1.0, type := .join, ownerOrigin := "o", name := "n" }

/-! ### Commands -/

#guard encode (ToJSON.toJSON ({ frameId := "f" } : PGetStorageKeyForFrame))
  = "{\"frameId\":\"f\"}"
#guard Command.commandName ({ frameId := "f" } : PGetStorageKeyForFrame)
  = "Storage.getStorageKeyForFrame"
#guard decodeAs "{\"storageKey\": \"sk\"}" (α := GetStorageKeyForFrame) = .ok { storageKey := "sk" }

#guard Command.commandName ({ origin := "o", storageTypes := "cookies" } : PClearDataForOrigin)
  = "Storage.clearDataForOrigin"
#guard encode (ToJSON.toJSON ({ origin := "o", storageTypes := "cookies" } : PClearDataForOrigin))
  = "{\"origin\":\"o\",\"storageTypes\":\"cookies\"}"

#guard Command.commandName
    ({ storageKey := "sk", storageTypes := "cookies" } : PClearDataForStorageKey)
  = "Storage.clearDataForStorageKey"

#guard encode (ToJSON.toJSON ({} : PGetCookies)) = "{}"
#guard Command.commandName ({} : PGetCookies) = "Storage.getCookies"
#guard decodeAs "{\"cookies\": []}" (α := GetCookies) = .ok { cookies := [] }

#guard Command.commandName ({ cookies := [] } : PSetCookies) = "Storage.setCookies"
#guard encode (ToJSON.toJSON ({ cookies := [] } : PSetCookies)) = "{\"cookies\":[]}"

#guard encode (ToJSON.toJSON ({} : PClearCookies)) = "{}"
#guard Command.commandName ({} : PClearCookies) = "Storage.clearCookies"

#guard Command.commandName ({ origin := "o" } : PGetUsageAndQuota) = "Storage.getUsageAndQuota"
#guard decodeAs
    "{\"usage\": 1, \"quota\": 2, \"overrideActive\": true, \"usageBreakdown\": []}"
    (α := GetUsageAndQuota)
  = .ok { usage := 1, quota := 2, overrideActive := true, usageBreakdown := [] }

#guard Command.commandName ({ origin := "o" } : POverrideQuotaForOrigin)
  = "Storage.overrideQuotaForOrigin"
#guard encode (ToJSON.toJSON ({ origin := "o" } : POverrideQuotaForOrigin)) = "{\"origin\":\"o\"}"
#guard encode (ToJSON.toJSON ({ origin := "o", quotaSize := some 10 } : POverrideQuotaForOrigin))
  = "{\"origin\":\"o\",\"quotaSize\":10}"

#guard Command.commandName ({ origin := "o" } : PTrackCacheStorageForOrigin)
  = "Storage.trackCacheStorageForOrigin"
#guard Command.commandName ({ origin := "o" } : PTrackIndexedDBForOrigin)
  = "Storage.trackIndexedDBForOrigin"
#guard Command.commandName ({ storageKey := "sk" } : PTrackIndexedDBForStorageKey)
  = "Storage.trackIndexedDBForStorageKey"
#guard Command.commandName ({ origin := "o" } : PUntrackCacheStorageForOrigin)
  = "Storage.untrackCacheStorageForOrigin"
#guard Command.commandName ({ origin := "o" } : PUntrackIndexedDBForOrigin)
  = "Storage.untrackIndexedDBForOrigin"
#guard Command.commandName ({ storageKey := "sk" } : PUntrackIndexedDBForStorageKey)
  = "Storage.untrackIndexedDBForStorageKey"

#guard encode (ToJSON.toJSON ({} : PGetTrustTokens)) = "null"
#guard Command.commandName ({} : PGetTrustTokens) = "Storage.getTrustTokens"
#guard decodeAs "{\"tokens\": []}" (α := GetTrustTokens) = .ok { tokens := [] }

#guard Command.commandName ({ issuerOrigin := "i" } : PClearTrustTokens)
  = "Storage.clearTrustTokens"
#guard decodeAs "{\"didDeleteTokens\": true}" (α := ClearTrustTokens)
  = .ok { didDeleteTokens := true }

#guard Command.commandName
    ({ ownerOrigin := "o", name := "n" } : PGetInterestGroupDetails)
  = "Storage.getInterestGroupDetails"
#guard decodeAs
    ("{\"details\": {\"ownerOrigin\": \"o\", \"name\": \"n\", \"expirationTime\": 1.0, " ++
     "\"joiningOrigin\": \"j\", \"trustedBiddingSignalsKeys\": [], \"ads\": [], " ++
     "\"adComponents\": []}}")
    (α := GetInterestGroupDetails)
  = .ok
    { details :=
        { ownerOrigin := "o"
          name := "n"
          expirationTime := 1.0
          joiningOrigin := "j"
          trustedBiddingSignalsKeys := []
          ads := []
          adComponents := [] } }

#guard Command.commandName ({ enable := true } : PSetInterestGroupTracking)
  = "Storage.setInterestGroupTracking"
#guard encode (ToJSON.toJSON ({ enable := true } : PSetInterestGroupTracking))
  = "{\"enable\":true}"

end Tests.CDP.Domains.Storage
