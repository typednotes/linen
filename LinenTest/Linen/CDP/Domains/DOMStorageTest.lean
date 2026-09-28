/-
  Tests for `Linen.CDP.Domains.DOMStorage`.
-/
import Linen.CDP.Domains.DOMStorage

open CDP.Domains.DOMStorage
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.DOMStorage

/-! ### StorageId -/

#guard decodeAs "{\"isLocalStorage\": true}" (α := StorageId)
  = .ok { securityOrigin := none, storageKey := none, isLocalStorage := true }
#guard decodeAs "{\"securityOrigin\": \"o\", \"isLocalStorage\": false}" (α := StorageId)
  = .ok { securityOrigin := some "o", storageKey := none, isLocalStorage := false }
#guard encode (ToJSON.toJSON ({ isLocalStorage := true } : StorageId)) = "{\"isLocalStorage\":true}"

/-! ### Events -/

#guard Event.eventName (α := DomStorageItemAdded) = "DOMStorage.domStorageItemAdded"
#guard Event.eventName (α := DomStorageItemRemoved) = "DOMStorage.domStorageItemRemoved"
#guard Event.eventName (α := DomStorageItemUpdated) = "DOMStorage.domStorageItemUpdated"
#guard Event.eventName (α := DomStorageItemsCleared) = "DOMStorage.domStorageItemsCleared"

#guard decodeAs
    "{\"storageId\": {\"isLocalStorage\": true}, \"key\": \"k\", \"newValue\": \"v\"}"
    (α := DomStorageItemAdded)
  = .ok { storageId := { isLocalStorage := true }, key := "k", newValue := "v" }

/-! ### Commands -/

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PDisable) = "DOMStorage.disable"
#guard Command.commandName ({} : PEnable) = "DOMStorage.enable"
#guard Command.commandName ({ storageId := { isLocalStorage := true } } : PClear) = "DOMStorage.clear"
#guard Command.commandName
    ({ storageId := { isLocalStorage := true }, key := "k" } : PRemoveDOMStorageItem)
  = "DOMStorage.removeDOMStorageItem"
#guard Command.commandName
    ({ storageId := { isLocalStorage := true }, key := "k", value := "v" } : PSetDOMStorageItem)
  = "DOMStorage.setDOMStorageItem"
#guard encode (ToJSON.toJSON ({ storageId := { isLocalStorage := true }, key := "k", value := "v" } :
    PSetDOMStorageItem))
  = "{\"storageId\":{\"isLocalStorage\":true},\"key\":\"k\",\"value\":\"v\"}"

#guard decodeAs "{\"entries\": [[\"k\", \"v\"]]}" (α := GetDOMStorageItems)
  = .ok { entries := [["k", "v"]] }

end Tests.CDP.Domains.DOMStorage
