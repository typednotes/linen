/-
  Linen.CDP.Domains.DOMStorage — the `DOMStorage` CDP domain

  Query and modify DOM storage. Ports `CDP.Domains.DOMStorage` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.DOMStorage

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

abbrev SerializedStorageKey := String

/-- DOM Storage identifier. -/
structure StorageId where
  /-- Security origin for the storage. -/
  securityOrigin : Option String := none
  /-- Represents a key by which DOM Storage keys its `CachedStorageArea`s. -/
  storageKey : Option SerializedStorageKey := none
  /-- Whether the storage is local storage (not session storage). -/
  isLocalStorage : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON StorageId where
  parseJSON v := do
    .ok
      { securityOrigin := ← (← Value.getFieldOpt v "securityOrigin").mapM FromJSON.parseJSON
        storageKey := ← (← Value.getFieldOpt v "storageKey").mapM FromJSON.parseJSON
        isLocalStorage := ← Value.getField v "isLocalStorage" >>= FromJSON.parseJSON }

instance : ToJSON StorageId where
  toJSON p := Data.Json.object <|
    (p.securityOrigin.map fun v => ("securityOrigin", ToJSON.toJSON v)).toList
    ++ (p.storageKey.map fun v => ("storageKey", ToJSON.toJSON v)).toList
    ++ [("isLocalStorage", ToJSON.toJSON p.isLocalStorage)]

/-- A DOM Storage `(key, value)` item. -/
abbrev Item := List String

/-- The `DOMStorage.domStorageItemAdded` event. -/
structure DomStorageItemAdded where
  storageId : StorageId
  key : String
  newValue : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON DomStorageItemAdded where
  parseJSON v := do
    .ok
      { storageId := ← Value.getField v "storageId" >>= FromJSON.parseJSON
        key := ← Value.getField v "key" >>= FromJSON.parseJSON
        newValue := ← Value.getField v "newValue" >>= FromJSON.parseJSON }

instance : Event DomStorageItemAdded where
  eventName := "DOMStorage.domStorageItemAdded"

/-- The `DOMStorage.domStorageItemRemoved` event. -/
structure DomStorageItemRemoved where
  storageId : StorageId
  key : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON DomStorageItemRemoved where
  parseJSON v := do
    .ok
      { storageId := ← Value.getField v "storageId" >>= FromJSON.parseJSON
        key := ← Value.getField v "key" >>= FromJSON.parseJSON }

instance : Event DomStorageItemRemoved where
  eventName := "DOMStorage.domStorageItemRemoved"

/-- The `DOMStorage.domStorageItemUpdated` event. -/
structure DomStorageItemUpdated where
  storageId : StorageId
  key : String
  oldValue : String
  newValue : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON DomStorageItemUpdated where
  parseJSON v := do
    .ok
      { storageId := ← Value.getField v "storageId" >>= FromJSON.parseJSON
        key := ← Value.getField v "key" >>= FromJSON.parseJSON
        oldValue := ← Value.getField v "oldValue" >>= FromJSON.parseJSON
        newValue := ← Value.getField v "newValue" >>= FromJSON.parseJSON }

instance : Event DomStorageItemUpdated where
  eventName := "DOMStorage.domStorageItemUpdated"

/-- The `DOMStorage.domStorageItemsCleared` event. -/
structure DomStorageItemsCleared where
  storageId : StorageId
  deriving Repr, BEq, DecidableEq

instance : FromJSON DomStorageItemsCleared where
  parseJSON v := do .ok { storageId := ← Value.getField v "storageId" >>= FromJSON.parseJSON }

instance : Event DomStorageItemsCleared where
  eventName := "DOMStorage.domStorageItemsCleared"

/-- Parameters of the `DOMStorage.clear` command. -/
structure PClear where
  storageId : StorageId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClear where
  toJSON p := Data.Json.object [("storageId", ToJSON.toJSON p.storageId)]

instance : Command PClear where
  Response := Unit
  commandName _ := "DOMStorage.clear"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMStorage.disable` command: disables storage tracking,
    prevents storage events from being sent to the client. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where
  toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "DOMStorage.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMStorage.enable` command: enables storage tracking,
    storage events will now be delivered to the client. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where
  toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "DOMStorage.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMStorage.getDOMStorageItems` command. -/
structure PGetDOMStorageItems where
  storageId : StorageId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetDOMStorageItems where
  toJSON p := Data.Json.object [("storageId", ToJSON.toJSON p.storageId)]

/-- Response of the `DOMStorage.getDOMStorageItems` command. -/
structure GetDOMStorageItems where
  entries : List Item
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetDOMStorageItems where
  parseJSON v := do .ok { entries := ← Value.getField v "entries" >>= FromJSON.parseJSON }

instance : Command PGetDOMStorageItems where
  Response := GetDOMStorageItems
  commandName _ := "DOMStorage.getDOMStorageItems"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `DOMStorage.removeDOMStorageItem` command. -/
structure PRemoveDOMStorageItem where
  storageId : StorageId
  key : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRemoveDOMStorageItem where
  toJSON p := Data.Json.object [("storageId", ToJSON.toJSON p.storageId), ("key", ToJSON.toJSON p.key)]

instance : Command PRemoveDOMStorageItem where
  Response := Unit
  commandName _ := "DOMStorage.removeDOMStorageItem"
  decodeResponse _ := .ok ()

/-- Parameters of the `DOMStorage.setDOMStorageItem` command. -/
structure PSetDOMStorageItem where
  storageId : StorageId
  key : String
  value : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetDOMStorageItem where
  toJSON p := Data.Json.object
    [("storageId", ToJSON.toJSON p.storageId), ("key", ToJSON.toJSON p.key), ("value", ToJSON.toJSON p.value)]

instance : Command PSetDOMStorageItem where
  Response := Unit
  commandName _ := "DOMStorage.setDOMStorageItem"
  decodeResponse _ := .ok ()

end CDP.Domains.DOMStorage
