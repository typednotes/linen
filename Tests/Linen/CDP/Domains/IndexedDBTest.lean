/-
  Tests for `Linen.CDP.Domains.IndexedDB`.
-/
import Linen.CDP.Domains.IndexedDB

open CDP.Domains.IndexedDB
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.IndexedDB

-- ── Types ──

#guard decodeAs "\"array\"" (α := KeyPathType) = .ok .array
#guard encode (ToJSON.toJSON KeyPathType.null) = "\"null\""

#guard decodeAs "{\"type\": \"string\", \"string\": \"a.b\"}" (α := KeyPath)
  = .ok { type := .string, string := some "a.b" }
#guard encode (ToJSON.toJSON ({ type := .array, array := some ["a", "b"] } : KeyPath))
  = "{\"type\":\"array\",\"array\":[\"a\",\"b\"]}"

#guard decodeAs "{\"name\": \"idx\", \"keyPath\": {\"type\": \"string\"}, \"unique\": true, \"multiEntry\": false}"
    (α := ObjectStoreIndex)
  = .ok { name := "idx", keyPath := { type := .string }, unique := true, multiEntry := false }
#guard encode
    (ToJSON.toJSON ({ name := "idx", keyPath := { type := .string }, unique := true, multiEntry := false }
      : ObjectStoreIndex))
  = "{\"name\":\"idx\",\"keyPath\":{\"type\":\"string\"},\"unique\":true,\"multiEntry\":false}"

#guard decodeAs
    "{\"name\": \"store\", \"keyPath\": {\"type\": \"null\"}, \"autoIncrement\": true, \"indexes\": []}"
    (α := ObjectStore)
  = .ok { name := "store", keyPath := { type := .null }, autoIncrement := true, indexes := [] }

#guard decodeAs
    "{\"name\": \"db\", \"version\": 1, \"objectStores\": []}" (α := DatabaseWithObjectStores)
  = .ok { name := "db", version := 1, objectStores := [] }

#guard decodeAs "\"date\"" (α := KeyType) = .ok .date
#guard encode (ToJSON.toJSON KeyType.number) = "\"number\""

-- A plain, non-array key. `Key` has no `DecidableEq` (only `BEq`, since the
-- auto-deriving handler can't discharge it for a self-referential structure —
-- see the module docstring), so compare with `==` on the decoded value,
-- mirroring `CDP.Domains.Media`'s `PlayerError` tests.
#guard match decodeAs "{\"type\": \"number\", \"number\": 42}" (α := Key) with
  | .ok k => k == ({ type := .number, number := some 42 } : Key)
  | .error _ => false
#guard encode (ToJSON.toJSON ({ type := .string, string := some "k" } : Key))
  = "{\"type\":\"string\",\"string\":\"k\"}"

-- A self-referential array key, exercising `parseKey`/`encodeKey`'s recursion.
#guard match decodeAs
    "{\"type\": \"array\", \"array\": [{\"type\": \"number\", \"number\": 1}, {\"type\": \"string\", \"string\": \"x\"}]}"
    (α := Key) with
  | .ok k =>
    k == ({ type := .array
            array := some [{ type := .number, number := some 1 }, { type := .string, string := some "x" }] }
          : Key)
  | .error _ => false
#guard encode
    (ToJSON.toJSON
      ({ type := .array, array := some [{ type := .number, number := some 1 }] } : Key))
  = "{\"type\":\"array\",\"array\":[{\"type\":\"number\",\"number\":1}]}"

-- `KeyRange` has no `DecidableEq` either (it embeds `Option Key`), same reason.
#guard match decodeAs "{\"lowerOpen\": true, \"upperOpen\": false}" (α := KeyRange) with
  | .ok r => r == ({ lowerOpen := true, upperOpen := false } : KeyRange)
  | .error _ => false
#guard encode
    (ToJSON.toJSON
      ({ lower := some { type := .number, number := some 0 }, lowerOpen := false, upperOpen := true }
        : KeyRange))
  = "{\"lower\":{\"type\":\"number\",\"number\":0},\"lowerOpen\":false,\"upperOpen\":true}"

#guard decodeAs
    "{\"key\": {\"type\": \"string\", \"value\": \"k\"}, \"primaryKey\": {\"type\": \"number\", \"value\": 1}, \"value\": {\"type\": \"object\"}}"
    (α := DataEntry)
  |>.isOk

-- ── clearObjectStore ──

#guard Command.commandName ({ databaseName := "db", objectStoreName := "os" } : PClearObjectStore)
  = "IndexedDB.clearObjectStore"
#guard encode (ToJSON.toJSON ({ databaseName := "db", objectStoreName := "os" } : PClearObjectStore))
  = "{\"databaseName\":\"db\",\"objectStoreName\":\"os\"}"
#guard encode
    (ToJSON.toJSON
      ({ databaseName := "db", objectStoreName := "os", securityOrigin := some "http://x" }
        : PClearObjectStore))
  = "{\"databaseName\":\"db\",\"objectStoreName\":\"os\",\"securityOrigin\":\"http:\\/\\/x\"}"

-- ── deleteDatabase ──

#guard Command.commandName ({ databaseName := "db" } : PDeleteDatabase) = "IndexedDB.deleteDatabase"
#guard encode (ToJSON.toJSON ({ databaseName := "db" } : PDeleteDatabase)) = "{\"databaseName\":\"db\"}"

-- ── deleteObjectStoreEntries ──

#guard Command.commandName
    ({ databaseName := "db", objectStoreName := "os", keyRange := { lowerOpen := false, upperOpen := false } }
      : PDeleteObjectStoreEntries)
  = "IndexedDB.deleteObjectStoreEntries"
#guard encode
    (ToJSON.toJSON
      ({ databaseName := "db", objectStoreName := "os"
        , keyRange := { lowerOpen := false, upperOpen := false } } : PDeleteObjectStoreEntries))
  = "{\"databaseName\":\"db\",\"objectStoreName\":\"os\",\"keyRange\":{\"lowerOpen\":false,\"upperOpen\":false}}"

-- ── disable / enable ──

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PDisable) = "IndexedDB.disable"
#guard Command.commandName ({} : PEnable) = "IndexedDB.enable"

-- ── requestData ──

#guard Command.commandName
    ({ databaseName := "db", objectStoreName := "os", indexName := "", skipCount := 0, pageSize := 10 }
      : PRequestData)
  = "IndexedDB.requestData"
#guard encode
    (ToJSON.toJSON
      ({ databaseName := "db", objectStoreName := "os", indexName := "", skipCount := 0, pageSize := 10 }
        : PRequestData))
  = "{\"databaseName\":\"db\",\"objectStoreName\":\"os\",\"indexName\":\"\",\"skipCount\":0,\"pageSize\":10}"
-- `RequestData` embeds `List DataEntry`, and `DataEntry` embeds
-- `Runtime.RemoteObject` (itself lacking `DecidableEq`), so it too has no
-- `DecidableEq`; the empty-entries case is representable with `==`.
#guard match decodeAs "{\"objectStoreDataEntries\": [], \"hasMore\": false}" (α := RequestData) with
  | .ok r => r == ({ objectStoreDataEntries := [], hasMore := false } : RequestData)
  | .error _ => false

-- ── getMetadata ──

#guard Command.commandName ({ databaseName := "db", objectStoreName := "os" } : PGetMetadata)
  = "IndexedDB.getMetadata"
#guard encode (ToJSON.toJSON ({ databaseName := "db", objectStoreName := "os" } : PGetMetadata))
  = "{\"databaseName\":\"db\",\"objectStoreName\":\"os\"}"
#guard decodeAs "{\"entriesCount\": 3, \"keyGeneratorValue\": 4}" (α := GetMetadata)
  = .ok { entriesCount := 3, keyGeneratorValue := 4 }

-- ── requestDatabase ──

#guard Command.commandName ({ databaseName := "db" } : PRequestDatabase) = "IndexedDB.requestDatabase"
#guard encode (ToJSON.toJSON ({ databaseName := "db" } : PRequestDatabase)) = "{\"databaseName\":\"db\"}"
#guard decodeAs "{\"databaseWithObjectStores\": {\"name\": \"db\", \"version\": 1, \"objectStores\": []}}"
    (α := RequestDatabase)
  = .ok { databaseWithObjectStores := { name := "db", version := 1, objectStores := [] } }

-- ── requestDatabaseNames ──

#guard Command.commandName ({} : PRequestDatabaseNames) = "IndexedDB.requestDatabaseNames"
#guard encode (ToJSON.toJSON ({} : PRequestDatabaseNames)) = "{}"
#guard encode (ToJSON.toJSON ({ securityOrigin := some "http://x" } : PRequestDatabaseNames))
  = "{\"securityOrigin\":\"http:\\/\\/x\"}"
#guard decodeAs "{\"databaseNames\": [\"a\", \"b\"]}" (α := RequestDatabaseNames)
  = .ok { databaseNames := ["a", "b"] }

end Tests.CDP.Domains.IndexedDB
