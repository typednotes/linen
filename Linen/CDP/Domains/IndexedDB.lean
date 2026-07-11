/-
  Linen.CDP.Domains.IndexedDB — the `IndexedDB` CDP domain

  Ports `CDP.Domains.IndexedDB` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring. `DataEntry`
  reuses `CDP.Domains.Runtime.RemoteObject` for its `key`/`primaryKey`/`value`
  fields, exactly as upstream reuses `Runtime.RemoteObject`.

  `Key` is self-referential via `array : Option (List Key)` (an IndexedDB
  array key is itself a list of keys). `FromJSON`/`ToJSON` need a real
  termination proof for the same reason `CDP.Domains.Runtime.StackTrace`'s do
  (there via a singular `Option StackTrace` field rather than a
  `Option (List Key)` one); see `parseKey`/`encodeKey` below and
  `Data.Json.Value.lookup_sizeOf_lt`.

  Most commands take an optional `securityOrigin` *or* `storageKey` (upstream:
  "At least and at most one of securityOrigin, storageKey must be
  specified"); this constraint is documented but not encoded in the type,
  matching upstream (which also leaves it as a runtime precondition, not a
  type-level one).
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.IndexedDB

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)
open CDP.Domains.Runtime (RemoteObject)

-- ── Types ──

/-- Key path type. -/
inductive KeyPathType where
  | null | string | array
  deriving Repr, BEq, DecidableEq

instance : FromJSON KeyPathType where
  parseJSON
    | .string "null" => .ok .null
    | .string "string" => .ok .string
    | .string "array" => .ok .array
    | v => .error s!"failed to parse KeyPathType: {repr v}"

instance : ToJSON KeyPathType where
  toJSON | .null => .string "null" | .string => .string "string" | .array => .string "array"

/-- Key path. -/
structure KeyPath where
  /-- Key path type. -/
  type : KeyPathType
  /-- String value. -/
  string : Option String := none
  /-- Array value. -/
  array : Option (List String) := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON KeyPath where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        string := ← (← Value.getFieldOpt v "string").mapM FromJSON.parseJSON
        array := ← (← Value.getFieldOpt v "array").mapM FromJSON.parseJSON }

instance : ToJSON KeyPath where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type)]
    ++ (p.string.map fun v => ("string", ToJSON.toJSON v)).toList
    ++ (p.array.map fun v => ("array", ToJSON.toJSON v)).toList

/-- Object store index. -/
structure ObjectStoreIndex where
  /-- Index name. -/
  name : String
  /-- Index key path. -/
  keyPath : KeyPath
  /-- If `true`, index is unique. -/
  unique : Bool
  /-- If `true`, index allows multiple entries for a key. -/
  multiEntry : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON ObjectStoreIndex where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        keyPath := ← Value.getField v "keyPath" >>= FromJSON.parseJSON
        unique := ← Value.getField v "unique" >>= FromJSON.parseJSON
        multiEntry := ← Value.getField v "multiEntry" >>= FromJSON.parseJSON }

instance : ToJSON ObjectStoreIndex where
  toJSON p := Data.Json.object
    [ ("name", ToJSON.toJSON p.name), ("keyPath", ToJSON.toJSON p.keyPath)
    , ("unique", ToJSON.toJSON p.unique), ("multiEntry", ToJSON.toJSON p.multiEntry) ]

/-- Object store. -/
structure ObjectStore where
  /-- Object store name. -/
  name : String
  /-- Object store key path. -/
  keyPath : KeyPath
  /-- If `true`, object store has auto increment flag set. -/
  autoIncrement : Bool
  /-- Indexes in this object store. -/
  indexes : List ObjectStoreIndex
  deriving Repr, BEq, DecidableEq

instance : FromJSON ObjectStore where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        keyPath := ← Value.getField v "keyPath" >>= FromJSON.parseJSON
        autoIncrement := ← Value.getField v "autoIncrement" >>= FromJSON.parseJSON
        indexes := ← Value.getField v "indexes" >>= FromJSON.parseJSON }

instance : ToJSON ObjectStore where
  toJSON p := Data.Json.object
    [ ("name", ToJSON.toJSON p.name), ("keyPath", ToJSON.toJSON p.keyPath)
    , ("autoIncrement", ToJSON.toJSON p.autoIncrement), ("indexes", ToJSON.toJSON p.indexes) ]

/-- Database with an array of object stores. -/
structure DatabaseWithObjectStores where
  /-- Database name. -/
  name : String
  /-- Database version (type is not `integer`, as the standard requires the
      version number to be `unsigned long long`). -/
  version : Float
  /-- Object stores in this database. -/
  objectStores : List ObjectStore
  deriving Repr, BEq, DecidableEq

instance : FromJSON DatabaseWithObjectStores where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        version := ← Value.getField v "version" >>= FromJSON.parseJSON
        objectStores := ← Value.getField v "objectStores" >>= FromJSON.parseJSON }

instance : ToJSON DatabaseWithObjectStores where
  toJSON p := Data.Json.object
    [ ("name", ToJSON.toJSON p.name), ("version", ToJSON.toJSON p.version)
    , ("objectStores", ToJSON.toJSON p.objectStores) ]

/-- Key type. -/
inductive KeyType where
  | number | string | date | array
  deriving Repr, BEq, DecidableEq

instance : FromJSON KeyType where
  parseJSON
    | .string "number" => .ok .number
    | .string "string" => .ok .string
    | .string "date" => .ok .date
    | .string "array" => .ok .array
    | v => .error s!"failed to parse KeyType: {repr v}"

instance : ToJSON KeyType where
  toJSON | .number => .string "number" | .string => .string "string"
         | .date => .string "date" | .array => .string "array"

/-- Key. Self-referential via `array` — see the module docstring. -/
structure Key where
  /-- Key type. -/
  type : KeyType
  /-- Number value. -/
  number : Option Float := none
  /-- String value. -/
  string : Option String := none
  /-- Date value. -/
  date : Option Float := none
  /-- Array value. -/
  array : Option (List Key) := none
  deriving Repr, BEq

/-- Finish decoding a `Key` given its already-decoded recursive `array`
    field. Factored out so both branches of `parseKey` share it, mirroring
    `CDP.Domains.Runtime.finishStackTrace`. -/
private def finishKey (v : Value) (array : Option (List Key)) : Except String Key := do
  .ok
    { type := ← Value.getField v "type" >>= FromJSON.parseJSON
      number := ← (← Value.getFieldOpt v "number").mapM FromJSON.parseJSON
      string := ← (← Value.getFieldOpt v "string").mapM FromJSON.parseJSON
      date := ← (← Value.getFieldOpt v "date").mapM FromJSON.parseJSON
      array }

set_option linter.unusedVariables false in
mutual

/-- Decode a `Key`. A plain recursive `def` — rather than `array` going
    through the generic `FromJSON (List α)` instance — to sidestep the
    circular instance dependency a self-referential `instance : FromJSON Key`
    would otherwise have on itself. Terminates on `sizeOf`, via
    `Value.lookup_sizeOf_lt`. -/
def parseKey (v : Value) : Except String Key :=
  match h : v.lookup "array" with
  | none => finishKey v none
  | some .null => finishKey v none
  | some arrV =>
    match parseKeyList arrV with
    | .error e => .error e
    | .ok arr => finishKey v (some arr)
termination_by sizeOf v
decreasing_by exact Value.lookup_sizeOf_lt h

private def parseKeyList (v : Value) : Except String (List Key) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => parseKey p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega

end

instance : FromJSON Key where parseJSON := parseKey

private theorem Key.array_sizeOf_lt {k : Key} {arr : List Key} (h : k.array = some arr) :
    sizeOf arr < sizeOf k := by
  cases k with
  | mk type number string date array =>
    have h' : array = some arr := h
    subst h'
    simp only [Key.mk.sizeOf_spec, Option.some.sizeOf_spec]
    omega

set_option linter.unusedVariables false in
mutual

/-- Encode a `Key`. A plain recursive `def`, for the same reason `parseKey`
    is: sidesteps the circular instance dependency a self-referential
    `instance : ToJSON Key` would have on itself through the generic
    `ToJSON (List α)` instance. Terminates structurally on `Key.array`'s own
    `sizeOf`. -/
def encodeKey (k : Key) : Value :=
  match h : k.array with
  | none =>
    Data.Json.object <|
      [("type", ToJSON.toJSON k.type)]
      ++ (k.number.map fun v => ("number", ToJSON.toJSON v)).toList
      ++ (k.string.map fun v => ("string", ToJSON.toJSON v)).toList
      ++ (k.date.map fun v => ("date", ToJSON.toJSON v)).toList
  | some arr =>
    Data.Json.object <|
      [("type", ToJSON.toJSON k.type)]
      ++ (k.number.map fun v => ("number", ToJSON.toJSON v)).toList
      ++ (k.string.map fun v => ("string", ToJSON.toJSON v)).toList
      ++ (k.date.map fun v => ("date", ToJSON.toJSON v)).toList
      ++ [("array", encodeKeyList arr)]
termination_by sizeOf k
decreasing_by exact Key.array_sizeOf_lt h

private def encodeKeyList (l : List Key) : Value :=
  Value.array (l.map encodeKey).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega

end

instance : ToJSON Key where toJSON := encodeKey

/-- Key range. -/
structure KeyRange where
  /-- Lower bound. -/
  lower : Option Key := none
  /-- Upper bound. -/
  upper : Option Key := none
  /-- If `true` lower bound is open. -/
  lowerOpen : Bool
  /-- If `true` upper bound is open. -/
  upperOpen : Bool
  deriving Repr, BEq

instance : FromJSON KeyRange where
  parseJSON v := do
    .ok
      { lower := ← (← Value.getFieldOpt v "lower").mapM FromJSON.parseJSON
        upper := ← (← Value.getFieldOpt v "upper").mapM FromJSON.parseJSON
        lowerOpen := ← Value.getField v "lowerOpen" >>= FromJSON.parseJSON
        upperOpen := ← Value.getField v "upperOpen" >>= FromJSON.parseJSON }

instance : ToJSON KeyRange where
  toJSON p := Data.Json.object <|
    (p.lower.map fun v => ("lower", ToJSON.toJSON v)).toList
    ++ (p.upper.map fun v => ("upper", ToJSON.toJSON v)).toList
    ++ [("lowerOpen", ToJSON.toJSON p.lowerOpen), ("upperOpen", ToJSON.toJSON p.upperOpen)]

/-- Data entry. -/
structure DataEntry where
  /-- Key object. -/
  key : RemoteObject
  /-- Primary key object. -/
  primaryKey : RemoteObject
  /-- Value object. -/
  value : RemoteObject
  deriving Repr, BEq

instance : FromJSON DataEntry where
  parseJSON v := do
    .ok
      { key := ← Value.getField v "key" >>= FromJSON.parseJSON
        primaryKey := ← Value.getField v "primaryKey" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON DataEntry where
  toJSON p := Data.Json.object
    [ ("key", ToJSON.toJSON p.key), ("primaryKey", ToJSON.toJSON p.primaryKey)
    , ("value", ToJSON.toJSON p.value) ]

-- ── clearObjectStore ──

/-- Parameters of the `IndexedDB.clearObjectStore` command: clears all entries
    from an object store. -/
structure PClearObjectStore where
  /-- Database name. -/
  databaseName : String
  /-- Object store name. -/
  objectStoreName : String
  /-- At least and at most one of `securityOrigin`, `storageKey` must be
      specified. Security origin. -/
  securityOrigin : Option String := none
  /-- Storage key. -/
  storageKey : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClearObjectStore where
  toJSON p := Data.Json.object <|
    [("databaseName", ToJSON.toJSON p.databaseName), ("objectStoreName", ToJSON.toJSON p.objectStoreName)]
    ++ (p.securityOrigin.map fun v => ("securityOrigin", ToJSON.toJSON v)).toList
    ++ (p.storageKey.map fun v => ("storageKey", ToJSON.toJSON v)).toList

instance : Command PClearObjectStore where
  Response := Unit
  commandName _ := "IndexedDB.clearObjectStore"
  decodeResponse _ := .ok ()

-- ── deleteDatabase ──

/-- Parameters of the `IndexedDB.deleteDatabase` command: deletes a database. -/
structure PDeleteDatabase where
  /-- Database name. -/
  databaseName : String
  /-- At least and at most one of `securityOrigin`, `storageKey` must be
      specified. Security origin. -/
  securityOrigin : Option String := none
  /-- Storage key. -/
  storageKey : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDeleteDatabase where
  toJSON p := Data.Json.object <|
    [("databaseName", ToJSON.toJSON p.databaseName)]
    ++ (p.securityOrigin.map fun v => ("securityOrigin", ToJSON.toJSON v)).toList
    ++ (p.storageKey.map fun v => ("storageKey", ToJSON.toJSON v)).toList

instance : Command PDeleteDatabase where
  Response := Unit
  commandName _ := "IndexedDB.deleteDatabase"
  decodeResponse _ := .ok ()

-- ── deleteObjectStoreEntries ──

/-- Parameters of the `IndexedDB.deleteObjectStoreEntries` command: deletes a
    range of entries from an object store. -/
structure PDeleteObjectStoreEntries where
  databaseName : String
  objectStoreName : String
  /-- Range of entry keys to delete. -/
  keyRange : KeyRange
  /-- At least and at most one of `securityOrigin`, `storageKey` must be
      specified. Security origin. -/
  securityOrigin : Option String := none
  /-- Storage key. -/
  storageKey : Option String := none
  deriving Repr, BEq

instance : ToJSON PDeleteObjectStoreEntries where
  toJSON p := Data.Json.object <|
    [ ("databaseName", ToJSON.toJSON p.databaseName), ("objectStoreName", ToJSON.toJSON p.objectStoreName)
    , ("keyRange", ToJSON.toJSON p.keyRange) ]
    ++ (p.securityOrigin.map fun v => ("securityOrigin", ToJSON.toJSON v)).toList
    ++ (p.storageKey.map fun v => ("storageKey", ToJSON.toJSON v)).toList

instance : Command PDeleteObjectStoreEntries where
  Response := Unit
  commandName _ := "IndexedDB.deleteObjectStoreEntries"
  decodeResponse _ := .ok ()

-- ── disable ──

/-- Parameters of the `IndexedDB.disable` command: disables events from
    backend. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "IndexedDB.disable"
  decodeResponse _ := .ok ()

-- ── enable ──

/-- Parameters of the `IndexedDB.enable` command: enables events from
    backend. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "IndexedDB.enable"
  decodeResponse _ := .ok ()

-- ── requestData ──

/-- Parameters of the `IndexedDB.requestData` command: requests data from
    object store or index. -/
structure PRequestData where
  /-- Database name. -/
  databaseName : String
  /-- Object store name. -/
  objectStoreName : String
  /-- Index name, empty string for object store data requests. -/
  indexName : String
  /-- Number of records to skip. -/
  skipCount : Int
  /-- Number of records to fetch. -/
  pageSize : Int
  /-- At least and at most one of `securityOrigin`, `storageKey` must be
      specified. Security origin. -/
  securityOrigin : Option String := none
  /-- Storage key. -/
  storageKey : Option String := none
  /-- Key range. -/
  keyRange : Option KeyRange := none
  deriving Repr, BEq

instance : ToJSON PRequestData where
  toJSON p := Data.Json.object <|
    [ ("databaseName", ToJSON.toJSON p.databaseName), ("objectStoreName", ToJSON.toJSON p.objectStoreName)
    , ("indexName", ToJSON.toJSON p.indexName), ("skipCount", ToJSON.toJSON p.skipCount)
    , ("pageSize", ToJSON.toJSON p.pageSize) ]
    ++ (p.securityOrigin.map fun v => ("securityOrigin", ToJSON.toJSON v)).toList
    ++ (p.storageKey.map fun v => ("storageKey", ToJSON.toJSON v)).toList
    ++ (p.keyRange.map fun v => ("keyRange", ToJSON.toJSON v)).toList

/-- Response of the `IndexedDB.requestData` command. -/
structure RequestData where
  /-- Array of object store data entries. -/
  objectStoreDataEntries : List DataEntry
  /-- If `true`, there are more entries to fetch in the given range. -/
  hasMore : Bool
  deriving Repr, BEq

instance : FromJSON RequestData where
  parseJSON v := do
    .ok
      { objectStoreDataEntries := ← Value.getField v "objectStoreDataEntries" >>= FromJSON.parseJSON
        hasMore := ← Value.getField v "hasMore" >>= FromJSON.parseJSON }

instance : Command PRequestData where
  Response := RequestData
  commandName _ := "IndexedDB.requestData"
  decodeResponse := FromJSON.parseJSON

-- ── getMetadata ──

/-- Parameters of the `IndexedDB.getMetadata` command: gets metadata of an
    object store. -/
structure PGetMetadata where
  /-- Database name. -/
  databaseName : String
  /-- Object store name. -/
  objectStoreName : String
  /-- At least and at most one of `securityOrigin`, `storageKey` must be
      specified. Security origin. -/
  securityOrigin : Option String := none
  /-- Storage key. -/
  storageKey : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetMetadata where
  toJSON p := Data.Json.object <|
    [("databaseName", ToJSON.toJSON p.databaseName), ("objectStoreName", ToJSON.toJSON p.objectStoreName)]
    ++ (p.securityOrigin.map fun v => ("securityOrigin", ToJSON.toJSON v)).toList
    ++ (p.storageKey.map fun v => ("storageKey", ToJSON.toJSON v)).toList

/-- Response of the `IndexedDB.getMetadata` command. -/
structure GetMetadata where
  /-- The entries count. -/
  entriesCount : Float
  /-- The current value of key generator, to become the next inserted key
      into the object store. Valid if `objectStore.autoIncrement` is `true`. -/
  keyGeneratorValue : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetMetadata where
  parseJSON v := do
    .ok
      { entriesCount := ← Value.getField v "entriesCount" >>= FromJSON.parseJSON
        keyGeneratorValue := ← Value.getField v "keyGeneratorValue" >>= FromJSON.parseJSON }

instance : Command PGetMetadata where
  Response := GetMetadata
  commandName _ := "IndexedDB.getMetadata"
  decodeResponse := FromJSON.parseJSON

-- ── requestDatabase ──

/-- Parameters of the `IndexedDB.requestDatabase` command: requests database
    with given name in given frame. -/
structure PRequestDatabase where
  /-- Database name. -/
  databaseName : String
  /-- At least and at most one of `securityOrigin`, `storageKey` must be
      specified. Security origin. -/
  securityOrigin : Option String := none
  /-- Storage key. -/
  storageKey : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRequestDatabase where
  toJSON p := Data.Json.object <|
    [("databaseName", ToJSON.toJSON p.databaseName)]
    ++ (p.securityOrigin.map fun v => ("securityOrigin", ToJSON.toJSON v)).toList
    ++ (p.storageKey.map fun v => ("storageKey", ToJSON.toJSON v)).toList

/-- Response of the `IndexedDB.requestDatabase` command. -/
structure RequestDatabase where
  /-- Database with an array of object stores. -/
  databaseWithObjectStores : DatabaseWithObjectStores
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestDatabase where
  parseJSON v := do
    .ok { databaseWithObjectStores := ← Value.getField v "databaseWithObjectStores" >>= FromJSON.parseJSON }

instance : Command PRequestDatabase where
  Response := RequestDatabase
  commandName _ := "IndexedDB.requestDatabase"
  decodeResponse := FromJSON.parseJSON

-- ── requestDatabaseNames ──

/-- Parameters of the `IndexedDB.requestDatabaseNames` command: requests
    database names for given security origin. -/
structure PRequestDatabaseNames where
  /-- At least and at most one of `securityOrigin`, `storageKey` must be
      specified. Security origin. -/
  securityOrigin : Option String := none
  /-- Storage key. -/
  storageKey : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRequestDatabaseNames where
  toJSON p := Data.Json.object <|
    (p.securityOrigin.map fun v => ("securityOrigin", ToJSON.toJSON v)).toList
    ++ (p.storageKey.map fun v => ("storageKey", ToJSON.toJSON v)).toList

/-- Response of the `IndexedDB.requestDatabaseNames` command. -/
structure RequestDatabaseNames where
  /-- Database names for origin. -/
  databaseNames : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON RequestDatabaseNames where
  parseJSON v := do .ok { databaseNames := ← Value.getField v "databaseNames" >>= FromJSON.parseJSON }

instance : Command PRequestDatabaseNames where
  Response := RequestDatabaseNames
  commandName _ := "IndexedDB.requestDatabaseNames"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.IndexedDB
