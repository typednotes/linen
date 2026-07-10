/-
  Linen.CDP.Domains.Database — the `Database` CDP domain

  Ports `CDP.Domains.Database` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Database

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

abbrev DatabaseId := String

/-- A database object. -/
structure DB where
  id : DatabaseId
  domain : String
  name : String
  version : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON DB where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        domain := ← Value.getField v "domain" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        version := ← Value.getField v "version" >>= FromJSON.parseJSON }

instance : ToJSON DB where
  toJSON d := Data.Json.object
    [ ("id", ToJSON.toJSON d.id), ("domain", ToJSON.toJSON d.domain)
    , ("name", ToJSON.toJSON d.name), ("version", ToJSON.toJSON d.version) ]

/-- A database error. -/
structure DBError where
  message : String
  code : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON DBError where
  parseJSON v := do
    .ok
      { message := ← Value.getField v "message" >>= FromJSON.parseJSON
        code := ← Value.getField v "code" >>= FromJSON.parseJSON }

instance : ToJSON DBError where
  toJSON e := Data.Json.object [("message", ToJSON.toJSON e.message), ("code", ToJSON.toJSON e.code)]

/-- The `Database.addDatabase` event. -/
structure AddDatabase where
  database : DB
  deriving Repr, BEq, DecidableEq

instance : FromJSON AddDatabase where
  parseJSON v := do .ok { database := ← Value.getField v "database" >>= FromJSON.parseJSON }

instance : Event AddDatabase where
  eventName := "Database.addDatabase"

/-- Parameters of the `Database.disable` command: disables database tracking,
    prevents database events from being sent to the client. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Database.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Database.enable` command: enables database tracking,
    database events will now be delivered to the client. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Database.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Database.executeSQL` command. -/
structure PExecuteSQL where
  databaseId : DatabaseId
  query : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PExecuteSQL where
  toJSON p := Data.Json.object [("databaseId", ToJSON.toJSON p.databaseId), ("query", ToJSON.toJSON p.query)]

/-- Response of the `Database.executeSQL` command. -/
structure ExecuteSQL where
  columnNames : Option (List String) := none
  values : Option (List Value) := none
  sqlError : Option DBError := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ExecuteSQL where
  parseJSON v := do
    .ok
      { columnNames := ← (← Value.getFieldOpt v "columnNames").mapM FromJSON.parseJSON
        values := ← (← Value.getFieldOpt v "values").mapM FromJSON.parseJSON
        sqlError := ← (← Value.getFieldOpt v "sqlError").mapM FromJSON.parseJSON }

instance : Command PExecuteSQL where
  Response := ExecuteSQL
  commandName _ := "Database.executeSQL"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Database.getDatabaseTableNames` command. -/
structure PGetDatabaseTableNames where
  databaseId : DatabaseId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetDatabaseTableNames where
  toJSON p := Data.Json.object [("databaseId", ToJSON.toJSON p.databaseId)]

/-- Response of the `Database.getDatabaseTableNames` command. -/
structure GetDatabaseTableNames where
  tableNames : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetDatabaseTableNames where
  parseJSON v := do .ok { tableNames := ← Value.getField v "tableNames" >>= FromJSON.parseJSON }

instance : Command PGetDatabaseTableNames where
  Response := GetDatabaseTableNames
  commandName _ := "Database.getDatabaseTableNames"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.Database
