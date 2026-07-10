/-
  Tests for `Linen.CDP.Domains.Database`.
-/
import Linen.CDP.Domains.Database

open CDP.Domains.Database
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON Value)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Database

/-! ### DB / DBError -/

#guard decodeAs "{\"id\": \"1\", \"domain\": \"d\", \"name\": \"n\", \"version\": \"v\"}" (α := DB)
  = .ok { id := "1", domain := "d", name := "n", version := "v" }
#guard encode (ToJSON.toJSON ({ id := "1", domain := "d", name := "n", version := "v" } : DB))
  = "{\"id\":\"1\",\"domain\":\"d\",\"name\":\"n\",\"version\":\"v\"}"
#guard decodeAs "{\"message\": \"bad\", \"code\": 1}" (α := DBError) = .ok { message := "bad", code := 1 }

/-! ### Event -/

#guard Event.eventName (α := AddDatabase) = "Database.addDatabase"
#guard decodeAs "{\"database\": {\"id\": \"1\", \"domain\": \"d\", \"name\": \"n\", \"version\": \"v\"}}"
    (α := AddDatabase)
  = .ok { database := { id := "1", domain := "d", name := "n", version := "v" } }

/-! ### Commands -/

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PDisable) = "Database.disable"
#guard Command.commandName ({} : PEnable) = "Database.enable"
#guard Command.commandName ({ databaseId := "1", query := "SELECT 1" } : PExecuteSQL) = "Database.executeSQL"
#guard Command.commandName ({ databaseId := "1" } : PGetDatabaseTableNames)
  = "Database.getDatabaseTableNames"

/-! ### ExecuteSQL response — every field optional -/

#guard decodeAs "{}" (α := ExecuteSQL) = .ok { columnNames := none, values := none, sqlError := none }
#guard decodeAs "{\"columnNames\": [\"a\"], \"values\": [1], \"sqlError\": {\"message\": \"e\", \"code\": 2}}"
    (α := ExecuteSQL)
  = .ok { columnNames := some ["a"], values := some [Value.number 1]
        , sqlError := some { message := "e", code := 2 } }

#guard decodeAs "{\"tableNames\": [\"t1\", \"t2\"]}" (α := GetDatabaseTableNames)
  = .ok { tableNames := ["t1", "t2"] }

end Tests.CDP.Domains.Database
