/-
  Tests for `Linen.PostgREST.Config.Database`.
-/
import Linen.PostgREST.Config.Database

open PostgREST.Config

namespace Tests.PostgREST.Config.Database

/-! ### `DbUriParts.toUri` -/

#guard ({} : DbUriParts).toUri == "postgresql://postgres@localhost:5432/postgres"
#guard ({ dbHost := "db.example.com", dbPort := 5433, dbName := "app", dbUser := "app_user", dbPassword := some "s3cr3t" } : DbUriParts).toUri == "postgresql://app_user:s3cr3t@db.example.com:5433/app"

/-! ### `searchPathSql` -/

#guard searchPathSql ["public"] [] == "SET search_path TO \"public\""
#guard searchPathSql ["public", "api"] ["extensions"] ==
  "SET search_path TO \"public\", \"api\", \"extensions\""
#guard searchPathSql ["a\"b"] [] == "SET search_path TO \"a\"\"b\""

/-! ### `searchPathDisplay` -/

#guard searchPathDisplay ["public"] [] == "public"
#guard searchPathDisplay ["public", "api"] ["extensions"] == "public, api, extensions"

/-! ### `setRoleSql` / `resetRoleSql` -/

#guard setRoleSql "webuser" == "SET LOCAL ROLE \"webuser\""
#guard setRoleSql "a\"b" == "SET LOCAL ROLE \"a\"\"b\""
#guard resetRoleSql == "RESET ROLE"

/-! ### `TxMode` / `setTxModeSql` -/

#guard setTxModeSql .readOnly == "SET TRANSACTION READ ONLY"
#guard setTxModeSql .readWrite == "SET TRANSACTION READ WRITE"
#guard TxMode.readOnly != TxMode.readWrite

/-! ### `TxEnd` -/

#guard TxEnd.commit != TxEnd.rollback

end Tests.PostgREST.Config.Database
