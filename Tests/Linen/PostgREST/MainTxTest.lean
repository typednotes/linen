/-
  Tests for `Linen.PostgREST.MainTx`.
-/
import Linen.PostgREST.MainTx

open PostgREST.MainTx
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.MainTx

/-! ### `sqlLit` -/

#guard sqlLit "hello" == "'hello'"
#guard sqlLit "it's" == "'it''s'"

/-! ### `setSearchPath` -/

#guard setSearchPath ["public"] == "SET LOCAL search_path TO \"public\""
#guard setSearchPath ["public", "api"] == "SET LOCAL search_path TO \"public\", \"api\""
#guard setSearchPath ["a\"b"] == "SET LOCAL search_path TO \"a\"\"b\""

/-! ### `setRole` -/

#guard setRole "webuser" == "SET LOCAL role TO 'webuser'"
#guard setRole "it's" == "SET LOCAL role TO 'it''s'"

/-! ### `setRequestContext` -/

#guard setRequestContext "GET" "/users" "webuser" [("role", "webuser")] [("Accept", "application/json")] ==
  [ "SET LOCAL role TO 'webuser'"
  , "SET LOCAL request.jwt.claims TO '{\"role\": \"webuser\"}'"
  , "SET LOCAL request.method TO 'GET'"
  , "SET LOCAL request.path TO '/users'"
  , "SET LOCAL request.headers TO '{\"Accept\": \"application/json\"}'" ]

#guard setRequestContext "POST" "/rpc/f" "anon" [] [] ==
  [ "SET LOCAL role TO 'anon'"
  , "SET LOCAL request.jwt.claims TO '{}'"
  , "SET LOCAL request.method TO 'POST'"
  , "SET LOCAL request.path TO '/rpc/f'"
  , "SET LOCAL request.headers TO '{}'" ]

/-! ### `preRequestSql` -/

#guard preRequestSql { qiSchema := "public", qiName := "pre_request" } == "SELECT \"public\".\"pre_request\"()"

end Tests.PostgREST.MainTx
