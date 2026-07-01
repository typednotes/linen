/-
  Tests for `Linen.PostgREST.SchemaCache`.
-/
import Linen.PostgREST.SchemaCache

open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.SchemaCache

def usersQi : QualifiedIdentifier := { qiSchema := "public", qiName := "users" }
def postsQi : QualifiedIdentifier := { qiSchema := "public", qiName := "posts" }

def usersTable : Table :=
  { tableSchema := "public", tableName := "users" }

def usersPostsRel : Relationship :=
  { relTable := usersQi, relForeignTable := postsQi, relCardinality := .o2m
    relColumns := #[("id", "user_id")] }

def sumFn : Routine :=
  { funcSchema := "public", funcName := "sum_ab", funcReturnType := .single "int4" }

/-! ### `empty`
    `Table` and `Routine` don't derive `BEq` (Table carries a proof field;
    Routine matches upstream), so checks against them use `isEmpty`/`isNone`
    or compare a projected field instead of `==`. -/

#guard SchemaCache.empty.dbTables.isEmpty == true
#guard SchemaCache.empty.dbRelationships.isEmpty == true
#guard SchemaCache.empty.dbRoutines.isEmpty == true
#guard SchemaCache.empty.dbMediaHandlers == []
#guard SchemaCache.empty.dbTimezones == []
#guard SchemaCache.empty.dbPgVersion == none

def sc : SchemaCache :=
  { dbTables := [(usersQi, usersTable)]
    dbRelationships := [(usersQi, #[usersPostsRel])]
    dbRoutines := [(usersQi, #[sumFn])]
    dbRepresentations := [] }

/-! ### `findTable` -/

#guard (SchemaCache.findTable sc usersQi).map (·.tableName) == some "users"
#guard (SchemaCache.findTable sc postsQi).isNone == true

/-! ### `findRelationships` -/

#guard SchemaCache.findRelationships sc usersQi == #[usersPostsRel]
#guard SchemaCache.findRelationships sc postsQi == #[]

/-! ### `findRoutines` -/

#guard (SchemaCache.findRoutines sc usersQi).map (·.funcName) == #["sum_ab"]
#guard (SchemaCache.findRoutines sc postsQi).isEmpty == true

/-! ### `tablesInSchemas` -/

#guard (SchemaCache.tablesInSchemas sc ["public"]).map (·.tableName) == ["users"]
#guard (SchemaCache.tablesInSchemas sc ["private"]).isEmpty == true

/-! ### SQL query generation (basic sanity) -/

#guard (SchemaCache.tablesSql ["public"]).startsWith "SELECT"
#guard (SchemaCache.columnsSql ["public"]).startsWith "SELECT"
#guard (SchemaCache.relationshipsSql ["public"]).startsWith "SELECT"
#guard (SchemaCache.routinesSql ["public"]).startsWith "SELECT"
#guard SchemaCache.versionSql == "SELECT current_setting('server_version_num')::integer"

end Tests.PostgREST.SchemaCache
