/-
  Tests for `Linen.PostgREST.SchemaCache.Table`.
-/
import Linen.PostgREST.SchemaCache.Table

open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.SchemaCache.Table

def usersQi : QualifiedIdentifier := { qiSchema := "public", qiName := "users" }

def idCol : Column :=
  { colTable := usersQi, colName := "id", colNullable := false, colType := "int4", colIsPrimaryKey := true }

def nameCol : Column :=
  { colTable := usersQi, colName := "name", colNullable := true, colType := "text" }

/-! ### `Column` -/

#guard idCol.colMaxLen == none
#guard idCol.colEnum == []
#guard toString idCol == "public.users.id : int4"
#guard idCol == idCol
#guard idCol != nameCol

/-! ### `Table` -/

def usersTable : Table :=
  { tableSchema := "public"
    tableName := "users"
    tableColumns := #[idCol, nameCol]
    tablePrimaryKey := #[idCol] }

#guard usersTable.tableInsertable == true
#guard usersTable.tableUpdatable == true
#guard usersTable.tableDeletable == true
#guard usersTable.tableIsView == false

#guard usersTable.toQi == usersQi
#guard toString usersTable == "public.users (2 cols)"

/-! ### `findColumn` -/

#guard usersTable.findColumn "id" == some idCol
#guard usersTable.findColumn "missing" == none

/-! ### `columnNames` / `pkColumnNames` -/

#guard usersTable.columnNames == #["id", "name"]
#guard usersTable.pkColumnNames == #["id"]

/-! ### `hasPrimaryKey` -/

#guard usersTable.hasPrimaryKey == true

def noPkTable : Table :=
  { tableSchema := "public", tableName := "logs", tableColumns := #[nameCol] }

#guard noPkTable.hasPrimaryKey == false
#guard noPkTable.pkColumnNames == #[]

end Tests.PostgREST.SchemaCache.Table
