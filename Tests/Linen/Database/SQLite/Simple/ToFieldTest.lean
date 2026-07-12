/-
  Tests for `Linen.Database.SQLite.Simple.ToField`.
-/
import Linen.Database.SQLite.Simple.ToField

open Database.SQLite.Simple
open Database.SQLite3 (SQLData)

namespace Tests.Database.SQLite.Simple.ToField

#guard toField (5 : Int8) == SQLData.integer 5
#guard toField (5 : Int16) == SQLData.integer 5
#guard toField (5 : Int32) == SQLData.integer 5
#guard toField (5 : Int64) == SQLData.integer 5
#guard toField (-5 : Int) == SQLData.integer (-5)
#guard toField (5 : UInt8) == SQLData.integer 5
#guard toField (5 : UInt16) == SQLData.integer 5
#guard toField (5 : UInt32) == SQLData.integer 5
#guard toField (5 : UInt64) == SQLData.integer 5
#guard toField (5 : Nat) == SQLData.integer 5

#guard toField true == SQLData.integer 1
#guard toField false == SQLData.integer 0

#guard toField (3.5 : Float) == SQLData.float 3.5

#guard toField "hello" == SQLData.text "hello"
#guard toField (ByteArray.mk #[1, 2, 3]) == SQLData.blob (ByteArray.mk #[1, 2, 3])

#guard toField (SQLData.integer 42) == SQLData.integer 42
#guard toField (none : Option Int) == SQLData.null
#guard toField (some (7 : Int)) == SQLData.integer 7
#guard toField Types.Null.null == SQLData.null

#guard toField (Data.Time.Day.fromGregorian 2024 3 5) == SQLData.text "2024-03-05"

end Tests.Database.SQLite.Simple.ToField
