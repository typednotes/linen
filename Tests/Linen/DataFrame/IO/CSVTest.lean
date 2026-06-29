/-
  Tests for `Linen.DataFrame.IO.CSV` — RFC 4180 CSV read/write.

  IO (`readCsv`/`writeCsv`) is the trivial file wrapper around the pure
  `parseCsv`/`toCsv`, which are exercised here.
-/
import Linen.DataFrame.IO.CSV

open DataFrame

namespace Tests.DataFrameCSV

/-! ### parseCsvRaw (the field/row state machine) -/

#guard parseCsvRaw "a,b,c\n1,2,3" == #[#["a", "b", "c"], #["1", "2", "3"]]
#guard parseCsvRaw "x,y\n1,2\n3,4" == #[#["x", "y"], #["1", "2"], #["3", "4"]]
#guard parseCsvRaw "a,\"b,c\",d" == #[#["a", "b,c", "d"]]          -- quoted field with delimiter
#guard parseCsvRaw "\"a\"\"b\"" == #[#["a\"b"]]                     -- doubled quote → literal quote
#guard parseCsvRaw "a,b\r\n1,2\r\n" == #[#["a", "b"], #["1", "2"]]  -- CRLF endings
#guard parseCsvRaw "" == (#[] : Array (Array String))

/-! ### parseCsv: structure + type inference -/

private def df : DataFrame := parseCsv "name,age,active\nAlice,30,true\nBob,25,false"

#guard df.columnNames == ["name", "age", "active"]
#guard df.nRows == 2
#guard df.nColumns == 3
#guard df.getRow? 0 == some #[Value.str "Alice", Value.int 30, Value.bool true]
#guard df.getRow? 1 == some #[Value.str "Bob", Value.int 25, Value.bool false]
#guard (df.getColumn? "name").map (·.colType) == some ColumnType.str
#guard (df.getColumn? "age").map (·.colType) == some ColumnType.int
#guard (df.getColumn? "active").map (·.colType) == some ColumnType.bool

/-! ### float + null inference -/

#guard (parseCsv "x\n1.5\n2.5").getRow? 0 == some #[Value.float 1.5]
#guard ((parseCsv "x\n1.5\n2.5").getColumn? "x").map (·.colType) == some ColumnType.float
#guard (parseCsv "x\nNA\n5").getRow? 0 == some #[Value.null]
#guard ((parseCsv "x\nNA\n5").getColumn? "x").map (·.colType) == some ColumnType.int  -- null skipped

/-! ### no-header mode -/

private def dfNH : DataFrame := parseCsv "1,2\n3,4" { hasHeader := false }

#guard dfNH.columnNames == ["col0", "col1"]
#guard dfNH.nRows == 2
#guard dfNH.getRow? 0 == some #[Value.int 1, Value.int 2]

/-! ### toCsv -/

#guard df.toCsv == "name,age,active\nAlice,30,true\nBob,25,false"
#guard (parseCsv "a\n\"x,y\"").toCsv == "a\n\"x,y\""          -- comma field is re-quoted
#guard (DataFrame.empty.toCsv { hasHeader := false }) == ""

/-! ### round-trip -/

#guard (parseCsv df.toCsv).getRow? 0 == some #[Value.str "Alice", Value.int 30, Value.bool true]
#guard (parseCsv df.toCsv).columnNames == ["name", "age", "active"]

end Tests.DataFrameCSV
