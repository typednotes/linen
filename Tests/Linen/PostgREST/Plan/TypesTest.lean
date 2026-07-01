/-
  Tests for `Linen.PostgREST.Plan.Types`.
-/
import Linen.PostgREST.Plan.Types

open PostgREST.Plan
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.Plan.Types

/-! ### `JsonOperation` -/

#guard JsonOperation.arrowRight "foo" == JsonOperation.arrowRight "foo"
#guard JsonOperation.arrowRight "foo" != JsonOperation.arrowRightRight "foo"

/-! ### Filter and ordering operators -/

#guard FilterOperator.simple "eq" == FilterOperator.simple "eq"
#guard FilterOperator.simple "eq" != FilterOperator.simple "neq"
#guard FilterOperator.quantified "any" "eq" == FilterOperator.quantified "any" "eq"

#guard LogicOperator.and_ != LogicOperator.or_
#guard OrderDirection.asc != OrderDirection.desc
#guard OrderNulls.nullsFirst != OrderNulls.nullsLast

/-! ### `CoercibleField` -/

#guard ({ cfName := "age" } : CoercibleField).cfJsonPath == []
#guard ({ cfName := "age" } : CoercibleField).cfTransform == none
#guard ({ cfName := "age", cfTransform := some "integer" } : CoercibleField) ==
  ({ cfName := "age", cfTransform := some "integer" } : CoercibleField)
#guard ({ cfName := "age" } : CoercibleField) != ({ cfName := "height" } : CoercibleField)

/-! ### `AggregateFunction` -/

#guard AggregateFunction.count.toSql == "count"
#guard AggregateFunction.jsonAgg.toSql == "json_agg"
#guard AggregateFunction.jsonbAgg.toSql == "jsonb_agg"
#guard toString AggregateFunction.sum == "sum"
#guard toString AggregateFunction.max_ == "max"
#guard toString AggregateFunction.min_ == "min"
#guard AggregateFunction.count == AggregateFunction.count
#guard AggregateFunction.count != AggregateFunction.sum

/-! ### `CoercibleSelectField` -/

#guard ({ csField := { cfName := "age" } } : CoercibleSelectField).csAlias == none
#guard ({ csField := { cfName := "age" }, csAggregate := some .count } : CoercibleSelectField).csAggregate ==
  some AggregateFunction.count

/-! ### `CoercibleFilter` -/

#guard ({ cfField := { cfName := "age" }, cfOperator := .simple "gt", cfValue := "18" } : CoercibleFilter).cfValue == "18"

/-! ### `CoercibleLogicTree` -/

/-- `CoercibleLogicTree` has no `BEq`/`DecidableEq` (recursion through `Array`
    isn't auto-derivable), so tests pattern-match directly. -/
def isStmnt : CoercibleLogicTree → Bool
  | .stmnt _ => true
  | .expr .. => false

#guard isStmnt (.stmnt { cfField := { cfName := "age" }, cfOperator := .simple "gt", cfValue := "18" }) == true
#guard isStmnt (.expr false .and_ #[]) == false

/-! ### `CoercibleOrderTerm` -/

#guard ({ cotField := { cfName := "age" } } : CoercibleOrderTerm).cotDirection == OrderDirection.asc
#guard ({ cotField := { cfName := "age" } } : CoercibleOrderTerm).cotNulls == none
#guard ({ cotField := { cfName := "age" }, cotDirection := .desc, cotNulls := some .nullsLast } : CoercibleOrderTerm).cotDirection ==
  OrderDirection.desc

/-! ### `SpreadType` / `RelJsonEmbedMode` -/

#guard SpreadType.o2o != SpreadType.o2m
#guard RelJsonEmbedMode.jsonObject != RelJsonEmbedMode.jsonArray

/-! ### `ConflictAction` -/

def conflictColumns : ConflictAction → Array FieldName
  | .doNothing => #[]
  | .doUpdate columns => columns

#guard conflictColumns .doNothing == #[]
#guard conflictColumns (.doUpdate #["a", "b"]) == #["a", "b"]

end Tests.PostgREST.Plan.Types
