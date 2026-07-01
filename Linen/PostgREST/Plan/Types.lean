/-
  PostgREST.Plan.Types -- Plan types for resolved query plans

  Core types used by the query planner to represent a resolved query plan.
  These types describe fields with optional coercion, aggregation, filtering,
  ordering, and conflict resolution.

  ## Haskell source
  - `PostgREST.Plan.Types` (postgrest package)

  ## Design
  - `CoercibleField` represents a column reference with optional JSON path
    traversal and type coercion:
    $$\text{CoercibleField} = \{ \text{name} : \text{FieldName},\;
      \text{jsonPath} : [\text{JsonOp}],\; \text{cast} : \text{String}? \}$$
  - `CoercibleFilter` pairs a field with an operator and value
  - `CoercibleLogicTree` is a recursive boolean expression tree over filters
  - Filter operators, logic operators, order directions, and order nulls are
    defined locally (these duplicate ApiRequest.Types; will be unified later)
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.Plan

open PostgREST.SchemaCache.Identifiers

-- ────────────────────────────────────────────────────────────────────
-- JSON path operations
-- ────────────────────────────────────────────────────────────────────

/-- A JSON path operation applied to a column reference.
    $$\text{JsonOperation} \in \{ \mathtt{->}\,k,\; \mathtt{->>}\,k \}$$ -/
inductive JsonOperation where
  /-- `->` key: extract JSON object field as JSON. -/
  | arrowRight (key : String)
  /-- `->>` key: extract JSON object field as text. -/
  | arrowRightRight (key : String)
  deriving BEq, Repr

-- ────────────────────────────────────────────────────────────────────
-- Filter and ordering operators (local definitions)
-- NOTE: these duplicate ApiRequest.Types; will be unified later
-- ────────────────────────────────────────────────────────────────────

/-- A filter operator.
    - `simple`: standard SQL comparison (eq, neq, gt, lt, ...)
    - `fts`: full-text search operator
    - `quantified`: quantified comparison (any, all) -/
inductive FilterOperator where
  | simple (op : String)
  | fts (op : String)
  | quantified (quant : String) (op : String)
  deriving BEq, Repr

/-- Boolean logic operators for combining filters. -/
inductive LogicOperator where
  | and_
  | or_
  deriving BEq, Repr

/-- Order direction for ORDER BY clauses. -/
inductive OrderDirection where
  | asc
  | desc
  deriving BEq, Repr

/-- Null ordering for ORDER BY clauses. -/
inductive OrderNulls where
  | nullsFirst
  | nullsLast
  deriving BEq, Repr

-- ────────────────────────────────────────────────────────────────────
-- Coercible field
-- ────────────────────────────────────────────────────────────────────

/-- A field reference that may need type coercion.
    $$\text{CoercibleField} = \{ \text{name},\; \text{jsonPath},\;
      \text{transform},\; \text{irType},\; \text{default} \}$$
    The `cfTransform` is a `::` cast expression (e.g., `"text"`, `"integer"`).
    The `cfIRType` is the introspected type from the schema cache. -/
structure CoercibleField where
  cfName : FieldName
  cfJsonPath : List JsonOperation := []
  cfTransform : Option String := none
  cfIRType : Option String := none
  cfDefault : Option String := none
  deriving BEq, Repr

-- ────────────────────────────────────────────────────────────────────
-- Aggregation
-- ────────────────────────────────────────────────────────────────────

/-- Aggregation functions supported in SELECT expressions.
    $$\text{AggregateFunction} \in \{ \text{count}, \text{sum}, \text{avg},
      \text{max}, \text{min}, \text{json\_agg}, \text{jsonb\_agg} \}$$ -/
inductive AggregateFunction where
  | count
  | sum
  | avg
  | max_
  | min_
  | jsonAgg
  | jsonbAgg
  deriving BEq, Repr

/-- Render an aggregate function to its SQL name. -/
def AggregateFunction.toSql : AggregateFunction -> String
  | .count    => "count"
  | .sum      => "sum"
  | .avg      => "avg"
  | .max_     => "max"
  | .min_     => "min"
  | .jsonAgg  => "json_agg"
  | .jsonbAgg => "jsonb_agg"

instance : ToString AggregateFunction := ⟨AggregateFunction.toSql⟩

-- ────────────────────────────────────────────────────────────────────
-- Select field
-- ────────────────────────────────────────────────────────────────────

/-- A select field with optional alias, cast, and aggregation.
    $$\text{CoercibleSelectField} = \{ \text{field},\; \text{alias}?,\;
      \text{cast}?,\; \text{aggregate}? \}$$ -/
structure CoercibleSelectField where
  csField : CoercibleField
  csAlias : Option String := none
  csCast : Option String := none
  csAggregate : Option AggregateFunction := none
  deriving Repr

-- ────────────────────────────────────────────────────────────────────
-- Filter
-- ────────────────────────────────────────────────────────────────────

/-- A filter on a coercible field: field, operator, and literal value.
    $$\text{CoercibleFilter} = \text{CoercibleField} \times
      \text{FilterOperator} \times \text{String}$$ -/
structure CoercibleFilter where
  cfField : CoercibleField
  cfOperator : FilterOperator
  cfValue : String
  deriving Repr

-- ────────────────────────────────────────────────────────────────────
-- Logic tree
-- ────────────────────────────────────────────────────────────────────

/-- A recursive boolean expression tree over coercible filters.
    $$\text{CoercibleLogicTree} ::= \text{stmnt}(f)
      \mid \text{expr}(\neg?,\; \text{op},\; [\text{children}])$$ -/
inductive CoercibleLogicTree where
  | stmnt (filter : CoercibleFilter)
  | expr (negated : Bool) (op : LogicOperator) (children : Array CoercibleLogicTree)

-- ────────────────────────────────────────────────────────────────────
-- Order term
-- ────────────────────────────────────────────────────────────────────

/-- An ORDER BY term on a coercible field.
    $$\text{CoercibleOrderTerm} = \text{CoercibleField} \times
      \text{OrderDirection} \times \text{OrderNulls}?$$ -/
structure CoercibleOrderTerm where
  cotField : CoercibleField
  cotDirection : OrderDirection := .asc
  cotNulls : Option OrderNulls := none
  deriving Repr

-- ────────────────────────────────────────────────────────────────────
-- Spread and embed modes
-- ────────────────────────────────────────────────────────────────────

/-- Spread type for embedded resources. -/
inductive SpreadType where
  | o2o
  | o2m
  deriving BEq, Repr

/-- JSON embedding mode for relationships. -/
inductive RelJsonEmbedMode where
  | jsonObject
  | jsonArray
  deriving BEq, Repr

-- ────────────────────────────────────────────────────────────────────
-- Conflict resolution (upserts)
-- ────────────────────────────────────────────────────────────────────

/-- Conflict resolution action for INSERT ... ON CONFLICT.
    $$\text{ConflictAction} \in \{ \text{DO NOTHING},\;
      \text{DO UPDATE SET}(\text{cols}) \}$$ -/
inductive ConflictAction where
  | doNothing
  | doUpdate (columns : Array FieldName)
  deriving Repr

end PostgREST.Plan
