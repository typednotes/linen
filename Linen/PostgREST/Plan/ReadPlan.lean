/-
  PostgREST.Plan.ReadPlan -- Read plan (SELECT queries)

  A read plan represents a resolved SELECT query with possible embedded
  sub-queries (resource embedding).  Each embedded read becomes a lateral
  join in the generated SQL.

  ## Haskell source
  - `PostgREST.Plan.ReadPlan` (postgrest package)

  ## Design
  - Pagination reuses `PostgREST.RangeQuery.NonnegRange` (offset and
    optional limit, both non-negative by construction via `Nat`) rather
    than redeclaring an equivalent range type
  - `ReadPlan` is recursive via `rpRelationships`, where each embedded
    relationship produces a lateral sub-query:
    $$\text{ReadPlan} = \{ \text{select},\; \text{from},\; \text{where},\;
      \text{order},\; \text{range},\; \text{embeds} : [(\text{Rel}, \text{ReadPlan})] \}$$
-/

import Linen.PostgREST.Plan.Types
import Linen.PostgREST.SchemaCache.Identifiers
import Linen.PostgREST.SchemaCache.Relationship
import Linen.PostgREST.RangeQuery

namespace PostgREST.Plan

open PostgREST.SchemaCache.Identifiers
open PostgREST.SchemaCache
open PostgREST.RangeQuery

-- ────────────────────────────────────────────────────────────────────
-- Read plan
-- ────────────────────────────────────────────────────────────────────

/-- A read plan represents a SELECT query with possible embedded sub-queries.
    $$\text{ReadPlan} = \{ \text{select} : [\text{SelectField}],\;
      \text{from} : \text{QI},\; \text{where} : [\text{Filter}],\;
      \text{order} : [\text{OrderTerm}],\; \text{range} : \text{NonnegRange},\;
      \text{embeds} : [(\text{Rel}, \text{ReadPlan})],\;
      \text{isInner} : \text{Bool} \}$$

    - `rpSelect`: columns and expressions to select
    - `rpFrom`: the source table or view
    - `rpWhere`: filter conditions
    - `rpOrder`: ordering specification
    - `rpRange`: pagination (offset/limit)
    - `rpRelationships`: embedded reads via lateral joins
    - `rpIsInner`: whether the embed uses INNER (true) or LEFT (false) join
    - `rpAlias`: optional alias for this read in the query -/
structure ReadPlan where
  rpSelect : Array CoercibleSelectField
  rpFrom : QualifiedIdentifier
  rpWhere : Array CoercibleFilter
  rpOrder : Array CoercibleOrderTerm
  rpRange : NonnegRange := .unlimited
  rpRelationships : Array (Relationship × ReadPlan) := #[]
  rpIsInner : Bool := false
  rpAlias : Option String := none
  deriving Repr

/-- Whether this read plan has any embedded sub-queries. -/
def ReadPlan.hasEmbeds (rp : ReadPlan) : Bool :=
  !rp.rpRelationships.isEmpty

/-- The number of embedded sub-queries. -/
def ReadPlan.embedCount (rp : ReadPlan) : Nat :=
  rp.rpRelationships.size

/-- Whether this read plan applies any filters. -/
def ReadPlan.hasFilters (rp : ReadPlan) : Bool :=
  !rp.rpWhere.isEmpty

/-- Whether this read plan specifies an ordering. -/
def ReadPlan.hasOrdering (rp : ReadPlan) : Bool :=
  !rp.rpOrder.isEmpty

end PostgREST.Plan
