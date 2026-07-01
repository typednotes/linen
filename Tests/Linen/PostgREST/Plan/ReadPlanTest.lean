/-
  Tests for `Linen.PostgREST.Plan.ReadPlan`.
-/
import Linen.PostgREST.Plan.ReadPlan

open PostgREST.Plan
open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers
open PostgREST.RangeQuery

namespace Tests.PostgREST.Plan.ReadPlan

def usersPlan : ReadPlan :=
  { rpSelect := #[{ csField := { cfName := "id" } }, { csField := { cfName := "name" } }]
    rpFrom := { qiSchema := "public", qiName := "users" }
    rpWhere := #[]
    rpOrder := #[] }

/-! ### Defaults -/

#guard usersPlan.rpRange == NonnegRange.unlimited
#guard usersPlan.rpRelationships.isEmpty == true
#guard usersPlan.rpIsInner == false
#guard usersPlan.rpAlias == none

/-! ### `hasEmbeds` / `embedCount` -/

#guard usersPlan.hasEmbeds == false
#guard usersPlan.embedCount == 0

def usersPostsRel : Relationship :=
  { relTable := { qiSchema := "public", qiName := "users" }
    relForeignTable := { qiSchema := "public", qiName := "posts" }
    relCardinality := .o2m
    relColumns := #[("id", "user_id")] }

def postsPlan : ReadPlan :=
  { rpSelect := #[{ csField := { cfName := "id" } }]
    rpFrom := { qiSchema := "public", qiName := "posts" }
    rpWhere := #[]
    rpOrder := #[] }

def usersWithPostsPlan : ReadPlan :=
  { usersPlan with rpRelationships := #[(usersPostsRel, postsPlan)] }

#guard usersWithPostsPlan.hasEmbeds == true
#guard usersWithPostsPlan.embedCount == 1

/-! ### `hasFilters` / `hasOrdering` -/

#guard usersPlan.hasFilters == false
#guard usersPlan.hasOrdering == false

def filteredPlan : ReadPlan :=
  { usersPlan with rpWhere := #[{ cfField := { cfName := "age" }, cfOperator := .simple "gt", cfValue := "18" }] }

#guard filteredPlan.hasFilters == true

def orderedPlan : ReadPlan :=
  { usersPlan with rpOrder := #[{ cotField := { cfName := "name" } }] }

#guard orderedPlan.hasOrdering == true

/-! ### Custom range -/

def pagedPlan : ReadPlan :=
  { usersPlan with rpRange := { rangeOffset := 10, rangeLimit := some 20 } }

#guard pagedPlan.rpRange.rangeOffset == 10
#guard pagedPlan.rpRange.size == some 20
#guard pagedPlan.rpRange.isUnlimited == false

end Tests.PostgREST.Plan.ReadPlan
