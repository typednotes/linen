/-
  Tests for `Linen.PostgREST.Plan.MutatePlan`.
-/
import Linen.PostgREST.Plan.MutatePlan

open PostgREST.Plan
open PostgREST.SchemaCache.Identifiers
open PostgREST.RangeQuery

namespace Tests.PostgREST.Plan.MutatePlan

def usersTable : QualifiedIdentifier := { qiSchema := "public", qiName := "users" }

def insertPlan : MutatePlan :=
  .insert usersTable #[{ cfName := "name" }] "{\"name\":\"bob\"}" none
    #[{ csField := { cfName := "id" } }] #[]

def updatePlan : MutatePlan :=
  .update usersTable #[{ cfName := "name" }] "{\"name\":\"bob\"}"
    #[{ cfField := { cfName := "id" }, cfOperator := .simple "eq", cfValue := "1" }]
    #[] NonnegRange.unlimited

def deletePlan : MutatePlan :=
  .delete usersTable
    #[{ cfField := { cfName := "id" }, cfOperator := .simple "eq", cfValue := "1" }]
    #[{ csField := { cfName := "id" } }] NonnegRange.unlimited

/-! ### `targetTable` -/

#guard insertPlan.targetTable == usersTable
#guard updatePlan.targetTable == usersTable
#guard deletePlan.targetTable == usersTable

/-! ### `returningFields` -/

#guard insertPlan.returningFields.size == 1
#guard updatePlan.returningFields.size == 0
#guard deletePlan.returningFields.size == 1

/-! ### `hasReturning` -/

#guard insertPlan.hasReturning == true
#guard updatePlan.hasReturning == false
#guard deletePlan.hasReturning == true

end Tests.PostgREST.Plan.MutatePlan
