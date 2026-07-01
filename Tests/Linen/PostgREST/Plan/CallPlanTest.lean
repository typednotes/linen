/-
  Tests for `Linen.PostgREST.Plan.CallPlan`.
-/
import Linen.PostgREST.Plan.CallPlan

open PostgREST.Plan
open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.Plan.CallPlan

def sumFn : Routine :=
  { funcSchema := "public"
    funcName := "sum_ab"
    funcParams := #[{ ppName := "a", ppType := "int4", ppRequired := true },
                     { ppName := "b", ppType := "int4", ppRequired := true }]
    funcReturnType := .single "int4"
    funcVolatility := .immutable }

def sumCall : CallPlan :=
  { cpRoutine := sumFn
    cpParams := [("a", "1"), ("b", "2")]
    cpReturning := #[] }

/-! ### Defaults -/

#guard sumCall.cpPreferSingle == false

/-! ### `routineQi` -/

#guard sumCall.routineQi == ({ qiSchema := "public", qiName := "sum_ab" } : QualifiedIdentifier)

/-! ### `isSetof` / `isSafeForGet` -/

#guard sumCall.isSetof == false
#guard sumCall.isSafeForGet == true

def volatileFn : Routine :=
  { funcSchema := "public", funcName := "do_stuff", funcReturnType := .setof "record" }

def volatileCall : CallPlan :=
  { cpRoutine := volatileFn, cpParams := [], cpReturning := #[] }

#guard volatileCall.isSetof == true
#guard volatileCall.isSafeForGet == false

/-! ### `paramCount` -/

#guard sumCall.paramCount == 2
#guard volatileCall.paramCount == 0

end Tests.PostgREST.Plan.CallPlan
