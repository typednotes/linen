/-
  Tests for `Linen.PostgREST.SchemaCache.Routine`.
-/
import Linen.PostgREST.SchemaCache.Routine

open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.SchemaCache.Routine

/-! ### `Volatility` -/

#guard toString Volatility.immutable == "IMMUTABLE"
#guard toString Volatility.stable == "STABLE"
#guard toString Volatility.volatile == "VOLATILE"
#guard Volatility.immutable != Volatility.volatile

/-! ### `IsolationLevel` -/

#guard toString IsolationLevel.readCommitted == "READ COMMITTED"
#guard toString IsolationLevel.repeatableRead == "REPEATABLE READ"
#guard toString IsolationLevel.serializable == "SERIALIZABLE"

/-! ### `RoutineParam` -/

#guard ({ ppName := "x", ppType := "int4", ppRequired := true } : RoutineParam).ppMode == ParamMode.in_
#guard ({ ppName := "x", ppType := "int4", ppRequired := true } : RoutineParam) ==
  ({ ppName := "x", ppType := "int4", ppRequired := true } : RoutineParam)

/-! ### `RoutineReturnType` -/

#guard (RoutineReturnType.single "int4").isSetof == false
#guard (RoutineReturnType.setof "users").isSetof == true
#guard RoutineReturnType.void.isSetof == false

/-! ### `Routine` -/

def sumFn : Routine :=
  { funcSchema := "public"
    funcName := "sum_ab"
    funcParams := #[{ ppName := "a", ppType := "int4", ppRequired := true },
                     { ppName := "b", ppType := "int4", ppRequired := false }]
    funcReturnType := .single "int4"
    funcVolatility := .immutable }

#guard sumFn.toQi == ({ qiSchema := "public", qiName := "sum_ab" } : QualifiedIdentifier)
#guard sumFn.requiredParams == #["a"]
#guard sumFn.isSafeForGet == true
#guard toString sumFn == "public.sum_ab(2 params)"

def volatileFn : Routine :=
  { funcSchema := "public", funcName := "do_stuff", funcReturnType := .void }

#guard volatileFn.funcVolatility == Volatility.volatile
#guard volatileFn.isSafeForGet == false
#guard volatileFn.requiredParams == #[]

/-! ### Safety theorem -/

example : ∀ r : Routine, r.isSafeForGet = true ↔ r.funcVolatility ≠ .volatile :=
  Routine.isSafeForGet_iff_not_volatile

end Tests.PostgREST.SchemaCache.Routine
