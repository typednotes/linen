/-
  PostgREST.Plan.CallPlan -- RPC call plans

  Represents a resolved plan for calling a PostgreSQL stored function
  via the PostgREST RPC interface (`POST /rpc/function_name`).

  ## Haskell source
  - `PostgREST.Plan.CallPlan` (postgrest package)

  ## Design
  - `CallPlan` binds a `Routine` (from the schema cache) to concrete
    parameter values and a returning clause:
    $$\text{CallPlan} = \{ \text{routine} : \text{Routine},\;
      \text{params} : [(\text{String}, \text{String})],\;
      \text{returning} : [\text{SelectField}],\;
      \text{preferSingle} : \text{Bool} \}$$
  - `cpPreferSingle` controls whether the result is wrapped as a JSON
    object (true) or array (false), driven by the `Prefer: return=representation`
    header with `vnd.pgrst.object` accept type
-/

import Linen.PostgREST.Plan.Types
import Linen.PostgREST.SchemaCache.Routine

namespace PostgREST.Plan

open PostgREST.SchemaCache

-- ────────────────────────────────────────────────────────────────────
-- Call plan
-- ────────────────────────────────────────────────────────────────────

/-- A plan for calling a stored procedure via RPC.
    $$\text{CallPlan} = \{ \text{routine},\; \text{params},\;
      \text{returning},\; \text{preferSingle} \}$$ -/
structure CallPlan where
  /-- The routine to call (from the schema cache). -/
  cpRoutine : Routine
  /-- Parameter bindings: (param_name, value) pairs. -/
  cpParams : List (String × String)
  /-- Columns to return (RETURNING clause equivalent). -/
  cpReturning : Array CoercibleSelectField
  /-- Whether to return a single JSON object instead of an array. -/
  cpPreferSingle : Bool := false

/-- The qualified identifier of the routine being called. -/
def CallPlan.routineQi (cp : CallPlan) : PostgREST.SchemaCache.Identifiers.QualifiedIdentifier :=
  cp.cpRoutine.toQi

/-- Whether this call expects a set-returning function result. -/
def CallPlan.isSetof (cp : CallPlan) : Bool :=
  cp.cpRoutine.funcReturnType.isSetof

/-- Whether this call is safe for HTTP GET (immutable or stable function). -/
def CallPlan.isSafeForGet (cp : CallPlan) : Bool :=
  cp.cpRoutine.isSafeForGet

/-- The number of parameters being passed. -/
def CallPlan.paramCount (cp : CallPlan) : Nat :=
  cp.cpParams.length

end PostgREST.Plan
