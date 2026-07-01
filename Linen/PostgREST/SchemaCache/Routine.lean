/-
  PostgREST.SchemaCache.Routine — Stored procedure/function metadata

  Types representing PostgreSQL functions and procedures.  PostgREST
  exposes these as RPC endpoints: `POST /rpc/function_name`.

  ## Haskell source
  - `PostgREST.SchemaCache.Routine` (postgrest package)
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.SchemaCache

open PostgREST.SchemaCache.Identifiers

-- ────────────────────────────────────────────────────────────────────
-- Volatility
-- ────────────────────────────────────────────────────────────────────

/-- PostgreSQL function volatility classification.
    Determines cacheability and side effects. -/
inductive Volatility where
  | immutable
  | stable
  | volatile
  deriving BEq, Repr

instance : ToString Volatility where
  toString
    | .immutable => "IMMUTABLE"
    | .stable => "STABLE"
    | .volatile => "VOLATILE"

-- ────────────────────────────────────────────────────────────────────
-- Isolation level
-- ────────────────────────────────────────────────────────────────────

/-- Transaction isolation level for a routine. -/
inductive IsolationLevel where
  | readCommitted
  | repeatableRead
  | serializable
  deriving BEq, Repr

instance : ToString IsolationLevel where
  toString
    | .readCommitted => "READ COMMITTED"
    | .repeatableRead => "REPEATABLE READ"
    | .serializable => "SERIALIZABLE"

-- ────────────────────────────────────────────────────────────────────
-- Routine parameter
-- ────────────────────────────────────────────────────────────────────

/-- Parameter mode for a routine parameter. -/
inductive ParamMode where
  | in_
  | out
  | inout
  | variadic
  deriving BEq, Repr

/-- A single parameter of a PostgreSQL function. -/
structure RoutineParam where
  /-- Parameter name (empty for unnamed positional parameters). -/
  ppName : String
  /-- PostgreSQL type name. -/
  ppType : String
  /-- Whether this parameter is required (has no default). -/
  ppRequired : Bool
  /-- Parameter mode. -/
  ppMode : ParamMode := .in_
  deriving BEq, Repr

-- ────────────────────────────────────────────────────────────────────
-- Return type
-- ────────────────────────────────────────────────────────────────────

/-- Return type of a PostgreSQL function. -/
inductive RoutineReturnType where
  /-- Returns a single value of the given type. -/
  | single (typeName : String)
  /-- Returns a set of rows (SETOF). -/
  | setof (typeName : String)
  /-- Returns void (procedure or RETURNS VOID). -/
  | void
  deriving BEq, Repr

/-- Does this return type produce multiple rows? -/
def RoutineReturnType.isSetof : RoutineReturnType → Bool
  | .setof _ => true
  | _ => false

-- ────────────────────────────────────────────────────────────────────
-- Routine
-- ────────────────────────────────────────────────────────────────────

/-- A PostgreSQL function or procedure.
    $$\text{Routine} = \{ \text{schema}, \text{name}, \text{params},
      \text{returnType}, \text{volatility} \}$$ -/
structure Routine where
  /-- Function schema. -/
  funcSchema : Schema
  /-- Function name. -/
  funcName : FunctionName
  /-- Human-readable description. -/
  funcDescription : Option String := none
  /-- Function parameters. -/
  funcParams : Array RoutineParam := #[]
  /-- Return type. -/
  funcReturnType : RoutineReturnType
  /-- Volatility classification. -/
  funcVolatility : Volatility := .volatile
  /-- Transaction isolation level (if specified). -/
  funcIsoLevel : Option IsolationLevel := none
  /-- Whether the function returns a composite type (TABLE / record). -/
  funcReturnsComposite : Bool := false
  /-- The OID of the return type (for composite type column discovery). -/
  funcReturnTypeOid : Option Nat := none
  deriving Repr

/-- Get the qualified identifier for a routine. -/
def Routine.toQi (r : Routine) : QualifiedIdentifier :=
  { qiSchema := r.funcSchema, qiName := r.funcName }

/-- Get the required parameter names. -/
def Routine.requiredParams (r : Routine) : Array String :=
  (r.funcParams.filter (·.ppRequired)).map (·.ppName)

/-- Whether this routine can be called with a GET request
    (only immutable/stable functions). -/
def Routine.isSafeForGet (r : Routine) : Bool :=
  r.funcVolatility != .volatile

instance : ToString Routine where
  toString r := s!"{r.funcSchema}.{r.funcName}({r.funcParams.size} params)"

-- ────────────────────────────────────────────────────────────────────
-- Safety theorem
-- ────────────────────────────────────────────────────────────────────

/-- `isSafeForGet` is true if and only if the volatility is not `.volatile`.
    $$\text{isSafeForGet}(r) = \text{true} \iff
      r.\text{funcVolatility} \neq \text{volatile}$$
    This witnesses that only immutable and stable functions are safe for GET. -/
theorem Routine.isSafeForGet_iff_not_volatile (r : Routine) :
    r.isSafeForGet = true ↔ r.funcVolatility ≠ .volatile := by
  unfold Routine.isSafeForGet
  cases r.funcVolatility <;> simp <;> decide

end PostgREST.SchemaCache
