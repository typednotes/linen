/-
  PostgREST.Plan.MutatePlan -- Mutation plans (INSERT/UPDATE/DELETE)

  Represents resolved mutation plans for DML operations.  Each variant
  carries the target table, affected columns, payload, filter conditions,
  and returning clause.

  ## Haskell source
  - `PostgREST.Plan.MutatePlan` (postgrest package)

  ## Design
  - `MutatePlan` is a sum type with one variant per DML operation:
    $$\text{MutatePlan} \in \{ \text{Insert},\; \text{Update},\; \text{Delete} \}$$
  - INSERT carries an optional `ConflictAction` for upsert support
    (`ON CONFLICT DO NOTHING | DO UPDATE`)
  - UPDATE and DELETE carry a `NonnegRange` for `LIMIT`-ed mutations
    (PostgreSQL extension via CTE)
-/

import Linen.PostgREST.Plan.Types
import Linen.PostgREST.Plan.ReadPlan
import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.Plan

open PostgREST.SchemaCache.Identifiers
open PostgREST.RangeQuery

-- ────────────────────────────────────────────────────────────────────
-- Mutation plan
-- ────────────────────────────────────────────────────────────────────

/-- A mutation plan for INSERT, UPDATE, or DELETE operations.
    $$\text{MutatePlan} ::=
      \text{Insert}(\text{table}, \text{cols}, \text{json}, \text{conflict}?,
        \text{ret}, \text{where})
      \mid \text{Update}(\text{table}, \text{cols}, \text{json}, \text{where},
        \text{ret}, \text{range})
      \mid \text{Delete}(\text{table}, \text{where}, \text{ret}, \text{range})$$ -/
inductive MutatePlan where
  /-- INSERT INTO table (columns) VALUES (payloadJSON)
      with optional ON CONFLICT action and RETURNING clause. -/
  | insert (into : QualifiedIdentifier)
           (columns : Array CoercibleField)
           (payloadJSON : String)
           (onConflict : Option ConflictAction)
           (returning : Array CoercibleSelectField)
           (where_ : Array CoercibleFilter)
  /-- UPDATE table SET columns = payloadJSON WHERE where_
      with RETURNING clause and optional range limit. -/
  | update (table : QualifiedIdentifier)
           (columns : Array CoercibleField)
           (payloadJSON : String)
           (where_ : Array CoercibleFilter)
           (returning : Array CoercibleSelectField)
           (range_ : NonnegRange)
  /-- DELETE FROM table WHERE where_
      with RETURNING clause and optional range limit. -/
  | delete (from_ : QualifiedIdentifier)
           (where_ : Array CoercibleFilter)
           (returning : Array CoercibleSelectField)
           (range_ : NonnegRange)

/-- Get the target table of a mutation plan. -/
def MutatePlan.targetTable : MutatePlan -> QualifiedIdentifier
  | .insert into .. => into
  | .update table .. => table
  | .delete from_ .. => from_

/-- Get the returning clause of a mutation plan. -/
def MutatePlan.returningFields : MutatePlan -> Array CoercibleSelectField
  | .insert _ _ _ _ ret _ => ret
  | .update _ _ _ _ ret _ => ret
  | .delete _ _ ret _ => ret

/-- Whether this mutation has a RETURNING clause. -/
def MutatePlan.hasReturning (mp : MutatePlan) : Bool :=
  !mp.returningFields.isEmpty

end PostgREST.Plan
