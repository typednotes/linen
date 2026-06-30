/-
  Linen.Database.SQL.Statement — Type-safe parameterized SQL statements

  A `Statement params result` pairs a SQL query with a parameter encoder
  and a result decoder, ensuring at the type level that the encoder and
  decoder match the expected types.

  ## Haskell source
  - `Hasql.Statement` (hasql package)

  ## Design
  The phantom type parameters `params` and `result` ensure that:
  - The encoder (an `Encoders.Params`) converts `params` to SQL parameter arrays
  - The decoder (a `Decoders.Result`) converts a raw `PgResult` into `result`
  - Mismatches are caught at compile time

  Rather than re-declaring its own encoder/decoder aliases, a `Statement`
  composes the already-defined `Encoders.Params` and `Decoders.Result`, so
  `mapResult`/`contramapParams` reduce to `Result.map`/`Params.contramap`.
-/

import Linen.Database.SQL.Session
import Linen.Database.SQL.Encoders
import Linen.Database.SQL.Decoders
import Linen.Database.PostgreSQL.LibPQ

namespace Database.SQL.Statement

open Database.PostgreSQL.LibPQ
open Database.SQL.Session
open Database.SQL.Connection
open Database.SQL.Encoders (Params)
open Database.SQL.Decoders (Result)

-- ────────────────────────────────────────────────────────────────────
-- Statement
-- ────────────────────────────────────────────────────────────────────

/-- A type-safe parameterized SQL statement.
    $$\text{Statement}\ p\ r = \{ \text{sql} : \text{String},\
      \text{encode} : \text{Params}\ p,\ \text{decode} : \text{Result}\ r \}$$

    The `prepared` flag controls whether the statement uses PostgreSQL
    prepared statements (faster for repeated execution). -/
structure Statement (params : Type) (result : Type) where
  sql : String
  encode : Params params
  decode : Result result
  prepared : Bool := true

namespace Statement

/-- Run a statement within a session, encoding parameters and decoding results.
    $$\text{run} : \text{Statement}\ p\ r \to p \to \text{Session}\ r$$ -/
def run (stmt : Statement params result) (p : params) : Session result := do
  let conn ← read
  -- `prepared` is reserved for future PostgreSQL prepared-statement support;
  -- for now both prepared and unprepared statements use `execParams`.
  let pgResult ← execParams conn.raw stmt.sql (stmt.encode.encode p)
  let st ← resultStatus pgResult
  if st.isOk then
    match ← stmt.decode pgResult with
    | .ok a => return a
    | .error e => throw e
  else
    let msg ← resultErrorMessage pgResult
    throw (.queryError st msg)

/-- Create a statement that returns no result (INSERT, UPDATE, DELETE, etc.). -/
def command (sql : String) (encode : Params params) : Statement params Unit :=
  { sql, encode, decode := Result.unit, prepared := true }

/-- Create a statement that expects no parameters and returns no result. -/
def sql_ (sql : String) : Statement Unit Unit :=
  command sql Params.none

/-- Map the result type of a statement. -/
def mapResult (f : α → β) (stmt : Statement params α) : Statement params β :=
  { stmt with decode := Result.map f stmt.decode }

/-- Contramap the parameter type of a statement. -/
def contramapParams (f : β → α) (stmt : Statement α result) : Statement β result :=
  { stmt with encode := Params.contramap f stmt.encode }

end Statement
end Database.SQL.Statement
