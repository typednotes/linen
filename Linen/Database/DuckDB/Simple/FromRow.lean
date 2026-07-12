/-
  Linen.Database.DuckDB.Simple.FromRow — the `FromRow` class, `RowParser`

  Module #8 of `docs/imports/duckdb-simple/dependencies.md`, on #5
  (`Linen.Database.DuckDB.Simple.FromField`, for `Field`/`FromField`), #1
  (`…Internal`), and #3 (`…Ok`).

  ## Design

  This is `Linen.Database.SQLite.Simple.FromRow`'s exact same shape, ported
  fresh into `duckdb-simple`'s own namespace/`Ok`/`Field` (per
  `docs/imports/duckdb-simple/dependencies.md`'s precedence note: the two
  upstream packages share no code, so neither does this port) rather than
  imported — see that module's doc for the full rationale behind
  `RowParser`'s single state-passing function (collapsing upstream's
  `ReaderT`/`StateT`/`Ok` stack), `fieldWith`/`field`/
  `numFieldsRemaining`/`returnRowError`, and the dropped `GFromRow`/
  `FromRow [a]` instances, all of which apply here unchanged.

  ### Tuple-arity cutoff

  Upstream's `duckdb-simple` `FromRow` actually goes up to arity **10**
  (one step further than `sqlite-simple`'s own arity-7 ceiling that
  `Linen.Database.SQLite.Simple.FromRow`'s module doc explains). This port
  keeps the established **arity-7** cutoff anyway rather than chasing
  upstream's own arity per package: 7 is this codebase's own settled
  convention for "the usual small tuple arities" once `Cons`/`(:.)`
  composition is available to reach further (`(a,b,c,d,e,f,g) :. (h,i,j)`
  reaches arity 10 with no dedicated instance, exactly the same
  work-around `…SQLite.Simple.FromRow`'s module doc already describes), and
  `duckdb-simple`'s own sibling `ToRow` module (module #9 of this batch)
  only goes up to arity **5** upstream — keeping one shared cutoff across
  both of this batch's row modules (rather than mirroring each one's own,
  mismatched upstream ceiling) avoids `FromRow`/`ToRow` supporting a
  different maximum plain-tuple width for what is otherwise the same
  "a full result row" concept in this port.

  ## Haskell source
  - `Database.DuckDB.Simple.FromRow` (`duckdb-simple` package, version
    0.1.5.1)
-/

import Linen.Database.DuckDB.Simple.Ok
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.Simple.FromField

namespace Database.DuckDB.Simple

-- ────────────────────────────────────────────────────────────────────
-- `RowParser`
-- ────────────────────────────────────────────────────────────────────

/-- A parser that consumes zero or more consecutive `Field`s from a result
    row, threading the current column position and accumulating `Ok`
    failures (see the module doc for how this represents upstream's
    `ReaderT`/`StateT`/`Ok` stack). -/
structure RowParser (α : Type u) where
  /-- Run the parser against a row's `Field`s starting at the given column,
      returning the parsed value and the column position just past what it
      consumed. -/
  run : Array Field → Nat → Ok (α × Nat)

namespace RowParser

instance : Functor RowParser where
  map f p := ⟨fun fields col =>
    match p.run fields col with
    | .ok (a, col') => .ok (f a, col')
    | .errors es => .errors es⟩

instance : Applicative RowParser where
  pure a := ⟨fun _ col => .ok (a, col)⟩
  seq pf px := ⟨fun fields col =>
    match pf.run fields col with
    | .errors es => .errors es
    | .ok (f, col') =>
      match (px ()).run fields col' with
      | .errors es => .errors es
      | .ok (a, col'') => .ok (f a, col'')⟩

end RowParser

/-- Consume one column, decoding it with a caller-supplied `FieldParser`
    rather than an ambient `FromField` instance. -/
def fieldWith (p : FieldParser α) : RowParser α :=
  ⟨fun fields col =>
    if h : col < fields.size then
      match p fields[col] with
      | .ok a => .ok (a, col + 1)
      | .errors es => .errors es
    else
      .errors #[s!"column index {col} out of bounds (row has {fields.size} columns)"]⟩

/-- Consume one column, decoding it via its `FromField` instance. -/
def field [FromField α] : RowParser α := fieldWith fromField

/-- The number of columns not yet consumed by this parser. -/
def numFieldsRemaining : RowParser Nat :=
  ⟨fun fields col => .ok (fields.size - col, col)⟩

/-- Fail the row parse with a message, without consuming a column. -/
def returnRowError (msg : String) : RowParser α :=
  ⟨fun _ _ => .errors #[msg]⟩

-- ────────────────────────────────────────────────────────────────────
-- The `FromRow` class
-- ────────────────────────────────────────────────────────────────────

/-- A collection type that can be decoded from a sequence of `Field`s (a
    full result row). Instances are provided for tuples up to arity 7 (see
    the module doc) and for `Only`/`(:.)`. -/
class FromRow (α : Type u) where
  /-- Parse an entire row. -/
  fromRow : RowParser α

export FromRow (fromRow)

/-- Decode a full row of `Field`s via its `FromRow` instance. Trailing
    unconsumed columns are ignored, matching upstream; only running out of
    columns mid-parse is reported, via `fieldWith`'s own bounds check. -/
def runFromRow [FromRow α] (fields : Array Field) : Ok α :=
  match (fromRow (α := α)).run fields 0 with
  | .ok (a, _) => .ok a
  | .errors es => .errors es

-- ────────────────────────────────────────────────────────────────────
-- `Only` / tuple / `(:.)` instances
-- ────────────────────────────────────────────────────────────────────

instance [FromField a] : FromRow (Only a) where
  fromRow := (fun x => ({ fromOnly := x } : Only a)) <$> field

instance [FromField a] [FromField b] : FromRow (a × b) where
  fromRow := Prod.mk <$> field <*> field

instance [FromField a] [FromField b] [FromField c] : FromRow (a × b × c) where
  fromRow := (fun x y z => (x, y, z)) <$> field <*> field <*> field

instance [FromField a] [FromField b] [FromField c] [FromField d] :
    FromRow (a × b × c × d) where
  fromRow := (fun x y z w => (x, y, z, w)) <$> field <*> field <*> field <*> field

instance [FromField a] [FromField b] [FromField c] [FromField d] [FromField e] :
    FromRow (a × b × c × d × e) where
  fromRow := (fun v w x y z => (v, w, x, y, z)) <$> field <*> field <*> field <*> field <*> field

instance [FromField a] [FromField b] [FromField c] [FromField d] [FromField e] [FromField f] :
    FromRow (a × b × c × d × e × f) where
  fromRow := (fun u v w x y z => (u, v, w, x, y, z)) <$>
    field <*> field <*> field <*> field <*> field <*> field

instance [FromField a] [FromField b] [FromField c] [FromField d] [FromField e] [FromField f]
    [FromField g] : FromRow (a × b × c × d × e × f × g) where
  fromRow := (fun t u v w x y z => (t, u, v, w, x, y, z)) <$>
    field <*> field <*> field <*> field <*> field <*> field <*> field

instance [FromRow a] [FromRow b] : FromRow (Cons a b) where
  fromRow := (fun x y => ({ car := x, cdr := y } : Cons a b)) <$> fromRow <*> fromRow

end Database.DuckDB.Simple
